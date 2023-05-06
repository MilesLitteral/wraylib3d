{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Core.BuildBundler (
    buildWin64
    , buildUWP
    , precompileMetalShaders
    , compileMetalShaders
    , buildMetallib
    , buildMetalDyLib
    , configureMacOs
    , buildMacOs
    , configureLinux
    , configureAutoLinux
    , buildLinux
    ) where

    --windows support only for now
    import System.IO
    import System.Process 
    import Development.NSIS

    buildWin64 :: String -> String -> IO()
    buildWin64 filename title = writeFile (filename ++ ".nsi") $ nsis $ do
        name       (str  $ title ++ "-Game")            -- The name of the installer
        outFile    (str  $ title ++ ".exe")             -- Where to produce the installer
        installDir (str  $ "$DESKTOP/" ++ title)        -- The default installation directory
        requestExecutionLevel User               -- Request application privileges for Windows Vista
        -- Pages to display
        page Directory                           -- Pick where to install
        page InstFiles                           -- Give a progress bar while installing
        -- Groups fo files to install
        section "" [] $ do
            setOutPath "$INSTDIR"                -- Where to install files in this section
            file []    "app.hs"                  -- File to put into this section

    --  Presumed Path of MSBUILD: C:\WINDOWS\Microsoft.NET\Framework64\v4.0.30319>msbuild.exe 
    --  Example slnPath: "./projects/game/app/Hello.sln"
    --  Word of Warning: Before this truly works, a step will be necessary
    --                   to generate VS Solutions and Projects from WRL3D
    --                   Haskell Projects. At Minimum such Projects
    --                   would need to be linked (as .dlls) to the VS SLN
    --                   and either A) C++ is generated to interface with the WRL3D.dll
    --                   B) The Haskell somehow can interface with .NET
    --                   in a way, all the underlying components can be linked
    --                   with minimum pain (opengl.dll, vulkan.dll, sdl2.dll etc)
    buildUWP :: String -> IO Handle
    buildUWP slnPath = do
        (_, Just hout, _, _) <- createProcess (proc "msbuild.exe" [ slnPath ]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Be warned that none of these will Functions work unless this project is run on MacOS 
    -- or, somehow, you have a running xcode and xcrun on Linux/Windows
    -- for windows, the Metal Development Tools may provide a means to atleast build metallibs for later
    -- xcode compilation, or even xcproject generation for later compilation on a Mac

    -- Will Generate an .AIR file
    -- xcrun -sdk macosx metal -Os MyLibrary.metal xcrun -sdk macosx metal -Os MyLibrary.metal
    precompileMetalShaders :: String -> IO Handle
    precompileMetalShaders metal = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal", "-Os", (metal ++ ".metal")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Will Generate an .AIR file
    -- xcrun -sdk macosx metal -c MyLibrary.metal -o MyLibrary.air
    compileMetalShaders :: String -> IO Handle
    compileMetalShaders metal = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal", "-c", (metal ++ ".metal"), "-o", (metal ++ ".air")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- generate a Metal library by linking the AIR files. 
    -- xcrun -sdk macosx metal -frecord-sources -o LightsAndShadow.metallib Shadow.air PointLights.air DirectionalLight.air
    buildMetallib :: String -> [String] -> IO Handle
    buildMetallib libTitle airFiles = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" (zipWith (++) ["-sdk", "macosx", "metal", "-frecord-sources", "-o", (libTitle ++ ".metallib")] (map (\x -> x ++ ".air") airFiles))){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Separate the sources from the library and create a companion symbol file by running the metal-dsymutil command.
    -- xcrun -sdk macosx metal-dsymutil -flat -remove-source LightsAndShadow.metallib
    buildMetalDyLib :: String -> IO Handle
    buildMetalDyLib metallib = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal-dsymutil", "-flat", "-remove-source", (metallib ++ ".metallib")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Build Apple App
    -- xcodebuild -list -project <NAME>.xcodeproj/
    configureMacOs :: String -> IO Handle
    configureMacOs xcproj = do
        (_, Just hout, _, _) <- createProcess (proc "xcodebuild" ["-list", "-project", (xcproj ++ ".xcodeproj/")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- xcodebuild -workspace <WORKSPACE NAME> -scheme <SCHEME NAME> build
    buildMacOs :: String -> String -> IO Handle
    buildMacOs wrkspName schemeName = do
        (_, Just hout, _, _) <- createProcess (proc "xcodebuild" ["-workspace",  wrkspName, "-scheme", schemeName, "build"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
  
    -- Building a Game for WebAssembly:
    -- Build WRayLib3d as a dll with the -shared flag in ghc-options
    -- run buildWasm against the desired app.hs (ex: buildWasm /path/to/game/app (no suffix))

    -- wasm repo will then have the following structure:
    -- ./output/app/AssetBundles/ (.assetBundles)
    -- ./output/app/WRayLib3d.dll
    -- ./output/app/WebGL.dll?
    -- ./output/app/app.wasm
    -- Then, To Run the Game:
    -- wasmtime ./app.wasm

    -- tl;dr 
    --          buildWasm "./projects/game/app"
    -- <in cmd> wasmtime   ./output/game/app.wasm

    -- WebAssembly Build
    -- wasm32-wasi-ghc app.hs -o app.wasm
    -- wasmtime ./app.wasm
    buildWasm :: String -> IO Handle
    buildWasm appName = do
        (_, Just hout, _, _) <- createProcess (proc "wasm32-wasi-ghc" [(appName ++ ".hs"), "-o", (appName ++ ".wasm")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Linux Build
    -- ./project/configure
    configureLinux :: IO Handle
    configureLinux = do
        (_, Just hout, _, _) <- createProcess (proc "./configure" []){ cwd = Just "./", std_out = CreatePipe }
        return hout
    
    -- ./autogen.sh
    configureAutoLinux :: IO Handle
    configureAutoLinux = do
        (_, Just hout, _, _) <- createProcess (proc "autogen.sh" []){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- make ./project/
    buildLinux :: IO Handle
    buildLinux = do
        (_, Just hout, _, _) <- createProcess (proc "make" []){ cwd = Just "./", std_out = CreatePipe }
        return hout