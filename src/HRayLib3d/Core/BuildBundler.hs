{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Core.BuildBundler (
    buildWin64
    , buildUWP
    , precompileMetalShaders
    , compileStaticMetallib
    , buildMetalDylib
    , buildMetalDySyms
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

    -- C:\WINDOWS\Microsoft.NET\Framework64\v4.0.30319>msbuild.exe "C:\Users\Saurabh\Do
    -- cuments\Visual Studio 2015\Projects\SimplyNote\SimplyNote.sln"
    buildUWP :: String -> IO Handle
    buildUWP slnPath = do
        (_, Just hout, _, _) <- createProcess (proc "msbuild.exe" [ slnPath ]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Will Generate an .AIR file
    precompileMetalShaders :: String -> IO Handle
    precompileMetalShaders metal = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal", "-Os", (metal ++ ".metal")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Build Static Library
    -- xcrun -sdk macosx metal -Os MyLibrary.metal xcrun -sdk macosx metal -Os MyLibrary.metal
    -- xcrun -sdk macosx metal -c MyLibrary.metal -o MyLibrary.air

    compileStaticMetallib :: String -> IO Handle
    compileStaticMetallib metal = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal", "-c", (metal ++ ".metal"), "-o", (metal ++ ".air")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- generate a Metal library by linking the AIR files. 
    -- xcrun -sdk macosx metal -frecord-sources -o LightsAndShadow.metallib Shadow.air PointLights.air DirectionalLight.air
    buildMetalDylib :: String -> [String] -> IO Handle
    buildMetalDylib libTitle airFiles = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" (zipWith (++) ["-sdk", "macosx", "metal", "-frecord-sources", "-o", (libTitle ++ ".metallib")] (map (\x -> x ++ ".air") airFiles))){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Separate the sources from the library and create a companion symbol file by running the metal-dsymutil command.
    -- xcrun -sdk macosx metal-dsymutil -flat -remove-source LightsAndShadow.metallib
    buildMetalDySyms :: String -> IO Handle
    buildMetalDySyms metallib = do
        (_, Just hout, _, _) <- createProcess (proc "xcrun" ["-sdk", "macosx", "metal-dsymutil", "-flat", "-remove-source", (metallib ++ ".metallib")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- Build Apple App
    -- xcodebuild -list -project <NAME>.xcodeproj/
    -- xcodebuild -workspace <WORKSPACE NAME> -scheme <SCHEME NAME> build

    -- xcodebuild -list -project <NAME>.xcodeproj/
    configureMacOs :: String -> IO Handle
    configureMacOs xcproj = do
        (_, Just hout, _, _) <- createProcess (proc "xcodebuild" ["-list", "-project", (xcproj ++ ".xcodeproj/")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    buildMacOs :: String -> String -> IO Handle
    buildMacOs wrkspName schemeName = do
        (_, Just hout, _, _) <- createProcess (proc "xcodebuild" ["-workspace",  wrkspName, "-scheme", schemeName, "build"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
  
    configureLinux :: IO Handle
    configureLinux = do
        (_, Just hout, _, _) <- createProcess (proc "./configure" []){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    configureAutoLinux :: IO Handle
    configureAutoLinux = do
        (_, Just hout, _, _) <- createProcess (proc "autogen.sh" []){ cwd = Just "./", std_out = CreatePipe }
        return hout

    buildLinux :: IO Handle
    buildLinux = do
        (_, Just hout, _, _) <- createProcess (proc "make" []){ cwd = Just "./", std_out = CreatePipe }
        return hout