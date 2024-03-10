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
    , configureAutoLinux
    , buildLinux
    , startIDE
    ) where

    --windows support only for now
    import System.IO        ( Handle )
    import System.Process   ( createProcess, proc, shell, CreateProcess(std_out, cwd), StdStream(CreatePipe) ) 
    import Development.NSIS ( nsis, file, installDir, name, outFile, page, requestExecutionLevel, section, setOutPath, str, Level(User), Page(InstFiles, Directory) )

    compileCMakeProject :: String -> String
    compileCMakeProject projectName = ("cmake_minimum_required(VERSION 3.23)              \n"
                                    ++ "project("  ++ projectName ++ " LANGUAGES CXX)     \n"
                                        
                                    ++   "if (MSVC)                                       \n"
                                    ++   "\tset(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)      \n"
                                    ++   "endif()                                         \n"

                                    ++   "add_library(vulkan    SHARED IMPORTED [GLOBAL]) \n"
                                    ++   "add_library(wraylib3d SHARED IMPORTED [GLOBAL]) \n"

                                    ++   "add_executable(app)                             \n"
                                    ++   "target_sources(app PRIVATE \"app/hs2c.cpp\")    \n"
                                    ++   "target_link_libraries(app PUBLIC vulkan)        \n"
                                    ++   "target_link_libraries(app PUBLIC wraylib3d)     \n")


    compileAutoMakeProject :: String -> String 
    compileAutoMakeProject projectName = ("AC_INIT(["++ projectName ++"], [0.1], [author@mail.com]) \n"
                                          ++ "AM_INIT_AUTOMAKE                                      \n"
                                          ++ "AC_PROG_CC                                            \n" 
                                          ++ "AC_CONFIG_FILES([Makefile])                           \n"
                                          ++ "AC_OUTPUT                                             \n")

    compileAutoMakeConfigureProject :: String -> String
    compileAutoMakeConfigureProject projectName = "AUTOMAKE_OPTIONS  = foreign                  \n"
                                                   ++ "bin_PROGRAMS  = " ++ projectName ++    " \n"
                                                   ++ (projectName ++ "_SOURCES  = main.c       \n")

    buildWin64 :: String ->  String -> IO ()
    buildWin64 filename title = writeFile (filename ++ ".nsi") $ nsis $ do
                                    name       (str  $ title ++ "-Game")     -- The name of the installer
                                    outFile    (str  $ title ++ ".exe")      -- Where to produce the installer
                                    installDir (str  $ "$DESKTOP/" ++ title) -- The default installation directory
                                    requestExecutionLevel User               -- Request application privileges for Windows Vista
                                    -- Pages to display
                                    page Directory                           -- Pick where to install
                                    page InstFiles                           -- Give a progress bar while installing
                                    -- Groups fo files to install
                                    section "wraylib3d" []     $ do
                                        setOutPath "$INSTDIR"                -- Where to install files in this section
                                        file []    "wraylib3d.dll"           -- File to put into this section
                                    section (str filename) []  $ do
                                        setOutPath "$INSTDIR"                -- Where to install files in this section
                                        file []    "app.hs"                  -- File to put into this section

    --  Presumed Path of MSBUILD: C:\WINDOWS\Microsoft.NET\Framework64\v4.0.30319>msbuild.exe 
    --  Example slnPath: "./projects/game/app/Hello.sln"
    --  Word of Warning: Before this truly works, a step will be necessary
    --                   to generate VS Solutions and Projects from WRL3D
    --                   Haskell Projects. At minimum such Projects
    --                   would need to be linked (as .dlls) to the VS SLN
    --                   and either A) C++ is generated to interface with the WRL3D.dll
    --                   B) The Haskell somehow can interface with .NET
    --                   in a way, all the underlying component dlls can be linked
    --                   with minimum pain (opengl.dll, vulkan.dll, sdl2.dll etc)
    -- Run CMake first
    buildUWP :: String -> IO Handle
    buildUWP slnPath = do
        (_, Just hout, _, _) <- createProcess (proc "msbuild.exe" [ slnPath ]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- These functions provide a means to configure a project for later, actual, compilation on MacOS.
    -- Be warned that the build functions won't fully work unless this project is run on MacOS (xcodebuild is required) 
    -- or, somehow, you have a running xcode/xcodebuild and xcrun instance on Linux/Windows.

    -- For Windows, the Metal Development Tools can provide a means to atleast build metallibs for later
    -- xcode compilation, or xcproject generation on a viable Mac. For Linux you may use either
    -- my mtlpp library, or you can use metalcpp to fill in the needed headers.

    -- On non-MacOS the build functions will call CMake to confirm the appropriate xcode project is built

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
  
    -- Linux/Ubuntu specific via AutoMake
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

    -- https://webglfundamentals.org/webgl/lessons/webgl-boilerplate.html
    -- https://github.com/emscripten-core/emsdk Required
    -- GHCJS builds WebGL games
    buildWebGL :: String -> IO Handle
    buildWebGL appName = do
        (_, Just hout, _, _) <- createProcess (proc "ghcjs" [(appName ++ ".hs"), "-o", (appName ++ ".js")]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    -- | Boots the user specified IDE via console command ("atom ./",  "code ./")
    -- NOTE: All 'start' functions appends a reference of the process' pipe to the
    -- ManifestModel subprocess Lens so they can be closed all at once when the main 
    -- window is, or be communicated to later
    startIDE :: String -> IO Handle
    startIDE  txt = do
        (_, Just hout, _, _) <- createProcess (shell txt){ cwd = Just "./assets/projects/", std_out = CreatePipe } --(_, Just hout, _, _) 
        return hout

    -- Build for Mobile Functions (EXPERIMENTAL)
    -- runDjinni.sh ir.djinni
    -- Valid for both iOS and Android, also supports C++ code gen from .djinni files
    runDjinni :: [String] -> IO Handle
    runDjinni buildList = do
        (_, Just hout, _, _) <- createProcess (proc "./wraylib3d-xross/runDjinni.sh" buildList){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    -- TODO: Add Build functions for VR Headsets. This step can vastly vary depending on headset.
    -- For starters lets go with a Quest headset as the build largely requires using Djinni for Android
    -- and building an APK and certifying GLSL shaders for SPIR-V. The required functions would need to use commandline: 
    --`adb devices` (lists devices)

    -- WIFI MODE:
    --`adb tcpip <port>` (set for tcpip mode)
    --`adb connect <ipaddress>:<port>` (open a connection)

    -- USB Mode:
    -- `adb -s <headset_id>` (hint: `adb devices` command will show you the id of a USB connected headset)

    -- `adb install [flags] <apk-path>` (flags can be nothing)
    -- Example: `adb install -r C:\Dev\Android\MyProject\VrApp.apk` (-r for overwrite if apk was already installed) WARNING: install path must be absolute.

    -- `adb disconnect` (close connection on quit)
    -- `adb shell ip route` (Useful util command)

    -- compileProgram p (where spirv == true)
    -- It is necessary to emulate the same method, as much as possible, across headsets

    {-
     do
        (_, Just hout, _, _) <- createProcess (proc "./wraylib3d-xross/runDjinni.sh" buildList){ cwd = Just "./", std_out = CreatePipe }
        return hout
    -}

    buildQuestVRApp  :: IO ()
    buildQuestVRApp   = undefined

    buildHoloLensApp :: IO ()
    buildQuestVRApp   = undefined

    buildViveApp :: IO ()
    buildViveApp      = undefined

    buildMagicLeapApp :: IO ()
    buildMagicLeapApp = undefined

    -- Functions for building to open source or third party headset.
    buildVRApp  :: IO ()
    buildVRApp = undefined

    buildARApp  :: IO ()
    buildVRApp = undefined

    buildXRApp  :: IO ()
    buildVRApp = undefined