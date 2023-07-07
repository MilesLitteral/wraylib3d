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
                                                   ++ "bin_PROGRAMS      = "++ projectName ++ " \n"
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
    --                   Haskell Projects. At Minimum such Projects
    --                   would need to be linked (as .dlls) to the VS SLN
    --                   and either A) C++ is generated to interface with the WRL3D.dll
    --                   B) The Haskell somehow can interface with .NET
    --                   in a way, all the underlying components can be linked
    --                   with minimum pain (opengl.dll, vulkan.dll, sdl2.dll etc)
    -- Run CMake first
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

    -- https://webglfundamentals.org/webgl/lessons/webgl-boilerplate.html
    -- https://github.com/emscripten-core/emsdk Required
    -- GHCJS builds WebGL games
    buildWebGL :: String -> IO Handle
    buildWebGL appName = do
        (_, Just hout, _, _) <- createProcess (proc "ghcjs" [(appName ++ ".hs"), "-o", (appName ++ ".js")]){ cwd = Just "./", std_out = CreatePipe }
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

    -- | Boots the user specified IDE via console command ("atom ./",  "code ./")
    -- NOTE: All 'start' functions appends a reference of the process' pipe to the
    -- ManifestModel subprocess Lens so they can be closed all at once when the main 
    -- window is, or be communicated to later
    startIDE :: String -> IO Handle
    startIDE  txt = do
        (_, Just hout, _, _) <- createProcess (shell txt){ cwd = Just "./assets/projects/", std_out = CreatePipe } --(_, Just hout, _, _) 
        return hout

-- Build for Mobile Functions (EXPERIMENTAL)
-- # Multi-line comments can be added here. This comment will be propagated
-- # to each generated definition.
-- my_enum = enum {
--     option1;
--     option2;
--     option3;
-- }

-- my_flags = flags {
--     flag1;
--     flag2;
--     flag3;
--     no_flags = none;
--     all_flags = all;
-- }

-- my_record = record {
--     id:    i32;
--     info:  string;
--     store: set<string>;
--     hash:  map<string, i32>;

--     values: list<another_record>;

--     # Comments can also be put here

--     # Constants can be included
--     const string_const: string = "Constants can be put here";
--     const min_value: another_record = {
--         key1 = 0,
--         key2 = ""
--     };
-- }

-- another_record = record {
--     key1: i32;
--     key2: string;
-- } deriving (eq, ord)

-- # This interface will be implemented in C++ and can be called from any language.
-- my_cpp_interface = interface +c {
--     method_returning_nothing(value: i32);
--     method_returning_some_type(key: string): another_record;
--     static get_version(): i32;

--     # Interfaces can also have constants
--     const version: i32 = 1;
-- }

-- # This interface will be implemented in Java and ObjC and can be called from C++.
-- my_client_interface = interface +j +o {
--     log_string(str: string): bool;
-- }