# WRaylib3d (WRL3D)
## WRayLib3d Project Structure (Unity-Like Application)

    + Core  (Module)
        - Build Manager
            Handles the Building of Projects (WRLP) Into
            full native executables (planned support: Win32/64 (NSIS Executable), UWP, MacOS, iOS*, Linux      
            Application, Android*, Wasm), It may need a companion Launcer Manager.
            
            * = iOS Support is _theoretical_ whereas Android is possible but would require generating an
                Android Studio Project, this is a similar issue currently facing the UWP functionality
                which similarly has to convert this project into a functioning VS Solution/Project
        - Resource Bundler
            Handles The compiling of all project 3d Assets (and associated assets) into .AssetBundles which are 
            accessed at runtime. AssetBundles can be thought of like onion data structure, 
            ie: AssetBundle([(Key, GLB(Header, [GLTF]))])

        - Structured Data 
            Descriptions of All ingame classes as JSON data structures, these are then used for serialzing the game
            as save files or transmittable data. It could also be expanded to support XMLSerialization and Deserialization

        - Shader Precompiler Pipeline
            Handles packaging of applicable Shaders to ShaderCaches which are accessed at runtime
            this system is part of an ongoing process to completely deprecate the use of Quake3
            Shaders and move to a modern shader system
                      
            ? There is a Format the engine uses called "LC" a kind of custom scripting language that largely is used in relation to Engine Graphics, 
            I desire to replace Quake Shaders and (perhaps) LC as well or re appropriate it so that shaders are instead GLSL, or some other format 
            which can be used rather than emulating to make Quake happy. It also is a kind of future proofing
            
        - Window System
            Handles SDL2 Windowing and User Interface Creation at Application level
            At some point, it will have a full Front-End Interface to create Widgets in Window  
            this library uses Protea Audio and therefore can use OGGs and WAVs, there is an interest
            in maybe making an Audio System to go along with it.      

            When this exists fully, SDL2 will handle Input, Audio, Windowing, and Widgets therefore 
            separating concerns in terms of which modules handle what, its currently a bit of a mess
            
    + Game Engine (Module)
        - Data 
            Contains all Type Level descriptions of ingame data like Maps (BSP), GameCharacters, Models (MD3)
            There is an ongoing effort to add new Bits such as GLBs and GLTFs

        - Graphics
            Contains Functions for operating on the Data Types and Rendering them, currently the Rendering Pipeling
            presumes that everything will be interms of OpenGL Buffers, efforts are thus focused on having the GLBs
            read into lists of GLTFs then the GLTFs are loaded and read into OpenGL Buffers, at a later point in 
            Development the Rendering Pipeline will be backend agnostic and the Buffers will instead be a kind of 
            Intermediate Representation (IR), expanding on LambdaCube-IR and probably LambdaCube-Compiler
            Related Projects: LambdaCube-Vulkan, LambdaCube-Metal, LambdaCube-XR, LambdaCube-GL (The Backends)

            There's also a desire to upgrade LambdaCube-IR to use GraphQL as a querying language rather than JSON as it's currently using as GraphQL is incredibly flexible

        - Loader
            Contains functions for reading the Binary File Types into their Logical Data Types, a GLB and GLTF Loader
            is in Progress

        - MapViewer
            Module for Loading Levels and viewing them, Full of QoL TODOs that will be addressed later on in
            development

        - Realm
            Module for Games and their Definition, here game logic, entities, levels, physics, and all are defined
            This class is what users build via the UI and is actually "Compiled" by WRL3D when the user exports 
            their game, rather, the assets here and logic here is what will be compiled as an executable with dlls, 
            assetBundles,  and binaries. There is an interest here to add the DB Module connections, and a module for reading
            a companion .db file

        - All Other files in here relate to core Game Engine functions, Render System, Scene (Manager), and Utils will stay whereas there is a desire to work Collision and Content into their own Modules (Physics, and Content respectively)

    + Network
        - Database
            Low level Postgres-SQL Functions, so far it only contains Query, which is all that is necessary
            for Realm purposes; you are required to have PostgresSQL installed on your system to use pgl
            if you want to use this module and it is bound to pgl otherwise thise module creates segfaults

        - Requests
            Low Level HTTP Functions, it can query web addressed in terms of HTTPS, JSON, or Binary and can handle all
            those responses, in the future any UDP Streaming functions will be described here for Multiplayer functionality as will any SSH or Tunneling functions that are of future interest

        - Realms
            Levels as described in their Web format, Levels can be queried and their assets downloaded.
            This Module will probably be more important when Multiplayer is a thing in the future

        - Cloud (TBA)
            A Module which handles interfacing with a SaaS like Firebase or Supabase if the User Desires,
            look here for all analytic needs

    + Script Engine
        - All Definitions for HRuby, later will be a generalized interface that the user can use to script ingame actions
          , mods, and logic. Planned support: Python, Ruby, Lua

    + Benchmarks
        - Application Benchmark Utilities, it has a Benchmark for running the mapviewer, running the default game, and
        entries for custom benchmarking full games or individual functions

        ? There is a desire to make visualization functions so that benchmarking can be presented Graphically or non Graphically 

    + Guides (TBA)
        - A Module for Generating LaTeX PDFs via HaTeX (haskell latex)
        - This will be a general utility that could evolve into other
          notation forms for UI etc

    + Utils
        - Misc Application Utils, all Logging functions for example are here, as is prettyPrinting and Md3Show.
          the PrettyPrinter is entirely broken and needs to be fixed for generalized use
Config.Yaml
    Will be expanded to include Tests, examples, and alternative builds based on simple flags passed

### How To Run WRayLib3d

    -  System Prerequisites: ghc, stack, cabal (ghcup toolchain), opengl
    -  Core Dependencies: lambdacube-compiler, lambdacube-ir, lambdacube-gl (There is no need to satisfy optionals, and 
       they all come in the project anyway, enable and disable them with the config.yaml)
    - in commandline: stack run/stack build

### How To Run The MapViewer or Demo
First Download This .pk3 (In the future it will be replaced by an "AssetBundle" (MegaStore):
https://github.com/patdohere/dockerfiles/raw/master/quakejs-stack/quakejs/base/baseq3/pak0.pk3

Place "pak0.pk3" in the root directory (./WRaylib3d)
stack run mapviewer-debug

Notes:
M) lambdacube-metal requires mtlpp 
   https://github.com/MilesLitteral/mtlpp
W) Note if you wish to test the WebAssembly module you will need wasmtime installed on your machine
   https://wasmtime.dev/


