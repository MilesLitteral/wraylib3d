This list will be ever expanding:

4/22/23
    Past Success:
        Engine is building
        ScriptEngine is Building and now it's own module (functionality is another story)
        a test with the Foreign, Process, and Type Interfaces was done, only the Type (which
        is in Haskell anyway) succeeded. Research into RubyC Source and building it for ghc
        is necessary the Engine doesn't work but it could
~~Continue to Write out OpenXR-Raw, lambdacube-xr (Done)~~ 
        Go as far with this as desired, it could be replaced with vulkan-lib's OpenXR module 

~~Write lambdacube vulkan backend (Done)~~
        This is what will happen: vulkan-api -> vulkan, where the vulkan 
        library will handle OpenXR and Metal rendering 

        Build the MapViewer, Run it's Main write lambdacube-metal
        Test some of the other mains, give them defining functions

        lc is now compiling successfully!
        hruby is now compiling, currently the methods for it are very crude and will need upgrading when the time comes, RubyInterpreterSTM will become very useful (ToRuby and FromRuby typeclasses will be necessary)
        HRayLib3d v0.0.1.0 (Pre-Alpha) is Building!~
        RubyC Source with a .o or .a library is necessary to fix the script engine's Foreign and Internal aspects (ghc will build an interpreter from this source), this isn't a priority but an objective that should be kept in mind and fixed as soon as opportunity presents itself

    Build Issues: All Resolved, None to Report (4/23)
        TODO: post the latest GHC Report here, there are new build issues and big things to tackle
        - Implement the Language Parser (GLSL, Metal) (also figure out where LambdaCube's glsl shaderpipeline is and edit it into WRayLib ShaderPipeline style), it currently is from a submodule that is in the interest of this feature    
        - Implement GPUCommand, rename it (PipelineCommand is now its name), it could be a means to have the backend communicate with SDL2 and the Raw Widget
~~Cleanup code and consolidate any orphaned modules/functions~~

    High Priorities:
        - fix search query, reevalute sess' use
        - fix front end file system features
        - remove duplicate button in Controller menu
        - connect engine cloud, build, web, and controller feature to front end
        - add export controller features
        - add db load, remove entry, edit entry, and validate query features
        - editor options, network prefs, in project prefs
        - external deps list in build list
        - add on to editinglayer context (ex:build list)
        - fix footer features (dedicated server, tooltips, cameras on/off, net option)
        - fix project and level naming system
        - expand renderer window to include shader optiond
        - add button icon for CbType
        - add third party button icons, mouse & keyboard
        - add build button to build window
        - serialize .rlm (levels), and .wcb files (button profiles)
        - make meaningful ide integration
        -
        - Build a RenderPipelineWidget for 3d Rendering in the front-end
~~Build a multi-composite widget setup with fades and transitions for alternative windows/functions~~ 
        Done, window system is working, and fixed lists

        - Figure out sdl2's place in the design and create a true UI        
        haskell modules and systems (This may be the main Window and "Renderer" as a handler, it may be able to create SDL2.OpenGL, SDL2.Vulkan(therefore openXR and Metal Handling)), This is still in progress, Monomer is being used to build the UI and GUI

        - Prepare to move all work from the RealmViewer executable (outside of buildUI, handleEvent, and initApp) into WRayLib3d.WindowSystem
        as the file is quite large and has many features that are in progress, do this when all these inprogress features have been addressed

    Medium Priorities:
        - Devise replacements for the following formats (ideal replacements listed):
            PK3  -> MegaStore/HAssetBundle (Asset Package Format)
            WAV  -> MP3/FLAC (Audio, WAV can remain as a Legacy Format)
            MD3  -> GLB/GLTF (3d Models, md3 support can continue as a Legacy format)
            Q3S  -> GLSL (Shader Language, to be expanded beyond GLSL)
            vect -> V3 (Double) or Data.Vector a  (vect is a 12 year old library! and the inconsistent use of the same type is bad practice)
        - get back into the submodules and continue to implement the Vulkan, ~~GLES~~, Metal, and ~~SPIR-V(OpenGL)~~ backends
        - get working demos of the backends working with SDL2 instead of GLFW or figure out a clever solution 
            (It's said SDL2 can take a GLContext which is then manipulated by GLEW?)
        - answer the various TODO comments in code, replace all instances of IORef for example with a better type (Monomer could be an inspiration here)
        - Implement WRayLib3d.Network.FTP and WRayLib3d.Network.UDP 
        - Update Github Projects on these issues
        - write script engine front end components/widgets

    Lowest Priorities:
        - Implement all the technical replacements

    Back Burner:
        - Create a ScriptableComponent on the Haskell Side that Ruby can interface with (Foreign may be helpful here)
            - create a binding between Haskell and Ruby layers, secondly create such binding between the old and new  
        - Run bot logic via ruby, additionally have JSON support in ruby
        - Write a HRay3d Ruby Gem
        - Implement the StructuredData Module meaningfully (JSON, XML, YAML support)

    Distant Priorities:
        - Write a rendering backend for Vulkan and Metal (may need mtlpp, study the Dylib Appendix) (Modern GPU Support) https://hackage.haskell.org/package/vulkan
        - Package some kind of BSPWriter in UI (Like UnrealEngine)
        - Create a "Developer UI" vs a "Game UI"
        - Create a shell for Cloud Service support (bespoke, Supabase like system, see also: Aleph-API)
        - Create an AIO SDK for writing and running games in Haskell!