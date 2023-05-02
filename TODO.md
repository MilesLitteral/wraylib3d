This list will be ever expanding:

4/22/23
    Build Issues: All Resolved, None to Report (4/23)
        lc is now compiling successfully!
        hruby is now compiling, currently the methods for it are very crude and will need upgrading when the time comes, RubyInterpreterSTM will become very useful (ToRuby and FromRuby typeclasses will be necessary)
        HRayLib3d v0.0.1.0 (Pre-Alpha) is Building!~
        RubyC Source with a .o or .a library is necessary to fix the script engine's Foreign and Internal aspects (ghc will build an interpreter from this source), this isn't a priority but an objective that should be kept in mind and fixed as soon as opportunity presents itself

    High Priorities:
        Engine is building
        ScriptEngine is Building and now it's own module (functionality is another story)
        a test with the Foreign, Process, and Type Interfaces was done, only the Type (which
        is in Haskell anyway) succeeded. Research into RubyC Source and building it for ghc
        is necessary the Engine doesn't work but it could
        ~~Continue to Write out OpenXR-Raw, lambdacube-xr (Done)~~ Go as far with this as desired, it could be replaced with vulkan-lib's OpenXR module 

        ~~ Write lambdacube-vulkan backend (Done) ~~ This is what will happen: vulkan-api -> vulkan, where the vulkan 
        library will handle OpenXR and Metal rendering 

        Build the MapViewer, Run it's Main write lambdacube-metal
        Test some of the other mains, give them defining functions

    Medium Priorities:
        - Devise replacements for the following formats (ideal replacements listed):
            PK3  -> MegaStore/HAssetBundle (Asset Package Format)
            WAV  -> MP3/FLAC (Audio, WAV can remain as a Legacy Format)
            MD3  -> GLB/GLTF (3d Models, md3 support can continue as a Legacy format)
            Q3S  -> GLSL (Shader Language, to be expanded beyond GLSL)
            vect -> V3 (Double) or Data.Vector a  (vect is a 12 year old library! and the inconsistent use of the same type is bad practice)
        - Figure out sdl2's place in the design and create a true UI        
          haskell modules and systems (This may be the main Window and "Renderer" as a handler, it may be able to create SDL2.OpenGL, SDL2.Vulkan(therefore openXR and Metal Handling))
        - answer the various TODO comments in code, replace all instances of IORef for example with a better type (Monomer could be an inspiration here)

    Lowest Priorities:
        - Implement all the technical replacements

    Back Burner:
        - Create a ScriptableComponent on the Haskell Side that Ruby can interface with (Foreign may be helpful here)
            - create a binding between Haskell and Ruby layers, secondly create such binding between the old and new  
        - Run bot logic via ruby, additionally have JSON support in ruby
        - Write a HRay3d Ruby Gem

    Distant Priorities:
        - Write a rendering backend for Vulkan and Metal (may need mtlpp, study the Dylib Appendix) (Modern GPU Support) https://hackage.haskell.org/package/vulkan
        - Package some kind of BSPWriter in UI (Like UnrealEngine)
        - Create a "Developer UI" vs a "Game UI"
        - Create a shell for Cloud Service support (bespoke, Supabase like system, see also: Aleph-API)
        - Create an AIO SDK for writing and running games in Haskell!





