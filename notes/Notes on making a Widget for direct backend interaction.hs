Notes on making a Widget for direct backend use
Very important for the RendererWidget that is coming up
TODO:
1) wiring mouse/keyboard/gamepad inputs 
2) uploading a texture to your GPU/render engine 
3) providing a render function that can bind textures and render textured triangles.

https://github.com/fjvallarino/monomer/issues/24
 It is possible to make OpenGL-based widgets without rendering them to a texture. The render function of Widget runs
 in IO instead of a restricted monad; this allows calling any OpenGL function you need. 
 If other Renderer instances are added in the future (for Vulkan or Metal), this escape hatch 
 will allow making low-level rendering with them too. The objective of choosing IO instead of a 
 restricted monad was to give flexibility to applications that need this kind of functionality without 
 tying the library to any specific backend.

When doing low-level OpenGL rendering, some care needs to be taken:
Since NanoVG depends on OpenGL's context, modifying it may cause unexpected results. You will need to call Renderer's saveContext/restoreContext for this.
Unless you are using appRenderOnMainThread, doing anything related to OpenGL outside the render function will cause a crash due to OpenGL's threading constraints. Consequently, shader compilation has to happen in the render function; 
you will need to use an IORef for storing state (that you can create on init) to avoid re-compiling the shader or re-creating vertex buffers every time. I will improve the API to remove this workaround and allow initializing OpenGL resources with a mechanism equivalent to RunTask.
I'll add an example with proper documentation; I had this in my backlog, but I never got around to implementing it. I'll keep you posted.

https://github.com/fjvallarino/monomer/issues/261
Map is currently JS (nightmare), 3D view is openGL, profile view is canvas, bottom right is SVG.

Peter
peterjamesward commented on Feb 21
Ah. I've found your openGL example, that should help!

fjvallarino commented on Feb 21
@peterjamesward I think creating a custom widget is the best option in this case, yes. 
If you only need 2D, maybe you are fine with the drawing API provided by the library (which in turn uses NanoVG). 
If one of your maps is tiles based (similar to what some clients of OpenStreetMap do), it will be simpler than going 
full into OpenGL.

https://github.com/fjvallarino/monomer/issues/10
Monomer.Graphics.Types.Renderer (hs)
The library has a Renderer abstraction that is used by widgets for all rendering related operations. 
Currently, there is only one implementation based on NanoVG, although other implementations could be added.
 I recently added this section to Design decisions that could be of interest, especially for the alternative 
 renderer ideas.

Regarding GLFW, I think that one is going to be much harder. Originally I wanted to abstract SDL to be able to 
replace it with a different library (GLFW in particular), but it turned out to be complex, and I didn't feel it 
provided much value for what I needed at that point (plus I'd like to support mobile someday, which GLFW does not
 handle). At this point, Monomer is pretty much tied to SDL2.

In Monomer.Graphics.Types.Renderer 
This runs _before_ overlays of any type, and it's useful for the content of widgets created with low level APIs.
  -}
  createRawTask :: IO () -> IO (),
  -- | Renders the added raw tasks and clears its queue.
  renderRawTasks :: IO (),
  {-|
  Creates an overlay which does not rely on the abstractions provided by the
  Renderer. Well suited for pure OpenGL/Vulkan/Metal.

  This runs _after_ overlays based on Renderer.
  -}
  createRawOverlay :: IO () -> IO (),
  -- | Renders the added raw overlays and clears its queue.
  renderRawOverlays :: IO (),

Note regarding mixing lower-level rendering code
While still using the current NanoVG implementation, in your widget's render function, you can call createRawTask; 
the action provided to this function will be run outside the context of NanoVG. Since NanoVG uses OpenGL, this means 
you will be able to run any OpenGL command you want (and also means you have to take care of setting everything up, 
including shaders). Raw tasks are executed after rendering regular widgets (it's a separate rendering pass).

I don't currently have an example at hand, and it's been a while since I last tested this, so weird issues may happen
that I'm unaware of.

dpwiz commented on Aug 18, 2021
Hmm. Building on DearImGui is an interesting venue - opens up many different backends.

dpwiz commented on Aug 18, 2021
The polymorphic event story is a mess, I agree. I, too, decided to ditch it for now, in my engine and stuck with glfw instead.

fjvallarino commented on Aug 18, 2021
Just to clarify, the widgets are agnostic to SDL2. They handle events and requests using Monomer's types and functions.

What I mean when I say that it is tied to SDL2 is that the internals, which are not visible to library users, are 
programmed directly using SDL2 functions instead of using some abstraction. This is contrary to what happens with 
Renderer, which can be swapped for a different one.

fjvallarino commented on Aug 22, 2021
@dpwiz I'll close this issue for the time being. If you have any other questions or issues, don't hesitate to re-open it or create a new one. Thanks!


dpwiz commented on Sep 16, 2021
BTW, I've added most of the DrawList methods to dear-imgui bindings.
There are some event/input related functions I need to investigate, they might provide that cross-framework platform.

fjvallarino commented on Sep 17, 2021
@dpwiz nice! I'll take a look. My main concern is I'm relying on quite a few NanoVG features:

Painting with a texture inside an arbitrary path (can be seen when setting rounded corners on an image widget)
General curves (bezier, quadratic, etc), which are rendered pretty nicely.
I have to check how hard it is to get the same out of ImGUI's DrawList.

https://github.com/haskell-game/dear-imgui.hs/tree/main
https://github.com/fjvallarino/monomer/issues/52 (Bonus)