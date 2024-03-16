/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

#include "simd/simd.h";
#include "MetalKit.h";
#include "AAPLRenderer.h"

// Header shared between C code here, which executes Metal API commands, and .metal files, which
// uses these types as inputs to the shaders.
#include "AAPLShaderTypes.h"

// Main class performing the rendering
class AAPLRenderer
{
    MTL::Device _device;

    // The render pipeline generated from the vertex and fragment shaders in the .metal shader file.
    MTL::RenderPipelineState _pipelineState;

    // The command queue used to pass commands to the device.
    MTL::CommandQueue _commandQueue;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2 _viewportSize;
}

AAPLRenderer initWithMetalKitView:(MTKView* mtkView)
{
    self = new AAPLRenderer();
    if(self)
    {
        NS::Error *error;
        _device = mtkView.device;

        // Load all the shader files with a .metal file extension in the project.
        MTL::Library defaultLibrary     = _device.newDefaultLibrary();
        MTL::Function> vertexFunction   = defaultLibrary.newFunctionWithName("vertexShader");
        MTL::Function> fragmentFunction = defaultLibrary.newFunctionWithName("fragmentShader");

        // Configure a pipeline descriptor that is used to create a pipeline state.
        MTL::RenderPipelineDescriptor *pipelineStateDescriptor = new MTL::RenderPipelineDescriptor();
        pipelineStateDescriptor.label = "Simple Pipeline";
        pipelineStateDescriptor.vertexFunction   = vertexFunction;
        pipelineStateDescriptor.fragmentFunction = fragmentFunction;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;
        _pipelineState = _device.newRenderPipelineStateWithDescriptor(pipelineStateDescriptor, &error);
                
        // Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
        //  If the Metal API validation is enabled, you can find out more information about what
        //  went wrong.  (Metal API validation is enabled by default when a debug build is run
        //  from Xcode.)
        NS::Assert(_pipelineState, "Failed to create pipeline state: %@", error);

        // Create the command queue
        _commandQueue = _device.newCommandQueue();
    }
    return self;
}

/// Called whenever view changes orientation or is resized
void mtkView(MTKView* view, CGSize size)
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

/// Called whenever the view needs to render a frame.
void drawInMTKView(MTKView* view)
{
    static const AAPLVertex triangleVertices[] =
    {
        // 2D positions,    RGBA colors
        { {  250,  -250 }, { 1, 0, 0, 1 } },
        { { -250,  -250 }, { 0, 1, 0, 1 } },
        { {    0,   250 }, { 0, 0, 1, 1 } },
    };

    // Create a new command buffer for each render pass to the current drawable.
    MTL::CommandBuffer commandBuffer = _commandQueue.commandBuffer;
    commandBuffer.label = "MyCommand";

    // Obtain a renderPassDescriptor generated from the view's drawable textures.
    MTL::RenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil)
    {
        // Create a render command encoder.
        MTL::RenderCommandEncoder renderEncoder = commandBuffer.renderCommandEncoderWithDescriptor(renderPassDescriptor);
        renderEncoder.label = "MyRenderEncoder";

        // Set the region of the drawable to draw into.
        renderEncoder.setViewport((MTL::Viewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, 0.0, 1.0 });
        renderEncoder.setRenderPipelineState(_pipelineState);

        // Pass in the parameter data.
        renderEncoder.setVertexBytes(triangleVertices, sizeof(triangleVertices), AAPLVertexInputIndexVertices);
        renderEncoder.setVertexBytes(&_viewportSize, sizeof(_viewportSize), AAPLVertexInputIndexViewportSize);

        // Draw the triangle.
        renderEncoder.drawPrimitives(MTL::PrimitiveTypeTriangle, 0, 3);
        renderEncoder.endEncoding();

        // Schedule a present once the framebuffer is complete using the current drawable.
        commandBuffer.presentDrawable(view.currentDrawable);
    }

    // Finalize rendering here & push the command buffer to the GPU.
    commandBuffer.commit();
}