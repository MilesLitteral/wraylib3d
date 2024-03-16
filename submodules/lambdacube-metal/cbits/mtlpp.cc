

@import simd;
@import MetalKit;

#include "mtlpp.h"
#include "AAPLRenderer.h"
#include "AAPLMathUtilities.h"

// Include header shared between C code here, which executes Metal API commands, and .metal files
#include "AAPLShaderTypes.h"

// The max number of command buffers in flight
static const NS::UInteger AAPLMaxFramesInFlight = 3;
const unsigned int arrayLength = 10; //1 << 24;
const unsigned int bufferSize  = arrayLength * sizeof(float);

//xcrun -sdk macosx metal -c add.metal -o add.air
//xcrun -sdk macosx metallib add.air -o add.metallib

//xcrun -sdk macosx metal -c operations.metal -o operations.air
//xcrun -sdk macosx metallib operations.air -o operations.metallib

// Main class performing the rendering
class AAPLRenderer {
    //dispatch_semaphore_t _inFlightSemaphore;
    MTL::Buffer  _frameDataBuffer[AAPLMaxFramesInFlight];
    NS::UInteger _frameNumber;

    MTL::Device _device;
    MTL::CommandQueue _commandQueue;

    MTL::RenderPipelineState _renderPipeline;
    MTL::DepthStencilState   _depthState;
    MTL::Texture _colorMap;
    MTL::Buffer  _positionBuffer;
    MTL::Buffer  _texCoordBuffer;
    MTL::Buffer  _indexBuffer;
    MTL::Texture _colorTarget;

    MTL::RenderPassDescriptor *_renderPassDescriptor;
    MTL::VertexDescriptor     *_vertexDescriptor;

    // Projection matrix calculated as a function of view size
    matrix_float4x4 _projectionMatrix;
    float _rotation;

    MTL::ComputePipelineState _computePipeline;
    MTL::ComputePipelineDescriptor *_baseDescriptor;
    MTL::Size _dispatchExecutionSize;
    MTL::Size _threadsPerThreadgroup;
    NS::UInteger _threadgroupMemoryLength;

    /// Initialize with the MetalKit view with the Metal device used to render.  This MetalKit view
    /// object will also be used to set the pixelFormat and other properties of the drawable
    AAPLRenderer initWithMetalKitView(MTKView* mtkView)
    {
        self = new AAPLRenderer();
        if(self)
        {
            _device = mtkView.device;
            _inFlightSemaphore = dispatch_semaphore_create(AAPLMaxFramesInFlight);
            self.loadMetal(mtkView);
            self.loadAssets();
        }
        return self;
    }

    /// Create the Metal render state objects including shaders and render state pipeline objects
    void loadMetal(MTKView* mtkView)
    {
        self.setUpView(mtkView);
        MTL::Library metalLibrary = self.loadMetallib;

        self.createBuffers;
        self.createRenderStateWithLibrary(metalLibrary);
        self.createComputePipelineWithLibrary(metalLibrary);
        _commandQueue = _device.newCommandQueue();
    }

    // Load the Metal library created in the "Build Executable Metal Library" build phase.
    // This library includes the functions in AAPLShaders.metal and the UserDylib.metallib
    // created in the "Build Dynamic Metal Library" build phase.
    MTL::Library loadMetallib() {
        NS::Error *error;
        MTL::Library library = _device.newLibraryWithURL(NS::Bundle mainBundle, "AAPLShaders", "metallib", &error);
        NS::Assert(library, "Failed to load AAPLShaders dynamic metal library: %@", error);
        return library;
    }

    /// Set up properties of the view
    void setUpView(MTKView* mtkView) {
        mtkView.colorPixelFormat = MTL::PixelFormatBGRA8Unorm;
        mtkView.sampleCount = 1;
    }

    /// Create buffers modified each frame to animate the cube
    void createBuffers(){
        for(NS::UInteger i = 0; i < AAPLMaxFramesInFlight; i++) {
            _frameDataBuffer[i] = _device.newBufferWithLength(sizeof(AAPLPerFrameData), MTL::ResourceStorageModeShared);
            _frameDataBuffer[i].label = "FrameDataBuffer";
        }
    }

    /// Create the render pass descriptor, render pipeline state object, and depth state object to render the cube.
    void createRenderStateWithLibrary(MTL::Library metallib){
        NS::Error *error;
        // Set up render pass descriptor
        _renderPassDescriptor = new MTL::RenderPassDescriptor();
        _renderPassDescriptor.colorAttachments[0].clearColor  = MTL::ClearColorMake(0, 0, 0, 1);
        _renderPassDescriptor.colorAttachments[0].loadAction  = MTL::LoadActionClear();
        _renderPassDescriptor.colorAttachments[0].storeAction = MTL::StoreActionStore();
        _renderPassDescriptor.depthAttachment.loadAction      = MTL::LoadActionClear();
        _renderPassDescriptor.depthAttachment.storeAction     = MTL::StoreActionDontCare();

        // Set up render pipeline
        _vertexDescriptor = new MTL::VertexDescriptor();
        _vertexDescriptor.attributes[AAPLVertexAttributePosition].format = MTL::VertexFormatFloat3;
        _vertexDescriptor.attributes[AAPLVertexAttributePosition].offset = 0;
        _vertexDescriptor.attributes[AAPLVertexAttributePosition].bufferIndex = AAPLBufferIndexMeshPositions;

        _vertexDescriptor.attributes[AAPLVertexAttributeTexcoord].format = MTL::VertexFormatFloat2;
        _vertexDescriptor.attributes[AAPLVertexAttributeTexcoord].offset = 0;
        _vertexDescriptor.attributes[AAPLVertexAttributeTexcoord].bufferIndex = AAPLBufferIndexMeshGenerics;

        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stride   = 16;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stepRate = 1;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshPositions].stepFunction = MTL::VertexStepFunctionPerVertex;

        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stride   = 8;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stepRate = 1;
        _vertexDescriptor.layouts[AAPLBufferIndexMeshGenerics].stepFunction = MTL::VertexStepFunctionPerVertex;

        // Load the vertex function from the library
        MTL::Function vertexFunction  = metallib.newFunctionWithName("vertexShader");

        // Load the fragment function from the library
        MTL::Function fragmentFunction = metallib.newFunctionWithName("fragmentShader");

        MTL::RenderPipelineDescriptor *pipelineDescriptor = new MTL::RenderPipelineDescriptor();
        pipelineDescriptor.label = "RenderPipeline";
        pipelineDescriptor.sampleCount = 1;
        pipelineDescriptor.vertexFunction = vertexFunction;
        pipelineDescriptor.vertexDescriptor = _vertexDescriptor;
        pipelineDescriptor.fragmentFunction = fragmentFunction;
        pipelineDescriptor.colorAttachments[0].pixelFormat = MTL::PixelFormatRGBA8Unorm;
        pipelineDescriptor.depthAttachmentPixelFormat      = MTL::PixelFormatDepth32Float;
        _renderPipeline = _device.newRenderPipelineStateWithDescriptor(pipelineDescriptor, &error);

        NS::Assert(_renderPipeline, "Failed to create render pipeline state: %@", error);

        // Set up depth stencil state
        MTL::DepthStencilDescriptor *depthStateDesc = new MTL::DepthStencilDescriptor();
        depthStateDesc.depthCompareFunction = MTL::CompareFunctionLess()
        depthStateDesc.depthWriteEnabled    = true; //YES;
        _depthState = _device.newDepthStencilStateWithDescriptor(depthStateDesc);
    }

    void createComputePipelineWithLibrary(MTL::Library metallib){
        NS::Error *error;
        MTL::Function kernelFunction = metallib.newFunctionWithName("dylibKernel");
        _baseDescriptor = new MTL::ComputePipelineDescriptor();
        _baseDescriptor.computeFunction = kernelFunction;
        MTL::ComputePipelineDescriptor *descriptor = new MTL::ComputePipelineDescriptor();
        descriptor.computeFunction = _baseDescriptor.computeFunction;
        
        _computePipeline = _device.newComputePipelineStateWithDescriptor(descriptor, MTL::PipelineOptionNone, NULL, &error);
        NS::Assert(_computePipeline, "Error creating pipeline which links library from source: %@", error);
    }

    /// Load assets into metal objects
    void loadAssets(){
        // Create a buffer with positions to draw the cube.
        static const vector_float3 cubePositions[] =
        {
            // Front
            { -1, -1,  1 },
            { -1,  1,  1 },
            {  1,  1,  1 },
            {  1, -1,  1 },

            // Top
            { -1,  1,  1 },
            { -1,  1, -1 },
            {  1,  1, -1 },
            {  1,  1,  1 },

            // Right
            {  1, -1,  1 },
            {  1,  1,  1 },
            {  1,  1, -1 },
            {  1, -1, -1 },

            // Back
            { -1,  1, -1 },
            { -1, -1, -1 },
            {  1, -1, -1 },
            {  1,  1, -1 },

            // Bottom
            { -1, -1, -1 },
            { -1, -1,  1 },
            {  1, -1,  1 },
            {  1, -1, -1 },

            // Left
            { -1, -1, -1 },
            { -1,  1, -1 },
            { -1,  1,  1 },
            { -1, -1,  1 }
        };

        _positionBuffer = _device.newBufferWithBytes(cubePositions, sizeof(cubePositions), 0);
        
        // Create a buffer with texture coordinates to draw the cube.
        static const vector_float2 cubeTexCoords[] =
        {
            // Front
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Top
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Right
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Back
            { 1, 0 },
            { 1, 1 },
            { 0, 1 },
            { 0, 0 },

            // Bottom
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },

            // Right
            { 0, 0 },
            { 0, 1 },
            { 1, 1 },
            { 1, 0 },
        };

        _texCoordBuffer = _device.newBufferWithBytes(cubeTexCoords, sizeof(cubeTexCoords), 0);
        
        // Create the index buffer to draw the cube.
        static uint16_t indices[] =
        {
            // Front
                0,  2,  1,  0,  3,  2,

            // Top
                4,  6,  5,  4,  7,  6,

            // Right
                8, 10,  9,  8, 11, 10,

            // Back
            12, 14, 13, 12, 15, 14,

            // Bottom
            16, 18, 17, 16, 19, 18,

            // Left
            20, 22, 21, 20, 23, 22,
        };

        _indexBuffer = _device.newBufferWithBytes(indices, sizeof(indices), 0);
        

        // Load color texture from asset catalog
        MTKTextureLoader* textureLoader = MTKTextureLoader::alloc(_device);
        NS::Dictionary *textureLoaderOptions = {
            MTKTextureLoaderOptionTextureUsage       = MTL::TextureUsageShaderRead,
            MTKTextureLoaderOptionTextureStorageMode = MTL::StorageModePrivate
            };

        NS::Error *error;

        _colorMap = textureLoader.newTextureWithName("ColorMap", scaleFactor:1.0, NULL, textureLoaderOptions, &error);
        NS::Assert(_colorMap, "Error creating the Metal texture, error: %@.", error);
    }

    /// Update any scene state before encoding rendering commands to our drawable
    void updateSceneState()
    {
        NS::UInteger frameDataBufferIndex = _frameNumber % AAPLMaxFramesInFlight;

        AAPLPerFrameData *frameData = (AAPLPerFrameData*)_frameDataBuffer[frameDataBufferIndex].contents;

        frameData->projectionMatrix = _projectionMatrix;

        vector_float3   rotationAxis  = { 1, 1, 0 };
        matrix_float4x4 modelMatrix = matrix4x4_rotation(_rotation, rotationAxis);
        matrix_float4x4 viewMatrix  = matrix4x4_translation(0.0, 0.0, -6.0);

        frameData->modelViewMatrix = matrix_multiply(viewMatrix, modelMatrix);

        _rotation += .01;
    }

    /// Update the 3D projection matrix with the given size
    void updateProjectionMatrixWithSize(CGSize size)
    {
        /// Respond to drawable size or orientation changes here
        float aspect = size.width / (float)size.height;
        _projectionMatrix = matrix_perspective_right_hand(65.0f * (M_PI / 180.0f), aspect, 0.1f, 100.0f);
    }

    /// Create render targets for compute kernel inputs
    void createRenderTargetsWithSize(CGSize size)
    {
        MTLTextureDescriptor *renderTargetDesc = [MTLTextureDescriptor new];

        // Set up properties common to both color and depth textures.
        renderTargetDesc.width  = size.width;
        renderTargetDesc.height = size.height;
        renderTargetDesc.storageMode = MTL::StorageModePrivate;

        // Set up a color render texture target.
        renderTargetDesc.pixelFormat = MTL::PixelFormatRGBA8Unorm;
        renderTargetDesc.usage       = MTL::TextureUsageRenderTarget | MTL::TextureUsageShaderRead;
        _colorTarget =  _device.newTextureWithDescriptor(renderTargetDesc);

        // Set up a depth texture target.
        renderTargetDesc.pixelFormat = MTL::PixelFormatDepth32Float;
        renderTargetDesc.usage       = MTL::TextureUsageRenderTarget;
        MTL::Texture depthTarget   = _device.newTextureWithDescriptor(renderTargetDesc);

        // Set up the render pass descriptor with newly created textures.
        _renderPassDescriptor.colorAttachments[0].texture = _colorTarget;
        _renderPassDescriptor.depthAttachment.texture = depthTarget;
    }

    /// Called whenever view changes orientation or layout is changed
    void mtkView(MTKView* view, CGSize size)
    {
        // Update the aspect ratio and projection matrix since the view orientation or size has changed.
        self.updateProjectionMatrixWithSize(size);
        self.createRenderTargetsWithSize(size);
    }

    /// Called whenever the view needs to render
    void drawInMTKView(MTKView* view)
    {
        NS::UInteger frameDataBufferIndex = _frameNumber % AAPLMaxFramesInFlight;
        //dispatch_semaphore_wait(_inFlightSemaphore, DISPATCH_TIME_FOREVER);
        self.updateSceneState();

        // Render cube to offscreen texture
        MTL::CommandBuffer commandBuffer = _commandQueue.commandBuffer;
        commandBuffer.label = "Render CommandBuffer";

        MTL::RenderCommandEncoder renderEncoder = commandBuffer.renderCommandEncoderWithDescriptor(_renderPassDescriptor);
        // Render cube
        renderEncoder.label = "Render Encoder";
        renderEncoder.pushDebugGroup("Render Cube");

        renderEncoder.setFrontFacingWinding(MTL::WindingCounterClockwise);
        renderEncoder.setCullMode(MTL::CullModeBack);
        renderEncoder.setRenderPipelineState(_renderPipeline);
        renderEncoder.setDepthStencilState(_depthState);

        renderEncoder.setVertexBuffer(_positionBuffer,  0, AAPLBufferIndexMeshPositions);
        renderEncoder.setVertexBuffer(_texCoordBuffer,  0, AAPLBufferIndexMeshGenerics);
        renderEncoder.setVertexBuffer(_frameDataBuffer[frameDataBufferIndex],   0, AAPLBufferIndexFrameData);

        renderEncoder.setFragmentBuffer(_frameDataBuffer[frameDataBufferIndex], 0, AAPLBufferIndexFrameData);
        renderEncoder.setFragmentTexture(_colorMap, AAPLTextureIndexColorMap);
        renderEncoder.drawIndexedPrimitives(MTL::PrimitiveTypeTriangle, 36, MTL::IndexTypeUInt16, _indexBuffer, 0);

        renderEncoder.popDebugGroup();
        renderEncoder.endEncoding();
        commandBuffer.commit();
        

        // Use compute pipeline from function in dylib to process offscreen texture
        if(_computePipeline && view.currentDrawable)
        {
            MTL::CommandBuffer commandBuffer = _commandQueue.commandBuffer;
            commandBuffer.label = "Compute CommandBuffer";

            //__block dispatch_semaphore_t block_sema = _inFlightSemaphore;
            commandBuffer.addCompletedHandler(MTL::CommandBuffer buffer);
            //dispatch_semaphore_signal(block_sema);

            MTL::ComputeCommandEncoder computeEncoder = commandBuffer.computeCommandEncoder;
            computeEncoder.label = "Compute Encoder";
            computeEncoder.setComputePipelineState(_computePipeline);

            computeEncoder.setTexture(_colorTarget, AAPLTextureIndexComputeIn);
            computeEncoder.setTexture(view.currentDrawable.texture, AAPLTextureIndexComputeOut);
            computeEncoder.dispatchThreads(MTL::SizeMake(view.drawableSize.width, view.drawableSize.height, 1), MTL::SizeMake(16, 16, 1));
            computeEncoder.endEncoding();

            commandBuffer.presentDrawable(view.currentDrawable);
            commandBuffer.commit();
        }

        _frameNumber++;
    }

    /// Compile a dylib with the given program string then create a compute pipeline with the dylib
    void compileDylibWithString(NS::String* programString)
    {
        NS::Error *error;
        MTL::CompileOptions *options = new MTL::CompileOptions();
        options.libraryType = MTL::LibraryTypeDynamic;
        options.installName = NS::String::stringWithFormat("@executable_path/userCreatedDylib.metallib");

        MTL::Library lib = _device.newLibraryWithSource(programString, options, &error);
        if(!lib && error)
        {
            NS::Log("Error compiling library from source: %@", error);
            return;
        }
        
        MTL::DynamicLibrary dynamicLib = _device.newDynamicLibrary(lib, &error);
        if(!dynamicLib && error)
        {
            NS::Log("Error creating dynamic library from source library: %@", error);
            return;
        }
        
        MTL::ComputePipelineDescriptor *descriptor = new MTL::ComputePipelineDescriptor();
        descriptor.computeFunction = _baseDescriptor.computeFunction;
        descriptor.insertLibraries = dynamicLib;
        
        MTL::ComputePipelineState previousComputePipeline = _computePipeline;
        _computePipeline = _device.newComputePipelineStateWithDescriptor(descriptor, MTL::PipelineOptionNone, NULL, &error);
        if(!_computePipeline && error)
        {
            NS::Log("Error creating pipeline library from source library, using previous pipeline: %@", error);
            _computePipeline = previousComputePipeline;
            return;
        }
    }
        
    void deleteTextures(){
        MTL::RenderPipelineDescriptor pipelineStateDescriptor = new MTL::RenderPipelineDescriptor(); 
        pipelineStateDescriptor.label            = "Offscreen Render Pipeline Delete State";
        pipelineStateDescriptor.sampleCount      =  1;
        pipelineStateDescriptor.vertexFunction   =  defaultLibrary.newFunctionWithName("vertexShader");
        pipelineStateDescriptor.fragmentFunction =  defaultLibrary.newFunctionWithName("fragmentShader");
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = _renderTargetTexture.pixelFormat;

        _renderPipeline = _device.newRenderPipelineStateWithDescriptor(pipelineStateDescriptor, error);
        _renderPassDescriptor = new MTL::RenderPassDescriptor();
        _renderPassDescriptor.colorAttachments[0].clearColor = MTL::ClearColorMake(0,0,0,0);
        _renderPassDescriptor.colorAttachments[0].loadAction = MTL::LoadActionClear();

        MTL::RenderCommandEncoder renderEncoder = _commandQueue.commandBuffer.renderCommandEncoderWithDescriptor(_renderPassDescriptor);
        renderEncoder.label = "Offscreen Render Pass";
        renderEncoder.setRenderPipelineState(_renderPipeline);  
    }
}

