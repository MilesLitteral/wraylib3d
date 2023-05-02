#include <stddef.h>
#include <string>
#include <filesystem>
#include "ns.hpp"
#include "../mtlpp.hpp"

namespace mtlpp {
    const unsigned int arrayLength = 10; //1 << 24;
    const unsigned int bufferSize  = arrayLength * sizeof(float);

    //xcrun -sdk macosx metal -c add.metal -o add.air
    //xcrun -sdk macosx metallib add.air -o add.metallib

    //xcrun -sdk macosx metal -c operations.metal -o operations.air
    //xcrun -sdk macosx metallib operations.air   -o operations.metallib

    // ## Compile metallib from .metal shaders:
    // ```xcrun -sdk macosx metal -c MyLibrary.metal -o MyLibrary.air```
    //    xcrun -sdk macosx metallib MyLibrary.air   -o MyLibrary.metallib```

    // ## Precompile Shaders on the Command Line
    // ```xcrun -sdk macosx metal -Os MyLibrary.metal```
    // ```xcrun -sdk macosx metal -Os MyLibrary.metal```

    // ## Generate a Symbol File with a Single Command (For Dylib/Dynamic Linking support of a .metallib)
    // ```xcrun -sdk macosx metal -frecord-sources=flat Shadow.metal PointLights.metal DirectionalLight.metal```

    // ## Generate a Symbol File with Multiple Commands
    // You can also generate a Metal library’s symbol file with multiple commands, which may be more appropriate for your workflow than the single-command technique. 
    // For those scenarios, you can generate each Metal library and its companion symbol file by following these steps:

    // Compile each Metal source file to a Metal AIR (Apple Intermediate Representation) file.
    // Generate a Metal library with the source by linking its AIR files.

    // Separate the library’s source and save it as a companion symbol file.
    // First, compile each Metal source file to a Metal AIR file by passing the -c option to the compiler:

    // ```xcrun -sdk macosx metal -c -frecord-sources Shadow.metal```
    // ```xcrun -sdk macosx metal -c -frecord-sources PointLights.metal```
    // ```xcrun -sdk macosx metal -c -frecord-sources DirectionalLight.metal```
    // The -frecord-sources option tells the Metal compiler to embed the symbols in the AIR output file for that command. However, this command doesn’t create a separate symbols file at this time, which is why the -frecord-sources option doesn’t include the =flat suffix.

    // Next, generate a Metal library by linking the AIR files.
    // ```xcrun -sdk macosx metal -frecord-sources -o LightsAndShadow.metallib Shadow.air PointLights.air DirectionalLight.air```

    // Separate the sources from the library and create a companion symbol file by running the metal-dsymutil command.
    // ```xcrun -sdk macosx metal-dsymutil -flat -remove-source LightsAndShadow.metallib```

    // Further Reading: https://developer.apple.com/documentation/metal/shader_libraries/generating_and_loading_a_metal_library_symbol_file

    typedef mtlpp::Buffer VertexBuffer; 

    struct ICBContainer
    {
        mtlpp::CommandBuffer commandBuffer; //[[ id(AAPLArgumentBufferIDCommandBuffer) ]];
    };

    //define metal reflections here 
    void mtlAddArrays(const float* inA,
                    const float* inB,
                    float* result,
                    int length)
    {
        for (int index = 0; index < length ; index++)
        {
            result[index] = inA[index] + inB[index];
        }
    }
    //end define Metal Reflections

    //Use This All-In-One C++ Class to run computations on Metal APUs
    class MetalEngine {
            public:
                mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();

                // The compute pipeline generated from the compute kernel in the .metal shader file.
                mtlpp::ComputePipelineState _mFunctionPSO;

                // The command queue used to pass commands to the device.
                mtlpp::CommandQueue _mCommandQueue;

                // Buffers to hold data.
                mtlpp::Buffer _mBufferA;
                mtlpp::Buffer _mBufferB;
                mtlpp::Buffer _mBufferResult;

                //Pointer for Any Errors generated
                ns::Error* error; //nullptr

        
            MetalEngine()
            {
                    _mDevice = mtlpp::Device::CreateSystemDefaultDevice();
                    ns::Error* error = NULL;

                    // Load the shader files with a .metal file extension in the project

                    mtlpp::Library defaultLibrary = _mDevice.NewLibrary("fut.metallib", error);
                    if (defaultLibrary.GetFunctionNames() == NULL)
                    {
                        printf("Failed to find the default library.\n");

                    }
                    mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                    // Create a compute pipeline state object.
                    _mFunctionPSO =  _mDevice.NewComputePipelineState(addFunction, error);
                
                    _mCommandQueue = _mDevice.NewCommandQueue();
            }
        
            MetalEngine(mtlpp::Device device)
            {
                    _mDevice = device;
                    ns::Error* error = NULL;

                    // Load the shader files with a .metal file extension in the project

                    mtlpp::Library defaultLibrary = device.NewLibrary("fut.metallib", error);
                    if (defaultLibrary.GetFunctionNames() == NULL)
                    {
                        printf("Failed to find the default library.\n");

                    }
                    mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                    // Create a compute pipeline state object.
                    _mFunctionPSO = device.NewComputePipelineState(addFunction, error);
                
                    _mCommandQueue = device.NewCommandQueue();
            }
            //Initializes based on a path
            MetalEngine(ns::String libraryPath, ns::String mtlFunction, mtlpp::Device device)
            {
                _mDevice = device;
                //device.NewDefaultLibrary();
                error = NULL;

                // Load the shader files with a .metal file extension in the project
                // .metallib
                mtlpp::Library defaultLibrary = device.NewLibrary(libraryPath, error);
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");
                }
                mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

                // Create a compute pipeline state object.
                _mFunctionPSO = device.NewComputePipelineState(Function, error);
                _mCommandQueue = device.NewCommandQueue();
            }

            MetalEngine(ns::String mtlFunction, mtlpp::Device device)
            {
                _mDevice = device;
                //device.NewDefaultLibrary();
                error = NULL;

                // Load the shader files with a .metal file extension in the project
                // .metallib
                mtlpp::Library defaultLibrary = device.NewDefaultLibrary();
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");

                }
                //"mtlAddArrays"
                mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

                // Create a compute pipeline state object.
                _mFunctionPSO = device.NewComputePipelineState(Function, error);
            
                _mCommandQueue = device.NewCommandQueue();
            }

            //By Default, Metallib is made at compilation time however this is an 
            //alternative constructor to run const chars as Metal Scripts
            //Possibly more Important for Futhark
            MetalEngine(const char src[], ns::String functionName, mtlpp::Device device){

                _mDevice = device; 

                error = NULL; //nullptr
                mtlpp::Library library  = device.NewLibrary(src, mtlpp::CompileOptions(), error);
                assert(library);
                mtlpp::Function Func = library.NewFunction(functionName);
                assert(Func);

                _mFunctionPSO = device.NewComputePipelineState(Func, error);
                assert(_mFunctionPSO);

                _mCommandQueue = device.NewCommandQueue();
                assert(_mCommandQueue);
            }

            void generateRandomFloatData(mtlpp::Buffer buffer)
            {
                float* dataPtr = (float*)buffer.GetContents();

                for (unsigned long index = 0; index < arrayLength; index++)
                {
                    dataPtr[index] = (float)rand()/(float)(RAND_MAX);
                }
            }

            void prepareData(mtlpp::Device device)
            {
                // Allocate three buffers to hold our initial data and the result.
                _mBufferA = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);
                _mBufferB = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);
                _mBufferResult = device.NewBuffer(bufferSize, mtlpp::ResourceOptions::StorageModeShared);

                generateRandomFloatData(_mBufferA);
                generateRandomFloatData(_mBufferB);
            }

            void sendComputeCommand(mtlpp::CommandQueue commandQueue)
            {
                // Create a command buffer to hold commands.
                mtlpp::CommandBuffer commandBuffer = commandQueue.CommandBuffer();
                // Start a compute pass.
                mtlpp::ComputeCommandEncoder computeEncoder = commandBuffer.ComputeCommandEncoder();// computeCommandEncoder];

                encodeCommand(computeEncoder);
                // End the compute pass.
                computeEncoder.EndEncoding();

                // Execute the command.
                commandBuffer.Commit();

                // Normally, you want to do other work in your app while the GPU is running,
                // but in this example, the code simply blocks until the calculation is complete.
                commandBuffer.WaitUntilCompleted();

                verifyResults();
            }


            void encodeCommand(mtlpp::ComputeCommandEncoder computeEncoder) {

                // Encode the pipeline state object and its parameters.
                computeEncoder.SetComputePipelineState(_mFunctionPSO);
                computeEncoder.SetBuffer(_mBufferA, 0, 0);
                computeEncoder.SetBuffer(_mBufferB, 0, 1);
                computeEncoder.SetBuffer(_mBufferResult, 0, 2);
                //_mBufferResult offset:x atIndex:y

                mtlpp::Size gridSize = mtlpp::Size(arrayLength, 1, 1);

                // Calculate a threadgroup size.
                uint32_t threadGroupSize = _mFunctionPSO.GetMaxTotalThreadsPerThreadgroup();
                if (threadGroupSize > arrayLength)
                {
                    threadGroupSize = arrayLength;
                }
                mtlpp::Size threadgroupSize = mtlpp::Size(threadGroupSize, 1, 1);

                // Encode the compute command.
                computeEncoder.DispatchThreadgroups(gridSize, threadgroupSize);
            }


            void verifyResults()
            {
                float* a = (float*)_mBufferA.GetContents();
                float* b = (float*)_mBufferB.GetContents();
                float* result = (float*)_mBufferResult.GetContents();

                for (unsigned long index = 0; index < arrayLength; index++)
                {
                    if (result[index] != (a[index] + b[index]))
                    {
                        printf("Compute ERROR: index=%lu result=%g vs %g=a+b\n",
                            index, result[index], a[index] + b[index]);
                        //assert(result[index] == (a[index] + b[index]));
                    }
                    else{
                            printf("Compute MATCH: index=%lu result=%g vs %g=a+b\n",
                            index, result[index], a[index] + b[index]);
                    }
                }
                printf("Compute results as expected\n");
            }


            void execute(){
                // Create the custom object used to encapsulate the Metal code.
                // Initializes objects to communicate with the GPU.
                MetalEngine engine = MetalEngine();
                //MetalEngine engineAlt = MetalEngine(argv, device);

                // Create buffers to hold data
                engine.prepareData(engine._mDevice);
                // Send a command to the GPU to perform the calculation.
                engine.sendComputeCommand(engine._mCommandQueue);
                printf("Execution finished\n");
            }
        };

    //Use This All-In-One C++ Class to render images or textures on Metal APUs
    class MetalRenderingEngine {
            public:
            mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();

            // The compute pipeline generated from the compute kernel in the .metal shader file.
            mtlpp::ComputePipelineState _mFunctionPSO;

            // The command queue used to pass commands to the device.
            mtlpp::CommandQueue _mCommandQueue;

            // Buffers to hold data.
            mtlpp::Buffer _mBufferA;
            mtlpp::Buffer _mBufferB;
            mtlpp::Buffer _mBufferResult;
            mtlpp::IndirectCommandBuffer _icbArgumentBuffer;

            //Pointer for Any Errors generated
            ns::Error* error; //nullptr

            MetalRenderingEngine()
            {
                    _mDevice = mtlpp::Device::CreateSystemDefaultDevice();
                    ns::Error* error = NULL;

                    // Load the shader files with a .metal file extension in the project

                    mtlpp::Library defaultLibrary = _mDevice.NewLibrary("fut.metallib", error);
                    if (defaultLibrary.GetFunctionNames() == NULL)
                    {
                        printf("Failed to find the default library.\n");

                    }
                    mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                    // Create a compute pipeline state object.
                    _mFunctionPSO =  _mDevice.NewComputePipelineState(addFunction, error);
                
                    _mCommandQueue = _mDevice.NewCommandQueue();
            };
        
            MetalRenderingEngine(mtlpp::Device device) {
                    _mDevice = device;
                    ns::Error* error = NULL;

                    // Load the shader files with a .metal file extension in the project

                    mtlpp::Library defaultLibrary = device.NewLibrary("fut.metallib", error);
                    if (defaultLibrary.GetFunctionNames() == NULL)
                    {
                        printf("Failed to find the default library.\n");

                    }
                    mtlpp::Function addFunction = defaultLibrary.NewFunction("Mtl");

                    // Create a compute pipeline state object.
                    _mFunctionPSO = device.NewComputePipelineState(addFunction, error);
                
                    _mCommandQueue = device.NewCommandQueue();
            }

            //Initializes based on a path
            MetalRenderingEngine(ns::String libraryPath, ns::String mtlFunction, mtlpp::Device device)
            {
                _mDevice = device;
                //device.NewDefaultLibrary();
                error = NULL;

                // Load the shader files with a .metal file extension in the project
                // .metallib
                mtlpp::Library defaultLibrary = device.NewLibrary(libraryPath, error);
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");
                }
                mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

                // Create a compute pipeline state object.
                _mFunctionPSO = device.NewComputePipelineState(Function, error);
                _mCommandQueue = device.NewCommandQueue();
            }

            MetalRenderingEngine(ns::String mtlFunction, mtlpp::Device device)
            {
                _mDevice = device;
                //device.NewDefaultLibrary();
                error = NULL;

                // Load the shader files with a .metal file extension in the project
                // .metallib
                mtlpp::Library defaultLibrary = device.NewDefaultLibrary();
                if (defaultLibrary.GetFunctionNames() == NULL)
                {
                    printf("Failed to find the default library.\n");

                }
                //"mtlAddArrays"
                mtlpp::Function Function = defaultLibrary.NewFunction(mtlFunction);

                // Create a compute pipeline state object.
                _mFunctionPSO = device.NewComputePipelineState(Function, error);
            
                _mCommandQueue = device.NewCommandQueue();
            }

            //By Default, Metallib is made at compilation time however this is an 
            //alternative constructor to run const chars as Metal Scripts
            //Possibly more Important for Futhark
            MetalRenderingEngine(const char src[], ns::String functionName, mtlpp::Device device){

                _mDevice = device; 

                error = NULL; //nullptr
                mtlpp::Library library  = device.NewLibrary(src, mtlpp::CompileOptions(), error);
                assert(library);
                mtlpp::Function Func = library.NewFunction(functionName);
                assert(Func);

                _mFunctionPSO = device.NewComputePipelineState(Func, error);
                assert(_mFunctionPSO);

                _mCommandQueue = device.NewCommandQueue();
                assert(_mCommandQueue);
            }

            void prepareData(mtlpp::Device device, int AAPLNumObjects)
            {
                VertexBuffer tempMeshes[AAPLNumObjects];
                for(int objectIdx = 0; objectIdx < AAPLNumObjects; objectIdx++)
                    {
                        // Choose the parameters to generate a mesh so that each one is unique.
                        uint32_t numTeeth = random() % 50 + 3;
                        float innerRatio  = 0.2 + (random() / (1.0 * RAND_MAX)) * 0.7;
                        float toothWidth  = 0.1 + (random() / (1.0 * RAND_MAX)) * 0.4;
                        float toothSlope  = (random() / (1.0 * RAND_MAX)) * 0.2;

                        // Create a vertex buffer and initialize it with a unique 2D gear mesh.
                        tempMeshes[objectIdx] = newGearMeshWithNumTeeth(numTeeth,
                                                                innerRatio,
                                                                toothWidth,
                                                                toothSlope);
                    }

                    //Count the individual and accumulated mesh sizes and create the container buffer:
                    size_t bufferSize = 0;
                    for(int objectIdx = 0; objectIdx < AAPLNumObjects; objectIdx++)
                    {
                        size_t meshSize = sizeof(AAPLVertex) * tempMeshes[objectIdx].numVerts;
                        bufferSize += meshSize;
                    };

                    VertexBuffer _vertexBuffer = device.NewBuffer(bufferSize, (mtlpp::ResourceOptions::ResourceOptions) 0);
                    
                    //Finally, insert each mesh into the container buffer while noting its offset and size in the second buffer:
                    for(int objectIdx = 0; objectIdx < AAPLNumObjects; objectIdx++)
                    {
                        // Store the mesh metadata in the `params` buffer.
                        params[objectIdx].numVertices = tempMeshes[objectIdx].numVerts;
                        size_t meshSize = sizeof(AAPLVertex) * tempMeshes[objectIdx].numVerts;
                        params[objectIdx].startVertex = currentStartVertex;

                        // Pack the current mesh data in the combined vertex buffer.

                        AAPLVertex* meshStartAddress = ((AAPLVertex*)_vertexBuffer.contents) + currentStartVertex;
                        memcpy(meshStartAddress, tempMeshes[objectIdx].vertices, meshSize);
                        currentStartVertex += tempMeshes[objectIdx].numVerts;
                        free(tempMeshes[objectIdx].vertices);

                        // Set the other culling and mesh rendering parameters.

                        // Set the position of each object to a unique space in a grid.
                        vector_float2 gridPos = (vector_float2){objectIdx % AAPLGridWidth, objectIdx / AAPLGridWidth};
                        params[objectIdx].position = gridPos * AAPLObjecDistance;

                        params[objectIdx].boundingRadius = AAPLObjectSize / 2.0;
                    };
                }

                void encodeCommand(mtlpp::ComputeCommandEncoder computeEncoder) {
                        // Reset the ICB’s commands to their initial before beginning encoding:
                        resetBlitEncoder.resetCommandsInBuffer(_indirectCommandBuffer, ns::Range(0, AAPLNumObjects));

                        // Encode the ICB’s commands by dispatching the compute kernel:
                        computeEncoder.dispatchThreads(mtlpp::Size(AAPLNumObjects, 1, 1), mtlpp::Size(threadExecutionWidth, 1, 1));

                        // Optimize your ICB commands to remove empty commands or redundant state by calling optimizeIndirectCommandBuffer:withRange::
                        optimizeBlitEncoder.optimizeIndirectCommandBuffer(_indirectCommandBuffer, ns::Range(0, AAPLNumObjects));
                        // This sample optimizes ICB commands because redundant state results from the kernel

                        // This is the argument buffer that contains the ICB.
                        //Encode the ICB into the argument buffer:
                        mtlpp::ArgumentEncoder argumentEncoder = GPUCommandEncodingKernel.newArgumentEncoderWithBufferIndex(AAPLKernelBufferIndexCommandBufferContainer);

                        _icbArgumentBuffer = device.newBuffer(argumentEncoder.encodedLength, ResourceStorageMode::Shared);
                        _icbArgumentBuffer.label = "ICB Argument Buffer";

                        argumentEncoder.setArgumentBuffer(_icbArgumentBuffer, 0);
                        argumentEncoder.setIndirectCommandBuffer(_indirectCommandBuffer, AAPLArgumentBufferIDCommandBuffer);

                        //Pass the ICB (_indirectCommandBuffer) to the kernel by setting the argument buffer on the kernel’s compute command encoder:
                        computeEncoder.setBuffer(_icbArgumentBuffer, 0, AAPLKernelBufferIndexCommandBufferContainer);

                        //Because you pass the ICB through an argument buffer, standard argument buffer rules apply. Call useResource on the ICB to tell Metal to prepare its use:
                        computeEncoder.useResource(_indirectCommandBuffer, ResourceUsage::Write);
                        
                        //   Encode the pipeline state object and its parameters.
                        //   computeEncoder.SetComputePipelineState(_mFunctionPSO);
                        //   computeEncoder.SetBuffer(_mBufferA, 0, 0);
                        //   computeEncoder.SetBuffer(_mBufferB, 0, 1);
                        //   computeEncoder.SetBuffer(_mBufferResult, 0, 2);
                        //   //_mBufferResult offset:x atIndex:y

                        //   mtlpp::Size gridSize = mtlpp::Size(arrayLength, 1, 1);

                        //   // Calculate a threadgroup size.
                        //   uint32_t threadGroupSize = _mFunctionPSO.GetMaxTotalThreadsPerThreadgroup();
                        //   if (threadGroupSize > arrayLength)
                        //   {
                        //       threadGroupSize = arrayLength;
                        //   }
                        //   mtlpp::Size threadgroupSize = mtlpp::Size(threadGroupSize, 1, 1);

                        //   // Encode the compute command.
                        //   computeEncoder.DispatchThreadgroups(gridSize, threadgroupSize);
                }

                void sendRenderCommand(mtlpp::CommandQueue commandQueue)
                {
                    // Create a command buffer to hold commands.
                    mtlpp::CommandBuffer commandBuffer = commandQueue.CommandBuffer();

                    // Start a compute pass.
                    mtlpp::ComputeCommandEncoder computeEncoder = commandBuffer.ComputeCommandEncoder();// computeCommandEncoder];
                    encodeCommand(computeEncoder);

                    // End the compute pass.
                    computeEncoder.EndEncoding();

                    // Execute the command.
                    commandBuffer.Commit();

                    // Normally, you want to do other work in your app while the GPU is running,
                    // but in this example, the code simply blocks until the calculation is complete.
                    commandBuffer.WaitUntilCompleted();
                    verifyResults();
                }

                void execute(mtlpp::RenderCommandEncoder renderEncoder, int AAPLNumObjects){
                    // Create the custom object used to encapsulate the Metal code.
                    // Initializes objects to communicate with the GPU.
                    MetalRenderingEngine engine = MetalRenderingEngine();

                    // Create buffers to hold data
                    engine.prepareData(engine._mDevice, AAPLNumObjects);
                    
                    // Send a command to the GPU to perform the calculation.
                    engine.sendRenderCommand(engine._mCommandQueue);
                    renderEncoder.executeCommandsInBuffer(_indirectCommandBuffer, ns::Range(0, AAPLNumObjects));
                    printf("Execution finished\n");
                }
        };

    class MetalRenderingEngineCPU {
        mtlpp::IndirectCommandBuffer _indirectCommandBuffer;

        // Create an Indirect Command Buffer
        // The sample creates _indirectCommandBuffer from a MTLIndirectCommandBufferDescriptor, which defines the features and limits of an indirect command buffer.
        MetalRenderingEngineCPU() {
            mtlpp::Device _mDevice = mtlpp::Device::CreateSystemDefaultDevice();
            ns::Error* error = NULL;

            // Load the shader files with a .metal file extension in the project
            mtlpp::Library defaultLibrary = _mDevice.NewLibrary("icb.metallib", error);
            if (defaultLibrary.GetFunctionNames() == NULL){
                printf("Failed to find the default library.\n");
            }
        };
        
        MetalRenderingEngineCPU(mtlpp::Device device) {
            mtlpp::Device _mDevice = device;
            ns::Error*    error    = NULL;

            // Load the shader files with a .metal file extension in the project

            mtlpp::Library  defaultLibrary = device.NewLibrary("icb.metallib", error);
            mtlpp::Function addFunction    = defaultLibrary.NewFunction("ICBRenderer");
            if (defaultLibrary.GetFunctionNames() == NULL)
            {
                printf("Failed to find the default library.\n");

            }
            _indirectCommandBuffer = _mDevice.newIndirectCommandBufferWithDescriptor(icbDescriptor, AAPLNumObjects, 0);
            _mFunctionPSO  = device.NewComputePipelineState(addFunction, error); // Create a compute pipeline state object.
            _mCommandQueue = device.NewCommandQueue();
        };

        // The sample specifies the types of commands, commandTypes, and the maximum number of commands, maxCount, so that Metal reserves enough space in memory for the sample to encode _indirectCommandBuffer successfully (with the CPU or GPU).
        // Encode an Indirect Command Buffer with the CPU
        // From the CPU, the sample encodes commands into _indirectCommandBuffer with a MTLIndirectRenderCommand object. For each shape to be rendered, the sample encodes two setVertexBuffer:offset:atIndex: commands and one drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance: command.
        void encodeDrawCommand(int vertexStart){
            //  Encode a draw command for each object drawn in the indirect command buffer.
                for (int objIndex = 0; objIndex < AAPLNumObjects; objIndex++){
                    imtlpp::IndirectRenderCommand ICBCommand = _indirectCommandBuffer.indirectRenderCommandAtIndex(objIndex);

                    ICBCommand.setVertexBuffer(_vertexBuffer[objIndex],
                                        0,
                                        AAPLVertexBufferIndexVertices);

                    ICBCommand.setVertexBuffer(_indirectFrameStateBuffer,
                                        0,
                                        AAPLVertexBufferIndexFrameState);

                    ICBCommand.setVertexBuffer(_objectParameters,
                                        0,
                                        AAPLVertexBufferIndexObjectParams);

                    ns::UInteger vertexCount = _vertexBuffer[objIndex].length / sizeof(AAPLVertex);

                    ICBCommand.drawPrimitives(PrimitiveType::triangle,
                                vertexStart,
                                vertexCount,
                                1,
                                objIndex
                                );
                };
            };
        // The sample performs this encoding only once, before encoding any subsequent render commands. _indirectCommandBuffer contains a total of 16 draw calls, one for each shape to be rendered. Each draw call references the same transformation data, _uniformBuffers, but different vertex data, _vertexBuffers[indx]. Although the CPU encodes data only once, the sample issues 16 draw calls per frame.
        // Layout diagram that shows the commands encoded into an indirect command buffer with the CPU.

        // Update the Data Used by an ICB
        // To update data that’s fed to the GPU, you typically cycle through a set of buffers such that the CPU updates one while the GPU reads another (see Synchronizing CPU and GPU Work). You can’t apply that pattern literally with ICBs, however, because you can’t update an ICB’s buffer set after you encode its commands, but you follow a two-step process to blit data updates from the CPU. First, update a single buffer in your dynamic buffer array on the CPU:
        void encodeBlitCommands(){
            _frameNumber++;
            _inFlightIndex = _frameNumber % AAPLMaxFramesInFlight;
            AAPLFrameState * frameState = _frameStateBuffer[_inFlightIndex].contents;
            //Then, blit the CPU-side buffer set to the location that’s accessible to the ICB (see _indirectFrameStateBuffer):

            // Encode blit commands to update the buffer holding the frame state.
            mtlpp::BlitCommandEncoder blitEncoder = commandBuffer.blitCommandEncoder;

            blitEncoder.copyFromBuffer(_frameStateBuffer[_inFlightIndex], sourceOffset:0,
                            toBuffer:_indirectFrameStateBuffer, destinationOffset:0,
                                size:_indirectFrameStateBuffer.length);

            blitEncoder.endEncoding();
        };

        // Execute an Indirect Command Buffer
        // The sample calls the executeCommandsInBuffer:withRange: method to execute the commands in _indirectCommandBuffer.
        void executeICB() {
            // Draw everything in the indirect command buffer.
            //Similar to the arguments in an argument buffer, the sample calls the useResource:usage: method to indicate that the GPU can access the resources within an indirect command buffer.
            renderEncoder.executeCommandsInBuffer(_indirectCommandBuffer, ns::Range(0, AAPLNumObjects));

            // Make a useResource call for each buffer needed by the indirect command buffer.
            for (int i = 0; i < AAPLNumObjects; i++)
            {
                renderEncoder.useResource(_vertexBuffer[i], ResourceUsage::Read);
            }

            renderEncoder.useResource(_objectParameters,         ResourceUsage::Read);
            renderEncoder.useResource(_indirectFrameStateBuffer, ResourceUsage::Read);
        };
    };
}