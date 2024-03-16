#include "mtlpp.h"


const unsigned int arrayLength = 10; //1 << 24;
const unsigned int bufferSize = arrayLength * sizeof(float);

//xcrun -sdk macosx metal -c add.metal -o add.air
//xcrun -sdk macosx metallib add.air -o add.metallib

//xcrun -sdk macosx metal -c operations.metal -o operations.air
//xcrun -sdk macosx metallib operations.air -o operations.metallib

class MetalEngine {
    MTLDevice _mDevice = MTLDevice::CreateSystemDefaultDevice();
    MTLComputePipelineState _mFunctionPSO;
    MTLCommandQueue _mCommandQueue;
    NS::Error* error; //nullptr

    MetalEngine();
    MetalEngine(MTLDevice device);
    MetalEngine(NS::String libraryPath, NS::String mtlFunction, MTLDevice device);
    MetalEngine(NS::String mtlFunction, MTLDevice device);
    MetalEngine(const char src[], NS::String functionName, MTLDevice device);
    void generateRandomFloatData(MTLBuffer buffer);
    void commitRandomFloatData(MTLBuffer   buffer, char* floatData);
    void prepareData(MTLDevice device);
    void sendComputeCommand(MTLCommandQueue commandQueue);
    //void encodeCommand(mtlpp::ComputeCommandEncoder computeEncoder);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer, MTLBuffer buffer2);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer, MTLBuffer buffer2, MTLBuffer buffer3);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffers[], MTLBuffer bufferR);
    void execute();
    void verifyResults();
};

class MetalRenderingEngine {
    MTLDevice _mDevice = MTLDevice::CreateSystemDefaultDevice();
    MTLComputePipelineState _mFunctionPSO;
    MTLCommandQueue _mCommandQueue;
    // mtlpp::Buffer _mBufferA;
    // mtlpp::Buffer _mBufferB;
    // mtlpp::Buffer _mBufferResult;
    ns::Error* error; //nullptr

    MetalRenderingEngine();
    MetalRenderingEngine(MTLDevice device);
    MetalRenderingEngine(NS::String libraryPath, NS::String mtlFunction, MTLDevice device);
    MetalRenderingEngine(NS::String mtlFunction, MTLDevice device);
    MetalRenderingEngine(const char src[], NS::String functionName, MTLDevice device);

    void initializeRenderBuffer();
    void prepareData(MTLDevice device);
    void sendComputeCommand(MTLCommandQueue commandQueue);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer, MTLBuffer buffer2);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffer, MTLBuffer buffer2, MTLBuffer buffer3);
    void encodeCommand(MTLComputeCommandEncoder computeEncoder, MTLBuffer buffers[], MTLBuffer bufferR);
    void verifyResults();
    void execute();
};

class MetalRenderingEngineCPU {
    MTLIndirectCommandBuffer _indirectCommandBuffer;

    // #if TARGET_IOS
    //     supportICB = Device(); //[_view.device supportsFeatureSet:MTLFeatureSet_iOS_GPUFamily3_v4];
    // #else
    //     supportICB = Device(); //[_view.device supportsFeatureSet:MTLFeatureSet_macOS_GPUFamily2_v1];
    // #endif

    #if defined TARGET_MACOS || defined(__IPHONE_13_0)
            // Indicate that the render pipeline state object will be set in the render command encoder
            // (not by the indirect command buffer).
            // On iOS, this property only exists on iOS 13 and later.  It defaults to YES in earlier
            // versions
            if (@available(iOS 13.0, *)) {
                icbDescriptor.inheritPipelineState = YES;
            }
    #endif

    MetalRenderingEngineCPU();
    MetalRenderingEngineCPU(MTLDevice device);

    void encodeDrawCommand (int vertexStart);
    void encodeBlitCommands();
    void executeICB();
};
