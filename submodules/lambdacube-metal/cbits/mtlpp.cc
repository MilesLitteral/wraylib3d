#include "mtlpp.h"

MetalEngine             createComputeInstance(mtlpp::Device device){
    return (new MetalEngine(device));
}

MetalRenderingEngine    createRenderingInstanceGPU(mtlpp::Device device){
    return (new MetalRenderingEngine(device));
}

MetalRenderingEngineCPU createRenderingInstanceCPU(mtlpp::Device device){
    return (new MetalRenderingEngineCPU(device));
}
