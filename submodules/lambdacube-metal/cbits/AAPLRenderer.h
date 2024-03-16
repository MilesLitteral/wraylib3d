/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the renderer class that performs Metal setup and per-frame rendering.
*/

#ifndef AAPLRenderer_h
#define AAPLRenderer_h

#include "MetalKit.h";
#include "mtlpp.h"

// Platform independent renderer class
class AAPLRenderer : NS::Object<MTK::ViewDelegate>{
    AAPLRenderer initWithMetalKitView(MTKView* mtkView);
    void compileDylibWithString(NS::String*    programString);
}

#endif /* AAPLRenderer_h */
