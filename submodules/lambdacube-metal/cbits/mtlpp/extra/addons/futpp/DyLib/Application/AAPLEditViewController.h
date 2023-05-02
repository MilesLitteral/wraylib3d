/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for the cross-platform text editing view controller.
*/

#if defined(TARGET_IOS)
#include <UIKit/UIKit.h>
#define PlatformViewController UIViewController
#else
#include <AppKit/AppKit.h>
#define PlatformViewController NSViewController
#endif

#include <MetalKit/MetalKit.h>
#include "../Renderer/AAPLRenderer.h"

class AAPLEditViewController : PlatformViewController
{
    public:
    #if defined(TARGET_IOS)
    UITextView *textView;
    #else
    NSTextView *textView;
    #endif

    AAPLRenderer *renderer;
};
