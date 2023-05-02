/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of the cross-platform text editing view controller.
*/

#include "AAPLEditViewController.h"
#include <string.h> 

class AAPLEditViewController
{
    ns::String *_dylibString;
    double spaceToBottomValueStartValue;
    volatile ns::Object NSLayoutConstraint *spaceToBottomLayoutGuide;
};

void viewDidLoad()
{
    //[super viewDidLoad];
    
    ns::Error *error;
    ns::String *pathToFunction = NSBundle::mainBundle("AAPLUserCompiledFunction.metal");
    ns::String _dylibString = pathToFunction; //encoding:NSUTF8StringEncoding, error:&error

    printf("%@ Failed to load kernel string from file: %@", _dylibString, error);

#if defined(TARGET_IOS)
    _textView.text = _dylibString;
    spaceToBottomValueStartValue = spaceToBottomLayoutGuide.constant;
    registerForKeyboardNotifications(this);
#else
    _textView.setString(_dylibString);
#endif
}

/// When the "Compile" button is clicked the source for this dynamic library from the text view
/// will be compiled on the fly and inserted via insertLibraries.
#if defined(TARGET_IOS)
IBAction onClick(UIButton* sender)
{
    _textView.resignFirstResponder();
    _renderer.compileDylibWithString(_textView.text);
}

/// Handle keyboard notifications
void registerForKeyboardNotifications()
{
    ns::NSNotificationCenter defaultCenter(addObserver:self, keyboardWasShown(selector:@selector), UIKeyboardDidShowNotification(NULL);
    
    ns::NSNotificationCenter defaultCenter(addObserver:self, keyboardWillBeHidden(selector:@selector), name:UIKeyboardWillHideNotification object:nil];
}

void keyboardWasShown(ns::NSNotification* aNotification)
{
    NSDictionary* info = aNotification.userInfo();
    CGSize keyboardSize = info.objectForKey(UIKeyboardFrameEndUserInfoKey, CGRectValue.size);
    spaceToBottomLayoutGuide.constant = keyboardSize.height;
}

void keyboardWillBeHidden(ns::NSNotification* aNotification)
{
    spaceToBottomLayoutGuide.constant = spaceToBottomValueStartValue;
}

IBAction onClick(NSButton * sender)
{
    _renderer.compileDylibWithString(_textView);
}

#endif
