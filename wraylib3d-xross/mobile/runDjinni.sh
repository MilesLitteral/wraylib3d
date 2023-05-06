./djinni/src/run \
    --java-out JAVA_OUTPUT_FOLDER \
    --java-package com.example.jnigenpackage \
    --java-cpp-exception DbxException \  #Choose between a customized C++ exception in Java and java.lang.RuntimeException (the default).
    --ident-java-field mFooBar \         #Optional, this adds an "m" in front of Java field names
    \
    --cpp-out CPP_OUTPUT_FOLDER \
    \
    --jni-out JNI_OUTPUT_FOLDER \
    --ident-jni-class NativeFooBar \     # This adds a "Native" prefix to JNI class
    --ident-jni-file NativeFooBar \      # This adds a prefix to the JNI filenames otherwise the cpp and jni filenames are the same.
    \
    --objc-out OBJC_OUTPUT_FOLDER \
    --objc-type-prefix DB \              # Apple suggests Objective-C classes have a prefix for each defined type.
    \
    --objcpp-out OBJC_OUTPUT_FOLDER \
    \
    --idl MY_PROJECT.djinni
