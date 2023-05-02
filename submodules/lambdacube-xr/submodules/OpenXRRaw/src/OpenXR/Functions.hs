module OpenXR.Functions where
    
    import OpenXR.Types
    import OpenXR.Session
    import OpenXR.StructureType
    import OpenXR.InstanceLifecycle

    xR_TRUE  :: XrBool32
    xR_TRUE = True

    xR_FALSE :: XrBool32
    xR_FALSE = True

    -- | Getting the XrSystemID [5.1-2]
    -- A return of XR_ERROR_FORM_FACTOR_UNAVAILABLE indicates 
    -- the form factor is supported but temporarily unavailable; the 
    -- application may retry xrGetSystem.
    xrGetSystem :: XrInstance -> XrSystemGetInfo -> XrSystemId -> XrResult 
    xrGetSystem xrInstance getInfo systemId = undefined 

    -- Getting system properties [5.3]
    xrGetSystemProperties :: XrInstance -> XrSystemId  -> XrSystemProperties -> XrResult
    xrGetSystemProperties xrInstance systemId properties = undefined 

    -- Special values
    xr_NO_DURATION :: Int
    xr_NO_DURATION = 0

    xr_INFINITE_DURATION :: String
    xr_INFINITE_DURATION = "0x7fffffffffffffffLL"

    xrVersion :: XrVersion
    xrVersion = "1.0.0"

    xrDuration :: XrTime -> XrTime -> XrTime
    xrDuration start end = (start - end) 

    -- Type to string conversions [4.5]
    xrResultToString :: XrInstance -> XrResult -> String -> XrResult
    xrResultToString xrInstance value buffer = undefined

    xrStructureTypeToString :: XrInstance -> XrStructureType -> String -> XrResult
    xrStructureTypeToString xrInstance value buffer = undefined

    -- Path to string conversions [4.5]
    xrStringToPath :: XrInstance -> String -> XrPath -> XrResult
    xrStringToPath xrInstance pathString path = undefined

    xrPathToString :: XrInstance -> XrPath -> Int -> String -> XrResult 
    xrPathToString xrInstance path bufferCapacityInput buffer = undefined

    xrPollEvent    :: XrInstance -> XrEventDataBuffer -> XrResult
    xrPollEvent xrInstance eventData = undefined

    -- XR_KHR_android_thread_settings [12.3]
    -- If enabled, this extension allows the application to specify 
    -- the Android thread type.
    xrSetAndroidApplicationThreadKHR :: XrSession -> XrAndroidThreadTypeKHR -> Int -> XrResult 
    xrSetAndroidApplicationThreadKHR session threadType threadId = undefined 
