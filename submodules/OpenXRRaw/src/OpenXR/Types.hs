module OpenXR.Types where

    import Data.Int
    import OpenXR.StructureType

    -- Common types: 
    -- Boolean type [2.19]
    -- The only valid values are XR_TRUE or XR_FALSE.
    type    XrFlags64                       = Int
    type    XrBool32                        = Bool
    type    XrVersion                       = String
    type    XrTime                          = Double
    type    XrDuration                      = XrTime
    type    XrPath                          = FilePath
    type    XrSessionCreateFlags            = XrFlags64
    type    XrInstanceCreateFlags           = XrFlags64
    type    XrInputSourceLocalizedNameFlags = XrFlags64
    type    XrSwapchainCreateFlags          = XrFlags64
    type    XrSwapchainUsageFlags           = XrFlags64
    newtype XrSystemId                      = XrSystemId {u :: Int} deriving(Eq, Show)

    data Timespec = Timespec {sec :: !Int64, nsec :: !Int64} deriving (Eq, Show)

    -- Offsets, extents, and areas [2.16]
    -- Members indicate offset in meters if physical.
    data XrOffset2Df =
        XrOffset2Df {
            xfOff :: Float, 
            yfOff :: Float
        } deriving (Eq, Show)

    data XrOffset2Di = 
        XrOffset2Di {
            xiOff :: Int, 
            yiOff :: Int
        } deriving (Eq, Show)

    data XrQuaternionf =
        XrQuaternionf {
            x :: Float,
            y :: Float,
            z :: Float,
            w :: Float
        } deriving (Eq, Show)

    -- Members specify a rectangular area in meters if physical.
    data XrExtent2Df =
        XrExtent2Df {
            widthfEx  :: Float,
            heightfEx :: Float 
        } deriving (Eq, Show)

    data  XrExtent2Di =
        XrExtent2Di {
            widthiEx   :: Int,
            heightiEx  :: Int
        } deriving (Eq, Show)

    -- Members specify a rectangular area in meters if physical.

    data XrRect2Df =
        XrRect2Df {
            dfOffset :: XrOffset2Df,
            dfExtent :: XrExtent2Df
        } deriving (Eq, Show)

    data XrRect2Di =
        XrRect2Di {
            diOffset :: XrOffset2Di, 
            diExtent :: XrExtent2Di
        } deriving (Eq, Show)

    data XrVector2f =
        XrVector2f {
            v2fX :: Float,
            v2fY :: Float
        } deriving (Eq, Show)

    data XrVector3f =
        XrVector3f {
            v3fX :: Float,
            v3fY :: Float,
            v3fZ :: Float
        } deriving (Eq, Show)

    data XrColor4f =
        XrColor4f {
            r :: Float,
            g :: Float,
            b :: Float,
            a :: Float
        } deriving (Eq, Show)

    data XrPosef = 
        XrPosef {
            orientation :: XrQuaternionf,
            position    :: XrVector3f
        } deriving (Eq, Show)

    -- FOV angles [2.17]
    -- Angles are in radians from -π/2 to π/2.

    data XrFovf =
        XrFovf {
            angleLeft  :: Float,
            angleRight :: Float,
            angleUp    :: Float,
            angleDown  :: Float
        } deriving (Eq, Show)

    -- Event polling [2.20.1]
    -- The application is expected to allocate an event queue of 
    -- type XrEventDataBuffer and periodically call xrPollEvent. 
    -- If the event queue overflows, xrPollEvent will return the 
    -- XrEventDataEventsLost event

    data XrEventDataBuffer =
        XrEventDataBuffer {
            evdtype :: XrStructureType, --const void* next; 
            varying :: String
        } deriving (Eq, Show)

    data XrEventDataBaseHeader =
        XrEventDataBaseHeader {
            dbType :: XrStructureType --const void* next;
        } deriving (Eq, Show)

    data  XrEventDataEventsLost =
        XrEventDataEventsLost {
            evdltype :: XrStructureType, --const void* next; 
            lostEventCount :: Int
        } deriving (Eq, Show)

    data XrBaseInStructure = 
        XrBaseInStructure {
            xrInStructType :: XrStructureType,
            nextIn :: XrBaseInStructure
        } deriving (Eq, Show)

    data XrBaseOutStructure =
        XrBaseOutStructure {
            xrOutStructType  :: XrStructureType,
            nextOut :: XrBaseOutStructure
        } deriving (Eq, Show)

    -- #define XR_CURRENT_API_VERSION XR_MAKE_VERSION   (1, 0, 0)
    -- #define XR_MAKE_VERSION  (major, minor, patch)   ((((major) & 0xffffULL) << 48) | (((minor) & 0xffffULL) << 32) | ((patch) & 0xffffffffULL))
    -- #define XR_VERSION_MAJOR (version) (uint16_t)    (((uint64_t)(version) >> 48) & 0xffffULL)
    -- #define XR_VERSION_MINOR (version) (uint16_t)    (((uint64_t)(version) >> 32) & 0xffffULL)
    -- #define XR_VERSION_PATCH (version) (uint32_t)    ((uint64_t)(version) & 0xffffffffULL)

    -- Buffer size parameters [2.11]
    -- Some functions refer to input/output buffers with parameters 
    -- of the following form:
    -- XrResult xrFunction(uint32_t elementCapacityInput, uint32_t* elementCountOutput, float* elements);
    -- Two-call idiom for buffer size parameters
    -- First call xrFunction() with a valid elementCountOutput pointer 
    -- (always required), elements = NULL, and elementCapacityInput
    -- = 0 to get the number of elements in the buffer; allocate 
    -- sufficient space, then call xrFunction() again with the allocated 
    -- buffer's parameters

    data XrFormFactor =
        XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY
        |XR_FORM_FACTOR_HANDHELD_DISPLAY
        deriving (Eq, Show)
        
    data XrSystemGetInfo  =
        XrSystemGetInfo { 
            sysStType  :: XrStructureType, --next :: void, 
            formFactor :: XrFormFactor
        } deriving (Eq, Show)

    data XrSystemProperties =
        XrSystemProperties {
            sysType    :: XrStructureType, --next :: Void, 
            systemId   :: XrSystemId,  
            vendorId   :: Int,
            systemName :: String,
            graphicsProperties :: XrSystemGraphicsProperties,
            trackingProperties :: XrSystemTrackingProperties
        } deriving (Eq, Show)

    data XrSystemGraphicsProperties =
        XrSystemGraphicsProperties{
            maxSwapchainImageHeight :: Int, 
            maxSwapchainImageWidth  :: Int,
            maxLayerCount           :: Int
        } deriving (Eq, Show)

    data XrSystemTrackingProperties = 
        XrSystemTrackingProperties { 
            orientationTracking :: XrBool32,
            positionTracking    :: XrBool32
        } deriving (Eq, Show)

    data XR_USE_GRAPHICS =
        XR_USE_GRAPHICS_API_OPENGL
        | XR_USE_GRAPHICS_API_OPENGL_ES
        | XR_USE_GRAPHICS_API_VULKAN
        | XR_USE_GRAPHICS_API_D3D11
        | XR_USE_GRAPHICS_API_D3D12
        deriving (Eq, Show)

    data XR_USE_PLATFORM =
        XR_USE_PLATFORM_WIN32 
        | XR_USE_PLATFORM_XLIB 
        | XR_USE_PLATFORM_XCB 
        | XR_USE_PLATFORM_WAYLAND 
        | XR_USE_PLATFORM_ANDROID 
        deriving (Eq, Show)

    data XrVisibilityMaskTypeKHR  =
        XR_VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR
        |XR_VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR
        |XR_VISIBILITY_MASK_TYPE_LINE_LOOP_KHR
        |XR_VISIBILITY_MASK_TYPE_MAX_ENUM_KHR
        deriving (Eq, Show)

    data XrAndroidThreadTypeKHR =
        XR_ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR
        |XR_ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR
        |XR_ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR
        |XR_ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR
        |XR_ANDROID_THREAD_TYPE_MAX_ENUM_KHR
        deriving (Eq, Show)