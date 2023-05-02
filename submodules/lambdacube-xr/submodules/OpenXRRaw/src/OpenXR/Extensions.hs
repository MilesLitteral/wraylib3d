-- £ Extensions [12]
-- Extension naming convention [2.6]
-- XR_KHR_* Khronos-created extensions supported by 
-- multiple vendors
-- XR_EXT_* extensions supported by multiple vendors, 
-- possibly IP-restricted
module OpenXR.Extensions where
    
    import OpenXR.Types
    import OpenXR.Session
    import OpenXR.StructureType
    import OpenXR.InstanceLifecycle
    import OpenXR.ViewConfigurations

    data HDC 
    data LUID
    data HGLRC
    data EGLConfig
    data EGLContext
    data EGLDisplay

    data GLXContext
    data GLXDrawable
    data GLXFBConfig
    data XR_MAY_ALIAS

    data XCBVisualId
    data XCBGlxContext
    data XCBConnection
    data XCBGlxDrawable
    data XCBGlxFbconfig

    data WL_Display
    data D3D_FEATURE_LEVEL
    data ID3D11Texture2D
    data ID3D11Device

    data ID3D12Device
    data ID3D12Resource
    data ID3D12CommandQueue

    data Display

    instance Show HDC 
    instance Show LUID
    instance Show HGLRC
    instance Show EGLConfig
    instance Show EGLContext
    instance Show EGLDisplay

    instance Eq HDC 
    instance Eq LUID
    instance Eq HGLRC
    instance Eq EGLConfig
    instance Eq EGLContext
    instance Eq EGLDisplay

    instance Show GLXContext
    instance Show GLXDrawable
    instance Show GLXFBConfig
    instance Show XR_MAY_ALIAS

    instance Eq GLXContext
    instance Eq GLXDrawable
    instance Eq GLXFBConfig
    instance Eq XR_MAY_ALIAS

    instance Show XCBVisualId
    instance Show XCBGlxContext
    instance Show XCBConnection
    instance Show XCBGlxDrawable
    instance Show XCBGlxFbconfig

    instance Eq XCBVisualId
    instance Eq XCBGlxContext
    instance Eq XCBConnection
    instance Eq XCBGlxDrawable
    instance Eq XCBGlxFbconfig

    instance Show WL_Display
    instance Show D3D_FEATURE_LEVEL
    instance Show ID3D11Texture2D
    instance Show ID3D11Device

    instance Eq WL_Display
    instance Eq D3D_FEATURE_LEVEL
    instance Eq ID3D11Texture2D
    instance Eq ID3D11Device

    instance Show ID3D12Device
    instance Show ID3D12Resource
    instance Show ID3D12CommandQueue

    instance Eq ID3D12Device
    instance Eq ID3D12Resource
    instance Eq ID3D12CommandQueue

    instance Eq   Display
    instance Show Display

    data XrVisibilityMaskKHR =
        XrVisibilityMaskKHR {
            vmType              :: XrStructureType, --void* next; 
            vertexCapacityInput :: Int, 
            vertexCountOutput   :: Int,
            vertices            :: XrVector2f,
            indexCapacityInput  :: Int,
            indexCountOutput    :: Int,
            indices             :: Int
        } deriving (Eq, Show)

    data XrEventDataVisibilityMaskChangedKHR =
        XrEventDataVisibilityMaskChangedKHR {
            edvmType                  :: XrStructureType,  --             const void* next; 
            session               :: XrSession,
            viewConfigurationType :: XrViewConfigurationType,
            viewIndex             :: Int
        } deriving (Eq, Show)-- XrEventDataVisibilityMaskChangedKHR;

    data XrGraphicsRequirementsD3D12KHR =
        XrGraphicsRequirementsD3D12KHR {
            d12Type            :: XrStructureType, -- void* next; 
            d12AdapterLuid     :: LUID,
            d12MinFeatureLevel :: D3D_FEATURE_LEVEL
        } deriving (Eq, Show)

    data XrGraphicsRequirementsD3D11KHR = 
        XrGraphicsRequirementsD3D11KHR {
            d11Type          :: XrStructureType, -- void* next; 
            d11AdapterLuid     :: LUID, 
            d11MinFeatureLevel :: D3D_FEATURE_LEVEL
        } deriving (Eq, Show)

    data XrGraphicsBindingD3D12KHR =
        XrGraphicsBindingD3D12KHR {
            gbd12Type :: XrStructureType,         --const void* next; 
            device    :: ID3D12Device,
            queue     :: ID3D12CommandQueue
        } deriving (Eq, Show)

    data XrGraphicsBindingD3D11KHR =
        XrGraphicsBindingD3D11KHR {
            gbd11Type :: XrStructureType, --const void*     next; 
            d11Device    :: ID3D11Device
        } deriving (Eq, Show)
        
    -- XR_KHR opengl_enable [12.14]
    -- Support theOpenGL graphics API in an OpenXR runtime.
    data XrGraphicsBindingOpenGLWin32KHR =
        XrGraphicsBindingOpenGLWin32KHR {
            glType  :: XrStructureType, -- const void* next; 
            hGLRC   :: HGLRC,
            hDC     :: HDC
        } deriving (Eq, Show)

    data XrGraphicsBindingOpenGLXlibKHR =
        XrGraphicsBindingOpenGLXlibKHR {
            glxType   :: XrStructureType, 
            glxNext        :: XR_MAY_ALIAS, 
            glxXDisplay    :: Display, 
            glxVisualid    :: Int,
            glxFBConfig :: GLXFBConfig, 
            glxDrawable :: GLXDrawable,
            glxContext  :: GLXContext
        } deriving (Eq, Show) 

    data XrGraphicsBindingOpenGLXcbKHR =
        XrGraphicsBindingOpenGLXcbKHR {
            glxcbType  :: XrStructureType, -- const void* next; 
            glxcbConnection   :: XCBConnection,
            glxcbScreenNumber :: Int,
            glxcbFbconfigid   :: XCBGlxFbconfig, 
            glxcbVisualid     :: XCBVisualId,
            glxcbDrawable  :: XCBGlxDrawable,
            glxcbContext   :: XCBGlxContext
        } deriving (Eq, Show) -- XrGraphicsBindingOpenGLXcbKHR;

    data XrGraphicsBindingOpenGLWaylandKHR =
        XrGraphicsBindingOpenGLWaylandKHR {
            wyldType    :: XrStructureType, -- const void* next; 
            wyldDisplay :: WL_Display
        } deriving (Eq, Show) -- XrGraphicsBindingOpenGLWaylandKHR;

    data XrGraphicsRequirementsOpenGLKHR =
        XrGraphicsRequirementsOpenGLKHR {
            oglkhrType :: XrStructureType, --        void* next; 
            oglkhrMinApiVersionSupported :: XrVersion,
            oglkhrMaxApiVersionSupported :: XrVersion
        } deriving (Eq, Show) -- XrGraphicsRequirementsOpenGLKHR;

    data XrGraphicsRequirementsOpenGLESKHR =
        XrGraphicsRequirementsOpenGLESKHR {
            glesType               :: XrStructureType,  --            void* next; 
            glesMinApiVersionSupported :: XrVersion,
            glesMaxApiVersionSupported :: XrVersion
        } deriving (Eq, Show) --XrGraphicsRequirementsOpenGLESKHR;

    -- XR_KHR_opengl_es_enable [12.15]
    -- Support the OpenGL ES graphics API in an OpenXR runtime.
    data XrGraphicsBindingOpenGLESAndroidKHR =
        XrGraphicsBindingOpenGLESAndroidKHR {
            glesAType    :: XrStructureType, --        const void* next; 
            eglDisplay :: EGLDisplay, 
            eglConfig  :: EGLConfig, 
            eglContext :: EGLContext
        } deriving (Eq, Show) --XrGraphicsBindingOpenGLESAndroidKHR;

    data XrSwapchainImageD3D11KHR =
        XrSwapchainImageD3D11KHR {
            swD11Type      :: XrStructureType,         --void* next; 
            d11Texture   :: ID3D11Texture2D
        } deriving (Eq, Show)

    data XrSwapchainImageD3D12KHR =
        XrSwapchainImageD3D12KHR {
            swD12Type    :: XrStructureType, -- void* next; 
            d12Texture   :: ID3D12Resource
        } deriving (Eq, Show)

    -- Extensions (continued)
    data XrSwapchainImageOpenGLKHR =
        XrSwapchainImageOpenGLKHR {
            swGLType  :: XrStructureType, -- void* next; 
            glImage :: Int
        } deriving (Eq, Show) -- XrSwapchainImageOpenGLKHR;

    data XrSwapchainImageOpenGLESKHR =
        XrSwapchainImageOpenGLESKHR {
            swGLESType  :: XrStructureType, --            void* next; 
            glesImage :: Int
        } deriving (Eq, Show) --XrSwapchainImageOpenGLESKHR;

    -- XR_KHR_vulkan_swapchain_format_list [12.18]
    -- This extension enables the following:

    xrGetVisibilityMaskKHR :: XrSession -> XrViewConfigurationType -> Int -> XrVisibilityMaskTypeKHR -> XrVisibilityMaskKHR -> XrResult
    xrGetVisibilityMaskKHR session viewConfigurationType viewIndex visibilityMaskType visibilityMask = undefined

    xrGetOpenGLGraphicsRequirementsKHR   :: XrInstance -> XrSystemId -> XrGraphicsRequirementsOpenGLKHR -> XrResult
    xrGetOpenGLGraphicsRequirementsKHR   xrInstance systemId graphicsRequirements   = undefined

    xrGetOpenGLESGraphicsRequirementsKHR :: XrInstance -> XrSystemId -> XrGraphicsRequirementsOpenGLESKHR -> XrResult
    xrGetOpenGLESGraphicsRequirementsKHR xrInstance systemId graphicsRequirements = undefined

    -- XR_KHR_D3D11_enable [12.11]
    -- Support the D3D 11 graphics API in an OpenXR runtime.

    xrGetD3D11GraphicsRequirementsKHR :: XrInstance -> XrSystemId -> XrGraphicsRequirementsD3D11KHR -> XrResult
    xrGetD3D11GraphicsRequirementsKHR xrInstance systemId graphicsRequirements   = undefined

    -- XR_KHR_vulkan_enable [12.17]
    -- Support the Vulkan graphics API in an OpenXR runtime in 
    -- addition to those functions and structs shown under Sessions 
    -- on page 6 of this reference guide.

    xrGetVulkanGraphicsDeviceKHR     :: XrInstance -> XrSystemId -> VkInstance -> VkPhysicalDevice -> XrResult
    xrGetVulkanGraphicsDeviceKHR xrInstance systemId vkInstance vkPhysicalDevice = undefined

    xrGetVulkanInstanceExtensionsKHR :: XrInstance -> XrSystemId -> Int -> Int -> String -> XrResult
    xrGetVulkanInstanceExtensionsKHR xrInstance systemId bufferCapacityInput bufferCountOutput buffer = undefined

    xrGetVulkanDeviceExtensionsKHR   :: XrInstance -> XrSystemId -> Int -> Int -> String -> XrResult
    xrGetVulkanDeviceExtensionsKHR xrInstance systemId bufferCapacityInput bufferCountOutput buffer  = undefined

    -- XR_KHR_win32_convert_performance_counter_time  -- LARGE_INTEGER
    xrConvertWin32PerformanceCounterToTimeKHR :: XrInstance -> Int   -> XrTime -> XrResult 
    xrConvertWin32PerformanceCounterToTimeKHR xrInstance performanceCounter time = undefined

    --LARGE_INTEGER = Int
    xrConvertTimeToWin32PerformanceCounterKHR :: XrInstance -> XrTime -> Int -> XrResult
    xrConvertTimeToWin32PerformanceCounterKHR xrInstance time performanceCounter = undefined

    -- [12.19]
    -- This extension enables the following:

    -- XR_KHR_convert_timespec_time [12.9]
    -- Enabling this extension makes the following available.
    xrConvertTimespecTimeToTimeKHR :: XrInstance -> Timespec -> XrTime   -> XrResult
    xrConvertTimespecTimeToTimeKHR xrInstance timespecTime time  = undefined

    xrConvertTimeToTimespecTimeKHR :: XrInstance -> XrTime   -> Timespec -> XrResult
    xrConvertTimeToTimespecTimeKHR xrInstance  timespecTime time = undefined

