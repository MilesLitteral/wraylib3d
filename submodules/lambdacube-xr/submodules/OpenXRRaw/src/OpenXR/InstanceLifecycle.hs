module  OpenXR.InstanceLifecycle where 

    import OpenXR.Types
    import OpenXR.Session
    import OpenXR.StructureType
    import OpenXR.ViewConfigurations
    
    -- Instance lifecycle
    -- API layers and extensions [2.7, 4.1]
    -- API layers are inserted between the application and the 
    -- runtime to hook API calls for logging, debugging, validation, etc. 
    -- Extensions can expose new features or modify the behavior of 
    -- existing functions. Both extensions and API layers are selected 
    -- at XrInstance creation. To enable a layer, add its name to the 
    -- enabledApiLayerNames member of XrInstanceCreateInfo. To 
    -- enable an extension, add its name to the enabledExtensions
    -- member of XrInstanceCreateInfo.
    data XrInstance = XrInstance {
        handle :: Int,  
        ci     :: XrInstanceCreateInfo
    } deriving (Eq, Show)

    data XrApiLayerProperties =
        XrApiLayerProperties {
            apiType         :: XrStructureType, --        void* next; 
            apiLayerName    :: String,
            apiSpecVersion  :: XrVersion,
            apiLayerVersion :: Int,
            apiDescription  :: String
        } deriving (Eq, Show)

    data XrExtensionProperties = 
        XrExtensionProperties {
            extType             :: XrStructureType, -- void* next; 
            extExtensionName    :: String,
            extExtensionVersion :: Int
        } deriving (Eq, Show)

    -- Command function pointers [3.2]
    -- XrResult xrGetInstanceProcAddr(XrInstance xrInstance, 
    -- const char* name, PFN_xrVoidFunction* function);

    -- xrInstance lifecycle [4.2]
    -- Call xrCreateInstance to get an XrInstance handle. The xrInstance 
    -- manages the interface between the application and the OpenXR 
    -- runtime.
    data XrInstanceCreateInfo =
        XrInstanceCreateInfo {
            iciType :: XrStructureType, --const void* next; 
            iciCreateFlags           :: XrInstanceCreateFlags,
            iciApplicationInfo       :: XrApplicationInfo,
            iciEnabledApiLayerCount  :: Int, 
            iciEnabledApiLayerNames  :: String,
            iciEnabledExtensionCount :: Int,
            iciEnabledExtensionNames :: [String]
        } deriving (Eq, Show)

    --  createFlags must be 0
    data XrApplicationInfo =
        XrApplicationInfo { 
            applicationName    :: String,
            applicationVersion :: Int,
            engineName         :: String,
            engineVersion      :: Int,
            apiVersion         :: XrVersion
        } deriving (Eq, Show)

    -- XR_KHR_android_create_instance [12.1]
    -- This extension enables the following:
    data XrInstanceCreateInfoAndroidKHR =
        XrInstanceCreateInfoAndroidKHR { 
            iciaType                :: XrStructureType, --const void* next; 
            iciaApplicationVM       :: String, --void* 
            iciaApplicationActivity :: String -- void* 
        } deriving (Eq, Show)

    data XrInstanceProperties = 
        XrInstanceProperties { 
            ipType           :: XrStructureType, --    void* next; 
            ipRuntimeVersion :: XrVersion, 
            ipRuntimeName    :: String
        } deriving (Eq, Show)

    -- XrEventDataInstanceLossPending [4.4.2]
    -- Receiving this structure predicts a session loss at lossTime. The 
    -- application should call xrDestroyInstance and release xrInstance 
    -- resources.
    data XrEventDataInstanceLossPending =
        XrEventDataInstanceLossPending{ 
            edilpType     :: XrStructureType, --        const void* next; 
            edilpLossTime :: XrTime
        } deriving (Eq, Show)

    xrCreateInstance :: XrInstanceCreateInfo -> XrInstance -> XrResult
    xrCreateInstance createInfo xrInstance = undefined

    xrDestroyInstance :: XrInstance -> XrResult
    xrDestroyInstance xrInstance = undefined

    -- Instance information [4.3]
    -- Using Graphics APIs in runtimes
    -- Use extensions to enable access to OpenGL, OpenGL ES, Vulkan, 
    -- and Direct3D 11 and 12 graphics APIs. The extended functions 
    -- for using Vulkan are shown below. For others, see Extensions on 
    -- page 7 of this reference guide.
    
    xrGetInstanceProperties :: XrInstance -> XrInstanceProperties  -> XrResult
    xrGetInstanceProperties xrInstance instanceProperties = undefined

    xrEnumerateViewConfigurations :: XrInstance -> XrSystemId -> Int -> Int -> XrViewConfigurationType -> XrResult
    xrEnumerateViewConfigurations xrInstance systemId viewConfigurationTypeCapacityInput viewConfigurationTypeCountOutput viewConfigurationTypes = undefined

    xrGetViewConfigurationProperties :: XrInstance -> XrSystemId -> XrViewConfigurationType -> XrViewConfigurationProperties -> XrResult
    xrGetViewConfigurationProperties xrInstance systemId viewConfigurationType configurationProperties = undefined

    xrEnumerateViewConfigurationViews :: XrInstance -> XrSystemId -> XrViewConfigurationType -> Int -> Int -> XrViewConfigurationView -> XrResult 
    xrEnumerateViewConfigurationViews xrInstance systemId viewConfigurationType viewCapacityInput viewCountOutput views = undefined

    xrEnumerateApiLayerProperties :: Int -> Int -> XrApiLayerProperties -> XrResult
    xrEnumerateApiLayerProperties propertyCapacityInput propertyCountOutput properties = undefined

    xrEnumerateInstanceExtensionProperties :: String -> Int -> Int -> XrExtensionProperties -> XrResult
    xrEnumerateInstanceExtensionProperties layerName propertyCapacityInput propertyCountOutput properties = undefined

    -- £ [12.13] Enabled with XR_KHR_vulkan_enable
    xrGetVulkanGraphicsRequirementsKHR :: XrInstance -> XrSystemId -> XrGraphicsRequirementsVulkanKHR -> XrResult
    xrGetVulkanGraphicsRequirementsKHR xrInstance systemId graphicsRequirements = undefined

    -- OpenXR.Session Functions require both 
    xrCreateSession :: XrInstance -> XrSessionCreateInfo -> XrSession -> XrResult
    xrCreateSession xrInstance createInfo session = undefined

    xrEndSession :: XrSession -> XrResult
    xrEndSession session = undefined

    xrRequestExitSession :: XrSession -> XrResult
    xrRequestExitSession session = undefined
    