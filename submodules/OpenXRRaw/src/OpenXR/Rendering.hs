module OpenXR.Rendering where

    import OpenXR.Types
    import OpenXR.Spaces
    import OpenXR.Session
    import OpenXR.Compositing
    import OpenXR.StructureType
    import OpenXR.InstanceLifecycle
    import OpenXR.ViewConfigurations

    data JObject

    data XrViewStateFlags =
        XR_VIEW_STATE_ORIENTATION_VALID_BIT
        |XR_VIEW_STATE_POSITION_VALID_BIT
        |XR_VIEW_STATE_ORIENTATION_TRACKED_BIT
        |XR_VIEW_STATE_POSITION_TRACKED_BIT
        deriving (Eq, Show)
        
    data XrEnvironmentBlendMode =
        XR_ENVIRONMENT_BLEND_MODE_OPAQUE
        |XR_ENVIRONMENT_BLEND_MODE_ADDITIVE
        |XR_ENVIRONMENT_BLEND_MODE_ALPHA_BLEND
        deriving (Eq, Show)

    data XrSwapchainCreate = XR_SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT | XR_SWAPCHAIN_CREATE_STATIC_IMAGE_BIT

    data XrSwapchainUseage = 
        XR_SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT
        |XR_SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        |XR_SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT 
        |XR_SWAPCHAIN_USAGE_TRANSFER_SRC_BIT
        |XR_SWAPCHAIN_USAGE_TRANSFER_DST_BIT 
        |XR_SWAPCHAIN_USAGE_SAMPLED_BIT
        |XR_SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT
        deriving (Eq, Show)

    -- data XrTypeSwapchainImageKHR =
    --     XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR
    --     |XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR
    --     |XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR
    --     |XR_TYPE_SWAPCHAIN_IMAGE_D3D11_KHR
    --     |XR_TYPE_SWAPCHAIN_IMAGE_D3D12_KHR
    --     deriving (Eq, Show)

    data XrSwapchainCreateInfo =
        XrSwapchainCreateInfo {
            swcType        :: XrStructureType, --const void* next; 
            swcCreateFlags :: XrSwapchainCreateFlags, 
            swcUsageFlags  :: XrSwapchainUsageFlags, 
            swcFormat      :: Int,
            swcSampleCount :: Int,
            swcWidth       :: Int,
            swcHeight      :: Int,
            swcFaceCount   :: Int,
            swcArraySize   :: Int,
            swcMipCount    :: Int
        } deriving (Eq, Show)

    -- sampleCount, width, height, mipcount: Must not be 0 
    -- faceCount: 6 (for cubemaps) or 1
    -- arraySize: Must not be 0: 1 is for a 2D image
    data XrSwapchainImageBaseHeader =
        XrSwapchainImageBaseHeader {
            imgType :: XrStructureType  -- void* next;
        } deriving (Eq, Show)

    data XrSwapchainImageAcquireInfo =
        XrSwapchainImageAcquireInfo {
            siaType :: XrStructureType  -- const void* next;
        } deriving (Eq, Show)

    data XrSwapchainImageWaitInfo =
        XrSwapchainImageWaitInfo {
            siwType :: XrStructureType, -- const void* next; 
            siwTimeout :: XrDuration
        } deriving (Eq, Show)

    data XrSwapchainImageReleaseInfo =
        XrSwapchainImageReleaseInfo {
            sirType :: XrStructureType  -- const void* next;
        } deriving (Eq, Show)

    -- £ [12.18] XR_KHR_vulkan_swapchain_format_list
    -- Enables the Vulkan VK_KHR_image_format_list extension.
    data XrVulkanSwapchainFormatListCreateInfoKHR =
        XrVulkanSwapchainFormatListCreateInfoKHR {
            vsflciType            :: XrStructureType, -- const void* next; 
            vsflciViewFormatCount :: Int, 
            vsflciViewFormats     :: VkFormat
        } deriving (Eq, Show)

    data XrViewLocateInfo =
        XrViewLocateInfo {
            vliType                  :: XrStructureType, -- const void* next; 
            vliViewConfigurationType :: XrViewConfigurationType,
            vliDisplayTime           :: XrTime, 
            vliSpace                 :: XrSpace
        } deriving (Eq, Show)

    data XrView =
        XrView {
            vType :: XrStructureType, -- void* next; 
            vPose :: XrPosef, 
            vFov  :: XrFovf
        } deriving (Eq, Show)

    data XrViewState =
        XrViewState {
            vsType           :: XrStructureType, -- void* next; 
            vsViewStateFlags :: XrViewStateFlags
        } deriving (Eq, Show)

    data XrFrameWaitInfo =
        XrFrameWaitInfo {
            fwType :: XrStructureType  --const void* next;
        } deriving (Eq, Show)

    data XrFrameState =
        XrFrameState {
            fsType                   :: XrStructureType, -- void* next; 
            fsPredictedDisplayTime   :: XrTime, 
            fsPredictedDisplayPeriod :: XrDuration,
            fsShouldRender           :: XrBool32
        } deriving (Eq, Show)

    data XrFrameBeginInfo =
        XrFrameBeginInfo {
            fbiType :: XrStructureType -- const void* next;
        } deriving (Eq, Show)

    data XrFrameEndInfo =
        XrFrameEndInfo {
            feiType                 :: XrStructureType, -- const void* next; 
            feiDisplayTime          :: XrTime,
            feiEnvironmentBlendMode :: XrEnvironmentBlendMode,
            feiLayerCount           :: Int,
            feiLayers               :: XrCompositionLayerBaseHeader
        } deriving (Eq, Show)

    -- £ layers: A pointer to an array of Projection and/or Quad 
    -- types, or optionally:
    -- [12.5] If XR_KHR_composition_layer_cube is enabled, then 
    -- struct XrCompositionLayerCubeKHR can be used.
    -- [12.6] If XR_KHR_composition_layer_cylinder is enabled, 
    -- then struct XrCompositionLayerCylinderKHR can be used. 
    -- [12.8] If XR_KHR_composition_layer_equirect is enabled, 
    -- then struct XrCompositionLayerEquirectKHR can be used.

    -- Environment Blend Mode [10.5.7]
    xrEnumerateEnvironmentBlendModes :: XrInstance -> XrSystemId -> XrViewConfigurationType -> Int -> Int -> XrEnvironmentBlendMode -> XrResult
    xrEnumerateEnvironmentBlendModes xrInstance systemId viewConfigurationType environmentBlendModeCapacityInput environmentBlendModeCountOutput environmentBlendModes = undefined

    xrEnumerateSwapchainFormats :: XrSession -> Int -> Int -> Int -> XrResult
    xrEnumerateSwapchainFormats session formatCapacityInput  formatCountOutputformats = undefined

    xrEnumerateSwapchainImages  :: XrSwapchain -> Int -> Int -> XrSwapchainImageBaseHeader -> XrResult
    xrEnumerateSwapchainImages swapchain imageCapacityInput imageCountOutput images = undefined

    xrCreateSwapchain  :: XrSession -> XrSwapchainCreateInfo -> XrSwapchain ->  XrResult
    xrCreateSwapchain session createInfo swapchain = undefined 

    xrDestroySwapchain :: XrSwapchain -> XrResult
    xrDestroySwapchain swapchain = undefined 
    
    xrAcquireSwapchainImage :: XrSwapchain -> XrSwapchainImageAcquireInfo -> Int -> XrResult
    xrAcquireSwapchainImage swapchain acquireInfo index = undefined

    xrReleaseSwapchainImage :: XrSwapchain -> XrSwapchainImageReleaseInfo -> XrResult
    xrReleaseSwapchainImage swapchain releaseInfo = undefined

    xrWaitSwapchainImage :: XrSwapchain -> XrSwapchainImageWaitInfo -> XrResult
    xrWaitSwapchainImage swapchain waitInfo = undefined

    -- Rendering (continued)
    -- £ [12.2] XR_KHR_android_surface_swapchain
    -- This extension enables the Android swapchain function:
    xrCreateSwapchainAndroidSurfaceKHR :: XrSession -> XrSwapchainCreateInfo -> XrSwapchain -> JObject -> XrResult
    xrCreateSwapchainAndroidSurfaceKHR session info swapchain surface = undefined 

    -- View and Projection State [10.2]
    xrLocateViews :: XrSession -> XrViewLocateInfo -> XrViewState -> Int -> Int -> XrView -> XrResult
    xrLocateViews session viewLocateInfo viewState viewCapacityInput viewCountOutput views = undefined

    -- Frame Waiting [10.4]
    xrWaitFrame   :: XrSession -> XrFrameWaitInfo -> XrFrameState -> XrResult
    xrWaitFrame session frameWaitInfo frameState = undefined 

    --Frame Submission [10.5]
    xrBeginFrame  :: XrSession -> XrFrameBeginInfo -> XrResult
    xrBeginFrame session frameBeginInfo = undefined
    
    xrEndFrame    :: XrSession -> XrFrameEndInfo -> XrResult
    xrEndFrame session frameEndInfo = undefined
