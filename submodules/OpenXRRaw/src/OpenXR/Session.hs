module OpenXR.Session where 

    import OpenXR.Types
    import OpenXR.StructureType
    import OpenXR.ViewConfigurations 

    -- Session [9] 
    -- Session lifecycle [9.1]
    data XrSession  

    -- Rendering  [10] 
    -- Swapchains [10.1]
    -- Runtimes should support R8G8B8A8 and R8G8B8A8 sRGB 
    -- formats. With OpenGL-based graphics APIs, the texture 
    -- formats correspond to OpenGL internal formats. With 
    -- Direct3D-based graphics APIs, xrEnumerateSwapchainFormats
    -- never returns typeless formats. Only concrete formats are 
    -- returned or may be specified by applications for swapchain 
    -- creation.
    data XrSwapchain = 
        XrSwapchain {
            swHandle  :: Int, 
            swSession :: XrSession 
        } deriving (Eq, Show)
      
    data VkImage           -- stand-in for VkImage, replace with Vulkan type when possible for all
    data VkDevice          -- stand-in for VkDevice
    data VkFormat
    data VkInstance        -- stand-in for VkInstance
    data VkPhysicalDevice  -- stand-in for VkPhysicalDevice

    instance Show XrSession        
    instance Show VkImage   
    instance Show VkDevice      
    instance Show VkFormat 
    instance Show VkInstance       
    instance Show VkPhysicalDevice 

    instance Eq XrSession        
    instance Eq VkImage    
    instance Eq VkFormat    
    instance Eq VkInstance       
    instance Eq VkDevice          
    instance Eq VkPhysicalDevice 

    data XrSessionState =
        XR_SESSION_STATE_UNKNOWN
        |XR_SESSION_STATE_IDLE
        |XR_SESSION_STATE_READY
        |XR_SESSION_STATE_SYNCHRONIZED
        |XR_SESSION_STATE_VISIBLE
        |XR_SESSION_STATE_FOCUSED
        |XR_SESSION_STATE_STOPPING
        |XR_SESSION_STATE_LOSS_PENDING
        |XR_SESSION_STATE_EXITING 
        |XR_SESSION_STATE_MAX_ENUM 
        deriving (Eq, Show)

    data XrSessionCreateInfo = 
        XrSessionCreateInfo {
            sciType        :: XrStructureType, -- const void* next; 
            sciCreateFlags :: XrSessionCreateFlags,
            sciSystemId    :: XrSystemId
        } deriving (Eq, Show)

    data XrGraphicsRequirementsVulkanKHR =
        XrGraphicsRequirementsVulkanKHR {
            grvType :: XrStructureType, --    void* next; 
            grvMinApiVersionSupported :: XrVersion,
            grvMaxApiVersionSupported :: XrVersion
        } deriving (Eq, Show)

    data XrSwapchainImageVulkanKHR =
        XrSwapchainImageVulkanKHR {
            swivType  :: XrStructureType, --    void* next; 
            swivImage :: VkImage
        } deriving (Eq, Show)

    data XrGraphicsBindingVulkanKHRI =
        XrGraphicsBindingVulkanKHRI {
            gbvkType             :: XrStructureType, --    const void* next; 
            gbvkInstance         :: VkInstance,
            gbvkPhysicalDevice   :: VkPhysicalDevice,
            gbvkDevice           :: VkDevice, 
            gbvkQueueFamilyIndex :: Int,
            gbvkQueueIndex       :: Int
        } deriving (Eq, Show)

    -- Session Control [9.2]
    -- XrResult xrBeginSession(XrSession session, 
    -- const XrSessionBeginInfo* beginInfo);
    data XrSessionBeginInfo =
        XrSessionBeginInfo {
            sbiType                         :: XrStructureType, --         const void* next; 
            sbiPrimaryViewConfigurationType :: XrViewConfigurationType
        } deriving (Eq, Show)

    -- Session States [9.3]
    data XrEventDataSessionStateChanged =
        XrEventDataSessionStateChanged {
            edsscType    :: XrStructureType, -- const void* next; 
            edsscSession :: XrSession,  
            edsscState   :: XrSessionState, 
            edsscTime    :: XrTime
        } deriving (Eq, Show)

