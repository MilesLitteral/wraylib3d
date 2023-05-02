module OpenXR.ViewConfigurations where 

    import OpenXR.Types
    import OpenXR.StructureType
    
    -- View configurations [8]
    data XrViewConfiguration -- = XR_VIEW_CONFIGURATION_TYPE_PRIMARY_MONO | XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO

    data XrViewConfigurationType =
        XR_VIEW_CONFIGURATION_TYPE_PRIMARY_MONO 
        |XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO
        |XR_VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO --Provided by XR_VARJO_quad_views
        |XR_VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT --Provided by XR_MSFT_first_person_observer
        |XR_VIEW_CONFIGURATION_TYPE_MAX_ENUM
        deriving (Eq, Show)
        
    data XrViewConfigurationView =
        XrViewConfigurationView {
            vcvType :: XrStructureType, --void* next; 
            recommendedImageRectWidth       :: Int,
            maxImageRectWidth               :: Int,
            recommendedImageRectHeight      :: Int,
            maxImageRectHeight              :: Int,
            recommendedSwapchainSampleCount :: Int,
            maxSwapchainSampleCount         :: Int
        } deriving (Eq, Show)

    data XrViewConfigurationProperties =
        XrViewConfigurationProperties {
            vcpType                :: XrStructureType, --void* next; 
            vcpViewConfigurationType  :: XrViewConfigurationType, 
            vcpFovMutable             :: XrBool32
        } deriving (Eq, Show)