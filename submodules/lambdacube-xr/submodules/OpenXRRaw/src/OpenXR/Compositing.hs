module OpenXR.Compositing where 
        
    import OpenXR.Types
    import OpenXR.Spaces
    import OpenXR.Session
    import OpenXR.StructureType

    -- Compositing
    -- Compositing [10.5]
    -- Composition layers are submitted by the application via the 
    -- xrEndFrame call. All composition layers to be drawn must be 
    -- submitted with every xrEndFrame call. Composition layers 
    -- are drawn in the same order as they are specified in via 
    -- XrFrameEndInfo, with the 0th layer drawn first.
        
    data XrEyeVisibility         = XR_EYE_VISIBILITY_BOTH | XR_EYE_VISIBILITY_LEFT | XR_EYE_VISIBILITY_RIGHT
    data XrCompositionLayerFlags = XR_COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT | XR_COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT

    instance Eq   XrEyeVisibility 
    instance Show XrEyeVisibility
    instance Eq   XrCompositionLayerFlags 
    instance Show XrCompositionLayerFlags 

    data XrSwapchainSubImage =
        XrSwapchainSubImage {
            swapchain       :: XrSwapchain, 
            imageRect       :: XrRect2Di, 
            imageArrayIndex :: Int
        } deriving (Eq, Show)
        
    -- next: NULL or a pointer to an extension-specific structure:
    -- £ XrCompositionLayerColorModulationInfoKHR if the 
    -- XR_KHR_composition_layer_color_modulation extension 
    -- is enabled; 
    -- or
    -- £ XrCompositionLayerDepthInfoKHR if 
    -- XR_KHR_composition_layer_depth is enabled 

    data XrCompositionLayerBaseHeader =
        XrCompositionLayerBaseHeader {
            compType   :: XrStructureType, 
            layerFlags :: XrCompositionLayerFlags,
            space      :: XrSpace 
        } deriving (Eq, Show)

    data XrCompositionLayerProjection =
        XrCompositionLayerProjection {
            compLayerProjtype   :: XrStructureType, --const void* next; 
            compLayerLayerFlags :: XrCompositionLayerFlags,
            compLayerSpace      :: XrSpace, 
            compLayerViews      :: XrCompositionLayerProjectionView,
            compLayerViewCount  :: Int
        } deriving (Eq, Show)

    data XrCompositionLayerProjectionView =
        XrCompositionLayerProjectionView{
            clpvType     :: XrStructureType,  --const void* next; 
            clpvPose     :: XrPosef, 
            clpvFov      :: XrFovf, 
            clpvSubImage :: XrSwapchainSubImage
        } deriving (Eq, Show)

    -- £ XR_KHR_composition_layer_cube [12.25]
    -- This extension adds an additional layer type that enables direct 
    -- sampling from cubemaps.
    data XrCompositionLayerCubeKHR =
        XrCompositionLayerCubeKHR {
            clcuType            :: XrStructureType, --const void* next; 
            clcuLayerFlags      :: XrCompositionLayerFlags, 
            clcuSpace           :: XrSpace, 
            clcuEyeVisibility   :: XrEyeVisibility,
            clcuSwapchain       :: XrSwapchain, 
            clcuOrientation     :: XrQuaternionf,
            clcuImageArrayIndex :: Int
        } deriving (Eq, Show)

    -- £ XR_KHR_composition_layer_cylinder [12.6]
    -- This extension adds an additional layer type where the XR 
    -- runtime must map a texture stemming from a swapchain onto 
    -- the inside of a cylinder section. It can be imagined much the 
    -- same way a curved television display looks to a viewer.
    data XrCompositionLayerCylinderKHR =
        XrCompositionLayerCylinderKHR {
            clckType       :: XrStructureType,  --const void* next; 
            clckLayerFlags    :: XrCompositionLayerFlags,
            clckSpace         :: XrSpace, 
            clckEyeVisibility :: XrEyeVisibility, 
            clckSubImage      :: XrSwapchainSubImage, 
            clckPose          :: XrPosef, 
            clckRadius        :: Float,
            clckCentralAngle  :: Float,
            clckAspectRatio   :: Float
        } deriving (Eq, Show)

    -- £ XR_KHR_composition_layer_equirect [13.6]
    -- This extension adds an additional layer type where the XR 
    -- runtime must map an equirectangular coded image stemming 
    -- from a swapchain onto the inside of a sphere.
    data XrCompositionLayerEquirectKHR =
        XrCompositionLayerEquirectKHR {
            cleType          :: XrStructureType, --const void* next; 
            cleLayerFlags    :: XrCompositionLayerFlags, 
            cleSpace         :: XrSpace, 
            cleEyeVisibility :: XrEyeVisibility, 
            cleSubImage      :: XrSwapchainSubImage, 
            clePose          :: XrPosef,
            cleScale         :: XrVector2f, 
            cleBias          :: XrVector2f,
            cleRadius        :: Float
        } deriving (Eq, Show)

    data XrCompositionLayerColorModulationInfoKHR =
        XrCompositionLayerColorModulationInfoKHR {
            clcmType    :: XrStructureType,         -- const void* next; 
            colorScale  :: XrColor4f, 
            colorOffset :: XrColor4f
        } deriving (Eq, Show)

    data XrCompositionLayerDepthInfoKHR =
        XrCompositionLayerDepthInfoKHR {
            cldType     :: XrStructureType,         --const void* next; 
            cldSubImage    :: XrSwapchainSubImage, 
            cldMinDepth    :: Float, 
            cldMaxDepth    :: Float, 
            cldNearZ       :: Float, 
            cldFarZ        :: Float 
        } deriving (Eq, Show)

    data XrCompositionLayerQuad =
        XrCompositionLayerQuad {
            clqType         :: XrStructureType,  -- const void* next; 
            clLayerFlags    :: XrCompositionLayerFlags,
            clSpace         :: XrSpace,
            clEyeVisibility :: XrEyeVisibility,
            clSubImage      :: XrSwapchainSubImage, 
            clPose          :: XrPosef, 
            clSize          :: XrExtent2Df
        } deriving (Eq, Show)
