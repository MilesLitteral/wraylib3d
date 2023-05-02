module OpenXR.Spaces where

    import OpenXR.Types
    import OpenXR.Session
    import OpenXR.InputActions 
    import OpenXR.StructureType

    data XrSpace
    data XrVelocityFlags = XR_SPACE_VELOCITY_LINEAR_VALID_BIT | XR_SPACE_VELOCITY_ANGULAR_VALID_BIT
    type XrSpaceLocationFlags = XrFlags64
    type XrSpaceVelocityFlags = XrFlags64

    instance Eq   XrSpace 
    instance Show XrSpace 

    data XrLocationFlags =
        XR_SPACE_LOCATION_ORIENTATION_VALID_BIT
        |XR_SPACE_LOCATION_POSITION_VALID_BIT
        |XR_SPACE_LOCATION_ORIENTATION_TRACKED_BIT
        |XR_SPACE_LOCATION_POSITION_TRACKED_BIT
        deriving (Eq, Show)

    data XrReferenceSpaceType =
        XR_REFERENCE_SPACE_TYPE_VIEW
        |XR_REFERENCE_SPACE_TYPE_LOCAL
        |XR_REFERENCE_SPACE_TYPE_STAGE
        deriving (Eq, Show)

    data XrReferenceSpaceCreateInfo = 
        XrReferenceSpaceCreateInfo {
            rsciType                 :: XrStructureType, -- const void* next; 
            rsciReferenceSpaceType   :: XrReferenceSpaceType, 
            rsciPoseInReferenceSpace :: XrPosef
        } deriving (Eq, Show)

    -- An XrEventDataReferenceSpaceChangePending event is sent 
    -- to the application when the origin (and possibly bounds) of a 
    -- reference space is changing:

    data XrEventDataReferenceSpaceChangePending =
        XrEventDataReferenceSpaceChangePending {
            edrscpType                :: XrStructureType, -- const void* next; 
            edrscpSession             :: XrSession, 
            edrscpReferenceSpaceType  :: XrReferenceSpaceType,
            edrscpChangeTime          :: XrTime, 
            edrscpPoseValid           :: XrBool32,
            edrscpPoseInPreviousSpace :: XrPosef
        } deriving (Eq, Show)

    -- Action spaces [7.2]
    -- An XrSpace handle for a pose action is created using 
    -- xrCreateActionSpace, by specifying the chosen pose action 
    -- and an optional transform from its natural origin. Examples of 
    -- well-known pose action paths:
    -- /user/hand/left/input/grip
    -- /user/hand/left/input/aim
    -- /user/hand/right/input/grip
    -- /user/hand/right/input/aim

    data XrActionSpaceCreateInfo =
        XrActionSpaceCreateInfo {
            asciType              :: XrStructureType, -- const void* next; 
            asciAction            :: XrAction, 
            asciSubactionPath     :: XrPath, 
            asciPoseInActionSpace :: XrPosef
        } deriving (Eq, Show)

    data XrSpaceLocation =
        XrSpaceLocation {
            slType          :: XrStructureType,  -- void* next; 
            slLocationFlags :: XrSpaceLocationFlags,
            slPose          :: XrPosef
        } deriving (Eq, Show)

    -- XrSpaceVelocity may be passed in using the next chain of 
    -- XrSpaceLocation to determine the velocity.

    data XrSpaceVelocity =
        XrSpaceVelocity {
            svType            :: XrStructureType, -- void* next; 
            svVelocityFlags   :: XrSpaceVelocityFlags, 
            svLinearVelocity  :: XrVector3f, 
            svAngularVelocity :: XrVector3f
        } deriving (Eq, Show)

    xrCreateActionSpace :: XrSession  -> XrActionSpaceCreateInfo  ->  XrSpace -> XrResult
    xrCreateActionSpace session createInfo space = undefined

    -- Working with spaces [7.3]
    xrDestroySpace :: XrSpace -> XrResult
    xrDestroySpace space = undefined

    xrLocateSpace :: XrSpace -> XrSpace -> XrTime -> XrSpaceLocation -> XrResult
    xrLocateSpace space baseSpace time location = undefined

    -- Reference spaces [7.1]
    xrCreateReferenceSpace :: XrSession -> XrReferenceSpaceCreateInfo -> XrSpace -> XrResult
    xrCreateReferenceSpace session createInfo space = undefined

    xrEnumerateReferenceSpaces :: XrSession -> Int -> Int -> XrReferenceSpaceType -> XrResult
    xrEnumerateReferenceSpaces session spaceCapacityInput spaceCountOutput spaces = undefined
    
    xrGetReferenceSpaceBoundsRect :: XrSession -> XrReferenceSpaceType -> XrExtent2Df  -> XrResult
    xrGetReferenceSpaceBoundsRect session referenceSpaceType bounds = undefined
