-- Input and Haptics: Actions 
-- Actions are created at initialization time and later used to 
-- request input device state, create action spaces, or control 
-- haptic events.
module OpenXR.InputActions where

    import OpenXR.Types
    import OpenXR.Session
    import OpenXR.StructureType
    import OpenXR.InstanceLifecycle

    data XrAction =
        XR_ACTION_TYPE_BOOLEAN_INPUT
        |XR_ACTION_TYPE_FLOAT_INPUT
        |XR_ACTION_TYPE_VECTOR2F_INPUT
        |XR_ACTION_TYPE_POSE_INPUT
        |XR_ACTION_TYPE_VIBRATION_OUTPUT
        deriving(Eq, Show)

    data XrActionSet
    instance Eq   XrActionSet
    instance Show XrActionSet 
    
    data XrInputSource =
        XR_INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT
        |XR_INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT
        |XR_INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT
        deriving(Eq, Show)

    data XrActionSetCreateInfo =
        XrActionSetCreateInfo {
            asType                 :: XrStructureType, --    const void* next; 
            actionSetName          :: String, 
            localizedActionSetName :: String,
            priority               :: Int
        } deriving(Eq, Show)

    data XrActionCreateInfo = 
        XrActionCreateInfo {
            acType              :: XrStructureType, --        const void* next; 
            actionName          :: String,
            actionType          :: XrAction,
            countSubactionPaths :: Int,
            subactionPaths      :: XrPath, 
            localizedActionName :: String
        } deriving(Eq, Show)

    data XrInteractionProfileSuggestedBinding =
        XrInteractionProfileSuggestedBinding {
            ipsType                   :: XrStructureType, -- const void* next;  
            interactionProfile     :: XrPath, 
            countSuggestedBindings :: Int,
            suggestedBindings      :: XrActionSuggestedBinding
        } deriving(Eq, Show)

    data XrActionSuggestedBinding =
        XrActionSuggestedBinding {
            action  :: XrAction,
            binding :: XrPath
        } deriving(Eq, Show)

    -- Inputs and Haptics: Actions (continued)
    data XrEventDataInteractionProfileChanged =
        XrEventDataInteractionProfileChanged {
            ediType    :: XrStructureType, --         const void* next; 
            ediSession :: XrSession
        } deriving(Eq, Show)

    data XrSessionActionSetsAttachInfo =
        XrSessionActionSetsAttachInfo {
            sasaType            :: XrStructureType, --    const void* next; 
            sasaCountActionSets :: Int,
            sasaActionSets      :: XrActionSet
        } deriving(Eq, Show)

    data XrInteractionProfileInfo =
        XrInteractionProfileInfo {
            ipiType               :: XrStructureType, -- const void* next; 
            ipiInteractionProfile :: XrPath
        } deriving(Eq, Show)

    -- Reading Input Action State [11.5]
    data XrActionStateGetInfo =
        XrActionStateGetInfo {
            asgiType          :: XrStructureType, --    const void* next; 
            asgiAction        :: XrAction, 
            asgiSubactionPath :: XrPath
        } deriving(Eq, Show)

    data XrHapticActionInfo =
        XrHapticActionInfo {
            haiType          :: XrStructureType, --     const void* next; 
            haiAction        :: XrAction, 
            haiSubactionPath :: XrPath
        } deriving(Eq, Show)

    data XrActionStateBoolean =
        XrActionStateBoolean {
            asbType                 :: XrStructureType, --    void* next; 
            asbCurrentState         :: XrBool32,
            asbChangedSinceLastSync :: XrBool32,
            asbLastChangeTime       :: XrTime,
            asbIsActive             :: XrBool32
        } deriving(Eq, Show)

    data XrActionStateFloat =
        XrActionStateFloat {
            asfType                 :: XrStructureType, -- void* next; 
            asfCurrentState         :: Float, 
            asfChangedSinceLastSync :: XrBool32, 
            asfLastChangeTime       :: XrTime,
            asfIsActive             :: XrBool32
        } deriving(Eq, Show)

    data XrActionStateVector2f =
        XrActionStateVector2f{ 
            asv2fType :: XrStructureType, --void* next; 
            asv2fCurrentState         :: XrVector2f, 
            asv2fChangedSinceLastSync :: XrBool32, 
            asv2fLastChangeTime       :: XrTime,
            asv2fIsActive             :: XrBool32
        } deriving(Eq, Show)

    data XrActionStatePose =
        XrActionStatePose {
            aspType     :: XrStructureType,  -- void* next; 
            aspIsActive :: XrBool32
        } deriving(Eq, Show)

    data XrHapticBaseHeader =
        XrHapticBaseHeader {
            hbhType :: XrStructureType -- const void* next;
        } deriving(Eq, Show)

    data XrHapticVibration =
        XrHapticVibration {
            hvType      :: XrStructureType, --    const void* next; 
            hvDuration  :: XrDuration, 
            hvFrequency :: Float, 
            hvAmplitude :: Float
        } deriving(Eq, Show)

    data XrActionsSyncInfo =
        XrActionsSyncInfo {
            asiType :: XrStructureType, --const void* next; 
            asiCountActiveActionSets :: Int,
            asiActiveActionSets      :: XrActiveActionSet
        } deriving(Eq, Show)

    data XrActiveActionSet =
        XrActiveActionSet { 
            activeActionSet     :: XrActionSet,
            activeSubactionPath :: XrPath
        } deriving(Eq, Show)

    data XrBoundSourcesForActionEnumerateInfo =
        XrBoundSourcesForActionEnumerateInfo {
            bsfaeType   :: XrStructureType, --    const void* next; 
            bsfaeAction :: XrAction
        } deriving(Eq, Show)

    data XrInputSourceLocalizedNameGetInfo =
        XrInputSourceLocalizedNameGetInfo {
            islType         :: XrStructureType, --     const void* next; 
            sourcePath      :: XrPath, 
            whichComponents :: XrInputSourceLocalizedNameFlags
        } deriving(Eq, Show)

    -- whichComponents: A bitwise OR of 
    -- XR_INPUT_SOURCE_LOCALIZED_NAME_X_BIT where 
    -- X may be: USER_PATH, INTERACTION_PROFILE, 
    -- COMPONENT

    -- Actions [11.3]
    xrCreateAction :: XrActionSet -> XrActionCreateInfo -> XrAction -> XrResult
    xrCreateAction actionSet createInfo action = undefined 

    -- Action sets [11.2]
    xrCreateActionSet :: XrInstance -> XrActionSetCreateInfo -> XrActionSet -> XrResult
    xrCreateActionSet xrInstance createInfo actionSet = undefined

    xrDestroyAction :: XrAction -> XrResult
    xrDestroyAction action = undefined

    xrDestroyActionSet :: XrActionSet -> XrResult
    xrDestroyActionSet actionSet = undefined 

    -- Action Sources [11.8]
    xrEnumerateBoundSourcesForAction :: XrSession -> XrBoundSourcesForActionEnumerateInfo -> Int -> Int -> XrPath -> XrResult
    xrEnumerateBoundSourcesForAction session enumerateInfo sourceCapacityInput sourceCountOutput sources = undefined

    xrGetInputSourceLocalizedName :: XrSession -> XrInputSourceLocalizedNameGetInfo -> Int -> Int -> String -> XrResult 
    xrGetInputSourceLocalizedName session getInfo bufferCapacityInput bufferCountOutput buffer = undefined

    xrGetCurrentInteractionProfile :: XrSession -> XrPath -> XrInteractionProfileInfo -> XrResult  
    xrGetCurrentInteractionProfile session topLevelUserPath interactionProfile = undefined

    xrGetActionStateBoolean :: XrSession -> XrActionStateGetInfo -> XrActionStateBoolean ->  XrResult
    xrGetActionStateBoolean session getInfo state = undefined

    xrGetActionStateFloat :: XrSession  -> XrActionStateGetInfo -> XrActionStateFloat -> XrResult
    xrGetActionStateFloat session getInfo state = undefined

    xrGetActionStateVector2f :: XrSession -> XrActionStateGetInfo -> XrActionStateVector2f -> XrResult
    xrGetActionStateVector2f session getInfo state = undefined

    xrGetActionStatePose :: XrSession -> XrActionStateGetInfo -> XrActionStatePose -> XrResult
    xrGetActionStatePose session getInfo state = undefined 

    -- Output Actions and Haptics [11.6]
    xrApplyHapticFeedback :: XrSession -> XrHapticActionInfo -> XrHapticBaseHeader -> XrResult
    xrApplyHapticFeedback session hapticActionInfo hapticFeedback = undefined

    -- duration: nanoseconds or XR_MIN_HAPTIC_DURATION
    -- frequency: Hz or XR_FREQUENCY_UNSPECIFIED
    xrStopHapticFeedback :: XrSession -> XrHapticActionInfo -> XrResult
    xrStopHapticFeedback session hapticActionInfo = undefined

    --Input Action State Synchronization [11.7]
    xrSyncActions :: XrSession -> XrActionsSyncInfo -> XrResult
    xrSyncActions session syncInfo = undefined

    -- Suggested Bindings [11.4]
    -- Applications need to provide default bindings for their actions 
    -- to runtimes so that input data can be mapped appropriately 
    -- to the application’s actions. The bindings suggested by this 
    -- system are only a hint to the runtime.
    xrSuggestInteractionProfileBindings :: XrInstance -> XrInteractionProfileSuggestedBinding -> XrResult
    xrSuggestInteractionProfileBindings xrInstance suggestedBindings = undefined

    -- An action set becomes immutable when attathed to a session.
    xrAttachSessionActionSets :: XrSession -> XrSessionActionSetsAttachInfo -> XrResult
    xrAttachSessionActionSets session attachInfo = undefined
