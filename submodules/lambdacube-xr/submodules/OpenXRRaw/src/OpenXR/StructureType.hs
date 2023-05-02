
module OpenXR.StructureType (XrStructureType(..), XrResult(..)) where 

    data XrStructureType  
        = XR_TYPE_UNKNOWN
        |XR_TYPE_API_LAYER_PROPERTIES
        |XR_TYPE_E|XTENSION_PROPERTIES
        |XR_TYPE_INSTANCE_CREATE_INFO
        |XR_TYPE_SYSTEM_GET_INFO
        |XR_TYPE_SYSTEM_PROPERTIES
        |XR_TYPE_VIEW_LOCATE_INFO
        |XR_TYPE_VIEW
        |XR_TYPE_SESSION_CREATE_INFO
        |XR_TYPE_SWAPCHAIN_CREATE_INFO
        |XR_TYPE_SESSION_BEGIN_INFO
        |XR_TYPE_VIEW_STATE
        |XR_TYPE_FRAME_END_INFO
        |XR_TYPE_HAPTIC_VIBRATION
        |XR_TYPE_EVENT_DATA_BUFFER
        |XR_TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING
        |XR_TYPE_EVENT_DATA_SESSION_STATE_CHANGED
        |XR_TYPE_ACTION_STATE_BOOLEAN
        |XR_TYPE_ACTION_STATE_FLOAT
        |XR_TYPE_ACTION_STATE_VECTOR2F
        |XR_TYPE_ACTION_STATE_POSE 
        |XR_TYPE_ACTION_SET_CREATE_INFO 
        |XR_TYPE_ACTION_CREATE_INFO
        |XR_TYPE_INSTANCE_PROPERTIES
        |XR_TYPE_FRAME_WAIT_INFO
        |XR_TYPE_COMPOSITION_LAYER_PROJECTION 
        |XR_TYPE_COMPOSITION_LAYER_QUAD 
        |XR_TYPE_REFERENCE_SPACE_CREATE_INFO 
        |XR_TYPE_ACTION_SPACE_CREATE_INFO 
        |XR_TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING 
        |XR_TYPE_VIEW_CONFIGURATION_VIEW 
        |XR_TYPE_SPACE_LOCATION 
        |XR_TYPE_SPACE_VELOCITY 
        |XR_TYPE_FRAME_STATE 
        |XR_TYPE_VIEW_CONFIGURATION_PROPERTIES 
        |XR_TYPE_FRAME_BEGIN_INFO 
        |XR_TYPE_COMPOSITION_LAYER_PROJECTION_VIEW 
        |XR_TYPE_EVENT_DATA_EVENTS_LOST
        |XR_TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING 
        |XR_TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED 
        |XR_TYPE_INTERACTION_PROFILE_STATE 
        |XR_TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO 
        |XR_TYPE_SWAPCHAIN_IMAGE_WAIT_INFO 
        |XR_TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO
        |XR_TYPE_ACTION_STATE_GET_INFO
        |XR_TYPE_HAPTIC_ACTION_INFO
        |XR_TYPE_SESSION_ACTION_SETS_ATTACH_INFO 
        |XR_TYPE_ACTIONS_SYNC_INFO
        |XR_TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO 
        |XR_TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO 
    -- Provided by |XR_KHR_composition_layer_cube
        |XR_TYPE_COMPOSITION_LAYER_CUBE_KHR -- -- = 1000006000
    -- Provided by |XR_KHR_android_create_instance
        |XR_TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR -- -- = 1000008000
    -- Provided by |XR_KHR_composition_layer_depth
        |XR_TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR -- -- = 1000010000
    -- Provided by |XR_KHR_vulkan_swapchain_format_list
        |XR_TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR -- -- = 1000014000
    -- Provided by |XR_E|XT_performance_settings
        |XR_TYPE_EVENT_DATA_PERF_SETTINGS_E|XT -- -- = 1000015000
    -- Provided by |XR_KHR_composition_layer_cylinder
        |XR_TYPE_COMPOSITION_LAYER_CYLINDER_KHR -- -- = 1000017000
    -- Provided by |XR_KHR_composition_layer_equirect
        |XR_TYPE_COMPOSITION_LAYER_EQUIRECT_KHR -- -- = 1000018000
    -- Provided by |XR_E|XT_debug_utils
        |XR_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT -- -- = 1000019000
    -- Provided by |XR_E|XT_debug_utils
        |XR_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT -- -- = 1000019001
    -- Provided by |XR_E|XT_debug_utils
        |XR_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -- -- = 1000019002
    -- Provided by |XR_E|XT_debug_utils
        |XR_TYPE_DEBUG_UTILS_LABEL_EXT -- -- = 1000019003
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR -- -- = 1000023000
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR -- -- = 1000023001
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR -- -- = 1000023002
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR -- -- = 1000023003
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR -- -- = 1000023004
    -- Provided by |XR_KHR_opengl_enable
        |XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR -- -- = 1000023005
    -- Provided by |XR_KHR_opengl_es_enable
        |XR_TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR -- -- = 1000024001
    -- Provided by |XR_KHR_opengl_es_enable
        |XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR -- -- = 1000024002
    -- Provided by |XR_KHR_opengl_es_enable
        |XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR -- -- = 1000024003
    -- Provided by |XR_KHR_vulkan_enable
        |XR_TYPE_GRAPHICS_BINDING_VULKAN_KHR -- -- = 1000025000
    -- Provided by |XR_KHR_vulkan_enable
        |XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR -- -- = 1000025001
    -- Provided by |XR_KHR_vulkan_enable
        |XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR -- -- = 1000025002
    -- Provided by |XR_KHR_D3D11_enable
        |XR_TYPE_GRAPHICS_BINDING_D3D11_KHR -- -- = 1000027000
    -- Provided by |XR_KHR_D3D11_enable
        |XR_TYPE_SWAPCHAIN_IMAGE_D3D11_KHR -- -- = 1000027001
    -- Provided by |XR_KHR_D3D11_enable
        |XR_TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR -- -- = 1000027002
    -- Provided by |XR_KHR_D3D12_enable
        |XR_TYPE_GRAPHICS_BINDING_D3D12_KHR -- -- = 1000028000
    -- Provided by |XR_KHR_D3D12_enable
        |XR_TYPE_SWAPCHAIN_IMAGE_D3D12_KHR -- -- = 1000028001
    -- Provided by |XR_KHR_D3D12_enable
        |XR_TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR -- -- = 1000028002
    -- Provided by |XR_E|XT_eye_gaze_interaction
        |XR_TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT -- -- = 1000030000
    -- Provided by |XR_E|XT_eye_gaze_interaction
        |XR_TYPE_EYE_GAZE_SAMPLE_TIME_EXT -- -- = 1000030001
    -- Provided by |XR_KHR_visibility_mask
        |XR_TYPE_VISIBILITY_MASK_KHR -- -- = 1000031000
    -- Provided by |XR_KHR_visibility_mask
        |XR_TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR -- -- = 1000031001
    -- Provided by |XR_E|XT|X_overlay
        |XR_TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX -- -- = 1000033000
    -- Provided by |XR_E|XT|X_overlay
        |XR_TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX -- -- = 1000033003
    -- Provided by |XR_KHR_composition_layer_color_scale_bias
        |XR_TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR -- -- = 1000034000
    -- Provided by |XR_MSFT_spatial_anchor
        |XR_TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT -- -- = 1000039000
    -- Provided by |XR_MSFT_spatial_anchor
        |XR_TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT -- -- = 1000039001
    -- Provided by |XR_FB_composition_layer_image_layout
        |XR_TYPE_COMPOSITION_LAYER_IMAGE_LAYOUT_FB -- -- = 1000040000
    -- Provided by |XR_FB_composition_layer_alpha_blend
        |XR_TYPE_COMPOSITION_LAYER_ALPHA_BLEND_FB -- -- = 1000041001
    -- Provided by |XR_E|XT_view_configuration_depth_range
        |XR_TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT -- -- = 1000046000
    -- Provided by |XR_MND|X_egl_enable
        |XR_TYPE_GRAPHICS_BINDING_EGL_MNDX -- -- = 1000048004
    -- Provided by |XR_MSFT_spatial_graph_bridge
        |XR_TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT -- -- = 1000049000
    -- Provided by |XR_MSFT_spatial_graph_bridge
        |XR_TYPE_SPATIAL_GRAPH_STATIC_NODE_BINDING_CREATE_INFO_MSFT -- -- = 1000049001
    -- Provided by |XR_MSFT_spatial_graph_bridge
        |XR_TYPE_SPATIAL_GRAPH_NODE_BINDING_PROPERTIES_GET_INFO_MSFT -- -- = 1000049002
    -- Provided by |XR_MSFT_spatial_graph_bridge
        |XR_TYPE_SPATIAL_GRAPH_NODE_BINDING_PROPERTIES_MSFT -- -- = 1000049003
    -- Provided by |XR_E|XT_hand_tracking
        |XR_TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT -- -- = 1000051000
    -- Provided by |XR_E|XT_hand_tracking
        |XR_TYPE_HAND_TRACKER_CREATE_INFO_EXT -- -- = 1000051001
    -- Provided by |XR_E|XT_hand_tracking
        |XR_TYPE_HAND_JOINTS_LOCATE_INFO_EXT -- -- = 1000051002
    -- Provided by |XR_E|XT_hand_tracking
        |XR_TYPE_HAND_JOINT_LOCATIONS_EXT -- -- = 1000051003
    -- Provided by |XR_E|XT_hand_tracking
        |XR_TYPE_HAND_JOINT_VELOCITIES_EXT -- -- = 1000051004
    -- Provided by |XR_MSFT_hand_tracking_mesh
        |XR_TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT -- -- = 1000052000
    -- Provided by |XR_MSFT_hand_tracking_mesh
        |XR_TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT -- -- = 1000052001
    -- Provided by |XR_MSFT_hand_tracking_mesh
        |XR_TYPE_HAND_MESH_UPDATE_INFO_MSFT -- -- = 1000052002
    -- Provided by |XR_MSFT_hand_tracking_mesh
        |XR_TYPE_HAND_MESH_MSFT -- -- = 1000052003
    -- Provided by |XR_MSFT_hand_tracking_mesh
        |XR_TYPE_HAND_POSE_TYPE_INFO_MSFT -- -- = 1000052004
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT -- -- = 1000053000
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT -- -- = 1000053001
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT -- -- = 1000053002
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT -- -- = 1000053003
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT -- -- = 1000053004
    -- Provided by |XR_MSFT_secondary_view_configuration
        |XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT -- -- = 1000053005
    -- Provided by |XR_MSFT_controller_model
        |XR_TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT -- -- = 1000055000
    -- Provided by |XR_MSFT_controller_model
        |XR_TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT -- -- = 1000055001
    -- Provided by |XR_MSFT_controller_model
        |XR_TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT -- -- = 1000055002
    -- Provided by |XR_MSFT_controller_model
        |XR_TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT -- -- = 1000055003
    -- Provided by |XR_MSFT_controller_model
        |XR_TYPE_CONTROLLER_MODEL_STATE_MSFT -- -- = 1000055004
    -- Provided by |XR_EPIC_view_configuration_fov
        |XR_TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC -- -- = 1000059000
    -- Provided by |XR_MSFT_holographic_window_attachment
        |XR_TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT -- -- = 1000063000
    -- Provided by |XR_MSFT_composition_layer_reprojection
        |XR_TYPE_COMPOSITION_LAYER_REPROJECTION_INFO_MSFT -- -- = 1000066000
    -- Provided by |XR_MSFT_composition_layer_reprojection
        |XR_TYPE_COMPOSITION_LAYER_REPROJECTION_PLANE_OVERRIDE_MSFT -- -- = 1000066001
    -- Provided by |XR_FB_android_surface_swapchain_create
        |XR_TYPE_ANDROID_SURFACE_SWAPCHAIN_CREATE_INFO_FB -- -- = 1000070000
    -- Provided by |XR_FB_composition_layer_secure_content
        |XR_TYPE_COMPOSITION_LAYER_SECURE_CONTENT_FB -- -- = 1000072000
    -- Provided by |XR_FB_body_tracking
        |XR_TYPE_BODY_TRACKER_CREATE_INFO_FB -- -- = 1000076001
    -- Provided by |XR_FB_body_tracking
        |XR_TYPE_BODY_JOINTS_LOCATE_INFO_FB -- -- = 1000076002
    -- Provided by |XR_FB_body_tracking
        |XR_TYPE_SYSTEM_BODY_TRACKING_PROPERTIES_FB -- -- = 1000076004
    -- Provided by |XR_FB_body_tracking
        |XR_TYPE_BODY_JOINT_LOCATIONS_FB -- -- = 1000076005
    -- Provided by |XR_FB_body_tracking
        |XR_TYPE_BODY_SKELETON_FB -- -- = 1000076006
    -- Provided by |XR_E|XT_dpad_binding
        |XR_TYPE_INTERACTION_PROFILE_DPAD_BINDING_EXT -- -- = 1000078000
    -- Provided by |XR_VALVE_analog_threshold
        |XR_TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE -- -- = 1000079000
    -- Provided by |XR_E|XT_hand_joints_motion_range
        |XR_TYPE_HAND_JOINTS_MOTION_RANGE_INFO_EXT -- -- = 1000080000
    -- Provided by |XR_KHR_loader_init_android
        |XR_TYPE_LOADER_INIT_INFO_ANDROID_KHR -- -- = 1000089000
    -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR -- -- = 1000090000
    -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_VULKAN_DEVICE_CREATE_INFO_KHR -- -- = 1000090001
    -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR -- -- = 1000090003
    -- Provided by |XR_KHR_composition_layer_equirect2
        |XR_TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR -- -- = 1000091000
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_OBSERVER_CREATE_INFO_MSFT -- -- = 1000097000
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_CREATE_INFO_MSFT -- -- = 1000097001
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_NEW_SCENE_COMPUTE_INFO_MSFT -- -- = 1000097002
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_VISUAL_MESH_COMPUTE_LOD_INFO_MSFT -- -- = 1000097003
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_COMPONENTS_MSFT -- -- = 1000097004
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_COMPONENTS_GET_INFO_MSFT -- -- = 1000097005
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_COMPONENT_LOCATIONS_MSFT -- -- = 1000097006
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_COMPONENTS_LOCATE_INFO_MSFT -- -- = 1000097007
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_OBJECTS_MSFT -- -- = 1000097008
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_COMPONENT_PARENT_FILTER_INFO_MSFT -- -- = 1000097009
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_OBJECT_TYPES_FILTER_INFO_MSFT -- -- = 1000097010
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_PLANES_MSFT -- -- = 1000097011
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_PLANE_ALIGNMENT_FILTER_INFO_MSFT -- -- = 1000097012
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESHES_MSFT -- -- = 1000097013
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESH_BUFFERS_GET_INFO_MSFT -- -- = 1000097014
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESH_BUFFERS_MSFT -- -- = 1000097015
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESH_VERTE|X_BUFFER_MSFT -- -- = 1000097016
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESH_INDICES_UINT32_MSFT -- -- = 1000097017
    -- Provided by |XR_MSFT_scene_understanding
        |XR_TYPE_SCENE_MESH_INDICES_UINT16_MSFT -- -- = 1000097018
    -- Provided by |XR_MSFT_scene_understanding_serialization
        |XR_TYPE_SERIALIZED_SCENE_FRAGMENT_DATA_GET_INFO_MSFT -- -- = 1000098000
    -- Provided by |XR_MSFT_scene_understanding_serialization
        |XR_TYPE_SCENE_DESERIALIZE_INFO_MSFT -- -- = 1000098001
    -- Provided by |XR_FB_display_refresh_rate
        |XR_TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB -- -- = 1000101000
    -- Provided by |XR_HTC|X_vive_tracker_interaction
        |XR_TYPE_VIVE_TRACKER_PATHS_HTCX -- -- = 1000103000
    -- Provided by |XR_HTC|X_vive_tracker_interaction
        |XR_TYPE_EVENT_DATA_VIVE_TRACKER_CONNECTED_HTCX -- -- = 1000103001
    -- Provided by |XR_HTC_facial_tracking
        |XR_TYPE_SYSTEM_FACIAL_TRACKING_PROPERTIES_HTC -- -- = 1000104000
    -- Provided by |XR_HTC_facial_tracking
        |XR_TYPE_FACIAL_TRACKER_CREATE_INFO_HTC -- -- = 1000104001
    -- Provided by |XR_HTC_facial_tracking
        |XR_TYPE_FACIAL_E|XPRESSIONS_HTC -- -- = 1000104002
    -- Provided by |XR_FB_color_space
        |XR_TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB -- -- = 1000108000
    -- Provided by |XR_FB_hand_tracking_mesh
        |XR_TYPE_HAND_TRACKING_MESH_FB -- -- = 1000110001
    -- Provided by |XR_FB_hand_tracking_mesh
        |XR_TYPE_HAND_TRACKING_SCALE_FB -- -- = 1000110003
    -- Provided by |XR_FB_hand_tracking_aim
        |XR_TYPE_HAND_TRACKING_AIM_STATE_FB -- -- = 1000111001
    -- Provided by |XR_FB_hand_tracking_capsules
        |XR_TYPE_HAND_TRACKING_CAPSULES_STATE_FB -- -- = 1000112000
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_SYSTEM_SPATIAL_ENTITY_PROPERTIES_FB -- -- = 1000113004
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_SPATIAL_ANCHOR_CREATE_INFO_FB -- -- = 1000113003
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_SPACE_COMPONENT_STATUS_SET_INFO_FB -- -- = 1000113007
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_SPACE_COMPONENT_STATUS_FB -- -- = 1000113001
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_EVENT_DATA_SPATIAL_ANCHOR_CREATE_COMPLETE_FB -- -- = 1000113005
    -- Provided by |XR_FB_spatial_entity
        |XR_TYPE_EVENT_DATA_SPACE_SET_STATUS_COMPLETE_FB -- -- = 1000113006
    -- Provided by |XR_FB_foveation
        |XR_TYPE_FOVEATION_PROFILE_CREATE_INFO_FB -- -- = 1000114000
    -- Provided by |XR_FB_foveation
        |XR_TYPE_SWAPCHAIN_CREATE_INFO_FOVEATION_FB -- -- = 1000114001
    -- Provided by |XR_FB_foveation
        |XR_TYPE_SWAPCHAIN_STATE_FOVEATION_FB -- -- = 1000114002
    -- Provided by |XR_FB_foveation_configuration
        |XR_TYPE_FOVEATION_LEVEL_PROFILE_CREATE_INFO_FB -- -- = 1000115000
    -- Provided by |XR_FB_keyboard_tracking
        |XR_TYPE_KEYBOARD_SPACE_CREATE_INFO_FB -- -- = 1000116009
    -- Provided by |XR_FB_keyboard_tracking
        |XR_TYPE_KEYBOARD_TRACKING_QUERY_FB -- -- = 1000116004
    -- Provided by |XR_FB_keyboard_tracking
        |XR_TYPE_SYSTEM_KEYBOARD_TRACKING_PROPERTIES_FB -- -- = 1000116002
    -- Provided by |XR_FB_triangle_mesh
        |XR_TYPE_TRIANGLE_MESH_CREATE_INFO_FB -- -- = 1000117001
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_SYSTEM_PASSTHROUGH_PROPERTIES_FB -- -- = 1000118000
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_CREATE_INFO_FB -- -- = 1000118001
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_LAYER_CREATE_INFO_FB -- -- = 1000118002
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_COMPOSITION_LAYER_PASSTHROUGH_FB -- -- = 1000118003
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_GEOMETRY_INSTANCE_CREATE_INFO_FB -- -- = 1000118004
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_GEOMETRY_INSTANCE_TRANSFORM_FB -- -- = 1000118005
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_SYSTEM_PASSTHROUGH_PROPERTIES2_FB -- -- = 1000118006
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_STYLE_FB -- -- = 1000118020
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_COLOR_MAP_MONO_TO_RGBA_FB -- -- = 1000118021
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_COLOR_MAP_MONO_TO_MONO_FB -- -- = 1000118022
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_PASSTHROUGH_BRIGHTNESS_CONTRAST_SATURATION_FB -- -- = 1000118023
    -- Provided by |XR_FB_passthrough
        |XR_TYPE_EVENT_DATA_PASSTHROUGH_STATE_CHANGED_FB -- -- = 1000118030
    -- Provided by |XR_FB_render_model
        |XR_TYPE_RENDER_MODEL_PATH_INFO_FB -- -- = 1000119000
    -- Provided by |XR_FB_render_model
        |XR_TYPE_RENDER_MODEL_PROPERTIES_FB -- -- = 1000119001
    -- Provided by |XR_FB_render_model
        |XR_TYPE_RENDER_MODEL_BUFFER_FB -- -- = 1000119002
    -- Provided by |XR_FB_render_model
        |XR_TYPE_RENDER_MODEL_LOAD_INFO_FB -- -- = 1000119003
    -- Provided by |XR_FB_render_model
        |XR_TYPE_SYSTEM_RENDER_MODEL_PROPERTIES_FB -- -- = 1000119004
    -- Provided by |XR_FB_render_model
        |XR_TYPE_RENDER_MODEL_CAPABILITIES_REQUEST_FB -- -- = 1000119005
    -- Provided by |XR_KHR_binding_modification
        |XR_TYPE_BINDING_MODIFICATIONS_KHR -- -- = 1000120000
    -- Provided by |XR_VARJO_foveated_rendering
        |XR_TYPE_VIEW_LOCATE_FOVEATED_RENDERING_VARJO -- -- = 1000121000
    -- Provided by |XR_VARJO_foveated_rendering
        |XR_TYPE_FOVEATED_VIEW_CONFIGURATION_VIEW_VARJO -- -- = 1000121001
    -- Provided by |XR_VARJO_foveated_rendering
        |XR_TYPE_SYSTEM_FOVEATED_RENDERING_PROPERTIES_VARJO -- -- = 1000121002
    -- Provided by |XR_VARJO_composition_layer_depth_test
        |XR_TYPE_COMPOSITION_LAYER_DEPTH_TEST_VARJO -- -- = 1000122000
    -- Provided by |XR_VARJO_marker_tracking
        |XR_TYPE_SYSTEM_MARKER_TRACKING_PROPERTIES_VARJO -- -- = 1000124000
    -- Provided by |XR_VARJO_marker_tracking
        |XR_TYPE_EVENT_DATA_MARKER_TRACKING_UPDATE_VARJO -- -- = 1000124001
    -- Provided by |XR_VARJO_marker_tracking
        |XR_TYPE_MARKER_SPACE_CREATE_INFO_VARJO -- -- = 1000124002
    -- Provided by |XR_ML_frame_end_info
        |XR_TYPE_FRAME_END_INFO_ML -- -- = 1000135000
    -- Provided by |XR_ML_global_dimmer
        |XR_TYPE_GLOBAL_DIMMER_FRAME_END_INFO_ML -- -- = 1000136000
    -- Provided by |XR_ML_compat
        |XR_TYPE_COORDINATE_SPACE_CREATE_INFO_ML -- -- = 1000137000
    -- Provided by |XR_MSFT_spatial_anchor_persistence
        |XR_TYPE_SPATIAL_ANCHOR_PERSISTENCE_INFO_MSFT -- -- = 1000142000
    -- Provided by |XR_MSFT_spatial_anchor_persistence
        |XR_TYPE_SPATIAL_ANCHOR_FROM_PERSISTED_ANCHOR_CREATE_INFO_MSFT -- -- = 1000142001
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_SPACE_QUERY_INFO_FB -- -- = 1000156001
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_SPACE_QUERY_RESULTS_FB -- -- = 1000156002
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_SPACE_STORAGE_LOCATION_FILTER_INFO_FB -- -- = 1000156003
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_SPACE_UUID_FILTER_INFO_FB -- -- = 1000156054
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_SPACE_COMPONENT_FILTER_INFO_FB -- -- = 1000156052
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_EVENT_DATA_SPACE_QUERY_RESULTS_AVAILABLE_FB -- -- = 1000156103
    -- Provided by |XR_FB_spatial_entity_query
        |XR_TYPE_EVENT_DATA_SPACE_QUERY_COMPLETE_FB -- -- = 1000156104
    -- Provided by |XR_FB_spatial_entity_storage
        |XR_TYPE_SPACE_SAVE_INFO_FB -- -- = 1000158000
    -- Provided by |XR_FB_spatial_entity_storage
        |XR_TYPE_SPACE_ERASE_INFO_FB -- -- = 1000158001
    -- Provided by |XR_FB_spatial_entity_storage
        |XR_TYPE_EVENT_DATA_SPACE_SAVE_COMPLETE_FB -- -- = 1000158106
    -- Provided by |XR_FB_spatial_entity_storage
        |XR_TYPE_EVENT_DATA_SPACE_ERASE_COMPLETE_FB -- -- = 1000158107
    -- Provided by |XR_FB_foveation_vulkan
        |XR_TYPE_SWAPCHAIN_IMAGE_FOVEATION_VULKAN_FB -- -- = 1000160000
    -- Provided by |XR_FB_swapchain_update_state_android_surface
        |XR_TYPE_SWAPCHAIN_STATE_ANDROID_SURFACE_DIMENSIONS_FB -- -- = 1000161000
    -- Provided by |XR_FB_swapchain_update_state_opengl_es
        |XR_TYPE_SWAPCHAIN_STATE_SAMPLER_OPENGL_ES_FB -- -- = 1000162000
    -- Provided by |XR_FB_swapchain_update_state_vulkan
        |XR_TYPE_SWAPCHAIN_STATE_SAMPLER_VULKAN_FB -- -- = 1000163000
    -- Provided by |XR_FB_spatial_entity_sharing
        |XR_TYPE_SPACE_SHARE_INFO_FB -- -- = 1000169001
    -- Provided by |XR_FB_spatial_entity_sharing
        |XR_TYPE_EVENT_DATA_SPACE_SHARE_COMPLETE_FB -- -- = 1000169002
    -- Provided by |XR_FB_space_warp
        |XR_TYPE_COMPOSITION_LAYER_SPACE_WARP_INFO_FB -- -- = 1000171000
    -- Provided by |XR_FB_space_warp
        |XR_TYPE_SYSTEM_SPACE_WARP_PROPERTIES_FB -- -- = 1000171001
    -- Provided by |XR_FB_haptic_amplitude_envelope
        |XR_TYPE_HAPTIC_AMPLITUDE_ENVELOPE_VIBRATION_FB -- -- = 1000173001
    -- Provided by |XR_FB_scene
        |XR_TYPE_SEMANTIC_LABELS_FB -- -- = 1000175000
    -- Provided by |XR_FB_scene
        |XR_TYPE_ROOM_LAYOUT_FB -- -- = 1000175001
    -- Provided by |XR_FB_scene
        |XR_TYPE_BOUNDARY_2D_FB -- -- = 1000175002
    -- Provided by |XR_ALMALENCE_digital_lens_control
        |XR_TYPE_DIGITAL_LENS_CONTROL_ALMALENCE -- -- = 1000196000
    -- Provided by |XR_FB_scene_capture
        |XR_TYPE_EVENT_DATA_SCENE_CAPTURE_COMPLETE_FB -- -- = 1000198001
    -- Provided by |XR_FB_scene_capture
        |XR_TYPE_SCENE_CAPTURE_REQUEST_INFO_FB -- -- = 1000198050
    -- Provided by |XR_FB_spatial_entity_container
        |XR_TYPE_SPACE_CONTAINER_FB -- -- = 1000199000
    -- Provided by |XR_META_foveation_eye_tracked
        |XR_TYPE_FOVEATION_EYE_TRACKED_PROFILE_CREATE_INFO_META -- -- = 1000200000
    -- Provided by |XR_META_foveation_eye_tracked
        |XR_TYPE_FOVEATION_EYE_TRACKED_STATE_META -- -- = 1000200001
    -- Provided by |XR_META_foveation_eye_tracked
        |XR_TYPE_SYSTEM_FOVEATION_EYE_TRACKED_PROPERTIES_META -- -- = 1000200002
    -- Provided by |XR_FB_face_tracking
        |XR_TYPE_SYSTEM_FACE_TRACKING_PROPERTIES_FB -- -- = 1000201004
    -- Provided by |XR_FB_face_tracking
        |XR_TYPE_FACE_TRACKER_CREATE_INFO_FB -- -- = 1000201005
    -- Provided by |XR_FB_face_tracking
        |XR_TYPE_FACE_EXPRESSION_INFO_FB -- -- = 1000201002
    -- Provided by |XR_FB_face_tracking
        |XR_TYPE_FACE_EXPRESSION_WEIGHTS_FB -- -- = 1000201006
    -- Provided by |XR_FB_eye_tracking_social
        |XR_TYPE_EYE_TRACKER_CREATE_INFO_FB -- -- = 1000202001
    -- Provided by |XR_FB_eye_tracking_social
        |XR_TYPE_EYE_GAZES_INFO_FB -- -- = 1000202002
    -- Provided by |XR_FB_eye_tracking_social
        |XR_TYPE_EYE_GAZES_FB -- -- = 1000202003
    -- Provided by |XR_FB_eye_tracking_social
        |XR_TYPE_SYSTEM_EYE_TRACKING_PROPERTIES_FB -- -- = 1000202004
    -- Provided by |XR_FB_passthrough_keyboard_hands
        |XR_TYPE_PASSTHROUGH_KEYBOARD_HANDS_INTENSITY_FB -- -- = 1000203002
    -- Provided by |XR_FB_composition_layer_settings
        |XR_TYPE_COMPOSITION_LAYER_SETTINGS_FB -- -- = 1000204000
    -- Provided by |XR_FB_haptic_pcm
        |XR_TYPE_HAPTIC_PCM_VIBRATION_FB -- -- = 1000209001
    -- Provided by |XR_FB_haptic_pcm
        |XR_TYPE_DEVICE_PCM_SAMPLE_RATE_STATE_FB -- -- = 1000209002
    -- Provided by |XR_FB_composition_layer_depth_test
        |XR_TYPE_COMPOSITION_LAYER_DEPTH_TEST_FB -- -- = 1000212000
    -- Provided by |XR_META_local_dimming
        |XR_TYPE_LOCAL_DIMMING_FRAME_END_INFO_META -- -- = 1000216000
    -- Provided by |XR_OCULUS_e|Xternal_camera
        |XR_TYPE_EXTERNAL_CAMERA_OCULUS -- -- = 1000226000
    -- Provided by |XR_META_vulkan_swapchain_create_info
        |XR_TYPE_VULKAN_SWAPCHAIN_CREATE_INFO_META -- -- = 1000227000
    -- Provided by |XR_META_performance_metrics
        |XR_TYPE_PERFORMANCE_METRICS_STATE_META -- -- = 1000232001
    -- Provided by |XR_META_performance_metrics
        |XR_TYPE_PERFORMANCE_METRICS_COUNTER_META -- -- = 1000232002
    -- Provided by |XR_FB_spatial_entity_storage_batch
        |XR_TYPE_SPACE_LIST_SAVE_INFO_FB -- -- = 1000238000
    -- Provided by |XR_FB_spatial_entity_storage_batch
        |XR_TYPE_EVENT_DATA_SPACE_LIST_SAVE_COMPLETE_FB
    -- Provided by |XR_FB_spatial_entity_user
        |XR_TYPE_SPACE_USER_CREATE_INFO_FB
    -- Provided by |XR_META_headset_id
        |XR_TYPE_SYSTEM_HEADSET_ID_PROPERTIES_META
    -- Provided by |XR_HTC_passthrough
        |XR_TYPE_PASSTHROUGH_CREATE_INFO_HTC
    -- Provided by |XR_HTC_passthrough
        |XR_TYPE_PASSTHROUGH_COLOR_HTC
    -- Provided by |XR_HTC_passthrough
        |XR_TYPE_PASSTHROUGH_MESH_TRANSFORM_INFO_HTC
    -- Provided by |XR_HTC_passthrough
        |XR_TYPE_COMPOSITION_LAYER_PASSTHROUGH_HTC
    -- Provided by |XR_HTC_foveation
        |XR_TYPE_FOVEATION_APPLY_INFO_HTC
    -- Provided by |XR_HTC_foveation
        |XR_TYPE_FOVEATION_DYNAMIC_MODE_INFO_HTC
    -- Provided by |XR_HTC_foveation
        |XR_TYPE_FOVEATION_CUSTOM_MODE_INFO_HTC
    -- Provided by |XR_E|XT_active_action_set_priority
        |XR_TYPE_ACTIVE_ACTION_SET_PRIORITIES_EXT
        -- Provided by |XR_MND|X_force_feedback_curl
        |XR_TYPE_SYSTEM_FORCE_FEEDBACK_CURL_PROPERTIES_MNDX
        -- Provided by |XR_MND|X_force_feedback_curl
        |XR_TYPE_FORCE_FEEDBACK_CURL_APPLY_LOCATIONS_MNDX -- -- = 1000375001
        -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_GRAPHICS_BINDING_VULKAN2_KHR -- -- = |XR_TYPE_GRAPHICS_BINDING_VULKAN_KHR
        -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_SWAPCHAIN_IMAGE_VULKAN2_KHR -- -- = |XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR
        -- Provided by |XR_KHR_vulkan_enable2
        |XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN2_KHR -- -- = |XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR
        --Provided by |XR_FB_haptic_pcm
        |XR_TYPE_DEVICE_PCM_SAMPLE_RATE_GET_INFO_FB -- -- = |XR_TYPE_DEVICE_PCM_SAMPLE_RATE_STATE_FB
        |XR_STRUCTURE_TYPE_MA|X_ENUM -- -- = 0|X7FFFFFFF
        deriving (Eq, Show, Enum)
    
    data XrResult 
        = XR_SUCCESS
        |XR_TIMEOUT_EXPIRED -- = 1,
        |XR_SESSION_LOSS_PENDING -- = 3,
        |XR_EVENT_UNAVAILABLE -- = 4,
        |XR_SPACE_BOUNDS_UNAVAILABLE -- = 7,
        |XR_SESSION_NOT_FOCUSED -- = 8,
        |XR_FRAME_DISCARDED -- = 9,
        |XR_ERROR_VALIDATION_FAILURE -- = -1,
        |XR_ERROR_RUNTIME_FAILURE -- = -2,
        |XR_ERROR_OUT_OF_MEMORY -- = -3,
        |XR_ERROR_API_VERSION_UNSUPPORTED -- = -4,
        |XR_ERROR_INITIALIZATION_FAILED -- = -6,
        |XR_ERROR_FUNCTION_UNSUPPORTED -- = -7,
        |XR_ERROR_FEATURE_UNSUPPORTED -- = -8,
        |XR_ERROR_EXTENSION_NOT_PRESENT -- = -9,
        |XR_ERROR_LIMIT_REACHED -- = -10,
        |XR_ERROR_SIZE_INSUFFICIENT -- = -11,
        |XR_ERROR_HANDLE_INVALID -- = -12,
        |XR_ERROR_INSTANCE_LOST -- = -13,
        |XR_ERROR_SESSION_RUNNING -- = -14,
        |XR_ERROR_SESSION_NOT_RUNNING -- = -16,
        |XR_ERROR_SESSION_LOST -- = -17,
        |XR_ERROR_SYSTEM_INVALID -- = -18,
        |XR_ERROR_PATH_INVALID -- = -19,
        |XR_ERROR_PATH_COUNT_EXCEEDED -- = -20,
        |XR_ERROR_PATH_FORMAT_INVALID -- = -21,
        |XR_ERROR_PATH_UNSUPPORTED -- = -22,
        |XR_ERROR_LAYER_INVALID -- = -23,
        |XR_ERROR_LAYER_LIMIT_EXCEEDED -- = -24,
        |XR_ERROR_SWAPCHAIN_RECT_INVALID -- = -25,
        |XR_ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED -- = -26,
        |XR_ERROR_ACTION_TYPE_MISMATCH -- = -27,
        |XR_ERROR_SESSION_NOT_READY -- = -28,
        |XR_ERROR_SESSION_NOT_STOPPING -- = -29,
        |XR_ERROR_TIME_INVALID -- = -30,
        |XR_ERROR_REFERENCE_SPACE_UNSUPPORTED -- = -31,
        |XR_ERROR_FILE_ACCESS_ERROR -- = -32,
        |XR_ERROR_FILE_CONTENTS_INVALID -- = -33,
        |XR_ERROR_FORM_FACTOR_UNSUPPORTED -- = -34,
        |XR_ERROR_FORM_FACTOR_UNAVAILABLE -- = -35,
        |XR_ERROR_API_LAYER_NOT_PRESENT -- = -36,
        |XR_ERROR_CALL_ORDER_INVALID -- = -37,
        |XR_ERROR_GRAPHICS_DEVICE_INVALID -- = -38,
        |XR_ERROR_POSE_INVALID -- = -39,
        |XR_ERROR_INDEX_OUT_OF_RANGE -- = -40,
        |XR_ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED -- = -41,
        |XR_ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED -- = -42,
        |XR_ERROR_NAME_DUPLICATED -- = -44,
        |XR_ERROR_NAME_INVALID -- = -45,
        |XR_ERROR_ACTIONSET_NOT_ATTACHED -- = -46,
        |XR_ERROR_ACTIONSETS_ALREADY_ATTACHED -- = -47,
        |XR_ERROR_LOCALIZED_NAME_DUPLICATED -- = -48,
        |XR_ERROR_LOCALIZED_NAME_INVALID -- = -49,
        |XR_ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING -- = -50,
        |XR_ERROR_RUNTIME_UNAVAILABLE -- = -51,
        -- // Provided by XR_KHR_android_thread_settings
        |XR_ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR -- = -1000003000,
        -- // Provided by XR_KHR_android_thread_settings
        |XR_ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR -- = -1000003001,
        -- // Provided by XR_MSFT_spatial_anchor
        |XR_ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT -- = -1000039001,
        -- // Provided by XR_MSFT_secondary_view_configuration
        |XR_ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT -- = -1000053000,
        -- // Provided by XR_MSFT_controller_model
        |XR_ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT -- = -1000055000,
        -- // Provided by XR_MSFT_composition_layer_reprojection
        |XR_ERROR_REPROJECTION_MODE_UNSUPPORTED_MSFT -- = -1000066000,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_COMPUTE_NEW_SCENE_NOT_COMPLETED_MSFT -- = -1000097000,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_SCENE_COMPONENT_ID_INVALID_MSFT -- = -1000097001,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_SCENE_COMPONENT_TYPE_MISMATCH_MSFT -- = -1000097002,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_SCENE_MESH_BUFFER_ID_INVALID_MSFT -- = -1000097003,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_SCENE_COMPUTE_FEATURE_INCOMPATIBLE_MSFT -- = -1000097004,
        -- // Provided by XR_MSFT_scene_understanding
        |XR_ERROR_SCENE_COMPUTE_CONSISTENCY_MISMATCH_MSFT -- = -1000097005,
        -- // Provided by XR_FB_display_refresh_rate
        |XR_ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB -- = -1000101000,
        -- // Provided by XR_FB_color_space
        |XR_ERROR_COLOR_SPACE_UNSUPPORTED_FB -- = -1000108000,
        -- // Provided by XR_FB_spatial_entity
        |XR_ERROR_SPACE_COMPONENT_NOT_SUPPORTED_FB -- = -1000113000,
        -- // Provided by XR_FB_spatial_entity
        |XR_ERROR_SPACE_COMPONENT_NOT_ENABLED_FB -- = -1000113001,
        -- // Provided by XR_FB_spatial_entity
        |XR_ERROR_SPACE_COMPONENT_STATUS_PENDING_FB -- = -1000113002,
        -- // Provided by XR_FB_spatial_entity
        |XR_ERROR_SPACE_COMPONENT_STATUS_ALREADY_SET_FB -- = -1000113003,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_UNEXPECTED_STATE_PASSTHROUGH_FB -- = -1000118000,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_FEATURE_ALREADY_CREATED_PASSTHROUGH_FB -- = -1000118001,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_FEATURE_REQUIRED_PASSTHROUGH_FB -- = -1000118002,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_NOT_PERMITTED_PASSTHROUGH_FB -- = -1000118003,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_INSUFFICIENT_RESOURCES_PASSTHROUGH_FB -- = -1000118004,
        -- // Provided by XR_FB_passthrough
        |XR_ERROR_UNKNOWN_PASSTHROUGH_FB -- = -1000118050,
        -- // Provided by XR_FB_render_model
        |XR_ERROR_RENDER_MODEL_KEY_INVALID_FB -- = -1000119000,
        -- // Provided by XR_FB_render_model
        |XR_RENDER_MODEL_UNAVAILABLE_FB -- = 1000119020,
        -- // Provided by XR_VARJO_marker_tracking
        |XR_ERROR_MARKER_NOT_TRACKED_VARJO -- = -1000124000,
        -- // Provided by XR_VARJO_marker_tracking
        |XR_ERROR_MARKER_ID_INVALID_VARJO -- = -1000124001,
        -- // Provided by XR_MSFT_spatial_anchor_persistence
        |XR_ERROR_SPATIAL_ANCHOR_NAME_NOT_FOUND_MSFT -- = -1000142001,
        -- // Provided by XR_MSFT_spatial_anchor_persistence
        |XR_ERROR_SPATIAL_ANCHOR_NAME_INVALID_MSFT -- = -1000142002,
        -- // Provided by XR_FB_spatial_entity_sharing
        |XR_ERROR_SPACE_MAPPING_INSUFFICIENT_FB -- = -1000169000,
        -- // Provided by XR_FB_spatial_entity_sharing
        |XR_ERROR_SPACE_LOCALIZATION_FAILED_FB -- = -1000169001,
        -- // Provided by XR_FB_spatial_entity_sharing
        |XR_ERROR_SPACE_NETWORK_TIMEOUT_FB -- = -1000169002,
        -- // Provided by XR_FB_spatial_entity_sharing
        |XR_ERROR_SPACE_NETWORK_REQUEST_FAILED_FB -- = -1000169003,
        -- // Provided by XR_FB_spatial_entity_sharing
        |XR_ERROR_SPACE_CLOUD_STORAGE_DISABLED_FB -- = -1000169004,
        -- // Provided by XR_QCOM_tracking_optimization_settings
        |XR_ERROR_HINT_ALREADY_SET_QCOM -- = -1000306000,
        |XR_RESULT_MAX_ENUM -- = 0x7FFFFFFF
        deriving (Eq, Show)
 
    -- data XrSuccess = 
    --     XR_SUCCESS 
    --     |XR_TIMEOUT_EXPIRED
    --     |XR_SESSION_LOSS_PENDING 
    --     |XR_EVENT_UNAVAILABLE
    --     |XR_SESSION_NOT_FOCUSED
    --     |XR_FRAME_DISCARDED
    --     |XR_SPACE_BOUNDS_UNAVAILABLE
    --     deriving (Eq, Show)

    -- data XrError = 
    --     ACTION_TYPE_MISMATCH
    --     |ACTIONSET_NOT_ATTACHED
    --     |ACTIONSETS_ALREADY_ATTACHED
    --     |ANDROID_THREAD_SETTINGS_FAILURE_KHR
    --     |ANDROID_THREAD_SETTINGS_ID_INVALID_KHR
    --     |API_LAYER_NOT_PRESENT
    --     |API_VERSION_UNSUPPORTED
    --     |CALL_ORDER_INVALID
    --     |ENVIRONMENT_BLEND_MODE_UNSUPPORTED
    --     |EXTENSION_NOT_PRESENT
    --     |FEATURE_UNSUPPORTED
    --     |FILE_ACCESS_ERROR
    --     |FILE_CONTENTS_INVALID
    --     |FORM_FACTOR_UNAVAILABLE
    --     |FORM_FACTOR_UNSUPPORTED
    --     |FUNCTION_UNSUPPORTED
    --     |GRAPHICS_DEVICE_INVALID
    --     |HANDLE_INVALID
    --     |INDEX_OUT_OF_RANGE
    --     |INITIALIZATION_FAILED
    --     |INSTANCE_LOST
    --     |LAYER_INVALID
    --     |LAYER_LIMIT_EXCEEDED
    --     |LIMIT_REACHED
    --     |LOCALIZED_NAME_DUPLICATED
    --     |LOCALIZED_NAME_INVALID
    --     |NAME_DUPLICATED
    --     |NAME_INVALID
    --     |OUT_OF_MEMORY
    --     |PATH_COUNT_EXCEEDED
    --     |PATH_FORMAT_INVALID
    --     |PATH_INVALID
    --     |PATH_UNSUPPORTED
    --     |POSE_INVALID
    --     |REFERENCE_SPACE_UNSUPPORTED
    --     |RUNTIME_FAILURE
    --     |SESSION_LOST
    --     |SESSION_NOT_READY
    --     |SESSION_NOT_RUNNING
    --     |SESSION_NOT_STOPPING
    --     |SESSION_RUNNING
    --     |SIZE_INSUFFICIENT
    --     |SWAPCHAIN_FORMAT_UNSUPPORTED
    --     |SWAPCHAIN_RECT_INVALID
    --     |SYSTEM_INVALID
    --     |TIME_INVALID
    --     |VALIDATION_FAILURE
    --     |VIEW_CONFIGURATION_TYPE_UNSUPPORTED
    --     |XR_ERROR_FORM_FACTOR_UNAVAILABLE
    --     |XR_ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR
    --     |XR_ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR
    --     deriving (Eq, Show)

    -- data XrType =
    --     XR_TYPE_COMPOSITION_LAYER_PROJECTION
    --     |XR_TYPE_COMPOSITION_LAYER_QUAD
    --     |XR_TYPE_COMPOSITION_LAYER_CUBE_KHR
    --     |XR_TYPE_COMPOSITION_LAYER_CYLINDER_KHR
    --     |XR_TYPE_COMPOSITION_LAYER_EQUIRECT_KHR
    --     deriving (Eq, Show)

    -- createFlags must be 0
    -- £ next: A pointer to an xrInstance of XrGraphicsBindingX
    -- where X may be: D3D12KHR, D3D11KHR, 
    -- OpenGLESAndroidKHR, OpenGLWaylandKHR, 
    -- OpenGLXcbKHR,       OpenGLXlibKHR, 
    -- OpenGLWin32KHR,     VulkanKHR

    -- data XrGraphicsBinding =
    --     XrGraphicsBindingOpenGLESAndroidKHR
    --     |XrGraphicsBindingOpenGLWaylandKHR
    --     |XrGraphicsBindingOpenGLXcbKHR
    --     |XrGraphicsBindingOpenGLXlibKHR
    --     |XrGraphicsBindingOpenGLWin32KHR
    --     |XrGraphicsBindingVulkanKHR
    --     deriving (Eq, Show)

    -- data XrVulkanSwapchainFormatListCreateInfoKHR =
    --     XrVulkanSwapchainFormatListCreateInfoKHR {
    --         vsfType            :: XrStructureType, -- const void* next; 
    --         viewFormatCount :: Int, 
    --         viewFormats     :: VkFormat
    --     } deriving (Eq, Show) --XrVulkanSwapchainFormatListCreateInfoKHR;
    
    -- XR_KHR_visibility_mask [12.16]
    -- This extension enables the following:
    -- data XrVisibilityMask =
    --     XR_VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR
    --     |XR_VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR
    --     |XR_VISIBILITY_MASK_TYPE_LINE_LOOP_KHR