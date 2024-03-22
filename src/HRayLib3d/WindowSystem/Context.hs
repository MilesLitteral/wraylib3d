{-# LANGUAGE LambdaCase, TypeApplications, RecordWildCards, RankNTypes, ScopedTypeVariables, DataKinds #-}

module HRayLib3d.WindowSystem.Context where 

import Data.IORef
import Data.Time.Clock.System
import qualified Data.Vector.Storable   as SV
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString        as SB 

import           FRP.Elerea.Param
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Logger           as Logger
import qualified Control.Monad.Logger.CallStack as LoggerCS

import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Foreign.Ptr 

import           Graphics.UI.GLFW         (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create


import LambdaCube.IR
import LambdaCube.GL
import LambdaCube.GL.Mesh
import HRayLib3d.GameEngine.RealmViewer.Engine
import HRayLib3d.GameEngine.RealmViewer.Camera
import HRayLib3d.WindowSystem.RenderPipelineClient hiding (frames)
import Graphics.UI.GLFW as GLFW

-- data SwapChainSupportDetails
--   = SwapChainSupportDetails
--   { capabilities :: VkSurfaceCapabilitiesKHR
--   , formats      :: [VkSurfaceFormatKHR]
--   , presentModes :: [VkPresentModeKHR]
--   } deriving (Eq, Show)

-- -- | Use this to throw all exceptions in this project
-- data VulkanException
--   = VulkanException
--   { vkeCode    :: Maybe VkResult
--   , vkeMessage :: String
--   } deriving (Eq, Show, Read)

-- data ProgramState
--   = ProgramState
--   { currentStatus :: VkResult
--     -- ^ Result of the last vulkan command.
--     --   We may need it to check if result is some non-zero non-error code.
--   , loggingFunc   :: Logger.Loc
--                   -> Logger.LogSource
--                   -> Logger.LogLevel
--                   -> Logger.LogStr -> IO ()
--     -- ^ Enable monad-logger.
--   , startTime :: SystemTime
--     -- ^ Time for animations and physics
--   }

-- -- | Program is modelled as a combination of several transformers:
-- --
-- --   * ReaderT + IORef to model state
-- --   * ContT for exception handling and careful resource management
-- --   * ExceptT for exception handling
-- newtype Program r a = Program
--   { unProgram :: IORef ProgramState
--               -> (Either VulkanException a -> IO r) -> IO r }

-- -- | A common case when program parameter @r@ is restricted to be
-- --   either conter of the monad or an error.
-- type Program' a = HRayLib3d.WindowSystem.Context.Program (Either VulkanException a) a

-- withVkShaderStageCI :: VkDevice
--                     -> (CSize, Ptr Word32)
--                     -> VkShaderStageFlagBits
--                     -> (VkPipelineShaderStageCreateInfo -> IO a)
--                     -> IO a
-- withVkShaderStageCI dev shaderCode stageBit action = do
--     shaderModule <- createVulkanShaderModule dev shaderCode
--     let pssCreateInfo = createVk @VkPipelineShaderStageCreateInfo
--           $  set @"sType"  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
--           &* set @"pNext"  VK_NULL
--           &* set @"stage"  stageBit
--           &* set @"module" shaderModule
--           &* setStrRef @"pName" "main"
--     finally (action pssCreateInfo) $ do
--       destroyVulkanShaderModule dev shaderModule
--       touchVkData pssCreateInfo

-- withSwapChain :: VkDevice
--               -> SwapChainSupportDetails
--               -> DevQueues
--               -> VkSurfaceKHR
--               -> (SwapChainImgInfo -> IO a)
--               -> IO a
-- withSwapChain dev scsd queues surf action = do
--   surfFmt <- chooseSwapSurfaceFormat scsd
--   let spMode = chooseSwapPresentMode scsd
--   sExtent <- chooseSwapExtent scsd
--   putStrLn $ "Selected swap surface format: " ++ show surfFmt
--   putStrLn $ "Selected swap present mode: " ++ show spMode
--   putStrLn $ "Selected swap extent: " ++ show sExtent

--   -- try tripple buffering
--   let maxIC = getField @"maxImageCount" $ capabilities scsd
--       minIC = getField @"minImageCount" $ capabilities scsd
--       imageCount = if maxIC <= 0
--                    then max minIC 3
--                    else min maxIC $ max minIC 3

--   -- write VkSwapchainCreateInfoKHR
--   swCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swCreateInfoPtr -> do
--     writeField @"sType"
--       swCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
--     writeField @"pNext"
--       swCreateInfoPtr VK_NULL_HANDLE
--     writeField @"flags"
--       swCreateInfoPtr VK_ZERO_FLAGS
--     writeField @"surface"
--       swCreateInfoPtr surf
--     writeField @"minImageCount"
--       swCreateInfoPtr imageCount
--     writeField @"imageFormat"
--       swCreateInfoPtr (getField @"format" surfFmt)
--     writeField @"imageColorSpace"
--       swCreateInfoPtr (getField @"colorSpace" surfFmt)
--     writeField @"imageExtent"
--       swCreateInfoPtr sExtent
--     writeField @"imageArrayLayers"
--       swCreateInfoPtr 1
--     writeField @"imageUsage"
--       swCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
--     if graphicsQueue queues /= presentQueue queues
--     then do
--       writeField @"imageSharingMode"
--         swCreateInfoPtr VK_SHARING_MODE_CONCURRENT
--       writeField @"queueFamilyIndexCount"
--         swCreateInfoPtr 2
--       writeField @"pQueueFamilyIndices"
--         swCreateInfoPtr (qFamIndices queues)
--     else do
--       writeField @"imageSharingMode"
--         swCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
--       writeField @"queueFamilyIndexCount"
--         swCreateInfoPtr 0
--       writeField @"pQueueFamilyIndices"
--         swCreateInfoPtr VK_NULL_HANDLE
--     writeField @"preTransform"
--       swCreateInfoPtr (getField @"currentTransform" $ capabilities scsd)
--     writeField @"compositeAlpha"
--       swCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
--     writeField @"presentMode"
--       swCreateInfoPtr spMode
--     writeField @"clipped"
--       swCreateInfoPtr VK_TRUE
--     writeField @"oldSwapchain"
--       swCreateInfoPtr VK_NULL_HANDLE

--   swapChain <- alloca $ \swPtr -> do
--     throwingVK "vkCreateSwapchainKHR failed!"
--       $ vkCreateSwapchainKHR dev (unsafePtr swCreateInfo) VK_NULL_HANDLE swPtr
--     peek swPtr

--   swImgs <- asListVK
--     $ \x ->
--       throwingVK "vkGetSwapchainImagesKHR error"
--     . vkGetSwapchainImagesKHR dev swapChain x

--   let swInfo = SwapChainImgInfo
--         { swapchain   = swapChain
--         , swImgs      = swImgs
--         , swImgFormat = getField @"format" surfFmt
--         , swExtent    = sExtent
--         }

--   finally (action swInfo) $ do
--     vkDestroySwapchainKHR dev swapChain VK_NULL_HANDLE
--     touchVkData swCreateInfo

-- withGraphicsPipeline :: VkDevice
--                      -> SwapChainImgInfo
--                      -> [VkPipelineShaderStageCreateInfo]
--                      -> VkRenderPass
--                      -> (VkPipeline -> IO ())
--                      -> IO ()
-- withGraphicsPipeline
--     dev SwapChainImgInfo{..} shaderDescs renderPass action =
--   let -- vertex input
--       vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"vertexBindingDescriptionCount" 0
--         &* set @"pVertexBindingDescriptions" VK_NULL
--         &* set @"vertexAttributeDescriptionCount" 0
--         &* set @"pVertexAttributeDescriptions" VK_NULL

--       -- input assembly
--       inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
--         &* set @"primitiveRestartEnable" VK_FALSE

--       -- viewports and scissors
--       viewPort = createVk @VkViewport
--         $  set @"x" 0
--         &* set @"y" 0
--         &* set @"width" (fromIntegral $ getField @"width" swExtent)
--         &* set @"height" (fromIntegral $ getField @"height" swExtent)
--         &* set @"minDepth" 0
--         &* set @"maxDepth" 1

--       scissor = createVk @VkRect2D
--         $  set   @"extent" swExtent
--         &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

--       viewPortState = createVk @VkPipelineViewportStateCreateInfo
--         $ set @"sType"
--           VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"viewportCount" 1
--         &* setVkRef @"pViewports" viewPort
--         &* set @"scissorCount" 1
--         &* setVkRef @"pScissors" scissor

--       -- rasterizer
--       rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"depthClampEnable" VK_FALSE
--         &* set @"rasterizerDiscardEnable" VK_FALSE
--         &* set @"polygonMode" VK_POLYGON_MODE_FILL
--         &* set @"cullMode" VK_CULL_MODE_BACK_BIT
--         &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
--         &* set @"depthBiasEnable" VK_FALSE
--         &* set @"depthBiasConstantFactor" 0
--         &* set @"depthBiasClamp" 0
--         &* set @"depthBiasSlopeFactor" 0
--         &* set @"lineWidth" 1.0

--       -- multisampling
--       multisampling = createVk @VkPipelineMultisampleStateCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"sampleShadingEnable" VK_FALSE
--         &* set @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT
--         &* set @"minSampleShading" 1.0 -- Optional
--         &* set @"pSampleMask" VK_NULL -- Optional
--         &* set @"alphaToCoverageEnable" VK_FALSE -- Optional
--         &* set @"alphaToOneEnable" VK_FALSE -- Optional

--       -- Depth and stencil testing
--       -- we will pass null pointer in a corresponding place

--       -- color blending
--       colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
--         $  set @"colorWriteMask"
--             (   VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
--             .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT )
--         &* set @"blendEnable" VK_FALSE
--         &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE -- Optional
--         &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO -- Optional
--         &* set @"colorBlendOp" VK_BLEND_OP_ADD -- Optional
--         &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE -- Optional
--         &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO -- Optional
--         &* set @"alphaBlendOp" VK_BLEND_OP_ADD -- Optional

--       colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"logicOpEnable" VK_FALSE
--         &* set @"logicOp" VK_LOGIC_OP_COPY -- Optional
--         &* set @"attachmentCount" 1
--         &* setVkRef @"pAttachments" colorBlendAttachment
--         &* setAt @"blendConstants" @0 0.0 -- Optional
--         &* setAt @"blendConstants" @1 0.0 -- Optional
--         &* setAt @"blendConstants" @2 0.0 -- Optional
--         &* setAt @"blendConstants" @3 0.0 -- Optional


--     -- finally, create pipeline!
--   in withPipelineLayout dev $ \pipelineLayout ->
--       withArrayLen shaderDescs $ \stageCount stagesPtr ->
--         let gpCreateInfo = createVk @VkGraphicsPipelineCreateInfo
--               $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
--               &* set @"pNext" VK_NULL
--               &* set @"flags" VK_ZERO_FLAGS
--               &* set @"stageCount" (fromIntegral stageCount)
--               &* set @"pStages" stagesPtr
--               &* setVkRef @"pVertexInputState" vertexInputInfo
--               &* setVkRef @"pInputAssemblyState" inputAssembly
--               &* set @"pTessellationState" VK_NULL
--               &* setVkRef @"pViewportState" viewPortState
--               &* setVkRef @"pRasterizationState" rasterizer
--               &* setVkRef @"pMultisampleState" multisampling
--               &* set @"pDepthStencilState" VK_NULL
--               &* setVkRef @"pColorBlendState" colorBlending
--               &* set @"pDynamicState" VK_NULL
--               &* set @"layout" pipelineLayout
--               &* set @"renderPass" renderPass
--               &* set @"subpass" 0
--               &* set @"basePipelineHandle" VK_NULL_HANDLE
--               &* set @"basePipelineIndex" (-1)

--         in do
--           createGPfun <- vkGetDeviceProc @VkCreateGraphicsPipelines dev
--           graphicsPipeline <- withPtr gpCreateInfo
--                 $ \gpciPtr -> alloca $ \gpPtr -> do
--             throwingVK "vkCreateGraphicsPipelines failed!"
--               $ createGPfun dev VK_NULL 1 gpciPtr VK_NULL gpPtr
--             peek gpPtr


--           -- again, run an action and touch all allocated objects to make sure
--           -- they are alive at the moment of vulkan object destruction.
--           finally (action graphicsPipeline) $
--             vkDestroyPipeline dev graphicsPipeline VK_NULL


-- withRenderPass :: VkDevice -> SwapChainImgInfo
--                -> (VkRenderPass -> IO a) -> IO a
-- withRenderPass dev SwapChainImgInfo{..} action =
--   let -- attachment description
--       colorAttachment = createVk @VkAttachmentDescription
--         $  set @"flags" VK_ZERO_FLAGS
--         &* set @"format" swImgFormat
--         &* set @"samples" VK_SAMPLE_COUNT_1_BIT
--         &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
--         &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
--         &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
--         &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
--         &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
--         &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

--       -- subpasses and attachment references
--       colorAttachmentRef = createVk @VkAttachmentReference
--         $  set @"attachment" 0
--         &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

--       subpass = createVk @VkSubpassDescription
--         $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
--         &* set @"colorAttachmentCount" 1
--         &* setVkRef @"pColorAttachments" colorAttachmentRef
--         &* set @"pPreserveAttachments" VK_NULL
--         &* set @"pInputAttachments" VK_NULL

--       -- subpass dependencies
--       dependency = createVk @VkSubpassDependency
--         $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
--         &* set @"dstSubpass" 0
--         &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
--         &* set @"srcAccessMask" VK_ZERO_FLAGS
--         &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
--         &* set @"dstAccessMask"
--             (   VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
--             .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )

--       -- render pass
--       rpCreateInfo = createVk @VkRenderPassCreateInfo
--         $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"attachmentCount" 1
--         &* setVkRef @"pAttachments" colorAttachment
--         &* set @"subpassCount" 1
--         &* setVkRef @"pSubpasses" subpass
--         &* set @"dependencyCount" 1
--         &* setVkRef @"pDependencies" dependency
--   in do
--     renderPass <- withPtr rpCreateInfo $ \rpciPtr -> alloca $ \rpPtr -> do
--       throwingVK "vkCreatePipelineLayout failed!"
--         $ vkCreateRenderPass dev rpciPtr VK_NULL rpPtr
--       peek rpPtr
--     finally (action renderPass) $
--       vkDestroyRenderPass dev renderPass VK_NULL

-- -- | Low latency time in seconds since the start
-- getTime :: HRayLib3d.WindowSystem.Context.Program r Double
-- getTime = do
--     now <- liftIO getSystemTime
--     start <- startTime <$> get
--     let deltaSeconds = systemSeconds now - systemSeconds start
--         -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
--         deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds start)
--         -- Seconds in Double keep at least microsecond-precision for 285 years.
--         -- Float is not good enough even for millisecond-precision over more than a few hours.
--         seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
--     return seconds

-- -- | Throw VulkanException if something goes wrong
-- throwingVK :: String
--            -> IO VkResult
--            -> IO ()
-- throwingVK msg f = do
--   vkRez <- f
--   when (vkRez < VK_SUCCESS) $ throwIO $ VulkanException (Just vkRez) msg

-- -- | Throw VulkanException without error code
-- throwVKMsg :: String -> IO a
-- throwVKMsg msg = throwIO $ VulkanException Nothing msg

-- data SwapChainImgInfo
--   = SwapChainImgInfo
--   { swapchain   :: VkSwapchainKHR
--   , swImgs      :: [VkImage]
--   , swImgFormat :: VkFormat
--   , swExtent    :: VkExtent2D
--   } deriving (Eq, Show)
  
-- data DevQueues
--   = DevQueues
--   { graphicsQueue :: VkQueue
--   , presentQueue  :: VkQueue
--   , qFamIndices   :: Ptr Word32
--   } deriving (Eq, Show)

-- FPS tracking
data State = State { frames :: IORef Int, t0 :: IORef Double }

-- data RenderData
--   = RenderData
--   { renderFinished :: VkSemaphore
--   , imageAvailable :: VkSemaphore
--   , device         :: VkDevice
--   , swapChainInfo  :: SwapChainImgInfo
--   , deviceQueues   :: DevQueues
--   , imgIndexPtr    :: Ptr Word32
--   , commandBuffers :: [VkCommandBuffer]
--   }

-- data GPUData
--   = GPUData
--   { gpuTextures :: SV.Vector TextureData
--   , gpuMeshes   :: SV.Vector GPUMesh
--   }

-- allocateGPUData RenderJob{..} = GPUData <$> mapM uploadTex2D textures <*> mapM uploadMeshToGPU meshes
--   where uploadTex2D = uploadTexture2DToGPU . either error id . decodeImage . either error id . B64.decode . pack

-- disposeGPUData GPUData{..} = mapM_ disposeTexture gpuTextures >> mapM_ disposeMesh gpuMeshes

-- timeDiff m = (\s e -> realToFrac $ diffUTCTime e s) <$> getCurrentTime <* m <*> getCurrentTime

-- setUniformValue name = \case
--   VBool v   -> pack name @= return v
--   VV2B v    -> pack name @= return v
--   VV3B v    -> pack name @= return v
--   VV4B v    -> pack name @= return v
--   VWord v   -> pack name @= return v
--   VV2U v    -> pack name @= return v
--   VV3U v    -> pack name @= return v
--   VV4U v    -> pack name @= return v
--   VInt v    -> pack name @= return v
--   VV2I v    -> pack name @= return v
--   VV3I v    -> pack name @= return v
--   VV4I v    -> pack name @= return v
--   VFloat v  -> pack name @= return v
--   VV2F v    -> pack name @= return v
--   VV3F v    -> pack name @= return v
--   VV4F v    -> pack name @= return v
--   VM22F v   -> pack name @= return v
--   VM23F v   -> pack name @= return v
--   VM24F v   -> pack name @= return v
--   VM32F v   -> pack name @= return v
--   VM33F v   -> pack name @= return v
--   VM34F v   -> pack name @= return v
--   VM42F v   -> pack name @= return v
--   VM43F v   -> pack name @= return v
--   VM44F v   -> pack name @= return v

captureRate :: Double
captureRate = 30

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
  let t = 1000*t1
      fR = frames state
      tR = t0 state
  modifyIORef fR (+1)
  t0' <- readIORef tR
  writeIORef tR $ t0' + t
  when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ show seconds)
    writeIORef tR 0
    writeIORef fR 0

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

upEdge :: Signal Bool -> SignalGen p (Signal Bool)
upEdge s = transfer2 False (\_ cur prev _ -> cur && prev == False) s =<< delay False s

windowWidth, windowHeight :: Num a => a
windowWidth  = 800
windowHeight = 600

glfwMainLoop :: GLFW.Window -> IO () -> IO ()
glfwMainLoop w action = go
  where
    go = do
      should <- GLFW.windowShouldClose w
      unless should $ GLFW.pollEvents >> action >> go

-- sdl2 utility code
initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win

-- getFrameBuffer w h = do
--   glFinish
--   glBindFramebuffer GL_READ_FRAMEBUFFER 0
--   glReadBuffer GL_FRONT_LEFT
--   glBlitFramebuffer 0 0 (fromIntegral w) (fromIntegral h) 0 (fromIntegral h) (fromIntegral w) 0 GL_COLOR_BUFFER_BIT GL_NEAREST
--   glReadBuffer GL_BACK_LEFT
--   withFrameBuffer 0 0 w h $ \p -> SB.packCStringLen (castPtr p,w*h*4)

-- withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO a) -> IO a
-- withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
--     glPixelStorei GL_UNPACK_LSB_FIRST    0
--     glPixelStorei GL_UNPACK_SWAP_BYTES   0
--     glPixelStorei GL_UNPACK_ROW_LENGTH   0
--     glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
--     glPixelStorei GL_UNPACK_SKIP_ROWS    0
--     glPixelStorei GL_UNPACK_SKIP_PIXELS  0
--     glPixelStorei GL_UNPACK_SKIP_IMAGES  0
--     glPixelStorei GL_UNPACK_ALIGNMENT    1 -- normally 4!
--     glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
--     fn p

-- --Vulkan Make Window
-- withGLFWWindow :: String -> (GLFW.Window -> IO ()) -> IO ()
-- withGLFWWindow title action = do
--   GLFW.init >>= flip unless
--     (throwVKMsg "Failed to initialize GLFW.")

--   -- even if something bad happens, we need to terminate GLFW
--   flip finally (GLFW.terminate >> putStrLn "Terminated GLFW.") $ do
--     GLFW.getVersionString >>= mapM_ (putStrLn . ("GLFW version: " ++))

--     GLFW.vulkanSupported >>= flip unless
--       (throwVKMsg "GLFW reports that vulkan is not supported!")

--     GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
--     GLFW.windowHint $ WindowHint'Resizable False

--     mw <- GLFW.createWindow 800 600 title Nothing Nothing
--     case mw of
--       Nothing -> throwVKMsg "Failed to initialize GLFW window."
--       Just w  -> do
--         putStrLn "Initialized GLFW window."
--         finally (action w)
--                 (GLFW.destroyWindow w >> putStrLn "Closed GLFW window.")

-- -- Vulkan Boilerplate
-- isDeviceSuitable :: VkPhysicalDevice -> IO Bool
-- isDeviceSuitable _ = pure True

-- pickPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
-- pickPhysicalDevice vkInstance = do
--     devs <- alloca $ \deviceCountPtr -> do
--       throwingVK "pickPhysicalDevice: Failed to enumerate physical devices."
--         $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr VK_NULL_HANDLE
--       devCount <- fromIntegral <$> peek deviceCountPtr
--       when (devCount <= 0) $ throwVKMsg "Zero device count!"
--       putStrLn $ "Found " ++ show devCount ++ " devices."

--       allocaArray devCount $ \devicesPtr -> do
--         throwingVK "pickPhysicalDevice: Failed to enumerate physical devices."
--           $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr devicesPtr
--         peekArray devCount devicesPtr

--     selectFirstSuitable devs
--   where
--     selectFirstSuitable [] = throwVKMsg "No suitable devices!"
--     selectFirstSuitable (x:xs) = isDeviceSuitable x >>= \yes ->
--       if yes then pure x
--              else selectFirstSuitable xs

-- withGLFWVulkanInstance :: String -> (VkInstance -> IO a) -> IO a
-- withGLFWVulkanInstance progName action = do
--     -- get required extension names from GLFW
--     glfwReqExts <- GLFW.getRequiredInstanceExtensions
--     withVulkanInstance
--       progName
--       glfwReqExts
--       ["VK_LAYER_LUNARG_standard_validation"]
--       action
    
-- runWithVk :: String -> IO ()
-- runWithVk windowTitle = withGLFWWindow windowTitle $ \window ->
--        withGLFWVulkanInstance windowTitle $ \vulkanInstance -> do
--           dev <- pickPhysicalDevice vulkanInstance
--           putStrLn $ "Selected device: " ++ show dev
--           glfwMainLoop window (return ())

scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef = do
    time  <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
        bsp = getBSP levelData
        p0 = head . drop 1 . cycle $ getSpawnPoints levelData
    fblrPress' <- do
      j' <- upEdge $ (\(w,a,s,d,t,j) -> j) <$> fblrPress
      return $ (\(w,a,s,d,t,_) j' -> (w,a,s,d,t,j')) <$> fblrPress <*> j'
    controlledCamera <- userCamera (getTeleportFun levelData) bsp p0 mouseMove fblrPress'

    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)
    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) capturePress =<< delay False capturePress
    
    {-
    [clearWaypoints, setWaypoint, stopPlayback, startPlayback, incPlaybackSpeed, decPlaybackSpeed] <-
        forM (zip [edge, edge, edge, edge, return, return] [0..]) $ \(process, i) -> process (fmap (!! i) waypointPress)

    waypoints <- recordSignalSamples setWaypoint clearWaypoints ((\(camPos, targetPos, _, _) -> (camPos, targetPos)) <$> controlledCamera)
    playbackSpeed <- transfer2 100 (\dt inc dec speed -> speed + 10*dt*(if inc then 1 else if dec then -1 else 0)) incPlaybackSpeed decPlaybackSpeed
    splineCamera <- playbackCamera startPlayback stopPlayback playbackSpeed waypoints
    let activeCamera = do
            camData <- splineCamera
            case camData of
                Nothing -> controlledCamera
                Just camData -> return camData
    -}
    let activeCamera = controlledCamera
        setupGFX (camPos,camTarget,camUp,brushIndex) time (capturing,frameCount) = do
            (w,h) <- getFramebufferSize win
            -- hack
            let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
            noBSPCull <- keyIsPressed (Key'X)
            debugRender <- keyIsPressed (Key'C)
            updateRenderInput graphicsData (camPos,camTarget,camUp) w h time noBSPCull
            {-
            when (not $ null brushIndex) $ do
              putStrLn $ "brush collision: " ++ show (map (getModelIndexFromBrushIndex levelData) brushIndex)
            -}
-- #ifdef CAPTURE
--             let  captureA = do
--                   when capturing $ do
--                       glFinish
--                       withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
--                   writeIORef capRef capturing
-- #endif
--                  return ()
--            return (captureA,debugRender)
    r <- effectful3 setupGFX activeCamera time ((,) <$> capture <*> frameCount)
    return r

readInput compileRequest compileReady pplName rendererRef storage win s mousePos fblrPress capturePress waypointPress capRef = do
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
    t <- maybe 0 id <$> getTime
    setTime 0

    (x,y) <- getCursorPos win
    mousePos (realToFrac x,realToFrac y)

    fblrPress =<< ((,,,,,) <$> keyIsPressed Key'A <*> keyIsPressed Key'W <*> keyIsPressed Key'S <*> keyIsPressed Key'D
                           <*> keyIsPressed Key'RightShift <*> keyIsPressed Key'Space)
    capturePress =<< keyIsPressed Key'P
    waypointPress =<< mapM keyIsPressed [Key'R,Key'E,Key'1,Key'2,Key'F,Key'G]

    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    updateFPS s dt

    reload <- keyIsPressed Key'L
    when reload $ writeIORef compileRequest True
    readIORef compileReady >>= \case
      False -> return ()
      True -> do
        writeIORef compileReady False
        loadQuake3Graphics storage pplName >>= \case
          Nothing -> return ()
          Just a  -> do
            readIORef rendererRef >>= disposeRenderer
            writeIORef rendererRef a
    k <- keyIsPressed Key'Escape
    return $ if k then Nothing else Just (min 0.1 $ realToFrac dt) -- simulation must run at least 10 FPS, under 10 FPS won't be realtime

-- -- 
-- main :: IO ()
-- main = withGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"
--           $ \window ->
--        withGLFWVulkanInstance "05-GraphicsPipeline" $ \vulkanInstance ->
--        withSurface vulkanInstance window $ \vulkanSurface -> do
--         (Just scsd, pdev)
--           <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
--         withGraphicsDevice pdev vulkanSurface $ \dev queues ->
--           withSwapChain dev scsd queues vulkanSurface $ \swInfo ->
--           withImageViews dev swInfo $ \imgViews ->
--           withVkShaderStageCI dev
--               $(compileGLSL "shaders/triangle.vert")
--               VK_SHADER_STAGE_VERTEX_BIT
--               $ \shaderVert ->
--           withVkShaderStageCI dev
--               $(compileGLSL "shaders/triangle.frag")
--               VK_SHADER_STAGE_FRAGMENT_BIT
--               $ \shaderFrag ->
--           withRenderPass dev swInfo $ \renderPass ->
--           withGraphicsPipeline dev swInfo [shaderVert, shaderFrag] renderPass
--               $ \graphicsPipeline ->
--           withFramebuffers dev renderPass swInfo imgViews
--               $ \framebuffers ->
--           withCommandPool dev queues $ \commandPool ->
--           withCommandBuffers dev graphicsPipeline commandPool
--                              renderPass swInfo framebuffers
--               $ \cmdBuffers ->
--           withSemaphore dev $ \rendFinS ->
--           withSemaphore dev $ \imAvailS ->
--           alloca $ \imgIPtr -> do
--             let rdata = RenderData
--                   { renderFinished = rendFinS
--                   , imageAvailable = imAvailS
--                   , device         = dev
--                   , swapChainInfo  = swInfo
--                   , deviceQueues   = queues
--                   , imgIndexPtr    = imgIPtr
--                   , commandBuffers = cmdBuffers
--                   }
--             putStrLn $ "Selected physical device: " ++ show pdev
--             putStrLn $ "Createad surface: " ++ show vulkanSurface
--             putStrLn $ "Createad device: " ++ show dev
--             putStrLn $ "Createad queues: " ++ show queues
--             putStrLn $ "Createad swapchain: " ++ show swInfo
--             putStrLn $ "Createad image views: " ++ show imgViews
--             putStrLn $ "Createad vertex shader module: " ++ show shaderVert
--             putStrLn $ "Createad fragment shader module: " ++ show shaderFrag
--             putStrLn $ "Createad renderpass: " ++ show renderPass
--             putStrLn $ "Createad pipeline: " ++ show graphicsPipeline
--             putStrLn $ "Createad framebuffers: " ++ show framebuffers
--             putStrLn $ "Createad command pool: " ++ show commandPool
--             putStrLn $ "Createad command buffers: " ++ show cmdBuffers
--             glfwMainLoop window $ do
--               return () -- do some app logic
--               throwingVK "vkQueueWaitIdle failed!"
--                 $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata
--               drawFrame rdata
--             throwingVK "vkDeviceWaitIdle failed!"
--               $ vkDeviceWaitIdle dev

-- withFramebuffers :: VkDevice
--                  -> VkRenderPass
--                  -> SwapChainImgInfo
--                  -> [VkImageView]
--                  -> ([VkFramebuffer] -> IO a)
--                  -> IO a
-- withFramebuffers dev renderPass SwapChainImgInfo{..} imgviews action = do
--     bufs <- mapM createFB imgviews
--     finally (action bufs) $
--       forM_ bufs $ \fb ->
--         vkDestroyFramebuffer dev fb VK_NULL_HANDLE
--   where
--     createFB imgView =
--       let fbci = createVk
--             $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
--             &* set @"pNext" VK_NULL
--             &* set @"flags" VK_ZERO_FLAGS
--             &* set @"renderPass" renderPass
--             &* set @"attachmentCount" 1
--             &* setListRef @"pAttachments" [imgView]
--             &* set @"width" (getField @"width" swExtent)
--             &* set @"height" (getField @"height" swExtent)
--             &* set @"layers" 1
--       in alloca $ \fbPtr -> withPtr fbci $ \fbciPtr -> do
--         throwingVK "vkCreateFramebuffer failed!"
--           $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr
--         peek fbPtr

-- withCommandPool :: VkDevice -> DevQueues
--                 -> (VkCommandPool -> IO a)
--                 -> IO a
-- withCommandPool dev DevQueues{..} action = do

--   commandPool <- alloca $ \pPtr -> do
--     withPtr
--       ( createVk
--         $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--         &* set @"queueFamilyIndex" graphicsFamIdx
--       ) $ \ciPtr ->
--       throwingVK "vkCreateCommandPool failed!"
--         $ vkCreateCommandPool dev ciPtr VK_NULL pPtr
--     peek pPtr

--   finally (action commandPool) $
--     vkDestroyCommandPool dev commandPool VK_NULL

-- withCommandBuffers :: VkDevice
--                    -> VkPipeline
--                    -> VkCommandPool
--                    -> VkRenderPass
--                    -> SwapChainImgInfo
--                    -> [VkFramebuffer]
--                    -> ([VkCommandBuffer] -> IO a)
--                    -> IO a
-- withCommandBuffers
--     dev pipeline commandPool rpass  SwapChainImgInfo{..} fbs action
--   | buffersCount <- length fbs =
--   -- allocate a pointer to an array of command buffer handles
--   allocaArray buffersCount $ \cbsPtr -> do

--     let allocInfo = createVk @VkCommandBufferAllocateInfo
--           $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
--           &* set @"pNext" VK_NULL
--           &* set @"commandPool" commandPool
--           &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
--           &* set @"commandBufferCount" (fromIntegral buffersCount)

--     withPtr allocInfo $ \aiPtr ->
--       throwingVK "vkAllocateCommandBuffers failed!"
--         $ vkAllocateCommandBuffers dev aiPtr cbsPtr
--     commandBuffers <- peekArray buffersCount cbsPtr

--     -- record command buffers
--     forM_ (zip fbs commandBuffers) $ \(frameBuffer, cmdBuffer) -> do

--       -- begin commands
--       let cmdBufBeginInfo = createVk @VkCommandBufferBeginInfo
--             $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
--             &* set @"pNext" VK_NULL
--             &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

--       withPtr cmdBufBeginInfo
--         $ throwingVK "vkBeginCommandBuffer failed!"
--         . vkBeginCommandBuffer cmdBuffer

--       -- render pass
--       let renderPassBeginInfo = createVk @VkRenderPassBeginInfo
--             $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
--             &* set @"pNext" VK_NULL
--             &* set @"renderPass" rpass
--             &* set @"framebuffer" frameBuffer
--             &* setVk @"renderArea"
--                 (  setVk @"offset"
--                    ( set @"x" 0 &* set @"y" 0 )
--                 &* set @"extent" swExtent
--                 )
--             &* set @"clearValueCount" 1
--             &* setVkRef @"pClearValues"
--                ( createVk $ setVk @"color"
--                   $  setAt @"float32" @0 0
--                   &* setAt @"float32" @1 0
--                   &* setAt @"float32" @2 0.2
--                   &* setAt @"float32" @3 1
--                )

--       withPtr renderPassBeginInfo $ \rpibPtr ->
--         vkCmdBeginRenderPass cmdBuffer rpibPtr VK_SUBPASS_CONTENTS_INLINE

--       -- basic drawing commands
--       vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
--       vkCmdDraw cmdBuffer 3 1 0 0

--       -- finishing up
--       vkCmdEndRenderPass cmdBuffer

--       throwingVK "vkEndCommandBuffer failed!"
--         $ vkEndCommandBuffer cmdBuffer


--     finally (action commandBuffers) $
--       vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr

-- withSemaphore :: VkDevice
--               -> (VkSemaphore -> IO a)
--               -> IO a
-- withSemaphore dev action = do

--   semaphore <- alloca $ \sPtr -> do
--     withPtr
--       ( createVk
--         $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
--         &* set @"pNext" VK_NULL
--         &* set @"flags" VK_ZERO_FLAGS
--       ) $ \ciPtr -> throwingVK "vkCreateSemaphore failed!"
--                       $ vkCreateSemaphore dev ciPtr VK_NULL sPtr
--     peek sPtr

--   finally (action semaphore) $
--     vkDestroySemaphore dev semaphore VK_NULL

-- drawFrame :: RenderData -> IO ()
-- drawFrame RenderData {..} =
--     withArray commandBuffers
--       $ \commandBuffersPtr -> do

--     -- Acquiring an image from the swap chain
--     throwingVK "vkAcquireNextImageKHR failed!"
--       $ vkAcquireNextImageKHR
--           device swapchain maxBound
--           imageAvailable VK_NULL_HANDLE imgIndexPtr
--     bufPtr <- (\i -> commandBuffersPtr `plusPtr`
--                         (fromIntegral i * sizeOf (undefined :: VkCommandBuffer))
--               ) <$> peek imgIndexPtr

--     -- Submitting the command buffer
--     withPtr (mkSubmitInfo bufPtr) $ \siPtr ->
--       throwingVK "vkQueueSubmit failed!"
--         $ vkQueueSubmit graphicsQueue 1 siPtr VK_NULL


--     -- RENDERRR!!!
--     withPtr presentInfo $
--       throwingVK "vkQueuePresentKHR failed!" . vkQueuePresentKHR presentQueue
--   where
--     SwapChainImgInfo {..} = swapChainInfo
--     DevQueues {..} = deviceQueues
--     -- Submitting the command buffer
--     mkSubmitInfo bufPtr = createVk @VkSubmitInfo
--       $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
--       &* set @"pNext" VK_NULL
--       &* set @"waitSemaphoreCount" 1
--       &* setListRef @"pWaitSemaphores"   [imageAvailable]
--       &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
--       &* set @"commandBufferCount" 1
--       &* set @"pCommandBuffers" bufPtr
--       &* set @"signalSemaphoreCount" 1
--       &* setListRef @"pSignalSemaphores" [renderFinished]
--     -- Presentation
--     presentInfo = createVk @VkPresentInfoKHR
--       $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
--       &* set @"pNext" VK_NULL
--       &* set @"pImageIndices" imgIndexPtr
--       &* set        @"waitSemaphoreCount" 1
--       &* setListRef @"pWaitSemaphores" [renderFinished]
--       &* set        @"swapchainCount" 1
--       &* setListRef @"pSwapchains"    [swapchain]
      
-- -- Handle Events
-- appLoop :: Renderer -> IO ()
-- appLoop renderer = do
--     events <- pollEvents
--     let eventIsQPress event =
--             case eventPayload event of
--             KeyboardEvent keyboardEvent -> keyboardEventKeyMotion keyboardEvent == Pressed && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
--             _ -> False
--         qPressed = any eventIsQPress events
--     rendererDrawColor renderer $= V4 100 0 255 255
--     -- renderPrimitive  renderer $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
--     -- SDL.glSwapWindow window
--     clear   renderer
--     present renderer
--     unless qPressed (appLoop renderer)

-- createSDLWindow :: IO ()
-- createSDLWindow = do
--     initializeAll
--     window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
--     renderer <- createRenderer window (-1)    defaultRenderer
--     icon     <- SDL.loadBMP "./assets/WRL3D.bmp"
--     setWindowIcon window icon
--     appLoop renderer
--     destroyWindow window
--     -- glSwapWindow window
--     quit

-- createVulkanWindow :: IO ()
-- createVulkanWindow = do
--     initializeAll
--     window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
--     renderer <- createRenderer window (-1)    defaultRenderer
--     icon     <- Vulkan.loadBMP "./assets/WRL3D.bmp"
--     setWindowIcon window icon
--     appLoop renderer
--     destroyWindow window
--     -- glSwapWindow window
--     quit