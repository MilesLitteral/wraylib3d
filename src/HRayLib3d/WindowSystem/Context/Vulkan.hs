{-# LANGUAGE CPP, TemplateHaskell, TypeApplications, DataKinds, RecordWildCards #-}
module HRayLib3d.WindowSystem.Context.Vulkan where

-- import Data.IORef
-- import Data.Bits
-- import Data.Maybe 
-- import Data.Semigroup
-- import Data.Time.Clock.System

-- import           Control.Monad
-- import           Control.Exception
-- import qualified Control.Monad.Logger           as Logger
-- import           Language.Haskell.TH

-- import           Foreign.Marshal.Alloc
-- import           Foreign.Marshal.Array
-- import           Foreign.Storable
-- import           Foreign.Ptr 
-- import Graphics.UI.GLFW as GLFW

-- import           Graphics.Vulkan
-- import           Graphics.Vulkan.Core_1_0
-- import           Graphics.Vulkan.Ext.VK_KHR_surface
-- import           Graphics.Vulkan.Ext.VK_KHR_swapchain
-- import           Graphics.Vulkan.Marshal.Create

-- import           Control.Arrow         (first, second)
-- import           Control.Monad         (unless, when)
-- import           Data.Char
-- import           Data.List
-- import           Data.Maybe            (fromMaybe)
-- import           Foreign.Marshal.Array
-- import           GHC.Ptr               (Ptr (..))
-- import           Language.Haskell.TH
-- import           System.Directory
-- import           System.Exit
-- import           System.FilePath
-- import           System.IO
-- import           System.Process

-- data SwapChainImgInfo
--   = SwapChainImgInfo
--   { swapchain   :: VkSwapchainKHR
--   , swImgs      :: [VkImage]
--   , swImgFormat :: VkFormat
--   , swExtent    :: VkExtent2D
--   } deriving (Eq, Show)
  
-- data SwapChainSupportDetails
--   = SwapChainSupportDetails
--   { capabilities :: VkSurfaceCapabilitiesKHR
--   , formats      :: [VkSurfaceFormatKHR]
--   , presentModes :: [VkPresentModeKHR]
--   } deriving (Eq, Show)

-- data DevQueues
--   = DevQueues
--   { graphicsQueue  :: VkQueue
--   , presentQueue   :: VkQueue
--   , qFamIndices    :: Ptr Word32
--   , graphicsFamIdx :: Word32
--   , presentFamIdx  :: Word32
--   } deriving (Eq, Show)

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
-- type Program' a = HRayLib3d.WindowSystem.Context.Vulkan.Program (Either VulkanException a) a

-- -- | Use this to throw all exceptions in this project
-- data VulkanException
--   = VulkanException
--   { vkeCode    :: Maybe VkResult
--   , vkeMessage :: String
--   } deriving (Eq, Show, Read)

-- createVulkanShaderModule :: VkDevice -> (CSize, Ptr Word32) -> IO VkShaderModule
-- createVulkanShaderModule dev (codeSize, codePtr) =
--     withPtr smCreateInfo $ \smciPtr -> alloca $ \smPtr -> do
--       throwingVK "vkCreateShaderModule failed!"
--         $ vkCreateShaderModule dev smciPtr VK_NULL smPtr
--       peek smPtr
--   where
--     smCreateInfo = createVk @VkShaderModuleCreateInfo
--       $  set @"sType"    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
--       &* set @"pNext"    VK_NULL
--       &* set @"codeSize" codeSize
--       &* set @"pCode"    codePtr
--       &* set @"flags"    VK_ZERO_FLAGS

-- destroyVulkanShaderModule :: VkDevice -> VkShaderModule -> IO ()
-- destroyVulkanShaderModule dev = flip (vkDestroyShaderModule dev) VK_NULL

-- -- | Get GLSL shader from file and compile it using @glslangValidator@
-- --   tool if it is available. Panic otherwise :)
-- --
-- --   Type of expression is @Integral a => (a, Ptr Word32)@
-- --   where first value is the code size, second value is a pointer to SPIR-V.
-- --
-- --   Code size it checked to be multiple of 4.
-- compileGLSL :: FilePath -> ExpQ
-- compileGLSL fpath = do
--     (spirvFile,(ec, stdo, stde)) <- runIO $ do

--       validatorExe <-
--         fromMaybe
--           ( error $ unlines
--             [ "Cannot find glslangValidator executable."
--             , "Check if it is available in your $PATH."
--             , "Read more about it at "
--              ++ "https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
--             ]
--           )
--         <$> findExecutable "glslangValidator"


--       tmpDir <- getTemporaryDirectory
--       curDir <- getCurrentDirectory
--       createDirectoryIfMissing True tmpDir
--       let spirvCodeFile = tmpDir </> "haskell-spirv.tmp"
--           shaderFile = curDir </> fpath
--           shaderDir = takeDirectory shaderFile
--           shaderFName = takeFileName shaderFile

--       doesFileExist shaderFile >>= flip unless
--         (error $ "compileGLSL: " ++ shaderFile ++ " does not exist.")

--       doesFileExist spirvCodeFile >>= flip when
--         (removeFile spirvCodeFile)

--       (,) spirvCodeFile <$> readCreateProcessWithExitCode
--         (shell $ validatorExe ++ " -V -o " ++ spirvCodeFile ++ " " ++ shaderFName)
--           { cwd = Just shaderDir
--           } ""

--     runQ . reportGlslMsgs $ unlines [ stdo, stde]

--     case ec of
--       ExitSuccess   -> pure ()
--       ExitFailure i ->
--         error $
--         "glslangValidator exited with code " ++ show i ++ "."

--     contents <- runIO . withBinaryFile spirvFile ReadMode $ \h -> do
--         fsize <- hFileSize h
--         let contentSize = fromIntegral $ case rem fsize 4 of
--               0 -> fsize
--               k -> fsize + 4 - k
--         allocaArray contentSize $ \ptr -> do
--           hasRead <- hGetBuf h ptr contentSize
--           (++ replicate (contentSize - hasRead) 0) <$> peekArray hasRead ptr


--     return $ TupE
-- -- #if MIN_VERSION_template_haskell(2,16,0)
--         --   $ map Just
-- -- #endif
--            [ LitE . IntegerL . fromIntegral $ length contents
--            , AppE (ConE 'Ptr) (LitE $ StringPrimL contents) ]


-- reportGlslMsgs :: String -> Q ()
-- reportGlslMsgs s = case parseValidatorMsgs s of
--   (warns, errs) -> do
--     mapM_ reportWarning warns
--     mapM_ reportError errs

-- parseValidatorMsgs :: String -> ([String], [String])
-- parseValidatorMsgs = go . map strip . lines
--   where
--     strip = dropWhileEnd isSpace . dropWhile isSpace
--     go [] = ([],[])
--     go (x:xs) | "WARNING:" `isPrefixOf` x = first  (strip (drop 8 x):) $ go xs
--               | "ERROR:"   `isPrefixOf` x = second (strip (drop 6 x):) $ go xs
--               | otherwise = go xs

-- -- chooseSwapSurfaceFormat :: SwapChainSupportDetails -> IO VkSurfaceFormatKHR
-- -- chooseSwapSurfaceFormat SwapChainSupportDetails {..}
-- --     = argVal . getMin
-- --     . fromMaybe (throw $ VulkanException Nothing "No available surface formats!")
-- --     . getOption
-- --     . foldMap (Option . Just) <$> mapM fmtCost formats
-- --   where
-- --     argVal (Arg _ b) = b
-- --     fmtCost :: VkSurfaceFormatKHR -> IO (ArgMin Int VkSurfaceFormatKHR)
-- --     fmtCost f = case (getField @"format" f, getField @"colorSpace" f) of
-- --       (VK_FORMAT_UNDEFINED, _) ->
-- --         fmap (Data.Semigroup.Min . Arg 0) $ newVkData $ \sfPtr -> do
-- --           writeField @"format" sfPtr VK_FORMAT_B8G8R8A8_UNORM
-- --           writeField @"colorSpace" sfPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
-- --       (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) ->
-- --         pure . Data.Semigroup.Min $ Arg 1 f
-- --       (_, _) ->
-- --         pure . Data.Semigroup.Min $ Arg 2 f

-- -- chooseSwapPresentMode :: SwapChainSupportDetails -> VkPresentModeKHR
-- -- chooseSwapPresentMode SwapChainSupportDetails {..}
-- --     = argVal . getMin
-- --     . fromMaybe (Data.Semigroup.Min $ Arg 0 VK_PRESENT_MODE_FIFO_KHR)
-- --                 -- VK_PRESENT_MODE_FIFO_KHR is guaranteed to be available
-- --     . getOption
-- --     $ foldMap (Option . Just . pmCost) presentModes
-- --   where
-- --     argVal (Arg _ b) = b
-- --     pmCost :: VkPresentModeKHR -> ArgMin Int VkPresentModeKHR
-- --     pmCost VK_PRESENT_MODE_MAILBOX_KHR   = Data.Semigroup.Min $ Arg 0 VK_PRESENT_MODE_MAILBOX_KHR
-- --     pmCost VK_PRESENT_MODE_IMMEDIATE_KHR = Data.Semigroup.Min $ Arg 1 VK_PRESENT_MODE_IMMEDIATE_KHR
-- --     pmCost VK_PRESENT_MODE_FIFO_KHR      = Data.Semigroup.Min $ Arg 2 VK_PRESENT_MODE_FIFO_KHR
-- --     pmCost pm                            = Data.Semigroup.Min $ Arg 3 pm


-- chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
-- chooseSwapExtent SwapChainSupportDetails {..}
--     = newVkData @VkExtent2D $ \ePtr -> do
--     writeField @"width" ePtr $ max (ew $ getField @"minImageExtent" capabilities)
--                              $ min (ew $ getField @"maxImageExtent" capabilities)
--                                    (ew $ getField @"currentExtent"  capabilities)
--     writeField @"height" ePtr $ max (eh $ getField @"minImageExtent" capabilities)
--                               $ min (eh $ getField @"maxImageExtent" capabilities)
--                                     (eh $ getField @"currentExtent"  capabilities)
--   where
--     ew = getField @"width"
--     eh = getField @"height"


-- createVulkanInstance :: String -- ^ application name
--                      -> String -- ^ engine name
--                      -> [CString]
--                         -- ^ required extensions
--                         --   passed as a list of CStrings, because they are
--                         --   available either via vulkan-api pattern synonyms,
--                         --   or from GLFW
--                      -> [String]
--                         -- ^ required layer names
--                      -> IO VkInstance
-- createVulkanInstance progName engineName extensions layers =
--     withPtr iCreateInfo $ \iciPtr ->
--       alloca $ \vkInstPtr -> do
--         throwingVK "vkCreateInstance: Failed to create vkInstance."
--           $ vkCreateInstance iciPtr VK_NULL vkInstPtr
--         peek vkInstPtr
--   where
--     appInfo = createVk @VkApplicationInfo
--       $  set       @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
--       &* set       @"pNext" VK_NULL
--       &* setStrRef @"pApplicationName" progName
--       &* set       @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
--       &* setStrRef @"pEngineName" engineName
--       &* set       @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
--       &* set       @"apiVersion" (_VK_MAKE_VERSION 1 0 68)

--     iCreateInfo = createVk @VkInstanceCreateInfo
--       $  set           @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
--       &* set           @"pNext" VK_NULL
--       &* setVkRef      @"pApplicationInfo" appInfo
--       &* set           @"enabledLayerCount" (fromIntegral $ length layers)
--       &* setStrListRef @"ppEnabledLayerNames" layers
--       &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
--       &* setListRef    @"ppEnabledExtensionNames" extensions

-- destroyVulkanInstance :: VkInstance -> IO ()
-- destroyVulkanInstance vkInstance = vkDestroyInstance vkInstance VK_NULL

-- -- | Run an action with vulkan instance
-- withVulkanInstance :: String -- ^ program name
--                    -> [CString]
--                       -- ^ required extensions
--                       --   passed as a list of CStrings, because they are
--                       --   available either via vulkan-api pattern synonyms,
--                       --   or from GLFW
--                    -> [String]
--                       -- ^ required layer names
--                    -> (VkInstance -> IO a) -> IO a
-- withVulkanInstance progName extensions layers action = do
--     vkInstance <-
--       createVulkanInstance progName "My perfect Haskell engine"
--                            extensions layers
--     finally (action vkInstance) $ destroyVulkanInstance vkInstance

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


-- withPipelineLayout :: VkDevice -> (VkPipelineLayout -> IO a) -> IO a
-- withPipelineLayout dev action = do
--   pipelineLayout <- withPtr plCreateInfo $ \plciPtr -> alloca $ \plPtr -> do
--     throwingVK "vkCreatePipelineLayout failed!"
--       $ vkCreatePipelineLayout dev plciPtr VK_NULL plPtr
--     peek plPtr
--   finally (action pipelineLayout) $
--     vkDestroyPipelineLayout dev pipelineLayout VK_NULL
--   where
--     plCreateInfo = createVk @VkPipelineLayoutCreateInfo
--       $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
--       &* set @"pNext" VK_NULL
--       &* set @"flags" VK_ZERO_FLAGS
--       &* set @"setLayoutCount"         0       -- Optional
--       &* set @"pSetLayouts"            VK_NULL -- Optional
--       &* set @"pushConstantRangeCount" 0       -- Optional
--       &* set @"pPushConstantRanges"    VK_NULL -- Optional


-- withRenderPass :: VkDevice -> SwapChainImgInfo -> (VkRenderPass -> IO a) -> IO a
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

-- glfwMainLoop :: GLFW.Window -> IO () -> IO ()
-- glfwMainLoop w action = go
--   where
--     go = do
--       should <- GLFW.windowShouldClose w
--       unless should $ GLFW.pollEvents >> action >> go

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

-- -- createVulkanWindow :: IO ()
-- -- createVulkanWindow = do
-- --     initializeAll
-- --     window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
-- --     renderer <- createRenderer window (-1)    defaultRenderer
-- --     icon     <- Vulkan.loadBMP "./assets/WRL3D.bmp"
-- --     setWindowIcon window icon
-- --     appLoop renderer
-- --     destroyWindow window
-- --     -- glSwapWindow window
-- --     quit

-- -- -- | Low latency time in seconds since the start
-- -- getTime :: HRayLib3d.WindowSystem.Context.Program r Double
-- -- getTime = do
-- --     now <- liftIO getSystemTime
-- --     start <- startTime <$> get
-- --     let deltaSeconds = systemSeconds now - systemSeconds start
-- --         -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
-- --         deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds start)
-- --         -- Seconds in Double keep at least microsecond-precision for 285 years.
-- --         -- Float is not good enough even for millisecond-precision over more than a few hours.
-- --         seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
-- --     return seconds

-- -- timeDiff m = (\s e -> realToFrac $ diffUTCTime e s) <$> getCurrentTime <* m <*> getCurrentTime

-- -- | Get size of action output and then get the result,
-- --   performing data copy.
-- asListVK :: Storable x
--          => (Ptr Word32 -> Ptr x -> IO ())
--          -> IO [x]
-- asListVK action = alloca $ \counterPtr -> do
--   action counterPtr VK_NULL_HANDLE
--   counter <- fromIntegral <$> peek counterPtr
--   if counter <= 0
--   then pure []
--   else allocaArray counter $ \valPtr -> do
--     action counterPtr valPtr
--     peekArray counter valPtr

-- -- | Throw VulkanException if something goes wrong
-- throwingVK :: String -> IO VkResult -> IO ()
-- throwingVK msg f = do
--   vkRez <- f
--   when (vkRez < VK_SUCCESS) $ throwIO $ VulkanException (Just vkRez) msg

-- -- | Throw VulkanException without error code
-- throwVKMsg :: String -> IO a
-- throwVKMsg msg = throwIO $ VulkanException Nothing msg
