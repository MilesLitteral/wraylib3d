{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
module HRayLib3d.WindowSystem.RenderPipelineWidget () where

import System.IO ()
import GHC.Real  ()
import GHC.Float (float2Int, int2Float)
import Data.Int  (Int16)
import Data.Word (Word8)
import Control.Lens            ()
import Control.Monad.Identity  ()
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
import Control.Monad.IO.Class  ()
import Control.Concurrent      (ThreadId, forkOS)
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Monad.Trans.State.Lazy (StateT)
-- import qualified Data.Massiv.Array as A

-- | Manifest's Interface for speaking to Futhark
-- utilizes Manifut to accomplish this
type WRendererT m a  = StateT m a
-- type WRenderer       = StateT Identity
-- type WRendererIO     = StateT IO

class ToGPU a where
    toGPU :: a -> GPUObject b

instance ToGPU Float where
    toGPU a = GPUObject $ toInt16 a

newtype GPUObject a = GPUObject { gpuRawObject :: Int16 } deriving (Show, Eq)

data RendererHandle = RendererHandle {
  threadID :: ThreadId,
  procSem  :: MVar (Chan PipelineCommand) 
} deriving(Eq, Show)

instance Show (MVar (Chan PipelineCommand)) where
    show a = "MVar (Chan PipelineCommand)"

-- commands send from the front-end to the back end or vice versa
-- the hope is this could condone rendering or GPU enabling/accelerating
-- Monomer
data PipelineCommand
  = AllocRenderer   (GPUObject Int16)  -- {imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
  | ChangeRenderer  Float  --  {imgResp :: MP.Image MP.RGBA Float}
  | GPUSuccess
  | GPUFail
  | GPUExit
  deriving (Eq, Show)

data RenderType 
  = OpenGLRender
  | VulkanRender
  | MetalRender
  | OpenGLESRender
  deriving (Eq, Show)

-- | Takes StateT State and returns a WRayLib3d StateT ApplicationModel
-- class RunInteraction state where 
--     update  :: state -> ApplicationModel -> ApplicationModel 
--     shift   :: state -> StateT PipelineInfo   (StateT state WRendererIO) () -> StateT ApplicationModel (StateT state WRendererIO) () 

continuationPass :: Floating a => a -> (a -> r) -> r
continuationPass a k = k (sqrt a)

-- | Conversion Functions:
-- (Word8 <-> Int16)
toInt16 :: Float -> Int16
toInt16 = fromIntegral . float2Int

toFloat :: Int16 -> Float
toFloat = int2Float    . fromIntegral

-- chunkToInt16 ::  A.Array A.S A.Ix1 [Float]  -> A.Array A.D A.Ix1 [Int16]
-- chunkToInt16 a = A.map toInt16 a

-- chunkToWord8 ::  [Int16]  -> A.Array A.D A.Ix1 [Float]
-- chunkToWord8 a = A.map toFloat a 

gpuProcess :: Chan PipelineCommand -> IO ()
gpuProcess chan = undefined
    -- do 
    -- context <- getContext []
    -- runFutTIn context $ do 
    --     let {doGPUProcess = do
    --             command <- liftIO $ readChan chan
    --             case command of
    --                 GPUExit -> return () 
    --                 _ -> do 
    --                     result <- handlePipelineCommand command
    --                     liftIO $ writeChan chan result --writeChan
    --                     doFutharkProcess
    --         }
    --     doGPUProcess

handlePipelineCommand :: PipelineCommand -> IO PipelineCommand --WRendererIO PipelineCommand
handlePipelineCommand command = do 
    case command of
        ChangeRenderer a ->  return $ AllocRenderer (toGPU a) -- $ ImageResp $ Image result
        AllocRenderer  a ->  return $ GPUSuccess
        GPUExit          ->  return GPUExit
        -- GPUSuccess       ->  warning
        GPUFail          ->  error "var mismatch"

            -- gpuObjB = toGPU  b
            
            -- !futOutput <- E.diffNoAlphaImages futA futB --E.diffImages futA futB
            --run actual futhark diff operation
            -- return result in ImageResp
            -- result <- fromFuthark futOutput

createPipelineHandle :: IO (RendererHandle)
createPipelineHandle = do
    chan <- newChan
    mvar <- newMVar chan
    tid  <- forkOS $ gpuProcess chan
    return $ RendererHandle tid mvar

runPipelineCommand :: RendererHandle -> PipelineCommand -> IO PipelineCommand 
runPipelineCommand fut gcommand = do
    commChan <- takeMVar $ procSem fut
    writeChan commChan gcommand
    result <- readChan commChan
    putMVar (procSem fut) commChan
    return result
