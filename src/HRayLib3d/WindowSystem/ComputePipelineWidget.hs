{-# LANGUAGE FlexibleInstances #-}
module HRayLib3d.WindowSystem.ComputePipelineWidget where 
    
import System.IO ()
import GHC.Real  ()
import GHC.Float (float2Int, int2Float)
import Data.Int  (Int16)
import Data.Word (Word8)
import Control.Lens            ()
import Control.Monad.Identity  ()
import Control.Monad.IO.Class  ()
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Concurrent      (ThreadId, forkOS)
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
-- import qualified Data.Massiv.Array as A

-- TODO Create a GPU State Transformer.
-- This can be a means for a Compute Pipeline
-- ML in Gaming!
-- | Takes StateT State and returns a WRayLib3d StateT ApplicationModel
-- class RunInteraction state where 
--     update  :: state -> ApplicationModel -> ApplicationModel 
--     shift   :: state -> StateT PipelineInfo   (StateT state WRendererIO) () -> StateT ApplicationModel (StateT state WRendererIO) () 

class    ToGPU a where
    toGPU :: a -> GPUObject b

instance ToGPU Float where
    toGPU a = GPUObject $ toInt16 a

newtype GPUObject a = GPUObject { gpuRawObject :: Int16 } deriving (Show, Eq)

data RendererHandle = RendererHandle {
  threadID :: ThreadId,
  procSem  :: MVar (Chan ComputePipelineCommand) 
} deriving(Eq, Show)

-- commands send from the front-end to the back end or vice versa
-- the hope is this could condone rendering or GPU enabling/accelerating
-- Monomer
data ComputePipelineCommand
  = AllocRenderer   (GPUObject Int16)   -- {imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
  | ChangeRenderer  Float               --  {imgResp :: MP.Image MP.RGBA Float}
  | GPUSuccess
  | GPUFail
  | GPUExit
  deriving (Eq, Show)

instance Show (MVar (Chan ComputePipelineCommand)) where
    show a = "MVar (Chan ComputePipelineCommand)"

continuationPass :: Floating a => a -> (a -> r) -> r
continuationPass a k = k (sqrt a)

-- | Conversion Functions:
-- (Word8 <-> Int16)
toInt16 :: Float -> Int16
toInt16 = fromIntegral . float2Int

toFloat :: Int16 -> Float
toFloat = int2Float    . fromIntegral

chunkToInt16 ::  [Float] -> [Int16]
chunkToInt16 a = map toInt16 a

chunkToWord8 ::  [Int16]  -> [Float]
chunkToWord8 a = map toFloat a 

gpuProcess :: Chan ComputePipelineCommand -> IO ()
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

handlePipelineCommand :: ComputePipelineCommand -> IO ComputePipelineCommand --WRendererIO PipelineCommand
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

runPipelineCommand :: RendererHandle -> ComputePipelineCommand -> IO ComputePipelineCommand 
runPipelineCommand fut gcommand = do
    commChan <- takeMVar $ procSem fut
    writeChan commChan gcommand
    result <- readChan commChan
    putMVar (procSem fut) commChan
    return result

-- | Interface for speaking to GPU (Compute Pipeline)
-- type WRendererT m a  = StateT m a
-- type WRenderer       = StateT Identity
-- type WRendererIO     = StateT IO

-- newtype RenT c m a = RenT (Context -> m a)
-- type Ren     c     = WRendererTIn c Identity
-- type RenIO   c     = WRendererTIn c IO

-- instance MonadTrans (RenT c) where
--     lift a = RenT (\_ -> a)
--     {-# INLINEABLE lift #-}

-- instance Functor m => Functor (RenT c m) where
--     fmap f (RenT a) = RenT (fmap f.a)
--     {-# INLINEABLE fmap #-}

-- instance Applicative m => Applicative (RenT c m) where
--     pure a = RenT (\_ -> pure a)
--     (<*>) (RenT a) (RenT b) = RenT (\c -> a c <*> b c)
--     {-# INLINEABLE pure #-}
--     {-# INLINEABLE (<*>) #-}

-- instance Monad m => Monad (RenT c m) where
--     (>>=) (RenT a) f = RenT (\c -> a c >>= (\(RenT b) -> b c) . f)
--     {-# INLINEABLE (>>=) #-}

-- instance (MonadBase b m) => MonadBase b (RenT c m) where
--     liftBase = liftBaseDefault
--     {-# INLINEABLE liftBase #-}

-- instance MonadTransControl (RenT c) where
--     type StT (RenT c) a = a
--     liftWith a = RenT (\c -> a (\(RenT a') -> a' c))
--     restoreT = lift
--     {-# INLINEABLE liftWith #-}
--     {-# INLINEABLE restoreT #-}

-- instance MonadBaseControl b m => MonadBaseControl b (RenT c m) where
--     type StM (RenT c m) a = ComposeSt (RenT c) m a
--     liftBaseWith = defaultLiftBaseWith
--     restoreM = defaultRestoreM
--     {-# INLINEABLE liftBaseWith #-}
--     {-# INLINEABLE restoreM #-}

-- class FutharkObject wrapped raw | wrapped -> raw, raw -> wrapped where
--     wrapFO :: MVar Int -> ForeignPtr raw -> wrapped c
--     freeFO :: Ptr Raw.Futhark_context -> Ptr raw -> IO Int
--     fromFO :: wrapped c -> (MVar Int, ForeignPtr raw)
    
-- withFO :: FutharkObject wrapped raw => wrapped c -> (Ptr raw -> IO b) -> IO b
-- withFO = withForeignPtr . snd . fromFO

-- addReferenceFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped c -> RenT c m ()
-- addReferenceFO fo = lift . liftIO $
--     let (referenceCounter, _) = fromFO fo
--      in modifyMVar_ referenceCounter (\r -> pure (r+1))

-- finalizeFO :: (MonadIO m, FutharkObject wrapped raw) => wrapped c -> RenT c m ()
-- finalizeFO fo = lift . liftIO $
--     let (referenceCounter, pointer) = fromFO fo
--      in modifyMVar_ referenceCounter (\r 
--      -> if r > 0 
--             then pure (r-1) 
--             else finalizeForeignPtr pointer >> pure 0)

-- class (FutharkObject array rawArray, Storable element, M.Index dim) 
--     => FutharkArray array rawArray dim element 
--     | array -> dim, array -> element 
--     where
--         shapeFA  :: Ptr Raw.Futhark_context -> Ptr rawArray -> IO (M.Sz dim)
--         newFA    :: Ptr Raw.Futhark_context -> Ptr element -> M.Sz dim -> IO (Ptr rawArray)
--         valuesFA :: Ptr Raw.Futhark_context -> Ptr rawArray -> Ptr element -> IO Int 

-- class Input fo ho where
--     toFuthark   :: Monad m => ho -> RenT c m (fo c)

-- class Output fo ho where
--     fromFuthark :: Monad m => fo c -> RenT c m ho

-- data Context = Context (MVar Int) (ForeignPtr Raw.Futhark_context)



-- getContext :: [ContextOption] -> IO Context
-- getContext options = do
--     config <- Raw.context_config_new
--     mapM_ (setOption config) options
--     context <- Raw.context_new config
--     childCount <- newMVar 0
--     fmap (Context childCount)
--         $ FC.newForeignPtr context 
--         $ (forkIO $ freeContext childCount config context)
--         >> return ()

-- freeContext childCount config context 
--     = readMVar childCount >>= \n 
--     -> if n == 0 
--         then do Raw.context_free context
--                 Raw.context_config_free config
--         else yield >> freeContext childCount config context

-- inContext (Context _ fp) = withForeignPtr fp

-- getError context = do
--     cs <- inContext context Raw.context_get_error
--     s <- peekCString cs
--     F.free cs
--     error s

-- clearError context = inContext context Raw.context_get_error >>= F.free

-- clearCache context
--     = inContext context Raw.context_clear_caches >>= \code 
--     -> if code == 0 
--         then return ()
--         else getError context

-- syncContext context 
--     = inContext context Raw.context_sync >>= \code 
--     -> if code == 0 
--         then return ()
--         else getError context

-- inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
-- inContextWithError context f = do
--     code <- attempt
--     case code of
--         0 -> success
--         1 -> generalError
--         2 -> programError
--         3 -> outOfMemoryError
--         _ -> unknownError
--     where
--         attempt = inContext context f
--         success = return ()
--         failure = getError context
--         generalError = failure
--         programError = failure
--         outOfMemoryError = do
--             clearError context
--             performGC
--             code' <- attempt
--             if code' == 0
--                 then success
--                 else failure
--         unknownError = failure

-- runRenTIn :: Context -> (forall c. FutT c m a) -> m a
-- runRenTIn context (FutT a) = a context

-- runFutTWith :: [ContextOption] -> (forall c. FutT c m a) -> m a
-- runFutTWith options a
--     = unsafePerformIO
--     $ getContext options >>= \c -> return $ runRenTIn c a
-- runFutT = runFutTWith []

-- runFutIn :: Context -> (forall c. Fut c a) -> a
-- runFutIn context a = runIdentity $ runRenTIn context $ a

-- runFutWith :: [ContextOption] -> (forall c. Fut c a) -> a
-- runFutWith options a = runIdentity $ runFutTWith options a
-- runFut = runFutWith []

-- pureFut :: (Monad m) => Fut c a -> FutT c m a
-- pureFut (FutT a) = FutT (pure . runIdentity . a)

-- unsafeFromFutIO :: FutIO c a -> Fut c a
-- unsafeFromFutIO (FutT a) = FutT (Identity . unsafePerformIO . a)

-- unsafeLiftFromIO :: Monad m => (Context -> IO a) -> FutT c m a
-- unsafeLiftFromIO a = FutT (pure . unsafePerformIO . a)

-- wrapIO :: (Context -> IO a) -> FutIO c a
-- wrapIO a = FutT a  
