{-# LANGUAGE FlexibleInstances #-}
module HRayLib3d.ScriptEngine.HRuby.Core where

    import System.IO ()
    import GHC.Real  ()
    import GHC.Float (float2Int, int2Float)
    import Data.Int  (Int16)
    import Data.Word (Word8)
    import Data.Aeson (Value (Array, Bool, Number, String), object)
    import Data.Aeson.Key (fromText)
    import qualified Data.Text as T
    import qualified Data.Vector as V

    import Control.Lens            ()
    import Control.Monad (unless, when)
    import Control.Monad.Identity  ()
    import Control.Monad.IO.Class
    import HRayLib3d.Utils.Exception 
    import Control.Monad.Trans.State.Lazy (StateT)

    import Control.Concurrent      (ThreadId, forkOS)
    import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
    import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
    import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)

    import Foreign.Ruby.Safe ( loadFile,
      withRubyInterpreter,
      fromRuby,
      safeMethodCall,
      toRuby,
      freezeGC,
      RubyInterpreter 
      )
    import Foreign.Ruby.Helpers (ToRuby, FromRuby)
    import System.Exit (exitFailure)
    import System.Process
    import System.Directory
    import System.FilePath
    import HRayLib3d.ScriptEngine.HRuby.HRubyHS
    -- class    ToInterpreter a where
    --     toInterpreter :: a -> RubyObject a

    -- instance ToInterpreter Float where
    --     toInterpreter a = RubyObject a

    -- instance ToInterpreter Int16 where
    --     toInterpreter a = RubyObject a

    -- newtype RubyObject a = RubyObject { rbyRawObject :: a } deriving (Show, Eq)

    -- data RendererHandle = RendererHandle {
    --     threadID :: ThreadId,
    --     procSem  :: MVar (Chan ComputeRubyCommand) 
    -- } deriving(Eq, Show)

    -- -- commands send from the front-end to the back end or vice versa
    -- -- the hope is this could condone rendering or GPU enabling/accelerating
    -- -- Monomer
    -- data ComputeRubyCommand = 
    --     AllocRenderer   (RubyObject Int16)   -- {imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
    --     | ChangeRenderer  Float               --  {imgResp :: MP.Image MP.RGBA Float}
    --     | GPUSuccess
    --     | GPUFail
    --     | GPUExit
    --     deriving (Eq, Show)

    -- instance Show (MVar (Chan ComputeRubyCommand)) where
    --     show _ = "MVar (Chan ComputeRubyCommand)"
        
    -- toInt16 :: Float -> Int16
    -- toInt16 = fromIntegral . float2Int

    -- toFloat :: Int16 -> Float
    -- toFloat = int2Float    . fromIntegral

    -- chunkToInt16 ::  [Float] -> [Int16]
    -- chunkToInt16 = map toInt16 

    -- chunkToWord8 ::  [Int16]  -> [Float]
    -- chunkToWord8 = map toFloat 

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


    -- gpuProcess :: Chan ComputeRubyCommand -> IO ()
    -- gpuProcess chan = undefined
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

    -- handlePipelineCommand :: ComputeRubyCommand -> IO ComputeRubyCommand --WRendererIO PipelineCommand
    -- handlePipelineCommand command = do 
    --     case command of
    --         ChangeRenderer a ->  return $ AllocRenderer (toInterpreter $ toInt16 a) -- $ ImageResp $ Image result
    --         AllocRenderer  a ->  return GPUSuccess
    --         GPUExit          ->  return GPUExit
    --         -- GPUSuccess       ->  warning
    --         GPUFail          ->  error "var mismatch"
    --         _ -> error "invalid command"
    --             -- gpuObjB = toGPU  b
                
    --             -- !futOutput <- E.diffNoAlphaImages futA futB --E.diffImages futA futB
    --             --run actual futhark diff operation
    --             -- return result in ImageResp
    --             -- result <- fromFuthark futOutput

    -- createPipelineHandle :: IO RendererHandle
    -- createPipelineHandle = do
    --     chan <- newChan
    --     mvar <- newMVar chan
    --     tid  <- forkOS $ gpuProcess chan
    --     return $ RendererHandle tid mvar

    -- runPipelineCommand :: RendererHandle -> ComputeRubyCommand -> IO ComputeRubyCommand 
    -- runPipelineCommand fut gcommand = do
    --     commChan <- takeMVar $ procSem fut
    --     writeChan commChan gcommand
    --     result <- readChan commChan
    --     putMVar (procSem fut) commChan
    --     return result

    roundTrip :: (ToRuby a, FromRuby a, Eq a, Show a) => RubyInterpreter -> a -> IO Bool
    roundTrip i v = freezeGC i $ do
        rub <- toRuby i v >>= either (error . show) return
        nxt <- safeMethodCall i "TestClass" "testfunc" [rub]
        case nxt of
            Right x -> do
                out <- fromRuby i x >>= either (error . show) pure
                when (out /= v) (print out)
                return (v == out)
            Left rr -> print rr >> return False

    readRubyFile :: IO ()
    readRubyFile = withRubyInterpreter $ \i -> do
        loadFile i "./test/test.rb" >>= either (error . show) return

    compileToRubyBundle :: String -> IO ()
    compileToRubyBundle gemName = do
        --ex: gemName = "<filename>.gemspec"
        cwrd <- getCurrentDirectory 
        _    <- createProcess (proc "gem" [cwrd </> "build", gemName]){ std_out = CreatePipe }   {-(_, Just hout, _, _) <- -}
        return ()

    compileToRubyHS :: ToRubyHS a => [a] -> [RValueHS]
    compileToRubyHS input = map toRubyHS input
