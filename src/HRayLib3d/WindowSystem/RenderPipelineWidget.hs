{-# LANGUAGE BangPatterns #-}

module Manifest.Utils.Futhark where

--import Data.Word (Word8)
import GHC.Float(float2Int, int2Float)
import Data.Int (Int16)
import qualified Data.Massiv.Array as A

import Control.Monad.Identity
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Concurrent (forkOS)

import System.IO
import GHC.Real
import Manifut.Fut (FutT, Fut, FutIO) -- was Futhark.Fut
import Manifest.Utils.Communication 
import Manifest.Types
import qualified Manifut.Entries as E
import Manifut.Context(getContext)
import Manifut.Fut(runFutTIn)
import Manifut(Output(..), toFuthark, liftFut, fromFuthark)
import Manipipe.Image.Type

-- | Manifest's Interface for speaking to Futhark
-- utilizes Manifut to accomplish this
type MFutT m a = FutT m a
type MFut      = FutT Identity
type MFutIO    = FutT IO

-- | Conversion Functions:
-- (Word8 <-> Int16)
-- (A.Array A.S A.Ix1 Word8 <-> A.Array A.S A.Ix1 Int16)
toInt16 :: Float -> Int16
toInt16 = fromIntegral . float2Int

toFloat :: Int16 -> Float
toFloat = int2Float    . fromIntegral

chunkToInt16 ::  A.Array A.S A.Ix1 Float  -> A.Array A.D A.Ix1 Int16
chunkToInt16 a = A.map toInt16 a

chunkToWord8 ::  A.Array A.S A.Ix1 Int16  -> A.Array A.D A.Ix1 Float
chunkToWord8 a = A.map toFloat a 

futharkProcess :: Chan GPUCommand -> IO ()
futharkProcess chan = do 
    context <- getContext []
    runFutTIn context $ do 
        let {doFutharkProcess = do
                command <- liftIO $ readChan chan
                case command of
                    GPUExit -> return () 
                    _ -> do 
                        result <- handleGPUCommand command
                        liftIO $ writeChan chan result --writeChan
                        doFutharkProcess
            }
        doFutharkProcess

handleGPUCommand :: GPUCommand -> FutIO GPUCommand
handleGPUCommand command = do 
    case command of
        DiffImages a b -> do
            futA <- toFuthark $ unImage a
            futB <- toFuthark $ unImage b
            
            !futOutput <- E.diffNoAlphaImages futA futB --E.diffImages futA futB
            --run actual futhark diff operation
            -- return result in ImageResp
            result <- fromFuthark futOutput
            return $ ImageResp $ Image result

createFutharkHandle :: IO (FutHandle)
createFutharkHandle = do
    chan <- newChan
    mvar <- newMVar chan
    tid  <- forkOS $ futharkProcess chan
    return $ FutHandle tid mvar