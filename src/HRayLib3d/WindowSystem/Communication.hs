{-# LANGUAGE AllowAmbiguousTypes #-}

module Manifest.Utils.Communication(
    RunInteraction(..)
,   runFutharkCommand
) where

import Data.Text

import Monomer 
import Monomer.Main.Types 
import Monomer.Widgets.Single
import Monomer.Widgets.Singles.Image (ImageCfg)

import Control.Lens
import Control.Concurrent (forkOS)
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, readMVar, newEmptyMVar, takeMVar, putMVar, modifyMVar_)

import Maniga.Control.Regimen 
import Manipipe.Client.Monad

import Manifest.Types
import Manifut.Fut (FutIO)
import Manifut.Context

-- | Takes StateT State and returns a Manifest StateT ManimerModel
class RunInteraction state where 
    update  :: state -> ManifestModel -> ManifestModel 
    shift   :: state -> StateT ServerInfo   (StateT state FutIO) () -> StateT ManifestModel (StateT state FutIO) () 

runFutharkCommand :: FutHandle -> GPUCommand -> IO GPUCommand 
runFutharkCommand fut gcommand = do
    commChan <- takeMVar $ procSem fut
    writeChan commChan gcommand
    result <- readChan commChan
    putMVar (procSem fut) commChan
    return result
