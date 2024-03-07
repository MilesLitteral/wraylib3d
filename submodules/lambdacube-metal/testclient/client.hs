{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings, RecordWildCards #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector,(!))
import Data.ByteString.Char8 (unpack,pack)
import qualified Data.ByteString as SB
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Base64 as B64

import System.Exit
import Data.Time.Clock
import Data.Aeson
import Foreign

import qualified Network.WebSockets  as WS
import Network.Socket

import "GLFW-b" Graphics.UI.GLFW as GLFW
import "OpenGLRaw" Graphics.GL.Core33
import Codec.Picture as Juicy

import LambdaCube.IR
import LambdaCube.PipelineSchema
import LambdaCube.Mesh
import LambdaCube.Metal
import LambdaCube.Metal.Mesh
import TestData

main :: IO ()
main = do
  win <- initWindow "LambdaCube 3D Metal 3 Backend" 256 256
  GLFW.setWindowCloseCallback win $ Just $ \_ -> do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

  -- connect to the test server
  forever $ catchAll (setupConnection win) $ \_ -> do
    GLFW.pollEvents
    threadDelay 100000