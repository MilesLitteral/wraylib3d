{-# LANGUAGE BangPatterns, RankNTypes, FlexibleInstances #-}
module HRayLib3d.WindowSystem.RenderPipelineWidget () where

import System.IO ()
import GHC.Real  ()
import Data.Int  (Int16)
import Control.Lens            ()
import Control.Monad.Identity  ()
import Control.Monad.IO.Class  ()

newtype GPUObject a = GPUObject { gpuRawObject :: Int16 } deriving (Show, Eq)
-- commands send from the front-end to the back end or vice versa
-- the hope is this could condone rendering or GPU enabling/accelerating
-- Monomer
data RenderPipelineCommand
  = AllocRenderer   (GPUObject Int16)   -- {imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
  | ChangeRenderer  Float               --  {imgResp :: MP.Image MP.RGBA Float}
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
