{-# LANGUAGE LambdaCase, TypeApplications, RecordWildCards, RankNTypes, ScopedTypeVariables, DataKinds #-}

module HRayLib3d.WindowSystem.Context where 

  import Data.IORef
  
  -- FPS tracking
  data State = State { frames :: IORef Int, t0 :: IORef Double }

  -- data GPUData
  --   = GPUData
  --   { gpuTextures :: SV.Vector TextureData
  --   , gpuMeshes   :: SV.Vector GPUMesh
  --   }

  -- allocateGPUData RenderJob{..} = GPUData <$> mapM uploadTex2D textures <*> mapM uploadMeshToGPU meshes
  --   where uploadTex2D = uploadTexture2DToGPU . either error id . decodeImage . either error id . B64.decode . pack

  -- disposeGPUData GPUData{..} = mapM_ disposeTexture gpuTextures >> mapM_ disposeMesh gpuMeshes
