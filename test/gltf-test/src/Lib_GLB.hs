{-# LANGUAGE OverloadedStrings #-}

module Lib_GLB (
    GLBModel(..)
    ,   fromByteString
    ,   fromFile
    ,   validateGLB
    ,   readGLB
    ) where
        
import Codec.GLB  ( GLB(..), fromByteString, fromFile,chunkData, chunkLength)
import Data.Maybe ( fromJust )
import Data.Binary.Get
import qualified Data.Vector as V 

newtype GLBModel = GLBModel { glbRaw :: Codec.GLB.GLB } deriving Show

validateGLB :: Either (ByteOffset, String) GLB -> IO GLB
validateGLB gl = case gl of 
                    Left s    -> error $ "Parse Error, use Offset to read manually: " ++ show s
                    Right glb -> return glb 

readGLB :: String -> IO ()
readGLB glbFilename = do 
    -- open the glb file
    glbFile <- fromFile    glbFilename
    rawGLB  <- validateGLB glbFile

    -- After reading from the json, the file cusor will automatically be at the start of the binary header
    let glbHeader = header rawGLB
        glbChunks = chunks rawGLB

    print $ "header data: " ++ show glbHeader
    V.forM_ glbChunks (print . chunkLength)
    V.forM_ glbChunks (print . chunkData)    
