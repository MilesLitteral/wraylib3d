module Main (main) where

import Lib
import Lib_GLB

main :: IO ()
main = do
    print    "loading test_gltf_embedded (GlTF)"
    readGLTF "C:/Users/Manda/OneDrive/Documents/Github/gltf-test/test_files/gltf/test_gltf_embedded.gltf"
    --print "---------------------------------"
    --readGLB  "C:/Users/Manda/OneDrive/Documents/Github/gltf-test/test_files/gltf/test_glb.glb"
