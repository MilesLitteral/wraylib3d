module Main (main) where

import MegaStore
import Data.Text (pack)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    a1  <- loadFile "./assets/s1.png"
    a2  <- loadFile "./assets/s2.png"
    a3  <- loadFile "./assets/s3.png"
    a4  <- loadFile "./assets/s4.png"
    a5  <- loadFile "./assets/s5.png"

    let testSet = MegaStore [(pack "s1", a1), (pack "s2", a2), (pack "s3", a3), (pack "s4", a4), (pack "s5", a5)]
    createDirectoryIfMissing False "./test"
    saveStore "./test/testSet" testSet -- keystore of images
    print "Loading MegaStore testSet"
    loadedContents <- loadStore "./test/testSet.megastore"
    print "autoUnpack into ./test"
    autoUnpack "./test" loadedContents
    print "autoUnpack complete check folder"
