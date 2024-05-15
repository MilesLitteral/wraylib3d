{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module HRayLib3d.Utils.PrettyPrint where

import Data.Tree

import Text.Printf
import Control.Monad
import HRayLib3d.Utils.Project
import qualified Data.Text as Text

-- Print functions for Project Hierarchies
asTree :: Project -> Tree String 
asTree project = 
    case project of
        Project p r      -> Node(printf "%s %s" p $ show r) []
        ProjectGroup name projects   -> Node(Text.unpack name)  (map asTree projects)

prettyProject :: Project -> String
prettyProject = drawTree . asTree

prettyWRLP :: WRLPFile   -> [String]
prettyWRLP (WRLPFile w) = forM w (\x -> printf "FileName: %s, FileType: %s," (show . Text.unpack $ fst x) (show $ snd x))

