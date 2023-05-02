{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module HRayLib3d.Utils.PrettyPrint where

import Data.Tree
import Text.Printf
import HRayLib3d.Utils.Project
import HRayLib3d.Utils.Reporting
import qualified Data.Text as Text

asTree :: Project -> Tree String 
asTree project = 
    case project of
        Project (ProjectId p) name r -> Node(printf "%s (%d) %s" name p (show r))[]
        ProjectGroup name projects   -> Node(Text.unpack name) (map asTree projects)

prettyProject :: Project -> String
prettyProject = drawTree . asTree

-- prettyReport :: Report -> String
-- prettyReport r = 
--     printf
--         "Starting Loss: %.2f, Net: %.2f, Difference %.2f,"
--         (unLoss (trainingTime     $ startingLoss r))
--         (unLoss (timeExpenditure  $ loss         r))
--         (unLoss ((timeExpenditure $ difference   r) - (trainingTime     $ difference r)))

