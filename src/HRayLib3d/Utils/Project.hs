{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, DeriveGeneric, DeriveAnyClass, DataKinds #-}

module HRayLib3d.Utils.Project where

import Data.Text(Text, pack)
import GHC.Generics(Generic)
import Data.Aeson (ToJSON, FromJSON)

data Project     
    = Project      ProjectId Text MLRegimen 
    | ProjectGroup Text [Project] 
    deriving(Generic, ToJSON, FromJSON, Show, Eq)

appendProjectToGroup :: Project -> [Project] -> Project
appendProjectToGroup project projects = do
    let pg = [project]
    ProjectGroup (pack $ show $ id projects) (projects ++ pg) 
        -- where
        --     LossMetric            = trainingTime loss - timeExpenditure loss
        --     netLoss'              = getSum(foldMap asLoss ls)
        --     asLoss (MSE m)        = pure m
        --     asLoss (SSIM m)       = pure (negate m) 



    