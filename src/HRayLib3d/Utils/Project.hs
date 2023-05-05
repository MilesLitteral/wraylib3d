{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, DeriveGeneric, DeriveAnyClass, DataKinds #-}

module HRayLib3d.Utils.Project where

import Data.Text(Text, pack)
import GHC.Generics(Generic)
import Data.Aeson (ToJSON, FromJSON)

type Weights = String -- Path
type Model   = String -- Path
newtype LossValue = LossValue { unLoss         :: Double }    deriving(Generic, ToJSON, FromJSON, Show, Eq, Num)
newtype ProjectId = ProjectId { unProjectId    :: Int    }    deriving(Generic, ToJSON, FromJSON, Show, Eq, Num)

data Framework 
    = TensorFlow
    | JAX
    deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Architecture 
    = Architecture {
      arch    :: Framework, -- TF,   JAX, etc
      model   :: Model,     -- NeRF, GAN, etc
      weights :: Weights    -- .h5 filepath
    } deriving(Generic, ToJSON, FromJSON, Show, Eq)

data LossMetric 
    = LossMetric { 
    trainingTime      :: LossValue, 
    timeExpenditure   :: LossValue 
    } deriving(Generic, ToJSON, FromJSON, Num, Show, Eq)

data LossType 
    = MSE     LossMetric 
    | SSIM    LossMetric 
    deriving(Generic, ToJSON, FromJSON, Show, Eq)

data Project     
    = Project      ProjectId Text MLRegimen 
    | ProjectGroup Text [Project] 
    deriving(Generic, ToJSON, FromJSON, Show, Eq)

data MLRegimen = Regimen  Architecture LossType deriving(Generic, ToJSON, FromJSON, Show, Eq, Num)

appendProjectToGroup :: Project -> [Project] -> Project
appendProjectToGroup project projects = do
    let pg = [project]
    ProjectGroup (pack $ show $ id projects) (projects ++ pg) 
        -- where
        --     LossMetric            = trainingTime loss - timeExpenditure loss
        --     netLoss'              = getSum(foldMap asLoss ls)
        --     asLoss (MSE m)        = pure m
        --     asLoss (SSIM m)       = pure (negate m) 



    