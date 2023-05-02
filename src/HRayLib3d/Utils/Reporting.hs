{-# LANGUAGE OverloadedStrings, FlexibleContexts, UndecidableInstances #-}

module HRayLib3d.Utils.Reporting where

import Data.Map (union)
import Data.Monoid(getSum)
import HRayLib3d.Utils.Project
-- import qualified HRayLib3d.Utils.Database as DB
import qualified Control.Applicative as S
import qualified Data.Map as S

-- data Report
--     = Report {   
--         --lossType     :: L.Loss, 
--         loss         :: LossMetric,  
--         startingLoss :: LossMetric,   
--         difference   :: LossMetric
--     } deriving(Show, Eq)

-- instance Semigroup Report where
--     (Report l1 b1 n1 d1) <> (Report l2 b2 n2 d2) = Report (l2) (LossMetric ((trainingTime b1) + (trainingTime b2)) (timeExpenditure b2)) (LossMetric ((trainingTime n1) + (trainingTime n2)) (timeExpenditure n2)) (LossMetric ((trainingTime d1) + (trainingTime d2)) (timeExpenditure d2))

-- instance Monoid Report where
--   mempty = Report L.NONE (LossMetric 0 0) (LossMetric 0 0) (LossMetric 0 0)

-- calculateReport :: LossMetric -> [LossType] -> Report
-- calculateReport ls metrics =
--     Report
--     {  lossType     = lossType'
--     ,  startingLoss = startingLoss'
--     ,  loss         = loss'
--     ,  difference   = (LossMetric ((trainingTime startingLoss') - (trainingTime loss')) ((timeExpenditure startingLoss') - (timeExpenditure loss')))
--     }
--     where
--         lossType'               = undefined
--         startingLoss'           = LossMetric (trainingTime ls - timeExpenditure ls) $ timeExpenditure ls
--         loss'                   = getSum(foldMap asLoss metrics)
--         asLoss (MSE  m)         = pure (LossMetric (trainingTime m) (timeExpenditure m))
--         asLoss (SSIM m)         = pure (LossMetric (negate $ trainingTime m) (negate $ timeExpenditure m))

-- calculateProjectReport :: Project -> IO Report
-- calculateProjectReport = calc
--     where
--         calc(Project p _ _) = calculateReport <$> DB.getLoss p <*> DB.getTraining p
--         calc(ProjectGroup _ projects) = foldMap calc projects
