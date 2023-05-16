module Manifest.Utils.Chart where


import Data.List
import Data.Maybe
import Data.Typeable
import Data.Text
import Control.Lens
import qualified Manipipe                   as MP
import qualified Manipipe.Util.CairoHelpers as MP
import qualified Manipipe.Util.Chart        as MP
import qualified Data.Massiv.Array          as A
import qualified Data.Massiv.Array.IO       as A
import qualified Graphics.Rendering.Cairo   as C
import qualified Data.Array.MArray          as M
import qualified Data.Text                  as T
import qualified Monomer.Lens as L

import Graphics.UI.TinyFileDialogs

import Manipipe.Util.CairoHelpers(CairoColor(..))
import Manifest.Types (BytesImage(..), ManieyeChartSource(..), DatasetDir(..), ManieyeModel(..), selectedGraphic, length, ManieyeImageSource(..))
import Manifest.Utils.Prisms
import Manifest.Utils.Conversion(mpImageToBytesImage)
import GHC.Float (float2Double, float2Int)
import Monomer (Size(..))
import Monomer.Widgets.Composite

-- Task $ do
--       -- print $ "UPDATE CheckPoint SOURCES (Update Checkpoint List)"
--       -- print $ "Dataset Path: " ++ (model ^. (manieyeModel . datasetDir) ++ "/" ++ (model ^. (manieyeModel . selectedExperimentC))) 
--       -- print $ "Selected Output Dataset: " ++ src
--       -- print $ "Full Path: " ++ (((dirPath $ model ^. selectedRun) ++ "/Predictions/" ++ src ++ "/Images/")) 
--       (Just batchDir)     <- findInputDirectory (model  ^.  (manieyeModel . datasetDir))
--       (Just sceneDir)     <- findSceneDir       ((model ^.  (manieyeModel . datasetDir)))
--       batchDirContents    <- loadDirContents batchDir
--       datasetList         <- loadFolderContents  ((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ (model ^. (manieyeModel . selectedExperimentC)) ++ "/" ++ (extractFilePath $ model ^. selectedRun) ++ "/Predictions/")
--       datasetListContents <- mLoadFolderContents ((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ (model ^. (manieyeModel . selectedExperimentC)) ++ "/" ++ (extractFilePath $ model ^. selectedRun) ++ "/Predictions/" ++ src ++ "/Images/")   -- (model ^. selectedDataset)
--       let dataSets = (sort $ Prelude.map (Dataset) datasetList)
--           standardSource   =  ManieyeImageFolder $ sort $ Prelude.map (\x -> ((model ^.  (manieyeModel . datasetDir)) ++ "/Seq/") ++ x) batchDirContents
--           predictedSource  = case datasetListContents of 
--             Just lst -> ManieyeImageFolder $ sort $  Prelude.map (\x -> ((model  ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ (model ^. (manieyeModel . selectedExperimentC)) ++ "/" ++ (extractFilePath $ model ^. selectedRun) ++ "/Predictions/" ++ src ++ "/Images/" ++ x))  (lst)
--             Nothing  -> ManieyeImageNothing
--       --print "prepare diff"
--       --print $ "Predicted:" ++ show predictedSource
--       let differenceSource = (ManieyeDifferenceImages (images standardSource) (predictedSource == ManieyeImageNothing ? replicate (Data.List.length $ images standardSource) (((model ^. userHome) ++ "/Software/manimer/assets/images/broken.png")) :? (images predictedSource)))
--       -- masks <- messageBox "Load Masks" "Load Masked SV?" Question TFD.YN_Yes
--       -- case masks of
--       --   YN_No -> do
--       (notifyPopup "Warning" "Ignoring Mask SVs" Graphics.UI.TinyFileDialogs.Warning)
--       (MP.Mani (svData :: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ ((model  ^.  (manieyeModel . datasetDir))  ++ "/GroundTruths/" ++  "seq_sv.mani") --batchDir  </> "seq_sv.mani"
--       (MP.Mani (pSvData:: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ ((model  ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ (model ^. (manieyeModel . selectedExperimentC)) ++ "/" ++ (extractFilePath $ model ^. selectedRun) ++ "/Predictions/" ++ src ++ "/seq_sv.mani")
--       let --svChartSource     = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 0) $ Just  (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices pSvData) $ A.Ix1 0)
--           (A.Sz2 _ numCharts) = A.size svData
--           createChartSource idx = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 idx) $ Just  (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices pSvData) $ A.Ix1 idx)
--       chartLabels       <- loadLabelsFromReconSpec model  -- /GroundTruths/recon_spec.json
--       --print  $ "CHART LABELS: " ++ (show chartLabels)--replicateM (times) descriptor
--       putStrLn $ "Num charts: " ++ (show $ take numCharts [0..])
--       return $ UpdateImageSources ((model ^.  (manieyeModel . datasetDir))) (Prelude.map (dirPath) (model ^. activeExperiments))  [ standardSource, differenceSource, predictedSource ] (Prelude.map createChartSource $ take numCharts [0..]) (chartLabels), --(Just $ model ^.  (manieyeModel . selectedCheckpoint))
--       Model $ model & selectedChk .~ Dataset (src)

getNumVectors :: [ManieyeImageSource] -> Int
getNumVectors imageSources = case Data.List.length imageSources of
                        0 -> 0
                        _ -> Manifest.Types.length $ imageSources !! 0

-- widgetNodeFromKey :: WidgetEnv s e -> WidgetKey -> Maybe (WidgetNode s e)
-- widgetNodeFromKey wenv key = M.lookup key (wenv ^. L.widgetKeyMap)


createChartNodeKey :: Int -> Text
createChartNodeKey idx = pack ("Chart" ++ show idx)

-- | Checks if the node is within the visible viewport, and itself visible.
isWidgetNodeInfoVisible :: WidgetEnv s e -> WidgetNodeInfo -> Bool
isWidgetNodeInfoVisible wenv nodeInfo = (nodeInfo ^. L.visible) && isOverlapped where
  viewport     = (wenv ^. L.viewport)
  isOverlapped = rectsOverlap viewport (nodeInfo ^. L.viewport)

-- chartIsVisible :: ManifestWenv -> Int -> Bool
-- chartIsVisible wenv idx = 
--     let mNode = widgetNodeFromKey wenv (WidgetKey $ createChartNodeKey idx)
--     in case mNode of 
--       Just node -> isWidgetVisible wenv node 
--       Nothing -> error "Failed to get chart widget node"

chartIsVisible2 :: (Typeable s, Typeable e) => WidgetEnv s e -> Int -> Bool
chartIsVisible2 wenv idx =
    let mNodeInfo = nodeInfoFromKey wenv (WidgetKey $ createChartNodeKey idx)
    in case mNodeInfo of 
      Just nodeInfo -> isWidgetNodeInfoVisible wenv nodeInfo 
      Nothing -> error "Failed to get chart widget node"

-- printChartRect :: ManifestWenv -> Int -> IO ()
-- printChartRect wenv idx = do 
--     let mNodeInfo = nodeInfoFromKey wenv (WidgetKey $ ("vstack_chart"))--createChartNodeKey idx)
--     case mNodeInfo of 
--       Just nodeInfo -> do
--         putStrLn $ (show $ createChartNodeKey idx) ++ " fully in rect?: " ++ (show $ rectInRect (wenv ^. L.viewport) ((nodeInfo ^. L.viewport)))
--         -- isWidgetNodeInfoVisible wenv nodeInfo 
--       Nothing -> error "Failed to get chart widget node"



-- extractFilePath datasetDir =
--   case datasetDir of 
--     Dataset path-> path 
--     None -> ("N/A")

expandChartLabels :: [(String, Int)] -> [String]
expandChartLabels labels = Data.List.concat $ Prelude.map (\(label, count) -> Data.List.replicate count label) labels 

createMaskVisualizationChart :: MP.Futhark2d Float -> Double -> Double -> Double -> Double -> IO BytesImage
createMaskVisualizationChart maskData centerIndex hScale resW resH = do 
    let (A.Sz2 x y) = A.size maskData
        sliceStart = max 0 $ centerIndex - hScale
        sliceSize  = min ((hScale* 2) + 1) $ (fromIntegral x) - sliceStart
        chartData = A.compute $ A.extract' (A.Ix2 (round sliceStart) 0) (A.Sz2 (round sliceSize) y) maskData
    image <- MP.createBinaryHeatMap chartData MP.ccRed MP.ccBlue sliceStart (centerIndex - hScale) (centerIndex + hScale) resW resH
    return $ mpImageToBytesImage image $ T.pack $ "maskChart" ++ (show $ round centerIndex) ++ ".png" 

-- stroke
-- setLineWidth 0.5
-- --setLineJoin 
-- --setLineCap 
-- setDash 
-- strokePreserve

createSceneVectorChart :: MP.Futhark1d Float -> Maybe (MP.Futhark1d Float) -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> IO BytesImage
createSceneVectorChart inputSvData mPredSvData centerIndex hScale yAxisMin yAxisMax resW resH id = do
    let (A.Sz1 svLen) = A.size inputSvData
        valMinI = float2Double $ A.minimum' inputSvData
        valMaxI = float2Double $ A.maximum' inputSvData
        (valMin, valMax) = case mPredSvData of
            Nothing         -> (valMinI, valMaxI)
            Just predSvData -> (min valMinI $ float2Double $ A.minimum' predSvData, max valMaxI $ float2Double $ A.maximum' predSvData )
        sliceStart = max 0 $ centerIndex - hScale
        sliceSize  = min ((hScale* 2) + 1) $ (fromIntegral svLen) - sliceStart
        iChartData = A.compute $ A.extract' (A.Ix1 $ round sliceStart) (A.Sz1 $ round sliceSize) inputSvData 
        pChartData = case mPredSvData of
            Just pSvData -> Just $ A.compute $ A.extract' (A.Ix1 $ round sliceStart) (A.Sz1 $ round sliceSize) pSvData
            Nothing -> Nothing 
    image <- MP.createLineChartAutoLabeled iChartData pChartData sliceStart (centerIndex - hScale) (centerIndex + hScale) valMin valMax yAxisMin yAxisMax resW resH
    return $ BytesImage (T.pack $ "svChart" ++ (show id) ++ (show $ round centerIndex) ++ ".png") image $ Size resW resH

getNthChart :: ManieyeChartSource -> Double -> Int -> IO (BytesImage)
getNthChart source n id = do
    case source of
        ManieyeSVChart   iSvData pSvData -> createSceneVectorChart       iSvData  pSvData n 100 (-1.2) 1.2 1200 150 id
        ManieyeMaskChart maskData        -> createMaskVisualizationChart maskData         n 100 1200 150
        _                                -> undefined

-- updateGraphicSources model fp dirContent refreshEvt = [
--         Model $ model & ((ManieyeModel #^? model) . (selectedGraphic #^? model))    .~ (Dataset fp),
--         Event $ refreshEvt
--     ]

updateSource :: (Typeable s, Typeable e) => FilePath -> String -> String -> Lens s e
updateSource datasetDir selectedExperiment src targetEvt objectiveLens = [
    Task $ do
        print $ "UPDATE RUN SOURCES (Update Runs List)"
        print $ "Dataset Path: " ++ (datasetDir ++ "/" ++ selectedExperiment) 
        print $ "Selected Output Dataset: " ++ src
        print $ "Full Path: " ++ ((datasetDir ++ "/Output/" ++ selectedExperiment ++ "/" ++ src ++ "/Predictions/")) 
        (Just batchDir)     <- findInputDirectory datasetDir
        (Just sceneDir)     <- findSceneDir       datasetDir
        batchDirContents    <- loadDirContents batchDir
        datasetList         <- loadFolderContents  (datasetDir  ++ "/Output/" ++ selectedExperiment ++ "/" ++ src ++ "/Predictions/")
        datasetListContents <- mLoadFolderContents (datasetDir ++ "/Output/" ++ selectedExperiment ++ "/" ++ src ++ "/Predictions/" ++ (head datasetList) ++ "/Images/")   -- (model ^. selectedDataset)
        let dataSets = (sort $ Prelude.map (Dataset) datasetList)
            standardSource   =  ManieyeImageFolder $ sort $ Prelude.map (\x -> ((datasetDir) ++ "/Seq/") ++ x) batchDirContents
            predictedSource  = case datasetListContents of 
                Just lst -> ManieyeImageFolder $ sort $  Prelude.map (\x -> ((datasetDir)  ++ "/Output/" ++ (selectedExperiment) ++ "/Predictions/" ++ src ++ "/Images/" ++ x))  (lst)
                Nothing  -> ManieyeImageNothing
        print "prepare diff"
        print $ "Predicted:" ++ show predictedSource
        let differenceSource = (ManieyeDifferenceImages (images standardSource) (predictedSource == ManieyeImageNothing ? replicate (Data.List.length $ images standardSource) (((model ^. userHome) ++ "/Software/manimer/assets/images/broken.png")) :? (images predictedSource)))
        (notifyPopup "Warning" "Ignoring Mask SVs" Warning)
        (MP.Mani (svData :: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ ((model  ^.  (manieyeModel . datasetDir))  ++ "/GroundTruths/" ++  "seq_sv.mani") --batchDir  </> "seq_sv.mani"
        (MP.Mani (pSvData:: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ ((model  ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ (model ^. (manieyeModel . selectedExperimentC)) ++ "/" ++ (extractFilePath $ model ^. selectedRun) ++ "/Predictions/" ++ src ++ "/seq_sv.mani")
        let --svChartSource     = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 0) $ Just  (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices pSvData) $ A.Ix1 0)
            (A.Sz2 _ numCharts) = A.size svData
            createChartSource idx = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 idx) $ Just  (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices pSvData) $ A.Ix1 idx)
        chartLabels       <- loadLabelsFromReconSpec (model ^.)  -- /GroundTruths/recon_spec.json
        --print  $ "CHART LABELS: " ++ (show chartLabels)--replicateM (times) descriptor
        putStrLn $ "Num charts: " ++ (show $ take numCharts [0..])
        return $ targetEvt ((model ^.  (manieyeModel . datasetDir))) (datasetList) [ standardSource, differenceSource, predictedSource ] [] (Just src),
    Model $ model & objectiveLens .~ Dataset (src)
    ]

-- updateSourceLens model fp dirContents newSources newChartSources sb = [
--         Model $ model 
--             & manieyeModel . datasetDir   .~ fp 
--             & manieyeModel . imageSources .~ newSources 
--             & manieyeModel . chartSources .~ newChartSources 
--             & manieyeModel . selectedCheckpoint .~ (fromJust sb)
--             & manieyeModel . memImages    .~ ((map (\_ -> emptyBytesImage) newSources ))
--             & manieyeModel . chartImages  .~ ((map (\_ -> emptyBytesImage) newChartSources))
--             & manieyeModel . activeCheckpoints .~ (sort $ (Prelude.map (Dataset) dirContents)),
--         Event $ ChangeImages 0
--     ]

updateGraphicSources model fp dirContent refreshEvt = do
    case (has selectedGraphic model) of
        True -> [ Model $ model & selectedGraphic    .~ (Dataset fp) & selectedGraphicSrc .~ (fp) & activeGraphics     .~ (sort $ (Prelude.map (Dataset) dirContent)),
            Event $ refreshEvt
            ]   
        False -> [Model $ model & selectedGraphic    .~ (Dataset "")]

--  SetDatasetDir -> [--be *very careful* when editting this Event as all it's commented code is valid and sometimes preferred
--     Task $ do
--       print $ "SET DATASET DIRECTORY"
--       print $ "Path: " ++ ((model ^. userHome) ++ "/Datasets")
--       filePath <- liftIO $ osSelectFolderDialog "Path to Dataset Folder" (pack $ (model ^. userHome) ++ "/Datasets/")
--       case filePath of
--         Nothing  -> do
--           (notifyPopup "IOException" "Invalid File Path or Cancelled I/O Operation" Graphics.UI.TinyFileDialogs.Warning)
--           return $ AutoResolve
--         Just fp  -> do
--           (Just batchDir)     <- findInputDirectory (unpack fp)
--           (Just sceneDir)     <- findSceneDir ((unpack fp))
--           batchDirContents    <- (loadDirContents batchDir)
--           file <- (((model ^. ml_backend == "TF" )) ? (decodeFileEither ((model ^. userHome) ++ "/Software/manimer/tf_config.yaml") :: IO (Either ParseException MLConfig)) :? decodeFileEither ((model ^. userHome) ++ "/Software/manimer/torch_config.yaml") ) 
--           case file of
--             Left  e  -> error $ show e
--             Right fl -> do
--               --print $ show fl
--               let selected = (model ^. selectedExperiment) 
--               -- datasetContents: /home/askoch/Datasets/Kelvin23_75_30/Output//resnet_big_pen_zero_zero/Predictions/Images/
--               datasetExperimentList         <- loadFolderContents  ((unpack fp)   ++ "/Output/"  ++ (unpack selected)  ++ "/")      -- ++ (unpack selected)
--               datasetExperimentListContents <- mLoadFolderContents ((unpack fp)   ++ "/Output/"  ++ (datasetExperimentList !! 0) ++ "/Predictions/Images/") --Prelude.mapM_ (\x -> loadFolderContents  ((unpack fp) ++ "/Output/" ++ x ++ "/")) datasetExperimentList
--               --print $ "Contents: " ++ show datasetExperimentListContents
--               --datasetList         <- loadFolderContents  ((unpack fp) ++ "/Output/" )
--               --datasetListContents <- mLoadFolderContents ((unpack fp  ++ "/Output/" ++ (datasetList !! 0) ++ "/Predictions/Images/")) -- (model ^. selectedDataset)
--               let dataSets = (Prelude.map (Dataset) datasetExperimentList)
--                   standardSource   =  ManieyeImageFolder $ sort $ Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) batchDirContents
--                   predictedSource  = case datasetExperimentListContents of 
--                     Just lst -> ManieyeImageFolder $ sort $ Prelude.map (\x -> (unpack fp  ++ "/Output/" ++ (datasetExperimentList !! 0) ++ (unpack selected) ++ "/Predictions/Images/" ++ x))  (lst)
--                     Nothing  -> ManieyeImageNothing
--               let differenceSource = ManieyeDifferenceImages ((standardSource  == ManieyeImageNothing)  ? [((model ^. userHome) ++ "/Software/manimer/assets/images/broken.png")] :?  (images standardSource)) ((predictedSource == ManieyeImageNothing)  ? [((model ^. userHome) ++ "/Software/manimer/assets/images/broken.png")] :?  (images predictedSource))
--               --Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) --batchDirContents
--               --let presenceSource =  ManieyeImageFolder $ Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) batchDirContents
--               --standardSource   <- createRenderImageSource sceneDir batchDir StandardRender      "Standard"
--               --presenceSource   <- createRenderImageSource sceneDir batchDir PresenceRender      "Presence"
--               --visSource        <- createRenderImageSource sceneDir batchDir VisualizationRender "Visualization"
                  
--               orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE ("InitialContents: " ++ (unpack fp) ++ "/Seq/")
--               orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ZONE ("datasetContents: " ++ (unpack fp) ++ "/Output/" ++ (datasetExperimentList !! 0))
--               --orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Output: " ++ (show predictedSource))
--               --orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Difference: " ++ (show differenceSource))

--               -- initialContents  <- loadDirContents    ((unpack fp) ++ "/Seq/")
--               -- datasetContents  <- loadDirContents    ((unpack fp) ++ "/Output/date-time/Predictions/Images/")
      
--               --
--               -- let diffRaws     =  Data.List.zip initialContents datasetContents
--               -- diffImageList <- mapM (\(a, b) -> do
--               --   imgA <- readManiImage ((unpack fp) ++ "/Seq/" ++ a)
--               --   imgB <- readManiImage ((unpack fp) ++ "/Output/date-time/Predictions/Images/" ++ b)

--               --   (ImageResp result) <- runFutharkCommand (fromJust $ model ^. gpuContext) $ DiffImages imgA imgB
--               --   -- writeManiImage ("/tmp/diff/" ++ a ++ ".png") result 
--               --   return $ mpImageToBytesImage result $ pack $ a ++ "diff"
--               --   ) (take 50 $ diffRaws)
--               -- return $ GPUProcess diffRaws fp
--               --

--               print batchDir
--               print $ (unpack fp) ++ "/GroundTruths/" ++  "seq_sv.mani"
--               print $ (unpack fp) ++ "/GroundTruths/" ++  "seq_mask.mani"
--               return $ UpdateImageSources (unpack fp) (datasetExperimentList) [standardSource, differenceSource, predictedSource] [] (Nothing)

