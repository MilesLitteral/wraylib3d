{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-#OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- data Image layout ty  = Image
--     { layoutOf :: layout
--     , unImage  :: Array S Ix3 ty
--     } deriving (Show)

module Manifest.Utils.Conversion where 

import GHC.Word (Word8)

import Data.Text
import Data.Int (Int64)
import Data.Either (Either(Left, Right))
import Data.List

import Data.Massiv.Array.IO 
import GHC.Utils.Misc (uncurry3)
import qualified Data.ByteString.Internal as BS
import qualified Data.Massiv.Array.IO as I
import qualified Data.Massiv.Array as A
import qualified Data.Vector.Storable as V
import qualified Data.HashMap.Internal as Map
import Data.Vector.Storable.ByteString (vectorToByteString, byteStringToVector)

import qualified Data.Aeson as J
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import qualified Text.Read as R

import Control.Monad
import Control.Exception(onException)

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Codec.Picture.Png
import qualified Codec.Picture.Types as M

import System.FilePath
import System.Directory
import qualified Graphics.ColorModel as CM
import qualified Graphics.Pixel.ColorSpace as S

import Manifut
import Manifut.Fut
import Manifest.Types
import Manifest.Utils.Log
import Manifest.Utils.Futhark 
import qualified Manifut.Wrap as U
import qualified Manifut.Raw as Raw
import qualified Manifut.Context as C
import qualified Manifut.Entries as E
import qualified Manipipe as MP
import qualified Manipipe.Image as MP
import qualified Manipipe.Local.Type as MP
import qualified Manipipe.Local.LocalBlender as MP

import Monomer
import Monomer.Core.FromFractional
import qualified Monomer.Common.BasicTypes as M

readManiImage :: forall m l
          . ( A.MonadIO m
            , MP.IsImageLayout l
            )
          => FilePath
          -> m (MP.Image l Float)
readManiImage fileName = do 
    img <- A.liftIO $ I.readImage fileName --assume RGBA
    return $ MP.toLayout (MP.Image (A.compute . A.map MP.word8ToFloat . MP.pixelToRgba $ img))

writeManiImage :: A.MonadIO m
           => String
           -> MP.Image MP.RGBA Float
           -> m ()
writeManiImage fileName img = do 
  let  pixelArray :: A.Array A.S A.Ix2 (CM.Pixel (Alpha CM.RGB) Word8) = MP.rgbaToPixel . A.compute . A.map MP.floatToWord8 . MP.unImage . MP.fromLayout $ img
  A.liftIO $ I.writeImage fileName pixelArray

fast                     :: (a,b,c) -> a
fast (x,_,_)               =  x

loadBytesImage :: FilePath -> IO (BytesImage)
loadBytesImage path = do
    dynImg  <- Codec.Picture.readImage path
    let img :: Either String (Codec.Picture.Types.Image PixelRGBA8) = case dynImg of
                Left msg -> Left msg
                Right img -> Right $ convertRGBA8 img
    let result = case img of
                    Left _ -> emptyBytesImage  
                    Right img -> BytesImage (Data.Text.pack path) (uncurry3 BS.fromForeignPtr $ V.unsafeToForeignPtr  (imageData img)) (M.Size (fromIntegral $ Codec.Picture.Types.imageWidth img) (fromIntegral $ Codec.Picture.Types.imageHeight img))
    case img of
      Left _ -> putStrLn $ "Failed to open image: " ++ path 
      _ -> return ()
    return result

--(BS.fromForeignPtr $ V.unsafeToForeignPtr (imageData img))
decodeJpegToBytesImage :: BS.ByteString -> String -> BytesImage
decodeJpegToBytesImage jpegBs path = 
  let eDynImg = Codec.Picture.decodeJpeg jpegBs
      img = case eDynImg of 
        Left errMsg -> error errMsg
        Right img -> convertRGBA8 img
  in imageToBytesImage path img

decodePngToBytesImage :: BS.ByteString -> String -> BytesImage
decodePngToBytesImage pngBs path = 
  let eDynImg = Codec.Picture.decodePng pngBs
      img = case eDynImg of 
        Left errMsg -> error errMsg
        Right img -> convertRGBA8 img
  in imageToBytesImage path img
      

-- | Converts a Manipipe/Futhark/ManiImage into a BytesImage useable by Manifest
mpImageToBytesImage ::  MP.Image MP.RGBA Float -> Data.Text.Text -> BytesImage
mpImageToBytesImage array name = 
  let 
    (A.Sz3 h w _d) = A.size (MP.unImage array)
    convertedArray = A.map MP.floatToWord8 (MP.unImage array)
    flattenedArray = A.flatten convertedArray
    -- asBytes        = B.runPut (mapM_ B.put flattenedArray)
  in 
    BytesImage name (A.toByteString flattenedArray) (M.Size (fromIntegral w) (fromIntegral h)) 
    
imageToBytesImage :: FilePath -> Codec.Picture.Types.Image PixelRGBA8 -> BytesImage
imageToBytesImage path (Codec.Picture.Types.Image w h pixels) = BytesImage (pack path) (vectorToByteString pixels) (M.Size (fromIntegral w) (fromIntegral h))

getNthImage :: ManieyeImageSource -> Int -> IO (Maybe BytesImage) 
getNthImage source n = case source of
  ManieyeImageFolder imageList -> do
    let imagePath = imageList !! n
    result <- loadBytesImage imagePath
    return $ Just result
    `onException` return Nothing
  ManieyeImageRender lb renType sceneVectors grpName -> do
    mImage <- case renType of 
      StandardRender      -> MP.localSingleRenderQueued (MP.Mani $ sceneVectors A.!> n) lb
      PresenceRender      -> MP.localTrackingPointRenderQueued (MP.Mani $ sceneVectors A.!> n) lb
      VisualizationRender -> MP.localSingleRenderQueued (MP.Mani $ sceneVectors A.!> n) lb
    case mImage of
      Just image -> return $ Just $ mpImageToBytesImage image $ pack $ grpName ++ (show n) ++ ".png"
      Nothing    -> return Nothing
  ManieyeDifferenceFolder images -> do
    putStrLn $ "Diff images len: " ++ (show $ Data.List.length images)
    let result = images !! n
    putStrLn $ "Image size: " ++ (show $ size result)
    return $ Just $ images !! n 
    `onException` return Nothing
  ManieyeDifferenceImages a b -> do
    let imagePathA = a !! n
    let imagePathB = b !! n
    resultA <- loadBytesImage imagePathA
    resultB <- loadBytesImage imagePathB
    let diffBytesImage = findImageDifference resultA resultB 
    return $ Just diffBytesImage
    `onException` return Nothing
  ManieyeImageNothing -> return Nothing
  _ -> return Nothing

findImageDifference :: BytesImage -> BytesImage -> BytesImage
findImageDifference a b = 
    let imgA = bytesImageToImage a 
        imgB = bytesImageToImage b
        imgDiff = diff imgA imgB
    in
        imageToBytesImage ((Data.Text.unpack $ name a) ++ "Diff") imgDiff
  
createRenderImageSource :: FilePath -> FilePath -> RenderType -> String -> IO (ManieyeImageSource)
createRenderImageSource sceneInfoPath batchInfoPath renderType name = do
  homeDirectory <- getHomeDirectory
  lb <- MP.startLocalBlender $ homeDirectory </> "venv/blend/bin/python"
  (Just sceneJson) <- J.decodeFileStrict $ sceneInfoPath ++ "scene_def.json"
  MP.sendSceneToLocalBlender sceneJson sceneInfoPath lb
  (MP.Mani sceneVectors) <- MP.loadManifile $ batchInfoPath </> "sv.mani"
  (Just renderOptions) <- J.decodeFileStrict $ batchInfoPath ++ "render_options.json"
  updatedVectors <- case renderType of 
    StandardRender -> do
      MP.sendRenderOptionsToLocalBlender (MP.StandardMode renderOptions) lb
      return sceneVectors
    PresenceRender -> do
      trackingPoints  <- MP.loadManifile $ batchInfoPath </> "tracking_points.mani"
      presenceObjects <- MP.loadManifile $ batchInfoPath </> "presence_objects.mani"
      MP.sendRenderOptionsToLocalBlender (MP.PresenceMode (setUVRenderMode renderOptions) trackingPoints presenceObjects) lb
      return sceneVectors
    VisualizationRender -> do 
      (Just visOptions) <- J.decodeFileStrict $ batchInfoPath </> "vis_options.json"
      (MP.Mani masks)   <- MP.loadManifile $ batchInfoPath </> "masks.mani"
      MP.sendRenderOptionsToLocalBlender (MP.VisualizationMode renderOptions visOptions) lb
      return $ A.computeAs A.S $ A.append' (A.Dim 1) sceneVectors masks :: IO (A.Array A.S A.Ix2 Float) 

  return $ ManieyeImageRender lb renderType updatedVectors name

-- | These Are Pixel related Utilities for JuicyPixels Images; This is the compiled type of Images displayed by Manifest (in the Front-End)
-- ManiImages (Manipipe.Image) is the Type used for GPU work and by Futhark
-- These are the ONLY two image formats utilized by Manifest, this will be TYPE ENFORCED.

isImage :: FilePath -> Bool
isImage path = or [(Data.List.isSuffixOf ".png" path), (Data.List.isSuffixOf ".PNG" path), (Data.List.isSuffixOf ".jpg" path), (Data.List.isSuffixOf ".JPG" path), (Data.List.isSuffixOf ".jpeg" path), (Data.List.isSuffixOf ".JPEG" path)]

raw :: Either String DynamicImage -> DynamicImage 
raw i = case i of 
    Right b  -> b
    Left  x  -> undefined

-- | Get The Absolute Negative of a JuicyPixels Image
negative :: Codec.Picture.Types.Image PixelRGBA8 -> Codec.Picture.Types.Image PixelRGBA8
negative = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a

-- | Get The Absolute Difference between two images
diff :: Codec.Picture.Types.Image PixelRGBA8 -> Codec.Picture.Types.Image PixelRGBA8 -> Codec.Picture.Types.Image PixelRGBA8
diff a b = M.pixelMapXY (pixelDiff a) b

-- | Create a Blank PNG image
imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

-- | Get the Difference (Per-Pixel) between two images
pixelDiff :: Codec.Picture.Types.Image PixelRGBA8 -> Int -> Int -> PixelRGBA8 -> PixelRGBA8
pixelDiff img x y (PixelRGBA8 ra ga ba aa) = let (PixelRGBA8 rb gb bb ab) = pixelAt img x y 
                                                in PixelRGBA8 (channelDiff ra rb) (channelDiff ga gb) (channelDiff ba bb) 255 

-- | Get the Difference (Per-Channel) between two bytes
channelDiff :: Word8 -> Word8 -> Word8
channelDiff a b = let ia :: Int = fromIntegral a
                      ib :: Int = fromIntegral b
                      in fromIntegral $ abs (ia - ib) 

-- | Create a Negative from input (path) and save the results at output (path)
dynamicNegative :: FilePath -> FilePath -> FilePath -> IO ()
dynamicNegative  path output outputName = do
    dynamicImage <- Codec.Picture.readImage path
    let image = convertRGBA8 <$> dynamicImage
    let modified = negative  <$> image
    case modified of
            Left err -> print err
            Right image -> savePngImage (output ++ outputName) $ ImageRGBA8 image --"negative.png"

-- | Create a Negative from input (path) and save the results at output (path)
dynamicDiff :: FilePath -> FilePath -> FilePath -> IO ()
dynamicDiff  patha pathb output = do
    dynImgA <- Codec.Picture.readImage patha
    dynImgB <- Codec.Picture.readImage pathb
    let imgA = case dynImgA of
                    Left err  -> undefined
                    Right img -> convertRGBA8 img
    let imgB = case dynImgB of
                    Left err  -> undefined
                    Right img -> convertRGBA8  img
    let modified = (diff imgA imgB)
    x <- doesFileExist output  
    if x == True
        then orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_MESSAGE (output ++ " Already Exists")
        else savePngImage output $ ImageRGBA8 modified

getImages :: FilePath -> IO ([FilePath])
getImages path = do 
    exists <- doesDirectoryExist path
    if not exists 
        then return []
        else do
                dirContents <- listDirectory path
                let contentsWithPath = Data.List.map (\f -> path </> f) dirContents 
                filesOnly <- filterM doesFileExist contentsWithPath
                return filesOnly

splitStringOnChar     :: Char -> String -> [String]
splitStringOnChar char s =
      let compareChar = (\c -> c == char) in
      case Data.List.dropWhile compareChar s of
                      "" -> []
                      s' -> w : splitStringOnChar char s''
                            where (w, s'') = Data.List.break compareChar s'


numberFromImagePath :: FilePath -> Maybe Int
numberFromImagePath path = 
    let parts = splitStringOnChar '.' (takeFileName path) in
    case Data.List.length parts of
        2 -> R.readMaybe $ Data.List.head parts
        3 -> R.readMaybe $ Data.List.head parts
        _ -> Nothing

getImagesWithNum :: FilePath -> IO (Map.HashMap Int FilePath)
getImagesWithNum path = do 
    exists <- doesDirectoryExist path
    if not exists 
        then return Map.empty
        else do
                dirContents <- listDirectory path
                let contentsWithPath = Data.List.map (\f -> path </> f) dirContents 
                filesOnly <- filterM doesFileExist contentsWithPath
                let imagesOnly = Data.List.filter isImage filesOnly
                let maybeNums = Data.List.map numberFromImagePath imagesOnly
                let result = Map.fromList $ Data.List.foldl (\l (n,p) -> case n of
                                                        Just num -> l ++ (Data.List.singleton (num, p))
                                                        Nothing -> l) [] (Data.List.zip maybeNums imagesOnly)
                return result

zipImages :: [FilePath] -> [FilePath] -> [(FilePath, Maybe FilePath)]
zipImages a b = Data.List.zip a paddedB where paddedB = (Data.List.map Just b) ++ Data.List.take (Data.List.length a - Data.List.length b) (repeat Nothing)

pairImagesByNum :: Map.HashMap Int FilePath -> Map.HashMap Int FilePath -> [(FilePath, Maybe FilePath)]
pairImagesByNum a b = Data.List.map (\(n,p)->(p, Map.lookup n b)) (Map.toList a)

bytesImageToImage :: BytesImage -> Codec.Picture.Image PixelRGBA8
bytesImageToImage (BytesImage _name bytes (Monomer.Size w h)) = 
    let imgData::V.Vector Pixel8 = byteStringToVector bytes  in
        Image (fromFractional w) (fromFractional h) imgData

findInputDir :: FilePath -> IO (Maybe FilePath)
findInputDir basePath = do 
    let inputDirPath = basePath ++ "/Seq/"
    dirExists <- doesDirectoryExist inputDirPath
    let result = if dirExists 
                    then Just inputDirPath
                    else Nothing
    return result

findPredDir :: FilePath  -> IO (Maybe FilePath)
findPredDir basePath = do 
    let predDirPath = basePath ++ "/Train/"
    dirExists <- doesDirectoryExist predDirPath
    let result = if dirExists 
                    then Just predDirPath
                    else Nothing
    return result

-- Juicy Pixels Scaling Functions 
-- Be Warned that these functions will create a JuicyPixels (Image a) rather than Manipipe (Image l ty)  
-- their representation types (a ≠ ty) have a similar naming convention structure but are not equivalent;
-- for this reason use literal references to Manipipe.Image or Codec.Pictures.Types avoid any confusion 
basicScaleBilinear :: (Codec.Picture.Types.Pixel PixelRGBA8, Bounded (PixelBaseComponent PixelRGBA8), Integral (PixelBaseComponent PixelRGBA8))   => Int	-> Int -> Codec.Picture.Types.Image PixelRGBA8 -> Codec.Picture.Types.Image PixelRGBA8 
basicScaleBilinear = scaleBilinear

basicScaleBilinearBS :: (Codec.Picture.Types.Pixel PixelRGBA8, Bounded (PixelBaseComponent PixelRGBA8), Integral (PixelBaseComponent PixelRGBA8)) => Int  -> Int -> Codec.Picture.Types.Image PixelRGBA8 -> BS.ByteString
basicScaleBilinearBS sx sy img =   do
  let image = scaleBilinear sx sy img
  BL.toStrict $ Codec.Picture.encodePng $ image

basicScaleBilinearBI :: (Codec.Picture.Types.Pixel PixelRGBA8, Bounded (PixelBaseComponent PixelRGBA8), Integral (PixelBaseComponent PixelRGBA8)) => String -> Double -> Double -> Codec.Picture.Types.Image PixelRGBA8 -> BytesImage
basicScaleBilinearBI name newX newY img = do
    let iX :: Int = fromFractional newX 
    let iY :: Int = fromFractional newY
    let scaled = scaleBilinear (iX) (iY) img
    imageToBytesImage name scaled
    
-- | JuicyPixels: Endomorphic Modifications to Images
basicCrop :: Codec.Picture.Types.Pixel a => Int	-> Int -> Int -> Int -> Codec.Picture.Types.Image a	-> Codec.Picture.Types.Image a	
basicCrop = crop

basicFlipHorizontally :: Codec.Picture.Types.Pixel a => Codec.Picture.Types.Image a -> Codec.Picture.Types.Image a
basicFlipHorizontally = flipHorizontally

basicFlipVertically :: Codec.Picture.Types.Pixel a => Codec.Picture.Types.Image a -> Codec.Picture.Types.Image a
basicFlipVertically = flipVertically

basicRotateLeft90  :: Codec.Picture.Types.Pixel a => Codec.Picture.Types.Image a -> Codec.Picture.Types.Image a
basicRotateLeft90  = rotateLeft90

basicRotateRight90 :: Codec.Picture.Types.Pixel a => Codec.Picture.Types.Image a -> Codec.Picture.Types.Image a
basicRotateRight90 = rotateRight90

basicRotate180 :: Codec.Picture.Types.Pixel a => Codec.Picture.Types.Image a -> Codec.Picture.Types.Image a
basicRotate180 = rotate180

basicBeside :: Codec.Picture.Types.Pixel a => [Codec.Picture.Types.Image a] -> Codec.Picture.Types.Image a
basicBeside = beside

basicBelow  :: Codec.Picture.Types.Pixel a => [Codec.Picture.Types.Image a] -> Codec.Picture.Types.Image a 
basicBelow = below

-- Futhark Scaling Functions
bicubicInterpolateImageF :: A.MonadIO m => Int64 -> Int64 -> F32_3d -> FutT m F32_3d
bicubicInterpolateImageF = E.bicubicInterpolateImage

generateThumbnailF :: A.MonadIO m =>   F32_3d -> FutT m F32_3d
generateThumbnailF input = bicubicInterpolateImageF 32 32 input

generateAvatarF ::    A.MonadIO m =>   F32_3d -> FutT m F32_3d
generateAvatarF input  = bicubicInterpolateImageF  64 64  input

generatePreviewF ::   A.MonadIO m =>   F32_3d -> FutT m F32_3d
generatePreviewF input = bicubicInterpolateImageF 125 125 input

{-
  --Monomer Style: Alerts, Warnings, Confirmations
  -- Descriptor -> Event
  confirmationDialog :: (Typeable s, Typeable e) => e -> WidgetNode () e -> WidgetNode s e
  confirmationDialog d e = alert d e

  draggableDialog :: (Eq a, Typeable a, Typeable s, Typeable e) => a -> e -> WidgetNode () e -> WidgetNode s e
  draggableDialog msg d e = draggable msg (alert d e)

  customDialog :: (Typeable s, Typeable e) => e  -> [AlertCfg]  -> WidgetNode () e -> WidgetNode s e
  customDialog e c d = alert_ e c d

  --OS specific File Dialogs
  osSaveFileDialog :: Text -> Text	-> [Text]	-> Text	-> IO (Maybe Text)	
  osSaveFileDialog = saveFileDialog

  osOpenFileDialog :: Text -> Text	-> [Text]	-> Text	-> Bool	-> IO (Maybe [Text])	
  osOpenFileDialog = openFileDialog

  osSelectFolderDialog :: Text	-> Text	-> IO (Maybe Text)	
  osSelectFolderDialog = selectFolderDialog

  {- 
    evt:ManimerEvt configs:AlertCfg dialogBody:Text
    --
    -- data AlertCfg = AlertCfg {
    --   _alcTitle :: Maybe Text,
    --   _alcClose :: Maybe Text
    -- } 
  -}
-}