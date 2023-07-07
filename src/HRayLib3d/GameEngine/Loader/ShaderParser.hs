{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module HRayLib3d.GameEngine.Loader.ShaderParser
  ( parseShaders
  ) where

import GHC.Float ( float2Int )
import Control.Applicative hiding (many, some)
import Control.Monad
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char hiding ()
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Show.Pretty (ppShow)

import Data.Char (toLower,isSpace)
import Data.List (foldl')
import Data.Void
import LambdaCube.Linear
import Debug.Trace


-- import Data.ByteString.Char8 (ByteString,pack)
-- import HRayLib3d.Core.Parser.Language.GLSL (intConstant)
-- import qualified Data.ByteString.Char8 as BS8

import HRayLib3d.GameEngine.Data.Material

import Control.Monad.Trans.Writer.Strict
import Text.Read (readMaybe)
import qualified Data.Functor.Classes
import qualified Data.Functor.Identity

instance ShowErrorComponent (ErrorFancy Void)
type Parser = WriterT [String] (Parsec (ErrorFancy Void) String)

--https://gamedev.stackexchange.com/questions/44420/opengl-quake-3-shader-file-for-objects-for-trees
parseShaders :: String -> String -> Either String ([(String,CommonAttrs)],[String])
parseShaders fname src = case parse (runWriterT $ newlineConsumer *> many shader <* eof) fname $ map toLower src of
  Left err  -> Left $ errorBundlePretty err
  Right a   -> Right a

-- q3 shader related parsers
shader :: Parser (String, CommonAttrs)
shader = (\n l -> (n,finishShader $ foldl' (\s f -> f s) defaultCommonAttrs l)) <$>
  line filepath <* newlineSymbol "{" <*> many shaderAttribute <* newlineSymbol "}"

finishShader :: CommonAttrs -> CommonAttrs
finishShader ca = ca
    { caLight           = caLight        ca -- maybe global lighting (effect on material)
    , caSurfaceLight    = caSurfaceLight ca -- maybe fog
    , caDeformVertexes  = reverse $ caDeformVertexes ca
    , caStages          = fixedStages
    , caSort            = fixedSort
    }
  where
    -- fix sort value
    srt0        = caSort ca
    srt1        = if caIsSky ca then 2 else srt0
    srt2        = if caPolygonOffset ca && srt1 == 0 then 4 else srt1
    srt3        = snd $ foldl' fixBlendSort (True,srt2) fixedStages
      where
        fixBlendSort (False,s) _ = (False,s)
        fixBlendSort (True,s) sa = case saBlend sa of
            Nothing -> (False,s)
            _       -> let s1 = if s /= 0 then s else if saDepthWrite sa then 5 else 9 in (True,s1)

    srt4        = if srt3 == 0 then 3 else srt3
    fixedSort   = if null fixedStages then 7 else srt4

    fixedStages = reverse $ map fixStage $ caStages ca
    fixStage sa = sa
        { saTCMod       = reverse $ saTCMod sa
        , saTCGen       = tcGen
        , saRGBGen      = rgbGen
        , saBlend       = blend
        , saDepthWrite  = depthWr
        }
      where
        (depthWr,blend) = case saBlend sa of
            Just (B_One,B_Zero) -> (True,Nothing)
            a                   -> (saDepthWrite sa,a)
        rgbGen = case saRGBGen sa of
            RGB_Undefined   -> case saBlend sa of
                Nothing                 -> RGB_IdentityLighting
                Just (B_One,B_SrcAlpha) -> RGB_IdentityLighting
                _                       -> RGB_Identity
            a               -> a
        tcGen = case saTCGen sa of
            TG_Undefined    -> case saTexture sa of
                ST_Lightmap -> TG_Lightmap
                _           -> TG_Base
            a               -> a       

shaderAttribute :: Parser (CommonAttrs -> CommonAttrs)
shaderAttribute = choice [line general, stage, line unknownCommand]

general :: Parser (CommonAttrs -> CommonAttrs)
general = try $ choice [ sort, fogGen, qerLightImage, skyParms, cloudParms, backsided, sun, sky, fogParmsV, fogParms,  lightImage, lightSubdivide, nocarve, entitymergable, fogonly, lightning, globalTexture, editorImage, lightF, flare, surfaceLight, backsplash, lightMaps, surfaceparm, tesssize, cull, nopicmip, qertrans, deformVertexes, nomipmaps, polygonOffset, portal]

stage :: Parser (CommonAttrs -> CommonAttrs)
stage = (\fl ca -> ca {caStages = (foldl' (\s f -> f s) defaultStageAttrs fl):caStages ca}) <$ newlineSymbol "{" <*> many stageAttribute <* newlineSymbol "}"

stageAttribute :: Parser (StageAttrs -> StageAttrs)
stageAttribute = line $ choice
  [ try $ choice [alphaMap, alphaFunc, alphaGen, animMap, blendFunc, clampMap, depthFunc, depthWrite, detail, map_, rgbGen, tcGen, tcMod], unknownCommand]
  
-- -- Match the lowercase or uppercase form of 'c'
-- caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- -- Match the string 's', accepting either lowercase or uppercase form of each character 
-- caseInsensitiveString :: Parser String
-- caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

wave :: Parser Wave
wave = Wave <$> waveType <*> float <*> float <*> float <*> float where
  waveType = choice
    [ value WT_Sin "sin"
    , value WT_Triangle "triangle"
    , value WT_Square "square"
    , value WT_Sawtooth "sawtooth"
    , value WT_InverseSawtooth "inversesawtooth"
    , value WT_Noise "noise"
    ]

waveTypeOnly :: WriterT [String] (Parsec (ErrorFancy Void) String) WaveType
waveTypeOnly = choice
  [ value WT_Sin "sin"
  , value WT_Triangle "triangle"
  , value WT_Square "square"
  , value WT_Sawtooth "sawtooth"
  , value WT_InverseSawtooth "inversesawtooth"
  , value WT_Noise "noise"
  ]

fogGen :: Parser (CommonAttrs -> CommonAttrs)
fogGen = (\ca -> ca {caFogGen = caFogGen ca}) <$ symbol "foggen" <* waveTypeOnly <* float <* float <* float <* float

cloudParms :: Parser (CommonAttrs -> CommonAttrs)
cloudParms = (\ca -> ca {caCloudParms = caCloudParms ca}) <$ symbol "cloudparms" <* envArg <* valueF <* sizeValue 
  where 
    --image     = choice  [Nothing <$ symbol "-",  Just <$>  filepath  ]
    envArg    = choice  [Nothing <$ symbol "-",  Just <$>  filename  ]
    valueF    = choice  [Nothing <$ symbol "-",  Just <$>  float     ]
    sizeValue = choice  [Nothing <$ symbol "-",  Just <$>  sizeType  ]
    sizeType  = choice [
      NONE <$ symbol "-" 
      , value HALF "half"
      , value FULL "full"
      ]


skyParms :: Parser (CommonAttrs -> CommonAttrs)
skyParms = (\ca -> ca {caSkyParms = caSkyParms ca}) <$ skyDefinitionP
  where 
    skyDefinitionP = SkyParm <$ (symbol "skyparms" <* str <* valueF <* sizeValue)
    str       = choice  [
      Nothing <$ symbol "-",  
      Just <$> filename
      ]
    valueF    = choice  [
      Nothing <$ symbol "-",  
      Just <$> float     
      ]
    sizeValue = choice  [
      Nothing <$ symbol "-",  
      Just <$> sizeType  
      ]
    sizeType  = choice [
      NONE <$ symbol "-" 
      , value HALF "half"
      , value FULL "full"
      ]

fogParms :: Parser (CommonAttrs -> CommonAttrs)
fogParms = (\ca -> ca {caFogParms = caFogParms ca}) <$ fogInput -- symbol "fogparms" <* fogValue <* fogValue <* fogValue <* fogValue <* fogValue
  where 
    fogValue   = choice [ 
      Nothing <$ symbol "-", 
      Just    <$> float 
      ]
    fogInput   = FogDef   <$ symbol "fogparms" <* fogValue <* fogValue <* fogValue <* integer <* integer 

fogParmsV :: Parser (CommonAttrs -> CommonAttrs)
fogParmsV = (\ca -> ca {caFogParms = caFogParms ca}) <$ fogInput -- symbol "fogparms" <* fogValue <* fogValue <* fogValue <* fogValue <* fogValue
  where 
    fogValue   = choice [ 
      Nothing <$ symbol "-", 
      Just    <$> float 
      ]
    fogV3      = V3       <$ symbol "("         <* fogValue <*> fogValue <*> fogValue <* symbol ")"
    fogInput   = FogDefV3 <$ symbol "fogparmsv" <* fogV3    <*  integer

lightF :: Parser (CommonAttrs -> CommonAttrs)
lightF = (\a ca -> ca {caLight = a}) <$ symbol "light" <*> integer

surfaceLight :: Parser (CommonAttrs -> CommonAttrs)
surfaceLight = (\a ca -> ca {caSurfaceLight = a}) <$ symbol "q3map_surfacelight" <*> integer

flare :: Parser (CommonAttrs -> CommonAttrs)
flare = (\a ca -> ca {caFlare = a}) <$ symbol "q3map_flare"    <*> filename 
-- q3map_flare flareShader

qerLightImage :: Parser (CommonAttrs -> CommonAttrs)
qerLightImage = (\ca -> ca {caQerLightImage = caQerLightImage ca}) <$ symbol "qer_lightimage" <* filename 
-- q3map_flare flareShader

tesssize :: Parser (CommonAttrs -> CommonAttrs)
tesssize = (\a ca -> ca {caTessSize = a}) <$ symbol "tesssize" <*> integer

fogonly :: Parser (CommonAttrs -> CommonAttrs)
fogonly = (\ca -> ca {caFogOnly = True}) <$ symbol "fogonly"

lightning :: Parser (CommonAttrs -> CommonAttrs)
lightning = (\ca -> ca {caLightning = True}) <$ symbol "lightning"

cull :: Parser (CommonAttrs -> CommonAttrs)
cull = (\a ca -> ca {caCull = a}) <$ symbol "cull" <*> choice
  [ value CT_FrontSided "front"
  , value CT_TwoSided   "none"
  , value CT_TwoSided   "twosided"
  , value CT_TwoSided   "disable"
  , value CT_BackSided  "backsided"
  , value CT_BackSided  "backside"
  , value CT_BackSided  "back"
  ]

deformVertexes :: Parser (CommonAttrs -> CommonAttrs)
deformVertexes = (\v ca -> ca {caDeformVertexes = v:caDeformVertexes ca}) <$ symbol "deformvertexes" <*> choice
    [ value D_AutoSprite2 "autosprite2"
    , value D_AutoSprite  "autosprite"
    , D_Bulge  <$ symbol  "bulge"  <*> float <*> float <*> float
    , D_Move   <$ symbol  "move"   <*> v3    <*> wave
    , D_Normal <$ symbol  "normal" <*> float <*> float -- amplitude, frequency
    , value D_ProjectionShadow "projectionshadow"
    , value D_Text0 "text0"
    , value D_Text1 "text1"
    , value D_Text2 "text2"
    , value D_Text3 "text3"
    , value D_Text4 "text4"
    , value D_Text5 "text5"
    , value D_Text6 "text6"
    , value D_Text7 "text7"
    , (\s w -> D_Wave (if s == 0 then 100 else 1/s) w) <$ symbol "wave" <*> float <*> wave
    ]
  where
    v3 = V3 <$> float <*> float <*> float

nomipmaps :: Parser (CommonAttrs -> CommonAttrs)
nomipmaps = (\ca -> ca {caNoMipMaps = True}) <$ symbol "nomipmaps"

nopicmip    :: Parser (CommonAttrs -> CommonAttrs)
nopicmip  = (\ca -> ca {caNoPicMip  = True}) <$ symbol "nopicmip"
-- q3map_lightimage textures/hell/ironcrosslt1.blend.tga

polygonOffset :: Parser (CommonAttrs -> CommonAttrs)
polygonOffset = (\ca -> ca {caPolygonOffset = True}) <$ symbol "polygonoffset"

portal :: Parser (CommonAttrs -> CommonAttrs)
portal = (\ca -> ca {caSort = 1}) <$ symbol "portal"

sort :: Parser (CommonAttrs -> CommonAttrs)
sort = (\i ca -> ca {caSort = i}) <$ symbol "sort" <*> choice
  [ value 1  "portal"
  , value 2  "sky"
  , value 3  "opaque"
  , value 4  "decal"
  , value 5  "seethrough"
  , value 6  "banner"
  , value 10 "additive"
  , value 16 "nearest"
  , value 8  "underwater"
  , float
  ]

map_ :: Parser (StageAttrs -> StageAttrs)
map_ = (\v sa -> sa {saTexture = v}) <$ symbol "map" <*> choice
  [ value ST_Lightmap    "$lightmap"
  , value ST_FromBlack   "$fromblack"
  , value ST_WhiteImage  "$whiteimage"
  , ST_Map <$> filepath
  ]

clampMap :: Parser (StageAttrs -> StageAttrs)
clampMap = (\v sa -> sa {saTexture = ST_ClampMap v}) <$> (symbol "clampmap" *> filepath)

alphaMap :: Parser (StageAttrs -> StageAttrs)
alphaMap = (\sa -> sa {saAlphaMap = saAlphaMap sa})  <$ symbol "alphamap" <* alphaMapValue
  where 
    alphaMapValue = choice [
        value    ST_None      "$none"
        , value  ST_FromBlack "$fromblack"
        , ST_Map <$> filepath
      ]

animMap :: Parser (StageAttrs -> StageAttrs)
animMap = (\f v sa -> sa {saTexture = ST_AnimMap f v})   <$ symbol "animmap" <*> float <*> some filepath

backsplash :: Parser  (CommonAttrs -> CommonAttrs)
backsplash  = (\v sa -> sa  {caBackSplash  = v}) <$> backsplashData

backsplashData :: Parser BackSplash
backsplashData = BackSplash <$ symbol "q3map_backsplash" <*> float <*> float 

qertrans :: Parser  (CommonAttrs -> CommonAttrs)
qertrans  = (\v sa -> sa  {caQerTrans  = v}) <$> qerTransData

qerTransData :: Parser QER_Trans
qerTransData = QER_Trans <$ symbol "qer_trans" <*> float

surfaceparm :: Parser (CommonAttrs -> CommonAttrs)
surfaceparm = (\v sa -> sa {caSurfaceParm  = v}) <$ symbol "surfaceparm" <*> surfaceParmData

backsided :: Parser (CommonAttrs -> CommonAttrs)
backsided = (\sa -> sa {caBackSided  = caBackSided sa}) <$ symbol "backsided"

surfaceParmData :: Parser SurfaceParam
surfaceParmData = choice
  [ value AreaPortal    "areaportal"
  , value ClusterPortal "clusterportal"
  , value Detail        "detail"
  , value DoNotEnter    "donotenter"
  , value None          "none"
  , value NoDraw        "nodraw"
  , value NoDamage      "nodamage"
  , value NoDrop        "nodrop"
  , value NoDLight      "nodlight"
  , value NoLightMap    "nolightmap"
  , value NoImpact      "noimpact"
  , value NoMarks       "nomarks"
  , value NoMipMaps     "nomipmaps"
  , value NonSolid      "nonsolid"
  , value Origin        "origin"
  , value PlayerClip    "playerclip"
  , value LightFilter   "lightfilter"
  , value MetalSteps    "metalsteps"
  , value Slick         "slick"
  , value Structural    "structural"
  , value Trans         "trans"
  , value AlphaShadow   "alphashadow"
  , value Lava          "lava"
  , value Water         "water"
  , value Slime         "slime"
  , value Fog           "fog"
  , value Sky           "sky"
  ]

globalTexture :: Parser (CommonAttrs -> CommonAttrs)
globalTexture = (\sa -> sa {caGlobalTexture = True})    <$  symbol "q3map_globaltexture"
-- q3map_globaltexture -- Is this actually a StageAttr?

editorImage :: Parser (CommonAttrs -> CommonAttrs)
editorImage = (\v sa -> sa {caEditorImage = ST_Map v})  <$> (symbol "qer_editorimage"  *> filepath)
-- qer_editorimage textures/hell/ironcrosslt1.tga

sky :: Parser (CommonAttrs -> CommonAttrs)
sky = (\v sa -> sa {caSky = v})  <$> skyDefinition
-- sky        env/xnight2

skyDefinition  :: Parser SkyDef
skyDefinition  = SkyDef <$ symbol "sky"  <*> filepath

sun  :: Parser (CommonAttrs -> CommonAttrs)
sun  = (\v sa -> sa {caSun  = v}) <$> sunDefinition
  
sunDefinition :: Parser SunDef
sunDefinition = SunDef <$ symbol "q3map_sun" <*> integer <*> integer  <*> integer <*> integer <*> integer  <*> integer

lightImage  :: Parser (CommonAttrs -> CommonAttrs)
lightImage  = (\v sa -> sa {caLightImage   = ST_Map v}) <$> (symbol "q3map_lightimage" *> filepath)
-- q3map_lightimage textures/hell/ironcrosslt1.blend.tga

lightSubdivide  :: Parser (CommonAttrs -> CommonAttrs)
lightSubdivide  = (\v sa -> sa {caLightSubdivide   = v}) <$ symbol "q3map_lightsubdivide" <*> float
-- q3map_lightsubdivide 32

nocarve :: Parser (CommonAttrs -> CommonAttrs)
nocarve = (\sa -> sa {caNoCarve = True}) <$ symbol "qer_nocarve"

entitymergable :: Parser (CommonAttrs -> CommonAttrs)
entitymergable = (\sa -> sa {caEntityMergable = True}) <$ symbol "entitymergable"

lightMaps :: Parser (CommonAttrs -> CommonAttrs)
lightMaps = (\i ca -> ca {caSort = i}) <$ symbol "sort" <*>  choice
  [ value 1    "light"
  , value 1400 "q3map_surfacelight"
  ]	

blendFuncFunc :: Parser (Blending, Blending)
blendFuncFunc = choice
  [ value (B_One,B_One) "add"
  , value (B_One,B_One) "gl_add"
  , value (B_DstColor,B_Zero) "filter"
  , value (B_SrcAlpha,B_OneMinusSrcAlpha) "blend"
  ]

srcBlend :: Parser Blending
srcBlend = choice
  [ value B_DstAlpha "gl_dst_alpha"
  , value B_DstColor "gl_dst_color"
  , value B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , value B_OneMinusDstColor "gl_one_minus_dst_color"
  , value B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , value B_One "gl_one"
  , value B_SrcAlphaSaturate "gl_src_alpha_saturate"
  , value B_SrcAlpha "gl_src_alpha"
  , value B_Zero "gl_zero"
  ]

dstBlend :: Parser Blending
dstBlend = choice
  [ value B_DstAlpha "gl_dst_alpha"
  , value B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
  , value B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
  , value B_OneMinusSrcColor "gl_one_minus_src_color"
  , value B_One "gl_one"
  , value B_SrcAlpha "gl_src_alpha"
  , value B_SrcColor "gl_src_color"
  , value B_Zero "gl_zero"
  ]

blendFunc :: Parser (StageAttrs -> StageAttrs)
blendFunc = (\b sa -> sa {saBlend = Just b, saDepthWrite = dpWr sa}) <$ symbol "blendfunc" <*> choice [blendFuncFunc, (,) <$> srcBlend <*> dstBlend]
  where
    dpWr sa = saDepthMaskExplicit sa && saDepthWrite sa

rgbGen :: Parser (StageAttrs -> StageAttrs)
rgbGen = (\v sa -> sa {saRGBGen = v, saAlphaGen = alpha sa v}) <$ symbol "rgbgen" <*> choice
  [ RGB_Wave  <$ symbol "wave" <*> wave
  , RGB_Const <$ symbol "const" <* symbol "(" <*> float <*> float <*> float <* symbol ")"
  , value RGB_Entity "entity"
  , value RGB_ExactVertex "exactvertex"
  , value RGB_IdentityLighting "identitylighting"
  , value RGB_Identity "identity"
  , value RGB_LightingDiffuse "lightingdiffuse"
  , value RGB_OneMinusEntity "oneminusentity"
  , value RGB_OneMinusVertex "oneminusvertex"
  , value RGB_Vertex "vertex"
  ]
  where
    alpha sa v = case v of
        RGB_Vertex  -> case saAlphaGen sa of
            A_Identity  -> A_Vertex
            _           -> saAlphaGen sa
        _           -> saAlphaGen sa

alphaGen :: Parser (StageAttrs -> StageAttrs)
alphaGen = (\v sa -> sa {saAlphaGen = v}) <$ symbol "alphagen" <*> choice
  [ A_Wave <$ symbol         "wave"  <*> wave
  , A_Const <$ symbol        "const" <*> float
  , value A_Entity           "entity"
  , value A_Identity         "identity"
  , value A_LightingSpecular "lightingspecular"
  , value A_OneMinusEntity   "oneminusentity"
  , value A_OneMinusVertex   "oneminusvertex"
  , value A_Portal           "portal" <* float
  , value A_Vertex           "vertex"
  ]

tcGen :: Parser (StageAttrs -> StageAttrs)
tcGen = (\v sa -> sa {saTCGen = v}) <$ (symbol "texgen" <|> symbol "tcgen") <*> choice
  [ value TG_Environment "environment"
  , value TG_Lightmap "lightmap"
  , value TG_Base "texture"
  , value TG_Base "base"
  , TG_Vector <$ symbol "vector" <*> v3 <*> v3
  ]
  where
    v3 = V3 <$ symbol "(" <*> float <*> float <*> float <* symbol ")"

tcMod :: Parser (StageAttrs -> StageAttrs)
tcMod = (\v sa -> sa {saTCMod = v:saTCMod sa}) <$ symbol "tcmod" <*> choice
  [ value TM_EntityTranslate "entitytranslate"
  , TM_Rotate2d  <$ symbol "rotatetd"  <*> float <*> float 
  , TM_Rotate    <$ symbol "rotate"    <*> float 
  , TM_Scroll    <$ symbol "scroll"    <*> float <*> float -- <*> inputArgumentType   <*> inputArgumentType -- <*> inputArgumentType <*> inputArgumentType --(Just <$> float) {-inputArgumentType-}                  --TM_Scroll    <$ symbol "scroll" <*> float <*> float
  , TM_ScaleX    <$ symbol "scalex"    <*> float <*> float <*> float <*> float
  , TM_Scale     <$ symbol "scale"     <*> float <*> float
  , TM_Stretch   <$ symbol "stretch"   <*> wave  
  , TM_TurbL     <$ symbol "turbl"     <*> waveTypeOnly     <*> float <*> float <*> float <*> float --wave -- wave only works for shaders which use the base argument
  , TM_Transform <$ symbol "transform" <*> float <*> float  <*> float <*> float <*> float <*> float
  , TM_Turb      <$ symbol "turb"      <*> float <*> float  <*> float <*> float 
  ] 
  --where 
    --inputArgumentType   =  choice [ Nothing <$ symbol "tcmod", Just <$> float <*> float  <*> float <*> float ]

depthFunc     :: Parser (StageAttrs -> StageAttrs)
depthFunc  = (\v sa -> sa {saDepthFunc = v}) <$ symbol "depthfunc" <*> (value D_Lequal "lequal" <|> value D_Equal "equal")

depthWrite    :: Parser (StageAttrs -> StageAttrs)
depthWrite = (\sa -> sa {saDepthWrite = True, saDepthMaskExplicit = True}) <$ symbol "depthwrite"

detail :: Parser(StageAttrs -> StageAttrs)
detail = (\sa -> sa {saDetail = True})     <$ symbol  "detail"

alphaFunc     :: Parser (StageAttrs -> StageAttrs)
alphaFunc  = (\v sa -> sa {saAlphaFunc = Just v}) <$ symbol "alphafunc" <*> (value A_Gt0 "gt0" <|> value A_Lt128 "lt128" <|> value A_Ge128 "ge128")

-- parser primitives
lineComment   :: Parser ()
lineComment = L.skipLineComment "//"

blockComment  :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ oneOf (" \t" :: String)) lineComment blockComment

newlineConsumer :: Parser ()
newlineConsumer = L.space (void spaceChar) lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer -- do not consumes line breaks

newlineSymbol :: String -> Parser String
newlineSymbol = L.symbol newlineConsumer -- consumes line breaks

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

float :: Parser Float
float = realToFrac <$> L.signed spaceConsumer (lexeme floatLiteral) where
  floatLiteral = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.decimal
    ]

integer :: Parser Int
integer = float2Int <$> L.signed spaceConsumer (lexeme intLiteral) where
  intLiteral = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.decimal
    ]

filepath :: Parser String
filepath = lexeme $ some (satisfy $ not . isSpace)

filename :: Parser String
filename = filepath

value :: a -> String -> Parser a
value v w = const v <$> symbol w

line :: Parser a -> Parser a
line p = p <* skipTillEol <* newlineConsumer

skipTillEol :: Parser ()
skipTillEol = do
  n   <- lookAhead (choice [eol, string "{", string "}"])
  pos <- getSourcePos
  cmd <- string n
  unless (null cmd) $ tell ["LEFTOVER - " ++ sourcePosPretty pos ++ ": " ++ cmd]
  return ()

unknownCommand :: Parser (a -> a)
unknownCommand = do
  n   <-lookAhead (choice [eol])
  pos <- getSourcePos
  cmd <- some alphaNumChar
  args <- string n
  tell ["IGNORE - " ++ sourcePosPretty pos ++ ": " ++ cmd ++ args]
  return id

-- simple test
test :: IO ()
test = do
  let n = "/Users/csaba/games/quake3/unpack/scripts/base.shader"
  src <- readFile n
  case parseShaders n $ src of
    Left  e -> putStrLn e
    Right (x,w) -> putStrLn $ ppShow x ++ "\n" ++ unlines w
