{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, FlexibleInstances #-}
module HRayLib3d.GameEngine.Data.Material where

import Data.Binary       ( Binary  )
import GHC.Generics      ( Generic )
import LambdaCube.Linear ( V4, V3(..) )

type Vec3 = V3 Float
type Vec4 = V4 Float

data Entity
    = Entity
    { eAmbientLight     :: Vec4
    , eDirectedLight    :: Vec4
    , eLightDir         :: Vec3
    , eShaderRGBA       :: Vec4
    }
    deriving (Generic)

data WaveType
    = WT_None
    | WT_Sin
    | WT_Triangle
    | WT_Square
    | WT_Sawtooth
    | WT_InverseSawtooth
    | WT_Noise
    deriving (Show,Generic)

data Wave         = Wave !WaveType !Float !Float !Float !Float deriving (Show, Generic)
data SurfaceLight = SurfaceLight   !Float                      deriving (Show, Generic)
data SurfaceType  = SL                                         deriving (Show, Generic)

data Deform
    = D_AutoSprite
    | D_AutoSprite2
    | D_Bulge  !Float !Float !Float
    | D_Move   !Vec3  !Wave
    | D_Normal !Float !Float
    | D_ProjectionShadow
    | D_Text0
    | D_Text1
    | D_Text2
    | D_Text3
    | D_Text4
    | D_Text5
    | D_Text6
    | D_Text7
    | D_Wave !Float !Wave
    deriving (Show, Generic)

data CullType
    = CT_FrontSided
    | CT_BackSided
    | CT_TwoSided
    deriving (Show, Generic)

data SkyDef  = SkyDef  !String        deriving (Show,Generic)
-- Ex: sky	"env/castle"

data SunDef         = SunDef         !Int   !Int    !Int  !Int   !Int  !Int  deriving (Show, Generic) -- ex: q3map_sun	0.9 0.8 1 25 135 60
data CloudDef       = CloudDef       !Int   !Int    !Int                     deriving (Show, Generic)
data FogDef         = FogDef         !(Maybe Float) !(Maybe Float) !(Maybe Float)  !(Maybe Int)  !(Maybe Int)       deriving (Show, Generic) -- ex: fogparms 0 0 0 400 256
data FogDefV3       = FogDefV3       !(V3 Float)    !Int                     deriving (Show, Generic) -- ex: fogparms 0 0 0 400 256
data BackSplash     = BackSplash     !Float !Float                           deriving (Show, Generic)
data QER_Trans      = QER_Trans      !Float                                  deriving (Show, Generic)
data QER_LightImage = QER_LightImage !String                                 deriving (Show, Generic)
--see also: hell.shader, sky.shader, gfx.shader, liquid.shader

data SurfaceParam =
    AreaPortal
    | ClusterPortal
    | Detail
    | DoNotEnter
    | NoDLight
    | None
    | NoDraw
    | NoDrop
    | NoDamage
    | NoLightMap
    | NoImpact 
    | NoMarks
    | NoMipMaps
    | NonSolid
    | Origin
    | PlayerClip
    | LightFilter
    | MetalSteps
    | Slick
    | Structural
    | Trans
    | AlphaShadow
    | Lava
    | Water
    | Slime
    | Fog -- TBA
    | Sky -- TBA
    deriving (Show, Generic)

data TextureSizeParameter = 
    NONE
    | HALF
    | FULL
    deriving (Show, Generic)

data CloudParm = CloudParm {
    cloudEnvArg :: Maybe String, --ex: env/xnight2
    cloudCount  :: Maybe Int,
    cloudFill   :: Maybe TextureSizeParameter
} deriving (Show, Generic)

-- sky params ex:
{-
    surfaceparm noimpact
    surfaceparm nolightmap
    surfaceparm sky
    --qer_editorimage textures/skies/toxicbluesky.tga
    --q3map_surfacelight 500
    q3map_sun	1 1 0.5 150	30 60 -- associated param
    skyparms (env arg) 512 (CloudParam) --sky params actual 

    sky env/hell2
    cloudparms 512 full
    lightning
-}
data SkyParm = SkyParm {
    skyEnvArg :: Maybe String, --ex: env/xnight2
    skyCount  :: Maybe Int,
    skyFill   :: Maybe TextureSizeParameter
} deriving (Show, Generic)

-- fog params ex:
{-	
    fogonly (an associated option into it self)
    fogparms 0 0 0 400 256
-}
data FogGen = FogGen {
     fogWave :: WaveType,
     fogW    :: Float,
     fogX    :: Float, 
     fogY    :: Float, 
     fogZ    :: Float 
} deriving (Show, Generic)

data CommonAttrs
    = CommonAttrs
    { caNoPicMip        :: !Bool
    , caGlobalTexture   :: !Bool
    , caNoCarve         :: !Bool
    , caBackSided       :: !Bool
    , caFogGen          :: !FogGen
    , caQerLightImage   :: !QER_LightImage
    , caEditorImage     :: !StageTexture
    , caLightImage      :: !StageTexture
    , caSkyParms        :: !SkyParm    -- TODO
    , caLightSubdivide  :: !Float
    , caQerTrans        :: !QER_Trans  
    , caCloudParms      :: !CloudParm  -- TODO --
    , caCloudDef        :: !CloudDef   -- TODO
    , caFlare           :: !String
    , caLightning       :: !Bool       -- TODO
    , caFogParms        :: !(Either FogDef FogDefV3)    -- TODO
    , caFogParmsV3      :: !FogDefV3     -- TODO
    , caSun             :: !SunDef
    , caSky             :: !SkyDef
    , caBackSplash      :: !BackSplash
    , caSurfaceParm     :: !SurfaceParam
    , caLight           :: !Int
    , caSurfaceLight    :: !Int
    , caTessSize        :: !Int
    , caPortal          :: !Bool
    , caSort            :: !Float    -- default: 3 or 6 depends on blend function
    , caEntityMergable  :: !Bool
    , caFogOnly         :: !Bool
    , caCull            :: !CullType -- default: front
    , caDeformVertexes  :: ![Deform]
    , caNoMipMaps       :: !Bool
    , caPolygonOffset   :: !Bool
    , caStages          :: ![StageAttrs]
    -- parser internals (no strictness (!arg))
    , caIsSky           :: Bool
    }
    deriving (Show, Generic)

data RGBGen
    = RGB_Wave !Wave
    | RGB_Const !Float !Float !Float
    | RGB_Identity
    | RGB_IdentityLighting
    | RGB_Entity
    | RGB_OneMinusEntity
    | RGB_Detail -- !Bool
    | RGB_ExactVertex
    | RGB_Vertex
    | RGB_LightingDiffuse
    | RGB_OneMinusVertex
    | RGB_Undefined
    deriving (Show, Generic)

data AlphaGen
    = A_Wave !Wave
    | A_Const !Float
    | A_Portal
    | A_Identity
    | A_Entity
    | A_OneMinusEntity
    | A_Vertex
    | A_LightingSpecular
    | A_OneMinusVertex
    deriving (Show, Generic)

data TCGen
    = TG_Base
    | TG_Lightmap
    | TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    | TG_Vector !Vec3 !Vec3
    | TG_Undefined  -- FIXME: HACK!!
    deriving (Show, Generic)

data TCMod
    = TM_EntityTranslate
    | TM_Rotate    !Float
    | TM_Rotate2d  !Float !Float
    | TM_Scroll    !Float !Float
    | TM_Scale     !Float !Float
    | TM_ScaleX    !Float !Float !Float !Float
    | TM_Stretch   !Wave
    | TM_Transform !Float !Float !Float !Float !Float !Float
    | TM_Turb      !Float !Float !Float !Float
    | TM_TurbL     !WaveType !Float !Float !Float !Float
    deriving (Show, Generic)

data StageTexture
    = ST_None 
    | ST_Map        !String
    | ST_ClampMap   !String
    | ST_AnimMap    !Float ![String]
    | ST_Lightmap
    | ST_FromBlack
    | ST_WhiteImage
    deriving (Show, Eq, Ord, Generic)

data AlphaFunction
    = A_Gt0
    | A_Lt128
    | A_Ge128
    deriving (Show, Eq, Ord, Generic)

data DepthFunction
    = D_Equal
    | D_Lequal
    deriving (Show, Generic)

data Blending
    = B_DstAlpha
    | B_DstColor
    | B_One
    | B_OneMinusDstAlpha
    | B_OneMinusDstColor
    | B_OneMinusSrcAlpha
    | B_OneMinusSrcColor
    | B_SrcAlpha
    | B_SrcAlphaSaturate
    | B_SrcColor
    | B_Zero
    deriving (Show, Eq, Generic)

data StageAttrs
    = StageAttrs
    { saBlend             :: !(Maybe (Blending,Blending))
    , saRGBGen            :: !RGBGen
    , saAlphaGen          :: !AlphaGen
    , saTCGen             :: !TCGen
    , saTCMod             :: ![TCMod]
    , saTexture           :: !StageTexture
    , saDepthWrite        :: !Bool
    , saDepthFunc         :: !DepthFunction
    , saDetail            :: !Bool
    , saAlphaFunc         :: !(Maybe AlphaFunction)
    , saAlphaMap          :: StageTexture
    -- parser internals
    , saDepthMaskExplicit :: Bool
    , saTextureUniform    :: !String
    }
    deriving (Show, Generic)

deriving instance Generic   (V3 a)
instance Binary a => Binary (V3 a)

instance Binary RGBGen
instance Binary AlphaGen
instance Binary BackSplash
instance Binary TCGen
instance Binary TCMod
instance Binary WaveType
instance Binary StageTexture
instance Binary AlphaFunction
instance Binary Wave
instance Binary Deform
instance Binary QER_Trans
instance Binary DepthFunction
instance Binary CullType
instance Binary Blending
instance Binary StageAttrs
instance Binary FogDef
instance Binary FogDefV3

instance Binary CommonAttrs
instance Binary FogGen
instance Binary QER_LightImage
instance Binary SurfaceParam
instance Binary SkyParm
instance Binary CloudParm
instance Binary CloudDef
instance Binary SkyDef
instance Binary SunDef
instance Binary TextureSizeParameter

identityLight :: Float
identityLight = 1

defaultCommonAttrs :: CommonAttrs
defaultCommonAttrs =  CommonAttrs
    { caNoPicMip        = False
    , caNoCarve         = False
    , caBackSided       = False
    , caFogGen          = FogGen WT_None 0.1 0.1 0.1 0.1
    , caFlare           = ""
    , caGlobalTexture   = False
    , caQerLightImage   = QER_LightImage ""
    , caTessSize        = 0
    , caLightSubdivide  = 0
    , caFogParmsV3      = FogDefV3 (V3 0 0 0) 0
    , caQerTrans        = QER_Trans  0 
    , caBackSplash      = BackSplash 0 0
    , caEditorImage     = ST_WhiteImage
    , caLightImage      = ST_WhiteImage
    , caSkyParms        = SkyParm   Nothing Nothing Nothing 
    , caCloudParms      = CloudParm Nothing Nothing Nothing 
    , caFogParms        = Left $ FogDef   (Just 0) (Just 0) (Just 0) (Just 0) (Just 0)
    , caSun             = SunDef   0 0 0 0 0 0
    , caSky             = SkyDef ""
    , caCloudDef        = CloudDef 0 0 0     --CloudDef
    , caLightning       = False              --Bool
    , caSurfaceParm     = None
    , caSurfaceLight    = 0
    , caLight           = 0
    , caPortal          = False
    , caSort            = 0
    , caEntityMergable  = False
    , caFogOnly         = False
    , caCull            = CT_FrontSided
    , caDeformVertexes  = []
    , caNoMipMaps       = False
    , caPolygonOffset   = False
    , caStages          = []
    , caIsSky           = False
    }

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saBlend               = Nothing
    , saRGBGen              = RGB_Undefined
    , saAlphaGen            = A_Identity
    , saAlphaMap            = ST_None
    , saTCGen               = TG_Undefined
    , saTCMod               = []
    , saTexture             = ST_WhiteImage
    , saDepthWrite          = True
    , saDepthFunc           = D_Lequal
    , saDetail              = False
    , saAlphaFunc           = Nothing
    , saDepthMaskExplicit   = False
    , saTextureUniform      = mempty
    }
