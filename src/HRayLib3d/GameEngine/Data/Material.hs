{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module HRayLib3d.GameEngine.Data.Material where

import Data.Binary
import GHC.Generics (Generic)

import LambdaCube.Linear

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
    = WT_Sin
    | WT_Triangle
    | WT_Square
    | WT_Sawtooth
    | WT_InverseSawtooth
    | WT_Noise
    deriving (Show,Generic)

data Wave = Wave !WaveType !Float !Float !Float !Float deriving (Show,Generic)

data Deform
    = D_AutoSprite
    | D_AutoSprite2
    | D_Bulge !Float !Float !Float
    | D_Move !Vec3 !Wave
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
    deriving (Show,Generic)

data CullType
    = CT_FrontSided
    | CT_BackSided
    | CT_TwoSided
    deriving (Show,Generic)

data CommonAttrs
    = CommonAttrs
    { caSkyParms        :: !() -- TODO
    , caFogParms        :: !() -- TODO
    , caPortal          :: !Bool
    , caSort            :: !Float -- default: 3 or 6 depends on blend function
    , caEntityMergable  :: !Bool
    , caFogOnly         :: !Bool
    , caCull            :: !CullType -- default: front
    , caDeformVertexes  :: ![Deform]
    , caNoMipMaps       :: !Bool
    , caPolygonOffset   :: !Bool
    , caStages          :: ![StageAttrs]

    -- parser internals
    , caIsSky           :: Bool
    }
    deriving (Show,Generic)


data RGBGen
    = RGB_Wave !Wave
    | RGB_Const !Float !Float !Float
    | RGB_Identity
    | RGB_IdentityLighting
    | RGB_Entity
    | RGB_OneMinusEntity
    | RGB_ExactVertex
    | RGB_Vertex
    | RGB_LightingDiffuse
    | RGB_OneMinusVertex
    | RGB_Undefined
    deriving (Show,Generic)

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
    deriving (Show,Generic)

data TCGen
    = TG_Base
    | TG_Lightmap
    | TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    | TG_Vector !Vec3 !Vec3
    | TG_Undefined  -- FIXME: HACK!!
    deriving (Show,Generic)

data TCMod
    = TM_EntityTranslate
    | TM_Rotate !Float
    | TM_Scroll !Float !Float
    | TM_Scale !Float !Float
    | TM_Stretch !Wave
    | TM_Transform !Float !Float !Float !Float !Float !Float
    | TM_Turb !Float !Float !Float !Float
    deriving (Show,Generic)

data StageTexture
    = ST_Map        !String
    | ST_ClampMap   !String
    | ST_AnimMap    !Float ![String]
    | ST_Lightmap
    | ST_WhiteImage
    deriving (Show,Eq,Ord,Generic)

data AlphaFunction
    = A_Gt0
    | A_Lt128
    | A_Ge128
    deriving (Show,Eq,Ord,Generic)

data DepthFunction
    = D_Equal
    | D_Lequal
    deriving (Show,Generic)

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
    deriving (Show,Eq,Generic)

data StageAttrs
    = StageAttrs
    { saBlend       :: !(Maybe (Blending,Blending))
    , saRGBGen      :: !RGBGen
    , saAlphaGen    :: !AlphaGen
    , saTCGen       :: !TCGen
    , saTCMod       :: ![TCMod]
    , saTexture     :: !StageTexture
    , saDepthWrite  :: !Bool
    , saDepthFunc   :: !DepthFunction
    , saAlphaFunc   :: !(Maybe AlphaFunction)

    -- parser internals
    , saDepthMaskExplicit :: Bool
    , saTextureUniform    :: !String
    }
    deriving (Show,Generic)

deriving instance Generic (V3 a)
instance Binary RGBGen
instance Binary AlphaGen
instance Binary TCGen
instance Binary TCMod
instance Binary WaveType
instance Binary StageTexture
instance Binary AlphaFunction
instance Binary Wave
instance Binary a => Binary (V3 a)
instance Binary Deform
instance Binary DepthFunction
instance Binary CullType
instance Binary Blending
instance Binary StageAttrs
instance Binary CommonAttrs

identityLight :: Float
identityLight = 1

defaultCommonAttrs :: CommonAttrs
defaultCommonAttrs = CommonAttrs
    { caSkyParms        = ()
    , caFogParms        = ()
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
    { saBlend       = Nothing
    , saRGBGen      = RGB_Undefined
    , saAlphaGen    = A_Identity
    , saTCGen       = TG_Undefined
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = True
    , saDepthFunc   = D_Lequal
    , saAlphaFunc   = Nothing
    , saDepthMaskExplicit   = False
    , saTextureUniform  = mempty
    }
