{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module LambdaCube.DirectX.Type where

import Data.IORef
import Data.Int
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Data.ByteString

import LambdaCube.IR
import LambdaCube.Linear
import LambdaCube.DirectX.Bindings
import LambdaCube.PipelineSchema

type DXUniformName = ByteString

---------------
-- Input API --
---------------
{-
-- Buffer
    compileBuffer   :: [Array] -> IO Buffer
    bufferSize      :: Buffer -> Int
    arraySize       :: Buffer -> Int -> Int
    arrayType       :: Buffer -> Int -> ArrayType

-- Object
    addObject           :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
    removeObject        :: Renderer -> Object -> IO ()
    objectUniformSetter :: Object -> Trie InputSetter
-}

data Buffer -- internal type
    = Buffer
    { bufArrays :: Vector ArrayDesc
    , bufGLObj  :: GLuint
    }
    deriving (Show,Eq)

data ArrayDesc
    = ArrayDesc
    { arrType   :: ArrayType
    , arrLength :: Int  -- item count
    , arrOffset :: Int  -- byte position in buffer
    , arrSize   :: Int  -- size in bytes
    }
    deriving (Show,Eq)

{-
  handles:
    uniforms
    textures
    buffers
    objects

  DX11Storage can be attached to DX11Renderer
-}

{-
  pipeline input:
    - independent from pipeline
    - per object features: enable/disable visibility, set render ordering
-}
data DX11Uniform = forall a. Storable a => DX11Uniform !InputType !(IORef a)

instance Show DX11Uniform where
    show (DX11Uniform t _) = "DX11Uniform " ++ show t

data OrderJob
    = Generate
    | Reorder
    | Ordered

data DX11Slot
    = DX11Slot
    { objectMap     :: IntMap Object
    , sortedObjects :: Vector (Int, Object)
    , orderJob      :: OrderJob
    }

data DX11Storage
    = DX11Storage
    { schema        :: PipelineSchema
    , slotMap       :: Map String SlotName
    , slotVector    :: Vector (IORef DX11Slot)
    , objSeed       :: IORef Int
    , uniformSetter :: Map DX11UniformName InputSetter
    , uniformSetup  :: Map String DX11Uniform
    , screenSize    :: IORef (Word,Word)
    , pipelines     :: IORef (Vector (Maybe DX11Renderer)) -- attached pipelines
    }

data Object -- internal type
    = Object
    { objSlot       :: SlotName
    , objPrimitive  :: Primitive
    , objIndices    :: Maybe (IndexStream Buffer)
    , objAttributes :: Map String (Stream Buffer)
    , objUniSetter  :: Map GLUniformName InputSetter
    , objUniSetup   :: Map String DX11Uniform
    , objOrder      :: IORef Int
    , objEnabled    :: IORef Bool
    , objId         :: Int
    , objCommands   :: IORef (Vector (Vector [DX11ObjectCommand]))  -- pipeline id, program name, commands
    }

--------------
-- Pipeline --
--------------

data DX11Program
    = DX11Program
    { shaderObjects         :: [GLuint]
    , programObject         :: GLuint
    , inputUniforms         :: Map String GLint
    , inputTextures         :: Map String GLint   -- all input textures (render texture + uniform texture)
    , inputTextureUniforms  :: Set String
    , inputStreams          :: Map String (GLuint,String)
    }

data DX11Texture
    = DX11Texture
    { dxTextureObject   :: GLuint
    , dxTextureTarget   :: GLenum
    } deriving Eq

data InputConnection
    = InputConnection
    { icId                      :: Int              -- identifier (vector index) for attached pipeline
    , icInput                   :: DX11Storage
    , icSlotMapPipelineToInput  :: Vector SlotName  -- GLRenderer to GLStorage slot name mapping
    , icSlotMapInputToPipeline  :: Vector (Maybe SlotName)  -- GLStorage to GLRenderer slot name mapping
    }

data DX11Stream
    = DX11Stream
    { dxStreamCommands    :: IORef [DX11ObjectCommand]
    , dxStreamPrimitive   :: Primitive
    , dxStreamAttributes  :: Map String (Stream Buffer)
    , dxStreamProgram     :: ProgramName
    }

data DX11Renderer
    = DX11Renderer
    { dxPrograms        :: Vector DX11Program
    , dxTextures        :: Vector DX11Texture --ID3D11Texture2D
    , dxSamplers        :: Vector DX11Sampler
    , dxTargets         :: Vector GLRenderTarget
    , dxCommands        :: [DX11Command]
    , dxSlotPrograms    :: Vector [ProgramName] -- programs depend on a slot
    , dxInput           :: IORef (Maybe InputConnection)
    , dxSlotNames       :: Vector String
    , dxVAO             :: GLuint
    , dxTexUnitMapping  :: Map String (IORef GLint)   -- maps texture uniforms to texture units
    , dxStreams         :: Vector DX11Stream
    , dxDrawContextRef  :: IORef DX11DrawContext
    , dxForceSetup      :: IORef Bool
    , dxVertexBufferRef :: IORef GLuint
    , dxIndexBufferRef  :: IORef GLuint
    , dxDrawCallCounterRef :: IORef Int
    }

data DX11Sampler
    = DX11Sampler
    { dxSamplerObject :: GLuint
    } deriving Eq

data DX11RenderTarget
    = DX11RenderTarget
    { framebufferObject         :: GLuint
    , framebufferDrawbuffers    :: Maybe [GLenum]
    } deriving Eq

type DX11TextureUnit    = Int
type DX11UniformBinding = GLint

data DX11SamplerUniform
  = DX11SamplerUniform
  { dxUniformBinding    :: !DX11UniformBinding
  , dxUniformBindingRef :: IORef DX11UniformBinding
  }

instance Eq GLSamplerUniform where
  a == b = glUniformBinding a == glUniformBinding b

data DX11DrawContext
  = DX11DrawContext
  { dxRasterContext         :: !RasterContext
  , dxAccumulationContext   :: !AccumulationContext
  , dxRenderTarget          :: !GLRenderTarget
  , dxProgram               :: !GLuint
  , dxTextureMapping        :: ![(GLTextureUnit,GLTexture)]
  , dxSamplerMapping        :: ![(GLTextureUnit,GLSampler)]
  , dxSamplerUniformMapping :: ![(GLTextureUnit,GLSamplerUniform)]
  }

data DX11Command
  = DX11RenderSlot          !GLDrawContext !SlotName   !ProgramName
  | DX11RenderStream        !GLDrawContext !StreamName !ProgramName
  | DX11ClearRenderTarget   !GLRenderTarget ![ClearImage]

instance Show (IORef GLint) where
    show _ = "(IORef GLint)"

data DX11ObjectCommand
    = DX11SetUniform              !GLint !GLUniform
    | DX11BindTexture             !GLenum !(IORef GLint) !GLUniform               -- binds the texture from the gluniform to the specified texture unit and target
    | DX11SetVertexAttribArray    !GLuint !GLuint !GLint !GLenum !(Ptr ())        -- index buffer size type pointer
    | DX11SetVertexAttribIArray   !GLuint !GLuint !GLint !GLenum !(Ptr ())        -- index buffer size type pointer
    | DX11SetVertexAttrib         !GLuint !(Stream Buffer)                        -- index value
    | DX11DrawArrays              !GLenum !GLint !GLsizei                         -- mode first count
    | DX11DrawElements            !GLenum !GLsizei !GLenum !GLuint !(Ptr ())      -- mode count type buffer indicesPtr
    deriving Show

type SetterFun a = a -> IO ()

-- user will provide scalar input data via this type
data InputSetter
    = SBool  (SetterFun Bool)
    | SV2B   (SetterFun V2B)
    | SV3B   (SetterFun V3B)
    | SV4B   (SetterFun V4B)
    | SWord  (SetterFun Word32)
    | SV2U   (SetterFun V2U)
    | SV3U   (SetterFun V3U)
    | SV4U   (SetterFun V4U)
    | SInt   (SetterFun Int32)
    | SV2I   (SetterFun V2I)
    | SV3I   (SetterFun V3I)
    | SV4I   (SetterFun V4I)
    | SFloat (SetterFun Float)
    | SV2F   (SetterFun V2F)
    | SV3F   (SetterFun V3F)
    | SV4F   (SetterFun V4F)
    | SM22F  (SetterFun M22F)
    | SM23F  (SetterFun M23F)
    | SM24F  (SetterFun M24F)
    | SM32F  (SetterFun M32F)
    | SM33F  (SetterFun M33F)
    | SM34F  (SetterFun M34F)
    | SM42F  (SetterFun M42F)
    | SM43F  (SetterFun M43F)
    | SM44F  (SetterFun M44F)
    -- shadow textures
    | SSTexture1D
    | SSTexture2D
    | SSTextureCube
    | SSTexture1DArray
    | SSTexture2DArray
    | SSTexture2DRect
    -- float textures
    | SFTexture1D
    | SFTexture2D           (SetterFun TextureData)
    | SFTexture3D
    | SFTextureCube
    | SFTexture1DArray
    | SFTexture2DArray
    | SFTexture2DMS
    | SFTexture2DMSArray
    | SFTextureBuffer
    | SFTexture2DRect
    -- int textures
    | SITexture1D
    | SITexture2D
    | SITexture3D
    | SITextureCube
    | SITexture1DArray
    | SITexture2DArray
    | SITexture2DMS
    | SITexture2DMSArray
    | SITextureBuffer
    | SITexture2DRect
    -- uint textures
    | SUTexture1D
    | SUTexture2D
    | SUTexture3D
    | SUTextureCube
    | SUTexture1DArray
    | SUTexture2DArray
    | SUTexture2DMS
    | SUTexture2DMSArray
    | SUTextureBuffer
    | SUTexture2DRect

-- buffer handling
{-
    user can fills a buffer (continuous memory region)
    each buffer have a data descriptor, what describes the
    buffer content. e.g. a buffer can contain more arrays of stream types
-}

-- user will provide stream data using this setup function
type BufferSetter = (Ptr () -> IO ()) -> IO ()

-- specifies array component type (stream type in storage side)
--  this type can be overridden in GPU side, e.g ArrWord8 can be seen as TFloat or TWord also
data ArrayType
    = ArrWord8
    | ArrWord16
    | ArrWord32
    | ArrInt8
    | ArrInt16
    | ArrInt32
    | ArrFloat
    | ArrHalf     -- Hint: half float is not supported in haskell
    deriving (Show,Eq,Ord)

sizeOfArrayType :: ArrayType -> Int
sizeOfArrayType ArrWord8  = 1
sizeOfArrayType ArrWord16 = 2
sizeOfArrayType ArrWord32 = 4
sizeOfArrayType ArrInt8   = 1
sizeOfArrayType ArrInt16  = 2
sizeOfArrayType ArrInt32  = 4
sizeOfArrayType ArrFloat  = 4
sizeOfArrayType ArrHalf   = 2

-- describes an array in a buffer
data Array  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter

toStreamType :: InputType -> Maybe StreamType
toStreamType Word     = Just Attribute_Word
toStreamType V2U      = Just Attribute_V2U
toStreamType V3U      = Just Attribute_V3U
toStreamType V4U      = Just Attribute_V4U
toStreamType Int      = Just Attribute_Int
toStreamType V2I      = Just Attribute_V2I
toStreamType V3I      = Just Attribute_V3I
toStreamType V4I      = Just Attribute_V4I
toStreamType Float    = Just Attribute_Float
toStreamType V2F      = Just Attribute_V2F
toStreamType V3F      = Just Attribute_V3F
toStreamType V4F      = Just Attribute_V4F
toStreamType M22F     = Just Attribute_M22F
toStreamType M23F     = Just Attribute_M23F
toStreamType M24F     = Just Attribute_M24F
toStreamType M32F     = Just Attribute_M32F
toStreamType M33F     = Just Attribute_M33F
toStreamType M34F     = Just Attribute_M34F
toStreamType M42F     = Just Attribute_M42F
toStreamType M43F     = Just Attribute_M43F
toStreamType M44F     = Just Attribute_M44F
toStreamType _          = Nothing

fromStreamType :: StreamType -> InputType
fromStreamType Attribute_Word    = Word
fromStreamType Attribute_V2U     = V2U
fromStreamType Attribute_V3U     = V3U
fromStreamType Attribute_V4U     = V4U
fromStreamType Attribute_Int     = Int
fromStreamType Attribute_V2I     = V2I
fromStreamType Attribute_V3I     = V3I
fromStreamType Attribute_V4I     = V4I
fromStreamType Attribute_Float   = Float
fromStreamType Attribute_V2F     = V2F
fromStreamType Attribute_V3F     = V3F
fromStreamType Attribute_V4F     = V4F
fromStreamType Attribute_M22F    = M22F
fromStreamType Attribute_M23F    = M23F
fromStreamType Attribute_M24F    = M24F
fromStreamType Attribute_M32F    = M32F
fromStreamType Attribute_M33F    = M33F
fromStreamType Attribute_M34F    = M34F
fromStreamType Attribute_M42F    = M42F
fromStreamType Attribute_M43F    = M43F
fromStreamType Attribute_M44F    = M44F

-- user can specify streams using Stream type
-- a stream can be constant (ConstXXX) or can came from a buffer
data Stream b
    = ConstWord  Word32
    | ConstV2U   V2U
    | ConstV3U   V3U
    | ConstV4U   V4U
    | ConstInt   Int32
    | ConstV2I   V2I
    | ConstV3I   V3I
    | ConstV4I   V4I
    | ConstFloat Float
    | ConstV2F   V2F
    | ConstV3F   V3F
    | ConstV4F   V4F
    | ConstM22F  M22F
    | ConstM23F  M23F
    | ConstM24F  M24F
    | ConstM32F  M32F
    | ConstM33F  M33F
    | ConstM34F  M34F
    | ConstM42F  M42F
    | ConstM43F  M43F
    | ConstM44F  M44F
    | Stream 
        { streamType    :: StreamType
        , streamBuffer  :: b
        , streamArrIdx  :: Int
        , streamStart   :: Int
        , streamLength  :: Int
        }
    deriving Show

streamToStreamType :: Stream a -> StreamType
streamToStreamType s = case s of
    ConstWord  _ -> Attribute_Word
    ConstV2U   _ -> Attribute_V2U
    ConstV3U   _ -> Attribute_V3U
    ConstV4U   _ -> Attribute_V4U
    ConstInt   _ -> Attribute_Int
    ConstV2I   _ -> Attribute_V2I
    ConstV3I   _ -> Attribute_V3I
    ConstV4I   _ -> Attribute_V4I
    ConstFloat _ -> Attribute_Float
    ConstV2F   _ -> Attribute_V2F
    ConstV3F   _ -> Attribute_V3F
    ConstV4F   _ -> Attribute_V4F
    ConstM22F  _ -> Attribute_M22F
    ConstM23F  _ -> Attribute_M23F
    ConstM24F  _ -> Attribute_M24F
    ConstM32F  _ -> Attribute_M32F
    ConstM33F  _ -> Attribute_M33F
    ConstM34F  _ -> Attribute_M34F
    ConstM42F  _ -> Attribute_M42F
    ConstM43F  _ -> Attribute_M43F
    ConstM44F  _ -> Attribute_M44F
    Stream t _ _ _ _ -> t

-- stream of index values (for index buffer)
data IndexStream b
    = IndexStream
    { indexBuffer   :: b
    , indexArrIdx   :: Int
    , indexStart    :: Int
    , indexLength   :: Int
    }

newtype TextureData
    = TextureData
    { textureObject :: GLuint
    }
    deriving Storable

data Primitive
    = TriangleStrip
    | TriangleList
    | TriangleFan
    | LineStrip
    | LineList
    | PointList
    | TriangleStripAdjacency
    | TriangleListAdjacency
    | LineStripAdjacency
    | LineListAdjacency
    deriving (Eq,Ord,Bounded,Enum,Show)

type StreamSetter = Stream Buffer -> IO ()

-- storable instances
instance Storable a => Storable (V2 a) where
    sizeOf    _ = 2 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        return $! (V2 x y)

    poke q (V2 x y) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y

instance Storable a => Storable (V3 a) where
    sizeOf    _ = 3 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        z <- peekByteOff p (k*2)
        return $! (V3 x y z)

    poke q (V3 x y z) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y
        pokeByteOff p (k*2) z

instance Storable a => Storable (V4 a) where
    sizeOf    _ = 4 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        z <- peekByteOff p (k*2)
        w <- peekByteOff p (k*3)
        return $! (V4 x y z w)

    poke q (V4 x y z w) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y
        pokeByteOff p (k*2) z
        pokeByteOff p (k*3) w
