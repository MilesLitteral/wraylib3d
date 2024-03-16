{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module LambdaCube.Metal.Type where

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
import LambdaCube.Metal.Bindings
import LambdaCube.PipelineSchema

type MetalUniformName = ByteString

---------------
-- Input API --
---------------
{-
-- Buffer
    compileBuffer   :: [Array] -> IO Buffer
    bufferSize      :: Buffer  -> Int
    arraySize       :: Buffer  -> Int -> Int
    arrayType       :: Buffer  -> Int -> ArrayType

-- Object
    addObject           :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
    removeObject        :: Renderer -> Object -> IO ()
    objectUniformSetter :: Object -> Trie InputSetter
-}

data Buffer -- internal type
    = Buffer{ 
        bufArrays    :: Vector ArrayDesc
    ,   bufMetalObj  :: MTLBuffer
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

  MetalStorage can be attached to MetalRenderer
  MetalStorage represents a series of CommandBuffers ordered in a CommandQueue
-}

{-
  pipeline input:
    - independent from pipeline
    - per object features: enable/disable visibility, set render ordering
-}
data MetalUniform = forall a. Storable a => MetalUniform !InputType !(IORef a)

instance Show MetalUniform where
    show (GLUniform t _) = "MetalUniform " ++ show t

data OrderJob
    = Generate
    | Reorder
    | Ordered

data MetalSlot
    = MetalSlot
    { objectMap     :: IntMap Object
    , sortedObjects :: Vector (Int, Object)
    , orderJob      :: OrderJob
    }

data MetalStorage
    = MetalStorage
    { schema        :: PipelineSchema
    , slotMap       :: Map String SlotName
    , slotVector    :: Vector (IORef MetalSlot)
    , screenSize    :: IORef (Word, Word)
    , pipelines     :: IORef (Vector (Maybe MetalRenderer)) -- attached pipelines
    }

data Object -- internal type
    = Object
    { objSlot       :: SlotName
    , objPrimitive  :: Primitive
    , objIndices    :: Maybe (IndexStream Buffer)
    , objAttributes :: Map String (Stream Buffer)
    , objOrder      :: IORef Int
    , objEnabled    :: IORef Bool
    , objId         :: Int
    , objCommands   :: IORef (Vector (Vector [MetalObjectCommand]))  -- pipeline id, program name, commands
    }

--------------
-- Pipeline --
--------------

data MetalProgram
    = MetalProgram
    { shaderObjects         :: [MBuiltin] -- Vertex and Fragment Shaders
    , programObject         :: MBuiltin   -- The
    , inputUniforms         :: Map String MBuiltin
    , inputTextures         :: Map String MTLTexture   -- all input textures (MTLTexture)
    , inputMeshes           :: Map String MTLMesh 
    , inputStreams          :: Map String (Int, String)
    }

data MetalTexture
    = MetalTexture
    { mtlTextureObject   :: MTLTexture
    , mtlTextureTarget   :: Device
    } deriving Eq

data InputConnection
    = InputConnection
    { icId                      :: Int              -- identifier (vector index) for attached pipeline
    , icInput                   :: MetalStorage
    , icSlotMapPipelineToInput  :: Vector SlotName  -- GLRenderer to GLStorage slot name mapping
    , icSlotMapInputToPipeline  :: Vector (Maybe SlotName)  -- GLStorage to GLRenderer slot name mapping
    }

data MetalRenderPipeline --MetalStream
    = MetalStream
    { mtlStreamCommands    :: IORef [MetalObjectCommand]
    , mtlStreamPrimitive   :: Primitive
    , mtlStreamAttributes  :: Map String (Stream Buffer)
    , mtlStreamProgram     :: ProgramName
    }

data MetalRenderer --This is more or less an ICB pipeline
    = MetalRenderer
    { mtlLibraries      :: Vector MetalProgram
    , mtlTextures       :: Vector MetalTexture
    , mtlSamplers       :: Vector MetalSampler
    , mtlTargets         :: Vector MetalRenderTarget
    , mtlCommands        :: [MetalCommand]
    , mtlSlotPrograms    :: Vector [ProgramName] -- programs depend on a slot
    , mtlInput           :: IORef (Maybe InputConnection)
    , mtlSlotNames       :: Vector String
    , mtlCmdQ            :: MTLCommandQueue
    , mtlTexUnitMapping  :: Map String (IORef MTLUniform)   -- maps texture uniforms to texture units
    , mtlStreams         :: Vector MetalStream -- CommandQueue
    , mtlDrawContextRef  :: IORef MetalDrawContext
    , mtlForceSetup      :: IORef Bool
    , mtlVertexBufferRef    :: IORef MTLVertex
    , mtlFragmentBufferRef  :: IORef MTLFragment
    , mtlDrawCallCounterRef :: IORef Int
    }

data MetalSampler
    = MetalSampler
    { glSamplerObject :: UInt
    } deriving Eq

data MetalRenderTarget
    = MetalRenderTarget
    { framebufferObject         :: UInt
    , framebufferDrawbuffers    :: Maybe [GLenum]
    } deriving Eq

type MetalTextureUnit    = Int
type MetalUniformBinding = Int

data MetalSamplerUniform
  = GLSamplerUniform
  { glUniformBinding    :: !GLUniformBinding
  , glUniformBindingRef :: IORef GLUniformBinding
  }

instance Eq MetalSamplerUniform where
  a == b = glUniformBinding a == glUniformBinding b

data MetalDrawContext
  = MetalDrawContext
  { glRasterContext         :: !RasterContext
  , glAccumulationContext   :: !AccumulationContext
  , glRenderTarget          :: !MetalRenderTarget
  , glProgram               :: !MetalProgram
  , glTextureMapping        :: ![(MetalTextureUnit, MetalTexture)]
  , glSamplerMapping        :: ![(MetalTextureUnit, MetalSampler)]
  , glSamplerUniformMapping :: ![(MetalTextureUnit, MetalSamplerUniform)]
  }

data MetalCommand
  = MetalRenderSlot          !Device !SlotName !ProgramName
  | MetalRenderStream        !Device !StreamName !ProgramName
  | MetalClearRenderTarget   !MetalRenderTarget ![ClearImage]

instance Show (IORef MBuiltin) where
    show _ = "(IORef MBuiltin)"
CommandQueue
data MetalObjectCommand
    = MetalSetCommandQueue         !MTLBuffer !MTLCommandQueue
    | MetalBindTexture             !Device    !(IORef MTLTexture) !MetalUniform               -- binds the texture from the gluniform to the specified texture unit and target
    | MetalSetVertexArray          !MTLVertex !MTLFragment !MTLKernel !Device !(Ptr ())        -- index buffer size type pointer
    | MetalSetVertexAttrib         !MTLFragment  !(MBuiltin Buffer)                        -- index value
    | MetalDrawArrays              !Device !MTLCommandBuffer !MetalSizeN                         -- mode first count
    | MetalDrawElements            !Device !MetalSizeN !MTLCommandBuffer !(Ptr ())      -- mode count type buffer indicesPtr
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

-- describes an array in a buffer
data Array  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter

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
    { textureObject :: MBuiltin
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
    deriving (Eq, Ord, Bounded, Enum, Show)

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

sizeOfArrayType :: ArrayType -> Int
sizeOfArrayType at = case at of 
                        ArrWord8  -> 1
                        ArrWord16 -> 2
                        ArrWord32 -> 4
                        ArrInt8   -> 1
                        ArrInt16  -> 2
                        ArrInt32  -> 4
                        ArrFloat  -> 4
                        ArrHalf   -> 2


toStreamType :: InputType -> Maybe StreamType
toStreamType it = case it of 
                    Word     -> Just Attribute_Word
                    V2U      -> Just Attribute_V2U
                    V3U      -> Just Attribute_V3U
                    V4U      -> Just Attribute_V4U
                    Int      -> Just Attribute_Int
                    V2I      -> Just Attribute_V2I
                    V3I      -> Just Attribute_V3I
                    V4I      -> Just Attribute_V4I
                    Float    -> Just Attribute_Float
                    V2F      -> Just Attribute_V2F
                    V3F      -> Just Attribute_V3F
                    V4F      -> Just Attribute_V4F
                    M22F     -> Just Attribute_M22F
                    M23F     -> Just Attribute_M23F
                    M24F     -> Just Attribute_M24F
                    M32F     -> Just Attribute_M32F
                    M33F     -> Just Attribute_M33F
                    M34F     -> Just Attribute_M34F
                    M42F     -> Just Attribute_M42F
                    M43F     -> Just Attribute_M43F
                    M44F     -> Just Attribute_M44F
                    _        -> Nothing

fromStreamType :: StreamType -> InputType
fromStreamType st = case st of 
                        Attribute_Word    -> Word
                        Attribute_V2U     -> V2U
                        Attribute_V3U     -> V3U
                        Attribute_V4U     -> V4U
                        Attribute_Int     -> Int
                        Attribute_V2I     -> V2I
                        Attribute_V3I     -> V3I
                        Attribute_V4I     -> V4I
                        Attribute_Float   -> Float
                        Attribute_V2F     -> V2F
                        Attribute_V3F     -> V3F
                        Attribute_V4F     -> V4F
                        Attribute_M22F    -> M22F
                        Attribute_M23F    -> M23F
                        Attribute_M24F    -> M24F
                        Attribute_M32F    -> M32F
                        Attribute_M33F    -> M33F
                        Attribute_M34F    -> M34F
                        Attribute_M42F    -> M42F
                        Attribute_M43F    -> M43F
                        Attribute_M44F    -> M44F


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
