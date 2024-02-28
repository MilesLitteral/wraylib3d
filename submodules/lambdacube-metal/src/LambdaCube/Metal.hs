--TODO: Look at this code side by side with mtlpp to accurate reflect types
module LambdaCube.Metal (
    -- Schema
    module LambdaCube.PipelineSchema,
    -- IR
    V2(..),V3(..),V4(..),
    -- Array, Buffer, Texture
    MetalBuffer(..),          --Array(..),         -- MetalBuffer
    MetalBufferType(..),      --ArrayType(..),     -- MetalBufferType
    MetalCommandBuffer,       --Buffer,            -- MetalCommandBuffer -- MetalBuffer
    MetalCommandBufferSetter, --BufferSetter, -- MetalCommandBufferSetter -- Necessary?
    IndexStream(..),
    Stream(..),         -- MetalEngine
    StreamSetter,       -- MetalEngineSetter
    FetchPrimitive(..), -- FetchMetalMeshPrimitive(..)
    InputType(..),      -- MetalInputType(..)
    Primitive(..),      -- MetalMeshPrimitive(..)
    SetterFun,
    TextureData,        -- MetalTextureData
    InputSetter(..),
    fromStreamType,
    sizeOfArrayType,
    toStreamType,
    compileBuffer,
    disposeBuffer,
    updateBuffer,
    bufferSize,
    arraySize,
    arrayType,
    uploadTexture2DToGPU,
    uploadTexture2DToGPU',
    disposeTexture,

    -- Metal: CommandQueue, CommandEncoder, Mesh
    MetalUniformName,    --GLUniformName,
    MetalCommandQueue,   --GLRenderer, --MetalCommandQueue
    MetalCommandEncoder, --GLStorage,  --MetalCommandEncoder
    MetalMesh,           --Object,     --MetalMesh
    schema,
    schemaFromPipeline,
    allocRenderer,
    disposeRenderer,
    setStorage,
    renderFrame,
    allocStorage,
    disposeStorage,
    uniformSetter,
    addObject,
    removeObject,
    enableObject,
    setObjectOrder,
    objectUniformSetter,
    setScreenSize,
    sortSlotObjects,

    uniformBool,
    uniformV2B,
    uniformV3B,
    uniformV4B,

    uniformWord,
    uniformV2U,
    uniformV3U,
    uniformV4U,

    uniformInt,
    uniformV2I,
    uniformV3I,
    uniformV4I,

    uniformFloat,
    uniformV2F,
    uniformV3F,
    uniformV4F,

    uniformM22F,
    uniformM23F,
    uniformM24F,
    uniformM32F,
    uniformM33F,
    uniformM34F,
    uniformM42F,
    uniformM43F,
    uniformM44F,

    uniformFTexture2D,

    -- schema builder utility functions
    (@:),
    defObjectArray,
    defUniforms,
    makeSchema,

    (@=),
    updateUniforms,
    updateObjectUniforms
) where

{-
"kernel", "device", "texture2d", "texture", "uint2",       "access::read",   "access::write",
     "half",   "half4",  "bool",      "char",    "uchar",       "short",          "ushort",       
     "long",   "ulong",  "float",     "half",    "double",      "size_t",         "ptrdiff_t",    
     "int",    "uint",   "void", "[[thread_position_in_grid]]", "intptr_t",       "uintpyt_t" 
-}

import LambdaCube.IR
import LambdaCube.Linear
import LambdaCube.Metal.Type
import LambdaCube.Metal.Backend
import LambdaCube.Metal.Data
import LambdaCube.Metal.Input
import LambdaCube.PipelineSchema
import LambdaCube.PipelineSchemaUtil
