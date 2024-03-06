--TODO: Look at this code side by side with mtlpp to accurate reflect types
module LambdaCube.DirectX (
    -- Schema
    module LambdaCube.PipelineSchema,
    -- IR
    V2(..),V3(..),V4(..),
    -- Array, Buffer, Texture
    DX11Buffer(..),          --Array(..),
    DX11BufferType(..),      --ArrayType(..),
    DX11VertexBuffer,        --Buffer,
    DX11VertexBufferSetter,  --BufferSetter,
    IndexStream(..),
    Stream(..), 
    StreamSetter, 
    FetchPrimitive(..), -- FetchMetalMeshPrimitive(..)
    InputType(..), 
    Primitive(..), 
    SetterFun,
    TextureData, 
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

    DX11UniformName,    -- GLUniformName,
    DX11Renderer,       -- GLRenderer,
    DX11Storage,        -- GLStorage,
    Mesh,               -- Object,
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

    --TODO: Wrangle this
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


import LambdaCube.IR
import LambdaCube.Linear
import LambdaCube.DirectX.Type
import LambdaCube.DirectX.Backend
import LambdaCube.DirectX.Data
import LambdaCube.DirectX.Input
import LambdaCube.PipelineSchema
import LambdaCube.PipelineSchemaUtil
