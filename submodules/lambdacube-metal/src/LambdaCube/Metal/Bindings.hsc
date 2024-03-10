{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, CPP #-}

#include "mtlpp/mtlpp.hpp"
module LambdaCube.Metal.Bindings where

import Foreign
import Foreign.C

-- The Foreign Interface
-- Every 'result' from Metal will be a Pointer 
type MValue  = Ptr CULong

type MetalSizeN = CInt

metal_stdlib :: String
metal_stdlib = "<metal_stdlib>"

-- | The Metal Built-In Types
data MBuiltin = None                           -- The argument doesn't have a valid data type.
            |Structure                      -- A (Data) structure.
            |Array                          -- An array.
            |Pointer                         -- A pointer.
            |Kernel
            |Device
            |Void
            |ThreadPositionInGrid
            |Namespace     
            |Using
            -- Boolean Data Types
            |Bool1                           -- A Boolean value.
            |Bool2                          -- A two-component vector with Boolean values.
            |Bool3                          -- A three-component vector with Boolean values.
            |Bool4                          -- A four-component vector with Boolean values.
            -- Integer Data Types
            |Char1                          -- A signed 8-bit integer value.
            |Char2                          -- A two-component vector with signed 8-bit integer values.
            |Char3                          -- A three-component vector with signed 8-bit integer values.
            |Char4                          -- A four-component vector with signed 8-bit integer values.
            |UChar                          -- An unsigned 8-bit integer value.
            |UChar2                         -- A two-component vector with unsigned 8-bit integer values.
            |UChar3                         -- A three-component vector with unsigned 8-bit integer values.
            |UChar4                         -- A three-component vector with unsigned 8-bit integer values.
            |Short1                         -- A signed 16-bit integer value.
            |Short2                         -- A two-component vector with signed 16-bit integer values.
            |Short3                         -- A three-component vector with signed 16-bit integer values.
            |Short4                         -- A four-component vector with signed 16-bit integer values.
            |UShort                         -- An unsigned 16-bit integer value.
            |UShort2                        -- A two-component vector with unsigned 16-bit integer values.
            |UShort3                        -- A three-component vector with unsigned 16-bit integer values.
            |UShort4                        -- A four-component vector with unsigned 16-bit integer values.
            |Int1                           -- A signed 32-bit integer value.
            |Int2                           -- A two-component vector with signed 32-bit integer values.
            |Int3                           -- A three-component vector with signed 32-bit integer values.
            |Int4                           -- A four-component vector with signed 32-bit integer values.
            |UInt                           -- An unsigned 32-bit integer value.
            |UInt2                          -- A two-component vector with unsigned 32-bit integer values.
            |UInt3                          -- A three-component vector with unsigned 32-bit integer values.
            |UInt4                          -- A four-component vector with unsigned 32-bit integer values.
            |Long1                          -- A signed 64-bit integer value.
            |Long2                          -- A two-component vector with signed 64-bit integer values.
            |Long3                          -- A three-component vector with signed 64-bit integer values.
            |Long4                          -- A four-component vector with signed 64-bit integer values.
            |ULong                          -- An unsigned 64-bit integer value.
            |ULong2                         -- A two-component vector with unsigned 64-bit integer values.
            |ULong3                         -- A three-component vector with unsigned 64-bit integer values.
            |ULong4                         -- A four-component vector with unsigned 64-bit integer values.
            -- Floating-Point Data Types
            |Float1                         -- A 32-bit floating-point value.
            |Float2                         -- A two-component vector with 32-bit floating-point values.
            |Float3                         -- A three-component vector with 32-bit floating-point values.
            |Float4                         -- A four-component vector with 32-bit floating-point values.
            |Float2x2                       -- A 2x2 component matrix with 32-bit floating-point values.
            |Float2x3                       -- A 2x3 component matrix with 32-bit floating-point values.
            |Float2x4                       -- A 2x4 component matrix with 32-bit floating-point values.
            |Float3x2                       -- A 3x2 component matrix with 32-bit floating-point values.
            |Float3x3                       -- A 3x3 component matrix with 32-bit floating-point values.
            |Float3x4                       -- A 3x4 component matrix with 32-bit floating-point values.
            |Float4x2                       -- A 4x2 component matrix with 32-bit floating-point values.
            |Float4x3                       -- A 4x3 component matrix with 32-bit floating-point values.
            |Float4x4                       -- A 4x4 component matrix with 32-bit floating-point values.
            |Half                           -- A 16-bit floating-point value.
            |Half2                          -- A two-component vector with 16-bit floating-point values.
            |Half3                          -- A three-component vector with 16-bit floating-point values.
            |Half4                          -- A four-component vector with 16-bit floating-point values.
            |Half2x2                        -- A 2x2 component matrix with 16-bit floating-point values.
            |Half2x3                        -- A 2x3 component matrix with 16-bit floating-point values.
            |Half2x4                        -- A 2x4 component matrix with 16-bit floating-point values.
            |Half3x2                        -- A 3x2 component matrix with 16-bit floating-point values.
            |Half3x3                        -- A 3x3 component matrix with 16-bit floating-point values.
            |Half3x4                        -- A 3x4 component matrix with 16-bit floating-point values.
            |Half4x2                        -- A 4x2 component matrix with 16-bit floating-point values.
            |Half4x3                        -- A 4x3 component matrix with 16-bit floating-point values.
            |Half4x4                        -- A 4x4 component matrix with 16-bit floating-point values.
            -- Pixel Format Data Types
            |R8Snorm                        -- An ordinary format with one 8-bit, normalized, signed integer component.
            |R8Unorm                        -- An ordinary format with one 8-bit, normalized, unsigned integer component.
            |RG8Snorm                       -- An ordinary format with two 8-bit, normalized, signed integer components.
            |RG8Unorm                       -- An ordinary format with two 8-bit, normalized, unsigned integer components.
            |RGBA8Snorm                     -- An ordinary format with four 8-bit, normalized, signed integer components in RGBA order.
            |RGBA8Unorm                     -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order.
            |RGBA8Unorm_srgb                -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order with conversion between sRGB and linear space.
            |R16Snorm                       -- An ordinary format with one 16-bit, normalized, signed integer component.
            |R16Unorm                       -- An ordinary format with one 16-bit, normalized, unsigned integer component.
            |RG16Snorm                      -- An ordinary format with two 16-bit, normalized, signed integer components.
            |RG16Unorm                      -- An ordinary format with two 16-bit, normalized, unsigned integer components.
            |RGBA16Snorm                    -- An ordinary format with four 16-bit, normalized, signed integer components in RGBA order.
            |RGBA16Unorm                    -- An ordinary format with four 16-bit, normalized, unsigned integer components in RGBA order.
            |RGB9e5Float                    -- A packed 32-bit format with three 9-bit, floating-point color components.
            |RGB10a2Unorm                   -- A packed 32-bit format with normalized, unsigned integer color components.
            |RG11b10Float                   -- A packed 32-bit format with two 11-bit (for red and green) and one 10-bit (for blue) floating-point color components.
            -- Metal Object Data Types
            |MTLTexture                        -- A texture.
            |MTLSampler                        -- A texture sampler.
            |IndirectCommandBuffer          -- An indirect command buffer.
            |CommandBuffer
            |CommandQueue
            |Buffer 
            |BinaryArchive
            |RenderPipeline                 -- A render pipeline.
            |ComputePipeline                -- A compute pipeline.
            |VisibleFunctionTable,           -- A table of visible functions that the pipeline can call.
            |IntersectionFunctionTable      -- A table of intersection functions that the pipeline can call.
            |PrimitiveAccelerationStructure -- A low-level ray-tracing acceleration structure for a set of primitives.
            |InstanceAccelerationStructure  -- A high-level ray-tracing acceleration structure for a set of instances of low-level primitives.
            |MFalse
            |MTrue
            deriving (Show)

foreign import ccall        "createComputeInstance"            createComputeInstance       :: IO ()
foreign import ccall        "createRenderingInstanceGPU"       createRenderingInstanceGPU  :: IO ()
foreign import ccall        "createRenderingInstanceCPU"       createRenderingInstanceCPU  :: IO ()
foreign import ccall unsafe "&metal_kernel"                    metal_kernel                :: Ptr MValue
foreign import ccall   safe "metal_krncall"                    metal_krncall              :: MValue -> MValue -> MValue -> IO MValue
{-
foreign import ccall   safe "rb_funcall"                c_rb_funcall_4                :: FRValue -> RID -> Int -> FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
foreign import ccall unsafe "&rb_cObject"               rb_cObject                    :: Ptr FRValue-}

mtype :: MValue -> IO MBuiltin
mtype v = metalType v >>= \x -> case x of
    (#const none)           -> return MNone
    (#const kernel)  -> return Kernel)
    (#const bool1)          -> return Bool1
    (#const bool2)          -> return (MBuiltin Bool2)
    (#const bool3)          -> return (MBuiltin Bool3)
    (#const bool4)          -> return (MBuiltin Bool4)
    (#const char1)          -> return (MBuiltin Char1)
    (#const char2)          -> return (MBuiltin Char2)
    (#const char3)          -> return (MBuiltin Char3)
    (#const char4)          -> return (MBuiltin Char4)
    (#const uchar)          -> return (MBuiltin UChar)
    (#const uchar2)         -> return (MBuiltin UChar2)
    (#const uchar3)         -> return (MBuiltin UChar3)
    (#const uchar4)         -> return (MBuiltin UChar4)
    (#const short1)         -> return (MBuiltin Short1)
    (#const short2)         -> return (MBuiltin Short2)
    (#const short3)         -> return (MBuiltin Short3)
    (#const short4)         -> return (MBuiltin Short4)
    (#const ushort)         -> return (MBuiltin UShort)
    (#const ushort2)        -> return (MBuiltin UShort2)
    (#const ushort3)        -> return (MBuiltin UShort3)
    (#const ushort4)        -> return (MBuiltin UShort4)
    (#const int1)           -> return (MBuiltin Int1)
    (#const int2)           -> return (MBuiltin Int2)
    (#const int3)           -> return (MBuiltin Int3)
    (#const int4)           -> return (MBuiltin Int4)
    (#const uint)           -> return (MBuiltin UInt)
    (#const uint2)          -> return (MBuiltin UInt2)
    (#const uint3)          -> return (MBuiltin UInt3)
    (#const uint4)          -> return (MBuiltin UInt4)
    (#const long1)          -> return (MBuiltin Long1)
    (#const long2)          -> return (MBuiltin Long2)
    (#const long3)          -> return (MBuiltin Long3)
    (#const long4)          -> return (MBuiltin Long4)
    (#const ulong)          -> return (MBuiltin ULong)
    (#const ulong2)         -> return (MBuiltin ULong2)
    (#const ulong3)         -> return (MBuiltin ULong3)
    (#const ulong4)         -> return (MBuiltin ULong4)
    (#const float1)         -> return (MBuiltin Float1)
    (#const float2)         -> return (MBuiltin Float2)
    (#const float3)         -> return (MBuiltin Float3)
    (#const float4)         -> return (MBuiltin Float4)
    (#const float2x2)       -> return (MBuiltin Float2x2)
    (#const float2x3)       -> return (MBuiltin Float2x3)
    (#const float2x4)       -> return (MBuiltin Float2x4)
    (#const float3x2)       -> return (MBuiltin Float3x2)
    (#const float2x3)       -> return (MBuiltin Float2x3)
    (#const float2x4)       -> return (MBuiltin Float2x4)
    (#const float2x4)       -> return (MBuiltin Float3x3)
    (#const float2x4)       -> return (MBuiltin Float3x4)
    (#const float2x4)       -> return (MBuiltin Float4x2)
    (#const float2x4)       -> return (MBuiltin Float4x3)
    (#const float2x4)       -> return (MBuiltin Float4x4) 
    (#const thread_position_in_grid)   -> return ThreadPositionInGrid
    (#const structure)      -> return (MBuiltin Structure)
    (#const half)          -> return (MBuiltin Half)
    (#const half2)          -> return (MBuiltin Half2)
    (#const half3)          -> return (MBuiltin Half3)
    (#const half4)          -> return (MBuiltin Half4)
    (#const half2x2)        -> return (MBuiltin Half2x2)
    (#const half2x3)        -> return (MBuiltin Half2x3)
    (#const half2x4)        -> return (MBuiltin Half2x4)
    (#const half3x2)        -> return (MBuiltin Half3x2)
    (#const half3x3)        -> return (MBuiltin Half3x3)
    (#const half3x4)         -> return (MBuiltin Half3x4)
    (#const half4x2)         -> return (MBuiltin Half4x2)
    (#const half4x3)         -> return (MBuiltin Half4x3)
    (#const half4x4)       -> return (MBuiltin Half4x4)
    (#const r8Snorm)       -> return (MBuiltin R8Snorm)                        -- An ordinary format with one 8-bit, normalized, signed integer component.
    (#const r8Unorm)       -> return (MBuiltin R8Unorm)                        -- An ordinary format with one 8-bit, normalized, unsigned integer component.
    (#const half4x4)       -> return (MBuiltin RG8Snorm)                       -- An ordinary format with two 8-bit, normalized, signed integer components.
    (#const half4x4)       -> return (MBuiltin RG8Unorm)                       -- An ordinary format with two 8-bit, normalized, unsigned integer components.
    (#const half4x4)       -> return (MBuiltin RGBA8Snorm)                     -- An ordinary format with four 8-bit, normalized, signed integer components in RGBA order.
    (#const half4x4)       -> return (MBuiltin RGBA8Unorm)                     -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order.
    (#const half4x4)       -> return (MBuiltin RGBA8Unorm_srgb)                -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order with conversion between sRGB and linear space.
    (#const half4x4)       -> return (MBuiltin R16Snorm)                       -- An ordinary format with one 16-bit, normalized, signed integer component.
    (#const half4x4)       -> return (MBuiltin R16Unorm)                       -- An ordinary format with one 16-bit, normalized, unsigned integer component.
    (#const half4x4)       -> return (MBuiltin RG16Snorm)                      -- An ordinary format with two 16-bit, normalized, signed integer components.
    (#const half4x4)       -> return (MBuiltin RG16Unorm)                      -- An ordinary format with two 16-bit, normalized, unsigned integer components.
    (#const rgba16Snorm)       -> return (MBuiltin RGBA16Snorm)                    -- An ordinary format with four 16-bit, normalized, signed integer components in RGBA order.
    (#const rgba16Unorm)       -> return (MBuiltin RGBA16Unorm)                    -- An ordinary format with four 16-bit, normalized, unsigned integer components in RGBA order.
    (#const rgb9e5Float)       -> return (MBuiltin RGB9e5Float)                    -- A packed 32-bit format with three 9-bit, floating-point color components.
    (#const rgb10a2Unorm)       -> return (MBuiltin RGB10a2Unorm)                   -- A packed 32-bit format with normalized, unsigned integer color components.
    (#const rg11b10Float)       -> return (MBuiltin RG11b10Float)                   -- A packed 32-bit format with two 11-bit (for red and green) and one 10-bit (for blue) floating-point color components.
    (#const texture)       -> return (MBuiltin MTLTexture)                        -- A texture.
    (#const sampler)       -> return (MBuiltin MTLSampler)                        -- A texture sampler.
    (#const indirectCommandBuffer)        -> return (MBuiltin IndirectCommandBuffer)          -- An indirect command buffer.
    (#const renderPipeline)                       -> return (MBuiltin RenderPipeline)                 -- A render pipeline.
    (#const computePipeline)                      -> return (MBuiltin ComputePipeline)                -- A compute pipeline.
    (#const visibleFunctionTable)                 -> return (MBuiltin VisibleFunctionTable)           -- A table of visible functions that the pipeline can call.
    (#const intersectionFunctionTable)            -> return (MBuiltin IntersectionFunctionTable)      -- A table of intersection functions that the pipeline can call.
    (#const primitiveAccelerationStructure)       -> return (MBuiltin PrimitiveAccelerationStructure) -- A low-level ray-tracing acceleration structure for a set of primitives.
    (#const instanceAccelerationStructure)        -> return (MBuiltin InstanceAccelerationStructure)  -- A high-level ray-tracing acceleration structure for a set of instances of low-level primitives.
    (#const true)    -> return MTrue
    (#const false)   -> return MFalse
    (#const array)     -> return (MBuiltin Array)
    (#const pointer)   -> return (MBuiltin Pointer)
    (#const kernel)    -> return (MBuiltin Kernel)
    (#const device)    -> return (MBuiltin Device)
    (#const void)      -> return (MBuiltin Void)
    (#const METAL_SHADER_FILE)    -> return (MBuiltin MFile)
    _                       -> return MNone

-- | Defines an instance method.
-- The Haskell function must accept the receiver as the first argument.
-- If @argc >= 0@, the type of the arguments of the function must be `FRValue`.
metal_create_render_instance :: MValue -> String -> FunPtr a -> Int  -> IO ()
metal_create_render_instance r s f i = withCString s (\cs -> createRenderingInstanceGPU r cs f i)

metal_create_compute_instance :: MValue -> String -> FunPtr a -> Int -> IO ()
metal_create_compute_instance r s f i = withCString s (\cs -> createComputeInstance r cs f i)

metal_create_render_instance_cpu :: MValue -> String -> FunPtr a -> Int -> IO ()
metal_create_render_instance_cpu r s f i = withCString s (\cs -> createRenderingInstanceCPU r cs f i)

getkernel :: String -> IO MValue
getkernel s = do
    o <- peek metal_kernel
    return o 

