{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, CPP #-}

#include "cbits/mtlpp.h"
module LambdaCube.Metal.Bindings where

import Foreign
import Foreign.C

-- The Foreign Interface
-- Every 'result' from Metal will be a Pointer 
type MValue     = Ptr CULong
type MetalSizeN = CInt

metal_stdlib :: String
metal_stdlib = "<metal_stdlib>"

-- | The Metal Built-In Types
data MBuiltin = None                           -- The argument doesn't have a valid data type.
            |Structure                      -- A (Data) structure.
            |Array                          -- An array.
            |Pointer                         -- A pointer.
            |Kernel
            |Vertex
            |Fragment
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
            |MTexture                        -- A texture.
            |MSampler                        -- A texture sampler.
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

foreign import ccall        "createRendererInstance"          createRendererInstance  :: IO ()
foreign import ccall unsafe "&metal_kernel"                    metal_kernel                :: Ptr MValue
foreign import ccall   safe "metal_krncall"                    metal_krncall              :: MValue -> MValue -> MValue -> IO MValue
{-
foreign import ccall   safe "rb_funcall"                c_rb_funcall_4                :: FRValue -> RID -> Int -> FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
foreign import ccall unsafe "&rb_cObject"               rb_cObject                    :: Ptr FRValue-}

mtype :: MValue -> IO MBuiltin
mtype v = metalType v >>= \x -> case x of
    (#const none)          -> return MNone
    (#const kernel)        -> return Kernel
    (#const vertex)        -> return Vertex
    (#const fragment)      -> return Fragment
    (#const bool1)         -> return Bool1
    (#const bool2)         -> return Bool2
    (#const bool3)         -> return Bool3
    (#const bool4)         -> return Bool4
    (#const char1)         -> return Char1
    (#const char2)         -> return Char2
    (#const char3)         -> return Char3
    (#const char4)         -> return Char4
    (#const uchar)         -> return UChar
    (#const uchar2)        -> return UChar2
    (#const uchar3)        -> return UChar3
    (#const uchar4)        -> return UChar4
    (#const short1)        -> return Short1
    (#const short2)        -> return Short2
    (#const short3)        -> return Short3
    (#const short4)        -> return Short4
    (#const ushort)        -> return UShort
    (#const ushort2)       -> return UShort2
    (#const ushort3)       -> return UShort3
    (#const ushort4)       -> return UShort4
    (#const int1)          -> return Int1
    (#const int2)          -> return Int2
    (#const int3)          -> return Int3
    (#const int4)          -> return Int4
    (#const uint)          -> return UInt
    (#const uint2)         -> return UInt2
    (#const uint3)         -> return UInt3
    (#const uint4)         -> return UInt4
    (#const long1)         -> return Long1
    (#const long2)         -> return Long2
    (#const long3)         -> return Long3
    (#const long4)         -> return Long4
    (#const ulong)         -> return ULong
    (#const ulong2)        -> return ULong2
    (#const ulong3)        -> return ULong3
    (#const ulong4)        -> return ULong4
    (#const float1)        -> return Float1
    (#const float2)        -> return Float2
    (#const float3)        -> return Float3
    (#const float4)        -> return Float4
    (#const float2x2)      -> return Float2x2
    (#const float2x3)      -> return Float2x3
    (#const float2x4)      -> return Float2x4
    (#const float3x2)      -> return Float3x2
    (#const float2x3)      -> return Float2x3
    (#const float2x4)      -> return Float2x4
    (#const float2x4)      -> return Float3x3
    (#const float2x4)      -> return Float3x4
    (#const float2x4)      -> return Float4x2
    (#const float2x4)      -> return Float4x3
    (#const float2x4)      -> return Float4x4 
    (#const thread_position_in_grid)   -> return ThreadPositionInGrid
    (#const structure)     -> return  Structure
    (#const half)          -> return   Half
    (#const half2)         -> return  Half2
    (#const half3)         -> return  Half3
    (#const half4)         -> return  Half4
    (#const half2x2)       -> return  Half2x2
    (#const half2x3)       -> return  Half2x3
    (#const half2x4)       -> return  Half2x4
    (#const half3x2)       -> return  Half3x2
    (#const half3x3)       -> return  Half3x3
    (#const half3x4)       -> return Half3x4
    (#const half4x2)       -> return Half4x2
    (#const half4x3)       -> return Half4x3
    (#const half4x4)       -> return Half4x4
    (#const r8Snorm)       -> return R8Snorm                        -- An ordinary format with one 8-bit, normalized, signed integer component.
    (#const r8Unorm)       -> return R8Unorm                        -- An ordinary format with one 8-bit, normalized, unsigned integer component.
    (#const half4x4)       -> return RG8Snorm                       -- An ordinary format with two 8-bit, normalized, signed integer components.
    (#const half4x4)       -> return RG8Unorm                       -- An ordinary format with two 8-bit, normalized, unsigned integer components.
    (#const half4x4)       -> return RGBA8Snorm                     -- An ordinary format with four 8-bit, normalized, signed integer components in RGBA order.
    (#const half4x4)       -> return RGBA8Unorm                     -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order.
    (#const half4x4)       -> return RGBA8Unorm_srgb                -- An ordinary format with four 8-bit, normalized, unsigned integer components in RGBA order with conversion between sRGB and linear space.
    (#const half4x4)       -> return R16Snorm                       -- An ordinary format with one 16-bit, normalized, signed integer component.
    (#const half4x4)       -> return R16Unorm                       -- An ordinary format with one 16-bit, normalized, unsigned integer component.
    (#const half4x4)       -> return RG16Snorm                      -- An ordinary format with two 16-bit, normalized, signed integer components.
    (#const half4x4)       -> return RG16Unorm                      -- An ordinary format with two 16-bit, normalized, unsigned integer components.
    (#const rgba16Snorm)   -> return RGBA16Snorm                    -- An ordinary format with four 16-bit, normalized, signed integer components in RGBA order.
    (#const rgba16Unorm)   -> return RGBA16Unorm                    -- An ordinary format with four 16-bit, normalized, unsigned integer components in RGBA order.
    (#const rgb9e5Float)   -> return RGB9e5Float                    -- A packed 32-bit format with three 9-bit, floating-point color components.
    (#const rgb10a2Unorm)  -> return RGB10a2Unorm                  -- A packed 32-bit format with normalized, unsigned integer color components.
    (#const rg11b10Float)  -> return RG11b10Float                   -- A packed 32-bit format with two 11-bit (for red and green) and one 10-bit (for blue) floating-point color components.
    (#const texture)       -> return MTLTexture                        -- A texture.
    (#const sampler)       -> return MTLSampler                      -- A texture sampler.
    (#const indirectCommandBuffer)        -> return IndirectCommandBuffer          -- An indirect command buffer.
    (#const renderPipeline)                       -> return RenderPipeline                 -- A render pipeline.
    (#const computePipeline)                      -> return ComputePipeline                -- A compute pipeline.
    (#const visibleFunctionTable)                 -> return VisibleFunctionTable           -- A table of visible functions that the pipeline can call.
    (#const intersectionFunctionTable)            -> return IntersectionFunctionTable      -- A table of intersection functions that the pipeline can call.
    (#const primitiveAccelerationStructure)       -> return PrimitiveAccelerationStructure -- A low-level ray-tracing acceleration structure for a set of primitives.
    (#const instanceAccelerationStructure)        -> return InstanceAccelerationStructure  -- A high-level ray-tracing acceleration structure for a set of instances of low-level primitives.
    (#const true)      -> return MTrue
    (#const false)     -> return MFalse
    (#const array)     -> return Array
    (#const pointer)   -> return Pointer
    (#const kernel)    -> return Kernel
    (#const device)    -> return Device
    (#const void)      -> return Void
    (#const METAL_SHADER_FILE)    -> return MFile
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

