protocol MTLIndirectRenderCommand
Overview
Don’t implement this protocol; you get objects of this type by asking a MTLIndirectCommandBuffer for them.

Use this object to reset or encode a command. You must always reset a command before encoding a new command.

Topics
Setting Command Arguments
func setRenderPipelineState(MTLRenderPipelineState)
Sets the render pipeline state object used by the command.
Required.
func setVertexBuffer(MTLBuffer, offset: Int, at: Int)
Sets a vertex buffer argument for the command.
Required.
func setFragmentBuffer(MTLBuffer, offset: Int, at: Int)
Sets a fragment buffer argument for the command.
Required.
Encoding a Drawing Command
func drawPrimitives(MTLPrimitiveType, vertexStart: Int, vertexCount: Int, instanceCount: Int, baseInstance: Int)
Encodes a command to render a number of instances of primitives using vertex data in contiguous array elements, starting from the base instance.
Required.
func drawIndexedPrimitives(MTLPrimitiveType, indexCount: Int, indexType: MTLIndexType, indexBuffer: MTLBuffer, indexBufferOffset: Int, instanceCount: Int, baseVertex: Int, baseInstance: Int)
Encodes a command to render a number of instances of primitives using an index list specified in a buffer, starting from the base vertex of the base instance.
Required.
func drawPatches(Int, patchStart: Int, patchCount: Int, patchIndexBuffer: MTLBuffer?, patchIndexBufferOffset: Int, instanceCount: Int, baseInstance: Int, tessellationFactorBuffer: MTLBuffer, tessellationFactorBufferOffset: Int, tessellationFactorBufferInstanceStride: Int)
Encodes a command to render a number of instances of tessellated patches.
Required.
func drawIndexedPatches(Int, patchStart: Int, patchCount: Int, patchIndexBuffer: MTLBuffer?, patchIndexBufferOffset: Int, controlPointIndexBuffer: MTLBuffer, controlPointIndexBufferOffset: Int, instanceCount: Int, baseInstance: Int, tessellationFactorBuffer: MTLBuffer, tessellationFactorBufferOffset: Int, tessellationFactorBufferInstanceStride: Int)
Encodes a command to render a number of instances of tessellated patches, using a control point index buffer.
Required.
Resetting a Command
func reset()
Resets the command to its default state.
Required.
Relationships
Inherits From
NSObjectProtocol