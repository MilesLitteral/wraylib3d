protocol MTLIndirectComputeCommand : ns::Object
Overview
Don’t implement this protocol; you get objects of this type by asking a MTLIndirectCommandBuffer for them.

Use this object to reset or encode a command. You must always reset a command before encoding a new command.

Topics
Setting a Command's Arguments
func setComputePipelineState(MTLComputePipelineState)
Sets the command’s compute pipeline state object.
Required.
func setImageblockWidth(Int, height: Int)
Sets the size, in pixels, of the imageblock.
Required.
func setKernelBuffer(MTLBuffer, offset: Int, at: Int)
Sets a buffer for the compute function.
Required.
func setThreadgroupMemoryLength(Int, index: Int)
Sets the size of a block of threadgroup memory.
Required.
func setThreadgroupMemoryLength(Int, at: Int)
Sets the size of a block of threadgroup memory.
Deprecated
func setStageInRegion(MTLRegion)
Sets the region of the stage-in attributes to apply to the compute kernel.
Required.
func setStageIn(MTLRegion)
Sets the region of the stage-in attributes to apply to the compute kernel.
Deprecated
Synchronizing Command Execution
func setBarrier()
Adds a barrier to ensure that commands executed prior to this command are complete before this command executes.
Required.
func clearBarrier()
Removes any barrier set on the command.
Required.
Encoding a Compute Command
func concurrentDispatchThreadgroups(MTLSize, threadsPerThreadgroup: MTLSize)
Encodes a compute command using a grid aligned to threadgroup boundaries.
Required.
func concurrentDispatchThreads(MTLSize, threadsPerThreadgroup: MTLSize)
Encodes a compute command using an arbitrarily sized grid.
Required.
Resetting a Command
func reset()
Resets the command to its default state.
Required.