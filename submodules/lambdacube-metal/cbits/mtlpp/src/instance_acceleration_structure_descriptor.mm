class MTLInstanceAccelerationStructureDescriptor : MTLAccelerationStructureDescriptor
Overview
Metal provides acceleration structures with a two-level hierarchy. The bottom layer consists of primitive acceleration structures, which instance acceleration structures in the top level reference.

Topics
Specifying the Instance Structures
var instanceDescriptorType: MTLAccelerationStructureInstanceDescriptorType
The format of the instance data in the descriptor buffer.
var instancedAccelerationStructures: [MTLAccelerationStructure]?
The bottom-level acceleration structures that instances use in the instance acceleration structure .
enum MTLAccelerationStructureInstanceDescriptorType
Options for specifying different kinds of instance types.
Specifying the List of Instances
var instanceCount: Int
The number of instances in the instance descriptor buffer.
var instanceDescriptorBuffer: MTLBuffer?
A buffer that contains descriptions of each instance in the acceleration structure.
var instanceDescriptorBufferOffset: Int
The offset, in bytes, to the descripton of the first instance.
var instanceDescriptorStride: Int
The stride, in bytes, between instance descriptions.
Specifying Motion Data
var motionTransformCount: Int
The number of motion transforms in the motion transform buffer.
var motionTransformBuffer: MTLBuffer?
A buffer that contains descriptions of each motion transform in the acceleration structure.
var motionTransformBufferOffset: Int
The offset, in bytes, to the descripton of the first motion transform.
Relationships
Inherits From
MTLAccelerationStructureDescriptor