MTLVisibleFunctionTable
Topics
Setting a Table Entry
func setFunction(MTLFunctionHandle?, index: Int)
Sets a table entry to point to a callable function.
Required.
func setFunctions([(MTLFunctionHandle)?], range: Range<Int>)
Sets a range of table entries to point to an array of callable functions.
Instance Properties
var gpuResourceID: MTLResourceID
Required.
Relationships
Inherits From
MTLResource
See Also
Shader Functions
class MTLFunctionDescriptor
A description of a function object to create.
protocol MTLFunction
An object that represents a public shader function in a Metal library.
protocol MTLFunctionHandle
An object representing a function that you can add to a visible function table.
class MTLVisibleFunctionTableDescriptor
A specification of how to create a visible function table.
class MTLIntersectionFunctionDescriptor
A description of an intersection function that performs an intersection test.
class MTLIntersectionFunctionTableDescriptor
A specification of how to create an intersection function table.
protocol MTLIntersectionFunctionTable
A table of intersection functions that Metal calls to perform ray-tracing intersection tests.