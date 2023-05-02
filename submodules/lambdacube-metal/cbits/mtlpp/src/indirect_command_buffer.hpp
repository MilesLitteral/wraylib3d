// #pragma once

#include "defines.hpp"
#include "ns.hpp"

namespace mtlpp
{
    class IndirectCommandBuffer : public ns::Object
    {
        public:
            IndirectCommandBuffer();
            IndirectCommandBuffer(const ns::Handle& handle);

            // Determining the Maximum Number of Commands
            int size;
            ns::String label;
            
            // The number of commands contained in the indirect command buffer. Required.
            mtlpp::ResourceID gpuResourceID; // Instance Variable Required.    

            //Retrieving Commands
            // Gets the render command at the given index.  Required.
            mtlpp::IndirectRenderCommand  indirectRenderCommandAt(int rAt);

            // Gets the compute command at the given index. Required.
            mtlpp::IndirectComputeCommand indirectComputeCommandAt(int cAt);

            //Resetting Commands
            //Resets a range of commands to their default state.
            void reset(Range<int> r);
    };
}