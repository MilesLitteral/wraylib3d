// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from parcelable.djinni

#import "DBParcelableListMapSet+Private.h"
#import "DJIMarshal+Private.h"
#include <cassert>

namespace djinni_generated {

auto ParcelableListMapSet::toCpp(ObjcType obj) -> CppType
{
    assert(obj);
    return {::djinni::List<::djinni::Map<::djinni::String, ::djinni::Set<::djinni::String>>>::toCpp(obj.listMapSet)};
}

auto ParcelableListMapSet::fromCpp(const CppType& cpp) -> ObjcType
{
    return [[DBParcelableListMapSet alloc] initWithListMapSet:(::djinni::List<::djinni::Map<::djinni::String, ::djinni::Set<::djinni::String>>>::fromCpp(cpp.list_map_set))];
}

}  // namespace djinni_generated