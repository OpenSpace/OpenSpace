//
// Test Suite for C-API GEOSLineMerge

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
#include <cstdarg>
#include <cstdio>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capigeoslinemerge_data : public capitest::utility {
};

typedef test_group<test_capigeoslinemerge_data> group;
typedef group::object object;

group test_capigeoslinemerge_group("capi::GEOSLineMerge");

//
// Test Cases
//

template<>
template<>
void object::test<1>
()
{
    auto input = GEOSGeomFromWKT("MULTILINESTRING((0 0, 0 100),(0 -5, 0 0))");
    auto result = GEOSLineMerge(input);
    auto expected = GEOSGeomFromWKT("LINESTRING(0 -5,0 0,0 100)");

    ensure(GEOSEqualsExact(result, expected, 0));

    GEOSGeom_destroy(input);
    GEOSGeom_destroy(result);
    GEOSGeom_destroy(expected);
}

} // namespace tut

