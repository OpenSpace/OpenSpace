//
// Test Suite for C-API GEOSisClosed

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capiisclosed_data : public capitest::utility {};

typedef test_group<test_capiisclosed_data> group;
typedef group::object object;

group test_capiisclosed_group("capi::GEOSisClosed");

//
// Test Cases
//

template<>
template<>
void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(0 0, 1 0, 1 1)");
    int r = GEOSisClosed(geom1_);
    ensure_equals(r, 0);
}

template<>
template<>
void object::test<2>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(0 0, 0 1, 1 1, 0 0)");
    int r = GEOSisClosed(geom1_);
    ensure_equals(r, 1);
}

template<>
template<>
void object::test<3>
()
{
    geom1_ = GEOSGeomFromWKT("MULTILINESTRING ((1 1, 1 2, 2 2, 1 1), (0 0, 0 1, 1 1))");
    int r = GEOSisClosed(geom1_);
    ensure_equals(r, 0);
}

template<>
template<>
void object::test<4>
()
{
    geom1_ = GEOSGeomFromWKT("MULTILINESTRING ((1 1, 1 2, 2 2, 1 1), (0 0, 0 1, 1 1, 0 0))");
    int r = GEOSisClosed(geom1_);
    ensure_equals(r, 1);
}

} // namespace tut
