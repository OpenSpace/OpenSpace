// Test Suite for C-API LineString project functions

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capiproject_data : public capitest::utility {};

typedef test_group<test_capiproject_data> group;
typedef group::object object;

group test_capiproject_group("capi::GEOSProject");

//
// Test Cases
//

// Test basic usage
template<>
template<>
void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING (0 0, 0 2)");
    geom2_ = GEOSGeomFromWKT("POINT (1 1)");


    double dist = GEOSProject(geom1_, geom2_);
    ensure_equals(dist, 1.0);

    double dist_norm = GEOSProjectNormalized(geom1_, geom2_);
    ensure_equals(dist_norm, 0.5);
}

// Test non-linestring geometry (first argument) correctly returns -1.0
template<>
template<>
void object::test<2>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))");
    geom2_ = GEOSGeomFromWKT("POINT (1 1)");

    double dist = GEOSProject(geom1_, geom2_);
    ensure_equals(dist, -1.0);

    double dist_norm = GEOSProjectNormalized(geom1_, geom2_);
    ensure_equals(dist_norm, -1.0);
}

// Test non-point geometry (second argument) correctly returns -1.0
// https://trac.osgeo.org/geos/ticket/1058
template<>
template<>
void object::test<3>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING (0 0, 0 2)");
    geom2_ = GEOSGeomFromWKT("LINESTRING (0 0, 0 2)");

    double dist = GEOSProject(geom1_, geom2_);
    ensure_equals(dist, -1.0);

    double dist_norm = GEOSProjectNormalized(geom1_, geom2_);
    ensure_equals(dist_norm, -1.0);
}

// Test
// https://github.com/libgeos/geos/issues/475
template<>
template<>
void object::test<4>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING (0 0, 0 0)");
    geom2_ = GEOSGeomFromWKT("POINT (0 0)");

    double dist = GEOSProject(geom1_, geom2_);
    ensure_equals(dist, 0.0);

    double dist_norm = GEOSProjectNormalized(geom1_, geom2_);
    ensure_equals(dist_norm, 0.0);
}

} // namespace tut
