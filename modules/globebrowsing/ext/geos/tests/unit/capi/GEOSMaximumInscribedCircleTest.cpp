// $Id$
//
// Test Suite for C-API GEOSGetCentroid

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
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
struct test_capimaximuminscribedcircle_data : public capitest::utility {
    char* wkt_;

    test_capimaximuminscribedcircle_data()
        : wkt_(nullptr)
    {
        GEOSWKTWriter_setTrim(wktw_, 1);
        GEOSWKTWriter_setRoundingPrecision(wktw_, 8);
    }

    ~test_capimaximuminscribedcircle_data()
    {
        GEOSFree(wkt_);
    }

};

typedef test_group<test_capimaximuminscribedcircle_data> group;
typedef group::object object;

group test_capimaximuminscribedcircle_group("capi::GEOSMaximumInscribedCircle");

//
// Test Cases
//

// Single point
template<>
template<>
void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON ((100 200, 200 200, 200 100, 100 100, 100 200))");
    ensure(nullptr != geom1_);
    geom2_ = GEOSMaximumInscribedCircle(geom1_, 0.001);
    ensure(nullptr != geom2_);

    wkt_ = GEOSWKTWriter_write(wktw_, geom2_);

    ensure_equals(std::string(wkt_), std::string("LINESTRING (150 150, 150 200)"));
}

// Single point
template<>
template<>
void object::test<2>
()
{
    geom1_ = GEOSGeomFromWKT("MULTIPOINT ((100 100), (100 200), (200 200), (200 100))");
    ensure(nullptr != geom1_);
    geom2_ = GEOSLargestEmptyCircle(geom1_, nullptr, 0.001);
    ensure(nullptr != geom2_);

    wkt_ = GEOSWKTWriter_write(wktw_, geom2_);

    ensure_equals(std::string(wkt_), std::string("LINESTRING (150 150, 100 100)"));
}


} // namespace tut

