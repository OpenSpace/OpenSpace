//
// Test Suite for C-API GEOSBuffer and GEOSBufferWithStyle

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
struct test_capigeosbuffer_data : public capitest::utility {
    GEOSBufferParams* bp_;
    double area_;

    test_capigeosbuffer_data()
        : bp_(nullptr)
    {
        GEOSWKTWriter_setTrim(wktw_, 1);
    }

    ~test_capigeosbuffer_data()
    {
        GEOSBufferParams_destroy(bp_);
    }

};

typedef test_group<test_capigeosbuffer_data> group;
typedef group::object object;

group test_capigeosbuffer_group("capi::GEOSBuffer");

//
// Test Cases
//


// Buffer against empty point
template<>
template<>
void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("POINT EMPTY");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 1, 8,
                                 GEOSBUF_CAP_ROUND,
                                 GEOSBUF_JOIN_BEVEL,
                                 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(std::string(wkt_), std::string("POLYGON EMPTY"));
}

// Buffer against empty linestring
template<>
template<>
void object::test<2>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING EMPTY");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 1, 8,
                                 GEOSBUF_CAP_ROUND,
                                 GEOSBUF_JOIN_BEVEL,
                                 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(std::string(wkt_), std::string("POLYGON EMPTY"));
}

// Buffer against empty polygon
template<>
template<>
void object::test<3>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON EMPTY");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 1, 8,
                                 GEOSBUF_CAP_ROUND,
                                 GEOSBUF_JOIN_BEVEL,
                                 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(std::string(wkt_), std::string("POLYGON EMPTY"));
}

// Simple Buffer on a 2-vertices line (quadSegs: 1)
template<>
template<>
void object::test<4>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBuffer(geom1_, 5, 1);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 7);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 161.803, 0.001);

}

// Simple Buffer on a 2-vertices line (quadSegs: 2)
template<>
template<>
void object::test<5>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBuffer(geom1_, 5, 2);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 11);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 182.514, 0.001);
}

// Buffer with square end caps on a 2-vertices line (no matter quadSegs)
template<>
template<>
void object::test<6>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_ROUND, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 7);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 211.803, 0.001);

}

// Buffer with flat end caps on a 2-vertices line (no matter quadSegs)
template<>
template<>
void object::test<7>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_FLAT,
                                 GEOSBUF_JOIN_ROUND, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 5);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 111.803, 0.001);
}

// Buffer with flat end cap on a 2-vertices horizontal line
template<>
template<>
void object::test<8>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_FLAT,
                                 GEOSBUF_JOIN_ROUND, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 5);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 50.0, 0.001);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((10.0000000000000000 15.0000000000000000, 10.0000000000000000 5.0000000000000000, 5.0000000000000000 5.0000000000000000, 5.0000000000000000 15.0000000000000000, 10.0000000000000000 15.0000000000000000))"
                  ));
}

// Buffer with square end cap on a 2-vertices horizontal line
template<>
template<>
void object::test<9>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_ROUND, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 7);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 150.0, 0.001);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((10.0000000000000000 15.0000000000000000, 15.0000000000000000 15.0000000000000000, 15.0000000000000000 5.0000000000000000, 5.0000000000000000 5.0000000000000000, 0.0000000000000000 5.0000000000000009, 0.0000000000000000 15.0000000000000000, 10.0000000000000000 15.0000000000000000))"
                  ));
}

// Buffer with flat end cap and round join style
// on an L-shaped simple line
template<>
template<>
void object::test<10>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_ROUND, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 29);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 244.615, 0.001);

}

// Buffer with flat end cap and mitre join style
// on an L-shaped simple line
template<>
template<>
void object::test<11>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_MITRE, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 9);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 250.0, 0.001);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((5.0000000000000000 15.0000000000000000, 5.0000000000000000 20.0000000000000000, 5.0000000000000000 25.0000000000000000, 15.0000000000000000 25.0000000000000000, 15.0000000000000000 5.0000000000000000, 5.0000000000000000 5.0000000000000000, 0.0000000000000000 5.0000000000000009, 0.0000000000000000 15.0000000000000000, 5.0000000000000000 15.0000000000000000))"
                  ));
}

// Buffer with flat end cap and bevel join style
// on an L-shaped simple line
template<>
template<>
void object::test<12>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 20, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_BEVEL, 5.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 10);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 237.5, 0.001);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((5.0000000000000000 15.0000000000000000, 5.0000000000000000 20.0000000000000000, 5.0000000000000000 25.0000000000000000, 15.0000000000000000 25.0000000000000000, 15.0000000000000000 10.0000000000000000, 10.0000000000000000 5.0000000000000000, 5.0000000000000000 5.0000000000000000, 0.0000000000000000 5.0000000000000009, 0.0000000000000000 15.0000000000000000, 5.0000000000000000 15.0000000000000000))"
                  ));
}

// Buffer with flat end cap and bevel join style
// on an L-shaped simple line with different quadSegs and mitreLimit
// (result unaffected)
template<>
template<>
void object::test<13>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10, 10 20)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 5, 200, GEOSBUF_CAP_SQUARE,
                                 GEOSBUF_JOIN_BEVEL, 10.0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 10);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 237.5, 0.001);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((5.0000000000000000 15.0000000000000000, 5.0000000000000000 20.0000000000000000, 5.0000000000000000 25.0000000000000000, 15.0000000000000000 25.0000000000000000, 15.0000000000000000 10.0000000000000000, 10.0000000000000000 5.0000000000000000, 5.0000000000000000 5.0000000000000000, 0.0000000000000000 5.0000000000000009, 0.0000000000000000 15.0000000000000000, 5.0000000000000000 15.0000000000000000))"
                  ));
}

// Buffer with limited mitre  (1)
template<>
template<>
void object::test<14>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((0 0, 10 0, 10 10, 0 0))");
    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 2, 200, GEOSBUF_CAP_FLAT,
                                 GEOSBUF_JOIN_MITRE, 1);
    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 7);
    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 132.289, 0.001);
}

// Buffer with limited mitre  (2)
template<>
template<>
void object::test<15>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((0 0, 10 0, 10 10, 0 0))");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 2, 200, GEOSBUF_CAP_FLAT,
                                 GEOSBUF_JOIN_MITRE, 2);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 6);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 140.352, 0.001);

}

// Buffer with limited mitre  (3)
template<>
template<>
void object::test<16>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((0 0, 10 0, 10 10, 0 0))");

    ensure(nullptr != geom1_);

    geom2_ = GEOSBufferWithStyle(geom1_, 2, 200, GEOSBUF_CAP_FLAT,
                                 GEOSBUF_JOIN_MITRE, 3);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(GEOSGetNumCoordinates(geom2_), 4);

    ensure(0 != GEOSArea(geom2_, &area_));
    ensure_area(area_, 141.598, 0.001);

}

// Buffer with params:
// flat end cap on a straight line
template<>
template<>
void object::test<17>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10)");

    ensure(nullptr != geom1_);

    bp_ = GEOSBufferParams_create();

    GEOSBufferParams_setEndCapStyle(bp_, GEOSBUF_CAP_SQUARE);
    geom2_ = GEOSBufferWithParams(geom1_, bp_, 2);

    ensure(nullptr != geom2_);

    wkt_ = GEOSWKTWriter_write(wktw_, geom2_);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((10 12, 12 12, 12 8, 5 8, 3 8, 3 12, 10 12))"
                  ));
}

// Buffer with params:
// flat end cap on a straight line
// Single sided (left)
template<>
template<>
void object::test<18>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10)");

    ensure(nullptr != geom1_);

    bp_ = GEOSBufferParams_create();

    GEOSBufferParams_setEndCapStyle(bp_, GEOSBUF_CAP_SQUARE);
    GEOSBufferParams_setSingleSided(bp_, 1);
    geom2_ = GEOSBufferWithParams(geom1_, bp_, 2);

    ensure(nullptr != geom2_);

    wkt_ = GEOSWKTWriter_write(wktw_, geom2_);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((10 10, 5 10, 5 12, 10 12, 10 10))"
                  ));
}

// Buffer with params:
// flat end cap on a straight line
// Single sided (right)
template<>
template<>
void object::test<19>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(5 10, 10 10)");

    ensure(nullptr != geom1_);

    bp_ = GEOSBufferParams_create();

    GEOSBufferParams_setEndCapStyle(bp_, GEOSBUF_CAP_SQUARE);
    GEOSBufferParams_setSingleSided(bp_, 1);
    geom2_ = GEOSBufferWithParams(geom1_, bp_, -2);

    ensure(nullptr != geom2_);

    wkt_ = GEOSWKTWriter_write(wktw_, geom2_);

    ensure_equals(std::string(wkt_), std::string(
                      "POLYGON ((5 10, 10 10, 10 8, 5 8, 5 10))"
                  ));
}

// Single-sided buffer  (3)
// http://trac.osgeo.org/geos/ticket/455
template<>
template<>
void object::test<20>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(0 0, 10 0, 10 10)', -10)");

    ensure(nullptr != geom1_);

    geom2_ = GEOSSingleSidedBuffer(geom1_, 10, 8, GEOSBUF_JOIN_BEVEL, 0, 0);

    ensure(nullptr != geom2_);

    wkt_ = GEOSGeomToWKT(geom2_);

    ensure_equals(std::string(wkt_), std::string(
                      "LINESTRING (20.0000000000000000 10.0000000000000000, 20.0000000000000000 0.0000000000000000, 10.0000000000000000 -10.0000000000000000, 0.0000000000000000 -10.0000000000000000)"
                  ));

}

/*
// Invalid result polygon with full precision, fall back on lower precision
// https://trac.osgeo.org/geos/ticket/1131
template<>
template<>
void object::test<21>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((-6503873.862740669 -3656747.43316935, -6481859.9985945 -3656747.43316935, -6481859.9985945 -3688545.2369360398, -6506319.8476458 -3688545.2369360398, -6506319.8476458 -3664085.38788474, -6501427.87783554 -3664085.38788474, -6501427.87783554 -3661639.40297961, -6498981.89293041 -3661639.40297961, -6498981.89293041 -3659193.41807448, -6503873.862740669 -3659193.41807448, -6503873.862740669 -3656747.43316935))");
    ensure(nullptr != geom1_);

    bp_ = GEOSBufferParams_create();
    GEOSBufferParams_setQuadrantSegments(bp_, 1);
    geom2_ = GEOSBufferWithParams(geom1_, bp_, 2445.98490513);

    wkt_ = GEOSGeomToWKT(geom2_);
    std::cout << wkt_ << "\n";

    ensure(nullptr != geom2_);
    ensure("Buffer result polygon is not valid", GEOSisValid(geom2_));
}
*/

} // namespace tut
