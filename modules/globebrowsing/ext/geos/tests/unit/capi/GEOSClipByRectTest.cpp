//
// Test Suite for C-API GEOSClipByRect

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
// std
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <memory>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capigeosclipbyrect_data : public capitest::utility {
    void
    isEqual(GEOSGeom g, const char* exp_wkt)
    {
        geom3_ = GEOSGeomFromWKT(exp_wkt);
        bool eq = GEOSEquals(geom3_, g) != 0;
        if(! eq) {
            std::printf("EXP: %s\n", exp_wkt);
            char* obt_wkt = GEOSWKTWriter_write(wktw_, g);
            std::printf("OBT: %s\n", obt_wkt);
            free(obt_wkt);
        }
        ensure(eq);
    }

    test_capigeosclipbyrect_data()
    {
        GEOSWKTWriter_setTrim(wktw_, 1);
        GEOSWKTWriter_setRoundingPrecision(wktw_, 8);
    }
};

typedef test_group<test_capigeosclipbyrect_data> group;
typedef group::object object;

group test_capigeosclipbyrect_group("capi::GEOSClipByRect");

//
// Test Cases
//

/// Point outside
template<> template<> void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("POINT(0 0)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POINT EMPTY");
}

/// Point inside
template<> template<> void object::test<2>
()
{
    geom1_ = GEOSGeomFromWKT("POINT(15 15)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POINT(15 15)");
}

/// Point on boundary
template<> template<> void object::test<3>
()
{
    geom1_ = GEOSGeomFromWKT("POINT(15 10)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POINT EMPTY");
}

/// Line outside
template<> template<> void object::test<4>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(0 0, -5 5)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "LINESTRING EMPTY");
}

/// Line inside
template<> template<> void object::test<5>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(15 15, 16 15)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "LINESTRING(15 15, 16 15)");
}

/// Line on boundary
template<> template<> void object::test<6>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(10 15, 10 10, 15 10)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "LINESTRING EMPTY");
}

/// Line splitting rectangle
template<> template<> void object::test<7>
()
{
    geom1_ = GEOSGeomFromWKT("LINESTRING(10 5, 25 20)");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "LINESTRING (15 10, 20 15)");
}

/// Polygon shell (CCW) fully on rectangle boundary
template<> template<> void object::test<8>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((10 10, 20 10, 20 20, 10 20, 10 10))");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POLYGON((10 10, 20 10, 20 20, 10 20, 10 10))");
}

/// Polygon shell (CW) fully on rectangle boundary
template<> template<> void object::test<9>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((10 10, 10 20, 20 20, 20 10, 10 10))");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POLYGON((10 10, 20 10, 20 20, 10 20, 10 10))");
}

/// Polygon hole (CCW) fully on rectangle boundary
template<> template<> void object::test<10>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((0 0, 0 30, 30 30, 30 0, 0 0),(10 10, 20 10, 20 20, 10 20, 10 10))");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POLYGON EMPTY");
}

/// Polygon hole (CW) fully on rectangle boundary
template<> template<> void object::test<11>
()
{
    geom1_ = GEOSGeomFromWKT("POLYGON((0 0, 0 30, 30 30, 30 0, 0 0),(10 10, 10 20, 20 20, 20 10, 10 10))");
    geom2_ = GEOSClipByRect(geom1_, 10, 10, 20, 20);
    isEqual(geom2_, "POLYGON EMPTY");
}

/// Polygon fully within rectangle
template<> template<> void object::test<12>
()
{
    const char* wkt = "POLYGON((1 1, 1 30, 30 30, 30 1, 1 1),(10 10, 20 10, 20 20, 10 20, 10 10))";
    geom1_ = GEOSGeomFromWKT(wkt);
    geom2_ = GEOSClipByRect(geom1_, 0, 0, 40, 40);
    isEqual(geom2_, wkt);
}

/// Polygon overlapping rectangle
template<> template<> void object::test<13>
()
{
    const char* wkt = "POLYGON((0 0, 0 30, 30 30, 30 0, 0 0),(10 10, 20 10, 20 20, 10 20, 10 10))";
    geom1_ = GEOSGeomFromWKT(wkt);
    geom2_ = GEOSClipByRect(geom1_, 5, 5, 15, 15);
    isEqual(geom2_, "POLYGON ((5 5, 5 15, 10 15, 10 10, 15 10, 15 5, 5 5))");
}

/// Clipping invalid polygon
template<> template<> void object::test<14>
()
{
    const char* wkt = "POLYGON((1410 2055, 1410 2056, 1410 2057, 1410 2055))";
    geom1_ = GEOSGeomFromWKT(wkt);
    geom2_ = GEOSClipByRect(geom1_, -8, -8, 2056, 2056);
    if (geom2_ != nullptr) {
        char* obt_wkt = GEOSWKTWriter_write(wktw_, geom2_);
        std::printf("OBT: %s\n", obt_wkt);
    }
    ensure(nullptr == geom2_);
}

// Polygon fully covering rectangle
// https://trac.osgeo.org/postgis/ticket/4904
template<> template<> void object::test<15>
()
{
    //  POLYGON((0 0,10 0,10 10,0 10))
    //  Clip by ST_MakeEnvelope(2,2,5,5)
    GEOSCoordSequence *cs = GEOSCoordSeq_create(5, 2);
    GEOSCoordSeq_setXY(cs, 0,  0,  0);
    GEOSCoordSeq_setXY(cs, 1, 10,  0);
    GEOSCoordSeq_setXY(cs, 2, 10, 10);
    GEOSCoordSeq_setXY(cs, 3,  0, 10);
    GEOSCoordSeq_setXY(cs, 4,  0,  0);
    GEOSGeometry *shell = GEOSGeom_createLinearRing(cs);
    geom1_ = GEOSGeom_createPolygon(shell, NULL, 0);


    geom2_ = GEOSClipByRect(geom1_, 2, 2, 5, 5);
    isEqual(geom2_, "POLYGON ((2 2, 2 5, 5 5, 5 2, 2 2))");
}



} // namespace tut

