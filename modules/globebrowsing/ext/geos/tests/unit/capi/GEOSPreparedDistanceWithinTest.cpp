//
// Test Suite for C-API GEOSPreparedDistance

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
#include <geos/constants.h>
// std
#include <cstdarg>
#include <cstdio>
#include <cstdlib>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capigeosprepareddistancewithin_data : public capitest::utility {
    const GEOSPreparedGeometry* pgeom1_;

    test_capigeosprepareddistancewithin_data()
        : pgeom1_(nullptr)
    {}

    ~test_capigeosprepareddistancewithin_data()
    {
        GEOSPreparedGeom_destroy(pgeom1_);
    }

    void checkDistanceWithin(const char* wkt1, const char* wkt2,
                       double dist, char expectedResult)
    {
        geom1_ = GEOSGeomFromWKT(wkt1);
        ensure(nullptr != geom1_);
        pgeom1_ = GEOSPrepare(geom1_);
        ensure(nullptr != pgeom1_);
        geom2_ = GEOSGeomFromWKT(wkt2);
        ensure(nullptr != geom2_);


        int ret = GEOSPreparedDistanceWithin(pgeom1_, geom2_, dist);
        ensure_equals("return code", (int)ret, (int)expectedResult);

    }


};

typedef test_group<test_capigeosprepareddistancewithin_data> group;
typedef group::object object;

group test_capigeosprepareddistancewithin_group("capi::GEOSPreparedDistanceWithin");

//
// Test Cases
//

template<>
template<>
void object::test<1>
()
{
    checkDistanceWithin(
        "POLYGON EMPTY",
        "POLYGON EMPTY",
        geos::DoubleInfinity,
        0
    );
}

template<>
template<>
void object::test<2>
()
{
    checkDistanceWithin(
        "POLYGON((1 1,1 5,5 5,5 1,1 1))",
        "POLYGON((8 8, 9 9, 9 10, 8 8))",
        4.25,
        1
    );

}

template<>
template<>
void object::test<3>
()
{
    checkDistanceWithin(
        "POLYGON((1 1,1 5,5 5,5 1,1 1))",
        "POINT(2 2)",
        0,
        1
    );
}

template<>
template<>
void object::test<4>
()
{
    checkDistanceWithin(
        "LINESTRING(1 5,5 5,5 1,1 1)",
        "POINT(2 2)",
        1,
        1
    );
}

template<>
template<>
void object::test<5>
()
{
    checkDistanceWithin(
        "LINESTRING(0 0,10 10)",
        "LINESTRING(0 10,10 0)",
        0,
        1
    );
}

template<>
template<>
void object::test<6>
()
{
    checkDistanceWithin(
        "POLYGON((0 0,10 0,10 10,0 10,0 0))",
        "LINESTRING(8 5,12 5)",
        0,
        1
    );
}

template<>
template<>
void object::test<7>
()
{
    checkDistanceWithin(
        "LINESTRING EMPTY",
        "POINT EMPTY",
        geos::DoubleInfinity,
        0
    );
}

template<>
template<>
void object::test<8>
()
{
    checkDistanceWithin(
        "POINT EMPTY",
        "LINESTRING EMPTY",
        geos::DoubleInfinity,
        0
    );
}

template<>
template<>
void object::test<9>
()
{
    checkDistanceWithin(
        "POINT EMPTY",
        "POINT(0 0)",
        geos::DoubleInfinity,
        0
    );
}

template<>
template<>
void object::test<10>
()
{
    checkDistanceWithin(
        "LINESTRING(0 0, 10 0)",
        "POLYGON EMPTY",
        geos::DoubleInfinity,
        0
    );
}

} // namespace tut

