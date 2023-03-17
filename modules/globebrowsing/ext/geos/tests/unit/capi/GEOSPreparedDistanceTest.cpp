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
struct test_capigeosprepareddistance_data : public capitest::utility {
    const GEOSPreparedGeometry* pgeom1_;

    test_capigeosprepareddistance_data()
        : pgeom1_(nullptr)
    {}

    ~test_capigeosprepareddistance_data()
    {
        GEOSPreparedGeom_destroy(pgeom1_);
    }

    void checkDistance(const char* wkt1, const char* wkt2,
                       double dist, double tol=0)
    {
        geom1_ = GEOSGeomFromWKT(wkt1);
        ensure(nullptr != geom1_);
        pgeom1_ = GEOSPrepare(geom1_);
        ensure(nullptr != pgeom1_);
        geom2_ = GEOSGeomFromWKT(wkt2);
        ensure(nullptr != geom2_);


        double obt_dist;
        int ret = GEOSPreparedDistance(pgeom1_, geom2_, &obt_dist);
        ensure_equals("return code", ret, 1);
        ensure_equals("distance", obt_dist, dist, tol);

    }


};

typedef test_group<test_capigeosprepareddistance_data> group;
typedef group::object object;

group test_capigeosprepareddistance_group("capi::GEOSPreparedDistance");

//
// Test Cases
//

template<>
template<>
void object::test<1>
()
{
    checkDistance(
        "POLYGON EMPTY",
        "POLYGON EMPTY",
        geos::DoubleInfinity
    );
}

template<>
template<>
void object::test<2>
()
{
    checkDistance(
        "POLYGON((1 1,1 5,5 5,5 1,1 1))",
        "POLYGON((8 8, 9 9, 9 10, 8 8))",
        4.242640687119285, 1e-12
    );

}

template<>
template<>
void object::test<3>
()
{
    checkDistance(
        "POLYGON((1 1,1 5,5 5,5 1,1 1))",
        "POINT(2 2)",
        0
    );
}

template<>
template<>
void object::test<4>
()
{
    checkDistance(
        "LINESTRING(1 5,5 5,5 1,1 1)",
        "POINT(2 2)",
        1
    );
}

template<>
template<>
void object::test<5>
()
{
    checkDistance(
        "LINESTRING(0 0,10 10)",
        "LINESTRING(0 10,10 0)",
        0
    );
}

template<>
template<>
void object::test<6>
()
{
    checkDistance(
        "POLYGON((0 0,10 0,10 10,0 10,0 0))",
        "LINESTRING(8 5,12 5)",
        0
    );
}

template<>
template<>
void object::test<7>
()
{
    checkDistance(
        "LINESTRING EMPTY",
        "POINT EMPTY",
        geos::DoubleInfinity
    );
}

template<>
template<>
void object::test<8>
()
{
    checkDistance(
        "POINT EMPTY",
        "LINESTRING EMPTY",
        geos::DoubleInfinity
    );
}

} // namespace tut

