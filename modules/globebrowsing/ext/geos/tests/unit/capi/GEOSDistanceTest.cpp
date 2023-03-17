//
// Test Suite for C-API GEOSDistance

#include <tut/tut.hpp>
// geos
#include <geos_c.h>
#include <geos/constants.h>
// std
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <fenv.h>
#include <memory>
#include <cmath>

#include "capi_test_utils.h"

namespace tut {
//
// Test Group
//

// Common data used in test cases.
struct test_capigeosdistance_data : public capitest::utility {
    test_capigeosdistance_data() {
        GEOSWKTWriter_setTrim(wktw_, 1);
    }
};

typedef test_group<test_capigeosdistance_data> group;
typedef group::object object;

group test_capigeosdistance_group("capi::GEOSDistance");

//
// Test Cases
//

/// See http://trac.osgeo.org/geos/ticket/377
template<>
template<>
void object::test<1>
()
{
    geom1_ = GEOSGeomFromWKT("POINT(10 10)");
    geom2_ = GEOSGeomFromWKT("POINT(3 6)");

    double dist;
    int ret = GEOSDistance(geom1_, geom2_, &dist);

    ensure_equals(ret, 1);
    ensure_distance(dist, 8.06225774829855, 1e-12);
}

GEOSGeometry*
random_polygon(double x, double y, double r, std::size_t num_points)
{
    std::vector<double> angle(num_points);
    std::vector<double> radius(num_points);


    for(std::size_t i = 0; i < num_points; i++) {
        angle[i] = 2 * geos::MATH_PI * std::rand() / RAND_MAX;
        radius[i] = r * std::rand() / RAND_MAX;
    }

    std::sort(angle.begin(), angle.end());

    GEOSCoordSequence* seq_1 = GEOSCoordSeq_create(static_cast<unsigned int>(num_points), 2);
    for(unsigned int i = 0; i < num_points; i++) {
        auto idx = i == (num_points - 1) ? 0 : i;

        GEOSCoordSeq_setX(seq_1, i, x + radius[idx] * cos(angle[idx]));
        GEOSCoordSeq_setY(seq_1, i, y + radius[idx] * sin(angle[idx]));
    }

    return GEOSGeom_createPolygon(GEOSGeom_createLinearRing(seq_1), nullptr, 0);
}

/* Generate two complex polygons and verify that GEOSDistance and GEOSDistanceIndexed
 * return identical results.
 */
template<>
template<>
void object::test<2>
()
{
    std::srand(12345);

    GEOSGeometry* g1 = random_polygon(-3, -8, 7, 1000);
    GEOSGeometry* g2 = random_polygon(14, 22, 6, 500);

    double d_raw, d_indexed;
    ensure(GEOSDistance(g1, g2, &d_raw) != 0);
    ensure(GEOSDistanceIndexed(g1, g2, &d_indexed) != 0);

    ensure_equals(d_indexed, d_raw);

    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);
}

// https://github.com/libgeos/geos/issues/295
template<>
template<>
void object::test<3>
()
{
    GEOSGeometry* g1 = GEOSGeomFromWKT("MultiPolygon Z (EMPTY,((-0.14000000000000001 44.89999999999999858 0, -0.14699999999999999 44.90400000000000347 0, -0.14729999999999999 44.90500000000000114 0, -0.14000000000000001 44.89999999999999858 0)))");
    GEOSGeometry* g2 = GEOSGeomFromWKT("POLYGON ((0 0, 1 0, 1 1, 0 0))");

    double d;
    int status = GEOSDistance(g1, g2, &d);

    ensure_equals(status, 1);

    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);
}

// point distance does not raise floating point exception
template<>
template<>
void object::test<4>
()
{
    GEOSGeometry* g1 = GEOSGeomFromWKT("POINT (0 0)");
    GEOSGeometry* g2 = GEOSGeomFromWKT("POINT (1 1)");

    // clear all floating point exceptions
    feclearexcept (FE_ALL_EXCEPT);

    double d;
    int status = GEOSDistance(g1, g2, &d);

    ensure_equals(status, 1);
    ensure_equals(d, std::sqrt(2));

    // check for floating point overflow exceptions
    int raised = fetestexcept(FE_OVERFLOW);
    ensure_equals(raised & FE_OVERFLOW, 0);

    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);
}

// same distance between boundables should not raise floating point exception
template<>
template<>
void object::test<5>
()
{
    GEOSGeometry* g1 = GEOSGeomFromWKT("LINESTRING (0 0, 1 1)");
    GEOSGeometry* g2 = GEOSGeomFromWKT("LINESTRING (2 1, 1 2)");

    // clear all floating point exceptions
    feclearexcept (FE_ALL_EXCEPT);

    double d;
    int status = GEOSDistance(g1, g2, &d);

    ensure_equals(status, 1);
    // ensure_equals(d, std::sqrt(2));

    // check for floating point overflow exceptions
    int raised = fetestexcept(FE_OVERFLOW);
    ensure_equals(raised & FE_OVERFLOW, 0);

    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);
}

} // namespace tut

