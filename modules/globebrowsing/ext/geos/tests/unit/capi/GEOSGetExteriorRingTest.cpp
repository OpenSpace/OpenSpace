#include <tut/tut.hpp>
// geos
#include <geos_c.h>

#include "capi_test_utils.h"

namespace tut
{
    //
    // Test Group
    //

    struct test_geosgetexteriorring_data : public capitest::utility
    {
    };

    typedef test_group<test_geosgetexteriorring_data> group;
    typedef group::object object;

    group test_geosgetexteriorring("capi::GEOSGetExteriorRing");

    template <>
    template <>
    void object::test<1>()
    {
        geom1_ = fromWKT("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))");
        ensure(nullptr != geom1_);

        GEOSGeometry *result = const_cast<GEOSGeometry *>(GEOSGetExteriorRing(geom1_));
        ensure(nullptr != result);
        ensure_equals("LINEARRING (35 10, 45 45, 15 40, 10 20, 35 10)", toWKT(result));
    }

} // namespace tut
