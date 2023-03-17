/**********************************************************************
 *
 * GEOS - Geometry Engine Open Source
 * http://geos.osgeo.org
 *
 * Copyright (C) 2022 Martin Davis
 *
 * This is free software; you can redistribute and/or modify it under
 * the terms of the GNU Lesser General Public Licence as published
 * by the Free Software Foundation.
 * See the COPYING file for more information.
 *
 **********************************************************************/

// Test Suite for geos::operation::relate::RelateOp class

#include <tut/tut.hpp>
// geos
#include <geos/geom/Geometry.h>
#include <geos/io/WKTReader.h>
#include <geos/operation/relate/RelateOp.h>
// std
#include <cmath>
#include <string>
#include <memory>

using namespace geos::geom;
using namespace geos::operation::relate;

namespace tut {
//----------------------------------------------
// Test Group
//----------------------------------------------

struct test_relateop_data {
    geos::io::WKTReader wktreader;

    test_relateop_data()
    {}

    void checkRelate(const std::string& wkta, const std::string& wktb, const std::string& imExpected)
    {
        auto ga = wktreader.read(wkta);
        auto gb = wktreader.read(wktb);

        std::unique_ptr<IntersectionMatrix> im(ga->relate( gb.get() ));
        auto imActual = im->toString();
        ensure_equals(imExpected, imActual);
    }
};

typedef test_group<test_relateop_data> group;
typedef group::object object;

group test_relateop_group("geos::operation::relate::RelateOp");

//----------------------------------------------
// Test Cases
//----------------------------------------------

// 1 - test intersection of lines very close to a boundary endpoint
// See https://lists.osgeo.org/pipermail/postgis-users/2022-February/045266.html
//      https://github.com/locationtech/jts/pull/839
template<>
template<>
void object::test<1> ()
{
    checkRelate(
"LINESTRING (-29796.696826656284 138522.76848210802, -29804.3911369969 138519.3504205817)",
"LINESTRING (-29802.795222153436 138520.05937757515, -29802.23305474065 138518.7938969792)""FF10F0102",
    "F01FF0102"    );
}

// 2 - test intersection of lines very close to a boundary endpoint
// See https://lists.osgeo.org/pipermail/postgis-users/2022-February/045277.html
//      https://github.com/locationtech/jts/pull/839
template<>
template<>
void object::test<2> ()
{
    checkRelate(
"LINESTRING (-57.2681216 49.4063466, -57.267725199999994 49.406617499999996, -57.26747895046037 49.406750916517765)",
"LINESTRING (-57.267475399999995 49.4067465, -57.2675701 49.406864299999995, -57.267989 49.407135399999994)",
    "FF10F0102"    );
}

} // namespace tut
