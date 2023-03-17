//
// Test Suite for geos::operation::valid::RepeatedPointRemover class.
//

// tut
#include <tut/tut.hpp>
#include <utility.h>

// geos
#include <geos/operation/valid/RepeatedPointRemover.h>
#include <geos/geom/Geometry.h>
#include <geos/geom/LineString.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/io/WKTReader.h>
#include <geos/io/WKTWriter.h>

// std
#include <memory>
#include <string>

using geos::geom::Geometry;
using geos::geom::LineString;
using geos::geom::CoordinateSequence;
using geos::operation::valid::RepeatedPointRemover;


namespace tut {
//
// Test Group
//

// Common data used by tests
struct test_repeated_point_remover_test_data
{
    geos::io::WKTReader reader;
    geos::io::WKTWriter writer;

    test_repeated_point_remover_test_data()
        : reader() {};

    std::string
    wkt(const Geometry& geom)
    {
        writer.setTrim(true);
        return writer.write(&geom);
    }

    void
    checkSequence(const std::string& input, const std::string& expected, double tolerance = 0.0)
    {
        std::unique_ptr<Geometry> inGeom = reader.read(input);
        std::unique_ptr<Geometry> exGeom = reader.read(expected);

        const CoordinateSequence* inCoords = static_cast<LineString*>(inGeom.get())->getCoordinatesRO();
        auto outCoords = RepeatedPointRemover::removeRepeatedPoints(inCoords, tolerance);
        std::unique_ptr<Geometry> outGeom = inGeom->getFactory()->createLineString(std::move(outCoords));
        // std::cout << std::endl << wkt(*outGeom) << std::endl;
        // std::cout << wkt(*exGeom) << std::endl;
        ensure_equals_geometry(outGeom.get(), exGeom.get());
    }

    void
    checkGeometry(const std::string& input, const std::string& expected, double tolerance = 0.0)
    {
        std::unique_ptr<Geometry> inGeom = reader.read(input);
        std::unique_ptr<Geometry> outGeom = RepeatedPointRemover::removeRepeatedPoints(inGeom.get(), tolerance);
        ensure(outGeom.get());

        std::unique_ptr<Geometry> exGeom = reader.read(expected);

        // std::cout << std::endl << wkt(*outGeom) << std::endl;
        // std::cout << wkt(*exGeom) << std::endl;
        ensure_equals_geometry(outGeom.get(), exGeom.get());
    }


};

typedef test_group<test_repeated_point_remover_test_data> group;
typedef group::object object;

group test_repeated_point_remover_group("geos::operation::valid::RepeatedPointRemover");

template<>
template<>
void object::test<1>()
{
    checkSequence(
        "LINESTRING (3 7, 8 8, 8 8, 8 8, 10 9)",
        "LINESTRING (3 7, 8 8, 10 9)",
        0.0
        );
}

template<>
template<>
void object::test<2>()
{
    checkSequence(
        "LINESTRING (3 7, 8 8, 8 8, 8 8)",
        "LINESTRING (3 7, 8 8)",
        0.0
        );
}

template<>
template<>
void object::test<3>()
{
    checkSequence(
        "LINESTRING (0 0, 1 0, 4 0, 5 0)",
        "LINESTRING (0 0, 4 0)",
        3.0
        );
}

template<>
template<>
void object::test<4>()
{
    checkGeometry(
        "LINESTRING (0 0, 1 0, 4 0, 5 0)",
        "LINESTRING (0 0, 5 0)",
        3.0
        );
}


template<>
template<>
void object::test<5>()
{
    checkGeometry(
        "MULTIPOLYGON (((0 0, 9 0, 10 0, 10 10, 0 10, 0 1, 0 0)))",
        "MULTIPOLYGON (((0 0, 9 0, 10 10, 0 10, 0 0)))",
        3.0
        );
}



} // namespace tut


