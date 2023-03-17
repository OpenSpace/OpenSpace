//
// Ported from JTS junit/algorithm/RobustLineIntersectorTest.java rev. 1.1

#include <tut/tut.hpp>
// geos
#include <geos/io/WKBReader.h>
#include <geos/io/WKTReader.h>
#include <geos/algorithm/LineIntersector.h>
#include <geos/algorithm/PointLocation.h>
#include <geos/algorithm/Orientation.h>
#include <geos/geom/PrecisionModel.h>
#include <geos/geom/GeometryFactory.h>
#include <geos/geom/Geometry.h> // required for use in unique_ptr
#include <geos/geom/LineString.h>
#include <geos/geom/Coordinate.h>
#include <geos/geom/Point.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/geom/CoordinateArraySequence.h>
// std
#include <sstream>
#include <string>
#include <memory>


using namespace geos::geom; //
using geos::algorithm::LineIntersector;
using geos::algorithm::PointLocation;
using geos::algorithm::Orientation;


namespace tut {
//
// Test Group
//

struct test_robustlineintersector_data {

    typedef std::unique_ptr<Geometry> GeomPtr;

    LineIntersector i;

};

typedef test_group<test_robustlineintersector_data> group;
typedef group::object object;

group test_robustlineintersector_group(
    "geos::algorithm::RobustLineIntersector");




//
// Test Cases
//

// 1 - test2Lines
template<>
template<>
void object::test<1>
()
{
    Coordinate p1(10, 10);
    Coordinate p2(20, 20);
    Coordinate q1(20, 10);
    Coordinate q2(10, 20);
    Coordinate x(15, 15);
    i.computeIntersection(p1, p2, q1, q2);

    ensure_equals(i.getIntersectionNum(), (unsigned int)LineIntersector::POINT_INTERSECTION);
    ensure_equals(i.getIntersectionNum(), 1UL);
    ensure_equals(i.getIntersection(0), x);
    ensure("isProper", i.isProper());
    ensure("hasIntersection", i.hasIntersection());
}

// 2 - testCollinear1
template<>
template<>
void object::test<2>
()
{
    Coordinate p1(10, 10);
    Coordinate p2(20, 10);
    Coordinate q1(22, 10);
    Coordinate q2(30, 10);
    i.computeIntersection(p1, p2, q1, q2);

    ensure_equals(i.getIntersectionNum(), LineIntersector::NO_INTERSECTION);
    ensure_equals(i.getIntersectionNum(), 0UL);
    ensure("!isProper", !i.isProper());
    ensure("!hasIntersection", !i.hasIntersection());
}

// 3 - testCollinear2
template<>
template<>
void object::test<3>
()
{
    Coordinate p1(10, 10);
    Coordinate p2(20, 10);
    Coordinate q1(20, 10);
    Coordinate q2(30, 10);
    i.computeIntersection(p1, p2, q1, q2);

    ensure_equals(i.getIntersectionNum(), LineIntersector::POINT_INTERSECTION);
    ensure_equals(i.getIntersectionNum(), 1UL);
    ensure("!isProper", !i.isProper());
    ensure("hasIntersection", i.hasIntersection());
}

// 4 - testCollinear3
template<>
template<>
void object::test<4>
()
{
    Coordinate p1(10, 10);
    Coordinate p2(20, 10);
    Coordinate q1(15, 10);
    Coordinate q2(30, 10);
    i.computeIntersection(p1, p2, q1, q2);

    ensure_equals(i.getIntersectionNum(), LineIntersector::COLLINEAR_INTERSECTION);
    ensure_equals(i.getIntersectionNum(), 2UL);
    ensure("!isProper", !i.isProper());
    ensure("hasIntersection", i.hasIntersection());
}

// 5 - testCollinear4
template<>
template<>
void object::test<5>
()
{
    Coordinate p1(10, 10);
    Coordinate p2(20, 10);
    Coordinate q1(10, 10);
    Coordinate q2(30, 10);
    i.computeIntersection(p1, p2, q1, q2);

    ensure_equals(i.getIntersectionNum(), LineIntersector::COLLINEAR_INTERSECTION);
    ensure_equals(i.getIntersectionNum(), 2UL);
    ensure("!isProper", !i.isProper());
    ensure("hasIntersection", i.hasIntersection());
}

// 6 - testEndpointIntersection
template<>
template<>
void object::test<6>
()
{
    i.computeIntersection(Coordinate(100, 100), Coordinate(10, 100),
                          Coordinate(100, 10), Coordinate(100, 100));
    ensure("hasIntersection", i.hasIntersection());
    ensure_equals(i.getIntersectionNum(), 1UL);
}

// 7 - testEndpointIntersection2
template<>
template<>
void object::test<7>
()
{
    i.computeIntersection(Coordinate(190, 50), Coordinate(120, 100),
                          Coordinate(120, 100), Coordinate(50, 150));
    ensure("hasIntersection", i.hasIntersection());
    ensure_equals(i.getIntersectionNum(), 1UL);
    ensure_equals(i.getIntersection(1), Coordinate(120, 100));
}

// 8 - testOverlap
template<>
template<>
void object::test<8>
()
{
    i.computeIntersection(Coordinate(180, 200), Coordinate(160, 180),
                          Coordinate(220, 240), Coordinate(140, 160));
    ensure("hasIntersection", i.hasIntersection());
    ensure_equals(i.getIntersectionNum(), 2UL);
}

// 9 - testIsProper1
template<>
template<>
void object::test<9>
()
{
    i.computeIntersection(Coordinate(30, 10), Coordinate(30, 30),
                          Coordinate(10, 10), Coordinate(90, 11));
    ensure("hasIntersection", i.hasIntersection());
    ensure_equals(i.getIntersectionNum(), 1UL);
    ensure("isProper", i.isProper());
}

// 10 - testIsProper2
template<>
template<>
void object::test<10>
()
{
    i.computeIntersection(Coordinate(10, 30), Coordinate(10, 0),
                          Coordinate(11, 90), Coordinate(10, 10));
    ensure("hasIntersection", i.hasIntersection());
    ensure_equals(i.getIntersectionNum(), 1UL);
    ensure("!isProper", !i.isProper());
}

// 11 - testIsCCW
template<>
template<>
void object::test<11>
()
{
    ensure_equals(Orientation::index(
                      Coordinate(-123456789, -40),
                      Coordinate(0, 0),
                      Coordinate(381039468754763.0, 123456789)), 1);
}

// 12 - testIsCCW2
template<>
template<>
void object::test<12>
()
{
    ensure_equals(Orientation::index(
                      Coordinate(10, 10),
                      Coordinate(20, 20),
                      Coordinate(0, 0)), 0);
}

// 13 - testA
template<>
template<>
void object::test<13>
()
{
    Coordinate p1(-123456789, -40);
    Coordinate p2(381039468754763.0, 123456789);
    Coordinate q(0, 0);

    using geos::geom::CoordinateSequence;
    using geos::geom::GeometryFactory;
    using geos::geom::LineString;

    GeometryFactory::Ptr factory = GeometryFactory::create();
    CoordinateArraySequence* cs = new CoordinateArraySequence();
    cs->add(p1);
    cs->add(p2);

    GeomPtr l(factory->createLineString(cs));
    GeomPtr p(factory->createPoint(q));
    ensure(!l->intersects(p.get()));

    ensure(!PointLocation::isOnLine(q, cs));
    ensure_equals(Orientation::index(p1, p2, q), -1);

}

// Test intersects: point on segment with FLOAT PM
// X coordinate of 3rd and 4th vertices of the line are not
// float-point exact with X coordinate of the point.
// The X values differ after 14th decimal place:
// POINT (-23.1094689600055080 50.5195368635957180)
// --------------------^^^^^^^------------^^^^^^^^
// LINESTRING 3rd and 4th points
//        -23.1094689600055150 50.5223376452201340,
//        -23.1094689600055010 50.5169177629559480,
// --------------------^^^^^^^------------^^^^^^^^
// So, in float-point precision model, the point does DOES NOT intersect the segment.
template<>
template<>
void object::test<14>
()
{
    geos::io::WKBReader reader;

    // POINT located between 3rd and 4th vertex of LINESTRING
    // POINT(-23.1094689600055080 50.5195368635957180)
    std::string point("01010000009a266328061c37c0e21a172f80424940");
    // LINESTRING(-23.122057005539 50.5201976774794,-23.1153476966995 50.5133404815199,-23.1094689600055150 50.5223376452201340,-23.1094689600055010 50.5169177629559480,-23.0961967920942 50.5330464848094,-23.0887991006034 50.5258515213185,-23.0852302622362 50.5264582238409)
    std::string
    line("0102000000070000009909bf203f1f37c05c1d66d6954249404afe386d871d37c0a7eb1124b54149409c266328061c37c056d8bff5db42494098266328061c37c0034f7b5c2a42494060065c5aa01837c08ac001de3a4449408401b189bb1637c0b04e471a4f43494014ef84a6d11537c0b20dabfb62434940");
    std::stringstream sPoint(point);
    GeomPtr gPoint(reader.readHEX(sPoint));
    std::stringstream sLine(line);
    GeomPtr gLine(reader.readHEX(sLine));
    int ret = gLine->intersects(gPoint.get());
    ensure_equals(ret, 0);
}

// Test intersects: point on segment with FIXED PM
// X coordinate of 3rd and 4th vertices of the line are not
// float-point exact with X coordinate of the point.
// The X values differ after 14th decimal place:
// POINT (-23.1094689600055080 50.5195368635957180)
// --------------------^^^^^^^------------^^^^^^^^
// LINESTRING 3rd and 4th points
//        -23.1094689600055150 50.5223376452201340,
//        -23.1094689600055010 50.5169177629559480,
// --------------------^^^^^^^------------^^^^^^^^
// So, if float-point values are trimmed up to 14 decimal digits, the point DOES intersect the segment.

template<>
template<>
void object::test<15>
()
{
    using geos::geom::GeometryFactory;
    geos::geom::PrecisionModel pm(1e+13);
    GeometryFactory::Ptr factory = GeometryFactory::create(&pm);
    geos::io::WKBReader reader(*factory);

    // POINT located between 3rd and 4th vertex of LINESTRING
    // POINT(-23.1094689600055080 50.5195368635957180)
    std::string point("01010000009a266328061c37c0e21a172f80424940");
    // LINESTRING(-23.122057005539 50.5201976774794,-23.1153476966995 50.5133404815199,-23.1094689600055150 50.5223376452201340,-23.1094689600055010 50.5169177629559480,-23.0961967920942 50.5330464848094,-23.0887991006034 50.5258515213185,-23.0852302622362 50.5264582238409)
    std::string
    line("0102000000070000009909bf203f1f37c05c1d66d6954249404afe386d871d37c0a7eb1124b54149409c266328061c37c056d8bff5db42494098266328061c37c0034f7b5c2a42494060065c5aa01837c08ac001de3a4449408401b189bb1637c0b04e471a4f43494014ef84a6d11537c0b20dabfb62434940");
    std::stringstream sPoint(point);
    GeomPtr gPoint(reader.readHEX(sPoint));
    std::stringstream sLine(line);
    GeomPtr gLine(reader.readHEX(sLine));
    int ret = gLine->intersects(gPoint.get());
    ensure_equals(ret, 1);
}

} // namespace tut

