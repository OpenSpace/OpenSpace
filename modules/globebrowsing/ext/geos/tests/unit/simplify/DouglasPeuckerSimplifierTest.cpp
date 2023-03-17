//
// Test Suite for geos::simplify::DouglasPeuckerSimplifierTest

#include <tut/tut.hpp>
#include <utility.h>
// geos
#include <geos/io/WKTReader.h>
#include <geos/io/WKTWriter.h>
#include <geos/geom/PrecisionModel.h>
#include <geos/geom/GeometryFactory.h>
#include <geos/geom/Geometry.h>
#include <geos/geom/Coordinate.h>
#include <geos/geom/CoordinateSequenceFilter.h>
#include <geos/simplify/DouglasPeuckerSimplifier.h>
#include <geos/util.h>
// std
#include <string>
#include <memory>

namespace tut {
using namespace geos::simplify;

//
// Test Group
//

// Common data used by tests
struct test_dpsimp_data {
    geos::io::WKTReader wktreader;
    geos::io::WKTWriter wktwriter;

    typedef geos::geom::Geometry::Ptr GeomPtr;

    test_dpsimp_data()
        :
        wktreader()
    {}
};

typedef test_group<test_dpsimp_data> group;
typedef group::object object;

group test_dpsimp_group("geos::simplify::DouglasPeuckerSimplifier");

//
// Test Cases
//

// 1 - PolygonNoReduction
template<>
template<>
void object::test<1>
()
{
    std::string wkt("POLYGON((20 220, 40 220, 60 220, 80 220, 100 220, \
					120 220, 140 220, 140 180, 100 180, 60 180, 20 180, 20 220))");

    GeomPtr g(wktreader.read(wkt));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    // topology is unchanged
    ensure(simplified->equals(g.get()));
}

// 2 - PolygonReductionWithSplit
template<>
template<>
void object::test<2>
()
{
    std::string wkt_in("POLYGON ((40 240, 160 241, 280 240, 280 160, \
					160 240, 40 140, 40 240))");

    std::string wkt_ex("MULTIPOLYGON (((40.0 240.0, 160.0 240.0, 40.0 140.0, 40.0 240.0)), \
					((160.0 240.0, 280.0 240.0, 280.0 160.0, 160.0 240.0)))");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    // TODO: This test blows because if instability of geos.index.strtree::yComparator() predicate

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));

}

// 3 - PolygonReduction
template<>
template<>
void object::test<3>
()
{
    std::string wkt_in("POLYGON ((120 120, 121 121, 122 122, 220 120, \
					180 199, 160 200, 140 199, 120 120))");

    std::string wkt_ex("POLYGON ((120 120, 220 120, 180 199, 160 200, 140 199, 120 120))");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));

}

// 4 - PolygonWithTouchingHole
template<>
template<>
void object::test<4>
()
{
    std::string wkt_in("POLYGON ((80 200, 240 200, 240 60, 80 60, 80 200), \
					(120 120, 220 120, 180 199, 160 200, 140 199, 120 120))");

    std::string wkt_ex("POLYGON ((80 200, 240 200, 240 60, 80 60, 80 200), \
					(120 120, 220 120, 180 199, 160 200, 140 199, 120 120))");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));

}

// 5 - FlattishPolygon
template<>
template<>
void object::test<5>
()
{
    std::string wkt_in("POLYGON ((0 0, 50 0, 53 0, 55 0, 100 0, 70 1, 60 1, 50 1, 40 1, 0 0))");
    std::string wkt_ex("POLYGON EMPTY");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));
    //ensure_equals( *simplified, *expected );

}

// 6 - TinySquare
template<>
template<>
void object::test<6>
()
{
    std::string wkt_in("POLYGON ((0 5, 5 5, 5 0, 0 0, 0 1, 0 5))");
    std::string wkt_ex("POLYGON EMPTY");


    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));

}

// TinyHole
template<>
template<>
void object::test<7>
()
{
    std::string wkt_in("POLYGON ((10 10, 10 310, 370 310, 370 10, 10 10), (160 190, 180 190, 180 170, 160 190))");
    std::string wkt_ex("POLYGON ((10 10, 10 310, 370 310, 370 10, 10 10))");

    GeomPtr g(wktreader.read(wkt_in));
    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 30.0);

    ensure(simplified->isValid());
    ensure(simplified->equalsExact(expected.get()));
}

// 7 - TinyLineString
template<>
template<>
void object::test<8>
()
{
    std::string wkt_in("LINESTRING (0 5, 1 5, 2 5, 5 5)");
    std::string wkt_ex("LINESTRING (0 5, 5 5)");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));

}

// 8 - MultiPoint
template<>
template<>
void object::test<9>
()
{
    std::string wkt_in("MULTIPOINT(80 200, 240 200, 240 60, 80 60, 80 200, 140 199, 120 120)");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    // MultiPoint is *not* simplified
    ensure(simplified->equalsExact(g.get()));
}

// 9 - MultiLineString
template<>
template<>
void object::test<10>
()
{
    std::string wkt_in("MULTILINESTRING( (0 0, 50 0, 70 0, 80 0, 100 0), \
					(0 0, 50 1, 60 1, 100 0) )");

    std::string wkt_ex("MULTILINESTRING( (0 0, 100 0), (0 0, 100 0) )");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));
}

// 10 - GeometryCollection
template<>
template<>
void object::test<11>
()
{
    std::string wkt_in("GEOMETRYCOLLECTION ( \
					MULTIPOINT (80 200, 240 200, 240 60, 80 60, 80 200, 140 199, 120 120), \
					POLYGON ((80 200, 240 200, 240 60, 80 60, 80 200)), \
					LINESTRING (80 200, 240 200, 240 60, 80 60, 80 200, 140 199, 120 120) )");

    std::string wkt_ex("MULTILINESTRING( (0 0, 100 0), (0 0, 100 0) )");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 10.0);

    ensure(simplified->isValid());

    // Non simplification occurs
    ensure(simplified->equalsExact(g.get()));
}

// 11 - A kind of reversed simplification
template<>
template<>
void object::test<12>
()
{
    using namespace geos::geom;

    std::string
    wkt("MULTIPOLYGON(((0.561648 1,1 1,1 0,0.468083 0,0.52758 0.00800554,0.599683 0.0280924, 0.601611 0.265374, \
                        0.622693 0.316765,0.69507 0.357497,0.695623 0.429711,0.655111 0.502298, 0.696467 0.543147,0.840712 0.593546, \
                        0.882583 0.66546,0.852357 0.748213,0.84264 0.789567,0 .832667 0.841202,0.832667 0.841202,0.740538 0.873004, \
                        0.617349 0.905045,0.566576 0.977697,0.561648 1)),((0 0.801979,0.0308575 0.786234,0.0705513 0.631135, \
                        0.141616 0.527248,0.233985 0.505872,0.264777 0.526263,0.336631 0.505009,0.356603 0.422321,0.355803 0.350038, \
                        0.375252 0.205364,0.415206 0.0709182,0.45479 0,0 0,0 0,0 0.801979)))");

    GeomPtr g(wktreader.read(wkt));
    std::size_t const gN = g->getNumPoints();
    ensure_equals(gN, std::size_t(37));

    // 1) Simplify with 1/2048
    double const d1 = 1 / 2048.0;
    GeomPtr simplified1 = DouglasPeuckerSimplifier::simplify(g.get(), d1);
    ensure(simplified1->isValid());
    ensure(simplified1->equals(g.get()));
    std::size_t const simplifiedN1 = simplified1->getNumPoints();
    ensure_equals(simplifiedN1, std::size_t(36));
    //std::string const simplifiedWkd = wktwriter.write(simplified1.get());

    // 2) Multiply points by 2047
    struct Multiplier : public CoordinateSequenceFilter {
        double f;
        Multiplier(double p_f) : f(p_f) {}
        void
        filter_rw(CoordinateSequence& seq, std::size_t i) override
        {
            seq.setOrdinate(i, CoordinateSequence::X, seq[i].x * f);
            seq.setOrdinate(i, CoordinateSequence::Y, seq[i].y * f);
        }
        void
        filter_ro(const CoordinateSequence& seq, std::size_t i) override
        {
            ::geos::ignore_unused_variable_warning(seq);
            ::geos::ignore_unused_variable_warning(i);
        }
        bool
        isDone() const override
        {
            return false;
        }
        bool
        isGeometryChanged() const override
        {
            return true;
        }
    };

    Multiplier m(2047);
    g->apply_rw(m);
    std::size_t const multipliedN = g->getNumPoints();
    ensure_equals(multipliedN, std::size_t(37));
    //std::string const multipliedWkt = wktwriter.write(g.get());

    // 3) Simplify with 1.0
    double const d2 = 1.0;
    GeomPtr simplified2 = DouglasPeuckerSimplifier::simplify(g.get(), d2);
    ensure(simplified2->isValid());
    ensure(simplified2->equals(g.get()));
    std::size_t const simplifiedN2 = simplified2->getNumPoints();
    ensure_equals(simplifiedN2, std::size_t(36));
    //std::string const simplifiedWkt2 = wktwriter.write(simplified2.get());
}

// 13 - Polygon with inner ring whose extent is less than the simplify distance (#741)
template<>
template<>
void object::test<13>
()
{
    std::string wkt_in("POLYGON ((0 0,0 1,1 1,0 0),(0.1 0.1,0.2 0.1,0.2 0.2,0.1 0.1))");

    std::string wkt_ex("POLYGON ((0 0,0 1,1 1,0 0))");

    GeomPtr g(wktreader.read(wkt_in));

    GeomPtr expected(wktreader.read(wkt_ex));

    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(
                             g.get(), 0.5);

    ensure(simplified->isValid());

    ensure(simplified->equalsExact(expected.get()));
}

/**
* Test that a polygon made invalid by simplification
* is fixed in a sensible way.
* Fixed by buffer(0) area-base orientation
* See https://github.com/locationtech/jts/issues/498
*/
template<>
template<>
void object::test<14>
()
{
    std::string wkt_in("POLYGON ((21.32686 47.78723, 21.32386 47.79023, 21.32186 47.80223, 21.31486 47.81023, 21.32786 47.81123, 21.33986 47.80223, 21.33886 47.81123, 21.32686 47.82023, 21.32586 47.82723, 21.32786 47.82323, 21.33886 47.82623, 21.34186 47.82123, 21.36386 47.82223, 21.40686 47.81723, 21.32686 47.78723))");
    std::string wkt_ex("POLYGON ((21.32686 47.78723, 21.31486 47.81023, 21.32786 47.81123, 21.33986 47.80223, 21.328068201892744 47.823286782334385, 21.33886 47.82623, 21.34186 47.82123, 21.40686 47.81723, 21.32686 47.78723))");
    GeomPtr g(wktreader.read(wkt_in));
    GeomPtr expected(wktreader.read(wkt_ex));
    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(g.get(), 0.0036);
    ensure(simplified->isValid());
    ensure_equals_geometry(simplified.get(), expected.get());
}

  /**
   * Test that a collapsed polygon is removed.
   * Tests regression caused by unported JTS code.
   *
   * See https://trac.osgeo.org/geos/ticket/1115
   */
template<>
template<>
void object::test<15>
()
{
    std::string wkt_in("MULTIPOLYGON (((-76.02716827 36.55671692, -75.99866486 36.55665207, -75.91191864 36.54253006, -75.92480469 36.47397614, -75.97727966 36.4780159, -75.97628784 36.51792526, -76.02716827 36.55671692)), ((-75.90198517 36.55619812, -75.8781662 36.55587387, -75.77315521 36.22925568, -75.78317261 36.22519302, -75.90198517 36.55619812)))");
    std::string wkt_ex("POLYGON ((-76.02716827 36.55671692, -75.91191864 36.54253006, -75.92480469 36.47397614, -76.02716827 36.55671692))");
    GeomPtr g(wktreader.read(wkt_in));
    GeomPtr expected(wktreader.read(wkt_ex));
    GeomPtr simplified = DouglasPeuckerSimplifier::simplify(g.get(), 0.05);
    ensure(simplified->isValid());
    ensure_equals_geometry(simplified.get(), expected.get());
}


} // namespace tut
