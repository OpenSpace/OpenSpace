//
// Test Suite for geos::algorithm::hull::ConcaveHull

#include <tut/tut.hpp>
// geos
#include <geos/algorithm/hull/ConcaveHull.h>
#include <geos/geom/Geometry.h>
#include <geos/io/WKTReader.h>
#include <geos/io/WKTWriter.h>
#include <geos/constants.h>
#include <utility.h>
// std
#include <sstream>
#include <string>
#include <memory>

using geos::algorithm::hull::ConcaveHull;
using geos::io::WKTReader;
using geos::io::WKTWriter;
using geos::geom::Geometry;

namespace tut {
//
// Test Group
//

// dummy data, not used
struct test_concavehull_data {
    WKTReader reader_;

    test_concavehull_data() {};

    void
    checkHullByLengthRatio(const std::string& wkt, double threshold, const std::string& wktExpected)
    {
        std::unique_ptr<Geometry> geom = reader_.read(wkt);
        std::unique_ptr<Geometry> actual = ConcaveHull::concaveHullByLengthRatio(geom.get(), threshold);
        std::unique_ptr<Geometry> expected = reader_.read(wktExpected);
        ensure_equals_geometry(expected.get(), actual.get());
    }

    void
    checkHullByLengthRatioXYZ(const std::string& wkt, double threshold, const std::string& wktExpected)
    {
        std::unique_ptr<Geometry> geom = reader_.read(wkt);
        std::unique_ptr<Geometry> actual = ConcaveHull::concaveHullByLengthRatio(geom.get(), threshold);
        std::unique_ptr<Geometry> expected = reader_.read(wktExpected);
        ensure_equals_geometry(expected.get(), actual.get());
    }

    void
    checkHullByLength(const std::string& wkt, double threshold, const std::string& wktExpected)
    {
        std::unique_ptr<Geometry> geom = reader_.read(wkt);
        std::unique_ptr<Geometry> actual = ConcaveHull::concaveHullByLength(geom.get(), threshold);
        std::unique_ptr<Geometry> expected = reader_.read(wktExpected);
        ensure_equals_geometry(expected.get(), actual.get());
    }

    void
    checkHullWithHolesByLength(const std::string& wkt, double threshold, const std::string& wktExpected)
    {
        std::unique_ptr<Geometry> geom = reader_.read(wkt);
        std::unique_ptr<Geometry> actual = ConcaveHull::concaveHullByLength(geom.get(), threshold, true);
        std::unique_ptr<Geometry> expected = reader_.read(wktExpected);
        // std::cout << "ACTUAL" << std::endl << actual->toText() << std::endl;
        // std::cout << "EXPECT" << std::endl << expected->toText() << std::endl;
        ensure_equals_geometry(expected.get(), actual.get());
    }

};

typedef test_group<test_concavehull_data> group;
typedef group::object object;

group test_concavehull_group("geos::algorithm::hull::ConcaveHull");

//
// testLengthEmpty
//
template<>
template<>
void object::test<1>()
{
    checkHullByLength("MULTIPOINT EMPTY",
        70, "POLYGON EMPTY");
}

//
// testLengthPoint
//
template<>
template<>
void object::test<2>()
{
    checkHullByLength("MULTIPOINT ((10 10), (10 10))",
        70, "POINT (10 10)" );
}

//
// testLengthCollinear
//
template<>
template<>
void object::test<3>()
{
    checkHullByLength("LINESTRING (10 10, 20 20, 30 30))",
        70, "LINESTRING (10 10, 30 30)" );
}

//
// testLengthTriangle
//
template<>
template<>
void object::test<4>()
{
    checkHullByLength("MULTIPOINT ((10 10), (90 10), (30 70))",
        70, "POLYGON ((10 10, 30 70, 90 10, 10 10))" );
}

//
// testLengthChevron
//
template<>
template<>
void object::test<5>()
{
    checkHullByLength("MULTIPOINT ((10 10), (90 10), (30 70), (70 70), (50 60))",
        70, "POLYGON ((30 70, 70 70, 90 10, 50 60, 10 10, 30 70))" );
}

//
// testLengthZero
//
template<>
template<>
void object::test<6>()
{
    checkHullByLength("MULTIPOINT ((10 10), (90 10), (70 70), (50 60), (50 90), (40 70), (30 30))",
        0, "POLYGON ((10 10, 40 70, 50 90, 70 70, 90 10, 50 60, 30 30, 10 10))" );
}

//
// testLengthConvex
//
template<>
template<>
void object::test<7>()
{
    checkHullByLength("MULTIPOINT ((10 10), (90 10), (70 70), (50 60), (50 90), (40 70), (30 30))",
        100, "POLYGON ((10 10, 40 70, 50 90, 70 70, 90 10, 10 10))" );
}

//
// testLenRatioCShape
//
template<>
template<>
void object::test<8>()
{
    checkHullByLengthRatio("MULTIPOINT ((70 80), (80 90), (90 70), (50 80), (30 70), (20 40), (30 20), (50 10), (90 20), (40 50), (40 30), (41 67))",
        0.2, "POLYGON ((20 40, 30 70, 50 80, 80 90, 90 70, 70 80, 41 67, 40 50, 40 30, 90 20, 50 10, 30 20, 20 40))" );
}

//
// testLenRatioSShape
//
template<>
template<>
void object::test<9>()
{
    checkHullByLengthRatio("MULTIPOINT ((0 81), (65 86), (70 71), (80 59), (92 49), (107 44), (122 41), (137 40), (152 41), (167 42), (182 47), (195 55), (203 68), (201 83), (188 92), (173 97), (158 100), (143 103), (128 106), (113 109), (98 112), (83 115), (68 120), (53 125), (40 133), (28 143), (18 155), (13 170), (12 185), (16 200), (26 213), (38 223), (51 231), (66 236), (81 240), (96 243), (111 245), (126 245), (141 245), (156 245), (171 244), (186 241), (201 238), (216 233), (229 225), (242 216), (252 204), (259 190), (262 175), (194 171), (189 186), (178 197), (164 203), (149 205), (134 206), (119 205), (104 203), (89 198), (77 188), (80 173), (93 165), (108 160), (123 157), (138 154), (153 151), (168 149), (183 146), (198 142), (213 138), (227 132), (241 126), (253 116), (263 104), (269 90), (271 75), (270 60), (264 46), (254 34), (243 23), (229 16), (215 10), (200 6), (185 3), (170 1), (155 0), (139 0), (123 0), (108 1), (93 3), (78 5), (63 10), (49 16), (35 23), (23 33), (13 45), (6 59), (16 82), (32 83), (48 84), (245 174), (228 173), (211 172), (131 128), (63 148), (222 207), (127 230), (154 131), (240 82), (72 220), (210 32), (90 22), (206 208), (57 202), (195 117), (55 166), (246 55), (201 101), (224 73), (211 192), (42 176), (152 228), (172 113), (24 61), (76 33), (92 216), (46 69), (118 138), (169 23), (213 118), (221 56), (44 192), (118 22), (224 40), (56 57), (192 32), (179 220), (34 44), (145 18), (239 194), (40 155), (92 136), (231 106), (40 207), (108 228), (256 81), (28 185), (54 33), (74 205), (172 132), (221 93), (249 96), (69 47), (78 146), (155 115), (202 223))",
        0.1, "POLYGON ((16 200, 26 213, 38 223, 51 231, 66 236, 81 240, 96 243, 111 245, 126 245, 141 245, 156 245, 171 244, 186 241, 201 238, 216 233, 229 225, 242 216, 252 204, 259 190, 262 175, 245 174, 228 173, 211 172, 194 171, 189 186, 178 197, 164 203, 149 205, 134 206, 119 205, 104 203, 89 198, 77 188, 80 173, 93 165, 108 160, 123 157, 138 154, 153 151, 168 149, 183 146, 198 142, 213 138, 227 132, 241 126, 253 116, 263 104, 269 90, 271 75, 270 60, 264 46, 254 34, 243 23, 229 16, 215 10, 200 6, 185 3, 170 1, 155 0, 139 0, 123 0, 108 1, 93 3, 78 5, 63 10, 49 16, 35 23, 23 33, 13 45, 6 59, 0 81, 16 82, 32 83, 48 84, 65 86, 70 71, 80 59, 92 49, 107 44, 122 41, 137 40, 152 41, 167 42, 182 47, 195 55, 203 68, 201 83, 188 92, 173 97, 158 100, 143 103, 128 106, 113 109, 98 112, 83 115, 68 120, 53 125, 40 133, 28 143, 18 155, 13 170, 12 185, 16 200))" );
}

//------------------------------------------------
//
// testLenRatioZero
//
template<>
template<>
void object::test<10>()
{
    checkHullByLengthRatio("MULTIPOINT ((10 90), (10 10), (90 10), (90 90), (40 40), (60 30), (30 70), (40 60), (60 50), (60 72), (47 66), (90 60))",
        0, "POLYGON ((30 70, 10 90, 60 72, 90 90, 90 60, 90 10, 60 30, 10 10, 40 40, 60 50, 47 66, 40 60, 30 70))" );
}

//
// testLenRatioP5
//
template<>
template<>
void object::test<11>()
{
    checkHullByLengthRatio("MULTIPOINT ((10 90), (10 10), (90 10), (90 90), (40 40), (60 30), (30 70), (40 60), (60 50), (60 72), (47 66), (90 60))",
        0.5, "POLYGON ((30 70, 10 90, 60 72, 90 90, 90 60, 90 10, 60 30, 10 10, 40 40, 30 70))" );
}

//
// testLenRatioOne
//
template<>
template<>
void object::test<12>()
{
    checkHullByLengthRatio("MULTIPOINT ((10 90), (10 10), (90 10), (90 90), (40 40), (60 30), (30 70), (40 60), (60 50), (60 72), (47 66), (90 60))",
       1, "POLYGON ((10 10, 10 90, 90 90, 90 60, 90 10, 10 10))" );
}

//
// testLenRatioXYZChevronP5
//
template<>
template<>
void object::test<13>()
{
    checkHullByLengthRatioXYZ("MULTIPOINT Z ((10 10 1), (90 10 2), (30 70 3), (70 70 4), (50 60 5))",
       0.5, "POLYGON Z ((30 70 3, 70 70 4, 90 10 2, 50 60 5, 10 10 1, 30 70 3))" );
}


//------------------------------------------------
//
// testLengthHolesCircle
//
template<>
template<>
void object::test<14>()
{
    checkHullWithHolesByLength("MULTIPOINT ((90 20), (80 10), (45 5), (10 20), (20 10), (21 30), (40 20), (11 60), (20 70), (20 90), (40 80), (70 80), (80 60), (90 70), (80 90), (56 95), (95 45), (80 40), (70 20), (15 45), (5 40), (40 96), (60 15))",
       40, "POLYGON ((20 90, 40 96, 56 95, 80 90, 90 70, 95 45, 90 20, 80 10, 45 5, 20 10, 10 20, 5 40, 11 60, 20 90), (20 70, 15 45, 40 20, 70 20, 80 40, 80 60, 70 80, 40 80, 20 70))" );
}

//
// testLengthHolesCircle0
//
template<>
template<>
void object::test<15>()
{
    checkHullWithHolesByLength("MULTIPOINT ((90 20), (80 10), (45 5), (10 20), (20 10), (21 30), (40 20), (11 60), (20 70), (20 90), (40 80), (70 80), (80 60), (90 70), (80 90), (56 95), (95 45), (80 40), (70 20), (15 45), (5 40), (40 96), (60 15))",
       0, "POLYGON ((20 90, 40 96, 56 95, 70 80, 80 90, 90 70, 80 60, 95 45, 80 40, 70 20, 90 20, 80 10, 60 15, 45 5, 40 20, 40 80, 15 45, 21 30, 20 10, 10 20, 5 40, 11 60, 20 70, 20 90))" );
}



} // namespace tut


