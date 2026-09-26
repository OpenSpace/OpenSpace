/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <catch2/catch_test_macros.hpp>

#include <modules/space/kepler.h>
#include <ghoul/filesystem/filesystem.h>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

using namespace openspace::kepler;

namespace {
    Parameters v0 = {
        .name = "ATLAS CENTAUR 2",
        .id = "1963-047A",
        .inclination = 30.352800369262695,
        .semiMajorAxis = 7229.6502972150938,
        .ascendingNode = 304.75119018554688,
        .eccentricity = 0.054663788527250290,
        .argumentOfPeriapsis = 115.95459747314453,
        .meanAnomaly = 249.84049987792969,
        .epoch = 830224270.05232000,
        .period = 6117.7661132812500
    };
    Parameters v50 = {
        .name = "COSMOS 1953",
        .id = "1988-050A",
        .inclination = 82.505203247070312,
        .semiMajorAxis = 6856.3175825719800,
        .ascendingNode = 112.65910339355469,
        .eccentricity = 0.00096540001686662436,
        .argumentOfPeriapsis = 333.80349731445312,
        .meanAnomaly = 26.271499633789062,
        .epoch = 830259248.14105594,
        .period = 5650.0634765625000
    };
    Parameters v75 = {
        .name = "SL-8 R/B",
        .id = "1992-008B",
        .inclination = 82.927597045898438,
        .semiMajorAxis = 7356.2838711410686,
        .ascendingNode = 234.38470458984375,
        .eccentricity = 0.0034735701046884060,
        .argumentOfPeriapsis = 11.573499679565430,
        .meanAnomaly = 359.85360717773438,
        .epoch = 830224348.63657606,
        .period = 6279.2050781250000
    };
    Parameters v97 = {
        .name = "ISS (ZARYA)",
        .id = "1998-067A",
        .inclination = 51.631900787353516,
        .semiMajorAxis = 6797.9780886547951,
        .ascendingNode = 210.18159484863281,
        .eccentricity = 0.00068280001869425178,
        .argumentOfPeriapsis = 342.17788696289062,
        .meanAnomaly = 17.896900177001953,
        .epoch = 830227900.39974403,
        .period = 5578.1035156250000
    };
    Parameters v125 = {
        .name = "COSMOS 2428",
        .id = "2007-029A",
        .inclination = 70.939102172851562,
        .semiMajorAxis = 7227.3797636226582,
        .ascendingNode = 107.97290039062500,
        .eccentricity = 0.00091639999300241470,
        .argumentOfPeriapsis = 227.09060668945312,
        .meanAnomaly = 132.94470214843750,
        .epoch = 830271740.17014396,
        .period = 6114.8842773437500
    };
} // namespace

TEST_CASE("Kepler: TLE", "[kepler]") {
    const std::filesystem::path file = absPath("${TESTDIR}/kepler/tle-format.txt");

    std::vector<Parameters> res = readTleFile(file);
    REQUIRE(res.size() == 148);

    CHECK(res[0].name == v0.name);
    CHECK(res[0].id == "1963-47A"); // TLE has limited number of characters
    CHECK_THAT(res[0].inclination, Catch::Matchers::WithinRel(v0.inclination, 0.001));
    CHECK_THAT(res[0].semiMajorAxis, Catch::Matchers::WithinRel(v0.semiMajorAxis, 0.001));
    CHECK_THAT(res[0].ascendingNode, Catch::Matchers::WithinRel(v0.ascendingNode, 0.001));
    CHECK_THAT(res[0].eccentricity, Catch::Matchers::WithinRel(v0.eccentricity, 0.001));
    CHECK_THAT(
        res[0].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v0.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[0].meanAnomaly, Catch::Matchers::WithinRel(v0.meanAnomaly, 0.001));
    CHECK_THAT(res[0].epoch, Catch::Matchers::WithinRel(v0.epoch, 0.001));
    CHECK_THAT(res[0].period, Catch::Matchers::WithinRel(v0.period, 0.001));

    CHECK(res[50].name == v50.name);
    CHECK(res[50].id == "1988-50A"); // TLE has limited number of characters
    CHECK_THAT(res[50].inclination, Catch::Matchers::WithinRel(v50.inclination, 0.001));
    CHECK_THAT(
        res[50].semiMajorAxis,
        Catch::Matchers::WithinRel(v50.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[50].ascendingNode,
        Catch::Matchers::WithinRel(v50.ascendingNode, 0.001)
    );
    CHECK_THAT(res[50].eccentricity, Catch::Matchers::WithinRel(v50.eccentricity, 0.001));
    CHECK_THAT(
        res[50].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v50.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[50].meanAnomaly, Catch::Matchers::WithinRel(v50.meanAnomaly, 0.001));
    CHECK_THAT(res[50].epoch, Catch::Matchers::WithinRel(v50.epoch, 0.001));
    CHECK_THAT(res[50].period, Catch::Matchers::WithinRel(v50.period, 0.001));

    CHECK(res[75].name == v75.name);
    CHECK(res[75].id == "1992-08B"); // TLE has limited number of characters
    CHECK_THAT(res[75].inclination, Catch::Matchers::WithinRel(v75.inclination, 0.001));
    CHECK_THAT(
        res[75].semiMajorAxis,
        Catch::Matchers::WithinRel(v75.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[75].ascendingNode,
        Catch::Matchers::WithinRel(v75.ascendingNode, 0.001)
    );
    CHECK_THAT(res[75].eccentricity, Catch::Matchers::WithinRel(v75.eccentricity, 0.001));
    CHECK_THAT(
        res[75].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v75.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[75].meanAnomaly, Catch::Matchers::WithinRel(v75.meanAnomaly, 0.001));
    CHECK_THAT(res[75].epoch, Catch::Matchers::WithinRel(v75.epoch, 0.001));
    CHECK_THAT(res[75].period, Catch::Matchers::WithinRel(v75.period, 0.001));

    CHECK(res[97].name == v97.name);
    CHECK(res[97].id == "1998-67A"); // TLE has limited number of characters
    CHECK_THAT(res[97].inclination, Catch::Matchers::WithinRel(v97.inclination, 0.001));
    CHECK_THAT(
        res[97].semiMajorAxis,
        Catch::Matchers::WithinRel(v97.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[97].ascendingNode,
        Catch::Matchers::WithinRel(v97.ascendingNode, 0.001)
    );
    CHECK_THAT(res[97].eccentricity, Catch::Matchers::WithinRel(v97.eccentricity, 0.001));
    CHECK_THAT(
        res[97].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v97.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[97].meanAnomaly, Catch::Matchers::WithinRel(v97.meanAnomaly, 0.001));
    CHECK_THAT(res[97].epoch, Catch::Matchers::WithinRel(v97.epoch, 0.001));
    CHECK_THAT(res[97].period, Catch::Matchers::WithinRel(v97.period, 0.001));

    CHECK(res[125].name == v125.name);
    CHECK(res[125].id == "2007-29A"); // TLE has limited number of characters
    CHECK_THAT(res[125].inclination, Catch::Matchers::WithinRel(v125.inclination, 0.001));
    CHECK_THAT(
        res[125].semiMajorAxis,
        Catch::Matchers::WithinRel(v125.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[125].ascendingNode,
        Catch::Matchers::WithinRel(v125.ascendingNode, 0.001)
    );
    CHECK_THAT(
        res[125].eccentricity,
        Catch::Matchers::WithinRel(v125.eccentricity, 0.001)
    );
    CHECK_THAT(
        res[125].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v125.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[125].meanAnomaly, Catch::Matchers::WithinRel(v125.meanAnomaly, 0.001));
    CHECK_THAT(res[125].epoch, Catch::Matchers::WithinRel(v125.epoch, 0.001));
    CHECK_THAT(res[125].period, Catch::Matchers::WithinRel(v125.period, 0.001));
}

TEST_CASE("Kepler: OMM", "[kepler]") {
    const std::filesystem::path file = absPath("${TESTDIR}/kepler/omm-format.txt");

    std::vector<Parameters> res = readOmmFile(file);
    CHECK(res.size() == 148);

    CHECK(res[0].name == v0.name);
    CHECK(res[0].id == v0.id);
    CHECK_THAT(res[0].inclination, Catch::Matchers::WithinRel(v0.inclination, 0.001));
    CHECK_THAT(res[0].semiMajorAxis, Catch::Matchers::WithinRel(v0.semiMajorAxis, 0.001));
    CHECK_THAT(res[0].ascendingNode, Catch::Matchers::WithinRel(v0.ascendingNode, 0.001));
    CHECK_THAT(res[0].eccentricity, Catch::Matchers::WithinRel(v0.eccentricity, 0.001));
    CHECK_THAT(
        res[0].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v0.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[0].meanAnomaly, Catch::Matchers::WithinRel(v0.meanAnomaly, 0.001));
    CHECK_THAT(res[0].epoch, Catch::Matchers::WithinRel(v0.epoch, 0.001));
    CHECK_THAT(res[0].period, Catch::Matchers::WithinRel(v0.period, 0.001));

    CHECK(res[50].name == v50.name);
    CHECK(res[50].id == v50.id);
    CHECK_THAT(res[50].inclination, Catch::Matchers::WithinRel(v50.inclination, 0.001));
    CHECK_THAT(
        res[50].semiMajorAxis,
        Catch::Matchers::WithinRel(v50.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[50].ascendingNode,
        Catch::Matchers::WithinRel(v50.ascendingNode, 0.001)
    );
    CHECK_THAT(res[50].eccentricity, Catch::Matchers::WithinRel(v50.eccentricity, 0.001));
    CHECK_THAT(
        res[50].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v50.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[50].meanAnomaly, Catch::Matchers::WithinRel(v50.meanAnomaly, 0.001));
    CHECK_THAT(res[50].epoch, Catch::Matchers::WithinRel(v50.epoch, 0.001));
    CHECK_THAT(res[50].period, Catch::Matchers::WithinRel(v50.period, 0.001));

    CHECK(res[75].name == v75.name);
    CHECK(res[75].id == v75.id);
    CHECK_THAT(res[75].inclination, Catch::Matchers::WithinRel(v75.inclination, 0.001));
    CHECK_THAT(
        res[75].semiMajorAxis,
        Catch::Matchers::WithinRel(v75.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[75].ascendingNode,
        Catch::Matchers::WithinRel(v75.ascendingNode, 0.001)
    );
    CHECK_THAT(res[75].eccentricity, Catch::Matchers::WithinRel(v75.eccentricity, 0.001));
    CHECK_THAT(
        res[75].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v75.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[75].meanAnomaly, Catch::Matchers::WithinRel(v75.meanAnomaly, 0.001));
    CHECK_THAT(res[75].epoch, Catch::Matchers::WithinRel(v75.epoch, 0.001));
    CHECK_THAT(res[75].period, Catch::Matchers::WithinRel(v75.period, 0.001));

    CHECK(res[97].name == v97.name);
    CHECK(res[97].id == v97.id);
    CHECK_THAT(res[97].inclination, Catch::Matchers::WithinRel(v97.inclination, 0.001));
    CHECK_THAT(
        res[97].semiMajorAxis,
        Catch::Matchers::WithinRel(v97.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[97].ascendingNode,
        Catch::Matchers::WithinRel(v97.ascendingNode, 0.001)
    );
    CHECK_THAT(res[97].eccentricity, Catch::Matchers::WithinRel(v97.eccentricity, 0.001));
    CHECK_THAT(
        res[97].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v97.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[97].meanAnomaly, Catch::Matchers::WithinRel(v97.meanAnomaly, 0.001));
    CHECK_THAT(res[97].epoch, Catch::Matchers::WithinRel(v97.epoch, 0.001));
    CHECK_THAT(res[97].period, Catch::Matchers::WithinRel(v97.period, 0.001));

    CHECK(res[125].name == v125.name);
    CHECK(res[125].id == v125.id);
    CHECK_THAT(res[125].inclination, Catch::Matchers::WithinRel(v125.inclination, 0.001));
    CHECK_THAT(
        res[125].semiMajorAxis,
        Catch::Matchers::WithinRel(v125.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[125].ascendingNode,
        Catch::Matchers::WithinRel(v125.ascendingNode, 0.001)
    );
    CHECK_THAT(
        res[125].eccentricity,
        Catch::Matchers::WithinRel(v125.eccentricity, 0.001)
    );
    CHECK_THAT(
        res[125].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v125.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[125].meanAnomaly, Catch::Matchers::WithinRel(v125.meanAnomaly, 0.001));
    CHECK_THAT(res[125].epoch, Catch::Matchers::WithinRel(v125.epoch, 0.001));
    CHECK_THAT(res[125].period, Catch::Matchers::WithinRel(v125.period, 0.001));
}

TEST_CASE("Kepler: CSV", "[kepler]") {
    const std::filesystem::path file = absPath("${TESTDIR}/kepler/csv-format.txt");

    std::vector<Parameters> res = readCsvFile(file);
    CHECK(res.size() == 148);

    CHECK(res[0].name == v0.name);
    CHECK(res[0].id == v0.id);
    CHECK_THAT(res[0].inclination, Catch::Matchers::WithinRel(v0.inclination, 0.001));
    CHECK_THAT(res[0].semiMajorAxis, Catch::Matchers::WithinRel(v0.semiMajorAxis, 0.001));
    CHECK_THAT(res[0].ascendingNode, Catch::Matchers::WithinRel(v0.ascendingNode, 0.001));
    CHECK_THAT(res[0].eccentricity, Catch::Matchers::WithinRel(v0.eccentricity, 0.001));
    CHECK_THAT(
        res[0].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v0.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[0].meanAnomaly, Catch::Matchers::WithinRel(v0.meanAnomaly, 0.001));
    CHECK_THAT(res[0].epoch, Catch::Matchers::WithinRel(v0.epoch, 0.001));
    CHECK_THAT(res[0].period, Catch::Matchers::WithinRel(v0.period, 0.001));

    CHECK(res[50].name == v50.name);
    CHECK(res[50].id == v50.id);
    CHECK_THAT(res[50].inclination, Catch::Matchers::WithinRel(v50.inclination, 0.001));
    CHECK_THAT(
        res[50].semiMajorAxis,
        Catch::Matchers::WithinRel(v50.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[50].ascendingNode,
        Catch::Matchers::WithinRel(v50.ascendingNode, 0.001)
    );
    CHECK_THAT(res[50].eccentricity, Catch::Matchers::WithinRel(v50.eccentricity, 0.001));
    CHECK_THAT(
        res[50].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v50.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[50].meanAnomaly, Catch::Matchers::WithinRel(v50.meanAnomaly, 0.001));
    CHECK_THAT(res[50].epoch, Catch::Matchers::WithinRel(v50.epoch, 0.001));
    CHECK_THAT(res[50].period, Catch::Matchers::WithinRel(v50.period, 0.001));

    CHECK(res[75].name == v75.name);
    CHECK(res[75].id == v75.id);
    CHECK_THAT(res[75].inclination, Catch::Matchers::WithinRel(v75.inclination, 0.001));
    CHECK_THAT(
        res[75].semiMajorAxis,
        Catch::Matchers::WithinRel(v75.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[75].ascendingNode,
        Catch::Matchers::WithinRel(v75.ascendingNode, 0.001)
    );
    CHECK_THAT(res[75].eccentricity, Catch::Matchers::WithinRel(v75.eccentricity, 0.001));
    CHECK_THAT(
        res[75].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v75.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[75].meanAnomaly, Catch::Matchers::WithinRel(v75.meanAnomaly, 0.001));
    CHECK_THAT(res[75].epoch, Catch::Matchers::WithinRel(v75.epoch, 0.001));
    CHECK_THAT(res[75].period, Catch::Matchers::WithinRel(v75.period, 0.001));

    CHECK(res[97].name == v97.name);
    CHECK(res[97].id == v97.id);
    CHECK_THAT(res[97].inclination, Catch::Matchers::WithinRel(v97.inclination, 0.001));
    CHECK_THAT(
        res[97].semiMajorAxis,
        Catch::Matchers::WithinRel(v97.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[97].ascendingNode,
        Catch::Matchers::WithinRel(v97.ascendingNode, 0.001)
    );
    CHECK_THAT(res[97].eccentricity, Catch::Matchers::WithinRel(v97.eccentricity, 0.001));
    CHECK_THAT(
        res[97].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v97.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[97].meanAnomaly, Catch::Matchers::WithinRel(v97.meanAnomaly, 0.001));
    CHECK_THAT(res[97].epoch, Catch::Matchers::WithinRel(v97.epoch, 0.001));
    CHECK_THAT(res[97].period, Catch::Matchers::WithinRel(v97.period, 0.001));

    CHECK(res[125].name == v125.name);
    CHECK(res[125].id == v125.id);
    CHECK_THAT(res[125].inclination, Catch::Matchers::WithinRel(v125.inclination, 0.001));
    CHECK_THAT(
        res[125].semiMajorAxis,
        Catch::Matchers::WithinRel(v125.semiMajorAxis, 0.001)
    );
    CHECK_THAT(
        res[125].ascendingNode,
        Catch::Matchers::WithinRel(v125.ascendingNode, 0.001)
    );
    CHECK_THAT(
        res[125].eccentricity,
        Catch::Matchers::WithinRel(v125.eccentricity, 0.001)
    );
    CHECK_THAT(
        res[125].argumentOfPeriapsis,
        Catch::Matchers::WithinRel(v125.argumentOfPeriapsis, 0.001)
    );
    CHECK_THAT(res[125].meanAnomaly, Catch::Matchers::WithinRel(v125.meanAnomaly, 0.001));
    CHECK_THAT(res[125].epoch, Catch::Matchers::WithinRel(v125.epoch, 0.001));
    CHECK_THAT(res[125].period, Catch::Matchers::WithinRel(v125.period, 0.001));
}
