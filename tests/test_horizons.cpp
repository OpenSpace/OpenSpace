/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/json.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
#include <modules/space/horizonsfile.h>
#endif // OPENSPACE_MODULE_SPACE_ENABLED

using namespace openspace;
using json = nlohmann::json;

// Struct to hold the data for some of the tests
struct HorizonsTestData {
    std::string observer, target, start, stop, step, unit;
};

// Avoid repetitive code by using these functions
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
void testHorizonsAnswer(const HorizonsTestData& data, HorizonsType type,
                        const std::filesystem::path& filePath,
                        HorizonsResultCode expectedAnswerCode)
{
    const std::string url = constructHorizonsUrl(
        type,
        data.target,
        data.observer,
        data.start,
        data.stop,
        data.step,
        data.unit
    );
    const json answer = sendHorizonsRequest(url, filePath);
    HorizonsResultCode code = isValidHorizonsAnswer(answer);
    CHECK(code == expectedAnswerCode);

    CHECK(std::filesystem::is_regular_file(filePath));
    std::filesystem::remove(filePath);
    CHECK(!std::filesystem::is_regular_file(filePath));
}

void testHorizonsAnswerAndResult(const HorizonsTestData& data, HorizonsType type,
                                 const std::filesystem::path& filePath,
                                 HorizonsResultCode expectedAnswerCode,
                                 HorizonsResultCode expectedResultCode,
                                 bool shouldDeleteFile = true)
{
    const std::string url = constructHorizonsUrl(
        type,
        data.target,
        data.observer,
        data.start,
        data.stop,
        data.step,
        data.unit
    );
    json answer = sendHorizonsRequest(url, filePath);
    HorizonsResultCode answerCode = isValidHorizonsAnswer(answer);
    CHECK(answerCode == expectedAnswerCode);

    // Extract the result from the json object and test it
    auto result = answer.find("result");
    CHECK(result != answer.end());

    const HorizonsFile horizonsFile(filePath, *result);
    HorizonsResultCode resultCode = isValidHorizonsFile(horizonsFile.file());
    CHECK(resultCode == expectedResultCode);

    CHECK(std::filesystem::is_regular_file(filePath));
    if (shouldDeleteFile) {
        std::filesystem::remove(filePath);
        CHECK(!std::filesystem::is_regular_file(filePath));
    }
}

void testReadingHorizons(HorizonsType type, const std::filesystem::path& filePath,
                       const double t0, const double x0, const double y0, const double z0,
                       const double t1, const double x1, const double y1, const double z1,
                       const double t2, const double x2, const double y2, const double z2)
{
    // Get files and make sure they exist
    const std::filesystem::path kernel = absPath("${TESTDIR}/horizonsTest/naif0012.tls");
    CHECK(std::filesystem::is_regular_file(kernel));
    CHECK(std::filesystem::is_regular_file(filePath));

    // Initialize SpiceManager and load leap second kernel
    SpiceManager::initialize();
    openspace::SpiceManager::ref().loadKernel(kernel);

    // Read the file
    HorizonsResult result = readHorizonsFile(filePath);

    // Check the result
    CHECK(result.type == type);
    CHECK(result.errorCode == HorizonsResultCode::Valid);

    std::vector<HorizonsKeyframe> data = result.data;
    REQUIRE(data.size() == 3);

    CHECK(data[0].time == Catch::Approx(t0));
    CHECK(data[0].position.x == Catch::Approx(x0));
    CHECK(data[0].position.y == Catch::Approx(y0));
    CHECK(data[0].position.z == Catch::Approx(z0));


    CHECK(data[1].time == Catch::Approx(t1));
    CHECK(data[1].position.x == Catch::Approx(x1));
    CHECK(data[1].position.y == Catch::Approx(y1));
    CHECK(data[1].position.z == Catch::Approx(z1));


    CHECK(data[2].time == Catch::Approx(t2));
    CHECK(data[2].position.x == Catch::Approx(x2));
    CHECK(data[2].position.y == Catch::Approx(y2));
    CHECK(data[2].position.z == Catch::Approx(z2));

    // Clean up
    openspace::SpiceManager::ref().unloadKernel(kernel);
    openspace::SpiceManager::deinitialize();
}
#endif // OPENSPACE_MODULE_SPACE_ENABLED

// Test if the space module is enable or not
TEST_CASE("HorizonsFile: Space module", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    CHECK(true);
    LINFOC("test_horizons", "Downloading Horizons test data ...");
#else
    LERRORC("test_horizons", "These tests requires the Space module to be enabled");
    CHECK(false);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

// Test Horizons requests and error handling
// Test the errors that are captured by the error field in the json result
TEST_CASE("HorizonsFile: File size too large", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "-74"; // MRO
    data.start = "2005-08-13 00:00:00";
    data.stop = "2022-07-01 00:00:00";
    data.step = "1";
    data.unit = "m";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_1.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::ErrorSize;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswer(data, type, filePath, expectedAnswerCode);

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswer(data, type, filePath, expectedAnswerCode);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Time steps too large", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "-74"; // MRO
    data.start = "2005-08-13 00:00:00";
    data.stop = "2022-07-01 00:00:00";
    data.step = "1111111111";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_2.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::ErrorSpan;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswer(data, type, filePath, expectedAnswerCode);

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswer(data,type, filePath, expectedAnswerCode);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Outside available time range", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "Tesla";
    data.start = "1950-05-19 00:00:00";
    data.stop = "2000-05-19 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_3.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::ErrorTimeRange;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswer(data, type, filePath, expectedAnswerCode);

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswer(data, type, filePath, expectedAnswerCode);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: No observer", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "abcdefghijkl";
    data.target = "Tesla";
    data.start = "2021-05-19 00:00:00";
    data.stop = "2022-05-19 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_4.hrz"
    );
    const HorizonsResultCode answer = HorizonsResultCode::ErrorNoObserver;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswer(data, type, filePath, answer);

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswer(data, type, filePath, answer);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Observer and target same", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "500@4"; // Mars barycenter
    data.target = "4";       // Mars barycenter
    data.start = "2021-05-19 00:00:00";
    data.stop = "2022-05-19 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_5.hrz"
    );
    const HorizonsResultCode answer = HorizonsResultCode::ErrorObserverTargetSame;

    // This test is only for Observer type format
    const HorizonsType type = HorizonsType::Observer;
    testHorizonsAnswer(data, type, filePath, answer);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Multiple observer stations", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "l2";
    data.target = "Tesla";
    data.start = "2021-05-19 00:00:00";
    data.stop = "2022-05-19 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_6.hrz"
    );
    const HorizonsResultCode answer = HorizonsResultCode::MultipleObserverStations;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswer(data, type, filePath, answer);

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswer(data, type, filePath, answer);
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}


// Test errors that are nat captured by the error field in the json result
TEST_CASE("HorizonsFile: Multiple observers", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@mars";
    data.target = "Tesla";
    data.start = "2021-05-19 00:00:00";
    data.stop = "2022-05-19 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_7.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::MultipleObserver;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: No target", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "abcdefghijkl";
    data.start = "2021-05-20 00:00:00";
    data.stop = "2022-05-20 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_8.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::ErrorNoTarget;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Multiple targets (major bodies)", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "mars";
    data.start = "2021-05-20 00:00:00";
    data.stop = "2022-05-20 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_9.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::MultipleTarget;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Multiple targets (minor bodies case 1)", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "DES = 141P;";
    data.start = "2021-05-20 00:00:00";
    data.stop = "2022-05-20 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_10.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::MultipleTarget;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Multiple targets (minor bodies case 2)", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "borisov";
    data.start = "2021-05-20 00:00:00";
    data.stop = "2022-05-20 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_11.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::MultipleTarget;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Detect multiple observers or targets", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@l2";
    data.target = "90001048"; // 141P Machholz 2 epoch 2019
    data.start = "2021-05-23 00:00:00";
    data.stop = "2022-05-23 00:00:00";
    data.step = "1";
    data.unit = "d";
    const std::filesystem::path filePath = absPath(
        "${TESTDIR}/horizonsTest/horizonstest_12.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::MultipleObserver;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePath,
        expectedAnswerCode,
        expectedResultCode
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Valid request and response", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsTestData data;
    data.observer = "@ssb";
    data.target = "Tesla";
    data.start = "2022-05-22 00:00:00";
    data.stop = "2022-05-23 00:00:00";
    data.step = "12";
    data.unit = "h";
    const std::filesystem::path filePathVector = absPath(
        "${TESTDIR}/horizonsTest/validVectorFile.hrz"
    );
    const std::filesystem::path filePathObserver = absPath(
        "${TESTDIR}/horizonsTest/validObserverFile.hrz"
    );
    const HorizonsResultCode expectedAnswerCode = HorizonsResultCode::Valid;
    const HorizonsResultCode expectedResultCode = HorizonsResultCode::Valid;

    // Test Vector format
    HorizonsType type = HorizonsType::Vector;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePathVector,
        expectedAnswerCode,
        expectedResultCode,
        false
    );

    // Test Observer format
    type = HorizonsType::Observer;
    testHorizonsAnswerAndResult(
        data,
        type,
        filePathObserver,
        expectedAnswerCode,
        expectedResultCode,
        false
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}


// Test parsing of timeranges
// Since data can get updated this is tested towards stored files
TEST_CASE("HorizonsFile: Parsing time range with time", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const std::filesystem::path filePath =
        absPath("${TESTDIR}/horizonsTest/timerange_time.hrz");
    const HorizonsFile horizonsFile(filePath);

    // Parse time range
    const std::pair<std::string, std::string> timeRange = horizonsFile.parseValidTimeRange(
        "Trajectory files",
        "************",
        "Trajectory name"
    );

    CHECK(timeRange.first == "2005-Aug-12 T 12:42");
    CHECK(timeRange.second == "2022-Sep-02 T 02:55");
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Parsing time range without time", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const std::filesystem::path filePath =
        absPath("${TESTDIR}/horizonsTest/timerange_no_time.hrz");
    const HorizonsFile horizonsFile(filePath);

    // Parse time range
    const std::pair<std::string, std::string> timeRange = horizonsFile.parseValidTimeRange(
        "Trajectory files",
        "************",
        "Trajectory name",
        false
    );

    CHECK(timeRange.first == "2013-Dec-19");
    CHECK(timeRange.second == "2026-Sep-14");
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}


// Test parsing of matches
// Since data can get updated this is tested towards stored files
TEST_CASE("HorizonsFile: Parsing multiple matching observers", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const std::filesystem::path filePath =
        absPath("${TESTDIR}/horizonsTest/parse_multiple_observers.hrz");
    const HorizonsFile horizonsFile(filePath);

    // Parse matches
    const std::vector<std::string> matches =
        horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");

    REQUIRE(matches.size() == 3);
    CHECK(matches[0] ==
        "  ID#      Name                               Designation  IAU/aliases/other   "
    );
    CHECK(matches[1] ==
        "        3  Earth-Moon Barycenter                           EMB                  "
    );
    CHECK(matches[2] ==
        "      399  Earth                                           Geocenter            "
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Parsing multiple matching targets", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const std::filesystem::path filePath =
        absPath("${TESTDIR}/horizonsTest/parse_multiple_targets.hrz");
    const HorizonsFile horizonsFile(filePath);

    // Parse matches
    const std::vector<std::string> matches =
        horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");

    REQUIRE(matches.size() == 11);
    CHECK(matches[0] ==
        "  ID#      Name                               Designation  IAU/aliases/other   "
    );
    CHECK(matches[1] ==
        "        4  Mars Barycenter                                                      "
    );
    CHECK(matches[2] ==
        "      499  Mars                                                                 "
    );
    CHECK(matches[3] ==
        "       -3  Mars Orbiter Mission (MOM) (spacec              Mangalyaan           "
    );
    CHECK(matches[4] ==
        "      -41  Mars Express (spacecraft)                       MEX                  "
    );
    CHECK(matches[5] ==
        "      -53  Mars Odyssey (spacecraft)                                            "
    );
    CHECK(matches[6] ==
        "     -530  Mars Pathfinder (spacecraft)                    MPF                  "
    );
    CHECK(matches[7] ==
        "      -74  Mars Reconnaissance Orbiter (space              MRO                  "
    );
    CHECK(matches[8] ==
        "      -76  Mars Science Laboratory (spacecraf              Curiosity MSL        "
    );
    CHECK(matches[9] ==
        "     -143  ExoMars16 TGO (spacecraft)                                           "
    );
    CHECK(matches[10] ==
        "     -168  Mars2020 (spacecraft)                           Perserverance Ingenu "
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Parsing multiple matching stations", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const std::filesystem::path filePath =
        absPath("${TESTDIR}/horizonsTest/parse_multiple_stations.hrz");
    CHECK(std::filesystem::is_regular_file(filePath));
    const HorizonsFile horizonsFile(filePath);

    // Parse matches
    const std::vector<std::string> matches =
        horizonsFile.parseMatches("Observatory Name", "Multiple matching stations found");

    REQUIRE(matches.size() == 11);
    CHECK(matches[0] ==
        "   #   E. Lon    DXY      DZ    Observatory Name"
    );
    CHECK(matches[1] ==
        "  L20  18.3207 +308014 +293859  AG_Sarajevo Observatory, Sarajevo"
    );
    CHECK(matches[2] ==
        "  L21  27.4213 +307050 +294814  Ostrov Observatory, Constanta"
    );
    CHECK(matches[3] ==
        "  L22  27.6695 +295446 +306365  Barlad Observatory"
    );
    CHECK(matches[4] ==
        "  L23  27.8319 +299346 +302574  Schela"
    );
    CHECK(matches[5] ==
        "  L24  27.9289 +383214 -186499  Gauteng"
    );
    CHECK(matches[6] ==
        "  L25  14.4374 +255033 +340528  Smolecin"
    );
    CHECK(matches[7] ==
        "  L26  11.8102 +316937 +284229  Sanderphil Urban Obs., Civitavecchia"
    );
    CHECK(matches[8] ==
        "  L27   5.6470 +307222 +294693  29PREMOTE Obs., Dauban {Soulier]"
    );
    CHECK(matches[9] ==
        "  L28  15.4634 +323190 +277274  ISON-Castelgrande Observatory"
    );
    CHECK(matches[10] ==
        "  L29  18.0169 +391400 -168725  Drebach-South Observatory, Windhoek"
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}


// Test reading of data and compare a recent request with a stored file
TEST_CASE("HorizonsFile: Reading Vector data from request", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const HorizonsType type = HorizonsType::Vector;
    const std::filesystem::path filePathVector =
        absPath("${TESTDIR}/horizonsTest/validVectorFile.hrz");

    const double t0 = 706449669.18513119;
    const double x0 = -126379670172.70331;
    const double y0 = 63049830070.652786;
    const double z0 = -126710964556.55870;

    const double t1 = 706492869.18512082;
    const double x1 = -127019567853.94952;
    const double y1 = 62510445746.414017;
    const double z1 = -125904395646.64995;

    const double t2 = 706536069.18511045;
    const double x2 = -127654909093.56494;
    const double y2 = 61968790989.645737;
    const double z2 = -125093260079.10854;

    testReadingHorizons(
        type, filePathVector,
        t0, x0, y0, z0,
        t1, x1, y1, z1,
        t2, x2, y2, z2
    );

    // Clean up
    std::filesystem::remove(filePathVector);
    CHECK(!std::filesystem::is_regular_file(filePathVector));
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Reading Observer data from request", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const HorizonsType type = HorizonsType::Observer;
    const std::filesystem::path filePathObserver =
        absPath("${TESTDIR}/horizonsTest/validObserverFile.hrz");

    const double t0 = 706449669.18513119;
    const double x0 = -126371142157.29857;
    const double y0 = 63056923889.044579;
    const double z0 = -126721572150.18513;

    const double t1 = 706492869.18512082;
    const double x1 = -127011112787.76295;
    const double y1 = 62517559574.749786;
    const double z1 = -125915045896.36182;

    const double t2 = 706536069.18511045;
    const double x2 = -127646529740.40393;
    const double y2 = 61975921972.090714;
    const double z2 = -125103951590.60988;

    testReadingHorizons(
        type, filePathObserver,
        t0, x0, y0, z0,
        t1, x1, y1, z1,
        t2, x2, y2, z2
    );

    // Clean up
    std::filesystem::remove(filePathObserver);
    CHECK(!std::filesystem::is_regular_file(filePathObserver));
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}


TEST_CASE("HorizonsFile: Reading Vector data from file", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const HorizonsType type = HorizonsType::Vector;
    const std::filesystem::path filePathVector =
        absPath("${TESTDIR}/horizonsTest/vectorFileTest.hrz");

    const double t0 = 706449669.18513119;
    const double x0 = -126379670172.70331;
    const double y0 = 63049830070.652786;
    const double z0 = -126710964556.55870;

    const double t1 = 706492869.18512082;
    const double x1 = -127019567853.94952;
    const double y1 = 62510445746.414017;
    const double z1 = -125904395646.64995;

    const double t2 = 706536069.18511045;
    const double x2 = -127654909093.56494;
    const double y2 = 61968790989.645737;
    const double z2 = -125093260079.10854;

    testReadingHorizons(
        type, filePathVector,
        t0, x0, y0, z0,
        t1, x1, y1, z1,
        t2, x2, y2, z2
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

TEST_CASE("HorizonsFile: Reading Observer data from file", "[horizonsfile]") {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    const HorizonsType type = HorizonsType::Observer;
    const std::filesystem::path filePathObserver =
        absPath("${TESTDIR}/horizonsTest/observerFileTest.hrz");

    const double t0 = 706449669.18513119;
    const double x0 = -126371142157.29857;
    const double y0 = 63056923889.044579;
    const double z0 = -126721572150.18513;

    const double t1 = 706492869.18512082;
    const double x1 = -127011112787.76295;
    const double y1 = 62517559574.749786;
    const double z1 = -125915045896.36182;

    const double t2 = 706536069.18511045;
    const double x2 = -127646529740.40393;
    const double y2 = 61975921972.090714;
    const double z2 = -125103951590.60988;

    testReadingHorizons(
        type, filePathObserver,
        t0, x0, y0, z0,
        t1, x1, y1, z1,
        t2, x2, y2, z2
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}
