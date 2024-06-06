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

#include <catch2/catch_test_macros.hpp>

#include "modules/globebrowsing/src/timequantizer.h"
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

using namespace openspace;

namespace {
    int loadLSKKernel() {
        const int kernelID = openspace::SpiceManager::ref().loadKernel(
            absPath("${TESTDIR}/SpiceTest/spicekernels/naif0008.tls")
        );
        CHECK(kernelID == 1);
        return kernelID;
    }

    void singleTimeTest(Time& t, globebrowsing::TimeQuantizer& tq, bool clamp,
                        const std::string& input, const std::string& expected)
    {
        t.setTime(input);
        tq.quantize(t, clamp);
        CHECK(t.ISO8601() == expected);
    }

    void singleResolutionTest(globebrowsing::TimeQuantizer& tq,
                              const std::string& resolution,
                              const std::string& expectedType, bool expectFailure)
    {
        std::string res;
        try {
            tq.setResolution(resolution);
        }
        catch (const ghoul::RuntimeError & e) {
            res = e.message;
        }

        if (expectFailure) {
            CHECK(res.find(expectedType) != std::string::npos);
        }
        else {
            CHECK(res.find(expectedType) == std::string::npos);
        }
    }

    void singleStartTimeTest(globebrowsing::TimeQuantizer& tq,
                             const std::string& startTime,
                             const std::string& expectedErrSubstring, bool expectFailure)
    {
        std::string res;
        try {
            tq.setStartEndRange(startTime, startTime);
        }
        catch (const ghoul::RuntimeError & e) {
            res = e.message;
        }

        if (expectFailure) {
            CHECK(res.find(expectedErrSubstring) != std::string::npos);
        }
        else {
            CHECK(res.find(expectedErrSubstring) == std::string::npos);
        }
    }

    void singleStartTimeTest(const std::string& startTime,
                             const std::string& expectedErrSubstring, bool expectFailure)
    {
        std::string res;
        try {
            const globebrowsing::TimeQuantizer tq(startTime, startTime, "1d");
        }
        catch (const ghoul::RuntimeError & e) {
            res = e.message;
        }

        if (expectFailure) {
            CHECK(res.find(expectedErrSubstring) != std::string::npos);
        }
        else {
            CHECK(res.find(expectedErrSubstring) == std::string::npos);
        }
    }
} // namespace

TEST_CASE("TimeQuantizer: Test years resolution", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;
    Time testT;

    t1.setStartEndRange("2019-12-09T00:00:00", "2030-03-01T00:00:00");
    t1.setResolution("1y");

    singleTimeTest(testT, t1, true, "2020-12-08T23:59:59", "2019-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-12-09T00:00:00", "2020-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2021-12-08T23:59:58", "2020-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2022-12-09T00:00:02", "2022-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2022-11-08T13:00:15", "2021-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-12-09T00:00:00", "2020-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2024-12-08T23:59:59", "2023-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2024-12-09T00:00:01", "2024-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-12-31T00:00:01", "2020-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2021-01-01T00:00:00", "2020-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-12-31T23:59:59", "2020-12-09T00:00:00.000");

    t1.setResolution("3y");

    singleTimeTest(testT, t1, true, "2020-12-08T23:59:59", "2019-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2022-12-09T00:00:00", "2022-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2028-12-08T23:59:59", "2025-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2028-12-09T00:00:01", "2028-12-09T00:00:00.000");

    try {
        t1.setStartEndRange("2020-02-29T00:00:00", "2030-02-29T00:00:00");
    }
    catch (const ghoul::RuntimeError& e) {
        REQUIRE(e.message.find("Invalid start day value of 29 for the selected month "
            "on a yearly increment, valid days are 1 - 28") != std::string::npos);
    }
    t1.setStartEndRange("2020-02-28T00:00:00", "2030-02-28T00:00:00");

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test days resolution", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;
    Time testT;

    t1.setStartEndRange("2019-12-09T00:00:00", "2020-03-01T00:00:00");
    t1.setResolution("1d");

    singleTimeTest(testT, t1, true, "2020-01-07T05:15:45", "2020-01-07T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-01-07T00:00:00", "2020-01-07T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-01-07T00:00:01", "2020-01-07T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-01-06T23:59:59", "2020-01-06T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-01-31T23:59:59", "2020-01-31T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-02-01T00:00:00", "2020-02-01T00:00:00.000");
    singleTimeTest(testT, t1, false, "2020-02-01T00:00:00", "2020-02-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-02-29T00:00:02", "2020-02-29T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-01-01T00:00:00", "2020-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2020-03-02T14:00:00", "2020-03-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2019-12-15T14:00:00", "2019-12-15T00:00:00.000");
    singleTimeTest(testT, t1, true, "2019-12-08T23:59:00", "2019-12-09T00:00:00.000");
    singleTimeTest(testT, t1, true, "2019-12-05T14:29:00", "2019-12-09T00:00:00.000");

    t1.setStartEndRange("2016-05-28T00:00:00", "2021-09-01T00:00:00");
    t1.setResolution("4d");

    singleTimeTest(testT, t1, true, "2016-06-01T00:00:00", "2016-06-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-06-01T00:00:01", "2016-06-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-07-03T10:00:00", "2016-07-03T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-07-07T00:00:00", "2016-07-07T00:00:00.000");
    singleTimeTest(testT, t1, true, "2021-11-07T00:00:00", "2021-09-01T00:00:00.000");
    singleTimeTest(testT, t1, false, "2021-11-07T00:00:00", "2021-11-07T00:00:00.000");

    t1.setStartEndRange("2019-02-21T00:00:00", "2021-09-01T00:00:00");
    t1.setResolution("1d");

    singleTimeTest(testT, t1, true, "2020-03-01T00:30:00", "2020-03-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2019-03-04T00:00:02", "2019-03-04T00:00:00.000");

    t1.setStartEndRange("2020-01-31T00:00:00", "2020-07-31T00:00:00");
    t1.setResolution("1d");

    singleTimeTest(testT, t1, true, "2020-03-01T00:30:00", "2020-03-01T00:00:00.000");

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test months resolution", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;
    Time testT;

    t1.setStartEndRange("2017-01-28T00:00:00", "2020-09-01T00:00:00");
    t1.setResolution("1M");

    singleTimeTest(testT, t1, true, "2017-03-03T05:15:45", "2017-02-28T00:00:00.000");
    singleTimeTest(testT, t1, true, "2017-03-29T00:15:45", "2017-03-28T00:00:00.000");

    try {
        t1.setStartEndRange("2017-01-30T00:00:00", "2020-09-01T00:00:00");
    }
    catch (const ghoul::RuntimeError& e) {
        CHECK(e.message.find("Invalid start day value of 30 for monthly increment, "
                             "valid days are 1 - 28") != std::string::npos);
    }

    t1.setStartEndRange("2016-01-17T00:00:00", "2020-09-01T00:00:00");
    t1.setResolution("2M");

    singleTimeTest(testT, t1, true, "2016-01-27T05:15:45", "2016-01-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-03-16T08:15:45", "2016-01-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-03-17T18:00:02", "2016-03-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-05-18T00:00:02", "2016-05-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-11-17T10:15:45", "2016-11-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2017-01-18T05:15:45", "2017-01-17T00:00:00.000");

    t1.setResolution("3M");

    singleTimeTest(testT, t1, true, "2016-04-16T05:15:45", "2016-01-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2016-07-27T05:15:45", "2016-07-17T00:00:00.000");
    singleTimeTest(testT, t1, true, "2017-10-17T00:01:00", "2017-10-17T00:00:00.000");

    t1.setStartEndRange("2016-05-28T00:00:00", "2021-09-01T00:00:00");
    t1.setResolution("6M");

    singleTimeTest(testT, t1, true, "2016-11-28T00:00:05", "2016-11-28T00:00:00.000");
    singleTimeTest(testT, t1, true, "2017-05-30T04:15:45", "2017-05-28T00:00:00.000");
    singleTimeTest(testT, t1, true, "2017-10-17T05:01:00", "2017-05-28T00:00:00.000");

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test hours & minutes resolution", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;
    Time testT;

    t1.setStartEndRange("2019-02-21T00:00:00", "2021-09-01T00:00:00");
    t1.setResolution("2h");

    singleTimeTest(testT, t1, true, "2019-02-28T16:10:00", "2019-02-28T16:00:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:00:00", "2019-02-28T22:00:00.000");

    t1.setResolution("3h");

    singleTimeTest(testT, t1, true, "2019-02-28T21:10:00", "2019-02-28T21:00:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T12:00:00", "2019-02-28T12:00:00.000");

    t1.setStartEndRange("2019-02-21T00:00:00", "2021-09-01T00:00:00");
    t1.setResolution("30m");

    singleTimeTest(testT, t1, true, "2019-02-27T16:40:00", "2019-02-27T16:30:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:00:00", "2019-02-28T22:00:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:30:01", "2019-02-28T22:30:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T21:29:59", "2019-02-28T21:00:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:59:59", "2019-02-28T22:30:00.000");

    t1.setResolution("15m");

    singleTimeTest(testT, t1, true, "2019-02-28T16:40:00", "2019-02-28T16:30:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:00:00", "2019-02-28T22:00:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:30:01", "2019-02-28T22:30:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:15:01", "2019-02-28T22:15:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:29:59", "2019-02-28T22:15:00.000");
    singleTimeTest(testT, t1, true, "2019-02-28T22:59:59", "2019-02-28T22:45:00.000");

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test pre-2000 dates", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;
    Time testT;

    t1.setStartEndRange("1000-01-01T00:00:00", "2001-01-01T00:00:00");
    t1.setResolution("1d");

    singleTimeTest(testT, t1, true, "1980-02-28T16:10:00", "1980-02-28T00:00:00.000");
    singleTimeTest(testT, t1, true, "1930-04-30T22:00:00", "1930-04-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1910-05-31T00:00:00", "1910-05-31T00:00:00.000");
    singleTimeTest(testT, t1, true, "1969-09-30T12:00:00", "1969-09-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1900-11-30T20:11:07", "1900-11-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1800-04-30T22:00:20", "1800-04-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1799-07-31T02:13:17", "1799-07-31T00:00:00.000");
    singleTimeTest(testT, t1, true, "1860-01-30T22:00:00", "1860-01-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1944-04-12T05:55:55", "1944-04-12T00:00:00.000");
    singleTimeTest(testT, t1, true, "1000-01-01T00:00:00", "1000-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "1999-04-30T22:00:00", "1999-04-30T00:00:00.000");
    singleTimeTest(testT, t1, true, "1999-12-31T23:59:59", "1999-12-31T00:00:00.000");
    singleTimeTest(testT, t1, true, "2000-01-01T11:59:59", "2000-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2000-01-01T12:00:00", "2000-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2000-01-01T12:00:01", "2000-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2000-01-01T00:00:00", "2000-01-01T00:00:00.000");
    singleTimeTest(testT, t1, true, "2000-01-02T00:00:00", "2000-01-02T00:00:00.000");

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test valid resolutions", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;

    singleResolutionTest(t1, "29d", "(d)ay option.", true);
    singleResolutionTest(t1, "0d", "(d)ay option.", true);
    singleResolutionTest(t1, "5h", "(h)our option.", true);
    singleResolutionTest(t1, "11h", "(h)our option.", true);
    singleResolutionTest(t1, "12h", "(h)our option.", false);
    singleResolutionTest(t1, "78y", "(y)ear option.", false);
    singleResolutionTest(t1, "12m", "(m)inute option.", true);
    singleResolutionTest(t1, "1m", "(m)inute option.", true);
    singleResolutionTest(t1, "0m", "(m)inute option.", true);
    singleResolutionTest(t1, "15m", "(m)inute option.", false);
    singleResolutionTest(t1, "30m", "(m)inute option.", false);
    singleResolutionTest(t1, "31m", "(m)inute option.", true);
    singleResolutionTest(t1, "10s", "unit format", true);

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test start time pre-existing object", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();
    globebrowsing::TimeQuantizer t1;

    singleStartTimeTest(t1, "2017-01-20T00:00:00", "Invalid start", false);
    singleStartTimeTest(t1, "2017-01-29T00:00:00", "Invalid start day value", false);
    singleStartTimeTest(t1, "2017-01-28T12:00:00", "Invalid start time value", true);
    singleStartTimeTest(t1, "2017-01-28T00:01:00", "Invalid start time value", true);
    singleStartTimeTest(t1, "2017-01-28T00:00:01", "Invalid start time value", true);

    SpiceManager::deinitialize();
}

TEST_CASE("TimeQuantizer: Test start time using constructor", "[timequantizer]") {
    SpiceManager::initialize();

    loadLSKKernel();

    singleStartTimeTest("2017-01-20T00:00:00", "Invalid start", false);
    singleStartTimeTest("2017-01-32T00:00:00", "For January the day must be at least 1.0D0 and less than 3.2E+01", true);
    singleStartTimeTest("2017-01-28T12:00:00", "Invalid start time value", true);
    singleStartTimeTest("2017-01-28T00:01:00", "Invalid start time value", true);
    singleStartTimeTest("2017-01-28T00:00:01", "Invalid start time value", true);
    singleStartTimeTest("1980-01-20T00:00:00", "Invalid start", false);

    SpiceManager::deinitialize();
}
