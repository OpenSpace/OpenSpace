/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "catch2/catch.hpp"

#include <openspace/util/timeconversion.h>

using namespace openspace;

TEST_CASE("TimeConversion: Simplify Time Round", "[timeconversion]") {
    {
        std::pair<double, std::string_view> p = simplifyTime(0.0, false);
        CHECK(p.first == 0.0);
        CHECK(p.second == "seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(0.0, true);
        CHECK(p.first == 0.0);
        CHECK(p.second == "second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-9, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "nanoseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-9, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "nanosecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-6, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "microseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-6, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "microsecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-3, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "milliseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-3, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "millisecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(120.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "minutes");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(120.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "minute");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(7200.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "hours");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(7200.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "hour");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(172800.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "days");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(172800.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "day");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(5259492.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "months");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(5259492.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "month");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(63113904.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "years");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(63113904.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "year");
    }
}

TEST_CASE("TimeConversion: Simplify Time Fractional", "[timeconversion]") {
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-10, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "nanoseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-10, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "nanosecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-7, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "microseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-7, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "microsecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-4, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "milliseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-4, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "millisecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(3.2, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(3.2, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(192.0, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "minutes");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(192.0, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "minute");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(11520.0, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "hours");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(11520.0, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "hour");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(276480.0, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "days");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(276480.0, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "day");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(8415187.2, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "months");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(8415187.2, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "month");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(100982246.4, false);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "years");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(100982246.4, true);
        CHECK(Approx(p.first) == 3.2);
        CHECK(p.second == "year");
    }
}

TEST_CASE("TimeConversion: Split Time Multiple Round", "[timeconversion]") {
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(0.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 0.0);
        CHECK(p[0].second == "seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(0.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 0.0);
        CHECK(p[0].second == "second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-9, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-9, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-6, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "microseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-6, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "microsecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-3, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "milliseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-3, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "millisecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(120.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(120.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(7200.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "hours");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(7200.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(172800.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "days");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(172800.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "day");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(5259492.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "months");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(5259492.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "month");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(63113904.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "years");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(63113904.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "year");
    }
}

TEST_CASE("TimeConversion: Split Time Fractional", "[timeconversion]") {
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-10, false);
        REQUIRE(p.size() == 1);
        CHECK(Approx(p[0].first) == 3.2);
        CHECK(p[0].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-10, true);
        REQUIRE(p.size() == 1);
        CHECK(Approx(p[0].first) == 3.2);
        CHECK(p[0].second == "nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-7, false);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "microseconds");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-7, true);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "microsecond");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-4, false);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "milliseconds");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "microseconds");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-4, true);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "millisecond");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "microsecond");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(3.2, false);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "seconds");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "milliseconds");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(3.2, true);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "second");
        CHECK(Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "millisecond");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(192.0, false);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "minutes");
        CHECK(Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(192.0, true);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "minute");
        CHECK(Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(11520.0, false);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "hours");
        CHECK(Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(11520.0, true);
        REQUIRE(p.size() == 2);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "hour");
        CHECK(Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(276480.0, false);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "days");
        CHECK(Approx(p[1].first) == 4);
        CHECK(p[1].second == "hours");
        CHECK(Approx(p[2].first) == 48);
        CHECK(p[2].second == "minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(276480.0, true);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "day");
        CHECK(Approx(p[1].first) == 4);
        CHECK(p[1].second == "hour");
        CHECK(Approx(p[2].first) == 48);
        CHECK(p[2].second == "minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(8414838.0, false);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "months");
        CHECK(Approx(p[1].first) == 6.0);
        CHECK(p[1].second == "days");
        CHECK(Approx(p[2].first) == 2.0);
        CHECK(p[2].second == "hours");

    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(8414838.0, true);
        REQUIRE(p.size() == 3);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "month");
        CHECK(Approx(p[1].first) == 6.0);
        CHECK(p[1].second == "day");
        CHECK(Approx(p[2].first) == 2.0);
        CHECK(p[2].second == "hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981548.0, false);
        REQUIRE(p.size() == 4);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "years");
        CHECK(Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "months");
        CHECK(Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "days");
        CHECK(Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "hours");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(100981548.0, true);
        REQUIRE(p.size() == 4);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "year");
        CHECK(Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "month");
        CHECK(Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "day");
        CHECK(Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981676.388, false);
        REQUIRE(p.size() == 9);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "years");
        CHECK(Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "months");
        CHECK(Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "days");
        CHECK(Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "hours");
        CHECK(Approx(p[4].first) == 2.0);
        CHECK(p[4].second == "minutes");
        CHECK(Approx(p[5].first) == 8.0);
        CHECK(p[5].second == "seconds");
        CHECK(Approx(p[6].first) == 387.0);
        CHECK(p[6].second == "milliseconds");
        CHECK(Approx(p[7].first) == 999.0);
        CHECK(p[7].second == "microseconds");
        CHECK(Approx(p[8].first) == 996.54293059);
        CHECK(p[8].second == "nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981676.388, true);
        REQUIRE(p.size() == 9);
        CHECK(Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "year");
        CHECK(Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "month");
        CHECK(Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "day");
        CHECK(Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "hour");
        CHECK(Approx(p[4].first) == 2.0);
        CHECK(p[4].second == "minute");
        CHECK(Approx(p[5].first) == 8.0);
        CHECK(p[5].second == "second");
        CHECK(Approx(p[6].first) == 387.0);
        CHECK(p[6].second == "millisecond");
        CHECK(Approx(p[7].first) == 999.0);
        CHECK(p[7].second == "microsecond");
        CHECK(Approx(p[8].first) == 996.54293059);
        CHECK(p[8].second == "nanosecond");
    }
}
