/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/util/timeconversion.h>

using namespace openspace;

TEST_CASE("TimeConversion: Simplify Time Round", "[timeconversion]") {
    {
        std::pair<double, std::string_view> p = simplifyTime(0.0, false);
        CHECK(p.first == 0.0);
        CHECK(p.second == "Seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(0.0, true);
        CHECK(p.first == 0.0);
        CHECK(p.second == "Second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-9, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Nanoseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-9, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Nanosecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-6, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Microseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-6, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Microsecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-3, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Milliseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2e-3, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Millisecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(2.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(120.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Minutes");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(120.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Minute");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(7200.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Hours");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(7200.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Hour");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(172800.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Days");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(172800.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Day");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(5259492.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Months");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(5259492.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Month");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(63113904.0, false);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Years");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(63113904.0, true);
        CHECK(p.first == 2.0);
        CHECK(p.second == "Year");
    }
}

TEST_CASE("TimeConversion: Simplify Time Fractional", "[timeconversion]") {
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-10, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Nanoseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-10, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Nanosecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-7, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Microseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-7, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Microsecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-4, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Milliseconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(32e-4, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Millisecond");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(3.2, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Seconds");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(3.2, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Second");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(192.0, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Minutes");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(192.0, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Minute");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(11520.0, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Hours");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(11520.0, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Hour");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(276480.0, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Days");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(276480.0, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Day");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(8415187.2, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Months");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(8415187.2, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Month");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(100982246.4, false);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Years");
    }
    {
        std::pair<double, std::string_view> p = simplifyTime(100982246.4, true);
        CHECK(Catch::Approx(p.first) == 3.2);
        CHECK(p.second == "Year");
    }
}

TEST_CASE("TimeConversion: Split Time Multiple Round", "[timeconversion]") {
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(0.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 0.0);
        CHECK(p[0].second == "Seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(0.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 0.0);
        CHECK(p[0].second == "Second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-9, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-9, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-6, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Microseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-6, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Microsecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-3, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Milliseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2e-3, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Millisecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(2.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(120.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(120.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(7200.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Hours");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(7200.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(172800.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Days");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(172800.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Day");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(5259492.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Months");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(5259492.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Month");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(63113904.0, false);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Years");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(63113904.0, true);
        REQUIRE(p.size() == 1);
        CHECK(p[0].first == 2.0);
        CHECK(p[0].second == "Year");
    }
}

TEST_CASE("TimeConversion: Split Time Fractional", "[timeconversion]") {
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-10, false);
        REQUIRE(p.size() == 1);
        CHECK(Catch::Approx(p[0].first) == 3.2);
        CHECK(p[0].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-10, true);
        REQUIRE(p.size() == 1);
        CHECK(Catch::Approx(p[0].first) == 3.2);
        CHECK(p[0].second == "Nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-7, false);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Microseconds");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-7, true);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Microsecond");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-4, false);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Milliseconds");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Microseconds");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(32e-4, true);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Millisecond");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Microsecond");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "Nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(3.2, false);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Seconds");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Milliseconds");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(3.2, true);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Second");
        CHECK(Catch::Approx(p[1].first) == 200.0);
        CHECK(p[1].second == "Millisecond");
        // This is some floating point inaccuracy
        CHECK(p[2].first < 1e-3);
        CHECK(p[2].second == "Nanosecond");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(192.0, false);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Minutes");
        CHECK(Catch::Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "Seconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(192.0, true);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Minute");
        CHECK(Catch::Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "Second");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(11520.0, false);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Hours");
        CHECK(Catch::Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "Minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(11520.0, true);
        REQUIRE(p.size() == 2);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Hour");
        CHECK(Catch::Approx(p[1].first) == 12.0);
        CHECK(p[1].second == "Minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(276480.0, false);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Days");
        CHECK(Catch::Approx(p[1].first) == 4);
        CHECK(p[1].second == "Hours");
        CHECK(Catch::Approx(p[2].first) == 48);
        CHECK(p[2].second == "Minutes");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(276480.0, true);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Day");
        CHECK(Catch::Approx(p[1].first) == 4);
        CHECK(p[1].second == "Hour");
        CHECK(Catch::Approx(p[2].first) == 48);
        CHECK(p[2].second == "Minute");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(8414838.0, false);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Months");
        CHECK(Catch::Approx(p[1].first) == 6.0);
        CHECK(p[1].second == "Days");
        CHECK(Catch::Approx(p[2].first) == 2.0);
        CHECK(p[2].second == "Hours");

    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(8414838.0, true);
        REQUIRE(p.size() == 3);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Month");
        CHECK(Catch::Approx(p[1].first) == 6.0);
        CHECK(p[1].second == "Day");
        CHECK(Catch::Approx(p[2].first) == 2.0);
        CHECK(p[2].second == "Hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981548.0, false);
        REQUIRE(p.size() == 4);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Years");
        CHECK(Catch::Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "Months");
        CHECK(Catch::Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "Days");
        CHECK(Catch::Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "Hours");
    }
    {
        std::vector<std::pair<double, std::string_view>> p = splitTime(100981548.0, true);
        REQUIRE(p.size() == 4);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Year");
        CHECK(Catch::Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "Month");
        CHECK(Catch::Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "Day");
        CHECK(Catch::Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "Hour");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981676.388, false);
        REQUIRE(p.size() == 9);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Years");
        CHECK(Catch::Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "Months");
        CHECK(Catch::Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "Days");
        CHECK(Catch::Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "Hours");
        CHECK(Catch::Approx(p[4].first) == 2.0);
        CHECK(p[4].second == "Minutes");
        CHECK(Catch::Approx(p[5].first) == 8.0);
        CHECK(p[5].second == "Seconds");
        CHECK(Catch::Approx(p[6].first) == 387.0);
        CHECK(p[6].second == "Milliseconds");
        CHECK(Catch::Approx(p[7].first) == 999.0);
        CHECK(p[7].second == "Microseconds");
        CHECK(Catch::Approx(p[8].first) == 996.54293059);
        CHECK(p[8].second == "Nanoseconds");
    }
    {
        std::vector<std::pair<double, std::string_view>> p =
            splitTime(100981676.388, true);
        REQUIRE(p.size() == 9);
        CHECK(Catch::Approx(p[0].first) == 3.0);
        CHECK(p[0].second == "Year");
        CHECK(Catch::Approx(p[1].first) == 2.0);
        CHECK(p[1].second == "Month");
        CHECK(Catch::Approx(p[2].first) == 12.0);
        CHECK(p[2].second == "Day");
        CHECK(Catch::Approx(p[3].first) == 4.0);
        CHECK(p[3].second == "Hour");
        CHECK(Catch::Approx(p[4].first) == 2.0);
        CHECK(p[4].second == "Minute");
        CHECK(Catch::Approx(p[5].first) == 8.0);
        CHECK(p[5].second == "Second");
        CHECK(Catch::Approx(p[6].first) == 387.0);
        CHECK(p[6].second == "Millisecond");
        CHECK(Catch::Approx(p[7].first) == 999.0);
        CHECK(p[7].second == "Microsecond");
        CHECK(Catch::Approx(p[8].first) == 996.54293059);
        CHECK(p[8].second == "Nanosecond");
    }
}
