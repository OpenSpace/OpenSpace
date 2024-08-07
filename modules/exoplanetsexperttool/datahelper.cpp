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

#include <modules/exoplanetsexperttool/datahelper.h>

#include <charconv>
#include <cmath>

namespace openspace::data {

float parseFloatData(const std::string& str) {
#ifdef WIN32
    float result;
    auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
    if (ec == std::errc()) {
        return result;
    }
    return std::numeric_limits<float>::quiet_NaN();
#else
    // clang is missing float support for std::from_chars
    return !str.empty() ? std::stof(str.c_str(), nullptr) : NAN;
#endif
};

double parseDoubleData(const std::string& str) {
#ifdef WIN32
    double result;
    auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
    if (ec == std::errc()) {
        return result;
    }
    return std::numeric_limits<double>::quiet_NaN();
#else
    // clang is missing double support for std::from_chars
    return !str.empty() ? std::stod(str.c_str(), nullptr) : NAN;
#endif
};

int parseIntegerData(const std::string& str) {
    int result;
    auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
    if (ec == std::errc()) {
        return result;
    }
    return std::numeric_limits<int>::quiet_NaN();
};

bool compareValuesWithNan(float lhs, float rhs) {
    if (std::isnan(lhs)) {
        // nan should be considered smaller than non-nan values
        if (!std::isnan(rhs)) {
            return true;
        }
        // if both are nan, return false
        return false;
    }

    // rhs is nan, but not lhs
    if (std::isnan(rhs)) {
        return false;
    }
    return lhs < rhs;
}

bool caseInsensitiveLessThan(const char* lhs, const char* rhs) {
    int res = _stricmp(lhs, rhs);
    return res <= 0;
}

} // namespace openspace::data
