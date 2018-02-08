/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___DISTANCECONVERSION___H__
#define __OPENSPACE_CORE___DISTANCECONVERSION___H__

#include <array>
#include <string>
#include <utility>

namespace openspace {

enum class DistanceUnit {
    Nanometer = 0,
    Micrometer,
    Millimeter,
    Meter,
    Kilometer,
    AU,
    Lighthour,
    Lightday,
    Lightmonth,
    Lightyear,
    Parsec,
    Kiloparsec,
    Megaparsec,
    Gigaparsec
};
constexpr std::array<DistanceUnit, static_cast<int>(DistanceUnit::Gigaparsec) + 1>
DistanceUnits = {
    DistanceUnit::Nanometer, DistanceUnit::Micrometer, DistanceUnit::Millimeter,
    DistanceUnit::Meter, DistanceUnit::Kilometer, DistanceUnit::AU,
    DistanceUnit::Lighthour, DistanceUnit::Lightday, DistanceUnit::Lightmonth,
    DistanceUnit::Lightyear, DistanceUnit::Parsec, DistanceUnit::Kiloparsec,
    DistanceUnit::Megaparsec, DistanceUnit::Gigaparsec
};

std::pair<double, std::string> simplifyDistance(double meters,
    bool forceSingularForm = false);

double convertDistance(double meters, DistanceUnit requestedUnit);

std::string nameForDistanceUnit(DistanceUnit unit, bool pluralForm = false);
DistanceUnit distanceUnitFromString(const std::string& unit);

} // namespace openspace

#endif // __OPENSPACE_CORE___DISTANCECONVERSION___H__
