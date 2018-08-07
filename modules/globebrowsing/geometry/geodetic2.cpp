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

#include <modules/globebrowsing/geometry/geodetic2.h>

namespace openspace::globebrowsing {

Geodetic2::Geodetic2(double latitude, double longitude)
    : lat(latitude)
    , lon(longitude)
{}

glm::dvec2 Geodetic2::toLonLatVec2() const {
    return glm::dvec2(lon, lat);
}

bool Geodetic2::operator==(const Geodetic2& other) const {
    return lat == other.lat && lon == other.lon;
}

Geodetic2 Geodetic2::operator+(const Geodetic2& other) const {
    return Geodetic2(lat + other.lat, lon + other.lon);
}

Geodetic2 Geodetic2::operator-(const Geodetic2& other) const {
    return Geodetic2(lat - other.lat, lon - other.lon);
}

Geodetic2 Geodetic2::operator*(double scalar) const {
    return Geodetic2(lat * scalar, lon * scalar);
}

Geodetic2 Geodetic2::operator/(double scalar) const {
    return Geodetic2(lat / scalar, lon / scalar);
}

} // namespace openspace::globebrowsing
