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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC2___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC2___H__

#include <ghoul/glm.h>

namespace openspace::globebrowsing {

struct Geodetic2 {
    Geodetic2(double latitude = 0.0, double longitude = 0.0);
    Geodetic2(const Geodetic2& src) = default;

    //static Geodetic2 fromCartesian(const Vec3& v);
    //Vec3 asUnitCartesian() const;

    glm::dvec2 toLonLatVec2() const;

    bool operator==(const Geodetic2& other) const;
    bool operator!=(const Geodetic2& other) const { return !(*this == (other)); }

    Geodetic2 operator+(const Geodetic2& other) const;
    Geodetic2 operator-(const Geodetic2& other) const;
    Geodetic2 operator*(double scalar) const;
    Geodetic2 operator/(double scalar) const;

    double lat;
    double lon;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC2___H__
