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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC3___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC3___H__

namespace openspace::globebrowsing {

struct Geodetic2 {
    Geodetic2(double latitude = 0.0, double longitude = 0.0)
        : lat(latitude)
        , lon(longitude)
    {}

    Geodetic2 operator+(const Geodetic2& other) const {
        return Geodetic2(lat + other.lat, lon + other.lon);
    }

    Geodetic2 operator-(const Geodetic2& other) const {
        return Geodetic2(lat - other.lat, lon - other.lon);
    }

    Geodetic2 operator*(double scalar) const {
        return Geodetic2(lat * scalar, lon * scalar);
    }

    Geodetic2 operator/(double scalar) const {
        return Geodetic2(lat / scalar, lon / scalar);
    }

    double lat;
    double lon;
};

struct Geodetic3 {
    Geodetic2 geodetic2;
    double height;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEODETIC3___H__
