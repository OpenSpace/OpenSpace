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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ANGLE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ANGLE___H__

#include <ghoul/glm.h>
#include <memory>
//#include <math.h>

namespace openspace::globebrowsing {

template <typename T>
class Angle {
public:
    /** = 0 radians = 0 degrees = no revolution */
    static const Angle<T> ZERO;

    /** = PI/2 radians = 90 degrees = quarter of a revolution */
    static const Angle<T> QUARTER;

    /** = PI radians = 180 degrees = half a revolution */
    static const Angle<T> HALF;

    /** = 2PI radians = 360 degrees = a full revolution */
    static const Angle<T> FULL;

    static Angle<T> fromRadians(T radians);
    static Angle<T> fromDegrees(T degrees);

    /** Copy constructor */
    Angle<T>(const Angle<T>& other);

public:
    inline T asRadians() const;
    inline T asDegrees() const;

    Angle<T> operator+(const Angle<T>& rhs) const;
    Angle<T> operator-(const Angle<T>& rhs) const;
    Angle<T> operator*(T rhs) const;
    Angle<T> operator/(T rhs) const;

    Angle<T> operator-() const;

    void operator+=(const Angle<T>& rhs);
    void operator-=(const Angle<T>& rhs);
    void operator*=(T rhs);
    void operator/=(T rhs);

    bool operator<(const Angle<T>& rhs) const;
    bool operator<=(const Angle<T>& rhs) const;
    bool operator>(const Angle<T>& rhs) const;
    bool operator>=(const Angle<T>& rhs) const;
    bool operator==(const Angle<T>& rhs) const;
    bool operator!=(const Angle<T>& rhs) const;

    /**
     * Normalizes the angle to the interval [0, 2pi[
     */
    Angle<T>& normalize();

    /**
    * Normalizes the angle to the interval [center - pi, center + pi[
    */
    Angle<T>& normalizeAround(const Angle<T>& center);

    /**
    * Clamps the angle to the interval [min, max].
    * Default arguments are [0, 2pi].
    */
    Angle<T>& clamp(const Angle<T>& min = ZERO, const Angle<T>& max = FULL);

    Angle<T> abs() const;

private:
    /** Private constructor. Use factory methods to avoid unit confusion */
    Angle<T>(T rad);

    T _radians;
};

using dAngle = Angle<double>;
using fAngle = Angle<float>;

} // namespace openspace::globebrowsing

#include "angle.inl"

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ANGLE___H__
