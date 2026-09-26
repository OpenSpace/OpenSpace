/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/misc/interpolator.h>

#include <ghoul/misc/assert.h>
#include <glm/gtx/quaternion.hpp>

namespace ghoul {

template <>
glm::quat interpolateLinear(double t, const glm::quat& p0, const glm::quat& p1) {
    return glm::slerp(p0, p1, static_cast<float>(t));
}

template <>
glm::dquat interpolateLinear(double t, const glm::dquat& p0, const glm::dquat& p1) {
    return glm::slerp(p0, p1, t);
}

// Based on implementation by Mika Rantanen, but without tension
// https://qroph.github.io/2018/07/30/smooth-paths-using-catmull-rom-splines.html
glm::dvec3 interpolateCatmullRom(double t, const glm::dvec3& p0, const glm::dvec3& p1,
                                 const glm::dvec3& p2, const glm::dvec3& p3, double alpha)
{
    ghoul_assert(t >= 0.0 && t <= 1.0, "Interpolation variable must be in range [0,1]");
    ghoul_assert(alpha >= 0.0 && alpha <= 1.0, "Alpha must be in range [0,1]");

    const double t01 = std::pow(glm::distance(p0, p1), alpha);
    const double t12 = std::pow(glm::distance(p1, p2), alpha);
    const double t23 = std::pow(glm::distance(p2, p3), alpha);

    constexpr double Epsilon = 1E-7;
    const glm::dvec3 zero = glm::dvec3(0.0);
    const glm::dvec3 m01 = (t01 > Epsilon) ? (p1 - p0) / t01 : zero;
    const glm::dvec3 m23 = (t23 > Epsilon) ? (p3 - p2) / t23 : zero;
    const glm::dvec3 m02 = (t01 + t12 > Epsilon) ? (p2 - p0) / (t01 + t12) : zero;
    const glm::dvec3 m13 = (t12 + t23 > Epsilon) ? (p3 - p1) / (t12 + t23) : zero;

    const glm::dvec3 m1 = p2 - p1 + t12 * (m01 - m02);
    const glm::dvec3 m2 = p2 - p1 + t12 * (m23 - m13);

    const glm::dvec3 a = 2.0 * (p1 - p2) + m1 + m2;
    const glm::dvec3 b = -3.0 * (p1 - p2) - m1 - m1 - m2;
    const glm::dvec3& c = m1;
    const glm::dvec3& d = p1;

    return a * t * t * t + b * t * t + c * t + d;
}

} // namespace ghoul
