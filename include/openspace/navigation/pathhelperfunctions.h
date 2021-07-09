/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___PATHHELPERFUNCTIONS___H__
#define __OPENSPACE_CORE___PATHHELPERFUNCTIONS___H__

#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <cmath>
#include <functional>
#include <vector>

namespace openspace::helpers {

    // Make interpolator parameter t [0,1] progress only inside a subinterval
    double shiftAndScale(double t, double newStart, double newEnd);

    /*
     * Compute a quaternion that represents the rotation looking from \p eye to
     * \p center, with the specified \p up direction
     */
    glm::dquat lookAtQuaternion(glm::dvec3 eye, glm::dvec3 center, glm::dvec3 up);

    /*
     * Compute a view direction vector from a quaternion representing a rotation
     */
    glm::dvec3 viewDirection(const glm::dquat& q);

    /*
     * Calculate the intersection of a line and a sphere.
     * The line segment is defined from \p p1 to \p p2.
     * The sphere is defined by the radius \p r and center point \p center.
     * The resulting intersection point is stored in the \p intersectionPoint parameter.
     *
     * In the case of two intersection points, only care anout the first one.
     *
     * \return True if the line between \p p1 and \p p2 intersects the sphere given by
     *         \p r and \p center, and false otherwise
     */
    bool lineSphereIntersection(glm::dvec3 p1, glm::dvec3 p2, glm::dvec3 center,
        double r, glm::dvec3& intersectionPoint);

    /*
     * Check if the point p is inside of the sphere defined by radius r and center
     * point c
     */
    bool isPointInsideSphere(const glm::dvec3& p, const glm::dvec3& c, double r);

    /*
     * Approximate integral of function f over inteval [t0, t1] using Simpson's rule.
     * The integer n is the number of partitions and must be an even number.
     */
    double simpsonsRule(double t0, double t1, int n, std::function<double(double)> f);

    /*
     * Approximate integral of function f over inteval [t0, t1] using
     * 5-point Gauss-Legendre quadrature
     * https://en.wikipedia.org/wiki/Gaussian_quadrature
     */
    double fivePointGaussianQuadrature(double t0, double t1,
        std::function<double(double)> f);

} // namespace openspace::helpers

namespace openspace::splines {

    // TODO: Move these to ghoul's interpolator file (and make template versions)

    /*
     * Catmull-Rom curve interpolation based on implementation by Mika Rantanen
     * https://qroph.github.io/2018/07/30/smooth-paths-using-catmull-rom-splines.html
     * Centripetal version alpha = 0, uniform for alpha = 0.5 and chordal for alpha = 1
     */
    glm::dvec3 catmullRom(double t, const glm::dvec3& p0, const glm::dvec3& p1,
        const glm::dvec3& p2, const glm::dvec3& p3, double alpha = 0.5);

    /*
     * Compute the interpolation along the cubic Bézier curve defined by the points p0, 
     * p1, p2, and p3. The curve will pass through p0 and p3
     */
    glm::dvec3 cubicBezier(double t, const glm::dvec3& p0, const glm::dvec3& p1,
        const glm::dvec3& p2, const glm::dvec3& p3);

    glm::dvec3 linear(double t, const glm::dvec3& cp1, const glm::dvec3& cp2);

} // namespace openspace::splines
#endif // __OPENSPACE_CORE___PATHHELPERFUNCTIONS___H__
