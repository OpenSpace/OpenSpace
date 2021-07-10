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

#include <openspace/navigation/pathhelperfunctions.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/easing.h>

namespace {
    constexpr const char* _loggerCat = "Helpers";
} // namespace

namespace openspace::helpers {

    // Shift and scale to a subinterval [start,end]
    double shiftAndScale(double t, double start, double end) {
        ghoul_assert(0.0 < start && start < end && end < 1.0,
            "Values must be 0.0 < start < end < 1.0!");
        double tScaled = t / (end - start) - start;
        return std::max(0.0, std::min(tScaled, 1.0));
    }

    glm::dquat lookAtQuaternion(glm::dvec3 eye, glm::dvec3 center, glm::dvec3 up) {
        glm::dmat4 lookAtMat = glm::lookAt(eye, center, up);
        return glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));
    }

    glm::dvec3 viewDirection(const glm::dquat& q) {
        return glm::normalize(q * glm::dvec3(0.0, 0.0, -1.0));
    };

    /*
     * Source: http://paulbourke.net/geometry/circlesphere/raysphere.c
     */
    bool lineSphereIntersection(glm::dvec3 p1, glm::dvec3 p2, glm::dvec3 center,
                                double r, glm::dvec3& intersectionPoint)
    {
        long double a, b, c;
        glm::dvec3 dp = p2 - p1;

        a = dp.x * dp.x + dp.y * dp.y + dp.z * dp.z;
        b = 2.0 * (dp.x * (p1.x - center.x) + dp.y * (p1.y - center.y) +
            dp.z * (p1.z - center.z));
        c = center.x * center.x + center.y * center.y + center.z * center.z;
        c += p1.x * p1.x + p1.y * p1.y + p1.z * p1.z;
        c -= 2.0 * (center.x * p1.x + center.y * p1.y + center.z * p1.z);
        c -= r * r;

        long double intersectionTest = b * b - 4.0 * a * c;

        // No intersection
        if (std::abs(a) < 0 || intersectionTest < 0.0) {
            return false;
        }
        // Intersection
        else {
            // Only care about the first intersection point if we have two
            const double t = (-b - std::sqrt(intersectionTest)) / (2.0 *a);

            // Check if utside of line segment between p1 and p2
            if (t <= 0 || t >= 1.0) {
                return false;
            }

            intersectionPoint = p1 + t * dp;
            return true;
        }
    }

    bool isPointInsideSphere(const glm::dvec3& p, const glm::dvec3& c, double r) {
        const glm::dvec3 v = c - p;
        const long double squaredDistance = v.x * v.x + v.y * v.y + v.z * v.z;
        const long double squaredRadius = r * r;

        return (squaredDistance <= squaredRadius);
    }

    double simpsonsRule(double t0, double t1, int n, std::function<double(double)> f) {
        ghoul_assert(n % 2 == 0, "n must be an even number");
        ghoul_assert(n >= 2, "Number of partitions, n, must be at least 2");

        const double h = (t1 - t0) / static_cast<double>(n);
        const double endpoints = f(t0) + f(t1);
        double times4 = 0.0;
        double times2 = 0.0;

        // weight 4
        for (int i = 1; i < n; i += 2) {
            double t = t0 + i * h;
            times4 += f(t);
        }

        // weight 2
        for (int i = 2; i < n; i += 2) {
            double t = t0 + i * h;
            times2 += f(t);
        }

        return (h / 3) * (endpoints + 4 * times4 + 2 * times2);
    }

    double fivePointGaussianQuadrature(double t0, double t1,
                                       std::function<double(double)> f)
    {
        struct GaussLengendreCoefficient {
            double abscissa; // xi
            double weight;   // wi
        };

        static constexpr GaussLengendreCoefficient coefficients[] = {
            { 0.0, 0.5688889 },
            { -0.5384693, 0.47862867 },
            { 0.5384693, 0.47862867 },
            { -0.90617985, 0.23692688 },
            { 0.90617985, 0.23692688 }
        };

        const double a = t0;
        const double b = t1;
        double sum = 0.0;
        for (auto coefficient : coefficients) {
            // change of interval to [a, b] from [-1, 1] (also 0.5 * (b - a) below)
            double const t = 0.5 * ((b - a) * coefficient.abscissa + (b + a));
            sum += f(t) * coefficient.weight;
        }
        return 0.5 * (b - a) * sum;
    }

} // namespace openspace::helpers

namespace openspace::splines {

    // Based on implementation by Mika Rantanen, but without tension 
    // https://qroph.github.io/2018/07/30/smooth-paths-using-catmull-rom-splines.html
    glm::dvec3 catmullRom(double t, const glm::dvec3& p0, const glm::dvec3& p1,
                          const glm::dvec3& p2, const glm::dvec3& p3, double alpha)
    {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");

        double t01 = std::pow(glm::distance(p0, p1), alpha);
        double t12 = std::pow(glm::distance(p1, p2), alpha);
        double t23 = std::pow(glm::distance(p2, p3), alpha);

        constexpr const double Epsilon = 1E-7;
        const glm::dvec3 zero = glm::dvec3(0.0);
        glm::dvec3 m01 = (t01 > Epsilon) ? (p1 - p0) / t01 : zero;
        glm::dvec3 m23 = (t23 > Epsilon) ? (p3 - p2) / t23 : zero;
        glm::dvec3 m02 = (t01 + t12 > Epsilon) ? (p2 - p0) / (t01 + t12) : zero;
        glm::dvec3 m13 = (t12 + t23 > Epsilon) ? (p3 - p1) / (t12 + t23) : zero;

        glm::dvec3 m1 = p2 - p1 + t12 * (m01 - m02);
        glm::dvec3 m2 = p2 - p1 + t12 * (m23 - m13);

        glm::dvec3 a = 2.0 * (p1 - p2) + m1 + m2;
        glm::dvec3 b = -3.0 * (p1 - p2) - m1 - m1 - m2;
        glm::dvec3 c = m1;
        glm::dvec3 d = p1;

        return a * t * t * t
            + b * t * t
            + c * t
            + d;
    }

    glm::dvec3 cubicBezier(double t, const glm::dvec3& p0, const glm::dvec3& p1,
                           const glm::dvec3& p2, const glm::dvec3& p3)
    {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");

        double a = 1.0 - t;
        return p0 * a * a * a
            + p1 * t * a * a * 3.0
            + p2 * t * t * a * 3.0
            + p3 * t * t * t;
    }

    glm::dvec3 linear(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2) {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");
        return cp1 * (1.0 - t) + cp2 * t;
    }
} // namespace openspace::splines
