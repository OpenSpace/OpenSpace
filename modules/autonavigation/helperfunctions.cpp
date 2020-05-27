/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/autonavigation/helperfunctions.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "Helpers";
    const double Epsilon = 1E-7;
} // namespace

namespace openspace::autonavigation::helpers {

    // Shift and scale to a subinterval [start,end]
    double shiftAndScale(double t, double start, double end) {
        ghoul_assert(0.0 < start && start < end&& end < 1.0,
            "Values must be 0.0 < start < end < 1.0!");
        double tScaled = t / (end - start) - start;
        return std::max(0.0, std::min(tScaled, 1.0));
    }

    glm::dquat getLookAtQuaternion(glm::dvec3 eye, glm::dvec3 center, glm::dvec3 up) {
        glm::dmat4 lookAtMat = glm::lookAt(eye, center, up);
        return glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));
    }

    /*
       Calculate the intersection of a line and a sphere
       The line segment is defined from p1 to p2
       The sphere is of radius r and centered at sc
       There are potentially two points of intersection given by
       p = p1 + mu1 (p2 - p1)
       p = p1 + mu2 (p2 - p1)
       Source: http://paulbourke.net/geometry/circlesphere/raysphere.c
    */
    bool lineSphereIntersection(glm::dvec3 p1, glm::dvec3 p2, glm::dvec3 sc, double r, 
                                                         glm::dvec3& intersectionPoint)
    {   
        long double a, b, c;
        glm::dvec3 dp = p2 - p1;

        a = dp.x * dp.x + dp.y * dp.y + dp.z * dp.z;
        b = 2 * (dp.x * (p1.x - sc.x) + dp.y * (p1.y - sc.y) + dp.z * (p1.z - sc.z));
        c = sc.x * sc.x + sc.y * sc.y + sc.z * sc.z;
        c += p1.x * p1.x + p1.y * p1.y + p1.z * p1.z;
        c -= 2 * (sc.x * p1.x + sc.y * p1.y + sc.z * p1.z);
        c -= r * r;

        long double intersectionTest = b * b - 4.0 * a * c;

        // no intersection
        if (std::abs(a) < Epsilon || intersectionTest < 0.0) {
            return false;
        }
        else {
            // only care about the first intersection point if we have two
            double t = (-b - std::sqrt(intersectionTest)) / (2.0 *a);

            if (t <= Epsilon || t >= abs(1.0 - Epsilon)) return false;

            intersectionPoint = p1 + t * dp;
            return true;
        }
    }

    bool isPointInsideSphere(const glm::dvec3& p, const glm::dvec3& c, double r) {
        glm::dvec3 v = c - p;
        long double squaredDistance = v.x * v.x + v.y * v.y + v.z * v.z;
        long double squaredRadius = r * r;
        if (squaredDistance <= squaredRadius) {
            return true;
        }
        return false;
    }

} // helpers

namespace openspace::autonavigation::interpolation {

    // Based on implementation by Mika Rantanen https://qroph.github.io/2018/07/30/smooth-paths-using-catmull-rom-splines.html
    glm::dvec3 catmullRom(double t, const glm::dvec3& p0, const glm::dvec3& p1,
        const glm::dvec3& p2, const glm::dvec3& p3, double alpha)
    {
        glm::dvec3 m01, m02, m23, m13;

        double t01 = pow(glm::distance(p0, p1), alpha);
        double t12 = pow(glm::distance(p1, p2), alpha);
        double t23 = pow(glm::distance(p2, p3), alpha);

        m01 = (t01 > Epsilon) ? (p1 - p0) / t01 : glm::dvec3{};
        m23 = (t23 > Epsilon) ? (p3 - p2) / t23 : glm::dvec3{};
        m02 = (t01 + t12 > Epsilon) ? (p2 - p0) / (t01 + t12) : glm::dvec3{};
        m13 = (t12 + t23 > Epsilon) ? (p3 - p1) / (t12 + t23) : glm::dvec3{};

        glm::dvec3 m1 = p2 - p1 + t12 * (m01 - m02);
        glm::dvec3 m2 = p2 - p1 + t12 * (m23 - m13);

        glm::dvec3 a = 2.0 * (p1 - p2) + m1 + m2;
        glm::dvec3 b = -3.0 * (p1 - p2) - m1 - m1 - m2;
        glm::dvec3 c = m1;
        glm::dvec3 d = p1;

        return
            a * t * t * t +
            b * t * t +
            c * t +
            d;
    }

    glm::dvec3 cubicBezier(double t, const glm::dvec3& cp1, const glm::dvec3& cp2,
        const glm::dvec3& cp3, const glm::dvec3& cp4)
    {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");

        double a = 1.0 - t;
        return cp1 * a * a * a
            + cp2 * t * a * a * 3.0
            + cp3 * t * t * a * 3.0
            + cp4 * t * t * t;
    }

    glm::dvec3 linear(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2) {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");
        return cp1 * (1.0 - t) + cp2 * t;
    }

    glm::dvec3 hermite(double t, const glm::dvec3 &p1, const glm::dvec3 &p2,
                       const glm::dvec3 &tangent1, const glm::dvec3 &tangent2)
    {
        ghoul_assert(t >= 0 && t <= 1.0, "Interpolation variable out of range [0, 1]");

        if (t <= 0.0) return p1;
        if (t >= 1.0) return p2;

        const double t2 = t * t;
        const double t3 = t2 * t;

        // calculate basis functions
        double const a0 = (2.0*t3) - (3.0*t2) + 1.0;
        double const a1 = (-2.0*t3) + (3.0*t2);
        double const b0 = t3 - (2.0*t2) + t;
        double const b1 = t3 - t2;

        return (a0 * p1) + (a1 * p2) + (b0 * tangent1) + (b1 * tangent2);
    }

    // uniform if tKnots are equally spaced, or else non uniform
    glm::dvec3 piecewiseCubicBezier(double t, const std::vector<glm::dvec3>& points, 
                                                  const std::vector<double>& tKnots) 
    {
        ghoul_assert(points.size() > 4, "Minimum of four control points needed for interpolation!");
        ghoul_assert((points.size() - 1) % 3 == 0, "A vector containing 3n + 1 control points must be provided!");
        int nrSegments = (points.size() - 1) / 3;
        ghoul_assert(nrSegments == (tKnots.size() - 1), "Number of interval times must match number of segments");

        if (t <= 0.0) return points.front();
        if (t >= 1.0) return points.back();

        // compute current segment index
        std::vector<double>::const_iterator segmentEndIt =
            std::lower_bound(tKnots.begin(), tKnots.end(), t);
        unsigned int segmentIdx = (segmentEndIt - 1) - tKnots.begin();

        double segmentStart = tKnots[segmentIdx];
        double segmentDuration = (tKnots[segmentIdx + 1] - tKnots[segmentIdx]);
        double tScaled = (t - segmentStart) / segmentDuration;

        unsigned int idx = segmentIdx * 3;

        // Interpolate using De Casteljau's algorithm
        return interpolation::cubicBezier(tScaled, points[idx], points[idx + 1],
            points[idx + 2], points[idx + 3]);
    }

    glm::dvec3 piecewiseLinear(double t, const std::vector<glm::dvec3>& points, 
                                             const std::vector<double>& tKnots)
    {
        ghoul_assert(points.size() == tKnots.size(), "Must have equal number of points and times!");
        ghoul_assert(points.size() > 2, "Minimum of two control points needed for interpolation!");

        size_t nrSegments = points.size() - 1;

        if (t <= 0.0) return points.front();
        if (t >= 1.0) return points.back();

        // compute current segment index
        std::vector<double>::const_iterator segmentEndIt =
            std::lower_bound(tKnots.begin(), tKnots.end(), t);
        unsigned int idx = (segmentEndIt - 1) - tKnots.begin();

        double segmentStart = tKnots[idx];
        double segmentDuration = (tKnots[idx + 1] - tKnots[idx]);
        double tScaled = (t - segmentStart) / segmentDuration;

        return interpolation::linear(tScaled, points[idx], points[idx + 1]);
    }

} // interpolation
