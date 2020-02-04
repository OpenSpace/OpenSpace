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

namespace openspace::autonavigation::helpers {

    // Shift and scale to a subinterval [start,end]
    double shiftAndScale(double t, double start, double end) { 
        ghoul_assert(0.0 < start && start < end  && end < 1.0, 
            "Values must be 0.0 < start < end < 1.0!");
        double tScaled = t / (end - start) - start;
        return std::max(0.0, std::min(tScaled, 1.0));
    }

} // helpers

namespace openspace::autonavigation::interpolation {
   
    // TODO: make all these into template functions

    glm::dvec3 cubicBezier(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2,
                                     const glm::dvec3 &cp3, const glm::dvec3 &cp4 )
    {
        double a = 1.0 - t;
        return cp1 * a * a * a
            + cp2 * t * a * a * 3.0
            + cp3 * t * t * a * 3.0
            + cp4 * t * t * t;
    }

    glm::dvec3 linear(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2) {
        return cp1 * (1.0 - t) + cp2 * t;
    }

    glm::dvec3 piecewiseCubicBezier(double t, const std::vector<glm::dvec3> &points) {
        size_t n = points.size();
        ghoul_assert(n > 4, "Minimum of four control points needed for interpolation!");

        ghoul_assert((n - 1) % 3 == 0, "A vector containing 3n + 1 control points must be provided!");

        size_t nrSegments = (size_t)std::floor((n - 1) / 3.0);

        // for points equally spaced in time
        double tSegment = std::fmod(t*nrSegments, 1.0);
        tSegment = std::max(0.0, std::min(tSegment, 1.0));

        size_t idx = std::floor(t*nrSegments) * 3;

        // prevent stepping past the last segment if t = 1.0
        if (idx > n - 4) {
            return points.back();
        }

        return cubicBezier(tSegment, points[idx], points[idx + 1], 
            points[idx + 2], points[idx + 3]);
    }

    glm::dvec3 piecewiseLinear(double t, const std::vector<glm::dvec3> &points) {
        size_t n = points.size();
        ghoul_assert(n > 2, "Minimum of two control points needed for interpolation!");

        size_t nrSegments = n - 1;

        // for points equally spaced in time
        double tSegment = std::fmod( t*nrSegments, 1.0 ); 
        tSegment = std::max(0.0, std::min(tSegment, 1.0));

        size_t idx = std::floor(t*nrSegments);

        // prevent stepping past the last segment if t = 1.0
        if (idx > n - 1) {
            return points.back();
        }

        return linear(tSegment, points[idx], points[idx + 1]); 
    }

} // interpolator

