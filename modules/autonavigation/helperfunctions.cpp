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

namespace openspace::autonavigation::easingfunctions {

double linear(double t) { return t; };

double step(double t) { return (t > 0.5); };

double circularEaseOut(double p){
    return sqrt((2 - p) * p);
}

double cubicEaseIn(double t) { return (t*t*t); };

double cubicEaseOut(double t) {
    double p = 1 - t;
    return (p*p*p);
}

double cubicEaseInOut(double t) {
    if (t < 0.5) {
        return 4 * t * t * t;
    }
    else {
        double f = ((2 * t) - 2);
        return 0.5 * f * f * f + 1;
    }
}

double quadraticEaseInOut(double t) {
    if (t < 0.5) {
        return 2 * t * t;
    }
    else {
        return (-2 * t * t) + (4 * t) - 1;
    }
}

double exponentialEaseInOut(double t) {
    if (t == 0.0 || t == 1.0) return t;

    if (t < 0.5) {
        return 0.5 * glm::pow(2, (20 * t) - 10);
    }
    else {
        return -0.5 * glm::pow(2, (-20 * t) + 10) + 1;
    }
}

} // easingfunctions

namespace openspace::autonavigation::interpolator {
   
    // TODO: make into template function
    glm::dvec3 cubicBezier(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2,
                                     const glm::dvec3 &cp3, const glm::dvec3 &cp4 )
    {
        double a = 1.0 - t;
        return cp1 * a * a * a
            + cp2 * t * a * a * 3.0
            + cp3 * t * t * a * 3.0
            + cp4 * t * t * t;
    }

    glm::dvec3 piecewiseCubicBezier(double t, const std::vector<glm::dvec3> &controlPoints) {
        size_t n = controlPoints.size();
        ghoul_assert(n > 4, "Minimum of four control points needed for interpolation!");

        double n_seg = (n - 1.0) / 3.0;
        ghoul_assert(std::fmod(n_seg, 1.0) == 0, "A vector containing 3n + 1 control points must be provided!");

        // for control points equally spaced in time
        double t_seg = std::fmod(t * n_seg, 1.0);
        t_seg = std::max(0.0, std::min(t_seg, 1.0));

        size_t idx = std::floor(t * n_seg) * 3;

        return  cubicBezier(t_seg, controlPoints[idx], controlPoints[idx + 1],
            controlPoints[idx + 2], controlPoints[idx + 3]);
    }

    glm::dvec3 piecewiseLinear(double t, const std::vector<glm::dvec3> &controlPoints) {
        size_t n = controlPoints.size();
        ghoul_assert(n > 2, "Minimum of two control points needed for interpolation!");

        size_t n_seg = n - 1;

        // for control points equally spaced in time
        double t_seg = std::fmod( t*n_seg, 1.0 ); 
        t_seg = std::max(0.0, std::min(t_seg, 1.0));

        size_t idx = std::floor(t*n_seg);

        return  (1.0 - t_seg) * controlPoints[idx] + t_seg * controlPoints[idx + 1];
    }

} // interpolator

