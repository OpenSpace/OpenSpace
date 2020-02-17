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

    glm::dquat getLookAtQuaternion(glm::dvec3 eye, glm::dvec3 center, glm::dvec3 up) {
        glm::dmat4 lookAtMat = glm::lookAt(eye, center, up);
        return glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));
    }

} // helpers

namespace openspace::autonavigation::interpolation {

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

} // interpolation

