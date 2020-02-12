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

#ifndef __OPENSPACE_MODULE___EASINGFUNCTIONS___H__
#define __OPENSPACE_MODULE___EASINGFUNCTIONS___H__

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/logging/logmanager.h>
#include <vector>
#include <cmath>
#include <algorithm> 

namespace openspace::autonavigation::helpers {

    // Make interpolator parameter t [0,1] progress only inside a subinterval
    double shiftAndScale(double t, double newStart, double newEnd);

    glm::dquat getLookAtQuaternion(glm::dvec3 eye, glm::dvec3 center, glm::dvec3 up);

} // helpers

// TODO: include interpolator.h to helperfunctions
// error when interpolator.h is included and used both here and in pathsegment

// TODO: also make these template functions instead

namespace openspace::autonavigation::interpolation {

    glm::dvec3 cubicBezier(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2, 
                                     const glm::dvec3 &cp3, const glm::dvec3 &cp4);

    glm::dvec3 linear(double t, const glm::dvec3 &cp1, const glm::dvec3 &cp2);

    // TODO: remove
    glm::dvec3 piecewiseLinear(double t, const std::vector<glm::dvec3> &controlPoints);

} // namespace
#endif
