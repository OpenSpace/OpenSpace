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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___ROTATIONINTERPOLATOR___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___ROTATIONINTERPOLATOR___H__

#include <modules/autonavigation/camerastate.h>

namespace openspace::autonavigation {

enum RotationMethod {
    Slerp,
    PiecewiseSlerp,
    Fixed,
    LookAt
};

class PathCurve;

class RotationInterpolator {
public:
    RotationInterpolator() = default;
    RotationInterpolator(const CameraState& start, const CameraState& end, 
        PathCurve* curve, RotationMethod method);

    glm::dquat rotationAt(double u);

private:
    CameraState _start;
    CameraState _end;
    PathCurve* _curve = nullptr;
    RotationMethod _method;

    glm::dquat easedSlerp(double u);
    glm::dquat lookAtInterpolator(double u); // for debug
    glm::dquat piecewiseSlerp(double u);
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___ROTATIONINTERPOLATOR___H__
