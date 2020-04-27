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

#include <ghoul/glm.h>

namespace openspace::autonavigation {

class PathCurve;

class RotationInterpolator {
public:
    RotationInterpolator() = default;
    RotationInterpolator(glm::dquat start, glm::dquat end);

    virtual glm::dquat interpolate(double u) = 0;

protected:
    glm::dquat _start;
    glm::dquat _end;
};

class EasedSlerpInterpolator : public RotationInterpolator {
public:
    EasedSlerpInterpolator(glm::dquat start, glm::dquat end);
    glm::dquat interpolate(double u);
};

class LookAtInterpolator : public RotationInterpolator {
public:
    LookAtInterpolator(glm::dquat start, glm::dquat end, glm::dvec3 startLookAtPos,
        glm::dvec3 endLookAtPos, PathCurve* path);
    glm::dquat interpolate(double u);

private:
    glm::dvec3 _startLookAtPos;
    glm::dvec3 _endLookAtPos;
    PathCurve* _path = nullptr;
};

class PiecewiseLookAtInterpolator : public RotationInterpolator {
public:
    PiecewiseLookAtInterpolator(glm::dquat start, glm::dquat end,
        glm::dvec3 startTargetPos, glm::dvec3 endTargetPos, PathCurve* path);
    glm::dquat interpolate(double u);

private:
    glm::dvec3 _startTargetPos;
    glm::dvec3 _endTargetPos;
    PathCurve* _path = nullptr;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___ROTATIONINTERPOLATOR___H__
