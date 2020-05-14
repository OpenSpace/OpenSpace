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

#include <modules/autonavigation/rotationinterpolator.h>

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/pathcurves.h>
#include <modules/autonavigation/waypoint.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

namespace {
    constexpr const char* _loggerCat = "RotationInterpolator";
} // namespace

namespace openspace::autonavigation {

RotationInterpolator::RotationInterpolator(const glm::dquat start, const glm::dquat end)
    : _start(start), _end(end)
{}

EasedSlerpInterpolator::EasedSlerpInterpolator(glm::dquat start, glm::dquat end)
    : RotationInterpolator(start, end)
{}

glm::dquat EasedSlerpInterpolator::interpolate(double u) {
    double uScaled = helpers::shiftAndScale(u, 0.1, 0.9);
    uScaled = ghoul::cubicEaseInOut(uScaled);
    return glm::slerp(_start, _end, uScaled);
}

LookAtRotator::LookAtRotator(glm::dquat start, glm::dquat end, 
                                       glm::dvec3 startLookAtPos, 
                                       glm::dvec3 endLookAtPos, 
                                       PathCurve* path)
    : RotationInterpolator(start, end),
    _startLookAtPos(startLookAtPos), 
    _endLookAtPos(endLookAtPos), 
    _path(path)
{}

glm::dquat LookAtRotator::interpolate(double u) {
    double tStart = 0.15;
    double tEnd = 0.7;
    double uNew = helpers::shiftAndScale(u, tStart, tEnd);
    uNew = ghoul::cubicEaseInOut(uNew);

    glm::dvec3 lookAtPos = interpolation::linear(uNew, _startLookAtPos, _endLookAtPos);
    glm::dvec3 startUpVec = _start * glm::dvec3(0.0, 1.0, 0.0);

    return helpers::getLookAtQuaternion(_path->positionAt(u), lookAtPos, startUpVec);
}

LookAtInterpolator::LookAtInterpolator(glm::dquat start, glm::dquat end,
                                       glm::dvec3 startLookAtPos,
                                       glm::dvec3 endLookAtPos,
                                       PathCurve* path)
    : RotationInterpolator(start, end),
    _startLookAtPos(startLookAtPos),
    _endLookAtPos(endLookAtPos),
    _path(path)
{}

// Turn towards start node, turn towards end node, then turn to end view
glm::dquat LookAtInterpolator::interpolate(double u) {
    //TODO: base on curve position?
    double u1 = 0.2;
    double u2 = 0.8;

    glm::dvec3 startPosition = _path->positionAt(0.0);
    glm::dvec3 endPosition = _path->positionAt(1.0);

    glm::dvec3 lookAtPos;
    if (u < u1) {
        // Create aim positions not too close to camera based on view direction
        glm::dvec3 startViewDir = glm::normalize(_start * glm::dvec3(0.0, 0.0, -1.0));
        double startViewDist = glm::length(startPosition - _startLookAtPos);
        glm::dvec3 startViewPos = startPosition + startViewDist * startViewDir;
        double uNew = u / u1;
        uNew = ghoul::cubicEaseInOut(uNew);
        lookAtPos = ghoul::interpolateLinear(uNew, startViewPos, _startLookAtPos);
    }
    else if (u <= u2) {
        double uNew = (u - u1) / (u2 - u1);
        uNew = ghoul::cubicEaseInOut(uNew);
        lookAtPos = ghoul::interpolateLinear(uNew, _startLookAtPos, _endLookAtPos);
    }
    else if (u2 < u) {
        glm::dvec3 endViewDir = glm::normalize(_end * glm::dvec3(0.0, 0.0, -1.0));
        double endViewDist = glm::length(endPosition - _endLookAtPos);
        glm::dvec3 endViewPos = endPosition + endViewDist * endViewDir;
        double uNew = (u - u2) / (1.0 - u2);
        uNew = ghoul::cubicEaseInOut(uNew);
        lookAtPos = ghoul::interpolateLinear(uNew, _endLookAtPos, endViewPos);
    }

    // handle up vector
    glm::dvec3 startUp = _start * glm::dvec3(0.0, 1.0, 0.0); 
    glm::dvec3 endUp = _end * glm::dvec3(0.0, 1.0, 0.0);

    double uUp = ghoul::cubicEaseInOut(u);
    glm::dvec3 up = ghoul::interpolateLinear(uUp, startUp, endUp);

    return helpers::getLookAtQuaternion(_path->positionAt(u), lookAtPos, up);
}

} // namespace openspace::autonavigation
