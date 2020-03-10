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

#include <modules/autonavigation/pathcurves.h>

#include <modules/autonavigation/helperfunctions.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "RotationInterpolator";
} // namespace

namespace openspace::autonavigation {

RotationInterpolator::RotationInterpolator(
    const Waypoint& start, const Waypoint& end, PathCurve* curve, RotationMethod method)
    : _start(start), _end(end), _method(method)
{
    _curve = curve;
}

glm::dquat RotationInterpolator::rotationAt(double u) {
    switch (_method)
    {
    case Slerp:
        return easedSlerp(u);
        break;
    case PiecewiseSlerp:
        return piecewiseSlerp(u);
        break;
    case Fixed:
        return _start.rotation();
        break;
    case LookAt:
        return lookAtInterpolator(u);
        break;
    default:
        LERROR("Non-implemented orientation interpolation method!");
        return _start.rotation();
        break;
    }
}

glm::dquat RotationInterpolator::easedSlerp(double u) {
    double uScaled = helpers::shiftAndScale(u, 0.1, 0.9);
    uScaled = ghoul::cubicEaseInOut(uScaled);
    return glm::slerp(_start.rotation(), _end.rotation(), uScaled);
}

// Look at start node until tStart, then turn to look at end node from tEnd
// Will overwrite rotation of navigation states! 
glm::dquat RotationInterpolator::lookAtInterpolator(double u) {
    double tStart = 0.15;
    double tEnd = 0.7;
    double uNew = helpers::shiftAndScale(u, tStart, tEnd);
    uNew = ghoul::cubicEaseInOut(uNew);

    glm::dvec3 startNodePos = _start.node()->worldPosition();
    glm::dvec3 endNodePos = _end.node()->worldPosition();
    glm::dvec3 lookAtPos = interpolation::linear(uNew, startNodePos, endNodePos);

    glm::dvec3 startUpVec = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);

    return helpers::getLookAtQuaternion(_curve->positionAt(u), lookAtPos, startUpVec);
}

// Interpolate between a number of keyframes for orientation using SLERP
glm::dquat RotationInterpolator::piecewiseSlerp(double u) {
    ghoul_assert(_curve, "Rotation interpolation requires access to curve positions.");

    // breakpoints for subintervals
    const double u1 = 0.3;
    const double u2 = 0.8; // TODO: these should probably be based on distance

    glm::dvec3 startNodePos = _start.node()->worldPosition();
    glm::dvec3 endNodePos = _end.node()->worldPosition();

    glm::dvec3 startUpVec = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);
    glm::dvec3 endUpVec = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);

    glm::dquat lookAtStartQ =
        helpers::getLookAtQuaternion(_curve->positionAt(u1), startNodePos, startUpVec);

    glm::dquat lookAtEndQ =
        helpers::getLookAtQuaternion(_curve->positionAt(u2), endNodePos, endUpVec);

    std::vector<std::pair<glm::dquat, double>> keyframes{
        {_start.rotation(), 0.0},
        {lookAtStartQ, u1},
        {lookAtEndQ, u2},
        {_end.rotation(), 1.0}
    };

    // Find the current segment and compute interpolation
    glm::dquat result;
    for (int i = 0; i < keyframes.size() - 1; ++i) {
        double ui = keyframes[i].second;
        double uNext = keyframes[i + 1].second;
        if (u <= uNext) {
            double uScaled = (u - ui) / (uNext - ui);
            uScaled = ghoul::quadraticEaseInOut(uScaled);
            result = glm::slerp(keyframes[i].first, keyframes[i + 1].first, uScaled);
            break;
        }
    }

    return result;
}

} // namespace openspace::autonavigation
