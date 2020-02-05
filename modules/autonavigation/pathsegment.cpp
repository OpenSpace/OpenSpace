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

#include <modules/autonavigation/pathsegment.h>

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/pathcurves.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/easing.h>

namespace {
    constexpr const char* _loggerCat = "PathSegment";
} // namespace

namespace openspace::autonavigation {

PathSegment::PathSegment(
    CameraState start, CameraState end, double startTime, CurveType type)
    : _start(start), _end(end), _startTime(startTime), _curveType(type)
{
    initCurve();

    // TODO: compute default duration based on curve length 
    // Also, when compensatng for simulation time later we need to make a guess for 
    // the duration, based on the current position of the target. 
    _duration = 5;
}

void PathSegment::setStart(CameraState cs) {
    _start = std::move(cs);
    initCurve();
    // TODO later: maybe recompute duration as well...
}

void PathSegment::setDuration(double d) {
    _duration = d;
}

const CameraState PathSegment::start() const { return _start; }

const CameraState PathSegment::end() const { return _end; }

const double PathSegment::duration() const { return _duration; }

const double PathSegment::startTime() const { return _startTime; }

// TODO: remove function for debugging
const std::vector<glm::dvec3> PathSegment::getControlPoints() const {
    return _curve->getPoints();
}

glm::dvec3 PathSegment::getPositionAt(double t) const {
    t = ghoul::cubicEaseInOut(t); 
    return _curve->valueAt(t);    
}

glm::dquat PathSegment::getRotationAt(double t) const {
    // TODO: improve how rotation is computed

    switch (_curveType) {
    case CurveType::Bezier3: 
    {
        return piecewiseSlerpRotation(t);
        break;
    }
    default:
    {
        double tSlerp = helpers::shiftAndScale(t, 0.1, 0.9);
        tSlerp = ghoul::cubicEaseInOut(tSlerp);
        return glm::slerp(_start.rotation, _end.rotation, tSlerp);
    }
    }
}

const glm::dquat PathSegment::piecewiseSlerpRotation(double t) const {
    // breakpoints for subintervals
    const double t1 = 0.3;
    const double t2 = 0.8; // TODO: these should probably be based on distance

    glm::dvec3 posAtT1 = getPositionAt(t1);
    glm::dvec3 posAtT2 = getPositionAt(t2);

    glm::dvec3 startNodePos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 endNodePos = sceneGraphNode(_end.referenceNode)->worldPosition();

    glm::dvec3 startUpVec = _start.rotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtStartMat = glm::lookAt(posAtT1, startNodePos, startUpVec);
    glm::dquat lookAtStartQuad = glm::normalize(glm::inverse(glm::quat_cast(lookAtStartMat)));

    glm::dvec3 endUpVec = _end.rotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dmat4 lookAtEndMat = glm::lookAt(posAtT2, endNodePos, endUpVec);
    glm::dquat lookAtEndQuad = glm::normalize(glm::inverse(glm::quat_cast(lookAtEndMat)));

    // Interpolate based on the subintervals
    double tScaled;
    glm::dquat startQuad, endQuad;

    if (t < t1) {
        tScaled = t / t1;
        startQuad = _start.rotation;
        endQuad = lookAtStartQuad;
    }
    else if (t < t2) {
        tScaled = (t - t1) / (t2 - t1);
        startQuad = lookAtStartQuad;
        endQuad = lookAtEndQuad;
    }
    else { // t <= 1.0
        tScaled = (t - t2) / (1.0 - t2);
        startQuad = lookAtEndQuad;
        endQuad = _end.rotation;
    }

    // TODO: experiment with easing functions
    tScaled = ghoul::quadraticEaseInOut(tScaled);

    return glm::slerp(startQuad, endQuad, tScaled);
}

// Initialise the curve, based on the start, end state and curve type
void PathSegment::initCurve() {
    // in case there already is a curve object, reset the pointer. 
    _curve.reset();

    switch (_curveType) {
    case CurveType::Bezier:
        _curve = std::make_shared<BezierCurve>(_start, _end);
        break;
    case CurveType::Bezier2:
        _curve = std::make_shared<Bezier2Curve>(_start, _end);
        break;
    case CurveType::Bezier3:
        _curve = std::make_shared<Bezier3Curve>(_start, _end);
        break;
    case CurveType::Linear:
        _curve = std::make_shared<LinearCurve>(_start, _end);
        break;
    case CurveType::Linear2:
        _curve = std::make_shared<Linear2Curve>(_start, _end);
        break;
    case CurveType::Pause:
        _curve = std::make_shared<PauseCurve>(_start);
        break;
    default:
        LERROR("Could not create curve. Type does not exist!");
        return;
    }

    _length = _curve->arcLength();
    //LINFO(fmt::format("Curve length: {}", _length));
}

} // namespace openspace::autonavigation
