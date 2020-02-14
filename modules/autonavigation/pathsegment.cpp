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

const double PathSegment::endTime() const { return _startTime + _duration; }

const double PathSegment::length() const { return _curve->length(); }

// TODO: remove function for debugging
const std::vector<glm::dvec3> PathSegment::getControlPoints() const {
    return _curve->getPoints();
}


// Speed function for the segment, where time is a value in the range [0, duration]
// OBS! If integrated over the curve it must match the total length or the curve
const double PathSegment::speedAtTime(double time) const {
    
    // TODO: validate time

    double speed = speedFunction(time / _duration);

    // apply duration constraint (eq. 14 in Eberly)
    double speedSum = 0.0;
    const int steps = 100;
    double dt = duration() / steps;
    for (double t = 0.0; t <= 1.0; t += 1.0 / steps) {
        speedSum += dt * speedFunction(t);
    }

    // TODO: save speed sum value somewhere

    speed = (length() * speedFunction(time / _duration)) / speedSum;

    return speed;
}

glm::dvec3 PathSegment::getPositionAt(double t) const {
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

// Interpolate between a number of keyframes for orientation using SLERP
const glm::dquat PathSegment::piecewiseSlerpRotation(double t) const {
    // breakpoints for subintervals
    const double t1 = 0.3;
    const double t2 = 0.8; // TODO: these should probably be based on distance
    std::vector<double> times{ 0.0, t1, t2, 1.0 };

    glm::dvec3 startNodePos = sceneGraphNode(_start.referenceNode)->worldPosition();
    glm::dvec3 endNodePos = sceneGraphNode(_end.referenceNode)->worldPosition();

    glm::dvec3 startUpVec = _start.rotation * glm::dvec3(0.0, 1.0, 0.0);
    glm::dvec3 endUpVec = _end.rotation * glm::dvec3(0.0, 1.0, 0.0);

    glm::dquat lookAtStartQ =
        helpers::getLookAtQuaternion(getPositionAt(t1), startNodePos, startUpVec);

    glm::dquat lookAtEndQ =
        helpers::getLookAtQuaternion(getPositionAt(t2), endNodePos, endUpVec);

    std::vector<glm::dquat> keyframes{ 
        _start.rotation, 
        lookAtStartQ, 
        lookAtEndQ, 
        _end.rotation 
    };

    ghoul_assert(keyframes.size() == times.size(), "Must have one time value per keyframe.");

    // Find the current segment and compute interpolation
    glm::dquat result;
    for (int i = 0; i < keyframes.size() - 1; ++i) {
        if (t <= times[i + 1]) {
            double tScaled = (t - times[i]) / (times[i + 1] - times[i]);
            tScaled = ghoul::quadraticEaseInOut(tScaled);
            result = glm::slerp(keyframes[i], keyframes[i + 1], tScaled);
            break;
        }
    }

    return result;
}

// Initialise the curve, based on the start, end state and curve type
void PathSegment::initCurve() {
    // in case there already is a curve object, reset the pointer. 
    _curve.reset();

    switch (_curveType) {
    case CurveType::Bezier3:
        _curve = std::make_shared<Bezier3Curve>(_start, _end);
        break;
    case CurveType::Linear:
        _curve = std::make_shared<LinearCurve>(_start, _end);
        break;
    case CurveType::Pause:
        _curve = std::make_shared<PauseCurve>(_start);
        break;
    default:
        LERROR("Could not create curve. Type does not exist!");
        return;
    }
}

// The speed function, describing the shape of the speed curve with values in range [0,1]
// OBS! The value must later be normalised so that the speed matches the duration of the segment.
double PathSegment::speedFunction(double t) const {
    const double t1 = 0.25;
    const double t2 = 0.75; // > t1
    // TODO: more advanced computation of limits

    double speed = 1.0;
    double tScaled;

    // accelerate
    if (t < t1) {
        tScaled = t / t1;
        speed = ghoul::cubicEaseInOut(tScaled);
    }
    // deaccelerate
    else if (t > t2) {
        tScaled = (t - t2) / (1.0 - t2);
        speed = 1.0 - ghoul::cubicEaseInOut(tScaled);
    }

    return speed;
}

} // namespace openspace::autonavigation
