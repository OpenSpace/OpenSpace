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

#include <modules/autonavigation/autonavigationmodule.h>
#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/pathcurve.h>
#include <modules/autonavigation/rotationinterpolator.h>
#include <modules/autonavigation/speedfunction.h>
#include <modules/autonavigation/curves/avoidcollisioncurve.h>
#include <modules/autonavigation/curves/zoomoutoverviewcurve.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PathSegment";

    const double Epsilon = 1E-7;
} // namespace

namespace openspace::autonavigation {

PathSegment::PathSegment(Waypoint start, Waypoint end, CurveType type,
                         std::optional<double> duration)
    : _start(start), _end(end), _curveType(type)
{
    initCurve();

    if (duration.has_value()) {
        _duration = duration.value();
    }
    else {
        _duration = std::log(pathLength());

        auto module = global::moduleEngine->module<AutoNavigationModule>();
        _duration /= module->AutoNavigationHandler().speedScale();
    }
}

void PathSegment::setStartPoint(Waypoint cs) {
    _start = std::move(cs);
    initCurve();
    // TODO later: maybe recompute duration as well...
}

const Waypoint PathSegment::startPoint() const { return _start; }

const Waypoint PathSegment::endPoint() const { return _end; }

const double PathSegment::duration() const { return _duration; }

const double PathSegment::pathLength() const { return _curve->length(); }

// TODO: remove function for debugging
const std::vector<glm::dvec3> PathSegment::controlPoints() const {
    return _curve->points();
}

CameraPose PathSegment::traversePath(double dt) {
    if (!_curve || !_rotationInterpolator || !_speedFunction) {
        // TODO: handle better (abort path somehow)
        return _start.pose;
    }

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    const int nSteps = handler.integrationResolutionPerFrame();

    double displacement = helpers::simpsonsRule(
        _progressedTime,
        _progressedTime + dt,
        nSteps,
        [this](double t) { return speedAtTime(t); }
    );

    _progressedTime += dt;
    _traveledDistance += displacement;

    return interpolatedPose(_traveledDistance);
}

std::string PathSegment::currentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeDetails.identifier : _start.nodeDetails.identifier;
}

bool PathSegment::hasReachedEnd() const {
    return (_traveledDistance / pathLength()) >= 1.0;
}

double PathSegment::speedAtTime(double time) const {
    return _speedFunction->scaledValue(time, _duration, pathLength());
}

CameraPose PathSegment::interpolatedPose(double distance) const {
    double u = distance / pathLength();
    CameraPose cs;
    cs.position = _curve->positionAt(u);
    cs.rotation = _rotationInterpolator->interpolate(u);
    return cs;
}

void PathSegment::initCurve() {
    _curve.reset();

    switch (_curveType)
    {
    case CurveType::AvoidCollisionLookAt:
        _curve = std::make_unique<AvoidCollisionCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<LookAtInterpolator>(
            _start.rotation(),
            _end.rotation(),
            _start.node()->worldPosition(),
            _end.node()->worldPosition(),
            _curve.get()
        );

        _speedFunction = std::make_unique<QuinticDampenedSpeed>();
        break;

    case CurveType::AvoidCollision:
        _curve = std::make_unique<AvoidCollisionCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<EasedSlerpInterpolator>(
            _start.rotation(),
            _end.rotation()
        );
        _speedFunction = std::make_unique<QuinticDampenedSpeed>();
        break;

    case CurveType::Linear:
        _curve = std::make_unique<LinearCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<EasedSlerpInterpolator>(
            _start.rotation(),
            _end.rotation()
        );
        _speedFunction = std::make_unique<QuinticDampenedSpeed>();
        break;

    case CurveType::ZoomOutOverview:
        _curve = std::make_unique<ZoomOutOverviewCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<LookAtInterpolator>(
            _start.rotation(),
            _end.rotation(),
            _start.node()->worldPosition(),
            _end.node()->worldPosition(),
            _curve.get()
        );
        _speedFunction = std::make_unique<QuinticDampenedSpeed>();
        break;

    default:
        LERROR("Could not create curve. Type does not exist!");
        return;
    }

    if (!_curve || !_rotationInterpolator || !_speedFunction) {
        LERROR("Curve type has not been properly initialized.");
        return;
    }
}

} // namespace openspace::autonavigation
