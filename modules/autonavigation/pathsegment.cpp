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
#include <modules/autonavigation/avoidcollisioncurve.h>
#include <modules/autonavigation/pathcurves.h>
#include <modules/autonavigation/rotationinterpolator.h>
#include <modules/autonavigation/speedfunction.h>
#include <modules/autonavigation/zoomoutoverviewcurve.h>
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

    // TODO: handle duration better
    if (duration.has_value()) {
        _duration = duration.value();
    }
    else {
        _duration = std::log(pathLength());
        //LINFO(fmt::format("Default duration: {}", _duration));
    }
}

void PathSegment::setStart(Waypoint cs) {
    _start = std::move(cs);
    initCurve();
    // TODO later: maybe recompute duration as well...
}

const Waypoint PathSegment::start() const { return _start; }

const Waypoint PathSegment::end() const { return _end; }

const double PathSegment::duration() const { return _duration; }

const double PathSegment::pathLength() const { return _curve->length(); }

// TODO: remove function for debugging
const std::vector<glm::dvec3> PathSegment::getControlPoints() const {
    return _curve->getPoints();
}

CameraPose PathSegment::traversePath(double dt) {
    if (!_curve || !_rotationInterpolator || !_speedFunction) {
        // TODO: handle better (abort path somehow)
        return _start.pose;
    }

    AutoNavigationModule* module = global::moduleEngine.module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    const int nrSteps = handler.nrSimulationStepsPerFrame(); 

    // compute displacement along the path during this frame
    double displacement = 0.0;
    double h = dt / nrSteps;
    for (int i = 0; i < nrSteps; ++i) {
        double t = _progressedTime + i * h;
        double speed = 0.5 * (speedAtTime(t - 0.01*h) + speedAtTime(t + 0.01*h)); // average
        displacement += h * speed;
    }

    _traveledDistance += displacement;
    double relativeDisplacement = _traveledDistance / pathLength();

    // TEST: 
    //LINFO("-----------------------------------");
    //LINFO(fmt::format("relativeDisplacement = {}", relativeDisplacement));
    //LINFO(fmt::format("progressedTime = {}", _progressedTime));

    _progressedTime += dt;

    return interpolatedPose(relativeDisplacement);
}

std::string PathSegment::getCurrentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeDetails.identifier : _start.nodeDetails.identifier;
}

bool PathSegment::hasReachedEnd() const {
    return (_traveledDistance / pathLength()) >= 1.0;
}

double PathSegment::speedAtTime(double time) const {
    return _speedFunction->scaledValue(time, _duration, pathLength());
}

CameraPose PathSegment::interpolatedPose(double u) const {
    CameraPose cs;
    cs.position = _curve->positionAt(u);
    cs.rotation = _rotationInterpolator->interpolate(u);
    return cs;
}

void PathSegment::initCurve() {
    _curve.reset();

    switch (_curveType) 
    {
    case CurveType::AvoidCollision:
        _curve = std::make_unique<AvoidCollisionCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<EasedSlerpInterpolator>(
            _start.rotation(),
            _end.rotation()
            );
        _speedFunction = std::make_unique<SexticDampenedSpeed>();
        break;

    case CurveType::Bezier3:
        _curve = std::make_unique<Bezier3Curve>(_start, _end);
        _rotationInterpolator = std::make_unique<LookAtInterpolator>(
            _start.rotation(),
            _end.rotation(),
            _start.node()->worldPosition(),
            _end.node()->worldPosition(),
            _curve.get()
        );
        _speedFunction = std::make_unique<SexticDampenedSpeed>(); 
        break;

    case CurveType::Linear:
        _curve = std::make_unique<LinearCurve>(_start, _end);
        _rotationInterpolator = std::make_unique<EasedSlerpInterpolator>(
            _start.rotation(), 
            _end.rotation()
        );
        _speedFunction = std::make_unique<SexticDampenedSpeed>();
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
        _speedFunction = std::make_unique<SexticDampenedSpeed>();
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
