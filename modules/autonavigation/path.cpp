/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/autonavigation/path.h>

#include <modules/autonavigation/autonavigationmodule.h>
#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/pathcurve.h>
#include <modules/autonavigation/speedfunction.h>
#include <modules/autonavigation/curves/avoidcollisioncurve.h>
#include <modules/autonavigation/curves/zoomoutoverviewcurve.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

namespace {
    constexpr const char* _loggerCat = "Path";
} // namespace

namespace openspace::autonavigation {

Path::Path(Waypoint start, Waypoint end, CurveType type,
           std::optional<double> duration)
    : _start(start), _end(end), _curveType(type)
{
    switch (_curveType) {
        case CurveType::AvoidCollision:
            _curve = std::make_unique<AvoidCollisionCurve>(_start, _end);
            break;
        case CurveType::Linear:
            _curve = std::make_unique<LinearCurve>(_start, _end);
            break;
        case CurveType::ZoomOutOverview:
            _curve = std::make_unique<ZoomOutOverviewCurve>(_start, _end);
            break;
        default:
            LERROR("Could not create curve. Type does not exist!");
            throw ghoul::MissingCaseException();
    }

    _speedFunction = SpeedFunction(SpeedFunction::Type::DampenedQuintic);

    const auto defaultDuration = [](double pathlength) {
        auto module = global::moduleEngine->module<AutoNavigationModule>();
        const double speedScale = module->AutoNavigationHandler().speedScale();
        return std::log(pathlength) / speedScale;
    };

    _duration = duration.value_or(defaultDuration(pathLength()));
}

Waypoint Path::startPoint() const { return _start; }

Waypoint Path::endPoint() const { return _end; }

double Path::duration() const { return _duration; }

double Path::pathLength() const { return _curve->length(); }

std::vector<glm::dvec3> Path::controlPoints() const {
    return _curve->points();
}

CameraPose Path::traversePath(double dt) {
    const int resolution = 50;

    double displacement = helpers::simpsonsRule(
        _progressedTime,
        _progressedTime + dt,
        resolution,
        [this](double t) { return speedAtTime(t); }
    );

    _progressedTime += dt;
    _traveledDistance += displacement;

    return interpolatedPose(_traveledDistance);
}

std::string Path::currentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeDetails.identifier : _start.nodeDetails.identifier;
}

bool Path::hasReachedEnd() const {
    return (_traveledDistance / pathLength()) >= 1.0;
}

double Path::speedAtTime(double time) const {
    return _speedFunction.scaledValue(time, _duration, pathLength());
}

CameraPose Path::interpolatedPose(double distance) const {
    double u = distance / pathLength();
    CameraPose cs;
    cs.position = _curve->positionAt(u);
    cs.rotation = interpolateRotation(u);
    return cs;
}

glm::dquat Path::interpolateRotation(double u) const {
    switch (_curveType) {
        case CurveType::AvoidCollision:
        case CurveType::Linear:
            return interpolation::easedSlerp(_start.rotation(), _end.rotation(), u);
        case CurveType::ZoomOutOverview:
        {
            const double u1 = 0.2;
            const double u2 = 0.8;

            const glm::dvec3 startPos = _curve->positionAt(0.0);
            const glm::dvec3 endPos = _curve->positionAt(1.0);
            const glm::dvec3 startNodePos = _start.node()->worldPosition();
            const glm::dvec3 endNodePos = _end.node()->worldPosition();

            glm::dvec3 lookAtPos;
            if (u < u1) {
                // Compute a position in front of the camera at the start orientation
                const double inFrontDistance = glm::distance(startPos, startNodePos);
                const glm::dvec3 viewDir = helpers::viewDirection(_start.rotation());
                const glm::dvec3 inFrontOfStart = startPos + inFrontDistance * viewDir;

                double uScaled = ghoul::cubicEaseInOut(u / u1);
                lookAtPos = 
                    ghoul::interpolateLinear(uScaled, inFrontOfStart, startNodePos);
            }
            else if (u <= u2) {
                double uScaled = ghoul::cubicEaseInOut((u - u1) / (u2 - u1));
                lookAtPos = ghoul::interpolateLinear(uScaled, startNodePos, endNodePos);
            }
            else if (u2 < u) {
                // Compute a position in front of the camera at the end orientation
                const double inFrontDistance = glm::distance(endPos, endNodePos);
                const glm::dvec3 viewDir = helpers::viewDirection(_end.rotation());
                const glm::dvec3 inFrontOfEnd = endPos + inFrontDistance * viewDir;

                double uScaled = ghoul::cubicEaseInOut((u - u2) / (1.0 - u2));
                lookAtPos = ghoul::interpolateLinear(uScaled, endNodePos, inFrontOfEnd);
            }

            // Handle up vector separately
            glm::dvec3 startUp = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);
            glm::dvec3 endUp = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);

            double uUp = helpers::shiftAndScale(u, u1, u2);
            uUp = ghoul::sineEaseInOut(uUp);
            glm::dvec3 up = ghoul::interpolateLinear(uUp, startUp, endUp);

            return helpers::lookAtQuaternion(_curve->positionAt(u), lookAtPos, up);
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

} // namespace openspace::autonavigation
