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

    const auto defaultDuration = [](double pathlength) {
        auto module = global::moduleEngine->module<AutoNavigationModule>();
        const double speedScale = module->AutoNavigationHandler().speedScale();
        return std::log(pathlength) / speedScale;
    };

    _duration = duration.value_or(defaultDuration(pathLength()));

    // Compute speed factor to match the generated path length and duration, by
    // traversing the path and computing how much faster/slower it should be
    const int nSteps = 500;
    const double dt = (_duration / nSteps) > 0.01 ? (_duration / nSteps) : 0.01;

    while (!hasReachedEnd()) {
        traversePath(dt);
    }

    _speedFactorFromDuration = _progressedTime / _duration;

    // Reset playback variables
    _traveledDistance = 0.0;
    _progressedTime = 0.0;
}

Waypoint Path::startPoint() const { return _start; }

Waypoint Path::endPoint() const { return _end; }

double Path::duration() const { return _duration; }

double Path::pathLength() const { return _curve->length(); }

std::vector<glm::dvec3> Path::controlPoints() const {
    return _curve->points();
}

CameraPose Path::traversePath(double dt) {
    const double speed = _speedFactorFromDuration * speedAlongPath(_traveledDistance);
    const double displacement =  dt * speed;

    _progressedTime += dt;
    _traveledDistance += displacement;

    return interpolatedPose(_traveledDistance);
}

std::string Path::currentAnchor() const {
    bool pastHalfway = (_traveledDistance / pathLength()) > 0.5;
    return (pastHalfway) ? _end.nodeIdentifier : _start.nodeIdentifier;
}

bool Path::hasReachedEnd() const {
    return (_traveledDistance / pathLength()) >= 1.0;
}

CameraPose Path::interpolatedPose(double distance) const {
    const double relativeDistance = distance / pathLength();
    CameraPose cs;
    cs.position = _curve->positionAt(relativeDistance);
    cs.rotation = interpolateRotation(relativeDistance);
    return cs;
}

glm::dquat Path::interpolateRotation(double t) const {
    switch (_curveType) {
        case CurveType::AvoidCollision:
        case CurveType::Linear:
            return interpolation::easedSlerp(_start.rotation(), _end.rotation(), t);
        case CurveType::ZoomOutOverview:
        {
            const double t1 = 0.2;
            const double t2 = 0.8;

            const glm::dvec3 startPos = _curve->positionAt(0.0);
            const glm::dvec3 endPos = _curve->positionAt(1.0);
            const glm::dvec3 startNodePos = _start.node()->worldPosition();
            const glm::dvec3 endNodePos = _end.node()->worldPosition();

            glm::dvec3 lookAtPos;
            if (t < t1) {
                // Compute a position in front of the camera at the start orientation
                const double inFrontDistance = glm::distance(startPos, startNodePos);
                const glm::dvec3 viewDir = helpers::viewDirection(_start.rotation());
                const glm::dvec3 inFrontOfStart = startPos + inFrontDistance * viewDir;

                const double tScaled = ghoul::cubicEaseInOut(t / t1);
                lookAtPos = 
                    ghoul::interpolateLinear(tScaled, inFrontOfStart, startNodePos);
            }
            else if (t <= t2) {
                const double tScaled = ghoul::cubicEaseInOut((t - t1) / (t2 - t1));
                lookAtPos = ghoul::interpolateLinear(tScaled, startNodePos, endNodePos);
            }
            else if (t > t2) {
                // Compute a position in front of the camera at the end orientation
                const double inFrontDistance = glm::distance(endPos, endNodePos);
                const glm::dvec3 viewDir = helpers::viewDirection(_end.rotation());
                const glm::dvec3 inFrontOfEnd = endPos + inFrontDistance * viewDir;

                const double tScaled = ghoul::cubicEaseInOut((t - t2) / (1.0 - t2));
                lookAtPos = ghoul::interpolateLinear(tScaled, endNodePos, inFrontOfEnd);
            }

            // Handle up vector separately
            glm::dvec3 startUp = _start.rotation() * glm::dvec3(0.0, 1.0, 0.0);
            glm::dvec3 endUp = _end.rotation() * glm::dvec3(0.0, 1.0, 0.0);

            double tUp = helpers::shiftAndScale(t, t1, t2);
            tUp = ghoul::sineEaseInOut(tUp);
            glm::dvec3 up = ghoul::interpolateLinear(tUp, startUp, endUp);

            return helpers::lookAtQuaternion(_curve->positionAt(t), lookAtPos, up);
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

double Path::speedAlongPath(double traveledDistance) {
    const glm::dvec3 endNodePos = _end.node()->worldPosition();
    const glm::dvec3 startNodePos = _start.node()->worldPosition();

    const CameraPose prevPose = interpolatedPose(_traveledDistance);
    const double distanceToEndNode = glm::distance(prevPose.position, endNodePos);
    const double distanceToStartNode = glm::distance(prevPose.position, startNodePos);

    // Decide which is the closest node
    SceneGraphNode* closestNode = _start.node();
    glm::dvec3 closestPos = startNodePos;

    if (distanceToEndNode < distanceToStartNode) {
        closestPos = endNodePos;
        closestNode = _end.node();
    }

    const double distanceToClosestNode = glm::distance(closestPos, prevPose.position);
    double speed = distanceToClosestNode;

    // Dampen speed in beginning of path
    const double startUpDistance = 2.0 * _start.node()->boundingSphere();
    if (_traveledDistance < startUpDistance) {
        speed *= _traveledDistance / startUpDistance + 0.01;
    }

    // Dampen speed in end of path
    // Note: this leads to problems when the full length of the path is really big
    const double closeUpDistance = 2.0 * _end.node()->boundingSphere();
    if (_traveledDistance > (pathLength() - closeUpDistance)) {
        const double remainingDistance = pathLength() - _traveledDistance;
        speed *= remainingDistance / closeUpDistance + 0.01;
    }

    // TODO: also dampen speed based on curvature, or make sure the curve has a rounder shape

    // TODO: check for when path is shorter than the starUpDistance or closeUpDistance variables

    return speed;
}

} // namespace openspace::autonavigation
