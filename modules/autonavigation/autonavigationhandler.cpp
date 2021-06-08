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

#include <modules/autonavigation/autonavigationhandler.h>

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/pathinstruction.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/quaternion.hpp>
#include <algorithm>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "AutoNavigationHandler";

    constexpr openspace::properties::Property::PropertyInfo DefaultCurveOptionInfo = {
        "DefaultCurveOption",
        "Default Curve Option",
        "The defualt curve type chosen when generating a path, if none is specified"
        // TODO: right now there is no way to specify a type for a single path
    };

    constexpr openspace::properties::Property::PropertyInfo IncludeRollInfo = {
        "IncludeRoll",
        "Include Roll",
        "If disabled, roll is removed from the interpolation of camera orientation"
    };

    constexpr openspace::properties::Property::PropertyInfo StopBehaviorInfo = {
        "StopBehavior",
        "Stop Behavior",
        "A camera motion behavior that is applied when no path is being played"
    };

    constexpr openspace::properties::Property::PropertyInfo 
        ApplyStopBehaviorWhenIdleInfo = 
    {
        "ApplyStopBehaviorWhenIdle",
        "Apply Stop Behavior When Idle",
        "If enabled, the camera is controlled using the set stop behavior when "
        "no path is playing"
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedScaleInfo = {
        "SpeedScale",
        "Speed Scale",
        "Scale factor that affects the default speed for a camera path."
    };

    constexpr openspace::properties::Property::PropertyInfo OrbitSpeedFactorInfo = {
        "OrbitSpeedFactor",
        "Orbit Speed Factor",
        "Controls the speed of the orbiting around an anchor."
    };
} // namespace

namespace openspace::autonavigation {

AutoNavigationHandler::AutoNavigationHandler()
    : properties::PropertyOwner({ "AutoNavigationHandler" })
    , _defaultCurveOption(
        DefaultCurveOptionInfo, 
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _includeRoll(IncludeRollInfo, false)
    , _stopBehavior(
        StopBehaviorInfo, 
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _applyStopBehaviorWhenIdle(ApplyStopBehaviorWhenIdleInfo, false)
    , _speedScale(SpeedScaleInfo, 1.f, 0.01f, 2.f)
    , _orbitSpeedFactor(OrbitSpeedFactorInfo, 0.5, 0.0, 20.0)
{
    _defaultCurveOption.addOptions({
        { Path::CurveType::AvoidCollision, "AvoidCollision" },
        { Path::CurveType::Linear, "Linear" },
        { Path::CurveType::ZoomOutOverview, "ZoomOutOverview"}
    });
    addProperty(_defaultCurveOption);

    addProperty(_includeRoll);
    addProperty(_speedScale);

    addProperty(_applyStopBehaviorWhenIdle);

    // Must be listed in the same order as in enum definition
    _stopBehavior.addOptions({
        { StopBehavior::None, "None" },
        { StopBehavior::Orbit, "Orbit" }
    });
    _stopBehavior = StopBehavior::None;
    addProperty(_stopBehavior);

    addProperty(_orbitSpeedFactor);
}

AutoNavigationHandler::~AutoNavigationHandler() {} // NOLINT

Camera* AutoNavigationHandler::camera() const {
    return global::navigationHandler->camera();
}

const SceneGraphNode* AutoNavigationHandler::anchor() const {
    return global::navigationHandler->anchorNode();
}

double AutoNavigationHandler::speedScale() const {
    return _speedScale;
}

bool AutoNavigationHandler::noCurrentPath() const {
    return _currentPath == nullptr;
}

bool AutoNavigationHandler::hasFinished() const {
    if (noCurrentPath()) {
        return true;
    }

    return _currentPath->hasReachedEnd();
}

void AutoNavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!_isPlaying) {
        // TODO: Determine how this should work
        if (_applyStopBehaviorWhenIdle) {
            applyStopBehavior(deltaTime);
        }
        return;
    }

    if (noCurrentPath()) {
        return;
    }

    // If for some reason the time is no longer paused, pause it again
    if (!global::timeManager->isPaused()) {
        global::timeManager->setPause(true);
        LINFO("Cannot start simulation time during camera motion");
    }

    CameraPose newPose = _currentPath->traversePath(deltaTime);
    const std::string newAnchor = _currentPath->currentAnchor();

    // Set anchor node in orbitalNavigator, to render visible nodes and add activate
    // navigation when we reach the end.
    std::string currentAnchor = anchor()->identifier();
    if (currentAnchor != newAnchor) {
        global::navigationHandler->orbitalNavigator().setAnchorNode(newAnchor);
    }

    if (!_includeRoll) {
        removeRollRotation(newPose, deltaTime);
    }

    camera()->setPositionVec3(newPose.position);
    camera()->setRotation(newPose.rotation);

    // TODO: make this work again
    if (_currentPath->hasReachedEnd()) {
        LINFO("Reached end of path");
        _isPlaying = false;
        return;
    }
}

void AutoNavigationHandler::createPath(PathInstruction& instruction) {
    clearPath();

    std::vector<Waypoint> waypoints = instruction.waypoints();
    Waypoint waypointToAdd;

    if (waypoints.empty()) {
        LWARNING("No path was created from instruction. Failed creating waypoints");
        return;
    }
    else {
        // TODO: allow for an instruction to represent a list of waypoints
        waypointToAdd = waypoints[0];
    }

    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    _currentPath = std::make_unique<Path>(
        instruction.startPoint(),
        waypointToAdd,
        Path::CurveType(curveType),
        instruction.duration()
    );

    LINFO("Successfully generated camera path");
    startPath();
}

void AutoNavigationHandler::clearPath() {
    LINFO("Clearing path...");
    _currentPath = nullptr;
}

void AutoNavigationHandler::startPath() {
    if (noCurrentPath()) {
        LERROR("There is no path to start");
        return;
    }

    //OBS! Until we can handle simulation time: early out if not paused
    if (!global::timeManager->isPaused()) {
        LERROR("Simulation time must be paused to run a camera path");
        return;
    }

    LINFO("Starting path...");
    _isPlaying = true;
}

//void AutoNavigationHandler::continuePath() {
//    if (hasFinished()) {
//        LERROR("No path to resume (path is empty or has finished).");
//        return;
//    }
//
//    if (_isPlaying) {
//        LERROR("Cannot resume a path that is already playing");
//        return;
//    }
//
//    LINFO("Continuing path...");
//    _isPlaying = true;
//}

void AutoNavigationHandler::abortPath() {
    if (!_isPlaying) {
        LWARNING("No camera path is playing");
        return;
    }
    _isPlaying = false;
    LINFO("Aborted camera path");
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::curvePositions(int nSteps) const {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> positions;
    const double du = 1.0 / nSteps;
    const double length = _currentPath->pathLength();
    for (double u = 0.0; u < 1.0; u += du) {
        glm::dvec3 position = _currentPath->interpolatedPose(u * length).position;
        positions.push_back(position);
    }
    positions.push_back(_currentPath->endPoint().position());

    return positions;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dquat> AutoNavigationHandler::curveOrientations(int nSteps) const {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dquat> orientations;
    const double du = 1.0 / nSteps;
    const double length = _currentPath->pathLength();
    for (double u = 0.0; u <= 1.0; u += du) {
        const glm::dquat orientation = 
            _currentPath->interpolatedPose(u * length).rotation;
        orientations.push_back(orientation);
    }
    orientations.push_back(_currentPath->endPoint().rotation());

    return orientations;
}


// TODO: remove when not needed or combined into pose version
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::curveViewDirections(int nSteps) const {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> viewDirections;
    const double du = 1.0 / nSteps;
    for (double u = 0.0; u < 1.0; u += du) {
        const glm::dquat orientation = _currentPath->interpolatedPose(u).rotation;
        const glm::dvec3 direction = glm::normalize(
            orientation * glm::dvec3(0.0, 0.0, -1.0)
        );
        viewDirections.push_back(direction);
    }

    const glm::dquat orientation = _currentPath->interpolatedPose(1.0).rotation;
    const glm::dvec3 direction = glm::normalize(
        orientation * glm::dvec3(0.0, 0.0, -1.0)
    );
    viewDirections.push_back(direction);

    return viewDirections;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::controlPoints() const {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> points;
    const std::vector<glm::dvec3> curvePoints = _currentPath->controlPoints();
    points.insert(points.end(), curvePoints.begin(), curvePoints.end());

    return points;
}

void AutoNavigationHandler::removeRollRotation(CameraPose& pose, double deltaTime) {
    const glm::dvec3 anchorPos = anchor()->worldPosition();
    const glm::dvec3 cameraDir = glm::normalize(
        pose.rotation * Camera::ViewDirectionCameraSpace
    );
    const double anchorToPosDistance = glm::distance(anchorPos, pose.position);
    const double notTooCloseDistance = deltaTime * anchorToPosDistance;
    glm::dvec3 lookAtPos = pose.position + notTooCloseDistance * cameraDir;
    glm::dquat rollFreeRotation = helpers::lookAtQuaternion(
        pose.position,
        lookAtPos,
        camera()->lookUpVectorWorldSpace()
    );
    pose.rotation = rollFreeRotation;
}

void AutoNavigationHandler::applyStopBehavior(double deltaTime) {
    switch (_stopBehavior) {
        case StopBehavior::None:
            // Do nothing
            break;
        case StopBehavior::Orbit:
            orbitAnchorNode(deltaTime);
            break;
        default:
            throw ghoul::MissingCaseException();
    }
}

void AutoNavigationHandler::orbitAnchorNode(double deltaTime) {
    ghoul_assert(anchor() != nullptr, "Node to orbit must be set!");

    const glm::dvec3 prevPosition = camera()->positionVec3();
    const glm::dquat prevRotation = camera()->rotationQuaternion();
    const glm::dvec3 nodeCenter = anchor()->worldPosition(); 

    const double speedFactor = 0.1 * _orbitSpeedFactor;

    // Compute orbit speed based on factor and distance to surface
    const double orbitRadius = glm::distance(prevPosition, nodeCenter);
    const double distanceToSurface = orbitRadius - anchor()->boundingSphere();
    const double orbitSpeed = distanceToSurface * speedFactor;

    // Compute a new position along the orbit
    const glm::dvec3 up = camera()->lookUpVectorWorldSpace();
    const glm::dquat lookAtNodeRotation = helpers::lookAtQuaternion(
        prevPosition,
        nodeCenter,
        up
    );
    const glm::dvec3 targetForward = lookAtNodeRotation * glm::dvec3(0.0, 0.0, -1.0);
    const glm::dvec3 rightOrbitTangent = glm::normalize(glm::cross(targetForward, up));

    glm::dvec3 newPosition = prevPosition + orbitSpeed * deltaTime * rightOrbitTangent;

    // Adjust for numerical error - make sure we stay at the same height
    const glm::dvec3 nodeToNewPos = newPosition - nodeCenter;
    const double targetHeight = glm::distance(prevPosition, nodeCenter);
    const double heightDiff = glm::length(nodeToNewPos) - targetHeight;
    newPosition -= heightDiff * glm::normalize(nodeToNewPos);

    // Rotate along the orbit, but keep relative orientation with regards to the anchor
    const glm::dquat localRotation = glm::inverse(lookAtNodeRotation) * prevRotation;
    const glm::dquat newLookAtRotation =
        helpers::lookAtQuaternion(newPosition, nodeCenter, up);

    const glm::dquat newRotation = newLookAtRotation * localRotation;

    camera()->setPositionVec3(newPosition);
    camera()->setRotation(newRotation);
}

} // namespace openspace::autonavigation
