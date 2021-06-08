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
#include <openspace/engine/windowdelegate.h>
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
        "The defualt curve type chosen when generating a path, if none is specified."
    };

    constexpr openspace::properties::Property::PropertyInfo IncludeRollInfo = {
        "IncludeRoll",
        "Include Roll",
        "If disabled, roll is removed from the interpolation of camera orientation."
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultStopBehaviorInfo = {
        "DefaultStopBehavior",
        "Default Stop Behavior",
        "The default camera behavior that is applied when the camera reaches and stops "
        "at a target."
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyStopBehaviorWhenIdleInfo = {
        "ApplyStopBehaviorWhenIdle",
        "Apply Stop Behavior When Idle",
        "If enabled, the camera is controlled using the default stop behavior even when"
        "no path is playing."
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedScaleInfo = {
        "SpeedScale",
        "Speed Scale",
        "Scale factor that affects the default speed for a camera path."
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
        DefaultStopBehaviorInfo, 
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _applyStopBehaviorWhenIdle(ApplyStopBehaviorWhenIdleInfo, false)
    , _speedScale(SpeedScaleInfo, 1.f, 0.01f, 2.f)
{
    addPropertySubOwner(_atNodeNavigator);

    _defaultCurveOption.addOptions({
        { CurveType::AvoidCollision, "AvoidCollision" },
        { CurveType::Linear, "Linear" },
        { CurveType::ZoomOutOverview, "ZoomOutOverview"}
    });
    addProperty(_defaultCurveOption);

    addProperty(_includeRoll);
    addProperty(_speedScale);

    addProperty(_applyStopBehaviorWhenIdle);

    // Must be listed in the same order as in enum definition
    _stopBehavior.addOptions({
        { AtNodeNavigator::Behavior::None, "None" },
        { AtNodeNavigator::Behavior::Orbit, "Orbit" }
    });
    _stopBehavior = AtNodeNavigator::Behavior::None;
    _stopBehavior.onChange([this]() {
        _atNodeNavigator.setBehavior(
            AtNodeNavigator::Behavior(_stopBehavior.value())
        );
    });
    addProperty(_stopBehavior);
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
        // For testing, apply at node behavior when idle
        // @TODO: Determine how this should work instead
        if (_applyStopBehaviorWhenIdle) {
            _atNodeNavigator.updateCamera(deltaTime);
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

    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    std::vector<Waypoint> waypoints = instruction.waypoints;
    Waypoint waypointToAdd;

    if (waypoints.empty()) {
        LWARNING("No path was created from instruction. Failed creating waypoints");
        return;
    }
    else {
        // TODO: allow for an instruction to represent a list of waypoints
        waypointToAdd = waypoints[0];
    }

    _currentPath = std::make_unique<Path>(
        instruction.startPoint,
        waypointToAdd,
        CurveType(curveType),
        instruction.duration
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

} // namespace openspace::autonavigation
