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

#include <modules/autonavigation/autonavigationhandler.h>

#include <modules/autonavigation/helperfunctions.h>
#include <modules/autonavigation/instruction.h>
#include <modules/autonavigation/pathcurves.h>
#include <modules/autonavigation/pathspecification.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/quaternion.hpp>
#include <algorithm>

namespace {
    constexpr const char* _loggerCat = "AutoNavigationHandler";

    constexpr const openspace::properties::Property::PropertyInfo MinimalBoundingSphereInfo = {
        "MinimalBoundingSphere",
        "Minimal BoundingSphere",
        "The minimal allowed value for a bounding sphere. Used for computation of target "
        "positions and path generation, to avoid issues when there is no bounding sphere."
    };

    constexpr const openspace::properties::Property::PropertyInfo DefaultCurveOptionInfo = {
        "DefaultCurveOption",
        "Default Curve Option",
        "The defualt curve type chosen when generating a path, if none is specified."
    };

    constexpr const openspace::properties::Property::PropertyInfo IncludeRollInfo = {
        "IncludeRollInfo",
        "Include Roll",
        "If disabled, roll is removed from the interpolation of camera orientation."
    };

} // namespace

namespace openspace::autonavigation {

AutoNavigationHandler::AutoNavigationHandler()
    : properties::PropertyOwner({ "AutoNavigationHandler" })
    , _minAllowedBoundingSphere(MinimalBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _defaultCurveOption(DefaultCurveOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _includeRoll(IncludeRollInfo, false)
{
    addProperty(_minAllowedBoundingSphere);

    _defaultCurveOption.addOptions({
        { CurveType::Bezier3, "Bezier3" },
        { CurveType::Linear, "Linear"} 
    });
    addProperty(_defaultCurveOption);
    addProperty(_includeRoll);
}

AutoNavigationHandler::~AutoNavigationHandler() {} // NOLINT

Camera* AutoNavigationHandler::camera() const {
    return global::navigationHandler.camera();
}

bool AutoNavigationHandler::hasFinished() const {
    int lastIndex = (int)_pathSegments.size() - 1;
    return _currentSegmentIndex > lastIndex; 
}

void AutoNavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!_isPlaying || _pathSegments.empty()) return;

    std::unique_ptr<PathSegment> &currentSegment = _pathSegments[_currentSegmentIndex];

    CameraPose newPose = currentSegment->traversePath(deltaTime);
    std::string newAnchor = currentSegment->getCurrentAnchor();

    // Set anchor node in orbitalNavigator, to render visible nodes and add activate
    // navigation when we reach the end.
    std::string currentAnchor = global::navigationHandler.anchorNode()->identifier();
    if (currentAnchor != newAnchor) {
        global::navigationHandler.orbitalNavigator().setAnchorNode(newAnchor);
    }

    if (!_includeRoll) {
        const double notTooCloseDistance = 100.0;
        glm::dvec3 cameraDir = glm::normalize(newPose.rotation * Camera::ViewDirectionCameraSpace);
        glm::dvec3 lookAtPos = newPose.position + notTooCloseDistance * cameraDir;
        glm::dquat rollFreeRotation = helpers::getLookAtQuaternion(
            newPose.position,
            lookAtPos,
            camera()->lookUpVectorWorldSpace()
        );
        newPose.rotation = rollFreeRotation;
    }

    camera()->setPositionVec3(newPose.position);
    camera()->setRotation(newPose.rotation);

    if (currentSegment->hasReachedEnd()) {
        _currentSegmentIndex++;

        if (hasFinished()) {
            LINFO("Reached end of path.");
            _isPlaying = false;
            return;
        }

        if (_stopAtTargets) {
            pausePath();
            return;
        }
    }
}

void AutoNavigationHandler::createPath(PathSpecification& spec) {
    clearPath();

    bool success = true;
    for (int i = 0; i < spec.instructions()->size(); i++) {
        const Instruction& ins = spec.instructions()->at(i);
        success = handleInstruction(ins, i);

        if (!success)
            break;
    }

    _stopAtTargets = spec.stopAtTargets();

    // Check if we have a specified start navigation state. If so, update first segment
    if (spec.hasStartState() && _pathSegments.size() > 0) {
        Waypoint startState{ spec.startState() , _minAllowedBoundingSphere};
        _pathSegments[0]->setStart(startState);
    }

    if (success) {
        LINFO("Succefully generated camera path.");
        startPath();
    }
    else
        LERROR("Could not create path.");
}

void AutoNavigationHandler::clearPath() {
    LINFO("Clearing path...");
    _pathSegments.clear();
    _currentSegmentIndex = 0;
}

void AutoNavigationHandler::startPath() {
    if (_pathSegments.empty()) {
        LERROR("Cannot start an empty path.");
        return;
    }

    // TODO: remove this line at the end of our project. Used to simplify testing
    global::timeManager.setPause(true);

    //OBS! Until we can handle simulation time: early out if not paused
    if (!global::timeManager.isPaused()) {
        LERROR("Simulation time must be paused to run a camera path.");
        return;
    }

    LINFO("Starting path...");
    _isPlaying = true;
}

void AutoNavigationHandler::pausePath() {
    if (!_isPlaying) {
        LERROR("Cannot pause a path that isn't playing");
        return;
    }
    LINFO(fmt::format("Paused path at target {} / {}", _currentSegmentIndex, _pathSegments.size()));
    _isPlaying = false;
}

void AutoNavigationHandler::continuePath() {
    if (_pathSegments.empty() || hasFinished()) {
        LERROR("No path to resume (path is empty or has finished).");
        return;
    }

    if (_isPlaying) {
        LERROR("Cannot resume a path that is already playing");
        return;
    }

    LINFO("Continuing path...");

    // Recompute start camera state for the upcoming path segment,
    _pathSegments[_currentSegmentIndex]->setStart(wayPointFromCamera());
    _isPlaying = true;
}

void AutoNavigationHandler::stopPath() {
    _isPlaying = false;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::getCurvePositions(int nPerSegment) {
    std::vector<glm::dvec3> positions;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return positions;
    }

    const double du = 1.0 / nPerSegment;

    for (std::unique_ptr<PathSegment> &p : _pathSegments) {
        for (double u = 0.0; u < 1.0; u += du) {
            glm::dvec3 position = p->interpolatedPose(u).position;
            positions.push_back(position);
        }
    }

    return positions;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::getControlPoints() {
    std::vector<glm::dvec3> points;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return points;
    }

    for (std::unique_ptr<PathSegment> &p : _pathSegments) {
        std::vector<glm::dvec3> curvePoints = p->getControlPoints();
        points.insert(points.end(), curvePoints.begin(), curvePoints.end());
    }

    return points;
}

Waypoint AutoNavigationHandler::wayPointFromCamera() {
    glm::dvec3 pos = camera()->positionVec3();
    glm::dquat rot = camera()->rotationQuaternion();
    std::string node = global::navigationHandler.anchorNode()->identifier();
    return Waypoint{ pos, rot, node, _minAllowedBoundingSphere };
}

Waypoint AutoNavigationHandler::lastWayPoint() {
    return _pathSegments.empty() ? wayPointFromCamera() : _pathSegments.back()->end();
}

bool AutoNavigationHandler::handleInstruction(const Instruction& ins, int index) {
    bool success = true;
    switch (ins.type)
    {
    case InstructionType::TargetNode:
        success = handleTargetNodeInstruction(ins);
        break;

    case InstructionType::NavigationState:
        success = handleNavigationStateInstruction(ins);
        break;

    case InstructionType::Pause:
        success = handlePauseInstruction(ins);
        break;

    default:
        LERROR("Non-implemented instruction type.");
        success = false;
        break;
    }

    if (!success) {
        LERROR(fmt::format("Failed handling instruction number {}.", std::to_string(index + 1)));
        return false;
    }

    return true;
}

bool AutoNavigationHandler::handleTargetNodeInstruction(const Instruction& ins) {
    // Verify instruction type
    TargetNodeInstructionProps* props = 
        dynamic_cast<TargetNodeInstructionProps*>(ins.props.get());

    if (!props) {
        LERROR("Could not handle target node instruction.");
        return false;
    }

    // Compute end state 
    std::string& identifier = props->targetNode;
    const SceneGraphNode* targetNode = sceneGraphNode(identifier);

    if (!targetNode) {
        LERROR(fmt::format("Could not find node '{}' to target", identifier));
        return false;
    }

    glm::dvec3 targetPos;
    if (props->position.has_value()) {
        // note that the anchor and reference frame is our targetnode. 
        // The position in instruction is given is relative coordinates.
        targetPos = targetNode->worldPosition() + 
            targetNode->worldRotationMatrix() * props->position.value();
    }
    else {
        // TODO: Instead of this case, allow the curve to set its final position

        glm::dvec3 nodePos = targetNode->worldPosition();
        glm::dvec3 nodeToPrev = lastWayPoint().position() - nodePos;
        // TODO: compute position in a more clever way

        const double radius = WaypointNodeDetails::findValidBoundingSphere(targetNode, _minAllowedBoundingSphere);
        const double defaultHeight = 2 * radius;

        bool hasHeight = props->height.has_value();
        double height = hasHeight ? props->height.value() : defaultHeight;

        // move target position out from surface, along vector to camera
        targetPos = nodePos + glm::normalize(nodeToPrev) * (radius + height);
    }

    glm::dmat4 lookAtMat = glm::lookAt(
        targetPos,
        targetNode->worldPosition(),
        camera()->lookUpVectorWorldSpace()
    );

    glm::dquat targetRot = glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));

    Waypoint endState{ targetPos, targetRot, identifier, _minAllowedBoundingSphere };

    addSegment(endState, ins.props->duration);
    return true;
}

bool AutoNavigationHandler::handleNavigationStateInstruction(const Instruction& ins) {
    // Verify instruction type
    NavigationStateInstructionProps* props =
        dynamic_cast<NavigationStateInstructionProps*>(ins.props.get());

    if (!props) {
        LERROR(fmt::format("Could not handle navigation state instruction."));
        return false;
    }

    Waypoint endState{ props->navState , _minAllowedBoundingSphere };

    addSegment(endState, ins.props->duration);
    return true;
}

bool AutoNavigationHandler::handlePauseInstruction(const Instruction& ins) {
    // Verify instruction type
    PauseInstructionProps* props =
        dynamic_cast<PauseInstructionProps*>(ins.props.get());

    if (!props) {
        LERROR(fmt::format("Could not handle pause instruction."));
        return false;
    }

    // TODO: implement more complex behavior later

    addPause(ins.props->duration);
    return true;
}

void AutoNavigationHandler::addPause(std::optional<double> duration) {
    Waypoint waypoint = lastWayPoint();
    PathSegment segment = PathSegment(waypoint, waypoint, CurveType::Pause);

    // TODO: implement more complex behavior later

    // TODO: handle duration better
    if (duration.has_value()) {
        segment.setDuration(duration.value());
    }
    _pathSegments.push_back(std::unique_ptr<PathSegment>(new PathSegment(segment)));
}

void AutoNavigationHandler::addSegment(Waypoint& waypoint, std::optional<double> duration){
    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    PathSegment segment = PathSegment(lastWayPoint(), waypoint, CurveType(curveType));

    // TODO: handle duration better
    if (duration.has_value()) {
        segment.setDuration(duration.value());
    }
    _pathSegments.push_back(std::unique_ptr<PathSegment>(new PathSegment(segment)));
}

} // namespace openspace::autonavigation
