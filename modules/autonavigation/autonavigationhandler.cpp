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

} // namespace

namespace openspace::autonavigation {

AutoNavigationHandler::AutoNavigationHandler()
    : properties::PropertyOwner({ "AutoNavigationHandler" })
    , _minAllowedBoundingSphere(MinimalBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _defaultCurveOption(DefaultCurveOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    addProperty(_minAllowedBoundingSphere);

    _defaultCurveOption.addOptions({
        { CurveType::Bezier3, "Bezier3" },
        { CurveType::Linear, "Linear"} 
    });
    addProperty(_defaultCurveOption);
}

AutoNavigationHandler::~AutoNavigationHandler() {} // NOLINT

Camera* AutoNavigationHandler::camera() const {
    return global::navigationHandler.camera();
}

double AutoNavigationHandler::pathDuration() const {
    if (_pathSegments.empty()) 
        return 0.0;

    return _pathSegments.back().endTime();
}

bool AutoNavigationHandler::hasFinished() const {
    return _currentTime > pathDuration();
}

Waypoint AutoNavigationHandler::wayPointFromCamera() {
    glm::dvec3 pos = camera()->positionVec3();
    glm::dquat rot = camera()->rotationQuaternion();
    std::string node = global::navigationHandler.anchorNode()->identifier();
    return Waypoint{pos, rot, node};
}

Waypoint AutoNavigationHandler::lastWayPoint() {
    return _pathSegments.empty() ? wayPointFromCamera() : _pathSegments.back().end();
}

void AutoNavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!_isPlaying || _pathSegments.empty()) return;

    PathSegment currentSegment = _pathSegments[_currentSegmentIndex];

    // compute interpolated camera state
    double prevDistance = _distanceAlongCurrentSegment;
    double displacement = deltaTime * currentSegment.speedAtTime(_currentTime - currentSegment.startTime());
    _distanceAlongCurrentSegment += displacement;

    double relativeDisplacement = _distanceAlongCurrentSegment / currentSegment.pathLength();
    relativeDisplacement = std::max(0.0, std::min(relativeDisplacement, 1.0));

    CameraPose newPose = currentSegment.interpolate(relativeDisplacement);
    std::string newAnchor = currentSegment.getCurrentAnchor(relativeDisplacement);

    // Set anchor node in orbitalNavigator, to render visible nodes and add activate
    // navigation when we reach the end.
    std::string currentAnchor = global::navigationHandler.anchorNode()->identifier();
    if (currentAnchor != newAnchor) {
        global::navigationHandler.orbitalNavigator().setAnchorNode(newAnchor);
    }

    camera()->setPositionVec3(newPose.position);
    camera()->setRotation(newPose.rotation);

    // Have we walked past the current segment?
    if (_currentTime > currentSegment.endTime()) {
        _currentSegmentIndex++;
        _distanceAlongCurrentSegment = 0.0;

        // Stepped past the last segment
        if (_currentSegmentIndex > _pathSegments.size() - 1) {
            LINFO("Reached end of path.");
            _isPlaying = false;
            return;
        }

        currentSegment = _pathSegments[_currentSegmentIndex];

        if (_stopAtTargets) {
            pausePath();
            return;
        }
    }

    _currentTime += deltaTime;
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

    // OBS! Would it be better to save the spec in the handler class? 
    _stopAtTargets = spec.stopAtTargets();

    // Check if we have a specified start navigation state. If so, update first segment
    if (spec.hasStartState() && _pathSegments.size() > 0) {
        Waypoint startState{ spec.startState() };
        _pathSegments[0].setStart(startState);
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
    _currentTime = 0.0;
    _currentSegmentIndex = 0;
    _distanceAlongCurrentSegment = 0.0;
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
    _currentTime = 0.0;
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
    _pathSegments[_currentSegmentIndex].setStart(wayPointFromCamera());
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

    for (PathSegment &p : _pathSegments) {
        for (double u = 0.0; u < 1.0; u += du) {
            auto position = p.interpolate(u).position;
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

    for (PathSegment &p : _pathSegments) {
        std::vector<glm::dvec3> curvePoints = p.getControlPoints();
        points.insert(points.end(), curvePoints.begin(), curvePoints.end());
    }

    return points;
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
        glm::dvec3 nodeToPrev= lastWayPoint().position() - nodePos;
        // TODO: compute position in a more clever way

        const double radius = findValidBoundingSphere(targetNode);
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

    Waypoint endState{ targetPos, targetRot, identifier };

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

    Waypoint endState{ props->navState };

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
    double startTime = pathDuration();
    Waypoint waypoint = lastWayPoint();
    PathSegment newSegment{ waypoint, waypoint, startTime, CurveType::Pause };

    // TODO: implement more complex behavior later

    // TODO: handle duration better
    if (duration.has_value()) {
        newSegment.setDuration(duration.value());
    }
    _pathSegments.push_back(newSegment);
}

void AutoNavigationHandler::addSegment(Waypoint& waypoint, std::optional<double> duration) 
{
    double startTime = pathDuration();

    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    PathSegment newSegment{ lastWayPoint(), waypoint, startTime, CurveType(curveType) };

    // TODO: handle duration better
    if (duration.has_value()) {
        newSegment.setDuration(duration.value());
    }
    _pathSegments.push_back(newSegment);
}

double AutoNavigationHandler::findValidBoundingSphere(const SceneGraphNode* node) {
    double bs = static_cast<double>(node->boundingSphere());

    if (bs < _minAllowedBoundingSphere) {

        // If the bs of the target is too small, try to find a good value in a child node.
        // Only check the closest children, to avoid deep traversal in the scene graph. Also,
        // the possibility to find a bounding sphere represents the visual size of the 
        // target well is higher for these nodes.
        for (SceneGraphNode* child : node->children()) {
            bs = static_cast<double>(child->boundingSphere());
            if (bs > _minAllowedBoundingSphere) {
                LWARNING(fmt::format(
                    "The scene graph node '{}' has no, or a very small, bounding sphere. Using bounding sphere of child node '{}' in computations.",
                    node->identifier(), 
                    child->identifier()
                ));

                return bs;
            }
        }

        LWARNING(fmt::format("The scene graph node '{}' has no, or a very small,"
            "bounding sphere. This might lead to unexpected results.", node->identifier()));

        bs = _minAllowedBoundingSphere;
    }

    return bs;
}

} // namespace openspace::autonavigation
