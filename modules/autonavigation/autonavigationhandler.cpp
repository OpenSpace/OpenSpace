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
#include <modules/autonavigation/pathspecification.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
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

} // namespace

namespace openspace::autonavigation {

// Temporary function to convert a string to one of the bools above. 
// TODO: move to a better place / rewrite
CurveType stringToCurveType(std::string str) {
    if (str.empty()) 
        return CurveType::None;

    std::transform(str.begin(), str.end(), str.begin(), ::tolower);

    if (str == "bezier3") {
        return CurveType::Bezier3;
    }
    else if (str == "linear") {
        return CurveType::Linear;
    }
    else {
        LERROR(fmt::format("'{}' is not a valid curve type! Choosing default.", str));
        return CurveType::None;
    }
}

AutoNavigationHandler::AutoNavigationHandler()
    : properties::PropertyOwner({ "AutoNavigationHandler" })
    , _minAllowedBoundingSphere(MinimalBoundingSphereInfo, 10.0, 1.0, 3e10)
{
    addProperty(_minAllowedBoundingSphere);
}

AutoNavigationHandler::~AutoNavigationHandler() {} // NOLINT

Camera* AutoNavigationHandler::camera() const {
    return global::navigationHandler.camera();
}

double AutoNavigationHandler::pathDuration() const {
    double sum = 0.0;
    for (const PathSegment& ps : _pathSegments) {
        sum += ps.duration();
    }
    return sum;
}

bool AutoNavigationHandler::hasFinished() const {
    return _currentTime > pathDuration();
}

CameraState AutoNavigationHandler::currentCameraState() {
    CameraState cs;
    cs.position = camera()->positionVec3();
    cs.rotation = camera()->rotationQuaternion();
    cs.referenceNode = global::navigationHandler.anchorNode()->identifier();
    return cs;
}

void AutoNavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!_isPlaying || _pathSegments.empty()) return;

    _currentTime += deltaTime;

    PathSegment& cps = _pathSegments[_currentSegmentIndex];

    // Have we walked past the current segment?
    if (_currentTime > cps.endTime()) {
        _currentSegmentIndex++;
        _distanceAlongCurrentSegment = 0.0;

        // WStepped past the last segment
        if (_currentSegmentIndex > _pathSegments.size() - 1) {
            LINFO("Reached end of path.");
            _isPlaying = false;
            return;
        }

        cps = _pathSegments[_currentSegmentIndex];

        if (_stopAtTargets) {
            pausePath();
            return;
        }
    }

    double prevDistance = _distanceAlongCurrentSegment;
    double displacement = deltaTime * cps.speedAtTime(_currentTime - cps.startTime());
    _distanceAlongCurrentSegment += displacement;

    double relativeDisplacement = _distanceAlongCurrentSegment / cps.pathLength();
    relativeDisplacement = std::max(0.0, std::min(relativeDisplacement, 1.0));

    // When halfway along a curve, set anchor node in orbitalNavigator, to render 
    // visible nodes and add possibility to navigate when we reach the end.
    std::string targetAnchor = cps.end().referenceNode;
    std::string currentAnchor = global::navigationHandler.anchorNode()->identifier();

    if ((relativeDisplacement > 0.5) && (currentAnchor != targetAnchor)) {
        global::navigationHandler.orbitalNavigator().setAnchorNode(targetAnchor);
    } 

    glm::dvec3 cameraPosition = cps.getPositionAt(relativeDisplacement);
    glm::dquat cameraRotation = cps.getRotationAt(relativeDisplacement);

    camera()->setPositionVec3(cameraPosition); 
    camera()->setRotation(cameraRotation);
}

void AutoNavigationHandler::createPath(PathSpecification& spec) {
    clearPath();

    _pathCurveType = stringToCurveType(spec.curveType());

    bool success = true;
    for (int i = 0; i < spec.instructions()->size(); i++) {
        const Instruction& ins = spec.instructions()->at(i);
        success = handleInstruction(ins, i);

        if (!success)
            break;
    }

    // OBS! Would it be better to save the spec in the handler class? 
    _stopAtTargets = spec.stopAtTargets();

    // Check if we have a specified start state. If so, update the first segment
    if (spec.hasStartState() && _pathSegments.size() > 0) {
        CameraState startState = cameraStateFromNavigationState(spec.startState());
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

    // Recompute start camera state for the upcoming path segment, to avoid clipping to
    // the old camera state.
    _pathSegments[_currentSegmentIndex].setStart(currentCameraState());
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
            auto position = p.getPositionAt(u);
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

bool AutoNavigationHandler::handleInstruction(const Instruction& instruction, int index) {
    bool success = true;
    switch (instruction.type)
    {
    case InstructionType::TargetNode:
        success = handleTargetNodeInstruction(instruction);
        break;

    case InstructionType::NavigationState:
        success = handleNavigationStateInstruction(instruction);
        break;

    case InstructionType::Pause:
        success = handlePauseInstruction(instruction);
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

bool AutoNavigationHandler::handleTargetNodeInstruction(const Instruction& instruction) {
    // Verify instruction type
    TargetNodeInstructionProps* props = 
        dynamic_cast<TargetNodeInstructionProps*>(instruction.props.get());

    if (!props) {
        LERROR("Could not handle target node instruction.");
        return false;
    }

    CameraState startState = 
        _pathSegments.empty() ? currentCameraState() : _pathSegments.back().end();

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
        targetPos = computeTargetPositionAtNode(
            targetNode, 
            startState.position,
            props->height
        );
    }

    glm::dmat4 lookAtMat = glm::lookAt(
        targetPos,
        targetNode->worldPosition(),
        camera()->lookUpVectorWorldSpace()
    );

    glm::dquat targetRot = glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));

    CameraState endState = CameraState{ targetPos, targetRot, identifier };

    addSegment(startState, endState, instruction.props->duration);
    return true;
}

bool AutoNavigationHandler::handleNavigationStateInstruction(
    const Instruction& instruction)
{
    // Verify instruction type
    NavigationStateInstructionProps* props =
        dynamic_cast<NavigationStateInstructionProps*>(instruction.props.get());

    if (!props) {
        LERROR(fmt::format("Could not handle navigation state instruction."));
        return false;
    }

    CameraState startState =
        _pathSegments.empty() ? currentCameraState() : _pathSegments.back().end();

    interaction::NavigationHandler::NavigationState ns = props->navState;
    CameraState endState = cameraStateFromNavigationState(ns);

    addSegment(startState, endState, instruction.props->duration);
    return true;
}

bool AutoNavigationHandler::handlePauseInstruction(const Instruction& instruction)
{
    // Verify instruction type
    PauseInstructionProps* props =
        dynamic_cast<PauseInstructionProps*>(instruction.props.get());

    if (!props) {
        LERROR(fmt::format("Could not handle pause instruction."));
        return false;
    }

    CameraState state =_pathSegments.empty() 
        ?  currentCameraState() 
        : _pathSegments.back().end();

    // TODO: implement more complex behavior later

    addPause(state, instruction.props->duration);
    return true;
}

void AutoNavigationHandler::addSegment(CameraState& start, 
    CameraState& end, std::optional<double> duration) 
{
    // compute startTime 
    double startTime = 0.0;
    if (!_pathSegments.empty()) {
        PathSegment& last = _pathSegments.back();
        startTime = last.startTime() + last.duration();
    }

    bool hasType = (_pathCurveType != CurveType::None);

    PathSegment newSegment = hasType 
        ? PathSegment{ start, end, startTime, _pathCurveType } 
        : PathSegment{ start, end, startTime };

    // TODO: handle duration better
    if (duration.has_value()) {
        newSegment.setDuration(duration.value());
    }
    _pathSegments.push_back(newSegment);
}

void AutoNavigationHandler::addPause(CameraState& state, std::optional<double> duration) {
    // compute startTime 
    double startTime = 0.0;
    if (!_pathSegments.empty()) {
        PathSegment& last = _pathSegments.back();
        startTime = last.startTime() + last.duration();
    }

    PathSegment newSegment = PathSegment{ state, state, startTime, CurveType::Pause };

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

glm::dvec3 AutoNavigationHandler::computeTargetPositionAtNode(
    const SceneGraphNode* node, glm::dvec3 prevPos, std::optional<double> heightOptional)
{
    glm::dvec3 targetPos = node->worldPosition();
    glm::dvec3 targetToPrevVector = prevPos - targetPos;
    // TODO: compute position in a more clever way

    const double radius = findValidBoundingSphere(node);
    const double defaultHeight = 2 * radius;

    bool hasHeight = heightOptional.has_value();
    double height = hasHeight ? heightOptional.value() : defaultHeight;

    // move target position out from surface, along vector to camera
    targetPos += glm::normalize(targetToPrevVector) * (radius + height);

    return targetPos;
}

CameraState AutoNavigationHandler::cameraStateFromNavigationState(
    const interaction::NavigationHandler::NavigationState& ns) 
{
    // OBS! The following code is exactly the same as used in 
    // NavigationHandler::applyNavigationState. Should probably be made into a function.
    const SceneGraphNode* referenceFrame = sceneGraphNode(ns.referenceFrame);
    const SceneGraphNode* anchorNode = sceneGraphNode(ns.anchor); // The anchor is also the target

    if (!anchorNode) {
        LERROR(fmt::format("Could not find node '{}' to target. Returning empty state.", ns.anchor));
        return CameraState{};
    }

    const glm::dvec3 anchorWorldPosition = anchorNode->worldPosition();
    const glm::dmat3 referenceFrameTransform = referenceFrame->worldRotationMatrix();

    const glm::dvec3 targetPositionWorld = anchorWorldPosition +
        glm::dvec3(referenceFrameTransform * glm::dvec4(ns.position, 1.0));

    glm::dvec3 up = ns.up.has_value() ?
        glm::normalize(referenceFrameTransform * ns.up.value()) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the aim is centered in view.
    glm::dvec3 neutralView =
        glm::normalize(anchorWorldPosition - targetPositionWorld);

    glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        neutralView,
        up
    )));

    glm::dquat pitchRotation = glm::angleAxis(ns.pitch, glm::dvec3(1.f, 0.f, 0.f));
    glm::dquat yawRotation = glm::angleAxis(ns.yaw, glm::dvec3(0.f, -1.f, 0.f));

    glm::quat targetRotation = neutralCameraRotation * yawRotation * pitchRotation;

    return CameraState{ targetPositionWorld, targetRotation, ns.anchor };
}

} // namespace openspace::autonavigation
