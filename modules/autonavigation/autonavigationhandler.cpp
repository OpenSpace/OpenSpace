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
#include <modules/autonavigation/pathinstruction.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
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

    constexpr openspace::properties::Property::PropertyInfo StopAtTargetsPerDefaultInfo = {
        "StopAtTargetsPerDefault",
        "Stop At Targets Per Default",
        "Applied during path creation. If enabled, stops are automatically added "
        "between the path segments. The user must then choose to continue the path "
        "after reaching a target"
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

    constexpr openspace::properties::Property::PropertyInfo RelevantNodeTagsInfo = {
        "RelevantNodeTags",
        "Relevant Node Tags",
        "List of tags for the nodes that are relevant for path creation, for example "
        "when avoiding collisions."
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultPositionOffsetAngleInfo = {
        "DefaultPositionOffsetAngle",
        "Default Position Offset Angle",
        "Used for creating a default position at a target node. The angle (in degrees) "
        "specifies the deviation from the line connecting the target node and the sun, "
        "in the direction of the camera position at the start of the path."
    };

    constexpr openspace::properties::Property::PropertyInfo PickClosestTargetPosInfo = {
        "PickClosestTargetPosition",
        "Pick Closest Target Position",
        "If this flag is set to two the target position for a node based motion is "
        "computed along the line from the previous camera position."
    };

    constexpr openspace::properties::Property::PropertyInfo IntegrationResolutionInfo = {
        "IntegrationResolution",
        "Path Integration Resolution",
        "The number of steps used to integrate along the spline curve every frame. A "
        "larger number increases the precision, at the cost of reduced efficiency."
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
    , _relevantNodeTags(RelevantNodeTagsInfo)
    , _defaultPositionOffsetAngle(DefaultPositionOffsetAngleInfo, 30.f, -90.f, 90.f)
    , _pickClosestTargetPosition(PickClosestTargetPosInfo, false)
    , _integrationResolutionPerFrame(IntegrationResolutionInfo, 100, 10, 500)
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

    addProperty(_applyStopBehaviorWhenIdle);

    // Add the relevant tags
    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };;
    _relevantNodeTags.onChange([this]() { _relevantNodes = findRelevantNodes(); });
    addProperty(_relevantNodeTags);

    addProperty(_defaultPositionOffsetAngle);
    addProperty(_pickClosestTargetPosition);
    addProperty(_integrationResolutionPerFrame);
    addProperty(_speedScale);
}

AutoNavigationHandler::~AutoNavigationHandler() {} // NOLINT

Camera* AutoNavigationHandler::camera() const {
    return global::navigationHandler->camera();
}

const SceneGraphNode* AutoNavigationHandler::anchor() const {
    return global::navigationHandler->anchorNode();
}

const std::vector<SceneGraphNode*>& AutoNavigationHandler::relevantNodes() const {
    return _relevantNodes;
}

int AutoNavigationHandler::integrationResolutionPerFrame() const {
    return _integrationResolutionPerFrame;
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

    if (noCurrentPath()) {
        return;
    }

    if (!_isPlaying) {
        // For testing, apply at node behavior when idle
        // @TODO: Determine how this should work instead
        if (_applyStopBehaviorWhenIdle) {
            _atNodeNavigator.updateCamera(deltaTime);
        }
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

    // TODO: do this in some initialize function instead
    _relevantNodes = findRelevantNodes();

    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    std::vector<Waypoint> waypoints = instruction.waypoints();
    Waypoint waypointToAdd;

    if (waypoints.empty()) {
        if (instruction.type == PathInstruction::Type::Node) {
            // TODO: allow curves to compute default waypoint instead
            waypointToAdd = computeDefaultWaypoint(instruction);
        }
        else {
            LWARNING("No path was created from instruction. Failed creating waypoints");
            return;
        }
    }
    else {
        // TODO: allow for an instruction to represent a list of waypoints
        waypointToAdd = waypoints[0];
    }

    bool hasStartState = instruction.startState.has_value();
    Waypoint startState = hasStartState ? Waypoint(instruction.startState.value()) 
        : waypointFromCamera();

    _currentPath = std::make_unique<Path>(
        startState,
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
//
//    // recompute start camera state for the upcoming path segment,
//    _currentPath.segments[_currentSegmentIndex].setStartPoint(wayPointFromCamera());
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
std::vector<glm::dvec3> AutoNavigationHandler::curvePositions(int nPerSegment) {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> positions;
    const double du = 1.0 / nPerSegment;
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
std::vector<glm::dquat> AutoNavigationHandler::curveOrientations(int nPerSegment) {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dquat> orientations;
    const double du = 1.0 / nPerSegment;
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
std::vector<glm::dvec3> AutoNavigationHandler::curveViewDirections(int nPerSegment) {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> viewDirections;
    const double du = 1.0 / nPerSegment;
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
std::vector<glm::dvec3> AutoNavigationHandler::controlPoints() {
    if (noCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> points;
    const std::vector<glm::dvec3> curvePoints = _currentPath->controlPoints();
    points.insert(points.end(), curvePoints.begin(), curvePoints.end());

    return points;
}

Waypoint AutoNavigationHandler::waypointFromCamera() {
    const glm::dvec3 pos = camera()->positionVec3();
    const glm::dquat rot = camera()->rotationQuaternion();
    const std::string node = global::navigationHandler->anchorNode()->identifier();
    return Waypoint{ pos, rot, node };
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

// Test if the node lies within a given proximity radius of any relevant node in the scene
SceneGraphNode* AutoNavigationHandler::findNodeNearTarget(const SceneGraphNode* node) {
    glm::dvec3 nodePos = node->worldPosition();
    std::string nodeId = node->identifier();

    const float proximityRadiusFactor = 3.f;

    for (SceneGraphNode* n : _relevantNodes) {
        if (n->identifier() == nodeId)
            continue;

        float bs = static_cast<float>(n->boundingSphere());
        float proximityRadius = proximityRadiusFactor * bs;
        const glm::dmat4 invModelTransform = glm::inverse(n->modelTransform());
        const glm::dvec3 posInModelCoords = invModelTransform * glm::dvec4(nodePos, 1.0);

        bool isClose = helpers::isPointInsideSphere(
            posInModelCoords,
            glm::dvec3(0.0, 0.0, 0.0),
            proximityRadius
        );

        if (isClose) {
            return n;
        }
    }

    return nullptr;
}

// OBS! The desired default waypoint may vary between curve types.
// TODO: let the curves update the default position if no exact position is required
Waypoint AutoNavigationHandler::computeDefaultWaypoint(const PathInstruction& ins) {
    const SceneGraphNode* targetNode = sceneGraphNode(ins.nodeIdentifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", ins.nodeIdentifier));
        return Waypoint();
    }

    const float epsilon = 1e-5f;
    const glm::dvec3 nodePos = targetNode->worldPosition();
    const glm::dvec3 sunPos = glm::dvec3(0.0, 0.0, 0.0);
    const SceneGraphNode* closeNode = findNodeNearTarget(targetNode);
    const glm::dvec3 prevPos = waypointFromCamera().position();

    // Position
    glm::dvec3 stepDirection;
    if (closeNode) {
        // If the node is close to another node in the scene, make sure that the
        // position is set to minimize risk of collision
        stepDirection = glm::normalize(nodePos - closeNode->worldPosition());
    }
    else if (glm::length(sunPos - nodePos) < epsilon) {
        // Special case for when the target is the Sun. Assumption: want an overview of
        // the solar system, and not stay in the orbital plane
        stepDirection = glm::dvec3(0.0, 0.0, 1.0);
    }
    else if (_pickClosestTargetPosition) {
        stepDirection = glm::normalize(prevPos - nodePos);
    }
    else {
        // Go to a point that is being lit up by the sun, slightly offsetted from sun
        // direction
        const glm::dvec3 targetToPrev = prevPos - nodePos;
        const glm::dvec3 targetToSun = sunPos - nodePos;

        const glm::dvec3 axis = glm::normalize(glm::cross(targetToPrev, targetToSun));
        const float angle = glm::radians(-1.f * _defaultPositionOffsetAngle);
        const glm::dquat offsetRotation = angleAxis(static_cast<double>(angle), axis);

        stepDirection = glm::normalize(offsetRotation * targetToSun);
    }

    const double radius = WaypointNodeDetails::findValidBoundingSphere(targetNode);
    const double defaultHeight = 2.0 * radius;
    const double height = ins.height.has_value() ? ins.height.value() : defaultHeight;

    const glm::dvec3 targetPos = nodePos + stepDirection * (radius + height);

    // Up direction
    glm::dvec3 up = camera()->lookUpVectorWorldSpace();
    if (ins.useTargetUpDirection) {
        // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth, 
        // which is where it matters the most. A better solution would be to make each 
        // sgn aware of its own 'up' and query 
        up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
    }

    // Rotation
    const glm::dvec3 lookAtPos = targetNode->worldPosition();
    const glm::dquat targetRot = helpers::lookAtQuaternion(targetPos, lookAtPos, up);

    return Waypoint{ targetPos, targetRot, ins.nodeIdentifier };
}

std::vector<SceneGraphNode*> AutoNavigationHandler::findRelevantNodes() {
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    const std::vector<std::string> relevantTags = _relevantNodeTags;

    if (allNodes.empty() || relevantTags.empty())
        return std::vector<SceneGraphNode*>{};

    auto isRelevant = [&](const SceneGraphNode* node) {
        const std::vector<std::string> tags = node->tags();
        auto result = std::find_first_of(
            relevantTags.begin(),
            relevantTags.end(),
            tags.begin(),
            tags.end()
        );

        // does not match any tags => not interesting
        if (result == relevantTags.end()) {
            return false;
        }

        return node->renderable() && (node->boundingSphere() > 0.0);
    };

    std::vector<SceneGraphNode*> resultingNodes{};
    std::copy_if(
        allNodes.begin(),
        allNodes.end(),
        std::back_inserter(resultingNodes),
        isRelevant
    );

    return resultingNodes;
}

} // namespace openspace::autonavigation
