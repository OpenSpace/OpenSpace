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
    , _defaultCurveOption(DefaultCurveOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _includeRoll(IncludeRollInfo, false)
    , _stopAtTargetsPerDefault(StopAtTargetsPerDefaultInfo, false)
    , _defaultStopBehavior(DefaultStopBehaviorInfo, properties::OptionProperty::DisplayType::Dropdown)
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
        { CurveType::ZoomOutOverview, "ZoomOutOverview"},
        { CurveType::AvoidCollisionLookAt, "AvoidCollisionLookAt" }
    });
    addProperty(_defaultCurveOption);

    addProperty(_includeRoll);
    addProperty(_stopAtTargetsPerDefault);

    // Must be listed in the same order as in enum definition
    _defaultStopBehavior.addOptions({
        { AtNodeNavigator::Behavior::None, "None" },
        { AtNodeNavigator::Behavior::Orbit, "Orbit" }
    });
    _defaultStopBehavior = AtNodeNavigator::Behavior::None;
    addProperty(_defaultStopBehavior);

    addProperty(_applyStopBehaviorWhenIdle);

    // Add the relevant tags
    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };;
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

bool AutoNavigationHandler::hasFinished() const {
    if (_pathSegments.empty()) {
        return true;
    }

    unsigned int lastIndex = (unsigned int)_pathSegments.size() - 1;
    return _currentSegmentIndex > lastIndex;
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

void AutoNavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!_isPlaying || _pathSegments.empty()) {
        // for testing, apply at node behavior when idle
        if (_applyStopBehaviorWhenIdle) {
            if (_atNodeNavigator.behavior() != _defaultStopBehavior.value()) {
                _atNodeNavigator.setBehavior(
                    AtNodeNavigator::Behavior(_defaultStopBehavior.value())
                );
            }
            _atNodeNavigator.updateCamera(deltaTime);
        }
        return;
    }

    if (_activeStop) {
        applyStopBehaviour(deltaTime);
        return;
    }

    // If for some reason the time is no longer paused, pause it again
    if (!global::timeManager->isPaused()) {
        global::timeManager->setPause(true);
        LINFO("Cannot start simulation time during camera motion");
    }

    std::unique_ptr<PathSegment> &currentSegment = _pathSegments[_currentSegmentIndex];

    CameraPose newPose = currentSegment->traversePath(deltaTime);
    const std::string newAnchor = currentSegment->currentAnchor();

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

    if (currentSegment->hasReachedEnd()) {
        _currentSegmentIndex++;

        if (hasFinished()) {
            LINFO("Reached end of path.");
            _isPlaying = false;
            return;
        }

        int stopIndex = _currentSegmentIndex - 1;

        if (_stops[stopIndex].shouldStop) {
            pauseAtTarget(stopIndex);
            return;
        }
    }
}

void AutoNavigationHandler::createPath(PathSpecification& spec) {
    clearPath();

    // TODO: do this in some initialize function instead, and update
    // when list of tags is updated. Also depends on scene change?
    _relevantNodes = findRelevantNodes();

    if (spec.stopAtTargetsSpecified()) {
        _stopAtTargetsPerDefault = spec.stopAtTargets();
        LINFO("Property for stop at targets per default was overridden by path specification.");
    }

    const int nInstructions = static_cast<int>(spec.instructions()->size());

    for (int i = 0; i < nInstructions; i++) {
        const Instruction* instruction = spec.instruction(i);
        if (instruction) {
            addSegment(instruction, i);

            // Add info about stops between segments
            if (i < nInstructions - 1) {
                addStopDetails(instruction);
            }
        }
    }

    // Check if we have a specified start navigation state. If so, update first segment
    if (spec.hasStartState() && _pathSegments.size() > 0) {
        Waypoint startState{ spec.startState() };
        _pathSegments[0]->setStart(startState);
    }

    LINFO(fmt::format(
        "Succefully generated camera path with {} segments.", _pathSegments.size()
    ));
    startPath();
}

void AutoNavigationHandler::clearPath() {
    LINFO("Clearing path...");
    _pathSegments.clear();
    _stops.clear();
    _currentSegmentIndex = 0;
}

void AutoNavigationHandler::startPath() {
    if (_pathSegments.empty()) {
        LERROR("Cannot start an empty path.");
        return;
    }

    ghoul_assert(
        _stops.size() == (_pathSegments.size() - 1),
        "Must have exactly one stop entry between every segment."
    );

    //OBS! Until we can handle simulation time: early out if not paused
    if (!global::timeManager->isPaused()) {
        LERROR("Simulation time must be paused to run a camera path.");
        return;
    }

    LINFO("Starting path...");
    _isPlaying = true;
    _activeStop = nullptr;
}

void AutoNavigationHandler::continuePath() {
    if (_pathSegments.empty() || hasFinished()) {
        LERROR("No path to resume (path is empty or has finished).");
        return;
    }

    if (_isPlaying && !_activeStop) {
        LERROR("Cannot resume a path that is already playing");
        return;
    }

    LINFO("Continuing path...");

    // recompute start camera state for the upcoming path segment,
    _pathSegments[_currentSegmentIndex]->setStart(wayPointFromCamera());
    _activeStop = nullptr;
}

void AutoNavigationHandler::abortPath() {
    if (!_isPlaying) {
        LWARNING("No camera path is playing.");
        return;
    }
    _isPlaying = false;
    LINFO("Aborted camera path.");
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::curvePositions(int nPerSegment) {
    std::vector<glm::dvec3> positions;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return positions;
    }

    const double du = 1.0 / nPerSegment;

    for (std::unique_ptr<PathSegment>& p : _pathSegments) {
        const double length = p->pathLength();
        for (double u = 0.0; u < 1.0; u += du) {
            glm::dvec3 position = p->interpolatedPose(u * length).position;
            positions.push_back(position);
        }
        positions.push_back(p->end().position());
    }

    return positions;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dquat> AutoNavigationHandler::curveOrientations(int nPerSegment) {
    std::vector<glm::dquat> orientations;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return orientations;
    }

    const double du = 1.0 / nPerSegment;

    for (std::unique_ptr<PathSegment>& p : _pathSegments) {
        const double length = p->pathLength();
        for (double u = 0.0; u <= 1.0; u += du) {
            const glm::dquat orientation = p->interpolatedPose(u * length).rotation;
            orientations.push_back(orientation);
        }
        orientations.push_back(p->end().rotation());
    }

    return orientations;
}


// TODO: remove when not needed or combined into pose version
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::curveViewDirections(int nPerSegment) {
    std::vector<glm::dvec3> viewDirections;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return viewDirections;
    }

    const double du = 1.0 / nPerSegment;

    for (std::unique_ptr<PathSegment>& p : _pathSegments) {
        for (double u = 0.0; u < 1.0; u += du) {
            const glm::dquat orientation = p->interpolatedPose(u).rotation;
            const glm::dvec3 direction = glm::normalize(
                orientation * glm::dvec3(0.0, 0.0, -1.0)
            );
            viewDirections.push_back(direction);
        }

        const glm::dquat orientation = p->interpolatedPose(1.0).rotation;
        const glm::dvec3 direction = glm::normalize(
            orientation * glm::dvec3(0.0, 0.0, -1.0)
        );
        viewDirections.push_back(direction);
    }

    return viewDirections;
}

// TODO: remove when not needed
// Created for debugging
std::vector<glm::dvec3> AutoNavigationHandler::controlPoints() {
    std::vector<glm::dvec3> points;

    if (_pathSegments.empty()) {
        LERROR("There is no current path to sample points from.");
        return points;
    }

    for (std::unique_ptr<PathSegment> &p : _pathSegments) {
        const std::vector<glm::dvec3> curvePoints = p->controlPoints();
        points.insert(points.end(), curvePoints.begin(), curvePoints.end());
    }

    return points;
}

Waypoint AutoNavigationHandler::wayPointFromCamera() {
    const glm::dvec3 pos = camera()->positionVec3();
    const glm::dquat rot = camera()->rotationQuaternion();
    const std::string node = global::navigationHandler->anchorNode()->identifier();
    return Waypoint{ pos, rot, node };
}

Waypoint AutoNavigationHandler::lastWayPoint() {
    return _pathSegments.empty() ? wayPointFromCamera() : _pathSegments.back()->end();
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

void AutoNavigationHandler::pauseAtTarget(int i) {
    if (!_isPlaying || _activeStop) {
        LERROR("Cannot pause a path that isn't playing");
        return;
    }

    if (i < 0 || i > _stops.size() - 1) {
        LERROR("Invalid target number: " + std::to_string(i));
        return;
    }

    _activeStop = &_stops[i];

    if (!_activeStop) return;

    _atNodeNavigator.setBehavior(_activeStop->behavior);

    bool hasDuration = _activeStop->duration.has_value();

    std::string infoString = hasDuration ?
        fmt::format("{} seconds", _activeStop->duration.value()) : "until continued";

    LINFO(fmt::format("Paused path at target {} / {} ({})",
        _currentSegmentIndex,
        _pathSegments.size(),
        infoString
    ));

    _progressedTimeInStop = 0.0;
}

void AutoNavigationHandler::applyStopBehaviour(double deltaTime) {
    _progressedTimeInStop += deltaTime;
    _atNodeNavigator.updateCamera(deltaTime);

    if (!_activeStop->duration.has_value()) return;

    if (_progressedTimeInStop >= _activeStop->duration.value()) {
        continuePath();
    }
}

void AutoNavigationHandler::addSegment(const Instruction* ins, int index) {
    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    std::vector<Waypoint> waypoints = ins->waypoints();
    Waypoint waypointToAdd;

    if (waypoints.size() == 0) {
        const TargetNodeInstruction* targetNodeIns =
            dynamic_cast<const TargetNodeInstruction*>(ins);

        if (targetNodeIns) {
            // TODO: allow curves to compute default waypoint instead
            waypointToAdd = computeDefaultWaypoint(targetNodeIns);
        }
        else {
            LWARNING(fmt::format(
                "No path segment was created from instruction {}. "
                "No waypoints could be created.",
                index
            ));
            return;
        }
    }
    else {
        // TODO: allow for a list of waypoints
        waypointToAdd = waypoints[0];
    }

    _pathSegments.push_back(std::make_unique<PathSegment>(
        lastWayPoint(),
        waypointToAdd,
        CurveType(curveType),
        ins->duration
    ));
}

void AutoNavigationHandler::addStopDetails(const Instruction* ins) {
    StopDetails stopEntry;
    stopEntry.shouldStop = _stopAtTargetsPerDefault.value();

    if (ins->stopAtTarget.has_value()) {
        stopEntry.shouldStop = ins->stopAtTarget.value();
    }

    if (stopEntry.shouldStop) {
        stopEntry.duration = ins->stopDuration;

        std::string anchorIdentifier = lastWayPoint().nodeDetails.identifier;
        stopEntry.behavior = AtNodeNavigator::Behavior(_defaultStopBehavior.value());

        if (ins->stopBehavior.has_value()) {
            std::string behaviorString = ins->stopBehavior.value();

            // This is a bit ugly, since it relies on the OptionProperty::Option and
            // AtNodeNavigator::Behavior being implicitly converted to the same int value.
            // TODO: Come up with a nicer solution (this get to work for now...)

            using Option = properties::OptionProperty::Option;
            std::vector<Option> options = _defaultStopBehavior.options();
            std::vector<Option>::iterator foundIt = std::find_if(
                options.begin(),
                options.end(),
                [&](Option& o) { return o.description == behaviorString; }
            );

            if (foundIt != options.end()) {
                stopEntry.behavior = AtNodeNavigator::Behavior((*foundIt).value);
            }
            else {
                LERROR(fmt::format(
                    "Stop behaviour '{}' is not a valid option. Using default behaviour.",
                    behaviorString
                ));
            }
        }
    }

    _stops.push_back(stopEntry);
}

// Test if the node lies within a given proximity radius of any relevant node in the scene
SceneGraphNode* AutoNavigationHandler::findNodeNearTarget(const SceneGraphNode* node) {
    glm::dvec3 nodePos = node->worldPosition();
    std::string nodeId = node->identifier();

    const float proximityRadiusFactor = 3.f;

    for (SceneGraphNode* n : _relevantNodes) {
        if (n->identifier() == nodeId)
            continue;

        float proximityRadius = proximityRadiusFactor * n->boundingSphere();
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
Waypoint AutoNavigationHandler::computeDefaultWaypoint(const TargetNodeInstruction* ins) {
    const SceneGraphNode* targetNode = sceneGraphNode(ins->nodeIdentifier);
    if (!targetNode) {
        LERROR(fmt::format("Could not find target node '{}'", ins->nodeIdentifier));
        return Waypoint();
    }

    const float epsilon = 1e-5f;
    const glm::dvec3 nodePos = targetNode->worldPosition();
    const glm::dvec3 sunPos = glm::dvec3(0.0, 0.0, 0.0);
    const SceneGraphNode* closeNode = findNodeNearTarget(targetNode);
    const glm::dvec3 prevPos = lastWayPoint().position();

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

    const bool hasHeight = ins->height.has_value();
    const double height = hasHeight ? ins->height.value() : defaultHeight;

    const glm::dvec3 targetPos = nodePos + stepDirection * (radius + height);

    // Up direction
    glm::dvec3 up = camera()->lookUpVectorWorldSpace();
    if (ins->setUpDirectionFromTarget()) {
        // @TODO (emmbr 2020-11-17) For now, this is hardcoded to look good for Earth,
        // which is where it matters the most. A better solution would be to make each
        // sgn aware of its own 'up' and query
        up = targetNode->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0);
    }

    // Rotation
    const glm::dvec3 lookAtPos = targetNode->worldPosition();
    const glm::dquat targetRot = helpers::lookAtQuaternion(targetPos, lookAtPos, up);

    return Waypoint{ targetPos, targetRot, ins->nodeIdentifier };
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
