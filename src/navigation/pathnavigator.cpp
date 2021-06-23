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

#include <openspace/navigation/pathnavigator.h>

#include <openspace/camera/camera.h>
#include <openspace/camera/camerapose.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/pathcreator.h>
#include <openspace/navigation/pathhelperfunctions.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "PathNavigator";

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

    constexpr const openspace::properties::Property::PropertyInfo MinBoundingSphereInfo = {
        "MinimalValidBoundingSphere",
        "Minimal Valid Bounding Sphere",
        "The minimal allowed value for a bounding sphere, in meters. Used for "
        "computation of target positions and path generation, to avoid issues when "
        "there is no bounding sphere."
    };

    constexpr openspace::properties::Property::PropertyInfo RelevantNodeTagsInfo = {
        "RelevantNodeTags",
        "Relevant Node Tags",
        "List of tags for the nodes that are relevant for path creation, for example "
        "when avoiding collisions."
    };
} // namespace

#include "pathnavigator_lua.inl"

namespace openspace::interaction {

PathNavigator::PathNavigator()
    : properties::PropertyOwner({ "PathNavigator" })
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
    , _minValidBoundingSphere(MinBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _relevantNodeTags(RelevantNodeTagsInfo)
{
    _defaultCurveOption.addOptions({
        { Path::CurveType::AvoidCollision, "AvoidCollision" },
        { Path::CurveType::Linear, "Linear" },
        { Path::CurveType::ZoomOutOverview, "ZoomOutOverview"}
    });
    addProperty(_defaultCurveOption);

    addProperty(_includeRoll);
    addProperty(_speedScale);

    // OBS! Stop behavior is broken as of core merge
    //addProperty(_applyStopBehaviorWhenIdle);

    //// Must be listed in the same order as in enum definition
    //_stopBehavior.addOptions({
    //    { StopBehavior::None, "None" },
    //    { StopBehavior::Orbit, "Orbit" }
    //});
    //_stopBehavior = StopBehavior::None;
    //addProperty(_stopBehavior);

    //addProperty(_orbitSpeedFactor);

    addProperty(_minValidBoundingSphere);

    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };;
    _relevantNodeTags.onChange([this]() { findRelevantNodes(); });
    addProperty(_relevantNodeTags);
}

PathNavigator::~PathNavigator() {} // NOLINT

Camera* PathNavigator::camera() const {
    return global::navigationHandler->camera();
}

const SceneGraphNode* PathNavigator::anchor() const {
    return global::navigationHandler->anchorNode();
}

double PathNavigator::speedScale() const {
    return _speedScale;
}

bool PathNavigator::hasCurrentPath() const {
    return _currentPath != nullptr;
}

bool PathNavigator::hasFinished() const {
    if (!hasCurrentPath()) {
        return true;
    }
    return _currentPath->hasReachedEnd();
}

bool PathNavigator::isPlayingPath() const {
    return !hasFinished();
}

void PathNavigator::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!hasCurrentPath()) {
        return;
    }

    if (!_isPlaying) {
        //// TODO: Determine how this should work
        //// OBS! Stop behavior is broken as of core merge
        //if (hasFinished() && _applyStopBehaviorWhenIdle) {
        //    applyStopBehavior(deltaTime);
        //}
        return;
    }

    // If for some reason the time is no longer paused, pause it again
    // TODO: Before we get here, one time tick happens. Should move this check to engine
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

    if (_currentPath->hasReachedEnd()) {
        LINFO("Reached end of path");
        _isPlaying = false;
        return;
    }
}

void PathNavigator::createPath(const ghoul::Dictionary& dictionary) {
    // TODO: Improve how curve types are handled
    const int curveType = _defaultCurveOption;

    // Ignore paths that are created during session recording, as the camera
    // position should have been recorded
    if (global::sessionRecording->isPlayingBack()) {
        return;
    }

    clearPath();
    try {
        _currentPath = std::make_unique<Path>(
            PathCreator::createPath(dictionary, Path::CurveType(curveType))
        );
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(fmt::format("Could not create path. Reason: ", e.message));
        return;
    }

    LINFO("Successfully generated camera path");
}

void PathNavigator::clearPath() {
    LINFO("Clearing path...");
    _currentPath = nullptr;
}

void PathNavigator::startPath() {
    if (!hasCurrentPath()) {
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

void PathNavigator::abortPath() {
    if (!_isPlaying) {
        LWARNING("No camera path is playing");
        return;
    }
    _isPlaying = false;
    clearPath(); // TODO: instead of clearing this could be handled better

    LINFO("Aborted camera path");
}

void PathNavigator::pausePath() {
    if (hasFinished()) {
        LERROR("No path to pause (path is empty or has finished).");
        return;
    }

    if (!_isPlaying) {
        LERROR("Cannot pause a path that is not playing");
        return;
    }

    LINFO("Path paused");
    _isPlaying = false;
}

void PathNavigator::continuePath() {
    if (hasFinished()) {
        LERROR("No path to resume (path is empty or has finished).");
        return;
    }

    if (_isPlaying) {
        LERROR("Cannot resume a path that is already playing");
        return;
    }

    LINFO("Continuing path...");
    _isPlaying = true;
}

// Created for debugging
std::vector<glm::dvec3> PathNavigator::curvePositions(int nSteps) const {
    if (!hasCurrentPath()) {
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

// Created for debugging
std::vector<glm::dquat> PathNavigator::curveOrientations(int nSteps) const {
    if (!hasCurrentPath()) {
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


// Created for debugging
std::vector<glm::dvec3> PathNavigator::curveViewDirections(int nSteps) const {
    if (!hasCurrentPath()) {
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

// Created for debugging
std::vector<glm::dvec3> PathNavigator::controlPoints() const {
    if (!hasCurrentPath()) {
        LERROR("There is no current path to sample points from.");
        return {};
    }

    std::vector<glm::dvec3> points;
    const std::vector<glm::dvec3> curvePoints = _currentPath->controlPoints();
    points.insert(points.end(), curvePoints.begin(), curvePoints.end());

    return points;
}

double PathNavigator::minValidBoundingSphere() const {
    return _minValidBoundingSphere;
}

const std::vector<SceneGraphNode*>& PathNavigator::relevantNodes() {
    if (!_hasInitializedRelevantNodes) {
        findRelevantNodes();
        _hasInitializedRelevantNodes = true;
    }

    return _relevantNodes;
}

void PathNavigator::findRelevantNodes() {
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    const std::vector<std::string> relevantTags = _relevantNodeTags;

    if (allNodes.empty() || relevantTags.empty()) {
        _relevantNodes = std::vector<SceneGraphNode*>();
        return;
    }

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

    std::vector<SceneGraphNode*> resultingNodes;
    std::copy_if(
        allNodes.begin(),
        allNodes.end(),
        std::back_inserter(resultingNodes),
        isRelevant
    );

    _relevantNodes = resultingNodes;
}

void PathNavigator::removeRollRotation(CameraPose& pose, double deltaTime) {
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

void PathNavigator::applyStopBehavior(double deltaTime) {
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

void PathNavigator::orbitAnchorNode(double deltaTime) {
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

scripting::LuaLibrary PathNavigator::luaLibrary() {
    return {
        "pathnavigation",
        {
            {
                "isFlying",
                &luascriptfunctions::isFlying,
                {},
                "",
                "Returns true if a camera path is currently running, and false otherwise"
            },
            {
                "continuePath",
                &luascriptfunctions::continuePath,
                {},
                "",
                "Continue playing a paused camera path"
            },
            {
                "pausePath",
                &luascriptfunctions::pausePath,
                {},
                "",
                "Pause a playing camera path"
            },
            {
                "stopPath",
                &luascriptfunctions::stopPath,
                {},
                "",
                "Stops a path, if one is being played"
            },
            {
                "goTo",
                &luascriptfunctions::goTo,
                {},
                "string [, bool, double]",
                "Move the camera to the node with the specified name. The optional double "
                "specifies the duration of the motion. If the optional bool is set to true "
                "the target up vector for camera is set based on the target node. Either of "
                "the optional parameters can be left out."
            },
            {
                "goToHeight",
                &luascriptfunctions::goToHeight,
                {},
                "string, double [, bool, double]",
                "Move the camera to the node with the specified name. The second input "
                "parameter is the desired target height. The optional double "
                "specifies the duration of the motion. If the optional bool is set to true "
                "the target up vector for camera is set based on the target node. Either of "
                "the optional parameters can be left out."
            },
            {
                "generatePath",
                &luascriptfunctions::generatePath,
                {},
                "table",
                "Generate the path as described by the lua table input argument"
            },
        }
    };
}

} // namespace openspace::interaction
