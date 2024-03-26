/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/events/eventengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/collisionhelper.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>
#include <vector>
#include "pathnavigator_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "PathNavigator";

    constexpr openspace::properties::Property::PropertyInfo DefaultCurveOptionInfo = {
        "DefaultPathType",
        "Default Path Type",
        "The default path type chosen when generating a path or flying to a target. "
        "See wiki for alternatives. The shape of the generated path will be different "
        "depending on the path type",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IncludeRollInfo = {
        "IncludeRoll",
        "Include Roll",
        "If disabled, roll is removed from the interpolation of camera orientation",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedScaleInfo = {
        "SpeedScale",
        "Speed Scale",
        "Scale factor that the speed will be multiplied with during path traversal. "
        "Can be used to speed up or slow down the camera motion, depending on if the "
        "value is larger than or smaller than one",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorOnFinishInfo = {
        "ApplyIdleBehaviorOnFinish",
        "Apply Idle Behavior on Finish",
        "If set to true, the chosen IdleBehavior of the OrbitalNavigator will be "
        "triggered once the path has reached its target",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ArrivalDistanceFactorInfo = {
        "ArrivalDistanceFactor",
        "Arrival Distance Factor",
        "A factor used to compute the default distance from a target scene graph node "
        "when creating a camera path. The factor will be multipled with the node's "
        "bounding sphere to compute the target height from the bounding sphere of the "
        "object",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RotationSpeedFactorInfo = {
        "RotationSpeedFactor",
        "Rotation Speed Factor (Linear Path)",
        "Affects how fast the camera rotates to the target rotation during a linear "
        "path. A value of 1 means that the camera will rotate 90 degrees in about 5 "
        "seconds. A value of 2 means twice that time, i.e. 10 seconds, and so on",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MinBoundingSphereInfo = {
        "MinimalValidBoundingSphere",
        "Minimal Valid Bounding Sphere",
        "The minimal allowed value for a bounding sphere, in meters. Used for "
        "computation of target positions and path generation, to avoid issues when "
        "there is no bounding sphere",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RelevantNodeTagsInfo = {
        "RelevantNodeTags",
        "Relevant Node Tags",
        "List of tags for the nodes that are relevant for path creation, for example "
        "when avoiding collisions",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::interaction {

PathNavigator::PathNavigator()
    : properties::PropertyOwner({ "PathNavigator", "Path Navigator" })
    , _defaultPathType(
        DefaultCurveOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _includeRoll(IncludeRollInfo, false)
    , _speedScale(SpeedScaleInfo, 1.f, 0.01f, 2.f)
    , _applyIdleBehaviorOnFinish(IdleBehaviorOnFinishInfo, false)
    , _arrivalDistanceFactor(ArrivalDistanceFactorInfo, 2.0, 0.1, 20.0)
    , _linearRotationSpeedFactor(RotationSpeedFactorInfo, 2.f, 0.1f, 3.f)
    , _minValidBoundingSphere(MinBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _relevantNodeTags(RelevantNodeTagsInfo)
{
    _defaultPathType.addOptions({
        { static_cast<int>(Path::Type::AvoidCollision), "AvoidCollision" },
        { static_cast<int>(Path::Type::ZoomOutOverview), "ZoomOutOverview" },
        { static_cast<int>(Path::Type::Linear), "Linear" },
        {
            static_cast<int>(Path::Type::AvoidCollisionWithLookAt),
            "AvoidCollisionWithLookAt"
        }
    });
    addProperty(_defaultPathType);

    addProperty(_includeRoll);
    addProperty(_speedScale);
    addProperty(_applyIdleBehaviorOnFinish);
    addProperty(_arrivalDistanceFactor);
    addProperty(_linearRotationSpeedFactor);
    addProperty(_minValidBoundingSphere);

    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };
    _relevantNodeTags.onChange([this]() { findRelevantNodes(); });
    addProperty(_relevantNodeTags);
}

PathNavigator::~PathNavigator() {}

Camera* PathNavigator::camera() const {
    return global::navigationHandler->camera();
}

const SceneGraphNode* PathNavigator::anchor() const {
    return global::navigationHandler->anchorNode();
}

const Path* PathNavigator::currentPath() const {
    return _currentPath.get();
}

double PathNavigator::speedScale() const {
    return _speedScale;
}

double PathNavigator::arrivalDistanceFactor() const {
    return _arrivalDistanceFactor;
}

float PathNavigator::linearRotationSpeedFactor() const {
    return _linearRotationSpeedFactor;
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
    return hasCurrentPath() && _isPlaying;
}

bool PathNavigator::isPaused() const {
    return hasCurrentPath() && !_isPlaying;
}

float PathNavigator::estimatedRemainingTimeInPath() const {
    return hasCurrentPath() ? _currentPath->estimatedRemainingTime(_speedScale) : 0.f;
}

void PathNavigator::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!hasCurrentPath() || !_isPlaying) {
        return;
    }

    if (!_currentPath->startPoint().node() || !_currentPath->endPoint().node()) {
        LERROR(
            "One of the scene graph nodes used in an active camera path "
            "was removed. Aborting path"
        );
        abortPath();
        global::navigationHandler->orbitalNavigator().setFocusNode("Root", false);
        return;
    }

    if (_setCameraToEndNextFrame) {
        LDEBUG("Skipped to end of camera path");
        _currentPath->quitPath();

        const interaction::Waypoint endPoint = _currentPath->endPoint();
        camera()->setPose(endPoint.pose());
        global::navigationHandler->orbitalNavigator().setFocusNode(
            endPoint.nodeIdentifier(),
            false
        );
        if (endPoint.aimIdentifier().has_value()) {
            global::navigationHandler->orbitalNavigator().setAimNode(
                *endPoint.aimIdentifier()
            );
        }

        handlePathEnd();
        _setCameraToEndNextFrame = false;
        return;
    }

    // Prevent long delta times due to e.g. computations from other actions to cause
    // really big jumps in the motion along the path
    // OBS! Causes problems if the general FPS is lower than 10, but then the user should
    // probably not use the camera paths anyways
    if (deltaTime > 0.1) {
        deltaTime = 0.01;
    }

    CameraPose newPose = _currentPath->traversePath(deltaTime, _speedScale);
    const std::string newAnchor = _currentPath->currentAnchor();

    // Set anchor node in orbitalNavigator, to render visible nodes and add activate
    // navigation when we reach the end.
    const std::string currentAnchor = anchor()->identifier();
    if (currentAnchor != newAnchor) {
        global::navigationHandler->orbitalNavigator().setFocusNode(newAnchor, false);
    }

    if (!_includeRoll) {
        removeRollRotation(newPose);
    }

    camera()->setPose(newPose);

    if (_currentPath->hasReachedEnd()) {
        LINFO("Reached target");

        // Also set the aim once the path is finished, if one should be set
        if (_currentPath->endPoint().aimIdentifier().has_value()) {
            global::navigationHandler->orbitalNavigator().setAimNode(
                *_currentPath->endPoint().aimIdentifier()
            );
        }

        handlePathEnd();
        return;
    }
}

void PathNavigator::createPath(const ghoul::Dictionary& dictionary) {
    const OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
    if (m == OpenSpaceEngine::Mode::SessionRecordingPlayback) {
        // Silently ignore any paths that are being created during a session recording
        // playback. The camera path should already have been recorded
        return;
    }

    clearPath();

    try {
        _currentPath = std::make_unique<Path>(createPathFromDictionary(dictionary));
    }
    catch (const documentation::SpecificationError& e) {
        LERROR("Could not create camera path");
        logError(e);
    }
    catch (const PathCurve::TooShortPathError&) {
        // Do nothing
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(std::format("Could not create path. Reason: {}", e.message));
        return;
    }

    LDEBUG("Successfully generated camera path");
}

void PathNavigator::clearPath() {
    if (_currentPath) {
        LDEBUG("Clearing path");
    }
    _currentPath = nullptr;
}

void PathNavigator::startPath() {
    if (!hasCurrentPath()) {
        LERROR("There is no path to start");
        return;
    }

    const bool success = global::openSpaceEngine->setMode(
        OpenSpaceEngine::Mode::CameraPath
    );
    if (!success) {
        LERROR("Could not start camera path");
        return; // couldn't switch to camera path mode
    }

    // Always pause the simulation time when flying, to aovid problem with objects
    // moving. However, keep track of whether the time was running before the path
    // was started, so we can reset it on finish
    if (!global::timeManager->isPaused()) {
        openspace::global::scriptEngine->queueScript(
            "openspace.time.setPause(true)",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );

        _startSimulationTimeOnFinish = true;
        LINFO("Pausing time simulation during path traversal");
    }

    LINFO("Starting path");
    _isPlaying = true;

    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();
    global::navigationHandler->orbitalNavigator().resetVelocities();

    global::eventEngine->publishEvent<events::EventCameraPathStarted>(
        _currentPath->startPoint().node(),
        _currentPath->endPoint().node()
    );
}

void PathNavigator::abortPath() {
    if (hasFinished()) {
        LWARNING("No camera path is playing");
        return;
    }
    handlePathEnd();
    LINFO("Aborted camera path");
}

void PathNavigator::pausePath() {
    if (hasFinished()) {
        LERROR("No path to pause (path is empty or has finished)");
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
        LERROR("No path to resume (path is empty or has finished)");
        return;
    }

    if (_isPlaying) {
        LERROR("Cannot resume a path that is already playing");
        return;
    }

    LINFO("Continuing path");
    _isPlaying = true;
}

void PathNavigator::skipToEnd() {
    if (!openspace::global::navigationHandler->pathNavigator().isPlayingPath()) {
        LWARNING("No camera path is currently active");
    }

    _setCameraToEndNextFrame = true;
}

Path::Type PathNavigator::defaultPathType() const {
    return static_cast<Path::Type>(_defaultPathType.value());
}

double PathNavigator::minValidBoundingSphere() const {
    return _minValidBoundingSphere;
}

double PathNavigator::findValidBoundingSphere(const SceneGraphNode* node) const {
    ghoul_assert(node != nullptr, "Node must not be nulltpr");
    auto sphere = [](const SceneGraphNode* n) {
        // Use the biggest of the bounding sphere and interaction sphere,
        // so we don't accidentally choose a bounding sphere that is much smaller
        // than the interaction sphere of the node
        const double bs = n->boundingSphere();
        const double is = n->interactionSphere();
        return std::max(is, bs);
    };

    double result = sphere(node);

    if (result < _minValidBoundingSphere) {
        LDEBUG(std::format(
            "The scene graph node '{}' has no, or a very small, bounding sphere. Using "
            "minimal value of {}. This might lead to unexpected results",
            node->identifier(), _minValidBoundingSphere.value()
        ));
        result = _minValidBoundingSphere;
    }

    return result;
}

const std::vector<SceneGraphNode*>& PathNavigator::relevantNodes() {
    if (!_hasInitializedRelevantNodes) {
        findRelevantNodes();
        _hasInitializedRelevantNodes = true;
    }

    return _relevantNodes;
}

void PathNavigator::handlePathEnd() {
    _isPlaying = false;
    global::openSpaceEngine->resetMode();

    if (_startSimulationTimeOnFinish) {
        openspace::global::scriptEngine->queueScript(
            "openspace.time.setPause(false)",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
        _startSimulationTimeOnFinish = false;
    }

    if (_applyIdleBehaviorOnFinish) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle("
                "'NavigationHandler.OrbitalNavigator.IdleBehavior.ApplyIdleBehavior',"
                "true"
            ");",
            openspace::scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            openspace::scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

    global::eventEngine->publishEvent<events::EventCameraPathFinished>(
        _currentPath->startPoint().node(),
        _currentPath->endPoint().node()
    );
}

void PathNavigator::findRelevantNodes() {
    const std::vector<SceneGraphNode*>& allNodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    const std::vector<std::string> relevantTags = _relevantNodeTags;

    if (allNodes.empty() || relevantTags.empty()) {
        _relevantNodes = std::vector<SceneGraphNode*>();
        return;
    }

    auto isRelevant = [&relevantTags](const SceneGraphNode* node) {
        const std::vector<std::string>& tags = node->tags();
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

SceneGraphNode* PathNavigator::findNodeNearTarget(const SceneGraphNode* node) {
    constexpr float LengthEpsilon = 1e-5f;
    const std::vector<SceneGraphNode*>& relNodes =
        global::navigationHandler->pathNavigator().relevantNodes();

    for (SceneGraphNode* n : relNodes) {
        bool isSame = (n->identifier() == node->identifier());
        // If the nodes are in the very same position, they are probably representing
        // the same object
        isSame |=
            glm::distance(n->worldPosition(), node->worldPosition()) < LengthEpsilon;

        if (isSame) {
            continue;
        }

        constexpr float proximityRadiusFactor = 3.f;

        const float bs = static_cast<float>(n->boundingSphere());
        const float proximityRadius = proximityRadiusFactor * bs;
        const glm::dvec3 posInModelCoords =
            glm::inverse(n->modelTransform()) * glm::dvec4(node->worldPosition(), 1.0);

        const bool isClose = collision::isPointInsideSphere(
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

void PathNavigator::removeRollRotation(CameraPose& pose) const {
    // The actual position for the camera does not really matter. Use the origin,
    // to avoid precision problems when we have large values for the position
    const glm::dvec3 cameraPos = glm::dvec3(0.0);
    const glm::dvec3 cameraDir = glm::normalize(
        pose.rotation * Camera::ViewDirectionCameraSpace
    );

    // The actual distance does not really matter either. Just has to be far
    // enough away from the camera
    constexpr double NotTooCloseDistance = 10000.0;

    const glm::dvec3 lookAtPos = cameraPos + NotTooCloseDistance * cameraDir;

    const glm::dquat rollFreeRotation = ghoul::lookAtQuaternion(
        cameraPos,
        lookAtPos,
        camera()->lookUpVectorWorldSpace()
    );

    pose.rotation = rollFreeRotation;
}

scripting::LuaLibrary PathNavigator::luaLibrary() {
    return {
        "pathnavigation",
        {
            codegen::lua::IsFlying,
            codegen::lua::ContinuePath,
            codegen::lua::PausePath,
            codegen::lua::StopPath,
            codegen::lua::SkipToEnd,
            codegen::lua::FlyTo,
            codegen::lua::FlyToHeight,
            codegen::lua::FlyToNavigationState,
            codegen::lua::ZoomToFocus,
            codegen::lua::ZoomToDistance,
            codegen::lua::ZoomToDistanceRelative,
            codegen::lua::CreatePath
        }
    };
}

} // namespace openspace::interaction
