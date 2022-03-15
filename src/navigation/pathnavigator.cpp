/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "PathNavigator";

    constexpr openspace::properties::Property::PropertyInfo DefaultCurveOptionInfo = {
        "DefaultPathType",
        "Default Path Type",
        "The default path type chosen when generating a path or flying to a target. "
        "See wiki for alternatives. The shape of the generated path will be different "
        "depending on the path type."
    };

    constexpr openspace::properties::Property::PropertyInfo IncludeRollInfo = {
        "IncludeRoll",
        "Include Roll",
        "If disabled, roll is removed from the interpolation of camera orientation"
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedScaleInfo = {
        "SpeedScale",
        "Speed Scale",
        "Scale factor that the speed will be mulitplied with during path traversal. "
        "Can be used to speed up or slow down the camera motion, depending on if the "
        "value is larger than or smaller than one."
    };

    constexpr openspace::properties::Property::PropertyInfo IdleBehaviorOnFinishInfo = {
        "ApplyIdleBehaviorOnFinish",
        "Apply Idle Behavior on Finish",
        "If set to true, the chosen IdleBehavior of the OrbitalNavigator will be "
        "triggered once the path has reached its target."
    };

    constexpr openspace::properties::Property::PropertyInfo ArrivalDistanceFactorInfo = {
        "ArrivalDistanceFactor",
        "Arrival Distance Factor",
        "A factor used to compute the default distance from a target scene graph node "
        "when creating a camera path. The factor will be multipled with the node's "
        "bounding sphere to compute the target height from the bounding sphere of the "
        "object."
    };

    constexpr const openspace::properties::Property::PropertyInfo MinBoundingSphereInfo =
    {
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
    , _defaultPathType(
        DefaultCurveOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _includeRoll(IncludeRollInfo, false)
    , _speedScale(SpeedScaleInfo, 1.f, 0.01f, 2.f)
    , _applyIdleBehaviorOnFinish(IdleBehaviorOnFinishInfo, false)
    , _arrivalDistanceFactor(ArrivalDistanceFactorInfo, 2.0, 0.1, 20.0)
    , _minValidBoundingSphere(MinBoundingSphereInfo, 10.0, 1.0, 3e10)
    , _relevantNodeTags(RelevantNodeTagsInfo)
{
    _defaultPathType.addOptions({
        { Path::Type::AvoidCollision, "AvoidCollision" },
        { Path::Type::ZoomOutOverview, "ZoomOutOverview"},
        { Path::Type::Linear, "Linear" },
        { Path::Type::AvoidCollisionWithLookAt, "AvoidCollisionWithLookAt"}
    });
    addProperty(_defaultPathType);

    addProperty(_includeRoll);
    addProperty(_speedScale);
    addProperty(_applyIdleBehaviorOnFinish);
    addProperty(_arrivalDistanceFactor);
    addProperty(_minValidBoundingSphere);

    _relevantNodeTags = std::vector<std::string>{
        "planet_solarSystem",
        "moon_solarSystem"
    };
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

const Path* PathNavigator::currentPath() const {
    return _currentPath.get();
}

double PathNavigator::speedScale() const {
    return _speedScale;
}

double PathNavigator::arrivalDistanceFactor() const {
    return _arrivalDistanceFactor;
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

void PathNavigator::updateCamera(double deltaTime) {
    ghoul_assert(camera() != nullptr, "Camera must not be nullptr");

    if (!hasCurrentPath() || !_isPlaying) {
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
    std::string currentAnchor = anchor()->identifier();
    if (currentAnchor != newAnchor) {
        global::navigationHandler->orbitalNavigator().setFocusNode(newAnchor, false);
    }

    if (!_includeRoll) {
        removeRollRotation(newPose, deltaTime);
    }

    camera()->setPose(newPose);

    if (_currentPath->hasReachedEnd()) {
        LINFO("Reached target");
        handlePathEnd();

        if (_applyIdleBehaviorOnFinish) {
            constexpr const char* ApplyIdleBehaviorScript =
                "openspace.setPropertyValueSingle("
                    "'NavigationHandler.OrbitalNavigator.IdleBehavior.ApplyIdleBehavior',"
                    "true"
                ");";

            global::scriptEngine->queueScript(
                ApplyIdleBehaviorScript,
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
        return;
    }
}

void PathNavigator::createPath(const ghoul::Dictionary& dictionary) {
    OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
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
        for (const documentation::TestResult::Offense& o : e.result.offenses) {
            LERRORC(o.offender, ghoul::to_string(o.reason));
        }
        for (const documentation::TestResult::Warning& w : e.result.warnings) {
            LWARNINGC(w.offender, ghoul::to_string(w.reason));
        }
    }
    catch (const PathCurve::TooShortPathError&) {
        // Do nothing
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(fmt::format("Could not create path. Reason: ", e.message));
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

    if (!global::openSpaceEngine->setMode(OpenSpaceEngine::Mode::CameraPath)) {
        LERROR("Could not start camera path");
        return; // couldn't switch to camera path mode
    }

    // Always pause the simulation time when flying, to aovid problem with objects
    // moving. However, keep track of whether the time was running before the path
    // was started, so we can reset it on finish
    if (!global::timeManager->isPaused()) {
        openspace::global::scriptEngine->queueScript(
            "openspace.time.setPause(true)",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        _startSimulationTimeOnFinish = true;
        LINFO("Pausing time simulation during path traversal");
    }

    LINFO("Starting path");
    _isPlaying = true;

    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();
    global::navigationHandler->orbitalNavigator().resetVelocities();
}

void PathNavigator::abortPath() {
    if (!_isPlaying) {
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

Path::Type PathNavigator::defaultPathType() const {
    const int pathType = _defaultPathType;
    return Path::Type(pathType);
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
        double bs = n->boundingSphere();
        double is = n->interactionSphere();
        return is > bs ? is : bs;
    };

    double result = sphere(node);

    if (result < _minValidBoundingSphere) {
        // If the bs of the target is too small, try to find a good value in a child node.
        // Only check the closest children, to avoid deep traversal in the scene graph.
        // Alsp. the chance to find a bounding sphere that represents the visual size of
        // the target well is higher for these nodes
        for (SceneGraphNode* child : node->children()) {
            result = sphere(child);
            if (result > _minValidBoundingSphere) {
                LDEBUG(fmt::format(
                    "The scene graph node '{}' has no, or a very small, bounding sphere. "
                    "Using bounding sphere of child node '{}' in computations.",
                    node->identifier(),
                    child->identifier()
                ));
                return result;
            }
        }

        LDEBUG(fmt::format(
            "The scene graph node '{}' has no, or a very small, bounding sphere. Using "
            "minimal value. This might lead to unexpected results.",
            node->identifier())
        );

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

    if (_startSimulationTimeOnFinish) {
        openspace::global::scriptEngine->queueScript(
            "openspace.time.setPause(false)",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    _startSimulationTimeOnFinish = false;
    global::openSpaceEngine->resetMode();
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
    glm::dquat rollFreeRotation = ghoul::lookAtQuaternion(
        pose.position,
        lookAtPos,
        camera()->lookUpVectorWorldSpace()
    );
    pose.rotation = rollFreeRotation;
}

scripting::LuaLibrary PathNavigator::luaLibrary() {
    return {
        "pathnavigation",
        {
            {
                "isFlying",
                &luascriptfunctions::isFlying,
                "",
                "Returns true if a camera path is currently running, and false otherwise"
            },
            {
                "continuePath",
                &luascriptfunctions::continuePath,
                "",
                "Continue playing a paused camera path"
            },
            {
                "pausePath",
                &luascriptfunctions::pausePath,
                "",
                "Pause a playing camera path"
            },
            {
                "stopPath",
                &luascriptfunctions::stopPath,
                "",
                "Stops a path, if one is being played"
            },
            {
                "flyTo",
                &luascriptfunctions::flyTo,
                "string [, bool, double]",
                "Move the camera to the node with the specified identifier. The optional "
                "double specifies the duration of the motion, in seconds. If the optional "
                "bool is set to true the target up vector for camera is set based on the "
                "target node. Either of the optional parameters can be left out."
            },
            {
                "flyToHeight",
                &luascriptfunctions::flyToHeight,
                "string, double [, bool, double]",
                "Move the camera to the node with the specified identifier. The second "
                "argument is the desired target height above the target node's bounding "
                "sphere, in meters. The optional double specifies the duration of the "
                "motion, in seconds. If the optional bool is set to true, the target "
                "up vector for camera is set based on the target node. Either of the "
                "optional parameters can be left out."
            },
            {
                "flyToNavigationState",
                &luascriptfunctions::flyToNavigationState,
                "table, [double]",
                "Create a path to the navigation state described by the input table. "
                "The optional double specifies the target duration of the motion, "
                "in seconds. Note that roll must be included for the target up "
                "direction to be taken into account."
            },
            {
                "zoomToFocus",
                &luascriptfunctions::zoomToFocus,
                "[duration]",
                "Zoom linearly to the current focus node, using the default distance."
                "The optional input parameter specifies the duration for the motion, "
                "in seconds."
            },
            {
                "zoomToDistance",
                &luascriptfunctions::zoomToDistance,
                "distance, [duration]",
                "Fly linearly to a specific distance in relation to the focus node. "
                "The distance is given in meters above the bounding sphere of the "
                "current focus node."
                "The optional input parameter specifies the duration for the motion, "
                "in seconds."
            },
            {
                "zoomToDistanceRelative",
                &luascriptfunctions::zoomToDistanceRelative,
                "distance, [duration]",
                "Fly linearly to a specific distance in relation to the focus node. "
                "The distance is given as a multiple of the bounding sphere of the "
                "current focus node. That is, a value of 1 will result in a position "
                "at a distance of one times the size of the bounding sphere away from "
                "the object."
                "The optional input parameter specifies the duration for the motion, "
                "in seconds."
            },
            {
                "createPath",
                &luascriptfunctions::createPath,
                "table",
                "Create a camera path as described by the lua table input argument"
            },
        }
    };
}

} // namespace openspace::interaction
