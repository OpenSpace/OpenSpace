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

#include <openspace/navigation/navigationhandler.h>

#include <openspace/camera/camera.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/scriptcamerastates.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/waypoint.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/query/query.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/profiling.h>
#include <glm/gtx/vector_angle.hpp>
#include <filesystem>
#include <fstream>
#include <numeric>

#include "navigationhandler_lua.inl"

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    constexpr std::string_view _loggerCat = "NavigationHandler";

    constexpr openspace::properties::Property::PropertyInfo DisableKeybindingsInfo = {
        "DisableKeybindings",
        "Disable all Keybindings",
        "Disables all keybindings without removing them. Please note that this does not "
        "apply to the key to open the console",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableMouseInputInfo = {
        "DisableMouseInputs",
        "Disable all mouse inputs",
        "Disables all mouse inputs and prevents them from affecting the camera",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableJoystickInputInfo = {
        "DisableJoystickInputs",
        "Disable all joystick inputs",
        "Disables all joystick inputs and prevents them from affecting the camera",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FrameInfo = {
        "UseKeyFrameInteraction",
        "Use keyframe interaction",
        "If this is set to 'true' the entire interaction is based off key frames rather "
        "than using the mouse interaction",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToFadeDurationInfo = {
        "JumpToFadeDuration",
        "JumpTo Fade Duration",
        "The number of seconds the fading of the rendering should take per default when "
        "navigating through a 'jump' transition. This is when the rendering is first "
        "faded to black, then the camera is moved, and then the rendering fades in again",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace::interaction {

NavigationHandler::NavigationHandler()
    : properties::PropertyOwner({ "NavigationHandler", "Navigation Handler" })
    , _disableKeybindings(DisableKeybindingsInfo, false)
    , _disableMouseInputs(DisableMouseInputInfo, false)
    , _disableJoystickInputs(DisableJoystickInputInfo, false)
    , _useKeyFrameInteraction(FrameInfo, false)
    , _jumpToFadeDuration(JumpToFadeDurationInfo, 1.f, 0.f, 10.f)
{
    addPropertySubOwner(_orbitalNavigator);
    addPropertySubOwner(_pathNavigator);

    addProperty(_disableKeybindings);
    addProperty(_disableMouseInputs);
    addProperty(_disableJoystickInputs);
    addProperty(_useKeyFrameInteraction);
    addProperty(_jumpToFadeDuration);
}

NavigationHandler::~NavigationHandler() {}

void NavigationHandler::initialize() {
    ZoneScoped;

    global::parallelPeer->connectionEvent().subscribe(
        "NavigationHandler",
        "statusChanged",
        [this]() {
            _useKeyFrameInteraction = (global::parallelPeer->status() ==
                ParallelConnection::Status::ClientWithHost);
        }
    );
}

void NavigationHandler::deinitialize() {
    ZoneScoped;

    global::parallelPeer->connectionEvent().unsubscribe("NavigationHandler");
}

void NavigationHandler::setFocusNode(SceneGraphNode* node) {
    ghoul_assert(node, "Focus node must not be nullptr");
    _orbitalNavigator.setFocusNode(node);
    _camera->setPositionVec3(anchorNode()->worldPosition());
}

void NavigationHandler::setCamera(Camera* camera) {
    _camera = camera;
    _orbitalNavigator.setCamera(camera);
}

void NavigationHandler::setNavigationStateNextFrame(const NavigationState& state) {
    _pendingState = state;
}

void NavigationHandler::setCameraFromNodeSpecNextFrame(NodeCameraStateSpec spec) {
    _pendingState = std::move(spec);
}

OrbitalNavigator& NavigationHandler::orbitalNavigator() {
    return _orbitalNavigator;
}

const OrbitalNavigator& NavigationHandler::orbitalNavigator() const {
    return _orbitalNavigator;
}

KeyframeNavigator& NavigationHandler::keyframeNavigator() {
    return _keyframeNavigator;
}

PathNavigator& NavigationHandler::pathNavigator() {
    return _pathNavigator;
}

bool NavigationHandler::isKeyFrameInteractionEnabled() const {
    return _useKeyFrameInteraction;
}

float NavigationHandler::jumpToFadeDuration() const {
    return _jumpToFadeDuration;
}

float NavigationHandler::interpolationTime() const {
    return _orbitalNavigator.retargetInterpolationTime();
}

void NavigationHandler::setInterpolationTime(float durationInSeconds) {
    _orbitalNavigator.setRetargetInterpolationTime(durationInSeconds);
}

void NavigationHandler::triggerFadeToTransition(const std::string& transitionScript,
                                                std::optional<float> fadeDuration)
{
    const float duration = fadeDuration.value_or(_jumpToFadeDuration);

    const std::string onArrivalScript = std::format(
        "{} "
        "openspace.setPropertyValueSingle("
        "'RenderEngine.BlackoutFactor', 1, {}, 'QuadraticEaseIn'"
        ")", transitionScript, duration
    );
    const std::string script = std::format(
        "openspace.setPropertyValueSingle("
        "'RenderEngine.BlackoutFactor', 0, {}, 'QuadraticEaseOut', [[{}]]"
        ")", duration, onArrivalScript
    );
    // No syncing, as this was called from a script that should have been synced already
    global::scriptEngine->queueScript(
        std::move(script),
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );
}

void NavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(_camera != nullptr, "Camera must not be nullptr");

    // If there is a state to set, do so immediately and then return
    if (_pendingState.has_value()) {
        applyPendingState();
        updateCameraTransitions();
        return;
    }

    const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    const bool playbackMode = (mode == OpenSpaceEngine::Mode::SessionRecordingPlayback);

    // If we're in session recording payback mode, the session recording is responsible
    // for navigation. So don't do anything more here
    if (playbackMode || !_camera) {
        return;
    }

    // Handle navigation, based on what navigator is active
    if (_useKeyFrameInteraction) {
        _keyframeNavigator.updateCamera(*_camera, playbackMode);
    }
    else if (mode == OpenSpaceEngine::Mode::CameraPath) {
        _pathNavigator.updateCamera(deltaTime);
        updateCameraTransitions();
    }
    else { // orbital navigator
        if (_disableJoystickInputs) {
            clearGlobalJoystickStates();
        }
        _orbitalNavigator.updateStatesFromInput(
            _mouseInputState,
            _keyboardInputState,
            deltaTime
        );
        _orbitalNavigator.updateCameraStateFromStates(deltaTime);
        updateCameraTransitions();
    }

    _orbitalNavigator.updateCameraScalingFromAnchor(deltaTime);
}

void NavigationHandler::applyPendingState() {
    ghoul_assert(_pendingState.has_value(), "Pending pose must have a value");

    std::variant<NodeCameraStateSpec, NavigationState> pending = *_pendingState;
    if (std::holds_alternative<NavigationState>(pending)) {
        const NavigationState ns = std::get<NavigationState>(pending);
        _orbitalNavigator.setAnchorNode(ns.anchor);
        _orbitalNavigator.setAimNode(ns.aim);
        _camera->setPose(ns.cameraPose());
    }
    else if (std::holds_alternative<NodeCameraStateSpec>(pending)) {
        const NodeCameraStateSpec spec = std::get<NodeCameraStateSpec>(pending);
        const Waypoint wp = computeWaypointFromNodeInfo(spec);

        _orbitalNavigator.setAnchorNode(wp.nodeIdentifier());
        _orbitalNavigator.setAimNode("");
        _camera->setPose(wp.pose());
    }

    resetNavigationUpdateVariables();
    _pendingState.reset();
}

void NavigationHandler::updateCameraTransitions() {
    // This function is concerned with managing transitions of the camera between
    // different distances of interest relative to the focus node. For each transition two
    // scenarios are handled;  SceneGraphNodes can have attached actions for each
    // transition, which are automatically triggered. Additionally, an
    // EventCameraTransition event is fired that contains information about the focus node
    // and the transition state that caused the vent to fire.

    // Diagram of events for a camera moving from right-to-left.
    // Interaction sphere is 'O' in middle, and ')' are spherical boundaries. The approach
    // factor, reach factor, and interaction sphere radius are all taken from the current
    // focus node.
    //
    // |<------------------->|  Approach factor * Interaction sphere
    //              |<------>|  Reach Factor * Interaction sphere
    //
    // (            (        O        )            )
    // ^            ^                 ^            ^
    // OnExit       OnMoveAway        OnReach      OnApproach
    const glm::dvec3 anchorPos = anchorNode()->worldPosition();
    const glm::dvec3 cameraPos = _camera->positionVec3();
    const double currDistance = glm::distance(anchorPos, cameraPos);
    const double d = anchorNode()->interactionSphere();
    const double af = anchorNode()->approachFactor();
    const double rf = anchorNode()->reachFactor();

    // Updated checks compared to last time, so we can check if we are still in the
    // approach or anchor sphere
    const bool isInApproachSphere = currDistance < d * af;
    const bool isInReachSphere = currDistance < d * rf;

    // Compare these to the values from last frame, to trigger the correct transition
    // events
    const bool wasInApproachSphere = _inAnchorApproachSphere;
    const bool wasInReachSphere = _inAnchorReachSphere;
    _inAnchorApproachSphere = isInApproachSphere;
    _inAnchorReachSphere = isInReachSphere;

    auto triggerApproachEvent = [this](const SceneGraphNode* node) {
        using namespace std::string_literals;
        if (!node->onApproachAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", node->identifier());
            dict.setValue("Transition", "Approaching"s);
            for (const std::string& action : node->onApproachAction()) {
                // No sync because events are always synced and sent to the connected
                // nodes and peers
                global::actionManager->triggerAction(
                    action,
                    dict,
                    interaction::ActionManager::ShouldBeSynchronized::No
                );
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            node,
            events::EventCameraFocusTransition::Transition::Approaching
        );
    };

    auto triggerReachEvent = [this](const SceneGraphNode* node) {
        using namespace std::string_literals;
        if (!node->onReachAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", node->identifier());
            dict.setValue("Transition", "Reaching"s);
            for (const std::string& action : node->onReachAction()) {
                // No sync because events are always synced and sent to the connected
                // nodes and peers
                global::actionManager->triggerAction(
                    action,
                    dict,
                    interaction::ActionManager::ShouldBeSynchronized::No
                );
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            node,
            events::EventCameraFocusTransition::Transition::Reaching
        );
     };

    auto triggerRecedeEvent = [this](const SceneGraphNode* node) {
        using namespace std::string_literals;
        if (!node->onRecedeAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", node->identifier());
            dict.setValue("Transition", "Receding"s);
            for (const std::string& action : node->onRecedeAction()) {
                // No sync because events are always synced and sent to the connected
                // nodes and peers
                global::actionManager->triggerAction(
                    action,
                    dict,
                    interaction::ActionManager::ShouldBeSynchronized::No
                );
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            node,
            events::EventCameraFocusTransition::Transition::Receding
        );
    };

    auto triggerExitEvent = [this](const SceneGraphNode* node) {
        using namespace std::string_literals;
        if (!node->onExitAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", node->identifier());
            dict.setValue("Transition", "Exiting"s);
            for (const std::string& action : node->onExitAction()) {
                // No sync because events are always synced and sent to the connected
                // nodes and peers
                global::actionManager->triggerAction(
                    action,
                    dict,
                    interaction::ActionManager::ShouldBeSynchronized::No
                );
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            node,
            events::EventCameraFocusTransition::Transition::Exiting
        );
    };

    const bool anchorWasChanged = anchorNode() != _lastAnchor;
    if (anchorWasChanged) {
        // The anchor was changed between frames, so the transitions we have to check
        // are a bit different. Just directly trigger the relevant events for the
        // respective node
        if (wasInReachSphere) {
            triggerRecedeEvent(_lastAnchor);
        }

        if (wasInApproachSphere) {
            triggerExitEvent(_lastAnchor);
        }

        if (_inAnchorApproachSphere) {
            triggerApproachEvent(anchorNode());
        }

        if (_inAnchorReachSphere) {
            triggerReachEvent(anchorNode());
        }
    }
    else {
        if (_inAnchorApproachSphere && !wasInApproachSphere) {
            // Transitioned to the approach sphere from somewhere further away => approach
            triggerApproachEvent(anchorNode());
        }

        if (_inAnchorReachSphere && !wasInReachSphere) {
            // Transitioned to the reach sphere from somewhere further away => reach
            triggerReachEvent(anchorNode());
        }

        if (!_inAnchorReachSphere && wasInReachSphere) {
            // Transitioned out of the reach sphere => recede / move away
            triggerRecedeEvent(anchorNode());
        }

        if (!_inAnchorApproachSphere && wasInApproachSphere) {
            // We transitioned out of the approach sphere => on exit
            triggerExitEvent(anchorNode());
        }
    }

    _lastAnchor = anchorNode();
}

void NavigationHandler::resetNavigationUpdateVariables() {
    _orbitalNavigator.resetVelocities();
    _orbitalNavigator.updatePreviousStateVariables();
}

const SceneGraphNode* NavigationHandler::anchorNode() const {
    return _orbitalNavigator.anchorNode();
}

Camera* NavigationHandler::camera() const {
    return _camera;
}

const MouseInputState& NavigationHandler::mouseInputState() const {
    return _mouseInputState;
}

const KeyboardInputState& NavigationHandler::keyboardInputState() const {
    return _keyboardInputState;
}

void NavigationHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (!_disableMouseInputs) {
        _mouseInputState.mouseButtonCallback(button, action);
    }
}

void NavigationHandler::mousePositionCallback(double x, double y) {
    if (!_disableMouseInputs) {
        _mouseInputState.mousePositionCallback(x, y);
    }
}

void NavigationHandler::mouseScrollWheelCallback(double pos) {
    if (!_disableMouseInputs) {
        _mouseInputState.mouseScrollWheelCallback(pos);
    }
}

void NavigationHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    // There is no need to disable the keyboard callback based on a property as the vast
    // majority of input is coming through Lua scripts anyway which are not blocked here
    _keyboardInputState.keyboardCallback(key, modifier, action);
}

bool NavigationHandler::disabledKeybindings() const {
    return _disableKeybindings;
}

bool NavigationHandler::disabledMouse() const {
    return _disableMouseInputs;
}

bool NavigationHandler::disabledJoystick() const {
    return _disableJoystickInputs;
}

NavigationState NavigationHandler::navigationState() const {
    const SceneGraphNode* referenceFrame = _orbitalNavigator.followingAnchorRotation() ?
        _orbitalNavigator.anchorNode() :
        sceneGraph()->root();
    ghoul_assert(
        referenceFrame,
        "The root will always exist and the anchor node ought to be reset when removed"
    );

    return navigationState(*referenceFrame);
}

NavigationState NavigationHandler::navigationState(
                                               const SceneGraphNode& referenceFrame) const
{
    const SceneGraphNode* anchor = _orbitalNavigator.anchorNode();

    if (!anchor) {
        LERROR("No anchor node could be found");
        return NavigationState();
    }

    const glm::dquat invNeutralRotation = glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        anchor->worldPosition() - _camera->positionVec3(),
        glm::normalize(_camera->lookUpVectorWorldSpace())
    ));

    const glm::dquat localRotation = invNeutralRotation * _camera->rotationQuaternion();
    const glm::dvec3 eulerAngles = glm::eulerAngles(localRotation);

    const double pitch = eulerAngles.x;
    const double yaw = -eulerAngles.y;

    // Need to compensate by redisual roll left in local rotation:
    const glm::dquat unroll = glm::angleAxis(eulerAngles.z, glm::dvec3(0.0, 0.0, 1.0));
    const glm::dvec3 neutralUp =
        glm::inverse(invNeutralRotation) * unroll * _camera->lookUpVectorCameraSpace();

    const glm::dmat3 invReferenceFrameTransform =
        glm::inverse(referenceFrame.modelTransform());

    const glm::dvec3 position = invReferenceFrameTransform *
        (glm::dvec4(_camera->positionVec3() - anchor->worldPosition(), 1.0));

    return NavigationState(
        _orbitalNavigator.anchorNode()->identifier(),
        _orbitalNavigator.aimNode() ? _orbitalNavigator.aimNode()->identifier() : "",
        referenceFrame.identifier(),
        position,
        invReferenceFrameTransform * neutralUp,
        yaw,
        pitch,
        global::timeManager->time().j2000Seconds()
    );
}

void NavigationHandler::saveNavigationState(const std::filesystem::path& filepath,
                                        const std::string& referenceFrameIdentifier) const
{
    ghoul_precondition(!filepath.empty(), "File path must not be empty");

    NavigationState state;
    if (!referenceFrameIdentifier.empty()) {
        const SceneGraphNode* referenceFrame = sceneGraphNode(referenceFrameIdentifier);
        if (!referenceFrame) {
            LERROR(std::format(
                "Could not find node '{}' to use as reference frame",
                referenceFrameIdentifier
            ));
            return;
        }
        state = navigationState(*referenceFrame);
    }
    else {
        state = navigationState();
    }

    std::filesystem::path absolutePath = absPath(filepath);
    if (!absolutePath.has_extension()) {
        // Adding the .navstate extension to the filepath if it came without one
        absolutePath.replace_extension(".navstate");
    }
    LINFO(std::format("Saving camera position: {}", absolutePath));

    std::ofstream ofs(absolutePath);

    if (!ofs.good()) {
        throw ghoul::RuntimeError(std::format(
            "Error saving navigation state to '{}'", filepath
        ));
    }

    ofs << state.toJson().dump(2);
}

void NavigationHandler::loadNavigationState(const std::string& filepath,
                                            bool useTimeStamp)
{
    std::filesystem::path absolutePath = absPath(filepath);
    LINFO(std::format("Reading camera state from file: {}", absolutePath));

    if (!absolutePath.has_extension()) {
        // Adding the .navstate extension to the filepath if it came without one
        absolutePath.replace_extension(".navstate");
    }

    if (!std::filesystem::is_regular_file(absolutePath)) {
        throw ghoul::FileNotFoundError(absolutePath, "NavigationState");
    }

    std::ifstream f = std::ifstream(absolutePath);
    const std::string contents = std::string(
        std::istreambuf_iterator<char>(f),
        std::istreambuf_iterator<char>()
    );

    if (contents.empty()) {
        throw::ghoul::RuntimeError(std::format(
            "Failed reading camera state from file: {}. File is empty", absolutePath
        ));
    }

    const nlohmann::json json = nlohmann::json::parse(contents);

    const NavigationState state = NavigationState(json);
    setNavigationStateNextFrame(state);

    if (useTimeStamp && state.timestamp.has_value()) {
        global::timeManager->setTimeNextFrame(Time(*state.timestamp));
    }
}

std::vector<std::string> NavigationHandler::listAllJoysticks() const {
    std::vector<std::string> result;
    result.reserve(global::joystickInputStates->size());

    for (const JoystickInputState& joystickInputState : *global::joystickInputStates) {
        if (!joystickInputState.name.empty()) {
            result.push_back(joystickInputState.name);
        }
    }
    return result;
}

void NavigationHandler::setJoystickAxisMapping(std::string joystickName, int axis,
                                               JoystickCameraStates::AxisType mapping,
                                            JoystickCameraStates::AxisInvert shouldInvert,
                                          JoystickCameraStates::JoystickType joystickType,
                                               bool isSticky,
                                               JoystickCameraStates::AxisFlip shouldFlip,
                                               double sensitivity)
{
    _orbitalNavigator.joystickStates().setAxisMapping(
        std::move(joystickName),
        axis,
        mapping,
        shouldInvert,
        joystickType,
        isSticky,
        shouldFlip,
        sensitivity
    );
}

void NavigationHandler::setJoystickAxisMappingProperty(std::string joystickName,
                                                       int axis,
                                                       std::string propertyUri,
                                                       float min, float max,
                                            JoystickCameraStates::AxisInvert shouldInvert,
                                                       bool isRemote)
{
    _orbitalNavigator.joystickStates().setAxisMappingProperty(
        std::move(joystickName),
        axis,
        std::move(propertyUri),
        min,
        max,
        shouldInvert,
        isRemote
    );
}

void NavigationHandler::setWebsocketAxisMapping(int axis,
                                                WebsocketCameraStates::AxisType mapping,
                                           WebsocketCameraStates::AxisInvert shouldInvert,
                                     WebsocketCameraStates::AxisNormalize shouldNormalize)
{
    _orbitalNavigator.websocketStates().setAxisMapping(
        axis,
        mapping,
        shouldInvert,
        shouldNormalize
    );
}


JoystickCameraStates::AxisInformation
NavigationHandler::joystickAxisMapping(const std::string& joystickName, int axis) const
{
    return _orbitalNavigator.joystickStates().axisMapping(joystickName, axis);
}

void NavigationHandler::setJoystickAxisDeadzone(const std::string& joystickName, int axis,
                                                float deadzone)
{
    _orbitalNavigator.joystickStates().setDeadzone(joystickName, axis, deadzone);
}

float NavigationHandler::joystickAxisDeadzone(const std::string& joystickName,
                                              int axis) const
{
    return _orbitalNavigator.joystickStates().deadzone(joystickName, axis);
}

void NavigationHandler::bindJoystickButtonCommand(const std::string& joystickName,
                                                  int button, std::string command,
                                                  JoystickAction action,
                                         JoystickCameraStates::ButtonCommandRemote remote,
                                                                std::string documentation)
{
    _orbitalNavigator.joystickStates().bindButtonCommand(
        joystickName,
        button,
        std::move(command),
        action,
        remote,
        std::move(documentation)
    );
}

void NavigationHandler::clearJoystickButtonCommand(const std::string& joystickName,
                                                   int button)
{
    _orbitalNavigator.joystickStates().clearButtonCommand(joystickName, button);
}

std::vector<std::string> NavigationHandler::joystickButtonCommand(
                                        const std::string& joystickName, int button) const
{
    return _orbitalNavigator.joystickStates().buttonCommand(joystickName, button);
}

void NavigationHandler::clearGlobalJoystickStates() {
    std::fill(
        global::joystickInputStates->begin(),
        global::joystickInputStates->end(),
        JoystickInputState()
    );
}

scripting::LuaLibrary NavigationHandler::luaLibrary() {
    return {
        "navigation",
        {
            codegen::lua::LoadNavigationState,
            codegen::lua::GetNavigationState,
            codegen::lua::SetNavigationState,
            codegen::lua::SaveNavigationState,
            codegen::lua::RetargetAnchor,
            codegen::lua::RetargetAim,
            codegen::lua::BindJoystickAxis,
            codegen::lua::BindJoystickAxisProperty,
            codegen::lua::JoystickAxis,
            codegen::lua::SetJoystickAxisDeadZone,
            codegen::lua::JoystickAxisDeadzone,
            codegen::lua::BindJoystickButton,
            codegen::lua::ClearJoystickButton,
            codegen::lua::JoystickButton,
            codegen::lua::AddGlobalRotation,
            codegen::lua::AddLocalRotation,
            codegen::lua::AddTruckMovement,
            codegen::lua::AddLocalRoll,
            codegen::lua::AddGlobalRoll,
            codegen::lua::TriggerIdleBehavior,
            codegen::lua::ListAllJoysticks,
            codegen::lua::TargetNextInterestingAnchor,
            codegen::lua::TargetPreviousInterestingAnchor,
            codegen::lua::DistanceToFocus,
            codegen::lua::DistanceToFocusBoundingSphere,
            codegen::lua::DistanceToFocusInteractionSphere
        }
    };
}

} // namespace openspace::interaction
