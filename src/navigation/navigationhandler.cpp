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
#include <openspace/network/parallelpeer.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/query/query.h>
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

    using namespace openspace;
    constexpr properties::Property::PropertyInfo KeyDisableMouseInputInfo = {
        "DisableMouseInputs",
        "Disable all mouse inputs",
        "Disables all mouse inputs and prevents them from affecting the camera"
    };

    constexpr properties::Property::PropertyInfo KeyDisableJoystickInputInfo = {
        "DisableJoystickInputs",
        "Disable all joystick inputs",
        "Disables all joystick inputs and prevents them from affecting the camera"
    };

    constexpr properties::Property::PropertyInfo KeyFrameInfo = {
        "UseKeyFrameInteraction",
        "Use keyframe interaction",
        "If this is set to 'true' the entire interaction is based off key frames rather "
        "than using the mouse interaction."
    };
} // namespace

namespace openspace::interaction {

NavigationHandler::NavigationHandler()
    : properties::PropertyOwner({ "NavigationHandler" })
    , _disableMouseInputs(KeyDisableMouseInputInfo, false)
    , _disableJoystickInputs(KeyDisableJoystickInputInfo, false)
    , _useKeyFrameInteraction(KeyFrameInfo, false)
{
    addPropertySubOwner(_orbitalNavigator);
    addPropertySubOwner(_pathNavigator);

    addProperty(_disableMouseInputs);
    addProperty(_disableJoystickInputs);
    addProperty(_useKeyFrameInteraction);
}

NavigationHandler::~NavigationHandler() {} // NOLINT

void NavigationHandler::initialize() {
    ZoneScoped

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
    ZoneScoped

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

void NavigationHandler::setNavigationStateNextFrame(NavigationState state) {
    _pendingNavigationState = std::move(state);
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

float NavigationHandler::interpolationTime() const {
    return _orbitalNavigator.retargetInterpolationTime();
}

void NavigationHandler::setInterpolationTime(float durationInSeconds) {
    _orbitalNavigator.setRetargetInterpolationTime(durationInSeconds);
}

void NavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(_camera != nullptr, "Camera must not be nullptr");

    // If there is a navigation state to set, do so immediately and then return
    if (_pendingNavigationState.has_value()) {
        applyNavigationState(*_pendingNavigationState);
        return;
    }

    OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    bool playbackMode = (mode == OpenSpaceEngine::Mode::SessionRecordingPlayback);

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

void NavigationHandler::applyNavigationState(const NavigationState& ns) {
    _orbitalNavigator.setAnchorNode(ns.anchor);
    _orbitalNavigator.setAimNode(ns.aim);
    _camera->setPose(ns.cameraPose());

    resetNavigationUpdateVariables();
    _pendingNavigationState.reset();
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

    using namespace std::string_literals;
    if (_inAnchorApproachSphere) {
        if (currDistance > d * (af + InteractionHystersis)) {
            // We left the approach sphere outwards
            _inAnchorApproachSphere = false;

            if (!anchorNode()->onExitAction().empty()) {
                ghoul::Dictionary dict;
                dict.setValue("Node", anchorNode()->identifier());
                dict.setValue("Transition", "Exiting"s);
                for (const std::string& action : anchorNode()->onExitAction()) {
                    global::actionManager->triggerAction(action, dict);
                }
            }

            global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
                _camera,
                anchorNode(),
                events::EventCameraFocusTransition::Transition::Exiting
            );
        }
        else if (currDistance < d * (rf - InteractionHystersis)) {
            // We transitioned from the approach sphere into the reach sphere
            _inAnchorApproachSphere = false;
            _inAnchorReachSphere = true;

            if (!anchorNode()->onReachAction().empty()) {
                ghoul::Dictionary dict;
                dict.setValue("Node", anchorNode()->identifier());
                dict.setValue("Transition", "Reaching"s);
                for (const std::string& action : anchorNode()->onReachAction()) {
                    global::actionManager->triggerAction(action, dict);
                }
            }

            global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
                _camera,
                anchorNode(),
                events::EventCameraFocusTransition::Transition::Reaching
            );
        }
    }
    else if (_inAnchorReachSphere && currDistance > d * (rf + InteractionHystersis)) {
        // We transitioned from the reach sphere to the approach sphere
        _inAnchorReachSphere = false;
        _inAnchorApproachSphere = true;

        if (!anchorNode()->onRecedeAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", anchorNode()->identifier());
            dict.setValue("Transition", "Receding"s);
            for (const std::string& action : anchorNode()->onRecedeAction()) {
                global::actionManager->triggerAction(action, dict);
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            anchorNode(),
            events::EventCameraFocusTransition::Transition::Receding
        );
    }
    else if (!_inAnchorApproachSphere && !_inAnchorReachSphere &&
             currDistance < d * (af - InteractionHystersis))
    {
        // We moved into the approach sphere
        _inAnchorApproachSphere = true;

        if (!anchorNode()->onApproachAction().empty()) {
            ghoul::Dictionary dict;
            dict.setValue("Node", anchorNode()->identifier());
            dict.setValue("Transition", "Approaching"s);
            for (const std::string& action : anchorNode()->onApproachAction()) {
                global::actionManager->triggerAction(action, dict);
            }
        }

        global::eventEngine->publishEvent<events::EventCameraFocusTransition>(
            _camera,
            anchorNode(),
            events::EventCameraFocusTransition::Transition::Approaching
        );
    }
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

    glm::dquat localRotation = invNeutralRotation * _camera->rotationQuaternion();
    glm::dvec3 eulerAngles = glm::eulerAngles(localRotation);

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
        invReferenceFrameTransform * neutralUp, yaw, pitch
    );
}

void NavigationHandler::saveNavigationState(const std::string& filepath,
                                            const std::string& referenceFrameIdentifier)
{
    NavigationState state;
    if (!referenceFrameIdentifier.empty()) {
        const SceneGraphNode* referenceFrame = sceneGraphNode(referenceFrameIdentifier);
        if (!referenceFrame) {
            LERROR(fmt::format(
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

    if (!filepath.empty()) {
        std::filesystem::path absolutePath = absPath(filepath);
        LINFO(fmt::format("Saving camera position: {}", absolutePath));

        std::ofstream ofs(absolutePath);
        ofs << "return " << ghoul::formatLua(state.dictionary());
        ofs.close();
    }
}

void NavigationHandler::loadNavigationState(const std::string& filepath) {
    const std::filesystem::path absolutePath = absPath(filepath);
    LINFO(fmt::format("Reading camera state from file: {}", absolutePath));

    if (!std::filesystem::is_regular_file(absolutePath)) {
        throw ghoul::FileNotFoundError(absolutePath.string(), "NavigationState");
    }

    ghoul::Dictionary navigationStateDictionary;
    try {
        ghoul::lua::loadDictionaryFromFile(
            absolutePath.string(),
            navigationStateDictionary
        );
        openspace::documentation::testSpecificationAndThrow(
            NavigationState::Documentation(),
            navigationStateDictionary,
            "NavigationState"
        );
        setNavigationStateNextFrame(NavigationState(navigationStateDictionary));
    }
    catch (ghoul::RuntimeError& e) {
        LERROR(fmt::format("Unable to set camera position: {}", e.message));
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
                                               double sensitivity)
{
    _orbitalNavigator.joystickStates().setAxisMapping(
        std::move(joystickName),
        axis,
        mapping,
        shouldInvert,
        joystickType,
        isSticky,
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
            codegen::lua::TargetNextInterestingAnchor
        }
    };
}

} // namespace openspace::interaction
