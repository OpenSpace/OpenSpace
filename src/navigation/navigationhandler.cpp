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

#include <openspace/navigation/navigationhandler.h>

#include <openspace/camera/camera.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
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

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;

    constexpr const char* _loggerCat = "NavigationHandler";

    const double Epsilon = 1E-7;

    using namespace openspace;
    constexpr const properties::Property::PropertyInfo KeyDisableMouseInputInfo = {
        "DisableMouseInputs",
        "Disable all mouse inputs",
        "Disables all mouse inputs and prevents them from affecting the camera"
    };

    constexpr const properties::Property::PropertyInfo KeyDisableJoystickInputInfo = {
        "DisableJoystickInputs",
        "Disable all joystick inputs",
        "Disables all joystick inputs and prevents them from affecting the camera"
    };

    constexpr const properties::Property::PropertyInfo KeyFrameInfo = {
        "UseKeyFrameInteraction",
        "Use keyframe interaction",
        "If this is set to 'true' the entire interaction is based off key frames rather "
        "than using the mouse interaction."
    };
} // namespace

#include "navigationhandler_lua.inl"

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

void NavigationHandler::resetCameraDirection() {
    _orbitalNavigator.startRetargetAnchor();
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

    if (_pendingNavigationState.has_value()) {
        applyNavigationState(*_pendingNavigationState);
        _orbitalNavigator.resetVelocities();
        _pendingNavigationState.reset();
    }
    else if (!_playbackModeEnabled && _camera) {
        if (_useKeyFrameInteraction) {
            _keyframeNavigator.updateCamera(*_camera, _playbackModeEnabled);
        }
        else if (_pathNavigator.isPlayingPath()) {
            _pathNavigator.updateCamera(deltaTime);
            updateCameraTransitions();
        }
        else {
            if (_disableJoystickInputs) {
                std::fill(
                    global::joystickInputStates->begin(),
                    global::joystickInputStates->end(),
                    JoystickInputState()
                );
            }
            _orbitalNavigator.updateStatesFromInput(_inputState, deltaTime);
            _orbitalNavigator.updateCameraStateFromStates(deltaTime);
            updateCameraTransitions();
        }

        _orbitalNavigator.updateCameraScalingFromAnchor(deltaTime);
    }

    // If session recording (playback mode) was started in the midst of a camera path,
    // abort the path
    if (_playbackModeEnabled && _pathNavigator.isPlayingPath()) {
        _pathNavigator.abortPath();
    }
}

void NavigationHandler::applyNavigationState(const NavigationState& ns) {
    _orbitalNavigator.setAnchorNode(ns.anchor);
    _orbitalNavigator.setAimNode(ns.aim);

    CameraPose pose = ns.cameraPose();
    _camera->setPositionVec3(pose.position);
    _camera->setRotation(pose.rotation);
    _orbitalNavigator.clearPreviousState();
    _orbitalNavigator.resetNodeMovements();
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


void NavigationHandler::setEnableKeyFrameInteraction() {
    _useKeyFrameInteraction = true;
}

void NavigationHandler::setDisableKeyFrameInteraction() {
    _useKeyFrameInteraction = false;
}

void NavigationHandler::triggerPlaybackStart() {
    _playbackModeEnabled = true;
}

void NavigationHandler::stopPlayback() {
    _orbitalNavigator.resetVelocities();
    _orbitalNavigator.resetNodeMovements();
    _playbackModeEnabled = false;
}

const SceneGraphNode* NavigationHandler::anchorNode() const {
    return _orbitalNavigator.anchorNode();
}

Camera* NavigationHandler::camera() const {
    return _camera;
}

const InputState& NavigationHandler::inputState() const {
    return _inputState;
}

void NavigationHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (!_disableMouseInputs) {
        _inputState.mouseButtonCallback(button, action);
    }
}

void NavigationHandler::mousePositionCallback(double x, double y) {
    if (!_disableMouseInputs) {
        _inputState.mousePositionCallback(x, y);
    }
}

void NavigationHandler::mouseScrollWheelCallback(double pos) {
    if (!_disableMouseInputs) {
        _inputState.mouseScrollWheelCallback(pos);
    }
}

void NavigationHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    // There is no need to disable the keyboard callback based on a property as the vast
    // majority of input is coming through Lua scripts anyway which are not blocked here
    _inputState.keyboardCallback(key, modifier, action);
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

void NavigationHandler::setJoystickAxisMapping(int axis,
                                               JoystickCameraStates::AxisType mapping,
                                            JoystickCameraStates::AxisInvert shouldInvert,
                                      JoystickCameraStates::AxisNormalize shouldNormalize,
                                               bool isSticky,
                                               double sensitivity)
{
    _orbitalNavigator.joystickStates().setAxisMapping(
        axis,
        mapping,
        shouldInvert,
        shouldNormalize,
        isSticky,
        sensitivity
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
NavigationHandler::joystickAxisMapping(int axis) const
{
    return _orbitalNavigator.joystickStates().axisMapping(axis);
}

void NavigationHandler::setJoystickAxisDeadzone(int axis, float deadzone) {
    _orbitalNavigator.joystickStates().setDeadzone(axis, deadzone);
}

float NavigationHandler::joystickAxisDeadzone(int axis) const {
    return _orbitalNavigator.joystickStates().deadzone(axis);
}

void NavigationHandler::bindJoystickButtonCommand(int button, std::string command,
                                                  JoystickAction action,
                                         JoystickCameraStates::ButtonCommandRemote remote,
                                                                std::string documentation)
{
    _orbitalNavigator.joystickStates().bindButtonCommand(
        button,
        std::move(command),
        action,
        remote,
        std::move(documentation)
    );
}

void NavigationHandler::clearJoystickButtonCommand(int button) {
    _orbitalNavigator.joystickStates().clearButtonCommand(button);
}

std::vector<std::string> NavigationHandler::joystickButtonCommand(int button) const {
    return _orbitalNavigator.joystickStates().buttonCommand(button);
}

scripting::LuaLibrary NavigationHandler::luaLibrary() {
    return {
        "navigation",
        {
            {
                "getNavigationState",
                &luascriptfunctions::getNavigationState,
                "[string]",
                "Return the current navigation state as a lua table. The optional "
                "argument is the scene graph node to use as reference frame. By default, "
                "the reference frame will picked based on whether the orbital navigator "
                "is currently following the anchor node rotation. If it is, the anchor "
                "will be chosen as reference frame. If not, the reference frame will be "
                "set to the scene graph root."
            },
            {
                "setNavigationState",
                &luascriptfunctions::setNavigationState,
                "table",
                "Set the navigation state. The argument must be a valid Navigation State."
            },
            {
                "saveNavigationState",
                &luascriptfunctions::saveNavigationState,
                "string, [string]",
                "Save the current navigation state to a file with the path given by the "
                "first argument. The optoinal second argument is the scene graph node to "
                "use as reference frame. By default, the reference frame will picked "
                "based on whether the orbital navigator is currently following the "
                "anchor node rotation. If it is, the anchor will be chosen as reference "
                "frame. If not, the reference frame will be set to the scene graph root."
            },
            {
                "loadNavigationState",
                &luascriptfunctions::loadNavigationState,
                "string",
                "Load a navigation state from file. The file should be a lua file "
                "returning the navigation state as a table formatted as a "
                "Navigation State, such as the output files of saveNavigationState."
            },
            {
                "retargetAnchor",
                &luascriptfunctions::retargetAnchor,
                "void",
                "Reset the camera direction to point at the anchor node"
            },
            {
                "retargetAim",
                &luascriptfunctions::retargetAim,
                "void",
                "Reset the camera direction to point at the aim node"
            },
            {
                "bindJoystickAxis",
                &luascriptfunctions::bindJoystickAxis,
                "int, axisType [, isInverted, isNormalized]",
                "Binds the axis identified by the first argument to be used as the type "
                "identified by the second argument. If 'isInverted' is 'true', the axis "
                "value is inverted, if 'isNormalized' is true the axis value is "
                "normalized from [-1, 1] to [0,1]."
            },
            {
                "joystickAxis",
                &luascriptfunctions::joystickAxis,
                "int",
                "Returns the joystick axis information for the passed axis. The "
                "information that is returned is the current axis binding as a string, "
                "whether the values are inverted as bool, and whether the value are "
                "normalized as a bool"
            },
            {
                "setAxisDeadZone",
                &luascriptfunctions::setJoystickAxisDeadzone,
                "int, float",
                "Sets the deadzone for a particular joystick axis which means that any "
                "input less than this value is completely ignored."
            },
            {
                "bindJoystickButton",
                &luascriptfunctions::bindJoystickButton,
                "int, string [, string, bool]",
                "Binds a Lua script to be executed when the joystick button identified "
                "by the first argument is triggered. The third argument determines when "
                "the script should be executed, this defaults to 'pressed', which means "
                "that the script is run when the user presses the button. The last "
                "argument determines whether the command is going to be executable "
                "locally or remotely. The latter being the default."
            },
            {
                "clearJoystickButton",
                &luascriptfunctions::clearJoystickButton,
                "int",
                "Removes all commands that are currently bound to the button identified "
                "by the first argument"
            },
            {
                "joystickButton",
                &luascriptfunctions::joystickButton,
                "int",
                "Returns the script that is currently bound to be executed when the "
                "provided button is pressed"
            },
            {
                "addGlobalRotation",
                &luascriptfunctions::addGlobalRotation,
                "double, double",
                "Directly adds to the global rotation of the camera"
            },
            {
                "addLocalRotation",
                &luascriptfunctions::addLocalRotation,
                "double, double",
                "Directly adds to the local rotation of the camera"
            },
            {
                "addTruckMovement",
                &luascriptfunctions::addTruckMovement,
                "double, double",
                "Directly adds to the truck movement of the camera"
            },
            {
                "addLocalRoll",
                &luascriptfunctions::addLocalRoll,
                "double, double",
                "Directly adds to the local roll of the camera"
            },
            {
                "addGlobalRoll",
                &luascriptfunctions::addGlobalRoll,
                "double, double",
                "Directly adds to the global roll of the camera"
            }
        }
    };
}

} // namespace openspace::interaction
