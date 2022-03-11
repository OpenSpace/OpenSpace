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

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;

    constexpr const char* _loggerCat = "NavigationHandler";

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

    /**
     * Load a navigation state from file. The file should be a lua file returning the
     * navigation state as a table formatted as a Navigation State, such as the output
     * files of saveNavigationState.
     */
    [[codegen::luawrap]] void loadNavigationState(std::string cameraStateFilePath) {
        if (cameraStateFilePath.empty()) {
            throw ghoul::lua::LuaError("Filepath string is empty");
        }

        global::navigationHandler->loadNavigationState(cameraStateFilePath);
    }

    /**
     * Return the current navigation state as a lua table. The optional argument is the
     * scene graph node to use as reference frame. By default, the reference frame will
     * picked based on whether the orbital navigator is currently following the anchor
     * node rotation. If it is, the anchor will be chosen as reference frame. If not, the
     * reference frame will be set to the scene graph root.
     */
    [[codegen::luawrap]] ghoul::Dictionary getNavigationState(
                                                         std::optional<std::string> frame)
    {
        interaction::NavigationState state;
        if (frame.has_value()) {
            const SceneGraphNode* referenceFrame = sceneGraphNode(*frame);
            if (!referenceFrame) {
                throw ghoul::lua::LuaError(
                    fmt::format("Could not find node '{}' as reference frame", *frame)
                );
            }
            state = global::navigationHandler->navigationState(*referenceFrame);
        }
        else {
            state = global::navigationHandler->navigationState();
        }

        ghoul::Dictionary res;
        res.setValue("Anchor", state.anchor);
        if (!state.aim.empty()) {
            res.setValue("Aim", state.aim);
        }
        if (!state.referenceFrame.empty()) {
            res.setValue("ReferenceFrame", state.referenceFrame);
        }
        res.setValue("ReferenceFrame", state.position);
        if (state.up.has_value()) {
            res.setValue("Up", *state.up);
        }
        if (state.yaw != 0) {
            res.setValue("Up", state.yaw);
        }
        if (state.pitch != 0) {
            res.setValue("Pitch", state.pitch);
        }
        return res;
    }

    // Set the navigation state. The argument must be a valid Navigation State.
    [[codegen::luawrap]] void setNavigationState(ghoul::Dictionary navigationState) {
        documentation::TestResult r = documentation::testSpecification(
            interaction::NavigationState::Documentation(),
            navigationState
        );

        if (!r.success) {
            throw ghoul::lua::LuaError(
                fmt::format("Could not set camera state: {}", ghoul::to_string(r))
            );
        }

        global::navigationHandler->setNavigationStateNextFrame(
            interaction::NavigationState(navigationState)
        );
    }

    /**
     * Save the current navigation state to a file with the path given by the first
     * argument. The optoinal second argument is the scene graph node to use as reference
     * frame. By default, the reference frame will picked based on whether the orbital
     * navigator is currently following the anchor node rotation. If it is, the anchor
     * will be chosen as reference frame. If not, the reference frame will be set to the
     * scene graph root.
     */
    [[codegen::luawrap]] void saveNavigationState(std::string path,
                                                  std::string frame = "")
    {
        if (path.empty()) {
            throw ghoul::lua::LuaError("Filepath string is empty");
        }

        global::navigationHandler->saveNavigationState(path, frame);
    }

    // Reset the camera direction to point at the anchor node
    [[codegen::luawrap]] void retargetAnchor() {
        global::navigationHandler->orbitalNavigator().startRetargetAnchor();
    }

    // Reset the camera direction to point at the aim node
    [[codegen::luawrap]] void retargetAim() {
        global::navigationHandler->orbitalNavigator().startRetargetAim();
    }

    /**
     * Finds the input joystick with the given 'name' and binds the axis identified by the
     * second argument to be used as the type identified by the third argument. If
     * 'isInverted' is 'true', the axis value is inverted. 'joystickType' is if the
     * joystick behaves more like a joystick or a trigger, where the first is the default.
     * If 'isSticky' is 'true', the value is calculated relative to the previous value. If
     * 'sensitivity' is given then that value will affect the sensitivity of the axis
     * together with the global sensitivity.
     */
    [[codegen::luawrap]] void bindJoystickAxis(std::string joystickName, int axis,
                                               std::string axisType,
                                               bool shouldInvert = false,
                                               std::string joystickType = "JoystickLike",
                                               bool isSticky = false,
                                               double sensitivity = 0.0)
    {
        global::navigationHandler->setJoystickAxisMapping(
            std::move(joystickName),
            axis,
            ghoul::from_string<interaction::JoystickCameraStates::AxisType>(axisType),
            interaction::JoystickCameraStates::AxisInvert(shouldInvert),
            ghoul::from_string<interaction::JoystickCameraStates::JoystickType>(
                joystickType
                ),
            isSticky,
            sensitivity
        );
    }

    /**
     * Finds the input joystick with the given 'name' and binds the axis identified by the
     * second argument to be bound to the property identified by the third argument. 'min'
     * and 'max' is the minimum and the maximum allowed value for the given property and
     * the axis value is rescaled from [-1, 1] to [min, max], default is [0, 1]. If
     * 'isInverted' is 'true', the axis value is inverted. The last argument determines
     * whether the property change is going to be executed locally or remotely, where the
     * latter is the default.
     */
    [[codegen::luawrap]] void bindJoystickAxisProperty(std::string joystickName, int axis,
                                                       std::string propertyUri,
                                                       float min = 0.f, float max = 1.f,
                                                       bool shouldInvert = false,
                                                       bool isRemote = true)
    {
        global::navigationHandler->setJoystickAxisMappingProperty(
            std::move(joystickName),
            axis,
            std::move(propertyUri),
            min,
            max,
            interaction::JoystickCameraStates::AxisInvert(shouldInvert),
            isRemote
        );
    }

    /**
     * Finds the input joystick with the given 'name' and returns the joystick axis
     * information for the passed axis. The information that is returned is the current
     * axis binding as a string, whether the values are inverted as bool, the joystick
     * type as a string, whether the axis is sticky as bool, the sensitivity as number,
     * the property uri bound to the axis as string (empty is type is not Property), the
     * min and max values for the property as numbers and whether the property change will
     * be executed remotly as bool.
     */
    [[codegen::luawrap]] std::tuple<std::string, bool, std::string, bool, double,
        std::string, float, float, bool> joystickAxis(std::string joystickName, int axis)
    {
        using AI = interaction::JoystickCameraStates::AxisInformation;
        AI info = global::navigationHandler->joystickAxisMapping(joystickName, axis);

        return {
            ghoul::to_string(info.type),
            static_cast<bool>(info.invert),
            ghoul::to_string(info.joystickType),
            info.isSticky,
            info.sensitivity,
            info.propertyUri,
            info.minValue,
            info.maxValue,
            info.isRemote
        };
    }

    /**
     * Finds the input joystick with the given 'name' and sets the deadzone for a
     * particular joystick axis, which means that any input less than this value is
     * completely ignored.
     */
    [[codegen::luawrap]] void setJoystickAxisDeadZone(std::string joystickName, int axis,
                                                      float deadzone)
    {
        global::navigationHandler->setJoystickAxisDeadzone(joystickName, axis, deadzone);
    }

    [[codegen::luawrap]] float joystickAxisDeadzone(std::string joystickName, int axis) {
        float deadzone = global::navigationHandler->joystickAxisDeadzone(
            joystickName,
            axis
        );
        return deadzone;
    }

    /**
     * Finds the input joystick with the given 'name' and binds a Lua script given by the
     * third argument to be executed when the joystick button identified by the second
     * argument is triggered. The fifth argument determines when the script should be
     * executed, this defaults to 'Press', which means that the script is run when the
     * user presses the button. The fourth arguemnt is the documentation of the script in
     * the third argument. The sixth argument determines whether the command is going to
     * be executable locally or remotely, where the latter is the default.
     */
    [[codegen::luawrap]] void bindJoystickButton(std::string joystickName, int button,
                                                 std::string command,
                                                 std::string documentation,
                                                 std::string action = "Press",
                                                 bool isRemote = true)
    {
        interaction::JoystickAction act =
            ghoul::from_string<interaction::JoystickAction>(action);

        global::navigationHandler->bindJoystickButtonCommand(
            joystickName,
            button,
            command,
            act,
            interaction::JoystickCameraStates::ButtonCommandRemote(isRemote),
            documentation
        );
    }

    /**
     * Finds the input joystick with the given 'name' and removes all commands that are
     * currently bound to the button identified by the second argument.
     */
    [[codegen::luawrap]] void clearJoystickButton(std::string joystickName, int button) {
        global::navigationHandler->clearJoystickButtonCommand(joystickName, button);
    }

    /**
     * Finds the input joystick with the given 'name' and returns the script that is
     * currently bound to be executed when the provided button is pressed.
     */
    [[codegen::luawrap]] std::string joystickButton(std::string joystickName, int button)
    {
        const std::vector<std::string>& cmds =
            global::navigationHandler->joystickButtonCommand(joystickName, button);

        std::string cmd = std::accumulate(
            cmds.cbegin(),
            cmds.cend(),
            std::string(),
            [](const std::string& lhs, const std::string& rhs) {
                return lhs + ";" + rhs;
            }
        );
        return cmd;
    }

    // Directly adds to the global rotation of the camera
    [[codegen::luawrap]] void addGlobalRotation(double v1, double v2) {
        global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRotation(
            glm::dvec2(v1, v2)
        );
    }

    // Directly adds to the local rotation of the camera
    [[codegen::luawrap]] void addLocalRotation(double v1, double v2) {
        global::navigationHandler->orbitalNavigator().scriptStates().addLocalRotation(
            glm::dvec2(v1, v2)
        );
    }

    // Directly adds to the truck movement of the camera
    [[codegen::luawrap]] void addTruckMovement(double v1, double v2) {
        global::navigationHandler->orbitalNavigator().scriptStates().addTruckMovement(
            glm::dvec2(v1, v2)
        );
    }

    // Directly adds to the local roll of the camera
    [[codegen::luawrap]] void addLocalRoll(double v1, double v2) {
        global::navigationHandler->orbitalNavigator().scriptStates().addLocalRoll(
            glm::dvec2(v1, v2)
        );
    }

    // Directly adds to the global roll of the camera
    [[codegen::luawrap]] void addGlobalRoll(double v1, double v2) {
        global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRoll(
            glm::dvec2(v1, v2)
        );
    }

#include "navigationhandler_codegen.cpp"
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
            codegen::lua::loadNavigationState,
            codegen::lua::getNavigationState,
            codegen::lua::setNavigationState,
            codegen::lua::saveNavigationState,
            codegen::lua::retargetAnchor,
            codegen::lua::retargetAim,
            codegen::lua::bindJoystickAxis,
            codegen::lua::bindJoystickAxisProperty,
            codegen::lua::joystickAxis,
            codegen::lua::setJoystickAxisDeadZone,
            codegen::lua::joystickAxisDeadzone,
            codegen::lua::bindJoystickButton,
            codegen::lua::clearJoystickButton,
            codegen::lua::joystickButton,
            codegen::lua::addGlobalRotation,
            codegen::lua::addLocalRotation,
            codegen::lua::addTruckMovement,
            codegen::lua::addLocalRoll,
            codegen::lua::addGlobalRoll
        }
    };
}

} // namespace openspace::interaction
