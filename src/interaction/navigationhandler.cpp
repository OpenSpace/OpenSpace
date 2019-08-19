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

#include <openspace/interaction/navigationhandler.h>

#include <openspace/engine/globals.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/documentation/verifier.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <glm/gtx/vector_angle.hpp>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "NavigationHandler";

    constexpr const char* KeyAnchor = "Anchor";
    constexpr const char* KeyAim = "Aim";
    constexpr const char* KeyPosition = "Position";
    constexpr const char* KeyUp = "Up";
    constexpr const char* KeyYaw = "Yaw";
    constexpr const char* KeyPitch = "Pitch";
    constexpr const char* KeyReferenceFrame = "ReferenceFrame";
    const double Epsilon = 1E-7;

    constexpr const openspace::properties::Property::PropertyInfo KeyFrameInfo = {
        "UseKeyFrameInteraction",
        "Use keyframe interaction",
        "If this is set to 'true' the entire interaction is based off key frames rather "
        "than using the mouse interaction."
    };

} // namespace

#include "navigationhandler_lua.inl"

namespace openspace::interaction {


ghoul::Dictionary
    openspace::interaction::NavigationHandler::NavigationState::dictionary() const
{
    ghoul::Dictionary cameraDict;
    cameraDict.setValue(KeyPosition, position);
    cameraDict.setValue(KeyAnchor, anchor);

    if (anchor != referenceFrame) {
        cameraDict.setValue(KeyReferenceFrame, referenceFrame);
    }
    if (!aim.empty()) {
        cameraDict.setValue(KeyAim, aim);
    }
    if (up.has_value()) {
        cameraDict.setValue(KeyUp, up.value());

        if (std::abs(yaw) > Epsilon) {
            cameraDict.setValue(KeyYaw, yaw);
        }
        if (std::abs(pitch) > Epsilon) {
            cameraDict.setValue(KeyPitch, pitch);
        }
    }

    return cameraDict;
}

openspace::interaction::NavigationHandler::NavigationState::NavigationState(
    const ghoul::Dictionary& dictionary)
{
    const bool hasAnchor = dictionary.hasValue<std::string>(KeyAnchor);
    const bool hasPosition = dictionary.hasValue<glm::dvec3>(KeyPosition);
    if (!hasAnchor || !hasPosition) {
        throw ghoul::RuntimeError(
            "Position and Anchor need to be defined for navigation dictionary."
        );
    }

    anchor = dictionary.value<std::string>(KeyAnchor);
    position = dictionary.value<glm::dvec3>(KeyPosition);

    if (dictionary.hasValue<std::string>(KeyReferenceFrame)) {
        referenceFrame = dictionary.value<std::string>(KeyReferenceFrame);
    }
    else {
        referenceFrame = anchor;
    }
    if (dictionary.hasValue<std::string>(KeyAim)) {
        aim = dictionary.value<std::string>(KeyAim);
    }

    if (dictionary.hasValue<glm::dvec3>(KeyUp)) {
        up = dictionary.value<glm::dvec3>(KeyUp);

        if (dictionary.hasValue<double>(KeyYaw)) {
            yaw = dictionary.value<double>(KeyYaw);
        }
        if (dictionary.hasValue<double>(KeyPitch)) {
            pitch = dictionary.value<double>(KeyPitch);
        }
    }
}

openspace::interaction::NavigationHandler::NavigationState::NavigationState(
    std::string anchor,
    std::string aim,
    std::string referenceFrame,
    glm::dvec3 position,
    std::optional<glm::dvec3> up,
    double yaw,
    double pitch)
    : anchor(std::move(anchor))
    , aim(std::move(aim))
    , referenceFrame(std::move(referenceFrame))
    , position(std::move(position))
    , up(std::move(up))
    , yaw(yaw)
    , pitch(pitch)
{}

NavigationHandler::NavigationHandler()
    : properties::PropertyOwner({ "NavigationHandler" })
    , _useKeyFrameInteraction(KeyFrameInfo, false)
{
    // Add the properties
    addProperty(_useKeyFrameInteraction);
    addPropertySubOwner(_orbitalNavigator);
}

NavigationHandler::~NavigationHandler() {} // NOLINT

void NavigationHandler::initialize() {
    global::parallelPeer.connectionEvent().subscribe(
        "NavigationHandler",
        "statusChanged",
        [this]() {
            _useKeyFrameInteraction = (global::parallelPeer.status() ==
                ParallelConnection::Status::ClientWithHost);
        }
    );
}

void NavigationHandler::deinitialize() {
    global::parallelPeer.connectionEvent().unsubscribe("NavigationHandler");
}

void NavigationHandler::setCamera(Camera* camera) {
    _camera = camera;
    _orbitalNavigator.setCamera(camera);
}

void NavigationHandler::setNavigationStateNextFrame(
    NavigationHandler::NavigationState state)
{
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
        applyNavigationState(_pendingNavigationState.value());
        _orbitalNavigator.resetVelocities();
        _pendingNavigationState.reset();
    }
    else if (!_playbackModeEnabled && _camera) {
        if (_useKeyFrameInteraction) {
            _keyframeNavigator.updateCamera(*_camera, _playbackModeEnabled);
        }
        else {
            _orbitalNavigator.updateStatesFromInput(_inputState, deltaTime);
            _orbitalNavigator.updateCameraStateFromStates(deltaTime);
        }
    }
}

void NavigationHandler::applyNavigationState(const NavigationHandler::NavigationState& ns)
{
    const SceneGraphNode* referenceFrame = sceneGraphNode(ns.referenceFrame);
    const SceneGraphNode* anchor = sceneGraphNode(ns.anchor);

    if (!anchor) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as anchor.", ns.referenceFrame
        ));
        return;
    }
    if (!ns.aim.empty() && !sceneGraphNode(ns.aim)) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as aim.", ns.referenceFrame
        ));
        return;
    }
    if (!referenceFrame) {
        LERROR(fmt::format(
            "Could not find scene graph node '{}' used as reference frame.",
            ns.referenceFrame)
        );
        return;
    }

    const glm::dvec3 anchorWorldPosition = anchor->worldPosition();
    const glm::dmat3 referenceFrameTransform = referenceFrame->worldRotationMatrix();

    _orbitalNavigator.setAnchorNode(ns.anchor);
    _orbitalNavigator.setAimNode(ns.aim);

    const SceneGraphNode* anchorNode = _orbitalNavigator.anchorNode();
    const SceneGraphNode* aimNode = _orbitalNavigator.aimNode();
    if (!aimNode) {
        aimNode = anchorNode;
    }

    const glm::dvec3 cameraPositionWorld = anchorWorldPosition +
        glm::dvec3(referenceFrameTransform * glm::dvec4(ns.position, 1.0));

    glm::dvec3 up = ns.up.has_value() ?
        glm::normalize(referenceFrameTransform * ns.up.value()) :
        glm::dvec3(0.0, 1.0, 0.0);

    // Construct vectors of a "neutral" view, i.e. when the aim is centered in view.
    glm::dvec3 neutralView =
        glm::normalize(aimNode->worldPosition() - cameraPositionWorld);

    glm::dquat neutralCameraRotation = glm::inverse(glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0),
        neutralView,
        up
    )));

    glm::dquat pitchRotation = glm::angleAxis(ns.pitch, glm::dvec3(1.f, 0.f, 0.f));
    glm::dquat yawRotation = glm::angleAxis(ns.yaw, glm::dvec3(0.f, -1.f, 0.f));

    _camera->setPositionVec3(cameraPositionWorld);
    _camera->setRotation(neutralCameraRotation * yawRotation * pitchRotation);
    _orbitalNavigator.clearPreviousState();
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

Camera* NavigationHandler::camera() const {
    return _camera;
}

const InputState& NavigationHandler::inputState() const {
    return _inputState;
}

void NavigationHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    _inputState.mouseButtonCallback(button, action);
}

void NavigationHandler::mousePositionCallback(double x, double y) {
    _inputState.mousePositionCallback(x, y);
}

void NavigationHandler::mouseScrollWheelCallback(double pos) {
    _inputState.mouseScrollWheelCallback(pos);
}

void NavigationHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    _inputState.keyboardCallback(key, modifier, action);
}

NavigationHandler::NavigationState NavigationHandler::navigationState(
    const SceneGraphNode& referenceFrame) const
{
    const SceneGraphNode* anchor = _orbitalNavigator.anchorNode();
    const SceneGraphNode* aim = _orbitalNavigator.aimNode();

    if (!aim) {
        aim = anchor;
    }

    const glm::dquat invNeutralRotation = glm::quat_cast(glm::lookAt(
        glm::dvec3(0.0, 0.0, 0.0),
        aim->worldPosition() - _camera->positionVec3(),
        glm::normalize(_camera->lookUpVectorWorldSpace())
    ));

    glm::dquat localRotation = invNeutralRotation * _camera->rotationQuaternion();
    glm::dvec3 eulerAngles = glm::eulerAngles(localRotation);

    const double pitch = eulerAngles.x;
    const double yaw = -eulerAngles.y;

    // Need to compensate by redisual roll left in local rotation:
    const glm::dquat unroll = glm::angleAxis(eulerAngles.z, glm::dvec3(0, 0, 1));
    const glm::dvec3 neutralUp =
        glm::inverse(invNeutralRotation) * unroll * _camera->lookUpVectorCameraSpace();

    const glm::dmat3 invReferenceFrameTransform =
        glm::inverse(referenceFrame.worldRotationMatrix());

    const glm::dvec3 position = invReferenceFrameTransform *
        (glm::dvec4(_camera->positionVec3() - anchor->worldPosition(), 1.0));

    return NavigationState(
        _orbitalNavigator.anchorNode()->identifier(),
        _orbitalNavigator.aimNode() ?
        _orbitalNavigator.aimNode()->identifier() : "",
        referenceFrame.identifier(),
        position,
        invReferenceFrameTransform * neutralUp, yaw, pitch
    );
}

void NavigationHandler::saveNavigationState(const std::string& filepath,
    const std::string& referenceFrameIdentifier)
{
    const SceneGraphNode* referenceFrame = _orbitalNavigator.followingAnchorRotation() ?
        _orbitalNavigator.anchorNode() :
        sceneGraph()->root();

    if (!referenceFrameIdentifier.empty()) {
        referenceFrame = sceneGraphNode(referenceFrameIdentifier);
        if (!referenceFrame) {
            LERROR(fmt::format(
                "Could not find node '{}' to use as reference frame",
                referenceFrameIdentifier
            ));
            return;
        }
    }

    if (!filepath.empty()) {
        std::string absolutePath = absPath(filepath);
        LINFO(fmt::format("Saving camera position: {}", absolutePath));

        ghoul::Dictionary cameraDict = navigationState(*referenceFrame).dictionary();
        ghoul::DictionaryLuaFormatter formatter;

        std::ofstream ofs(absolutePath.c_str());
        ofs << "return " << formatter.format(cameraDict);
        ofs.close();
    }
}

void NavigationHandler::loadNavigationState(const std::string& filepath) {
    const std::string absolutePath = absPath(filepath);
    LINFO(fmt::format("Reading camera state from file: {}", absolutePath));

    if (!FileSys.fileExists(absolutePath)) {
        throw ghoul::FileNotFoundError(absolutePath, "NavigationState");
    }

    ghoul::Dictionary navigationStateDictionary;
    try {
        ghoul::lua::loadDictionaryFromFile(absolutePath, navigationStateDictionary);
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
    JoystickCameraStates::AxisNormalize shouldNormalize)
{
    _orbitalNavigator.joystickStates().setAxisMapping(
        axis,
        mapping,
        shouldInvert,
        shouldNormalize
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

documentation::Documentation NavigationHandler::NavigationState::Documentation() {
    using namespace documentation;

    return {
        "Navigation State",
        "core_navigation_state",
        {
            {
                KeyAnchor,
                new StringVerifier,
                Optional::No,
                "The identifier of the anchor node."
            },
            {
                KeyAim,
                new StringVerifier,
                Optional::Yes,
                "The identifier of the aim node, if used."
            },
            {
                KeyReferenceFrame,
                new StringVerifier,
                Optional::Yes,
                "The identifier of the scene graph node to use as reference frame. "
                "If not specified, this will be the same as the anchor."
            },
            {
                KeyPosition,
                new DoubleVector3Verifier,
                Optional::No,
                "The position of the camera relative to the anchor node, "
                "expressed in meters in the specified reference frame."
            },
            {
                KeyUp,
                new DoubleVector3Verifier,
                Optional::Yes,
                "The up vector expressed in the coordinate system of the reference frame."
            },
            {
                KeyYaw,
                new DoubleVerifier,
                Optional::Yes,
                "The yaw angle in radians. "
                "Positive angle means yawing camera to the right."
            },
            {
                KeyPitch,
                new DoubleVerifier,
                Optional::Yes,
                "The pitch angle in radians. "
                "Positive angle means pitching camera upwards."
            },
        }
    };
}

scripting::LuaLibrary NavigationHandler::luaLibrary() {
    return {
        "navigation",
        {
            {
                "setNavigationState",
                &luascriptfunctions::setNavigationState,
                {},
                "table",
                "Set the navigation state. "
                "The argument must be a valid Navigation State."
            },
            {
                "saveNavigationState",
                &luascriptfunctions::saveNavigationState,
                {},
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
                {},
                "string",
                "Load a navigation state from file. The file should be a lua file "
                "returning the navigation state as a table formatted as a "
                "Navigation State, such as the output files of saveNavigationState."
            },
            {
                "retargetAnchor",
                &luascriptfunctions::retargetAnchor,
                {},
                "void",
                "Reset the camera direction to point at the anchor node"
            },
            {
                "retargetAim",
                &luascriptfunctions::retargetAim,
                {},
                "void",
                "Reset the camera direction to point at the aim node"
            },
            {
                "bindJoystickAxis",
                &luascriptfunctions::bindJoystickAxis,
                {},
                "int, axisType [, isInverted, isNormalized]",
                "Binds the axis identified by the first argument to be used as the type "
                "identified by the second argument. If 'isInverted' is 'true', the axis "
                "value is inverted, if 'isNormalized' is true the axis value is "
                "normalized from [-1, 1] to [0,1]."
            },
            {
                "joystickAxis",
                &luascriptfunctions::joystickAxis,
                {},
                "int",
                "Returns the joystick axis information for the passed axis. The "
                "information that is returned is the current axis binding as a string, "
                "whether the values are inverted as bool, and whether the value are "
                "normalized as a bool"
            },
            {
                "setAxisDeadZone",
                &luascriptfunctions::setJoystickAxisDeadzone,
                {},
                "int, float",
                "Sets the deadzone for a particular joystick axis which means that any "
                "input less than this value is completely ignored."
            },
            {
                "bindJoystickButton",
                &luascriptfunctions::bindJoystickButton,
                {},
                "int, string [, string, bool]",
                "Binds a Lua script to be executed when the joystick button identified "
                "by the first argument is triggered. The third argument determines when "
                "the script should be executed, this defaults to 'pressed', which means "
                "that the script is run when the user presses the button. The last "
                "argument determines whether the command is going to be executable "
                "locally or remotely. The latter being the default."
            },
            {
                "clearJoystickButotn",
                &luascriptfunctions::clearJoystickButton,
                {},
                "int",
                "Removes all commands that are currently bound to the button identified "
                "by the first argument"
            },
            {
                "joystickButton",
                &luascriptfunctions::joystickButton,
                {},
                "int",
                "Returns the script that is currently bound to be executed when the "
                "provided button is pressed"
            },
            {
                "addGlobalRotation",
                &luascriptfunctions::addGlobalRotation,
                {},
                "double, double",
                "Directly adds to the global rotation of the camera"
            },
            {
                "addLocalRotation",
                &luascriptfunctions::addLocalRotation,
                {},
                "double, double",
                "Directly adds to the local rotation of the camera"
            },
            {
                "addTruckMovement",
                &luascriptfunctions::addTruckMovement,
                {},
                "double, double",
                "Directly adds to the truck movement of the camera"
            },
            {
                "addLocalRoll",
                &luascriptfunctions::addLocalRoll,
                {},
                "double, double",
                "Directly adds to the local roll of the camera"
            },
            {
                "addGlobalRoll",
                &luascriptfunctions::addGlobalRoll,
                {},
                "double, double",
                "Directly adds to the global roll of the camera"
            }
        }
    };
}

} // namespace openspace::interaction
