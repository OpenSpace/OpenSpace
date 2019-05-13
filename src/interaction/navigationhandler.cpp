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
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/util/camera.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "NavigationHandler";

    constexpr const char* KeyAnchor = "Anchor";
    constexpr const char* KeyAim = "Aim";
    constexpr const char* KeyPosition = "Position";
    constexpr const char* KeyRotation = "Rotation";

    constexpr const openspace::properties::Property::PropertyInfo KeyFrameInfo = {
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
    , _useKeyFrameInteraction(KeyFrameInfo, false)
{

    _inputState = std::make_unique<InputState>();
    _orbitalNavigator = std::make_unique<OrbitalNavigator>();
    _keyframeNavigator = std::make_unique<KeyframeNavigator>();

    // Add the properties
    addProperty(_useKeyFrameInteraction);
    addPropertySubOwner(*_orbitalNavigator);
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
    _orbitalNavigator->setCamera(camera);
}

const OrbitalNavigator& NavigationHandler::orbitalNavigator() const {
    return *_orbitalNavigator;
}

OrbitalNavigator& NavigationHandler::orbitalNavigator() {
    return *_orbitalNavigator;
}

KeyframeNavigator& NavigationHandler::keyframeNavigator() const {
    return *_keyframeNavigator;
}

bool NavigationHandler::isKeyFrameInteractionEnabled() const {
    return _useKeyFrameInteraction;
}

float NavigationHandler::interpolationTime() const {
    return _orbitalNavigator->retargetInterpolationTime();
}

void NavigationHandler::setInterpolationTime(float durationInSeconds) {
    _orbitalNavigator->setRetargetInterpolationTime(durationInSeconds);
}

void NavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState must not be nullptr");
    ghoul_assert(_camera != nullptr, "Camera must not be nullptr");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else if ( ! _playbackModeEnabled ) {
        if (_camera) {
            if (_useKeyFrameInteraction) {
                _keyframeNavigator->updateCamera(*_camera, _playbackModeEnabled);
            }
            else {
                _orbitalNavigator->updateStatesFromInput(*_inputState, deltaTime);
                _orbitalNavigator->updateCameraStateFromStates(deltaTime);
            }
        }
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
    _playbackModeEnabled = false;
}

Camera* NavigationHandler::camera() const {
    return _camera;
}

const InputState& NavigationHandler::inputState() const {
    return *_inputState;
}

void NavigationHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    _inputState->mouseButtonCallback(button, action);
}

void NavigationHandler::mousePositionCallback(double x, double y) {
    _inputState->mousePositionCallback(x, y);
}

void NavigationHandler::mouseScrollWheelCallback(double pos) {
    _inputState->mouseScrollWheelCallback(pos);
}

void NavigationHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    _inputState->keyboardCallback(key, modifier, action);
}

void NavigationHandler::setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict)
{
    bool readSuccessful = true;

    std::string anchor;
    std::string aim;
    glm::dvec3 cameraPosition;
    glm::dvec4 cameraRotation; // Need to read the quaternion as a vector first.

    readSuccessful &= cameraDict.getValue(KeyAnchor, anchor);
    readSuccessful &= cameraDict.getValue(KeyPosition, cameraPosition);
    readSuccessful &= cameraDict.getValue(KeyRotation, cameraRotation);
    cameraDict.getValue(KeyAim, aim); // Aim is not required

    if (!readSuccessful) {
        throw ghoul::RuntimeError(
            "Position, Rotation and Focus need to be defined for camera dictionary."
        );
    }

    // Set state
    _orbitalNavigator->setAnchorNode(anchor);
    _orbitalNavigator->setAimNode(aim);

    _camera->setPositionVec3(cameraPosition);
    _camera->setRotation(glm::dquat(
        cameraRotation.x, cameraRotation.y, cameraRotation.z, cameraRotation.w));
}

ghoul::Dictionary NavigationHandler::cameraStateDictionary() {
    glm::dvec3 cameraPosition;
    glm::dquat quat;
    glm::dvec4 cameraRotation;

    cameraPosition = _camera->positionVec3();
    quat = _camera->rotationQuaternion();
    cameraRotation = glm::dvec4(quat.w, quat.x, quat.y, quat.z);

    ghoul::Dictionary cameraDict;
    cameraDict.setValue(KeyPosition, cameraPosition);
    cameraDict.setValue(KeyRotation, cameraRotation);
    cameraDict.setValue(KeyAnchor, _orbitalNavigator->anchorNode()->identifier());
    if (_orbitalNavigator->aimNode()) {
        cameraDict.setValue(KeyAim, _orbitalNavigator->aimNode()->identifier());
    }

    return cameraDict;
}

void NavigationHandler::saveCameraStateToFile(const std::string& filepath) {
    if (!filepath.empty()) {
        std::string fullpath = absPath(filepath);
        LINFO(fmt::format("Saving camera position: {}", filepath));

        ghoul::Dictionary cameraDict = cameraStateDictionary();

        // TODO(abock): Should get the camera state as a dictionary and save the
        // dictionary to a file in form of a lua state and not use ofstreams here.

        std::ofstream ofs(fullpath.c_str());

        glm::dvec3 p = _camera->positionVec3();
        glm::dquat q = _camera->rotationQuaternion();

        ofs << "return {" << std::endl;
        ofs << "    " << KeyAnchor << " = " << "\"" <<
            _orbitalNavigator->anchorNode()->identifier() << "\""
            << "," << std::endl;

        if (_orbitalNavigator->aimNode()) {
            ofs << "    " << KeyAim << " = " << "\"" <<
                _orbitalNavigator->aimNode()->identifier() << "\""
                << "," << std::endl;
        }

        ofs << "    " << KeyPosition << " = {"
            << std::to_string(p.x) << ", "
            << std::to_string(p.y) << ", "
            << std::to_string(p.z) << "}," << std::endl;
        ofs << "    " << KeyRotation << " = {"
            << std::to_string(q.w) << ", "
            << std::to_string(q.x) << ", "
            << std::to_string(q.y) << ", "
            << std::to_string(q.z) << "}," << std::endl;
        ofs << "}"<< std::endl;

        ofs.close();
    }
}

void NavigationHandler::restoreCameraStateFromFile(const std::string& filepath) {
    LINFO(fmt::format("Reading camera state from file: {}", filepath));
    if (!FileSys.fileExists(filepath)) {
        throw ghoul::FileNotFoundError(filepath, "CameraFilePath");
    }

    ghoul::Dictionary cameraDict;
    try {
        ghoul::lua::loadDictionaryFromFile(filepath, cameraDict);
        setCameraStateFromDictionary(cameraDict);
        _cameraUpdatedFromScript = true;
    }
    catch (ghoul::RuntimeError& e) {
        LWARNING("Unable to set camera position");
        LWARNING(e.message);
    }
}

void NavigationHandler::setJoystickAxisMapping(int axis,
                                                   JoystickCameraStates::AxisType mapping,
                                            JoystickCameraStates::AxisInvert shouldInvert,
                                      JoystickCameraStates::AxisNormalize shouldNormalize)
{
    _orbitalNavigator->joystickStates().setAxisMapping(
        axis,
        mapping,
        shouldInvert,
        shouldNormalize
    );
}

JoystickCameraStates::AxisInformation
NavigationHandler::joystickAxisMapping(int axis) const
{
    return _orbitalNavigator->joystickStates().axisMapping(axis);
}

void NavigationHandler::setJoystickAxisDeadzone(int axis, float deadzone) {
    _orbitalNavigator->joystickStates().setDeadzone(axis, deadzone);
}

float NavigationHandler::joystickAxisDeadzone(int axis) const {
    return _orbitalNavigator->joystickStates().deadzone(axis);
}

void NavigationHandler::bindJoystickButtonCommand(int button, std::string command,
                                                  JoystickAction action,
                                         JoystickCameraStates::ButtonCommandRemote remote,
                                                  std::string documentation)
{
    _orbitalNavigator->joystickStates().bindButtonCommand(
        button,
        std::move(command),
        action,
        remote,
        std::move(documentation)
    );
}

void NavigationHandler::clearJoystickButtonCommand(int button) {
    _orbitalNavigator->joystickStates().clearButtonCommand(button);
}

std::vector<std::string> NavigationHandler::joystickButtonCommand(int button) const {
    return _orbitalNavigator->joystickStates().buttonCommand(button);
}

scripting::LuaLibrary NavigationHandler::luaLibrary() {
    return {
        "navigation",
        {
            {
                "setCameraState",
                &luascriptfunctions::setCameraState,
                {},
                "object",
                "Set the camera state"
            },
            {
                "saveCameraStateToFile",
                &luascriptfunctions::saveCameraStateToFile,
                {},
                "string",
                "Save the current camera state to file"
            },
            {
                "restoreCameraStateFromFile",
                &luascriptfunctions::restoreCameraStateFromFile,
                {},
                "string",
                "Restore the camera state from file"
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
            }
        }
    };
}

} // namespace openspace::interaction
