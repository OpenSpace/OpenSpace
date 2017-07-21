/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <ghoul/glm.h>
#include <glm/gtx/quaternion.hpp>

#include <fstream>

namespace {
    const char* _loggerCat = "NavigationHandler";

    const char* KeyFocus = "Focus";
    const char* KeyPosition = "Position";
    const char* KeyRotation = "Rotation";
} // namespace

#include "navigationhandler_lua.inl"

namespace openspace::interaction {

NavigationHandler::NavigationHandler()
    : properties::PropertyOwner("NavigationHandler")
    , _origin("origin", "Origin", "")
    , _useKeyFrameInteraction("useKeyFrameInteraction", "Use keyframe interaction", "", false) // @TODO Missing documentation
{
    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
            return;
        }
        setFocusNode(node);
        resetCameraDirection();
    });

    _inputState = std::make_unique<InputState>();
    _orbitalNavigator = std::make_unique<OrbitalNavigator>();
    _keyframeNavigator = std::make_unique<KeyframeNavigator>();
    
    // Add the properties
    addProperty(_origin);
    addProperty(_useKeyFrameInteraction);
    addPropertySubOwner(*_orbitalNavigator);
}

NavigationHandler::~NavigationHandler()
{ }

void NavigationHandler::initialize() {
    OsEng.parallelConnection().connectionEvent()->subscribe("NavigationHandler", "statusChanged", [this]() {
    if (OsEng.parallelConnection().status() == ParallelConnection::Status::ClientWithHost) {
            _useKeyFrameInteraction = true;
        }
    });
}

void NavigationHandler::deinitialize() {
    OsEng.parallelConnection().connectionEvent()->unsubscribe("NavigationHandler");
}

void NavigationHandler::setFocusNode(SceneGraphNode* node) {
    _orbitalNavigator->setFocusNode(node);
    _camera->setFocusPositionVec3(focusNode()->worldPosition());
}

void NavigationHandler::setCamera(Camera* camera) {
    _camera = camera;
    //setFocusNode(_camera->parent());
}

void NavigationHandler::resetCameraDirection() {
    LINFO("Setting camera direction to point at focus node.");
    _orbitalNavigator->startInterpolateCameraDirection(*_camera);
}

const OrbitalNavigator& NavigationHandler::orbitalNavigator() const {
    return *_orbitalNavigator;
}

KeyframeNavigator& NavigationHandler::keyframeNavigator() const {
    return *_keyframeNavigator;
}

void NavigationHandler::updateCamera(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        if (_camera && focusNode()) {
            if (_useKeyFrameInteraction) {
                _keyframeNavigator->updateCamera(*_camera);
            }
            else {
                _orbitalNavigator->updateMouseStatesFromInput(*_inputState, deltaTime);
                _orbitalNavigator->updateCameraStateFromMouseStates(*_camera, deltaTime);    
            }
            _camera->setFocusPositionVec3(focusNode()->worldPosition());
        }
    }
}

SceneGraphNode* NavigationHandler::focusNode() const {
    return _orbitalNavigator->focusNode();
}

glm::dvec3 NavigationHandler::focusNodeToCameraVector() const {
    return _camera->positionVec3() - focusNode()->worldPosition();
}

glm::quat NavigationHandler::focusNodeToCameraRotation() const {
    glm::dmat4 invWorldRotation = glm::inverse(focusNode()->worldRotationMatrix());
    return glm::quat(invWorldRotation) * glm::quat(_camera->rotationQuaternion());
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

void NavigationHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    _inputState->keyboardCallback(key, modifier, action);
}

void NavigationHandler::setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict) {
    bool readSuccessful = true;

    std::string focus;
    glm::dvec3 cameraPosition;
    glm::dvec4 cameraRotation; // Need to read the quaternion as a vector first.

    readSuccessful &= cameraDict.getValue(KeyFocus, focus);
    readSuccessful &= cameraDict.getValue(KeyPosition, cameraPosition);
    readSuccessful &= cameraDict.getValue(KeyRotation, cameraRotation);

    if (!readSuccessful) {
        throw ghoul::RuntimeError(
            "Position, Rotation and Focus need to be defined for camera dictionary.");
    }

    SceneGraphNode* node = sceneGraphNode(focus);
    if (!node) {
        throw ghoul::RuntimeError(
            "Could not find a node in scenegraph called '" + focus + "'");
    }

    // Set state
    setFocusNode(node);
    _camera->setPositionVec3(cameraPosition);
    _camera->setRotation(glm::dquat(
        cameraRotation.x, cameraRotation.y, cameraRotation.z, cameraRotation.w));
}

ghoul::Dictionary NavigationHandler::getCameraStateDictionary() {
    glm::dvec3 cameraPosition;
    glm::dquat quat;
    glm::dvec4 cameraRotation;

    cameraPosition = _camera->positionVec3();
    quat = _camera->rotationQuaternion();
    cameraRotation = glm::dvec4(quat.w, quat.x, quat.y, quat.z);

    ghoul::Dictionary cameraDict;
    cameraDict.setValue(KeyPosition, cameraPosition);
    cameraDict.setValue(KeyRotation, cameraRotation);
    cameraDict.setValue(KeyFocus, focusNode()->name());

    return cameraDict;
}

void NavigationHandler::saveCameraStateToFile(const std::string& filepath) {
    if (!filepath.empty()) {
        std::string fullpath = absPath(filepath);
        LINFO("Saving camera position: " << filepath);

        ghoul::Dictionary cameraDict = getCameraStateDictionary();

        // TODO : Should get the camera state as a dictionary and save the dictionary to
        // a file in form of a lua state and not use ofstreams here.
        
        std::ofstream ofs(fullpath.c_str());
        
        glm::dvec3 p = _camera->positionVec3();
        glm::dquat q = _camera->rotationQuaternion();

        ofs << "return {" << std::endl;
        ofs << "    " << KeyFocus << " = " << "\"" << focusNode()->name() << "\"" << "," << std::endl;
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
    LINFO("Reading camera state from file: " << filepath);
    if (!FileSys.fileExists(filepath))
        throw ghoul::FileNotFoundError(filepath, "CameraFilePath");

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

scripting::LuaLibrary NavigationHandler::luaLibrary() {
    return {
        "navigation",
        {
            {
                "saveCameraStateToFile",
                &luascriptfunctions::saveCameraStateToFile,
                "string",
                "Save the current camera state to file"
            },
            {
                "restoreCameraStateFromFile",
                &luascriptfunctions::restoreCameraStateFromFile,
                "string",
                "Restore the camera state from file"
            },
            {
                "resetCameraDirection",
                &luascriptfunctions::resetCameraDirection,
                "void",
                "Reset the camera direction to point at the focus node"
            }
        }
    };
}

} // namespace openspace::interaction
