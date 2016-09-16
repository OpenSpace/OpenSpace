/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/interaction/interactionhandler.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

#include <glm/gtx/quaternion.hpp>

//#include <modules/globebrowsing/globes/renderableglobe.h>

#include <ghoul/glm.h>

#include <fstream>

namespace {
    const std::string _loggerCat = "InteractionHandler";

    const std::string KeyFocus = "Focus";
    const std::string KeyPosition = "Position";
    const std::string KeyRotation = "Rotation";
}

#include "interactionhandler_lua.inl"

namespace openspace {
namespace interaction {

// InteractionHandler
InteractionHandler::InteractionHandler()
    : _origin("origin", "Origin", "")
    , _coordinateSystem("coordinateSystem", "Coordinate System", "")
    , _rotationalFriction("rotationalFriction", "Rotational Friction", true)
    , _horizontalFriction("horizontalFriction", "Horizontal Friction", true)
    , _verticalFriction("verticalFriction", "Vertical Friction", true)
    , _sensitivity("sensitivity", "Sensitivity", 0.002, 0.0001, 0.02)
    , _rapidness("rapidness", "Rapidness", 1, 0.1, 60)
{
    setName("Interaction");

    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
            return;
        }
        setFocusNode(node);
        resetCameraDirection();
    });

    _coordinateSystem.onChange([this]() {
        OsEng.renderEngine().changeViewPoint(_coordinateSystem.value());
    });

    // Create the interactionModes
    _inputState = std::make_unique<InputState>();
    // Inject the same mouse states to both orbital and global interaction mode
    _mouseStates = std::make_unique<OrbitalInteractionMode::MouseStates>(0.002, 1);
    _interactionModes.insert(
        std::pair<std::string, std::shared_ptr<InteractionMode>>(
            "Orbital",
            std::make_shared<OrbitalInteractionMode>(_mouseStates)
            ));
    _interactionModes.insert(
        std::pair<std::string, std::shared_ptr<InteractionMode>>(
            "GlobeBrowsing",
            std::make_shared<GlobeBrowsingInteractionMode>(_mouseStates)
            ));
    _interactionModes.insert(
        std::pair<std::string, std::shared_ptr<InteractionMode>>(
            "Keyframe",
            std::make_shared<KeyframeInteractionMode>()
            ));

    // Set the interactionMode
    _currentInteractionMode = _interactionModes["Orbital"];

    // Define lambda functions for changed properties
    _rotationalFriction.onChange([&]() {
        _mouseStates->setRotationalFriction(_rotationalFriction);
    });
    _horizontalFriction.onChange([&]() {
        _mouseStates->setHorizontalFriction(_horizontalFriction);
    });
    _verticalFriction.onChange([&]() {
        _mouseStates->setVerticalFriction(_verticalFriction);
    });
    _sensitivity.onChange([&]() {
        _mouseStates->setSensitivity(_sensitivity);
    });
    _rapidness.onChange([&]() {
        _mouseStates->setVelocityScaleFactor(_rapidness);
    });

    // Add the properties
    addProperty(_origin);
    addProperty(_coordinateSystem);

    addProperty(_rotationalFriction);
    addProperty(_horizontalFriction);
    addProperty(_verticalFriction);
    addProperty(_sensitivity);
    addProperty(_rapidness);
}

InteractionHandler::~InteractionHandler() {

}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    _currentInteractionMode->setFocusNode(node);
}

void InteractionHandler::setCamera(Camera* camera) {
    _camera = camera;
}

void InteractionHandler::resetCameraDirection() {
    LINFO("Setting camera direction to point at focus node.");

    glm::dquat rotation = _camera->rotationQuaternion();
    glm::dvec3 focusPosition = focusNode()->worldPosition();
    glm::dvec3 cameraPosition = _camera->positionVec3();
    glm::dvec3 lookUpVector = _camera->lookUpVectorWorldSpace();

    glm::dvec3 directionToFocusNode = glm::normalize(focusPosition - cameraPosition);

    // To make sure the lookAt function won't fail
    static const double epsilon = 0.000001;
    if (glm::dot(lookUpVector, directionToFocusNode) > 1.0 - epsilon) {
        // Change the look up vector a little bit
        lookUpVector = glm::normalize(lookUpVector + glm::dvec3(epsilon));
    }

    // Create the rotation to look at  focus node
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        directionToFocusNode,
        lookUpVector);
    glm::dquat rotationLookAtFocusNode = normalize(quat_cast(inverse(lookAtMat)));

    // Update camera Rotation
    _camera->setRotation(rotationLookAtFocusNode);

    // Explicitly synch
    _camera->preSynchronization();
    _camera->postSynchronizationPreDraw();
}

void InteractionHandler::setInteractionMode(std::shared_ptr<InteractionMode> interactionMode) {
    // Focus node is passed over from the previous interaction mode
    SceneGraphNode* focusNode = _currentInteractionMode->focusNode();

    // Set the interaction mode
    _currentInteractionMode = interactionMode;

    // Update the focusnode for the new interaction mode
    _currentInteractionMode->setFocusNode(focusNode);
}

void InteractionHandler::setInteractionMode(const std::string& interactionModeKey) {
    if (_interactionModes.find(interactionModeKey) != _interactionModes.end()) {
        setInteractionMode(_interactionModes[interactionModeKey]);
        LINFO("Interaction mode set to '" << interactionModeKey << "'");
    }
    else {
        std::string listInteractionModes("");
        for (auto pair : _interactionModes) {
            listInteractionModes += "'" + pair.first + "', ";
        }
        LWARNING("'" << interactionModeKey <<
            "' is not a valid interaction mode. Candidates are " << listInteractionModes);
    }
}

void InteractionHandler::lockControls() {

}

void InteractionHandler::unlockControls() {

}

void InteractionHandler::preSynchronization(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        _currentInteractionMode->updateMouseStatesFromInput(*_inputState, deltaTime);
    }
}

void InteractionHandler::postSynchronizationPreDraw() {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        _currentInteractionMode->updateCameraStateFromMouseStates(*_camera);
        _camera->setFocusPositionVec3(focusNode()->worldPosition());
    }
}


SceneGraphNode* const InteractionHandler::focusNode() const {
    return _currentInteractionMode->focusNode();
}

Camera* const InteractionHandler::camera() const {
    return _camera;
}

const InputState& InteractionHandler::inputState() const {
    return *_inputState;
}


void InteractionHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    _inputState->mouseButtonCallback(button, action);
}

void InteractionHandler::mousePositionCallback(double x, double y) {
    _inputState->mousePositionCallback(x, y);
}

void InteractionHandler::mouseScrollWheelCallback(double pos) {
    _inputState->mouseScrollWheelCallback(pos);
}

void InteractionHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    _inputState->keyboardCallback(key, modifier, action);

    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        // iterate over key bindings
        auto ret = _keyLua.equal_range({ key, modifier });
        for (auto it = ret.first; it != ret.second; ++it) {
            OsEng.scriptEngine().queueScript(it->second.first,
                it->second.second ?
                scripting::ScriptEngine::RemoteScripting::Yes :
                scripting::ScriptEngine::RemoteScripting::No);
        }
    }
}

void InteractionHandler::setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict) {
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

    // Explicitly synch
    _camera->preSynchronization();
    _camera->postSynchronizationPreDraw();
}

ghoul::Dictionary InteractionHandler::getCameraStateDictionary() {
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

void InteractionHandler::saveCameraStateToFile(const std::string& filepath) {
    if (!filepath.empty()) {
        auto fullpath = absPath(filepath);
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

void InteractionHandler::restoreCameraStateFromFile(const std::string& filepath) {
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

void InteractionHandler::resetKeyBindings() {
    _keyLua.clear();
}

void InteractionHandler::bindKeyLocal(Key key, KeyModifier modifier, std::string lua) {
    _keyLua.insert({
        { key, modifier },
        std::make_pair(lua, false)
    });
}

void InteractionHandler::bindKey(Key key, KeyModifier modifier, std::string lua) {
    _keyLua.insert({
        { key, modifier },
        std::make_pair(lua, true)
    });
}

    
void InteractionHandler::writeKeyboardDocumentation(const std::string& type, const std::string& file)
{
    if (type == "text") {
        std::ofstream f(absPath(file));
        
        for (const auto& p : _keyLua) {
            std::string remoteScriptingInfo;
            bool remoteScripting = p.second.second;

            if (!remoteScripting) {
                remoteScriptingInfo = " (LOCAL)";
            }
            f << std::to_string(p.first) << ": " <<
                p.second.first << remoteScriptingInfo << std::endl;
        }
    }
    else {
        throw ghoul::RuntimeError(
            "Unsupported keyboard documentation type '" + type + "'",
            "InteractionHandler"
        );
    }
}

scripting::LuaLibrary InteractionHandler::luaLibrary() {
    return{
        "",
        {
            {
                "clearKeys",
                &luascriptfunctions::clearKeys,
                "",
                "Clear all key bindings"
            },
            {
                "bindKey",
                &luascriptfunctions::bindKey,
                "string, string",
                "Binds a key by name to a lua string command to execute both locally "
                "and to broadcast to clients if this is the host of a parallel session"
            },
            {
                "bindKeyLocal",
                &luascriptfunctions::bindKeyLocal,
                "string, string",
                "Binds a key by name to a lua string command to execute only locally"
            },
            {
                "setInteractionMode",
                &luascriptfunctions::setInteractionMode,
                "string",
                "Set the interaction mode for the camera"
            },
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
            },
        }
    };
}

void InteractionHandler::addKeyframe(const network::datamessagestructures::CameraKeyframe &kf) {
    _inputState->addKeyframe(kf);
}

void InteractionHandler::clearKeyframes() {
    _inputState->clearKeyframes();
}

void InteractionHandler::serialize(SyncBuffer* syncBuffer) {
    for (auto var : _interactionModes) {
        var.second->serialize(syncBuffer);
    }
}

void InteractionHandler::deserialize(SyncBuffer* syncBuffer) {
    for (auto var : _interactionModes) {
        var.second->deserialize(syncBuffer);
    }
}

} // namespace interaction
} // namespace openspace
