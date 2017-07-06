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

#include <openspace/interaction/interactionhandler.h>
#include <openspace/network/parallelconnection.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/string_cast.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif

#include <ghoul/glm.h>

#include <fstream>

namespace {
    const char* _loggerCat = "InteractionHandler";

    const char* KeyFocus = "Focus";
    const char* KeyPosition = "Position";
    const char* KeyRotation = "Rotation";

    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/keybindings/main.hbs";
    const char* KeybindingTemplateFilename = "${OPENSPACE_DATA}/web/keybindings/keybinding.hbs";
    const char* JsFilename = "${OPENSPACE_DATA}/web/keybindings/script.js";
} // namespace

#include "interactionhandler_lua.inl"

namespace openspace {
namespace interaction {

// InteractionHandler
InteractionHandler::InteractionHandler()
    : properties::PropertyOwner("Interaction")
    , DocumentationGenerator(
        "Documentation",
        "keybindings",
        {
            { "keybindingTemplate",  KeybindingTemplateFilename },
            { "mainTemplate", MainTemplateFilename }
        },
        JsFilename
    )
    , _origin("origin", "Origin", "")
    , _useKeyFrameInteraction("useKeyFrameInteraction", "Use keyframe interaction", false)
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
    _keyframeInteractionMode = std::make_unique<KeyframeInteractionMode>();
    
    // Add the properties
    addProperty(_origin);
    addProperty(_useKeyFrameInteraction);
	addPropertySubOwner(*_orbitalNavigator);
}

InteractionHandler::~InteractionHandler() {

}

void InteractionHandler::initialize() {
    OsEng.parallelConnection().connectionEvent()->subscribe("interactionHandler", "statusChanged", [this]() {
        if (OsEng.parallelConnection().status() == ParallelConnection::Status::ClientWithHost) {
            _useKeyFrameInteraction = true;
        }
    });
}

void InteractionHandler::deinitialize() {
    OsEng.parallelConnection().connectionEvent()->unsubscribe("interactionHandler");
}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    _orbitalNavigator->setFocusNode(node);
}

void InteractionHandler::setCamera(Camera* camera) {
    _camera = camera;
    setFocusNode(_camera->parent());
}

void InteractionHandler::resetCameraDirection() {
    LINFO("Setting camera direction to point at focus node.");
	_orbitalNavigator->startInterpolateCameraDirection(*_camera);
}

const OrbitalNavigator& InteractionHandler::orbitalNavigator() const {
    return *_orbitalNavigator;
}

void InteractionHandler::goToChunk(int x, int y, int level) {
    LWARNING("Interaction mode must be set to 'GlobeBrowsing'");
}

void InteractionHandler::goToGeo(double latitude, double longitude) {
    LWARNING("Interaction mode must be set to 'GlobeBrowsing'");
}

void InteractionHandler::lockControls() {

}

void InteractionHandler::unlockControls() {

}

void InteractionHandler::updateInputStates(double timeSinceLastUpdate) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");
    _orbitalNavigator->updateMouseStatesFromInput(*_inputState, timeSinceLastUpdate);
}

void InteractionHandler::updateCamera(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        if (_camera && focusNode()) {
            if (_useKeyFrameInteraction) {
                _keyframeInteractionMode->updateCameraStateFromMouseStates(*_camera, deltaTime);
            }
            else {
                _orbitalNavigator->updateCameraStateFromMouseStates(*_camera, deltaTime);    
            }
			_camera->setFocusPositionVec3(focusNode()->worldPosition());
        }
    }
}

SceneGraphNode* InteractionHandler::focusNode() const {
    return _orbitalNavigator->focusNode();
}

glm::dvec3 InteractionHandler::focusNodeToCameraVector() const {
    return _camera->positionVec3() - focusNode()->worldPosition();
}

glm::quat InteractionHandler::focusNodeToCameraRotation() const {
    glm::dmat4 invWorldRotation = glm::inverse(focusNode()->worldRotationMatrix());
    return glm::quat(invWorldRotation) * glm::quat(_camera->rotationQuaternion());
}

Camera* InteractionHandler::camera() const {
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
            auto remote = it->second.synchronization ?
                scripting::ScriptEngine::RemoteScripting::Yes :
                scripting::ScriptEngine::RemoteScripting::No;

            OsEng.scriptEngine().queueScript(it->second.command, remote);
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

void InteractionHandler::bindKeyLocal(Key key, KeyModifier modifier,
                                      std::string luaCommand, std::string documentation)
{
    _keyLua.insert({
        { key, modifier },
        { std::move(luaCommand), Synchronized::No, std::move(documentation) }
    });
}

void InteractionHandler::bindKey(Key key, KeyModifier modifier,
                                 std::string luaCommand, std::string documentation)
{
    _keyLua.insert({
        { key, modifier },
        { std::move(luaCommand), Synchronized::Yes, std::move(documentation) }
    });
}

    
std::string InteractionHandler::generateJson() const {
    std::stringstream json;
    json << "[";
    bool first = true;
    for (const auto& p : _keyLua) {
        if (!first) {
            json << ",";
        }
        first = false;
        json << "{";
        json << "\"key\": \"" << std::to_string(p.first) << "\",";
        json << "\"script\": \"" << p.second.command << "\",";
        json << "\"remoteScripting\": " << (p.second.synchronization ? "true," : "false,");
        json << "\"documentation\": \"" << p.second.documentation << "\"";
        json << "}";
    }
    json << "]";
    
    std::string jsonString = "";
    for (const char& c : json.str()) {
        if (c == '\'') {
            jsonString += "\\'";
        } else {
            jsonString += c;
        }
    }

    return jsonString;
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
                "string, string [,string]",
                "Binds a key by name to a lua string command to execute both locally "
                "and to broadcast to clients if this is the host of a parallel session. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
            },
            {
                "bindKeyLocal",
                &luascriptfunctions::bindKeyLocal,
                "string, string [,string]",
                "Binds a key by name to a lua string command to execute only locally. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
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
            {
                "goToChunk",
                &luascriptfunctions::goToChunk,
                "void",
                "Go to chunk with given index x, y, level"
            },
            {
                "goToGeo",
                &luascriptfunctions::goToGeo,
                "void",
                "Go to geographic coordinates latitude and longitude"
            },
        }
    };
}

void InteractionHandler::addKeyframe(double timestamp, KeyframeInteractionMode::CameraPose pose) {
    if (!_keyframeInteractionMode) {
        return;
    }
    _keyframeInteractionMode->timeline().addKeyframe(timestamp, pose);
}

void InteractionHandler::removeKeyframesAfter(double timestamp) {
    if (!_keyframeInteractionMode) {
        return;
    }
    _keyframeInteractionMode->timeline().removeKeyframesAfter(timestamp);
}

void InteractionHandler::clearKeyframes() {
    if (!_keyframeInteractionMode) {
        return;
    }
    _keyframeInteractionMode->timeline().clearKeyframes();
}

size_t InteractionHandler::nKeyframes() const {
    if (!_keyframeInteractionMode) {
        return 0;
    }
    return _keyframeInteractionMode->timeline().keyframes().size();
}

} // namespace interaction
} // namespace openspace
