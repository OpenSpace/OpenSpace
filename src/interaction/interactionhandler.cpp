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
#include <openspace/interaction/interactionmode.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>


#include <glm/gtx/quaternion.hpp>

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
    const char* HandlebarsFilename = "${OPENSPACE_DATA}/web/common/handlebars-v4.0.5.js";
    const char* JsFilename = "${OPENSPACE_DATA}/web/keybindings/script.js";
    const char* BootstrapFilename = "${OPENSPACE_DATA}/web/common/bootstrap.min.css";
    const char* CssFilename = "${OPENSPACE_DATA}/web/common/style.css";
} // namespace

#include "interactionhandler_lua.inl"

namespace openspace {
namespace interaction {

// InteractionHandler
InteractionHandler::InteractionHandler()
    : properties::PropertyOwner("Interaction")
    , _origin("origin", "Origin", "")
    , _rotationalFriction("rotationalFriction", "Rotational Friction", true)
    , _horizontalFriction("horizontalFriction", "Horizontal Friction", true)
    , _verticalFriction("verticalFriction", "Vertical Friction", true)
    , _sensitivity("sensitivity", "Sensitivity", 0.5f, 0.001f, 1.f)
    , _rapidness("rapidness", "Rapidness", 1.f, 0.1f, 60.f)
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

    // Create the interactionModes
    _inputState = std::make_unique<InputState>();
    // Inject the same mouse states to both orbital and global interaction mode
    _mouseStates = std::make_unique<OrbitalInteractionMode::MouseStates>(_sensitivity * pow(10.0,-4), 1);
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
        _mouseStates->setSensitivity(_sensitivity * pow(10.0,-4));
    });
    _rapidness.onChange([&]() {
        _mouseStates->setVelocityScaleFactor(_rapidness);
    });

    // Add the properties
    addProperty(_origin);

    addProperty(_rotationalFriction);
    addProperty(_horizontalFriction);
    addProperty(_verticalFriction);
    addProperty(_sensitivity);
    addProperty(_rapidness);
}

InteractionHandler::~InteractionHandler() {

}

void InteractionHandler::initialize() {
    OsEng.parallelConnection().connectionEvent()->subscribe("interactionHandler", "statusChanged", [this]() {
        if (OsEng.parallelConnection().status() == ParallelConnection::Status::ClientWithHost) {
            setInteractionMode("Keyframe");
        } else {
            auto keyframeModeIter = _interactionModes.find("Keyframe");
            if (keyframeModeIter != _interactionModes.end()) {
                if (_currentInteractionMode == keyframeModeIter->second) {
                    setInteractionMode("Orbital");
                }
            }
        }
    });
}

void InteractionHandler::deinitialize() {
    OsEng.parallelConnection().connectionEvent()->unsubscribe("interactionHandler");
}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    _currentInteractionMode->setFocusNode(node);
}

void InteractionHandler::setCamera(Camera* camera) {
    _camera = camera;
}

void InteractionHandler::resetCameraDirection() {
    LINFO("Setting camera direction to point at focus node.");
    _currentInteractionMode->rotateToFocusNodeInterpolator().start();
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
    
void InteractionHandler::goToChunk(int x, int y, int level) {
    std::shared_ptr<GlobeBrowsingInteractionMode> gbim =
        std::dynamic_pointer_cast<GlobeBrowsingInteractionMode> (_currentInteractionMode);
    
    if (gbim) {
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
        gbim->goToChunk(*_camera, globebrowsing::TileIndex(x,y,level), glm::vec2(0.5,0.5), true);
#endif
    } else {
        LWARNING("Interaction mode must be set to 'GlobeBrowsing'");
    }
}

void InteractionHandler::goToGeo(double latitude, double longitude) {
    std::shared_ptr<GlobeBrowsingInteractionMode> gbim =
    std::dynamic_pointer_cast<GlobeBrowsingInteractionMode> (_currentInteractionMode);
        
    if (gbim) {
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
        gbim->goToGeodetic2(
            *_camera,
            globebrowsing::Geodetic2(latitude, longitude) / 180 * glm::pi<double>(), true
        );
#endif
    } else {
        LWARNING("Interaction mode must be set to 'GlobeBrowsing'");
    }
}

void InteractionHandler::lockControls() {

}

void InteractionHandler::unlockControls() {

}

void InteractionHandler::updateInputStates(double timeSinceLastUpdate) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");
    _currentInteractionMode->updateMouseStatesFromInput(*_inputState, timeSinceLastUpdate);
}

void InteractionHandler::updateCamera(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        if (_camera && focusNode()) {
            _currentInteractionMode->updateCameraStateFromMouseStates(*_camera, deltaTime);
            _camera->setFocusPositionVec3(focusNode()->worldPosition());
        }
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

    
void InteractionHandler::writeKeyboardDocumentation(const std::string& type,
                                                    const std::string& file)
{
    if (type == "text") {
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(absPath(file));
        
        for (const auto& p : _keyLua) {
            std::string remoteScriptingInfo;
            bool remoteScripting = p.second.synchronization;

            if (!remoteScripting) {
                remoteScriptingInfo = " (LOCAL)";
            }
            f << std::to_string(p.first) << ": "
              << p.second.command << remoteScriptingInfo << '\n'
              << p.second.documentation << '\n';
        }
    }
    else if (type == "html") {
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(absPath(file));

        std::ifstream handlebarsInput(absPath(HandlebarsFilename));
        std::ifstream jsInput(absPath(JsFilename));

        std::string jsContent;
        std::back_insert_iterator<std::string> jsInserter(jsContent);

        std::copy(std::istreambuf_iterator<char>{handlebarsInput}, std::istreambuf_iterator<char>(), jsInserter);
        std::copy(std::istreambuf_iterator<char>{jsInput}, std::istreambuf_iterator<char>(), jsInserter);

        std::ifstream bootstrapInput(absPath(BootstrapFilename));
        std::ifstream cssInput(absPath(CssFilename));

        std::string cssContent;
        std::back_insert_iterator<std::string> cssInserter(cssContent);

        std::copy(std::istreambuf_iterator<char>{bootstrapInput}, std::istreambuf_iterator<char>(), cssInserter);
        std::copy(std::istreambuf_iterator<char>{cssInput}, std::istreambuf_iterator<char>(), cssInserter);

        std::ifstream mainTemplateInput(absPath(MainTemplateFilename));
        std::string mainTemplateContent{ std::istreambuf_iterator<char>{mainTemplateInput},
            std::istreambuf_iterator<char>{} };

        std::ifstream keybindingTemplateInput(absPath(KeybindingTemplateFilename));
        std::string keybindingTemplateContent{ std::istreambuf_iterator<char>{keybindingTemplateInput},
            std::istreambuf_iterator<char>{} };

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


        std::stringstream html;
        html << "<!DOCTYPE html>\n"
            << "<html>\n"
            << "\t<head>\n"
            << "\t\t<script id=\"mainTemplate\" type=\"text/x-handlebars-template\">\n"
            << mainTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t\t<script id=\"keybindingTemplate\" type=\"text/x-handlebars-template\">\n"
            << keybindingTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t<script>\n"
            << "var keybindings = JSON.parse('" << jsonString << "');\n"
            << "var version = [" << OPENSPACE_VERSION_MAJOR << ", " << OPENSPACE_VERSION_MINOR << ", " << OPENSPACE_VERSION_PATCH << "];\n"
            << "var generationTime = '" << Time::now().ISO8601() << "';\n"
            << jsContent << "\n"
            << "\t</script>\n"
            << "\t<style type=\"text/css\">\n"
            << cssContent << "\n"
            << "\t</style>\n"
            << "\t\t<title>Documentation</title>\n"
            << "\t</head>\n"
            << "\t<body>\n"
            << "\t<body>\n"
            << "</html>\n";

        f << html.str();


        /*
        for (const auto& p : _keyLua) {
            html << "\t\t<tr>\n"
                 << "\t\t\t<td>" << std::to_string(p.first) << "</td>\n"
                 << "\t\t\t<td>" << p.second.first << "</td>\n"
                 << "\t\t\t<td>" << (p.second.second ? "Yes" : "No") << "</td>\n"
                 << "\t\t</tr>\n";
        }*/
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

void InteractionHandler::addKeyframe(const datamessagestructures::CameraKeyframe &kf) {
    _inputState->addKeyframe(kf);
}

void InteractionHandler::clearKeyframes() {
    _inputState->clearKeyframes();
}

} // namespace interaction
} // namespace openspace
