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

#ifndef __INTERACTIONHANDLER_H__
#define __INTERACTIONHANDLER_H__

#include <openspace/interaction/keyboardcontroller.h>
#include <openspace/interaction/mousecontroller.h>
#include <openspace/interaction/interactionmode.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>


#include <list>

#include <mutex>

namespace openspace {

class Camera;
class SceneGraphNode;

namespace interaction {


class InteractionHandler : public properties::PropertyOwner
{
public:
    InteractionHandler();
    ~InteractionHandler();

    // Mutators
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);
    void resetCameraDirection();

    // Interaction mode setters
    void setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict);
    void setInteractionMode(const std::string& interactionModeKey);

    void resetKeyBindings();

    void addKeyframe(const network::datamessagestructures::CameraKeyframe &kf);
    void clearKeyframes();

    void bindKey(Key key, KeyModifier modifier, std::string lua);
    void lockControls();
    void unlockControls();

    //void update(double deltaTime);

    void preSynchronization(double deltaTime);
    void postSynchronizationPreDraw();

    void serialize(SyncBuffer* syncBuffer);
    void deserialize(SyncBuffer* syncBuffer);

    // Accessors
    ghoul::Dictionary getCameraStateDictionary();
    SceneGraphNode* const focusNode() const;
    Camera* const camera() const;
    const InputState& inputState() const;

    /**
    * Returns the Lua library that contains all Lua functions available to affect the
    * interaction. The functions contained are
    * - openspace::luascriptfunctions::setOrigin
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static scripting::LuaLibrary luaLibrary();

    // Callback functions 
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    void saveCameraStateToFile(const std::string& filepath);
    void restoreCameraStateFromFile(const std::string& filepath);
    void writeKeyboardDocumentation(const std::string& type, const std::string& file);

private:
    void setInteractionMode(std::shared_ptr<InteractionMode> interactionMode);

    bool _cameraUpdatedFromScript = false;

    std::multimap<KeyWithModifier, std::string > _keyLua;

    std::unique_ptr<InputState> _inputState;
    Camera* _camera;

    std::shared_ptr<InteractionMode> _currentInteractionMode;

    std::map<std::string, std::shared_ptr<InteractionMode>> _interactionModes;
    std::shared_ptr<OrbitalInteractionMode::MouseStates> _mouseStates;

    // Properties
    properties::StringProperty _origin;
    properties::StringProperty _coordinateSystem;

    properties::BoolProperty _rotationalFriction;
    properties::BoolProperty _horizontalFriction;
    properties::BoolProperty _verticalFriction;

    properties::FloatProperty _sensitivity;
    properties::FloatProperty _rapidness;
};

} // namespace interaction
} // namespace openspace

#endif // __INTERACTIONHANDLER_H__
