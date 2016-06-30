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
#include <modules/globebrowsing/globes/renderableglobe.h>

#include <mutex>

namespace openspace {

class Camera;
class SceneGraphNode;

namespace interaction {


//#define USE_OLD_INTERACTIONHANDLER
#ifdef USE_OLD_INTERACTIONHANDLER

class InteractionHandler : public properties::PropertyOwner {
public:
    InteractionHandler();
    ~InteractionHandler();

    // Mutators
    void setKeyboardController(KeyboardController* controller);
    void setMouseController(MouseController* controller);
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);
    void setInteractionSensitivity(float sensitivity);
    void resetKeyBindings();
    void setInvertRoll(bool invert);
    void setInvertRotation(bool invert);

    void addController(Controller* controller);
    void addKeyframe(const network::datamessagestructures::PositionKeyframe &kf);
    void clearKeyframes();

    void bindKey(Key key, KeyModifier modifier, std::string lua);

    void lockControls();
    void unlockControls();

    void update(double deltaTime);
    
    // Accessors
    const SceneGraphNode* const focusNode() const;
    const Camera* const camera() const;
    double deltaTime() const;
    float interactionSensitivity() const;
    bool invertRoll() const;
    bool invertRotation() const;

    /**
    * Returns the Lua library that contains all Lua functions available to affect the
    * interaction. The functions contained are
    * - openspace::luascriptfunctions::setOrigin
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static scripting::ScriptEngine::LuaLibrary luaLibrary();


    // Callback functions
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    // Interaction functions
    void orbitDelta(const glm::quat& rotation);
    void orbit(const float &dx, const float &dy, const float &dz, const float &dist);
    void rotateDelta(const glm::quat& rotation);
    void distanceDelta(const PowerScaledScalar& distance, size_t iterations = 0);
    void lookAt(const glm::quat& rotation);
    void setRotation(const glm::quat& rotation);

private:
    // Remove copy and move constructors
    InteractionHandler(const InteractionHandler&) = delete;
    InteractionHandler& operator=(const InteractionHandler&) = delete;
    InteractionHandler(InteractionHandler&&) = delete;
    InteractionHandler& operator=(InteractionHandler&&) = delete;

    // Settings
    float _controllerSensitivity;
    bool _invertRoll;
    bool _invertRotation;

    // Pointers to entities to affect
    Camera* _camera;
    SceneGraphNode* _focusNode;

    // Cached data
    double _deltaTime;
    std::mutex _mutex;

    //bool _validKeyLua;
    std::multimap<KeyWithModifier, std::string > _keyLua;

    
    KeyboardController* _keyboardController;
    MouseController* _mouseController;
    std::vector<Controller*> _controllers;

    properties::StringProperty _origin;
    properties::StringProperty _coordinateSystem;
    
    //remote controller
    std::vector<network::datamessagestructures::PositionKeyframe> _keyframes;
    double _currentKeyframeTime;
    std::mutex _keyframeMutex;
};

#else // USE_OLD_INTERACTIONHANDLER

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
    void setInteractionModeToOrbital();
    void setInteractionModeToGlobeBrowsing();

    void resetKeyBindings();

    void addKeyframe(const network::datamessagestructures::PositionKeyframe &kf);
    void clearKeyframes();

    void bindKey(Key key, KeyModifier modifier, std::string lua);
    void lockControls();
    void unlockControls();

    void update(double deltaTime);

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
    static scripting::ScriptEngine::LuaLibrary luaLibrary();

    // Callback functions 
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    void saveCameraStateToFile(const std::string& filepath);
    void restoreCameraStateFromFile(const std::string& filepath);

private:
    void setInteractionMode(std::shared_ptr<InteractionMode> interactionMode);

    bool _cameraUpdatedFromScript = false;

    std::multimap<KeyWithModifier, std::string > _keyLua;

    std::unique_ptr<InputState> _inputState;
    Camera* _camera;

    std::shared_ptr<InteractionMode> _currentInteractionMode;

    std::shared_ptr<OrbitalInteractionMode::MouseStates> _mouseStates;
    std::shared_ptr<OrbitalInteractionMode> _orbitalInteractionMode;
    std::shared_ptr<GlobeBrowsingInteractionMode> _globebrowsingInteractionMode;

    // Properties
    properties::StringProperty _origin;
    properties::StringProperty _coordinateSystem;

    properties::FloatProperty _rotationalFriction;
    properties::FloatProperty _horizontalFriction;
    properties::FloatProperty _verticalFriction;

    properties::FloatProperty _sensitivity;
    properties::FloatProperty _rapidness;
};

#endif // USE_OLD_INTERACTIONHANDLER

} // namespace interaction
} // namespace openspace

#endif // __INTERACTIONHANDLER_H__
