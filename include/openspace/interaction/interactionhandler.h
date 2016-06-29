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
#include <openspace/network/parallelconnection.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>

#include <list>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#endif

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


class InputState
{
public:
    InputState();
    ~InputState();

    // Callback functions
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double mouseX, double mouseY);
    void mouseScrollWheelCallback(double mouseScrollDelta);

    // Mutators
    void addKeyframe(const network::datamessagestructures::PositionKeyframe &kf);
    void clearKeyframes();

    // Accessors
    const std::list<std::pair<Key, KeyModifier> >& getPressedKeys();
    const std::list<MouseButton>& getPressedMouseButtons();
    glm::dvec2 getMousePosition();
    double getMouseScrollDelta();
    std::vector<network::datamessagestructures::PositionKeyframe>& getKeyFrames();

    bool isKeyPressed(std::pair<Key, KeyModifier> keyModPair);
    bool isMouseButtonPressed(MouseButton mouseButton);
private:
    // Input from keyboard and mouse
    std::list<std::pair<Key, KeyModifier> > _keysDown;
    std::list<MouseButton> _mouseButtonsDown;
    glm::dvec2 _mousePosition;
    double _mouseScrollDelta;

    // Remote input via keyframes
    std::vector<network::datamessagestructures::PositionKeyframe> _keyframes;
    std::mutex _keyframeMutex;
};

class InteractionMode
{
public:
    InteractionMode(std::shared_ptr<InputState> inputState);
    ~InteractionMode();

    // Mutators
    virtual void setFocusNode(SceneGraphNode* focusNode);
    void setCamera(Camera* camera);

    // Accessors
    SceneGraphNode* focusNode();
    Camera* camera();

    virtual void update(double deltaTime) = 0;
protected:
    /**
        Inner class that acts as a smoothing filter to a variable. The filter has a step
        response on a form that resembles the function y = 1-e^(-t/scale). The variable
        will be updates as soon as it is set to a value (calling the set() function).
    */
    template <typename T, typename ScaleType>
    class delayedVariable {
    public:
        delayedVariable(ScaleType scale) {
            _scale = scale;
        }
        void set(T value) {
            _targetValue = value;
            _currentValue = _currentValue + (_targetValue - _currentValue) * _scale;
        }
        T get() {
            return _currentValue;
        }
    private:
        ScaleType _scale;
        T _targetValue;
        T _currentValue;
    };

    struct MouseState {
        MouseState(double scale)
            : velocity(scale)
            , previousPosition(0.0, 0.0) {}
        glm::dvec2 previousPosition;
        delayedVariable<glm::dvec2, double> velocity;
    };

    std::shared_ptr<InputState> _inputState;
    SceneGraphNode* _focusNode;
    Camera* _camera;
};

class KeyframeInteractionMode : public InteractionMode
{
public:
    KeyframeInteractionMode(std::shared_ptr<InputState> inputState);
    ~KeyframeInteractionMode();

    virtual void update(double deltaTime);
private:
    double _currentKeyframeTime;
};

class OrbitalInteractionMode : public InteractionMode
{
public:
    OrbitalInteractionMode(
        std::shared_ptr<InputState> inputState,
        double sensitivity,
        double velocityScaleFactor);
    ~OrbitalInteractionMode();

    virtual void update(double deltaTime);

    void setRotationalFriction(bool friction);
    void setZoomFriction(bool friction);

protected:
    void updateMouseStatesFromInput(double deltaTime);
    void updateCameraStateFromMouseStates();

    double _sensitivity;

    MouseState _globalRotationMouseState;
    MouseState _localRotationMouseState;
    MouseState _truckMovementMouseState;
    MouseState _rollMouseState;

    glm::dquat _localCameraRotation;
    glm::dquat _globalCameraRotation;

    bool _rotationalFriction;
    bool _zoomFriction;
};

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
class GlobeBrowsingInteractionMode : public OrbitalInteractionMode
{
public:
    GlobeBrowsingInteractionMode(
        std::shared_ptr<InputState> inputState,
        double sensitivity,
        double velocityScaleFactor);
    ~GlobeBrowsingInteractionMode();

    virtual void setFocusNode(SceneGraphNode* focusNode);
    virtual void update(double deltaTime);
private:
    void updateCameraStateFromMouseStates();
    RenderableGlobe* _globe;
};
#endif


class InteractionHandler : public properties::PropertyOwner
{
public:
    InteractionHandler();
    ~InteractionHandler();

    // Mutators
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);

    // Interaction mode setters
    void setInteractionModeToOrbital();
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    void setInteractionModeToGlobeBrowsing();
#endif

    void resetKeyBindings();

    void addKeyframe(const network::datamessagestructures::PositionKeyframe &kf);
    void clearKeyframes();

    void bindKey(Key key, KeyModifier modifier, std::string lua);
    void lockControls();
    void unlockControls();

    void update(double deltaTime);

    // Accessors
    SceneGraphNode* const focusNode() const;
    Camera* const camera() const;
    std::shared_ptr<InputState> inputState() const;

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

private:
    void setInteractionMode(std::shared_ptr<InteractionMode> interactionMode);

    std::multimap<KeyWithModifier, std::string > _keyLua;

    std::shared_ptr<InputState> _inputState;

    std::shared_ptr<InteractionMode> _currentInteractionMode;

    std::shared_ptr<OrbitalInteractionMode> _orbitalInteractionMode;

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    std::shared_ptr<GlobeBrowsingInteractionMode> _globebrowsingInteractionMode;
#endif

    // Properties
    properties::StringProperty _origin;
    properties::StringProperty _coordinateSystem;

    properties::BoolProperty _rotationalFriction;
    properties::BoolProperty _zoomFriction;
};

#endif // USE_OLD_INTERACTIONHANDLER

} // namespace interaction
} // namespace openspace

#endif // __INTERACTIONHANDLER_H__
