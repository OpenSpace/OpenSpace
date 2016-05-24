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
#include <openspace/util/keys.h>

#include <mutex>

namespace openspace {

class Camera;
class SceneGraphNode;

namespace interaction {

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

#endif // FALSE


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

    // Accessors
    const std::list<std::pair<Key, KeyModifier> >& getPressedKeys();
    const std::list<MouseButton>& getPressedMouseButtons();
    glm::dvec2 getMousePosition();
    double getMouseScrollDelta();

    bool isKeyPressed(std::pair<Key, KeyModifier> keyModPair);
    bool isMouseButtonPressed(MouseButton mouseButton);
private:
    std::list<std::pair<Key, KeyModifier> > _keysDown;
    std::list<MouseButton> _mouseButtonsDown;
    glm::dvec2 _mousePosition;
    double _mouseScrollDelta;
};



class InteractionMode
{
public:
    InteractionMode(std::shared_ptr<InputState> inputState);
    ~InteractionMode();

    // Mutators
    void setFocusNode(SceneGraphNode* focusNode);
    void setCamera(Camera* camera);

    virtual void update(double deltaTime) = 0;
protected:

    template <typename T>
    T delay(T in, T target, double scale) {
        return in + (target - in) * scale;
    }

    std::shared_ptr<InputState> _inputState;
    SceneGraphNode* _focusNode;
    Camera* _camera;
};

class OrbitalInteractionMode : public InteractionMode
{
public:
    OrbitalInteractionMode(std::shared_ptr<InputState> inputState);
    ~OrbitalInteractionMode();

    virtual void update(double deltaTime);
private:
    glm::dvec2 _previousMousePositionGlobalRotation;
    glm::dvec2 _mouseVelocityTargetGlobalRotation;
    glm::dvec2 _mouseVelocityGlobalRotation;

    glm::dvec2 _previousMousePositionLocalRotation;
    glm::dvec2 _mouseVelocityTargetLocalRotation;
    glm::dvec2 _mouseVelocityLocalRotation;

    glm::dvec2 _previousMousePositionMove;
    glm::dvec2 _mouseVelocityTargetMove;
    glm::dvec2 _mouseVelocityMove;

    glm::dvec2 _previousMousePositionRoll;
    glm::dvec2 _mouseVelocityTargetRoll;
    glm::dvec2 _mouseVelocityRoll;

    glm::dquat _localCameraRotation;
    glm::dquat _globalCameraRotation;
};


class InteractionHandler : public properties::PropertyOwner
{
public:
    InteractionHandler();
    ~InteractionHandler();

    // Mutators
    void setKeyboardController(KeyboardController* controller);
    void setMouseController(MouseController* controller);
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);

    void resetKeyBindings();

    void addKeyframe(const network::datamessagestructures::PositionKeyframe &kf);
    void clearKeyframes();

    void bindKey(Key key, KeyModifier modifier, std::string lua);
    void lockControls();
    void unlockControls();

    void update(double deltaTime);

    // Accessors
    const SceneGraphNode* const focusNode() const;
    const Camera* const camera() const;
    //double deltaTime() const;

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
    std::shared_ptr<InteractionMode> _interactor;
    std::shared_ptr<InputState> _inputState;
};

} // namespace interaction
} // namespace openspace

#endif // __INTERACTIONHANDLER_H__
