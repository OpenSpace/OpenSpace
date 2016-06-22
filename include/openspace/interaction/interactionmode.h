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

#ifndef __INTERACTION_MODE_H__
#define __INTERACTION_MODE_H__

#include <openspace/interaction/keyboardcontroller.h>
#include <openspace/interaction/mousecontroller.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>

#include <modules/globebrowsing/globes/renderableglobe.h>



namespace openspace {

class Camera;
class SceneGraphNode;


namespace interaction {


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
        const std::list<std::pair<Key, KeyModifier> >& getPressedKeys() const;
        const std::list<MouseButton>& getPressedMouseButtons() const;
        glm::dvec2 getMousePosition() const;
        double getMouseScrollDelta() const;
        std::vector<network::datamessagestructures::PositionKeyframe>& getKeyFrames() const;

        bool isKeyPressed(std::pair<Key, KeyModifier> keyModPair) const;
        bool isMouseButtonPressed(MouseButton mouseButton) const;
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
    InteractionMode();
    ~InteractionMode();

    // Mutators
    virtual void setFocusNode(SceneGraphNode* focusNode);

    // Accessors
    SceneGraphNode* focusNode();

    virtual void update(Camera& camera, const InputState& inputState, double deltaTime) = 0;
    virtual void initialize(const Camera& camera) = 0;

protected:
    /**
        Inner class that acts as a smoothing filter to a variable. The filter has a step
        response on a form that resembles the function y = 1-e^(-t/scale). The variable
        will be updates as soon as it is set to a value (calling the set() function).
    */
    template <typename T, typename ScaleType>
    class DelayedVariable {
    public:
        DelayedVariable(ScaleType scale) {
            _scale = scale;
        }
        void set(T value, double dt) {
            _targetValue = value;
            _currentValue = _currentValue + (_targetValue - _currentValue) *
                min(_scale * dt, 1.0); // less or equal to 1.0 keeps it stable
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
        DelayedVariable<glm::dvec2, double> velocity;
    };

    SceneGraphNode* _focusNode = nullptr;
};

class KeyframeInteractionMode : public InteractionMode
{
public:
    KeyframeInteractionMode();
    ~KeyframeInteractionMode();

    virtual void update(double deltaTime);
    virtual void initialize(const Camera& camera);

private:
    double _currentKeyframeTime;
};

class OrbitalInteractionMode : public InteractionMode
{
public:
    /*!
        \param inputState
        \param sensitivity
        \param velocityScalefactor can be set to 60 to remove the inertia of the
            interaction. Lower value will make it harder to move the camera.
    */
    OrbitalInteractionMode(double sensitivity, double velocityScaleFactor);
    ~OrbitalInteractionMode();

    virtual void update(Camera& camera, const InputState& inputState, double deltaTime);
    virtual void initialize(const Camera& camera);

protected:
    void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
    void updateCameraStateFromMouseStates(Camera& camera);

    double _sensitivity;

    MouseState _globalRotationMouseState;
    MouseState _localRotationMouseState;
    MouseState _truckMovementMouseState;
    MouseState _rollMouseState;

    glm::dquat _localCameraRotation;
    glm::dquat _globalCameraRotation;
};

class GlobeBrowsingInteractionMode : public OrbitalInteractionMode
{
public:
    GlobeBrowsingInteractionMode(double sensitivity, double velocityScaleFactor);
    ~GlobeBrowsingInteractionMode();

    virtual void setFocusNode(SceneGraphNode* focusNode);
    virtual void update(Camera& camera, const InputState& inputState, double deltaTime);
private:
    void updateCameraStateFromMouseStates(Camera& camera);
    RenderableGlobe* _globe;
};

} // namespace interaction
} // namespace openspace

#endif // __INTERACTION_MODE_H__
