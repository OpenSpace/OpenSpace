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
protected:
    /**
        Inner class that acts as a smoothing filter to a variable. The filter has a step
        response on a form that resembles the function y = 1-e^(-t/scale). The variable
        will be updates as soon as it is set to a value (calling the set() function).
    */
    template <typename T, typename ScaleType>
    class DelayedVariable {
    public:
        DelayedVariable(ScaleType scaleFactor, ScaleType friction) {
            _scaleFactor = scaleFactor;
            _friction = glm::max(friction, ScaleType(0.0));
        }
        void set(T value, double dt) {
            _targetValue = value;
            _currentValue = _currentValue + (_targetValue - _currentValue) *
                min(_scaleFactor * dt, 1.0); // less or equal to 1.0 keeps it stable
        }
        void decelerate(double dt) {
            _currentValue = _currentValue + (- _currentValue) *
                min(_friction * dt, 1.0); // less or equal to 1.0 keeps it stable
        }
        void setHard(T value) {
            _targetValue = value;
            _currentValue = value;
        }
        void setFriction(ScaleType friction) {
            _friction = glm::max(friction, ScaleType(0.0));
        }
        void setScaleFactor(ScaleType scaleFactor) {
            _scaleFactor = scaleFactor;
        }
        T get() {
            return _currentValue;
        }
    private:
        ScaleType _scaleFactor;
        ScaleType _friction;
        T _targetValue;
        T _currentValue;
    };

    struct MouseState {
        MouseState(double scaleFactor)
            : velocity(scaleFactor, 1)
            , previousPosition(0.0, 0.0) {}
        void setFriction(double friction) {
            velocity.setFriction(friction);
        }
        void setVelocityScaleFactor(double scaleFactor) {
            velocity.setScaleFactor(scaleFactor);
        }
        glm::dvec2 previousPosition;
        DelayedVariable<glm::dvec2, double> velocity;
    };

    SceneGraphNode* _focusNode = nullptr;
    glm::dvec3 _previousFocusNodePosition;
};

class KeyframeInteractionMode : public InteractionMode
{
public:
    KeyframeInteractionMode();
    ~KeyframeInteractionMode();

    virtual void update(double deltaTime);

private:
    double _currentKeyframeTime;
};

class GlobeBrowsingInteractionMode;

class OrbitalInteractionMode : public InteractionMode
{
public:
    class MouseStates
    {
    public:
        /*!
        \param inputState
        \param sensitivity
        \param velocityScalefactor can be set to 60 to remove the inertia of the
        interaction. Lower value will make it harder to move the camera. 
        */
        MouseStates(double sensitivity, double velocityScaleFactor);
        void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
        void setFriction(double friction);
        void setSensitivity(double sensitivity);
        void setVelocityScaleFactor(double scaleFactor);
    private:
        friend class OrbitalInteractionMode;
        friend class GlobeBrowsingInteractionMode;
        double _sensitivity;

        MouseState _globalRotationMouseState;
        MouseState _localRotationMouseState;
        MouseState _truckMovementMouseState;
        MouseState _localRollMouseState;
        MouseState _globalRollMouseState;
    };

    OrbitalInteractionMode(std::shared_ptr<MouseStates> mouseStates);
    ~OrbitalInteractionMode();

    virtual void update(Camera& camera, const InputState& inputState, double deltaTime);

protected:
    void updateCameraStateFromMouseStates(Camera& camera);
    std::shared_ptr<MouseStates> _mouseStates;
};

class GlobeBrowsingInteractionMode : public OrbitalInteractionMode
{
public:
    GlobeBrowsingInteractionMode(std::shared_ptr<MouseStates> mouseStates);
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
