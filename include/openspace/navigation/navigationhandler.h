/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___NAVIGATIONHANDLER___H__
#define __OPENSPACE_CORE___NAVIGATIONHANDLER___H__

#include <openspace/documentation/documentation.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/joystickcamerastates.h>
#include <openspace/interaction/websocketcamerastates.h>
#include <openspace/navigation/keyframenavigator.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scene/profile.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>
#include <optional>

namespace openspace {
    class Camera;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::scripting { struct LuaLibrary; }

namespace openspace::interaction {

struct JoystickInputStates;
struct NavigationState;
struct WebsocketInputStates;
class KeyframeNavigator;
class OrbitalNavigator;
class PathNavigator;

class NavigationHandler : public properties::PropertyOwner {
public:
    NavigationHandler();
    ~NavigationHandler();

    void initialize();
    void deinitialize();

    // Mutators
    void setFocusNode(SceneGraphNode* node);
    void resetCameraDirection();

    void setCamera(Camera* camera);
    void setInterpolationTime(float durationInSeconds);

    void updateCamera(double deltaTime);
    void setEnableKeyFrameInteraction();
    void setDisableKeyFrameInteraction();
    void triggerPlaybackStart();
    void stopPlayback();

    // Accessors
    Camera* camera() const;
    const SceneGraphNode* anchorNode() const;
    const InputState& inputState() const;
    const OrbitalNavigator& orbitalNavigator() const;
    OrbitalNavigator& orbitalNavigator();
    KeyframeNavigator& keyframeNavigator();
    PathNavigator& pathNavigator();
    bool isKeyFrameInteractionEnabled() const;
    float interpolationTime() const;

    // Callback functions
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);

    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    void setJoystickAxisMapping(int axis, JoystickCameraStates::AxisType mapping,
        JoystickCameraStates::AxisInvert shouldInvert =
            JoystickCameraStates::AxisInvert::No,
        JoystickCameraStates::AxisNormalize shouldNormalize =
            JoystickCameraStates::AxisNormalize::No,
        bool isSticky = false, double sensitivity = 0.0
    );

    JoystickCameraStates::AxisInformation joystickAxisMapping(int axis) const;

    void setJoystickAxisDeadzone(int axis, float deadzone);
    float joystickAxisDeadzone(int axis) const;

    void bindJoystickButtonCommand(int button, std::string command, JoystickAction action,
        JoystickCameraStates::ButtonCommandRemote remote, std::string documentation);

    void clearJoystickButtonCommand(int button);
    std::vector<std::string> joystickButtonCommand(int button) const;

    // Websockets
    void setWebsocketAxisMapping(int axis, WebsocketCameraStates::AxisType mapping,
        WebsocketCameraStates::AxisInvert shouldInvert =
        WebsocketCameraStates::AxisInvert::No,
        WebsocketCameraStates::AxisNormalize shouldNormalize =
        WebsocketCameraStates::AxisNormalize::No);

    NavigationState navigationState() const;
    NavigationState navigationState(const SceneGraphNode& referenceFrame) const;

    void saveNavigationState(const std::string& filepath,
        const std::string& referenceFrameIdentifier);

    void loadNavigationState(const std::string& filepath);

    void setNavigationStateNextFrame(NavigationState state);

    /**
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static scripting::LuaLibrary luaLibrary();

private:
    void applyNavigationState(const NavigationState& ns);

    bool _playbackModeEnabled = false;

    InputState _inputState;
    Camera* _camera = nullptr;
    std::function<void()> _playbackEndCallback;

    OrbitalNavigator _orbitalNavigator;
    KeyframeNavigator _keyframeNavigator;
    PathNavigator _pathNavigator;

    std::optional<NavigationState> _pendingNavigationState;

    properties::BoolProperty _disableMouseInputs;
    properties::BoolProperty _disableJoystickInputs;
    properties::BoolProperty _useKeyFrameInteraction;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___NAVIGATIONHANDLER___H__
