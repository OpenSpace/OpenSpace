/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/interaction/joystickcamerastates.h>
#include <openspace/interaction/keyboardinputstate.h>
#include <openspace/interaction/mouseinputstate.h>
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
struct NodeCameraStateSpec;
struct WebsocketInputStates;
class KeyframeNavigator;
class OrbitalNavigator;
class PathNavigator;

class NavigationHandler : public properties::PropertyOwner {
public:
    NavigationHandler();
    virtual ~NavigationHandler() override;

    void initialize();
    void deinitialize();

    // Mutators
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);
    void setInterpolationTime(float durationInSeconds);

    void updateCamera(double deltaTime);

    void resetNavigationUpdateVariables();

    // Accessors
    Camera* camera() const;
    const SceneGraphNode* anchorNode() const;
    const MouseInputState& mouseInputState() const;
    const KeyboardInputState& keyboardInputState() const;
    const OrbitalNavigator& orbitalNavigator() const;
    OrbitalNavigator& orbitalNavigator();
    KeyframeNavigator& keyframeNavigator();
    PathNavigator& pathNavigator();

    bool isKeyFrameInteractionEnabled() const;
    float jumpToFadeDuration() const;
    float interpolationTime() const;

    // Callback functions
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);

    bool disabledKeybindings() const;
    bool disabledMouse() const;
    bool disabledJoystick() const;

    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    std::vector<std::string> listAllJoysticks() const;
    void setJoystickAxisMapping(std::string joystickName,
        int axis, JoystickCameraStates::AxisType mapping,
        JoystickCameraStates::AxisInvert shouldInvert =
            JoystickCameraStates::AxisInvert::No,
        JoystickCameraStates::JoystickType joystickType =
            JoystickCameraStates::JoystickType::JoystickLike,
        bool isSticky = false,
        JoystickCameraStates::AxisFlip shouldFlip = JoystickCameraStates::AxisFlip::No,
        double sensitivity = 0.0);

    void setJoystickAxisMappingProperty(std::string joystickName,
        int axis, std::string propertyUri,
        float min = 0.f, float max = 1.f,
        JoystickCameraStates::AxisInvert shouldInvert =
            JoystickCameraStates::AxisInvert::No,
        bool isRemote = true);

    JoystickCameraStates::AxisInformation joystickAxisMapping(
        const std::string& joystickName, int axis) const;

    void setJoystickAxisDeadzone(const std::string& joystickName, int axis,
        float deadzone);
    float joystickAxisDeadzone(const std::string& joystickName, int axis) const;

    void bindJoystickButtonCommand(const std::string& joystickName, int button,
        std::string command, JoystickAction action,
        JoystickCameraStates::ButtonCommandRemote remote, std::string documentation);

    void clearJoystickButtonCommand(const std::string& joystickName, int button);
    std::vector<std::string> joystickButtonCommand(const std::string& joystickName,
        int button) const;

    // Websockets
    void setWebsocketAxisMapping(int axis, WebsocketCameraStates::AxisType mapping,
        WebsocketCameraStates::AxisInvert shouldInvert =
        WebsocketCameraStates::AxisInvert::No,
        WebsocketCameraStates::AxisNormalize shouldNormalize =
        WebsocketCameraStates::AxisNormalize::No);

    NavigationState navigationState() const;
    NavigationState navigationState(const SceneGraphNode& referenceFrame) const;

    void saveNavigationState(const std::filesystem::path& filepath,
        const std::string& referenceFrameIdentifier) const;

    void loadNavigationState(const std::string& filepath, bool useTimeStamp);

    /**
     * Set camera state from a provided navigation state next frame. The actual position
     * will computed from the scene in the same frame as it is set.
     *
     * \param state The navigation state to compute a camera positon from
     */
    void setNavigationStateNextFrame(const NavigationState& state);

    /**
     * Set camera state from a provided node based camera specification structure, next
     * frame. The camera position will be computed to look at the node provided in the
     * node info. The actual position will computed from the scene in the same frame as
     * it is set.
     *
     * \param spec The node specification from which to compute the resulting camera pose
     */
    void setCameraFromNodeSpecNextFrame(NodeCameraStateSpec spec);

    /**
     * Trigger a transition script after first fading out the rendering, and fading in
     * the rendering when the script is finished. One example use case could be to fade
     * out, move the camera to another focus node, and then fade in
     *
     * \param transitionScript The Lua script to handle the transition. Can be anything
     * \param fadeDuration An optional duration for the fading. If unspecified, use the
     *                     JumpToFadeDuration property
     */
    void triggerFadeToTransition(const std::string& transitionScript,
        std::optional<float> fadeDuration = std::nullopt);

    /**
     * \return The Lua library that contains all Lua functions available to affect the
     *         interaction
     */
    static scripting::LuaLibrary luaLibrary();

private:
    void applyPendingState();
    void updateCameraTransitions();
    void clearGlobalJoystickStates();

    MouseInputState _mouseInputState;
    KeyboardInputState _keyboardInputState;
    Camera* _camera = nullptr;
    std::function<void()> _playbackEndCallback;

    bool _inAnchorApproachSphere = false;
    bool _inAnchorReachSphere = false;
    const SceneGraphNode* _lastAnchor = nullptr;

    OrbitalNavigator _orbitalNavigator;
    KeyframeNavigator _keyframeNavigator;
    PathNavigator _pathNavigator;

    std::optional<std::variant<NodeCameraStateSpec, NavigationState>> _pendingState;

    properties::BoolProperty _disableKeybindings;
    properties::BoolProperty _disableMouseInputs;
    properties::BoolProperty _disableJoystickInputs;
    properties::BoolProperty _useKeyFrameInteraction;
    properties::FloatProperty _jumpToFadeDuration;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___NAVIGATIONHANDLER___H__
