/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <openspace/util/mouse.h>

namespace {
    using namespace openspace;

    constexpr BoolPropertyInfo InvertMouseButtons = {
        .identifier = "InvertMouseButtons",
        .name = "Invert left and right mouse buttons",
        .description = "If this setting is enabled, the right mouse button is considered "
            "the primary one, rather than the left. This means that behavior usually "
            "mapped to the left mouse button will be mapped to the right mouse button, "
            "and vice versa.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo DisableKeybindingsInfo = {
        .identifier = "DisableKeybindings",
        .name = "Disable all keybindings",
        .description = "Disables all keybindings without removing them. Please note that "
            "this does not apply to the key to open the console.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo DisableMouseInputInfo = {
        .identifier = "DisableMouseInput",
        .name = "Disable all mouse inputs",
        .description = "Disables all mouse inputs and prevents them from affecting the "
            "camera.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo DisableJoystickInputInfo = {
        .identifier = "DisableJoystickInput",
        .name = "Disable all joystick inputs",
        .description = "Disables all joystick inputs and prevents them from affecting "
            "the camera.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo DisableTouchInputInfo = {
        .identifier = "DisableTouchInput",
        .name = "Disable all touch inputs",
        .description = "Disables all touch inputs and prevents them from affecting the "
            "camera.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr IntPropertyInfo MaxDoubleTapTimeInfo = {
        .identifier = "MaxDoubleTapTime",
        .name = "Max time for touch double tap detection (milliseconds)",
        .description = "Max tap delay (in ms) for double tap detection in touch "
            "interaction.",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 300,
            .minMax = { 10, 1000 }
        }
    };

    constexpr PropertyOwner::PropertyOwnerInfo MouseVisualizerInfo = {
        .identifier = "MouseInteractionVisualizer",
        .name = "Mouse Interaction Visualizer",
        .description = "The mouse interaction visualizer shows the distance the mouse "
            "has been moved since it was pressed down."
    };

    constexpr BoolPropertyInfo MouseVisualizerEnabledInfo = {
        .identifier = "Enabled",
        .name = "Enabled",
        .description = "If this setting is enabled, the mouse interaction will be "
            "visualized on the screen by showing the distance the mouse has been moved "
            "since it was pressed down.",
        .defaults = {
            .value = false
        }
    };

    constexpr Vec4PropertyInfo MouseVisualizerColorInfo = {
        .identifier = "Color",
        .name = "Color",
        .description = "The color used to render the line showing the mouse visualizer.",
        .defaults = {
            .value = glm::vec4(1.f),
            .minMax = { glm::vec4(0.f), glm::vec4(1.f) }
        }
    };
} // namespace

namespace openspace {

InteractionHandler::InteractionHandler()
    : PropertyOwner({ "InteractionHandler", "Interaction Handler" })
    , _invertMouseButtons(InvertMouseButtons)
    , _disableKeybindings(DisableKeybindingsInfo)
    , _disableMouseInputs(DisableMouseInputInfo)
    , _disableJoystickInputs(DisableJoystickInputInfo)
    , _disableTouchInputs(DisableTouchInputInfo)
    , _maxDoubleTapTime(MaxDoubleTapTimeInfo)
    , _mouseVisualizer({
        .owner = PropertyOwner(MouseVisualizerInfo),
        .enable = BoolProperty(MouseVisualizerEnabledInfo),
        .color = Vec4Property(MouseVisualizerColorInfo)
    })
{
    addProperty(_invertMouseButtons);

    addProperty(_disableKeybindings);
    addProperty(_disableMouseInputs);

    _disableJoystickInputs.onChange([this]() {
        if (_disableJoystickInputs) {
            clearJoystickStates();
        }
    });
    addProperty(_disableJoystickInputs);

    // TODO: This settings behaves pretty weirdly, as touch interaction will still sort of
    // work through injected mouse events. Consider changing this property (Is it really needed?)
    _disableTouchInputs.onChange([this]() {
        _touchInputState.clearInputs();
    });
    addProperty(_disableTouchInputs);

    _maxDoubleTapTime.onChange([this]() {
       _touchInputState.setMaxDoubleTapTime(_maxDoubleTapTime);
    });
    addProperty(_maxDoubleTapTime);

    addPropertySubOwner(_interactionMonitor);

    addPropertySubOwner(_mouseVisualizer.owner);
    _mouseVisualizer.owner.addProperty(_mouseVisualizer.enable);
    _mouseVisualizer.color.setViewOption(Property::ViewOptions::Color);
    _mouseVisualizer.owner.addProperty(_mouseVisualizer.color);

    addPropertySubOwner(_touchMarkers);
}

InteractionHandler::~InteractionHandler() {}

void InteractionHandler::initialize() {
    _touchInputState.initialize();

    // Set up rendering callbacks for touch markers
    global::callback::initializeGL->push_back([this]() {
        _touchMarkers.initializeGL();
    });

    global::callback::deinitializeGL->push_back([this]() {
        _touchMarkers.deinitializeGL();
    });
}

void InteractionHandler::preSynchronization() {
    _interactionMonitor.updateActivityState();

    // @TODO (2026-03-11 emmbr) Move to orbital or navigation handler? Would be good
    // to not have to deal with the engine mode here too
    OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    if (mode == OpenSpaceEngine::Mode::CameraPath ||
        mode == OpenSpaceEngine::Mode::SessionRecordingPlayback)
    {
        // Reset everything, to avoid problems once we process inputs again
        _touchInputState.clearInputs();
        return;
    }
}

void InteractionHandler::postDraw() {
    // Once the frame is finished, reset the touch interaction information
    _touchInputState.updateLastTouchPoints();
    _touchInputState.clearInputs();
}

const MouseInputState& InteractionHandler::mouseInputState() const {
    return _mouseInputState;
}

const KeyboardInputState& InteractionHandler::keyboardInputState() const {
    return _keyboardInputState;
}

WebsocketInputStates& InteractionHandler::websocketInputStates() {
    return _websocketInputStates;
}

const WebsocketInputStates& InteractionHandler::websocketInputStates() const {
    return _websocketInputStates;
}

JoystickInputStates& InteractionHandler::joystickInputStates() {
    return _joystickInputStates;
}

const JoystickInputStates& InteractionHandler::joystickInputStates() const {
    return _joystickInputStates;
}

TouchInputState& InteractionHandler::touchInputState() {
    return _touchInputState;
}

const TouchInputState& InteractionHandler::touchInputState() const {
    return _touchInputState;
}

bool InteractionHandler::disabledKeybindings() const {
    return _disableKeybindings;
}

bool InteractionHandler::disabledMouse() const {
    return _disableMouseInputs;
}

bool InteractionHandler::disabledJoystick() const {
    return _disableJoystickInputs;
}

bool InteractionHandler::disabledTouch() const {
    return _disableTouchInputs;
}

bool InteractionHandler::isPrimaryMouseButtonInverted() const {
    return _invertMouseButtons;
}

MouseButton InteractionHandler::primaryMouseButton() const {
    return _invertMouseButtons ? MouseButton::Button2 : MouseButton::Button1;
}

MouseButton InteractionHandler::secondaryMouseButton() const {
    return _invertMouseButtons ? MouseButton::Button1 : MouseButton::Button2;
}

bool InteractionHandler::isMouseFirstPress() const {
    return _mouseVisualizer.isMouseFirstPress;
}

glm::vec2 InteractionHandler::mouseClickPosition() const {
    return _mouseVisualizer.clickPosition;
}

void InteractionHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mouseButtonCallback(button, action);

    // The mouse visualizer keeps track of some useful states - let's always update them
    if (action == MouseAction::Press) {
        _mouseVisualizer.isMouseFirstPress = true;
        _mouseVisualizer.isMousePressed = true;
    }
    else if (action == MouseAction::Release) {
        _mouseVisualizer.isMousePressed = false;
        _mouseVisualizer.clickPosition = glm::vec2(0.f);
        _mouseVisualizer.currentPosition = glm::vec2(0.f);
    }

    markInteraction();
}

void InteractionHandler::mousePositionCallback(double x, double y) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mousePositionCallback(x, y);

    if (_mouseVisualizer.isMousePressed) {
        if (_mouseVisualizer.isMouseFirstPress) {
            _mouseVisualizer.clickPosition = glm::vec2(x, y);
            _mouseVisualizer.isMouseFirstPress = false;
        }

        _mouseVisualizer.currentPosition = glm::vec2(x, y);
    }

    markInteraction();
}

void InteractionHandler::mouseScrollWheelCallback(double pos) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mouseScrollWheelCallback(pos);

    markInteraction();
}

void InteractionHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    // There is no need to disable the keyboard callback based on a property as the vast
    // majority of input is coming through Lua scripts anyway which are not blocked here
    _keyboardInputState.keyboardCallback(key, modifier, action);

    markInteraction();
}

void InteractionHandler::touchDetectedCallback(TouchInput i) {
    markInteraction();
    _touchInputState.touchDetectedCallback(i);
}

void InteractionHandler::touchUpdatedCallback(TouchInput i) {
    markInteraction();
    _touchInputState.touchUpdatedCallback(i);
}

void InteractionHandler::touchExitCallback(TouchInput i) {
    markInteraction();
    _touchInputState.touchExitCallback(i);
}

void InteractionHandler::renderOverlay() {
    if (_mouseVisualizer.enable && _mouseVisualizer.isMousePressed) {
        constexpr glm::vec4 StartColor = glm::vec4(0.4f, 0.4f, 0.4f, 0.25f);
        rendering::renderLine(
            _mouseVisualizer.clickPosition,
            _mouseVisualizer.currentPosition,
            global::windowDelegate->currentWindowSize(),
            StartColor,
            _mouseVisualizer.color
        );
    }

    if (!disabledTouch()) {
        _touchMarkers.render(_touchInputState.touchPoints());
    }
}

void InteractionHandler::markInteraction() {
    _interactionMonitor.markInteraction();
}

void InteractionHandler::clearJoystickStates() {
    std::fill(
        _joystickInputStates.begin(),
        _joystickInputStates.end(),
        JoystickInputState()
    );
}

} // namespace openspace
