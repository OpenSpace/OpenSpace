/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

namespace {
    constexpr openspace::properties::Property::PropertyInfo DisableKeybindingsInfo = {
        "DisableKeybindings",
        "Disable all keybindings",
        "Disables all keybindings without removing them. Please note that this does not "
        "apply to the key to open the console.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableMouseInputInfo = {
        "DisableMouseInput",
        "Disable all mouse inputs",
        "Disables all mouse inputs and prevents them from affecting the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableJoystickInputInfo = {
        "DisableJoystickInput",
        "Disable all joystick inputs",
        "Disables all joystick inputs and prevents them from affecting the camera.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DisableTouchInputInfo = {
        "DisableTouchInput",
        "Disable all touch inputs",
        "Disables all touch inputs and prevents them from affecting the camera.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MaxDoubleTapTimeInfo = {
        "MaxDoubleTapTime",
        "Max time for touch double tap detection (milliseconds)",
        "Max tap delay (in ms) for double tap detection in touch interaction.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo MouseVisualizerInfo = {
        "MouseInteractionVisualizer",
        "Mouse Interaction Visualizer",
        "The mouse interaction visualizer shows the distance the mouse has been moved "
        "since it was pressed down."
    };

    constexpr openspace::properties::Property::PropertyInfo MouseVisualizerEnabledInfo = {
        "Enabled",
        "Enabled",
        "If this setting is enabled, the mouse interaction will be visualized on the "
        "screen by showing the distance the mouse has been moved since it was pressed "
        "down."
    };

    constexpr openspace::properties::Property::PropertyInfo MouseVisualizerColorInfo = {
        "Color",
        "Color",
        "The color used to render the line showing the mouse visualizer."
    };
} // namespace

namespace openspace::interaction {

InteractionHandler::InteractionHandler()
    : properties::PropertyOwner({ "InteractionHandler", "Interaction Handler" })
    , _disableKeybindings(DisableKeybindingsInfo, false)
    , _disableMouseInputs(DisableMouseInputInfo, false)
    , _disableJoystickInputs(DisableJoystickInputInfo, false)
    , _disableTouchInputs(DisableTouchInputInfo, false)
    , _maxDoubleTapTime(MaxDoubleTapTimeInfo, 300, 10, 1000)
    , _mouseVisualizer({
        properties::PropertyOwner(MouseVisualizerInfo),
        properties::BoolProperty(MouseVisualizerEnabledInfo, false),
        properties::Vec4Property(
            MouseVisualizerColorInfo,
            glm::vec4(1.f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        ),
        false,
        false,
        glm::vec2(0.f),
        glm::vec2(0.f)
     })
{
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
    _mouseVisualizer.color.setViewOption(properties::Property::ViewOptions::Color);
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

    // TODO: Move to orbital or navigation handler?
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
    // Once the fram is finished, reset the touch interaction information
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

void InteractionHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mouseButtonCallback(button, action);

    if (_mouseVisualizer.enable) {
        if (action == MouseAction::Press) {
            _mouseVisualizer.isMouseFirstPress = true;
            _mouseVisualizer.isMousePressed = true;
        }
        else if (action == MouseAction::Release) {
            _mouseVisualizer.isMousePressed = false;
            _mouseVisualizer.currentPosition = glm::vec2(0.f);
            _mouseVisualizer.clickPosition = glm::vec2(0.f);
        }
    }

    _interactionMonitor.markInteraction();
}

void InteractionHandler::mousePositionCallback(double x, double y) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mousePositionCallback(x, y);

    if (_mouseVisualizer.enable && _mouseVisualizer.isMousePressed) {
        if (_mouseVisualizer.isMouseFirstPress) {
            _mouseVisualizer.clickPosition = glm::vec2(x, y);
            _mouseVisualizer.isMouseFirstPress = false;
        }

        _mouseVisualizer.currentPosition = glm::vec2(x, y);
    }

    _interactionMonitor.markInteraction();
}

void InteractionHandler::mouseScrollWheelCallback(double pos) {
    if (_disableMouseInputs) {
        return;
    }
    _mouseInputState.mouseScrollWheelCallback(pos);

    _interactionMonitor.markInteraction();
}

void InteractionHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    // There is no need to disable the keyboard callback based on a property as the vast
    // majority of input is coming through Lua scripts anyway which are not blocked here
    _keyboardInputState.keyboardCallback(key, modifier, action);

    _interactionMonitor.markInteraction();
}

bool InteractionHandler::touchDetectedCallback(TouchInput i) {
    _interactionMonitor.markInteraction();
    return _touchInputState.touchDetectedCallback(i);
}

bool InteractionHandler::touchUpdatedCallback(TouchInput i) {
    _interactionMonitor.markInteraction();
    return _touchInputState.touchUpdatedCallback(i);
}

void InteractionHandler::touchExitCallback(TouchInput i) {
    _interactionMonitor.markInteraction();
    _touchInputState.touchExitCallback(i);
}

void InteractionHandler::renderOverlay() {
    if (_mouseVisualizer.enable && _mouseVisualizer.isMousePressed) {
        constexpr glm::vec4 StartColor = glm::vec4(0.4f, 0.4f, 0.4f, 0.25f);
        rendering::helper::renderLine(
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

} // namespace openspace::interaction
