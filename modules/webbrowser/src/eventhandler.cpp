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

#include <modules/webbrowser/include/eventhandler.h>

#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/keyboardinputstate.h>
#include <openspace/navigation/navigationhandler.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "WebBrowser:EventHandler";

    /**
     * Map from GLFW key codes to windows key codes, supported by JS and CEF.
     * See http://keycode.info/ for lookup
     *
     * \param key
     * \return the key code, if mapped or the GLFW key code
     */
    int mapFromGlfwToWindows(openspace::Key key) {
        switch (key) {
            case openspace::Key::BackSpace:   return 8;
            case openspace::Key::Tab:         return 9;
            case openspace::Key::Enter:       return 13;
            case openspace::Key::Left:        return 37;
            case openspace::Key::Up:          return 38;
            case openspace::Key::Right:       return 39;
            case openspace::Key::Down:        return 40;
            case openspace::Key::Delete:      return 46;
            default:                          return static_cast<int>(key);
        }
    }

    // Map from GLFW key codes to native key codes for Mac. The keys inserted here are
    // based from setting breakpoints in the CEF-bundled 'cefclient'
    // (browser_window_osr_mac.mm) as well as trial and error.
    // There is an issue for proper cross-platform key events in CEF:
    // https://bitbucket.org/chromiumembedded/cef/issues/1750
    // For now, the 'important' keys are inserted here manually.
    int mapFromGlfwToNative(openspace::Key key) {
        switch (key) {
            case openspace::Key::BackSpace:   return 51;
            case openspace::Key::LeftControl: return 59;
            case openspace::Key::LeftSuper:   return 55;
            case openspace::Key::Enter:       return 36;
            case openspace::Key::Left:        return 123;
            case openspace::Key::Right:       return 124;
            case openspace::Key::Up:          return 126;
            case openspace::Key::Down:        return 127;
            case openspace::Key::A:           return 97;
            case openspace::Key::Num0:        return 82;
            case openspace::Key::Num1:        return 83;
            case openspace::Key::Num2:        return 84;
            case openspace::Key::Num3:        return 85;
            case openspace::Key::Num4:        return 86;
            case openspace::Key::Num5:        return 87;
            case openspace::Key::Num6:        return 88;
            case openspace::Key::Num7:        return 89;
            case openspace::Key::Num8:        return 91; // Note: 91, not 90.
            case openspace::Key::Num9:        return 92;
            default:                          return static_cast<int>(key);
        }
    }

    int16_t mapFromGlfwToCharacter(openspace::Key key) {
        return static_cast<int16_t>(key);
    }

    // This is needed to avoid the backspace up event to trigger backspace.
    int16_t mapFromGlfwToUnmodifiedCharacter(openspace::Key key) {
        switch (key) {
            case openspace::Key::BackSpace:   return 127;
            case openspace::Key::A:           return 97;
            default:                          return static_cast<int16_t>(key);
        }
    }

    uint32_t mapToCefModifiers(openspace::KeyModifier modifiers) {
        uint32_t cefModifiers = 0;
        // Based on cef_event_flags_t in cef_types.h
        if (hasKeyModifier(modifiers, openspace::KeyModifier::Shift)) {
            cefModifiers |= 1 << 1;
        }
        if (hasKeyModifier(modifiers, openspace::KeyModifier::Control)) {
            cefModifiers |= 1 << 2;
        }
        if (hasKeyModifier(modifiers, openspace::KeyModifier::Alt)) {
            cefModifiers |= 1 << 3;
        }
        if (hasKeyModifier(modifiers, openspace::KeyModifier::Super)) {
            cefModifiers |= 1 << 7;
        }
        return cefModifiers;
    }

    /**
     * Get the number of milliseconds that is allowed between two clicks for it to count
     * as a double click
     */
    int doubleClickTime() {
#ifdef WIN32
        return GetDoubleClickTime();
#else
        return 500;
#endif
    }

    /**
     * Get the rectangle width around the first click in a double click that the second
     * click has to be _within_
     */
    int maxDoubleClickDistance() {
#ifdef WIN32
        return GetSystemMetrics(SM_CXDOUBLECLK);
#else
        return 4;
#endif
    }

} // namespace

namespace openspace {

void EventHandler::initialize() {
    global::callback::character->emplace(
        global::callback::character->begin(),
        [this](unsigned int charCode, KeyModifier mod, IsGuiWindow isGuiWindow) -> bool {
            if (_browserInstance && isGuiWindow) {
                return charCallback(charCode, mod);
            }
            return false;
        }
    );
    global::callback::keyboard->emplace(
        global::callback::keyboard->begin(),
        [this](Key key, KeyModifier mod, KeyAction action,
            IsGuiWindow isGuiWindow) -> bool
        {
            if (_browserInstance && isGuiWindow) {
                return keyboardCallback(key, mod, action);
            }
            return false;
        }
    );
    global::callback::mousePosition->emplace(
        global::callback::mousePosition->begin(),
        [this](double x, double y, IsGuiWindow isGuiWindow) -> bool {
            if (_browserInstance && isGuiWindow) {
                return mousePositionCallback(x, y);
            }
            return false;
        }
    );
    global::callback::mouseButton->emplace(
        global::callback::mouseButton->begin(),
        [this](MouseButton button, MouseAction action,
               KeyModifier mods, IsGuiWindow isGuiWindow) -> bool
        {
            if (_browserInstance && isGuiWindow) {
                return mouseButtonCallback(button, action, mods);
            }
            return false;
        }
    );
    global::callback::mouseScrollWheel->emplace(
        global::callback::mouseScrollWheel->begin(),
        [this](double x, double y, IsGuiWindow isGuiWindow) -> bool {
            if (_browserInstance && isGuiWindow) {
                return mouseWheelCallback(glm::ivec2(x, y));
            }
            return false;
        }
    );

    global::callback::touchDetected->emplace(
        global::callback::touchDetected->begin(),
        [this](TouchInput input) -> bool {
            if (!_browserInstance) {
                return false;
            }

            const glm::vec2 windowPos = input.currentWindowCoordinates();
            const bool hasContent = _browserInstance->hasContent(windowPos);
            if (!hasContent) {
                return false;
            }

            if (_validTouchStates.empty()) {
#ifdef WIN32
                CefTouchEvent event = touchEvent(
                    input,
                    cef_touch_event_type_t::CEF_TET_PRESSED
                );
                _browserInstance->sendTouchEvent(event);
#else
                _mousePosition.x = windowPos.x;
                _mousePosition.y = windowPos.y;
                _leftButton.down = true;
                _browserInstance->sendMouseClickEvent(
                    mouseEvent(),
                    MBT_LEFT,
                    false,
                    BrowserInstance::SingleClick
                );
#endif
            }

            _validTouchStates.emplace_back(input);

            global::interactionMonitor->markInteraction();
            return true;
        }
    );

    global::callback::touchUpdated->emplace(
        global::callback::touchUpdated->begin(),
        [this](TouchInput input) -> bool {
            if (!_browserInstance || _validTouchStates.empty()) {
                return false;
            }

            auto it = std::find_if(
                _validTouchStates.cbegin(),
                _validTouchStates.cend(),
                [&input](const TouchInput& state) {
                    return state.fingerId == input.fingerId &&
                           state.touchDeviceId == input.touchDeviceId;
                }
            );

            if (it == _validTouchStates.cbegin()) {
#ifdef WIN32
                CefTouchEvent event = touchEvent(
                    input,
                    cef_touch_event_type_t::CEF_TET_MOVED
                );
                _browserInstance->sendTouchEvent(event);
#else // ^^^^ WIN32 // !WIN32 vvvv
                const glm::vec2 windowPos = input.currentWindowCoordinates();
                _mousePosition.x = windowPos.x;
                _mousePosition.y = windowPos.y;
                _leftButton.down = true;
                _browserInstance->sendMouseMoveEvent(mouseEvent());
#endif // WIN32
                global::interactionMonitor->markInteraction();
                return true;
            }
            else if (it != _validTouchStates.cend()) {
                return true;
            }
            return false;
        }
    );

    global::callback::touchExit->emplace(
        global::callback::touchExit->begin(),
        [this](TouchInput input) {
            if (!_browserInstance) {
                return;
            }
            if (_validTouchStates.empty()) {
                return;
            }

            const auto found = std::find_if(
                _validTouchStates.cbegin(),
                _validTouchStates.cend(),
                [&input](const TouchInput& state) {
                    return state.fingerId == input.fingerId &&
                    state.touchDeviceId == input.touchDeviceId;
                }
            );

            if (found == _validTouchStates.cend()) {
                return;
            }
#ifdef WIN32
            CefTouchEvent event = touchEvent(
                input,
                cef_touch_event_type_t::CEF_TET_RELEASED
            );
            _browserInstance->sendTouchEvent(event);
#endif // WIN32
            _validTouchStates.erase(found);
#ifndef WIN32
            if (_validTouchStates.empty()) {
                const glm::vec2 windowPos = input.currentWindowCoordinates();
                _mousePosition.x = windowPos.x;
                _mousePosition.y = windowPos.y;
                _leftButton.down = false;
                _browserInstance->sendMouseClickEvent(
                    mouseEvent(),
                    MBT_LEFT,
                    true,
                    BrowserInstance::SingleClick
                );
            }
#endif // WIN32
        }
    );
}

bool EventHandler::mouseButtonCallback(MouseButton button, MouseAction action,
                                       KeyModifier mods)
{
    if (button != MouseButton::Left && button != MouseButton::Right) {
        return false;
    }

    global::interactionMonitor->markInteraction();
    MouseButtonState& state = (button == MouseButton::Left) ? _leftButton : _rightButton;

    int clickCount = BrowserInstance::SingleClick;

    // click or release?
    if (action == MouseAction::Release) {
        state.down = false;
    }
    else {
        if (isDoubleClick(state)) {
            ++clickCount;
        }
        else {
            state.lastClickTime = std::chrono::high_resolution_clock::now();
        }

        state.down = true;
        state.lastClickPosition = _mousePosition;
    }

    return _browserInstance->sendMouseClickEvent(
        mouseEvent(mods),
        (button == MouseButton::Left) ? MBT_LEFT : MBT_RIGHT,
        !state.down,
        clickCount
    );
}

bool EventHandler::isDoubleClick(const MouseButtonState& button) const {
    auto now = std::chrono::high_resolution_clock::now();
    const std::chrono::milliseconds maxTimeDifference(doubleClickTime());
    auto requiredTime = button.lastClickTime + maxTimeDifference;
    if (requiredTime < now) {
        return false;
    }

    // check position
    const float maxDist = maxDoubleClickDistance() / 2.f;
    const bool x = std::abs(_mousePosition.x - button.lastClickPosition.x) < maxDist;
    const bool y = std::abs(_mousePosition.y - button.lastClickPosition.y) < maxDist;

    return x && y;
}

bool EventHandler::mousePositionCallback(double x, double y) {
    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    _mousePosition.x = std::floor(static_cast<float>(x) * dpiScaling.x);
    _mousePosition.y = std::floor(static_cast<float>(y) * dpiScaling.y);
    _mousePosition =
        global::windowDelegate->mousePositionViewportRelative(_mousePosition);
    _browserInstance->sendMouseMoveEvent(mouseEvent());
    global::interactionMonitor->markInteraction();

    // Let the mouse event trickle on
    return false;
}

bool EventHandler::mouseWheelCallback(glm::ivec2 delta) {
#ifdef WIN32
    // scroll wheel returns very low numbers on Windows machines
    delta.x *= 50;
    delta.y *= 50;
#endif
    return _browserInstance->sendMouseWheelEvent(mouseEvent(), delta);
}

bool EventHandler::charCallback(unsigned int charCode, KeyModifier modifier) {
    CefKeyEvent keyEvent;
    keyEvent.windows_key_code = mapFromGlfwToWindows(Key(charCode));
    keyEvent.character = mapFromGlfwToCharacter(Key(charCode));
    keyEvent.native_key_code = mapFromGlfwToNative(Key(charCode));
    keyEvent.modifiers = static_cast<uint32>(modifier);
    keyEvent.type = KEYEVENT_CHAR;

    return _browserInstance->sendKeyEvent(keyEvent);
}

bool EventHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (specialKeyEvent(key, modifier, action)) {
        return true;
    }

    CefKeyEvent keyEvent;

    // TODO(klas): Use something less platform specific?
    keyEvent.windows_key_code = mapFromGlfwToWindows(key);
    keyEvent.native_key_code = mapFromGlfwToNative(key);
    keyEvent.unmodified_character = mapFromGlfwToUnmodifiedCharacter(key);
    keyEvent.modifiers = mapToCefModifiers(modifier);
    keyEvent.type = keyEventType(action);

    return _browserInstance->sendKeyEvent(keyEvent);
}

bool EventHandler::specialKeyEvent(Key key, KeyModifier mod, KeyAction action) {
    switch (key) {
        case Key::A:
            if (hasKeyModifier(mod, KeyModifier::Super)) {
                _browserInstance->selectAll();
                return true;
            }
            return false;
        case Key::Minus:
            if (hasKeyModifier(mod, KeyModifier::Shift)) {
                CefKeyEvent keyEvent;
                keyEvent.windows_key_code = mapFromGlfwToWindows(Key(45));
                keyEvent.character = mapFromGlfwToCharacter(Key(45));
                keyEvent.native_key_code = mapFromGlfwToNative(Key(45));
                keyEvent.modifiers = static_cast<uint32>(mod);
                keyEvent.type = keyEventType(action);
                _browserInstance->sendKeyEvent(keyEvent);
                return true;
            }
            return false;
        case Key::Period: {
            CefKeyEvent keyEvent;
            keyEvent.windows_key_code = mapFromGlfwToWindows(Key::KeypadDecimal);
            keyEvent.character = mapFromGlfwToCharacter(Key::KeypadDecimal);
            keyEvent.native_key_code = mapFromGlfwToNative(Key::KeypadDecimal);
            keyEvent.modifiers = static_cast<uint32>(mod);
            keyEvent.type = keyEventType(action);
            _browserInstance->sendKeyEvent(keyEvent);
            return true;
        }
        default:
            return false;
    }
}

cef_key_event_type_t EventHandler::keyEventType(KeyAction action) {
    return action == KeyAction::Release ? KEYEVENT_KEYUP : KEYEVENT_KEYDOWN;
}

CefMouseEvent EventHandler::mouseEvent(KeyModifier mods) const {
    CefMouseEvent event;
    event.x = static_cast<int>(_mousePosition.x);
    event.y = static_cast<int>(_mousePosition.y);

    if (_leftButton.down) {
        event.modifiers = EVENTFLAG_LEFT_MOUSE_BUTTON;
    }

    if (_rightButton.down) {
        event.modifiers = EVENTFLAG_RIGHT_MOUSE_BUTTON;
    }

    event.modifiers |= static_cast<uint32_t>(mapToCefModifiers(mods));
    return event;
}

#ifdef WIN32
CefTouchEvent EventHandler::touchEvent(const TouchInput& input,
                                       const cef_touch_event_type_t eventType) const
{
    const glm::vec2 windowPos = input.currentWindowCoordinates();
    CefTouchEvent event = {};
    event.id = static_cast<int>(input.fingerId);
    event.x = windowPos.x;
    event.y = windowPos.y;
    event.type = eventType;
    const std::vector<std::pair<Key, KeyModifier>>& keyMods =
        global::navigationHandler->keyboardInputState().pressedKeys();
    for (const std::pair<Key, KeyModifier>& p : keyMods) {
        const KeyModifier mods = p.second;
        event.modifiers |= static_cast<uint32_t>(mapToCefModifiers(mods));
    }
    event.pointer_type = cef_pointer_type_t::CEF_POINTER_TYPE_TOUCH;
    return event;
}
#endif // WIN32

void EventHandler::setBrowserInstance(BrowserInstance* browserInstance) {
    LDEBUG("Setting browser instance");
    _browserInstance = browserInstance;
}

void EventHandler::resetBrowserInstance() {
    _browserInstance = nullptr;
}

} // namespace openspace
