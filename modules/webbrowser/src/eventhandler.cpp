/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/interactionmonitor.h>
#include <ghoul/logging/logmanager.h>
#include <fmt/format.h>

namespace {
    constexpr const char* _loggerCat = "WebBrowser:EventHandler";

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

    // Map from GLFW key codes to native key codes for Mac.
    // The keys inserted here are based from setting breakpoints in
    // the CEF-bundled 'cefclient' (browser_window_osr_mac.mm)
    // as well as trial and error.
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
     * get the number of milliseconds that is allowed between two clicks for it to count
     * as a double click
     * @return
     */
    int doubleClickTime() {
#ifdef WIN32
        return GetDoubleClickTime();
#else
        return 500;
#endif
    }

    /**
     * get the rectangle width around the first click in a double click that the second
     * click has to be _within_
     * @return
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
    global::callback::character.emplace_back(
        [this](unsigned int charCode, KeyModifier mod) -> bool {
            if (_browserInstance) {
                return charCallback(charCode, mod);
            }
            return false;
        }
    );
    global::callback::keyboard.emplace_back(
        [this](Key key, KeyModifier mod, KeyAction action) -> bool {
            if (_browserInstance) {
                return keyboardCallback(key, mod, action);
            }
            return false;
        }
    );
    global::callback::mousePosition.emplace_back(
        [this](double x, double y) -> bool {
            if (_browserInstance) {
                return mousePositionCallback(x, y);
            }
            return false;
        }
    );
    global::callback::mouseButton.emplace_back(
        [this](MouseButton button, MouseAction action, KeyModifier mods) -> bool {
            if (_browserInstance) {
                return mouseButtonCallback(button, action, mods);
            }
            return false;
        }
    );
    global::callback::mouseScrollWheel.emplace_back(
        [this](double x, double y) -> bool {
            if (_browserInstance) {
                const glm::ivec2 delta(x, y);
                return mouseWheelCallback(delta);
            }
            return false;
        }
    );

    global::callback::touchDetected.emplace_back(
        [&](TouchInput input) -> bool {
            if (!_browserInstance) {
                return false;
            }

            const glm::vec2 windowPos = input.currentWindowCoordinates();
            const bool hasContent = _browserInstance->hasContent(
                static_cast<int>(windowPos.x),
                static_cast<int>(windowPos.y)
            );
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
                _validTouchStates.emplace_back(input);
            }
            else {
                _validTouchStates.emplace_back(input);
            }
            return true;
        }
    );

    global::callback::touchUpdated.emplace_back(
        [&](TouchInput input) -> bool {
            if (!_browserInstance) {
                return false;
            }
            if (_validTouchStates.empty()) {
                return false;
            }

            auto it = std::find_if(
                _validTouchStates.cbegin(),
                _validTouchStates.cend(),
                [&](const TouchInput& state){
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
#else
                glm::vec2 windowPos = input.currentWindowCoordinates();
                _mousePosition.x = windowPos.x;
                _mousePosition.y = windowPos.y;
                _leftButton.down = true;
                _browserInstance->sendMouseMoveEvent(mouseEvent());
#endif
                return true;
            }
            else if (it != _validTouchStates.cend()){
                return true;
            }
            return false;
        }
    );

    global::callback::touchExit.emplace_back(
        [&](TouchInput input) {
            if (!_browserInstance) {
                return;
            }
            if (_validTouchStates.empty()) {
                return;
            }

            const auto found = std::find_if(
                _validTouchStates.cbegin(),
                _validTouchStates.cend(),
                [&](const TouchInput& state){
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
#endif
            _validTouchStates.erase(found);
#ifndef WIN32
            if (_validTouchStates.empty()) {
                glm::vec2 windowPos = input.currentWindowCoordinates();
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
#endif
        }
    );
}

bool EventHandler::mouseButtonCallback(MouseButton button, MouseAction action,
                                       KeyModifier mods)
{
    if (button != MouseButton::Left && button != MouseButton::Right) {
        return false;
    }

    global::interactionMonitor.markInteraction();
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
    // check time
    using namespace std::chrono;

    auto now = high_resolution_clock::now();
    milliseconds maxTimeDifference(doubleClickTime());
    auto requiredTime = button.lastClickTime + maxTimeDifference;
    if (requiredTime < now) {
        return false;
    }

    // check position
    const float maxDist = maxDoubleClickDistance() / 2.f;
    const bool x = abs(_mousePosition.x - button.lastClickPosition.x) < maxDist;
    const bool y = abs(_mousePosition.y - button.lastClickPosition.y) < maxDist;

    return x && y;
}

bool EventHandler::mousePositionCallback(double x, double y) {
    const glm::vec2 dpiScaling = global::windowDelegate.dpiScaling();
    _mousePosition.x = floor(static_cast<float>(x) * dpiScaling.x);
    _mousePosition.y = floor(static_cast<float>(y) * dpiScaling.y);
    _browserInstance->sendMouseMoveEvent(mouseEvent());
    global::interactionMonitor.markInteraction();

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
    keyEvent.windows_key_code     = mapFromGlfwToWindows(key);
    keyEvent.native_key_code      = mapFromGlfwToNative(key);
    keyEvent.unmodified_character = mapFromGlfwToUnmodifiedCharacter(key);
    keyEvent.modifiers            = mapToCefModifiers(modifier);
    keyEvent.type                 = keyEventType(action);

    return _browserInstance->sendKeyEvent(keyEvent);
}

bool EventHandler::specialKeyEvent(Key key, KeyModifier mod, KeyAction) {
    switch (key) {
        case Key::F5:
            _browserInstance->reloadBrowser();
            return true;
        case Key::A:
            if (hasKeyModifier(mod, KeyModifier::Super)) {
                _browserInstance->selectAll();
                return true;
            }
            return false;
        default:
            return false;
    }
}

cef_key_event_type_t EventHandler::keyEventType(KeyAction action) {
    return action == KeyAction::Release ? KEYEVENT_KEYUP : KEYEVENT_KEYDOWN;
}

CefMouseEvent EventHandler::mouseEvent(KeyModifier mods) {
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
    const std::vector<std::pair<Key, KeyModifier>>& keyModVec =
        global::navigationHandler.inputState().pressedKeys();
    for (const std::pair<Key, KeyModifier>& keyModPair : keyModVec) {
        const KeyModifier mods = keyModPair.second;
        event.modifiers |= static_cast<uint32_t>(mapToCefModifiers(mods));
    }
    event.pointer_type = cef_pointer_type_t::CEF_POINTER_TYPE_TOUCH;
    return event;
}
#endif

void EventHandler::setBrowserInstance(BrowserInstance* browserInstance) {
    LDEBUG("Setting browser instance.");
    _browserInstance = browserInstance;
}

void EventHandler::resetBrowserInstance() {
    _browserInstance = nullptr;
}

} // namespace openspace
