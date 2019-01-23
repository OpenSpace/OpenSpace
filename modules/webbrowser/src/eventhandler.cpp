/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/logging/logmanager.h>
#include <fmt/format.h>

namespace {
    constexpr const char* _loggerCat = "WebBrowser:EventHandler";

    /**
    * Map from GLFW key codes to "regular" key codes, supported by JS and CEF.
    * See http://keycode.info/ for lookup
    *
    * \param key
    * \return the key code, if mapped or the GLFW key code
    */
    int mapFromGlfwToNative(openspace::Key key) {
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
        [this](MouseButton button, MouseAction action) -> bool {
            if (_browserInstance) {
                return mouseButtonCallback(button, action);
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
}

bool EventHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (button != MouseButton::Left && button != MouseButton::Right) {
        return false;
    }

    MouseButtonState& state = (button == MouseButton::Left) ? _leftButton : _rightButton;

    int clickCount = BrowserInstance::SingleClick;

    // click or release?
    if (action == MouseAction::Release) {
        state.down = false;
    } else {
        if (isDoubleClick(state)) {
            ++clickCount;
        } else {
            state.lastClickTime = std::chrono::high_resolution_clock::now();
        }

        state.down = true;
        state.lastClickPosition = _mousePosition;
    }

    return _browserInstance->sendMouseClickEvent(
        mouseEvent(),
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
    _mousePosition.x = floor(static_cast<float>(x));
    _mousePosition.y = floor(static_cast<float>(y));
    _browserInstance->sendMouseMoveEvent(mouseEvent());
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
    keyEvent.windows_key_code = charCode;
    keyEvent.modifiers = static_cast<uint32>(modifier);
    keyEvent.type = KEYEVENT_CHAR;
    // TODO(klas): figure out when to block
    return _browserInstance->sendKeyEvent(keyEvent);
}

bool EventHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (specialKeyEvent(key)) {
        return true;
    }

    CefKeyEvent keyEvent;
    // TODO(klas): Use something less platform specific?
    keyEvent.windows_key_code = mapFromGlfwToNative(key);
    keyEvent.modifiers        = mapToCefModifiers(modifier);
    keyEvent.type             = keyEventType(action);
    // TODO(klas): figure out when to block
    return _browserInstance->sendKeyEvent(keyEvent);
}

bool EventHandler::specialKeyEvent(Key key) {
    switch (key) {
        case Key::F5:
            _browserInstance->reloadBrowser();
            return true;
        default:
            return false;
    }
}

cef_key_event_type_t EventHandler::keyEventType(KeyAction action) {
    if (action == KeyAction::Release) {
        return KEYEVENT_KEYUP;
    } else {
        return KEYEVENT_KEYDOWN;
    }
}

CefMouseEvent EventHandler::mouseEvent() {
    CefMouseEvent event;
    event.x = static_cast<int>(_mousePosition.x);
    event.y = static_cast<int>(_mousePosition.y);

    if (_leftButton.down) {
        event.modifiers = EVENTFLAG_LEFT_MOUSE_BUTTON;
    }

    if (_rightButton.down) {
        event.modifiers = EVENTFLAG_RIGHT_MOUSE_BUTTON;
    }

    return event;
}

void EventHandler::setBrowserInstance(BrowserInstance* browserInstance) {
    LDEBUG("Setting browser instance.");
    _browserInstance = browserInstance;
}

void EventHandler::detachBrowser() {
    _browserInstance = nullptr;
}

} // namespace openspace
