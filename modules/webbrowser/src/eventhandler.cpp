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
#include <openspace/engine/openspaceengine.h>
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

} // namespace

namespace openspace {

void EventHandler::initialize() {
    OsEng.registerModuleCharCallback(
        [this](unsigned int charCode, KeyModifier mod) -> bool {
            if (_browserInstance) {
                return charCallback(charCode, mod);
            }
            return false;
        }
    );
    OsEng.registerModuleKeyboardCallback(
        [this](Key key, KeyModifier mod, KeyAction action) -> bool {
            if (_browserInstance) {
                return keyboardCallback(key, mod, action);
            }
            return false;
        }
    );
    OsEng.registerModuleMousePositionCallback(
        [this](double x, double y) -> bool {
            if (_browserInstance) {
                return mousePositionCallback(x, y);
            }
            return false;
        }
    );
    OsEng.registerModuleMouseButtonCallback(
        [this](MouseButton button, MouseAction action) -> bool {
            if (_browserInstance) {
                return mouseButtonCallback(button, action);
            }
            return false;
        }
    );
    OsEng.registerModuleMouseScrollWheelCallback(
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
    if (button != MouseButton::Left) {
        return false;
    }

    int clickCount = BrowserInstance::SingleClick;

    // click or release?
    if (action == MouseAction::Release) {
        _leftMouseDown = false;
    } else {
        if (isDoubleClick() ) {
            ++clickCount;
        } else {
            _lastClickTime = std::chrono::high_resolution_clock::now();
        }

        _leftMouseDown = true;
        _lastClickPosition = _mousePosition;
    }

    return _browserInstance->sendMouseClickEvent(
        mouseEvent(),
        MBT_LEFT,
        !_leftMouseDown,
        clickCount
    );
}

bool EventHandler::isDoubleClick() const {
    // check time
    using namespace std::chrono;
    auto now = high_resolution_clock::now();
    milliseconds maxTimeDifference(EventHandler::doubleClickTime());
    auto requiredTime = _lastClickTime + maxTimeDifference;
    if (requiredTime < now) {
        return false;
    }

    // check position
    const float maxDist = static_cast<float>(EventHandler::maxDoubleClickDistance() / 2);
    const bool x = abs(_mousePosition.x - _lastClickPosition.x) < maxDist;
    const bool y = abs(_mousePosition.y - _lastClickPosition.y) < maxDist;

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
    keyEvent.modifiers        = static_cast<uint32>(modifier);
    keyEvent.type             = KEYEVENT_CHAR;
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
    keyEvent.modifiers        = static_cast<uint32>(modifier);
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

    if (_leftMouseDown) {
        event.modifiers = EVENTFLAG_LEFT_MOUSE_BUTTON;
    }

    return event;
}

void EventHandler::setBrowserInstance(
                                  const std::shared_ptr<BrowserInstance>& browserInstance)
{
    LDEBUG("Setting browser instance.");
    _browserInstance = browserInstance;
}

void EventHandler::detachBrowser() {
    if (_browserInstance) {
        LDEBUG(fmt::format(
            "Detaching browser instance with use count {}", _browserInstance.use_count()
        ));
    }
    _browserInstance = nullptr;
}

int EventHandler::doubleClickTime() {
#ifdef WIN32
    return GetDoubleClickTime();
#else
    return 500;
#endif
}

int EventHandler::maxDoubleClickDistance() {
#ifdef WIN32
    return GetSystemMetrics(SM_CXDOUBLECLK);
#else
    return MaxDoubleClickDistance;
#endif
}

} // namespace openspace
