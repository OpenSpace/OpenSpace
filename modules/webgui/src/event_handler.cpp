/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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


#include <ext/ghoul/include/ghoul/logging/logmanager.h>
#include <ext/ghoul/ext/cppformat/fmt/format.h>
#include "include/event_handler.h"

namespace {
    std::string _loggerCat = "WebGUI:EventHandler";
}

namespace openspace {
void EventHandler::initialize() {
    OsEng.registerModuleCharCallback(
            [this](unsigned int charCode, KeyModifier mod) -> bool {
                if (true /*gui.isEnabled()*/) {
                    return charCallback(charCode, mod);
                } else {
                    return false;
                }
            }
    );
    OsEng.registerModuleKeyboardCallback(
            [this](Key key, KeyModifier mod, KeyAction action) -> bool {
                if (true /*gui.isEnabled()*/) {
                    return keyboardCallback(key, mod, action);
                } else {
                    return false;
                }
            }
    );
    OsEng.registerModuleMousePositionCallback(
            [this](double x, double y) -> bool {
                if (true /*gui.isEnabled()*/) {
                    return mousePositionCallback(x, y);
                } else {
                    return false;
                }
            }
    );
    OsEng.registerModuleMouseButtonCallback(
            [this](MouseButton button, MouseAction action) -> bool {
                if (true /*gui.isEnabled()*/) {
                    return mouseButtonCallback(button, action);
                } else {
                    return false;
                }
            }
    );
    OsEng.registerModuleMouseScrollWheelCallback(
            [this](double pos) -> bool {
                if (true /*gui.isEnabled()*/) {
                    return mouseWheelCallback(pos);
                } else {
                    return false;
                }
            }
    );
}

bool EventHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (button != MouseButton::Left) return false;

    if (action == MouseAction::Release) {
        leftMouseDown = false;
    } else {
        leftMouseDown = true;
    }
    browser->GetHost()->SendMouseClickEvent(mouseEvent(), MBT_LEFT, leftMouseDown, SINGLE_CLICK);

    // TODO(klas): Figure out when to block and when to not block
    return false;
}

bool EventHandler::mousePositionCallback(double x, double y) {
    mousePosition.x = (int) x;
    mousePosition.y = (int) y;
    CefMouseEvent localMouseEvent(mouseEvent());
    if (leftMouseDown) {
        localMouseEvent.modifiers = EVENTFLAG_LEFT_MOUSE_BUTTON;
        LDEBUGC("event", fmt::format("dragging: {}, {}", x, y));
    }
    browser->GetHost()->SendMouseMoveEvent(localMouseEvent, false);

    // Let the mouse event trickle on
    return false;
}

bool EventHandler::mouseWheelCallback(double position) {
    // TODO(klas): Support horizontal scrolling, use shift?
    // TODO(klas): Figure out how this should be used
    browser->GetHost()->SendMouseWheelEvent(mouseEvent(), (int) position, 0);
    return false;
}

bool EventHandler::charCallback(unsigned int charCode, KeyModifier modifier) {
    CefKeyEvent keyEvent;
    keyEvent.windows_key_code = charCode;
    keyEvent.modifiers        = static_cast<uint32>(modifier);
    keyEvent.type             = KEYEVENT_CHAR;
    browser->GetHost()->SendKeyEvent(keyEvent);

    // TODO(klas): figure out when to block
    return false;
}

bool EventHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (specialKeyEvent(key)) {
        return true;
    }

    CefKeyEvent keyEvent;
//        keyEvent.native_key_code  = mapFromGlfwToNative(key);
    keyEvent.windows_key_code = mapFromGlfwToNative(key);
    keyEvent.modifiers        = static_cast<uint32>(modifier);
    keyEvent.type             = keyEventType(action);
    browser->GetHost()->SendKeyEvent(keyEvent);

    // TODO(klas): figure out when to block
    return false;
}

/**
 * Detect if there is a special event that should be caught by the GUI before it is sent to CEF
 * @param key the pressed key
 * @return true if event found, false otherwise
 */
bool EventHandler::specialKeyEvent(Key key) {
    switch(key) {
        case Key::F5:
            reloadBrowser();
            return true;
        default: return false;
    }
}

/**
 * Map from GLFW key codes to "regular" key codes, supported by JS and CEF.
 * See http://keycode.info/ for lookup
 * @param key
 * @return the key code, if mapped or the GLFW key code
 */
int EventHandler::mapFromGlfwToNative(Key key) {
    switch (key) {
        case Key::BackSpace:      return 8;
        case Key::Tab:            return 9;
        case Key::Enter:          return 13;
        case Key::Left:           return 37;
        case Key::Up:             return 38;
        case Key::Right:          return 39;
        case Key::Down:           return 40;
        case Key::Delete:         return 46;
        default:                  return static_cast<int>(key);
    }
}

void EventHandler::reloadBrowser() {
    browser->Reload();
}

/**
 * Find the CEF key event to use for a given action
 * @param action
 * @return
 */
cef_key_event_type_t EventHandler::keyEventType(KeyAction action) {
    if (action == KeyAction::Release) {
        return KEYEVENT_KEYUP;
    } else {
//            return KEYEVENT_RAWKEYDOWN;
        return KEYEVENT_KEYDOWN;
    }
}

/**
 * Create a mouse event on the current cursor position
 * @return
 */
CefMouseEvent EventHandler::mouseEvent() {
    CefMouseEvent event;
    event.x = (int) mousePosition.x;
    event.y = (int) mousePosition.y;
    return event;
}
} // namespace openspace