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


#include "event_handler.h"

namespace {
    std::string _loggerCat = "WebGUI:EventHandler";
}

namespace openspace {
    void EventHandler::initialize() {
        OsEng.registerModuleKeyboardCallback(
                [this](Key key, KeyModifier mod, KeyAction action) -> bool {
                    if (true /*gui.isEnabled()*/) {
                        return keyBoardCallback(key, mod, action);
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
        bool mouseReleased = action == MouseAction::Release;
        // TODO(klas): Support more button types than only left
        CefBrowserHost::MouseButtonType leftButton = MBT_LEFT;
        browser->GetHost()->SendMouseClickEvent(mouseEvent(), leftButton, mouseReleased, SINGLE_CLICK);

        // TODO(klas): Figure out when to block and when to not block
        return false;
    }

    bool EventHandler::mousePositionCallback(double x, double y) {
        mousePosition.x = x;
        mousePosition.y = y;
        browser->GetHost()->SendMouseMoveEvent(mouseEvent(), false);

        // Let the mouse event trickle on
        return false;
    }

    bool EventHandler::mouseWheelCallback(double position) {
        // TODO(klas): Support horizontal scrolling, use shift?
        // TODO(klas): Figure out how this should be used
        browser->GetHost()->SendMouseWheelEvent(mouseEvent(), position, 0);
        return false;
    }

    bool EventHandler::keyBoardCallback(Key key, KeyModifier modifier, KeyAction action) {
        CefKeyEvent keyEvent;
        keyEvent.windows_key_code = static_cast<int>(key);
        keyEvent.modifiers        = 0;
//        keyEvent.modifiers        = modifier;
        keyEvent.type             = keyEventType(action);
//        browser->GetHost()->SendKeyEvent(keyEvent);
        keyEvent.type = KEYEVENT_CHAR;
        browser->GetHost()->SendKeyEvent(keyEvent);

        // TODO(klas): figure out when to block
        return false;
    }

    cef_key_event_type_t EventHandler::keyEventType(KeyAction action) {
        switch(action) {
            case KeyAction::Release:
                return KEYEVENT_KEYUP;
            case KeyAction::Press:
            case KeyAction::Repeat:
            default:
                return KEYEVENT_KEYDOWN;
        }
    }

    CefMouseEvent EventHandler::mouseEvent() {
        CefMouseEvent event;
        event.x = mousePosition.x;
        event.y = mousePosition.y;
        return event;
    }
} // namespace openspace