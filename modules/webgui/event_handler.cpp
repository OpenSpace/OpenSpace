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
        return false;
    }

    bool EventHandler::mouseWheelCallback(double position) {
        return false;
    }

    bool EventHandler::keyBoardCallback(Key key, KeyModifier modifier, KeyAction action) {
        return false;
    }


} // namespace openspace