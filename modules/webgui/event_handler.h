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


#ifndef __OPENSPACE_MODULE_WEBGUI___EVENT_HANDLER___H__
#define __OPENSPACE_MODULE_WEBGUI___EVENT_HANDLER___H__

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
//#include <include/cef_base.h>
#include <include/cef_browser.h>
#include <openspace/engine/openspaceengine.h>

namespace openspace {

class EventHandler {
public:
    EventHandler(CefRefPtr<CefBrowser> browser) : browser(browser), mousePosition(0,0) {};

    void initialize();

    const int SINGLE_CLICK = 1;

private:
    bool mouseButtonCallback(MouseButton, MouseAction);
    bool mousePositionCallback(double, double);
    bool mouseWheelCallback(double);
    bool keyBoardCallback(Key, KeyModifier, KeyAction);

    CefMouseEvent mouseEvent();
    cef_key_event_type_t EventHandler::keyEventType(KeyAction);

    CefRefPtr<CefBrowser> browser;
    glm::vec2 mousePosition;
};


}

#endif //__OPENSPACE_MODULE_WEBGUI___EVENT_HANDLER___H__
