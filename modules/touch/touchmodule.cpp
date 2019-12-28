/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/touch/touchmodule.h>
#include <modules/touch/include/win32_touch.h>

#include <modules/webgui/webguimodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <ghoul/logging/logmanager.h>
#include <sstream>
#include <string>
#include <iostream>

#ifdef OPENSPACE_MODULE_WEBBROWSER_ENABLED
#include <modules/webbrowser/webbrowsermodule.h>
#endif

using namespace TUIO;

namespace openspace {

bool TouchModule::processNewInput() {
    // Get new input from listener

    _listOfContactPoints = _ear.getInput();
    _ear.clearInput();
     // Set touch property to active (to void mouse input, mainly for mtdev bridges)
    _touch.touchActive(!_listOfContactPoints.empty());

    if (!_listOfContactPoints.empty()) {
        global::interactionMonitor.markInteraction();
    }

    // Erase old input id's that no longer exists
    _lastProcessed.erase(
        std::remove_if(
            _lastProcessed.begin(),
            _lastProcessed.end(),
            [this](const Point& point) {
        return std::find_if(
            _listOfContactPoints.begin(),
            _listOfContactPoints.end(),
            [&point](const TuioCursor& c) {
            return point.first == c.getSessionID();
        }
        ) == _listOfContactPoints.end(); }),
        _lastProcessed.end());

    // if tap occured, we have new input
    if (_listOfContactPoints.empty() && _lastProcessed.empty() && _ear.tap()) {
        TuioCursor c = _ear.getTap();
        _listOfContactPoints.push_back(c);
        _lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
        _touch.tap();
        return true;
    }

    // Check if we need to parse touchevent to the webgui
    processNewWebInput(_listOfContactPoints);

    // Return true if we got new input
    if (_listOfContactPoints.size() == _lastProcessed.size() &&
        !_listOfContactPoints.empty())
    {
        bool newInput = true;
        // go through list and check if the last registrered time is newer than the one in
        // lastProcessed (last frame)
        std::for_each(
            _lastProcessed.begin(),
            _lastProcessed.end(),
            [this, &newInput](Point& p) {
                std::vector<TuioCursor>::iterator cursor = std::find_if(
                    _listOfContactPoints.begin(),
                    _listOfContactPoints.end(),
                    [&p](const TuioCursor& c) { return c.getSessionID() == p.first; }
            );
            double now = cursor->getPath().back().getTuioTime().getTotalMilliseconds();
            if (!cursor->isMoving()) {
                 // if current cursor isn't moving, we want to interpret that as new input
                 // for interaction purposes
                newInput = true;
            }
            else if (p.second.getTuioTime().getTotalMilliseconds() == now) {
                newInput = false;
            }
        });
        return newInput;
    }
    else {
        return false;
    }
}

void TouchModule::processNewWebInput(const std::vector<TuioCursor>& listOfContactPoints) {
    bool isWebPositionCallbackZero =
        (_webPositionCallback.x == 0 && _webPositionCallback.y == 0);
    bool isSingleContactPoint = (listOfContactPoints.size() == 1);
    if (isSingleContactPoint && isWebPositionCallbackZero) {
        glm::ivec2 res = global::windowDelegate.currentWindowSize();
        glm::dvec2 pos = glm::vec2(
            listOfContactPoints.at(0).getScreenX(res.x),
            listOfContactPoints.at(0).getScreenY(res.y)
        );

#ifdef OPENSPACE_MODULE_WEBBROWSER_ENABLED
        WebBrowserModule& module = *(global::moduleEngine.module<WebBrowserModule>());
        if (module.eventHandler().hasContentCallback(pos.x, pos.y)) {
            _webPositionCallback = glm::vec2(pos.x, pos.y);
            module.eventHandler().touchPressCallback(pos.x, pos.y);
        }
    }
    // Send mouse release if not same point input
    else if (!isSingleContactPoint && !isWebPositionCallbackZero) {
        WebBrowserModule& module = *(global::moduleEngine.module<WebBrowserModule>());
        module.eventHandler().touchReleaseCallback(_webPositionCallback.x,
            _webPositionCallback.y);
        _webPositionCallback = glm::vec2(0, 0);
#endif
    }
}

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{
    addPropertySubOwner(_touch);
    addPropertySubOwner(_markers);

    global::callback::initializeGL.push_back([&]() {
        LDEBUGC("TouchModule", "Initializing TouchMarker OpenGL");
        _markers.initialize();
#ifdef WIN32
    // We currently only support one window of touch input internally
    // so here we grab the first window-handle and use it.
    void* nativeWindowHandle = global::windowDelegate.getNativeWindowHandle(0);
    if (nativeWindowHandle) {
        _win32TouchHook.reset(new Win32TouchHook(nativeWindowHandle));
    }
#endif
    });

    global::callback::deinitializeGL.push_back([&]() {
        LDEBUGC("TouchMarker", "Deinitialize TouchMarker OpenGL");
        _markers.deinitialize();
    });

    global::callback::preSync.push_back([&]() {
        _touch.setCamera(global::navigationHandler.camera());
        _touch.setFocusNode(global::navigationHandler.orbitalNavigator().anchorNode());

        if (processNewInput() && global::windowDelegate.isMaster()) {
            _touch.updateStateFromInput(_listOfContactPoints, _lastProcessed);
        }
        else if (_listOfContactPoints.empty()) {
            _touch.resetAfterInput();
        }

        // update lastProcessed
        _lastProcessed.clear();
        for (const TuioCursor& c : _listOfContactPoints) {
            _lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
        }
        // used to save data from solver, only calculated for one frame when user chooses
        // in GUI
        _touch.unitTest();
        // calculate the new camera state for this frame
        _touch.step(global::windowDelegate.deltaTime());
    });

    global::callback::render.push_back([&]() { _markers.render(_listOfContactPoints); });

}

TouchModule::~TouchModule() {
    //intentionally left empty
}

} // namespace openspace
