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

#ifndef __OPENSPACE_MODULE_WEBBROWSER___EVENT_HANDLER___H__
#define __OPENSPACE_MODULE_WEBBROWSER___EVENT_HANDLER___H__

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <ghoul/glm.h>
#include <chrono>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_browser.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace openspace {

class BrowserInstance;

class EventHandler {
public:
    void initialize();
    void setBrowser(const CefRefPtr<CefBrowser>& browser);
    void setBrowserInstance(const std::shared_ptr<BrowserInstance>& browserInstance);
    void detachBrowser();

private:
#if !defined(WIN32)
    static const int MaxDoubleClickDistance = 4;
#endif

    bool mouseButtonCallback(MouseButton button, MouseAction action);
    bool mousePositionCallback(double x, double y);
    bool mouseWheelCallback(glm::ivec2 delta);
    bool charCallback(unsigned int charCode, KeyModifier modifier);
    bool keyboardCallback(Key key, KeyModifier modifier, KeyAction action);

    /**
     * Detect if there is a special event that should be caught by the GUI before it is
     * sent to CEF.
     *
     * \param key the pressed key
     * \return true if event found, false otherwise
     */
    bool specialKeyEvent(Key key);

    /**
     * Create a mouse event on the current cursor position.
     *
     * \return
     */
    CefMouseEvent mouseEvent();

    /**
     * Find the CEF key event to use for a given action.
     *
     * \param action
     * \return
     */
    cef_key_event_type_t keyEventType(KeyAction action);

    bool _leftMouseDown = false;

    std::shared_ptr<BrowserInstance> _browserInstance = nullptr;
    glm::vec2 _mousePosition = { 0.f, 0.f };
    glm::vec2 _lastClickPosition = { 0.f, 0.f };
    std::chrono::high_resolution_clock::time_point _lastClickTime;

    /**
     * determines if a click should be sent as a double click or not
     * @return
     */
    bool isDoubleClick() const;

    /**
     * get the number of milliseconds that is allowed between two clicks for it to count
     * as a double click
     * @return
     */
    static int doubleClickTime();

    /**
     * get the rectangle width around the first click in a double click that the second
     * click has to be _within_
     * @return
     */
    static int maxDoubleClickDistance();
};


} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___EVENT_HANDLER___H__
