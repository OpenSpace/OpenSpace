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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H
#define __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_client.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include <ghoul/glm.h>
#include <string>

namespace openspace {

class BrowserClient;
class WebRenderHandler;

class BrowserInstance {
public:
    BrowserInstance(WebRenderHandler* renderer);
    ~BrowserInstance();

    void loadUrl(const std::string& url);
    /**
     * Load a local file.
     *
     * \param path The path to load
     * \return \c true if the path exists, \c false otherwise
     */
    bool loadLocalPath(std::string path);
    void initialize();

    /**
     * Call when the window has been reshaped.
     *
     * \param wrapper the windowWrapper capable of
     */
    void reshape(const glm::ivec2& windowSize);

    /**
     * encapsulate renderHandler's draw method
     */
    void draw();
    void close(bool force = false);

    bool sendKeyEvent(const CefKeyEvent& event);
    bool sendMouseClickEvent(const CefMouseEvent& event,
        CefBrowserHost::MouseButtonType button, bool mouseUp,
        int clickCount = SingleClick);

    bool sendMouseMoveEvent(const CefMouseEvent& event);

    /**
     * send scroll wheel event to browser
     *
     * \param event Key event with position
     * \param delta The scroll amount in pixels
     * \return if this scroll should be blocked or not
     */
    bool sendMouseWheelEvent(const CefMouseEvent& event, const glm::ivec2& delta);
    void reloadBrowser();

    const CefRefPtr<CefBrowser>& getBrowser() const;

    bool hasContent(int x, int y);

    const static int SingleClick = 1;

private:
    CefRefPtr<WebRenderHandler> _renderHandler;
    CefRefPtr<BrowserClient> _client;
    CefRefPtr<CefBrowser> _browser;
    bool _isInitialized = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H
