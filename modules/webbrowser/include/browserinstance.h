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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H__
#define __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H__

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#endif // __clang__

#include <include/cef_client.h>

#ifdef __clang__
#pragma clang diagnostic pop
#endif // __clang__

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include <ghoul/glm.h>
#include <string>

namespace openspace {

class BrowserClient;
class WebRenderHandler;
class WebKeyboardHandler;

class BrowserInstance {
public:
    static constexpr int SingleClick = 1;

    BrowserInstance(WebRenderHandler* renderer, WebKeyboardHandler* keyboardHandler);
    ~BrowserInstance();

    void loadUrl(std::string url);
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

#ifdef WIN32
    void sendTouchEvent(const CefTouchEvent& event) const;
#endif

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

    /**
     * Set the browser zoom level.
     * 1.0 = default, 2.0 = double, etc.
     */
    void setZoom(float ratio);

    void reloadBrowser();

    void selectAll();

    const CefRefPtr<CefBrowser>& getBrowser() const;

    bool hasContent(int x, int y);

    bool _shouldReshape = false;

private:
    CefRefPtr<WebRenderHandler> _renderHandler;
    CefRefPtr<WebKeyboardHandler> _keyboardHandler;
    CefRefPtr<BrowserClient> _client;
    CefRefPtr<CefBrowser> _browser;
    bool _isInitialized = false;
    double _zoomLevel = 1.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H__
