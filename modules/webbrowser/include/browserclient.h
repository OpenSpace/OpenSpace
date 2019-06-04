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

#ifndef __OPENSPACE_MODULE_WEBBROWSER___BROWSER_CLIENT___H__
#define __OPENSPACE_MODULE_WEBBROWSER___BROWSER_CLIENT___H__

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_client.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace openspace {

class WebRenderHandler;
class WebKeyboardHandler;

class BrowserClient : public CefClient {
public:

    class NoContextMenuHandler : public CefContextMenuHandler {
        bool RunContextMenu(CefRefPtr<CefBrowser> browser, CefRefPtr<CefFrame> frame,
            CefRefPtr<CefContextMenuParams> params, CefRefPtr<CefMenuModel> model,
            CefRefPtr<CefRunContextMenuCallback> callback) override;

        IMPLEMENT_REFCOUNTING(NoContextMenuHandler);
    };

    BrowserClient(WebRenderHandler* handler, WebKeyboardHandler* keyboardHandler);

    CefRefPtr<CefRenderHandler> GetRenderHandler() override;
    CefRefPtr<CefLifeSpanHandler> GetLifeSpanHandler() override;
    CefRefPtr<CefRequestHandler> GetRequestHandler() override;
    CefRefPtr<CefKeyboardHandler> GetKeyboardHandler() override;
    CefRefPtr<CefContextMenuHandler> GetContextMenuHandler() override;

private:
    CefRefPtr<CefRenderHandler> _renderHandler;
    CefRefPtr<CefKeyboardHandler> _keyboardHandler;
    CefRefPtr<CefLifeSpanHandler> _lifeSpanHandler;
    CefRefPtr<CefRequestHandler> _requestHandler;
    CefRefPtr<CefContextMenuHandler> _contextMenuHandler;

    IMPLEMENT_REFCOUNTING(BrowserClient);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___BROWSER_CLIENT___H__
