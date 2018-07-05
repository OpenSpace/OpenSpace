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

#ifndef __OPENSPACE_MODULE_WEBBROWSER___DEFAULT_BROWSER_LAUNCHER___H__
#define __OPENSPACE_MODULE_WEBBROWSER___DEFAULT_BROWSER_LAUNCHER___H__

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_life_span_handler.h>
#include <include/cef_request_handler.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace openspace {

class DefaultBrowserLauncher : public CefLifeSpanHandler, public CefRequestHandler {
public:
    bool OnBeforePopup(CefRefPtr<CefBrowser> parentBrowser,
        const CefPopupFeatures& popupFeatures, CefWindowInfo& windowInfo,
        const CefString& url, CefRefPtr<CefClient>& client, CefBrowserSettings& settings);

    bool OnOpenURLFromTab(CefRefPtr<CefBrowser> browser, CefRefPtr<CefFrame> frame,
        const CefString& target_url,
        CefRequestHandler::WindowOpenDisposition target_disposition, bool userGesture);

private:
    void launchBrowser(const std::string &url) const;
    IMPLEMENT_REFCOUNTING(DefaultBrowserLauncher);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___DEFAULT_BROWSER_LAUNCHER___H__
