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

#ifndef __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERAPP___H__
#define __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERAPP___H__

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_app.h>
#include <include/wrapper/cef_helpers.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace openspace {

/**
 * Custom interface to CefApp and CefRenderProcessHandler. This allows us to
 * set global objects within, and modify, the DOM.
 */
class WebBrowserApp : public CefApp, public CefRenderProcessHandler {
public:
    WebBrowserApp() = default;

    CefRefPtr<CefRenderProcessHandler> GetRenderProcessHandler() override;
    void OnContextCreated(CefRefPtr<CefBrowser> browser, CefRefPtr<CefFrame> frame,
        CefRefPtr<CefV8Context> context) override;

private:
    IMPLEMENT_REFCOUNTING(WebBrowserApp);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERAPP___H__
