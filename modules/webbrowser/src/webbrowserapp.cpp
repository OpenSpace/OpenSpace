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

#include "include/webbrowserapp.h"
//#include <modules/webbrowser/include/webbrowserapp.h>

namespace openspace {

CefRefPtr<CefRenderProcessHandler> WebBrowserApp::GetRenderProcessHandler() {
    return this;
}

void WebBrowserApp::OnContextCreated(CefRefPtr<CefBrowser>, CefRefPtr<CefFrame>,
                                     CefRefPtr<CefV8Context>)
{
//    CEF_REQUIRE_UI_THREAD();
//    CefRefPtr<CefV8Value> val = CefV8Value::CreateBool(true);
//    CefRefPtr<CefV8Value> global = context->GetGlobal();
//    global->SetValue("IsWithinCEF", val, V8_PROPERTY_ATTRIBUTE_NONE);
}

void WebBrowserApp::OnBeforeCommandLineProcessing(const CefString&,
                                                  CefRefPtr<CefCommandLine> commandLine)
{
    commandLine->AppendSwitch("disable-gpu");
    commandLine->AppendSwitch("disable-gpu-compositing");
    commandLine->AppendSwitch("enable-begin-frame-scheduling");
    commandLine->AppendSwitchWithValue("autoplay-policy", "no-user-gesture-required");
}

} // namespace openspace
