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

#include <modules/webbrowser/include/browserclient.h>

#include <modules/webbrowser/include/defaultbrowserlauncher.h>
#include <modules/webbrowser/include/webrenderhandler.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>

namespace openspace {

BrowserClient::BrowserClient(WebRenderHandler* handler, WebKeyboardHandler *keyboardHandler)
    : _renderHandler(handler),
    _keyboardHandler(keyboardHandler)
{
    DefaultBrowserLauncher* browserLauncher = new DefaultBrowserLauncher;
    _lifeSpanHandler = browserLauncher;
    _requestHandler = browserLauncher;
};

CefRefPtr<CefRenderHandler> BrowserClient::GetRenderHandler() {
    return _renderHandler;
}

CefRefPtr<CefLifeSpanHandler> BrowserClient::GetLifeSpanHandler() {
    return _lifeSpanHandler;
}

CefRefPtr<CefRequestHandler> BrowserClient::GetRequestHandler() {
    return _requestHandler;
}

CefRefPtr<CefKeyboardHandler> BrowserClient::GetKeyboardHandler() {
    return _keyboardHandler;
}

} // namespace openspace
