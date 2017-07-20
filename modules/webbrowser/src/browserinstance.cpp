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

#include <webbrowsermodule.h>
#include "include/browserinstance.h"

namespace {
    const char* _loggerCat = "CEF BrowserInstance";
}

namespace openspace {

BrowserInstance::BrowserInstance(WebRenderHandler* renderer) : _isInitialized(false) {
    _renderHandler = renderer;
    _client = new BrowserClient(_renderHandler);

    CefBrowserSettings browserSettings;
    CefWindowInfo windowInfo;
    bool renderTransparent = true;
    windowInfo.SetAsWindowless(nullptr, renderTransparent);
    std::string url = "";
    _browser = CefBrowserHost::CreateBrowserSync(windowInfo, _client.get(), url, browserSettings, NULL);

    // send to WebBrowserModule
    auto browserModule = OsEng.moduleEngine().module<WebBrowserModule>();
    if (browserModule) {
        browserModule->addBrowser(_browser);
    }
}

BrowserInstance::~BrowserInstance() {
    _browser->GetHost()->CloseBrowser(false);

    auto browserModule = OsEng.moduleEngine().module<WebBrowserModule>();
    if (browserModule) {
        browserModule->removeBrowser(_browser);
    }
}

void BrowserInstance::initialize() {
    WindowWrapper &wrapper = OsEng.windowWrapper();
    reshape(wrapper.currentWindowSize());

    _isInitialized = true;
}

void BrowserInstance::loadUrl(const std::string &url) {
    if (!_isInitialized) {
        initialize();
    }

    LDEBUG(fmt::format("Loading URL: {}", url));
    _browser->GetMainFrame()->LoadURL(url);
}

/**
 * Load a local file
 * @param path - the path to load
 * @return true if the path exists, false otherwise
 */
bool BrowserInstance::loadLocalPath(std::string path) {
    if (!FileSys.fileExists(path)) {
        LDEBUG(fmt::format("Could not find path `{}`, verify that it is correct.", path));
        return false;
    }

    loadUrl(absPath(path));
    return true;
}

/**
 * Call when the window has been reshaped
 * @param wrapper the windowWrapper capable of
 */
void BrowserInstance::reshape(const glm::ivec2 &windowSize) {
    _renderHandler->reshape(windowSize.x, windowSize.y);
    _browser->GetHost()->WasResized();
}

/**
 * encapsulate renderHandler's draw method
 */
void BrowserInstance::draw() {
    _renderHandler->draw();
}

const CefRefPtr<CefBrowser> &BrowserInstance::getBrowser() const {
    return _browser;
}

}
