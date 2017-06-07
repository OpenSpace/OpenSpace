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

#include "include/browser_instance.h"

namespace {
    const char* _loggerCat = "CEF BrowserInstance";
}

namespace openspace {

BrowserInstance::BrowserInstance(GUIRenderHandler* rh) : isInitialized(false) {
    renderHandler = rh;
    client = new BrowserClient(renderHandler);

    CefBrowserSettings browserSettings;
    CefWindowInfo windowInfo;
    bool renderTransparent = true;
    windowInfo.SetAsWindowless(nullptr, renderTransparent);
    std::string url = "";
    browser = CefBrowserHost::CreateBrowserSync(windowInfo, client.get(), url, browserSettings, NULL);
    eventHandler = std::make_shared<EventHandler>(EventHandler(browser));
}

BrowserInstance::~BrowserInstance() {
    browser->GetHost()->CloseBrowser(false);
}

void BrowserInstance::initialize() {
    renderHandler->initialize();
    eventHandler->initialize();

    WindowWrapper& wrapper = OsEng.windowWrapper();
    reshape(wrapper);

    isInitialized = true;

    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Render,
            [this](){
                WindowWrapper& wrapper = OsEng.windowWrapper();

                if (wrapper.isMaster()) {
                    if (wrapper.windowHasResized()) {
                        reshape(wrapper);
                    }

                    renderHandler->draw();
                }
            });
}

void BrowserInstance::load(const std::string& url) {
    if (!isInitialized) {
        initialize();
    }

    LDEBUG(fmt::format("Loading URL: {}", url));
    browser->GetMainFrame()->LoadURL(url);
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

    load(absPath(path));
    return true;
}

void BrowserInstance::reshape(WindowWrapper& wrapper) {
    glm::ivec2 windowSize = wrapper.currentWindowSize();
    renderHandler->reshape(windowSize.x, windowSize.y);
    browser->GetHost()->WasResized();
}

}