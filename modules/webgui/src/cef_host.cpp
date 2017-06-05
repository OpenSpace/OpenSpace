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

#include "include/cef_host.h"

namespace {
std::string _loggerCat = "WebGUI";
}

namespace openspace {

CefHost::CefHost() {
    LDEBUG("Initializing CEF...");
    CefMainArgs args;
    CefSettings settings;

    CefString(&settings.browser_subprocess_path).FromASCII((char*) SUBPROCESS_PATH.c_str());
    attachDebugSettings(settings);

    CefInitialize(args, settings, nullptr, NULL);
    LDEBUG("Initializing CEF... done!");

    renderHandler = new GUIRenderHandler();
    client = new BrowserClient(renderHandler);

    CefBrowserSettings browserSettings;
    CefWindowInfo windowInfo;
    bool renderTransparent = true;
    windowInfo.SetAsWindowless(nullptr, renderTransparent);
    std::string url = "";
    browser = CefBrowserHost::CreateBrowserSync(windowInfo, client.get(), url, browserSettings, NULL);
    eventHandler = std::make_shared<EventHandler>(EventHandler(browser));
}

CefHost::~CefHost() {
    // TODO(klas): This is causing a crash during shutdown
    browser->GetHost()->CloseBrowser(true);

    CefShutdown();
}

void CefHost::internalInitialize() {
    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Initialize,
            [this](){
                LDEBUG("Initializing CefHost members");
                initialize();
                renderHandler->initialize();
                eventHandler->initialize();
                initializeCallbacks();
            }
    );
    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Deinitialize,
            [this](){
                LDEBUG("Deinitializing CEF...");
                deinitialize();
                LDEBUG("Deinitializing CEF... done!");
            }
    );
}

void CefHost::initialize() {
//    loadLocalPath("${MODULE_WEBGUI}/web/transparent_test.html");
    load("http://localhost:8080/");
    WindowWrapper& wrapper = OsEng.windowWrapper();
    reshape(wrapper);
}

void CefHost::attachDebugSettings(CefSettings &settings) {
    settings.remote_debugging_port = 8088;
    LDEBUG(fmt::format("Remote WebGUI debugging available on http://localhost:{}", settings.remote_debugging_port));
}

void CefHost::deinitialize() {
}

void CefHost::reshape(WindowWrapper& wrapper) {
    glm::ivec2 windowSize = wrapper.currentWindowSize();
    renderHandler->reshape(windowSize.x, windowSize.y);
    browser->GetHost()->WasResized();
}

void CefHost::initializeCallbacks() {
    OsEng.registerModuleCallback(
            // This is done in the PostDraw phase so that it will render it on top of
            // everything else in the case of fisheyes. With this being in the Render callback
            // the GUI would be rendered on top of each of the cube faces
            OpenSpaceEngine::CallbackOption::Render,
            [this](){
                render();
            }
    );

}

/**
 * Load a local file
 * @param path - the path to load
 * @return true if the path exists, false otherwise
 */
bool CefHost::loadLocalPath(std::string path) {
    if (!FileSys.fileExists(path)) {
        LDEBUG(fmt::format("Could not find path `{}`, verify that it is correct.", path));
        return false;
    }

    load(absPath(path));
    return true;
}

void CefHost::load(const std::string &url) {
    LDEBUG(fmt::format("Loading URL: {}", url));
    browser->GetMainFrame()->LoadURL(url);
}

void CefHost::render() {
    WindowWrapper& wrapper = OsEng.windowWrapper();

    if (wrapper.isMaster()) {
        if (wrapper.windowHasResized()) {
            reshape(wrapper);
        }

        CefDoMessageLoopWork();
        renderHandler->draw();
    }
}

}

