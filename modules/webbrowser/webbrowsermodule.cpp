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

#include <modules/webbrowser/webbrowsermodule.h>

#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/cefhost.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* _loggerCat = "WebBrowser";

    #ifdef _MSC_VER
        constexpr const char* SubprocessSuf = ".exe";
    #elif __APPLE__
        constexpr const char* SubprocessSuf = ".app/Contents/MacOS/openspace_web_helper";
    #else
        constexpr const char* SubprocessSuf = "";
    #endif
} // namespace

namespace openspace {

WebBrowserModule::WebBrowserModule() : OpenSpaceModule(WebBrowserModule::Name) {
    global::callback::deinitialize.emplace_back([this]() { deinitialize(); });
}

void WebBrowserModule::internalDeinitialize() {
    if (!_enabled) {
        return;
    }

    _eventHandler.detachBrowser();

    bool forceBrowserShutdown = true;
    for (BrowserInstance* browser : _browsers) {
        browser->close(forceBrowserShutdown);
    }
}

std::string WebBrowserModule::findHelperExecutable() {
    std::string execLocation = absPath(_webHelperLocation + SubprocessSuf);
    if (!FileSys.fileExists(execLocation)) {
        LERROR(fmt::format(
            "Could not find web helper executable at location: {}" , execLocation
        ));
    }
    return execLocation;

}

void WebBrowserModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    _webHelperLocation = dictionary.value<std::string>("WebHelperLocation");
    if (dictionary.hasKeyAndValue<bool>("Enabled")) {
        _enabled = dictionary.value<bool>("Enabled");
    }

    const bool isGuiWindow =
        global::windowDelegate.hasGuiWindow() ?
        global::windowDelegate.isGuiWindow() :
        true;
    const bool isMaster = global::windowDelegate.isMaster();

    if (!_enabled || !isGuiWindow || !isMaster) {
        return;
    }

    LDEBUG("Starting CEF...");
    std::string helperLocation = findHelperExecutable();
    LDEBUG("Using web helper executable: " + helperLocation);
    _cefHost = std::make_unique<CefHost>(std::move(helperLocation));
    LDEBUG("Starting CEF... done!");

    global::callback::preSync.emplace_back([this]() {
        if (_cefHost && !_browsers.empty()) {
            _cefHost->doMessageLoopWork();
        }
    });

    _eventHandler.initialize();

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");
}

void WebBrowserModule::doMessageLoopWork() {
    if (_cefHost && !_browsers.empty()) {
        _cefHost->doMessageLoopWork();
    }
}

void WebBrowserModule::addBrowser(BrowserInstance* browser) {
    if (_enabled) {
        _browsers.push_back(browser);
    }
}

void WebBrowserModule::removeBrowser(BrowserInstance* browser) {
    if (!_enabled) {
        return;
    }
    const auto p = std::find(_browsers.begin(), _browsers.end(), browser);
    if (p != _browsers.end()) {
        _browsers.erase(p);
    } else {
        LWARNING("Could not find browser in list of browsers.");
    }

    LDEBUG(fmt::format("Number of browsers stored: {}", _browsers.size()));
}

void WebBrowserModule::attachEventHandler(BrowserInstance* browserInstance) {
    if (_enabled) {
        _eventHandler.setBrowserInstance(browserInstance);
    }
}

void WebBrowserModule::detachEventHandler() {
    if (_enabled) {
        _eventHandler.setBrowserInstance(nullptr);
    }
}

bool WebBrowserModule::isEnabled() const {
    return _enabled;
}

} // namespace openspace
