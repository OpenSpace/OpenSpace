/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <modules/webbrowser/include/eventhandler.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <filesystem>

namespace {
    constexpr std::string_view _loggerCat = "WebBrowser";

    #ifdef _MSC_VER
        constexpr std::string_view SubprocessPath = "OpenSpace_Helper.exe";
    #elif defined(__APPLE__)
        constexpr std::string_view SubprocessPath =
            "../Frameworks/OpenSpace Helper.app/Contents/MacOS/OpenSpace Helper";
    #else
        constexpr std::string_view SubprocessPath = "OpenSpace_Helper";
    #endif

    constexpr openspace::properties::Property::PropertyInfo
        UpdateBrowserBetweenRenderablesInfo =
    {
        "UpdateBrowserBetweenRenderables",
        "Update Browser Between Renderables",
        "Run the message loop of the browser between calls to render individual "
        "renderables. When disabled, the browser message loop only runs "
        "once per frame.",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo BrowserUpdateIntervalInfo = {
        "BrowserUpdateInterval",
        "Browser Update Interval",
        "The time in microseconds between running the message loop of the browser. "
        "Only used if UpdateBrowserBetweenRenderables is true.",
        openspace::properties::Property::Visibility::Developer
    };

    /**
     * Try to find the CEF Helper executable. It looks in the bin/openspace folder.
     * Therefore, if you change that this might cause a crash here.
     *
     * \return the absolute path to the file
     */
    std::filesystem::path findHelperExecutable() {
        const std::filesystem::path execLocation = absPath(std::format(
            "${{BIN}}/{}", SubprocessPath
        ));
        if (!std::filesystem::is_regular_file(execLocation)) {
            LERROR(std::format(
                "Could not find web helper executable at location: {}", execLocation
            ));
        }
        return execLocation;
    }

} // namespace

namespace openspace {

WebBrowserModule::WebBrowserModule()
    : OpenSpaceModule(WebBrowserModule::Name)
    , _updateBrowserBetweenRenderables(UpdateBrowserBetweenRenderablesInfo, true)
    , _browserUpdateInterval(BrowserUpdateIntervalInfo, 1.f, 1.f, 1000.f)
    , _eventHandler(new EventHandler)
{
    global::callback::deinitialize->emplace_back([this]() {
        ZoneScopedN("WebBrowserModule");

        deinitialize();
    });

    _browserUpdateInterval.onChange([this]() {
        webbrowser::interval = std::chrono::microseconds(
            static_cast<long long>(_browserUpdateInterval)
        );
    });

    _updateBrowserBetweenRenderables.onChange([this]() {
        if (_updateBrowserBetweenRenderables && !_browsers.empty()) {
            global::callback::webBrowserPerformanceHotfix = webbrowser::update;
        }
        else {
            global::callback::webBrowserPerformanceHotfix = nullptr;
        }
    });

    addProperty(_updateBrowserBetweenRenderables);
    addProperty(_browserUpdateInterval);
}

WebBrowserModule::~WebBrowserModule() {}

void WebBrowserModule::internalDeinitialize() {
    ZoneScoped;

    if (!_enabled) {
        return;
    }

    _eventHandler->resetBrowserInstance();

    const bool forceBrowserShutdown = true;
    for (BrowserInstance* browser : _browsers) {
        browser->close(forceBrowserShutdown);
    }
}

void WebBrowserModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    ZoneScoped;

    if (dictionary.hasValue<bool>("WebHelperLocation")) {
        _webHelperLocation = absPath(dictionary.value<std::string>("WebHelperLocation"));
    }
    else {
        _webHelperLocation = findHelperExecutable();
    }

    if (dictionary.hasValue<bool>("Enabled")) {
        _enabled = dictionary.value<bool>("Enabled");
    }

    LDEBUG(std::format("CEF using web helper executable: {}", _webHelperLocation));
    _cefHost = std::make_unique<CefHost>(_webHelperLocation.string());
    LDEBUG("Starting CEF... done");

    if (dictionary.hasValue<bool>(UpdateBrowserBetweenRenderablesInfo.identifier)) {
        _updateBrowserBetweenRenderables =
            dictionary.value<bool>(UpdateBrowserBetweenRenderablesInfo.identifier);
    }

    if (dictionary.hasValue<double>(BrowserUpdateIntervalInfo.identifier)) {
        _browserUpdateInterval = static_cast<float>(
            dictionary.value<double>(BrowserUpdateIntervalInfo.identifier)
        );
    }

    _eventHandler->initialize();

    // register ScreenSpaceBrowser
    ghoul::TemplateFactory<ScreenSpaceRenderable>* fScreenSpaceRenderable =
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");
}

void WebBrowserModule::addBrowser(BrowserInstance* browser) {
    ZoneScoped;

    if (_enabled) {
        _browsers.push_back(browser);
        if (_updateBrowserBetweenRenderables) {
            global::callback::webBrowserPerformanceHotfix = webbrowser::update;
        }
    }
}

void WebBrowserModule::removeBrowser(BrowserInstance* browser) {
    if (!_enabled) {
        return;
    }
    const auto p = std::find(_browsers.begin(), _browsers.end(), browser);
    if (p != _browsers.end()) {
        _browsers.erase(p);
    }
    else {
        LWARNING("Could not find browser in list of browsers");
    }

    if (_browsers.empty()) {
        global::callback::webBrowserPerformanceHotfix = nullptr;
    }

    LDEBUG(std::format("Number of browsers stored: {}", _browsers.size()));
}

void WebBrowserModule::attachEventHandler(BrowserInstance* browserInstance) {
    if (_enabled) {
        _eventHandler->setBrowserInstance(browserInstance);
    }
}

void WebBrowserModule::detachEventHandler() {
    if (_enabled) {
        _eventHandler->setBrowserInstance(nullptr);
    }
}

bool WebBrowserModule::isEnabled() const {
    return _enabled;
}

std::vector<documentation::Documentation> WebBrowserModule::documentations() const {
    return {
        ScreenSpaceBrowser::Documentation()
    };
}

/// Logic for the webbrowser performance hotfix, described in globalscallbacks.h
namespace webbrowser {

/**
 * The time interval to describe how often the CEF message loop needs to be pumped to work
 * properly. A value of 10000 us updates CEF a 100 times per second which is enough for
 * fluid interaction without wasting resources
 */
std::chrono::microseconds interval = std::chrono::microseconds(10000);
std::chrono::time_point<std::chrono::high_resolution_clock> latestCall;
CefHost* cefHost = nullptr;

void update() {
    const std::chrono::time_point<std::chrono::high_resolution_clock> timeBefore =
        std::chrono::high_resolution_clock::now();

    const std::chrono::microseconds duration =
        std::chrono::duration_cast<std::chrono::microseconds>(timeBefore - latestCall);

    if (duration > interval) {
        cefHost->doMessageLoopWork();

        const std::chrono::time_point<std::chrono::high_resolution_clock> timeAfter =
            std::chrono::high_resolution_clock::now();

        latestCall = timeAfter;
    }
}

} // namespace webbrowser

} // namespace openspace
