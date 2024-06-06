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

#include <modules/webbrowser/include/cefhost.h>

#include <modules/webbrowser/include/webbrowserapp.h>
#include <openspace/engine/globalscallbacks.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <format>

#ifdef __APPLE__
#include <include/wrapper/cef_library_loader.h>
#endif // __APPLE__

namespace {
    constexpr std::string_view _loggerCat = "CefHost";
} // namespace

namespace openspace {

CefHost::CefHost([[maybe_unused]] const std::string& helperLocation) {
    LDEBUG("Initializing CEF...");

    CefSettings settings;

#ifndef __APPLE__
    // Apple will always look for helper in a fixed location
    CefString(&settings.browser_subprocess_path).FromString(helperLocation);
#endif // __APPLE__

    settings.windowless_rendering_enabled = true;
    attachDebugSettings(settings);

#ifdef WIN32
    // Enable High-DPI support on Windows 7 or newer
    CefEnableHighDPISupport();
#endif // WIN32

#ifdef __APPLE__
    // Load the CEF framework library at runtime instead of linking directly as required
    // by the macOS sandbox implementation
    CefScopedLibraryLoader library_loader;
    if (!library_loader.LoadInMain()) {
        return;
    }
#endif // __APPLE__

    const CefRefPtr<WebBrowserApp> app = CefRefPtr<WebBrowserApp>(new WebBrowserApp);

    const CefMainArgs args;
    CefInitialize(args, settings, app.get(), nullptr);
    LDEBUG("Initializing CEF... done");
}

CefHost::~CefHost() {
    CefShutdown();
}

void CefHost::attachDebugSettings(CefSettings &settings) {
    settings.remote_debugging_port = 8088;

    LDEBUG(std::format(
        "Remote WebBrowser debugging available on http://localhost:{}",
        settings.remote_debugging_port
    ));
}

void CefHost::doMessageLoopWork() {
    ZoneScoped;

    CefDoMessageLoopWork();
}

} // namespace openspace
