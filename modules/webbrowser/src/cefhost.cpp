/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>
#include <include/cef_app.h>
#include <filesystem>
#include <string_view>

#ifdef WIN32
#pragma warning(push)
#pragma warning(disable : 4100)
#endif // WIN32

#include <include/wrapper/cef_helpers.h>

#ifdef WIN32
#pragma warning(pop)
#endif // WIN32

struct CefSettingsTraits;
template <typename T> class CefStructBase;
using CefSettings = CefStructBase<CefSettingsTraits>;

namespace {
    constexpr std::string_view _loggerCat = "CefHost";
} // namespace

namespace openspace {

CefHost::CefHost([[maybe_unused]] const std::string& helperLocation) {
    LDEBUG("Initializing CEF...");

    CefSettings settings;

#ifndef CEF_USE_SANDBOX
    LDEBUG("Disabling sandbox for CEF");
    settings.no_sandbox = 1;
#endif

    const std::filesystem::path root =
        std::filesystem::path(helperLocation).parent_path();
    const std::filesystem::path cefcache = std::format("{}/cefcache", root);
    CefString(&settings.root_cache_path).FromString(cefcache.string());
    CefString(&settings.browser_subprocess_path).FromString(helperLocation);

    settings.windowless_rendering_enabled = 1;

    settings.remote_debugging_port = 8088;
    LDEBUG(std::format(
        "Remote WebBrowser debugging available on http://localhost:{}",
        settings.remote_debugging_port
    ));

    // cf. https://github.com/chromiumembedded/cef/issues/3685
    settings.chrome_runtime = 1;

    const CefRefPtr<WebBrowserApp> app = CefRefPtr<WebBrowserApp>(new WebBrowserApp);

    const CefMainArgs args;
    const bool success = CefInitialize(args, settings, app.get(), nullptr);
    if (!success) {
        throw ghoul::RuntimeError("Failed initializing CEF Browser");
    }
    LDEBUG("Initializing CEF... done");
}

CefHost::~CefHost() {
    CefShutdown();
}

void CefHost::doMessageLoopWork() {
    ZoneScoped;

    CefDoMessageLoopWork();
}

} // namespace openspace
