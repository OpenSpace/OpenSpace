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

#include <modules/webbrowser/include/browserinstance.h>

#include <modules/webbrowser/include/browserclient.h>
#include <modules/webbrowser/include/webrenderhandler.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <filesystem>

namespace {
    constexpr std::string_view _loggerCat = "CEF BrowserInstance";
} // namespace

namespace openspace {

BrowserInstance::BrowserInstance(WebRenderHandler* renderer,
                                 WebKeyboardHandler* keyboardHandler)
    : _renderHandler(renderer)
    , _keyboardHandler(keyboardHandler)
    , _client(new BrowserClient(_renderHandler.get(), _keyboardHandler.get()))
{
    CefWindowInfo windowInfo;
    // On Windows and MacOS this function takes a pointer as a parameter, but Linux
    // requires this to be a long unsigned int, so we can't use nullptr here
    windowInfo.SetAsWindowless(0);

    CefBrowserSettings browserSettings;
    browserSettings.windowless_frame_rate = 60;

    _browser = CefBrowserHost::CreateBrowserSync(
        windowInfo,
        _client.get(),
        "",
        browserSettings,
        nullptr,
        nullptr
    );

    if (!_browser) {
        LERROR("Error when creating browser");
    }
}

BrowserInstance::~BrowserInstance() {
    _browser->GetHost()->CloseBrowser(false);
}

void BrowserInstance::initialize() {
    reshape(static_cast<glm::ivec2>(
        static_cast<glm::vec2>(global::windowDelegate->guiWindowResolution()) *
        global::windowDelegate->dpiScaling()
    ));
    _isInitialized = true;
    _shouldReshape = true;
}

void BrowserInstance::loadUrl(const std::string& url) {
    ghoul_assert(_isInitialized, "BrowserInstance should be initialized");

    if (!url.empty()) {
        LDEBUG(std::format("Loading URL '{}'", url));
        _browser->GetMainFrame()->LoadURL(url);
    }
    else {
        LWARNING("Provided browser URL is empty");
    }
}

bool BrowserInstance::loadLocalPath(std::string path) {
    if (!std::filesystem::is_regular_file(path)) {
        LDEBUG(std::format("Could not find path '{}', verify that it is correct", path));
        return false;
    }

    loadUrl(absPath(std::move(path)).string());
    return true;
}

void BrowserInstance::reshape(const glm::ivec2& windowSize) {
    ZoneScoped;

    _renderHandler->reshape(windowSize.x, windowSize.y);
    _browser->GetHost()->WasResized();
}

void BrowserInstance::draw() {
    ZoneScoped;
    TracyGpuZone("CEF Draw");


    if (_zoomLevel != _browser->GetHost()->GetZoomLevel()) {
        _browser->GetHost()->SetZoomLevel(_zoomLevel);
    }
    _renderHandler->draw();
}

void BrowserInstance::close(bool force) {
    if (force) {
        LDEBUG("Force closing browser");
    }
    else {
        LDEBUG("Closing browser");
    }
    _browser->GetHost()->CloseBrowser(force);
}

const CefRefPtr<CefBrowser>& BrowserInstance::getBrowser() const {
    return _browser;
}

bool BrowserInstance::sendKeyEvent(const CefKeyEvent& event) {
    _browser->GetHost()->SendKeyEvent(event);
    return false;
}

bool BrowserInstance::sendMouseClickEvent(const CefMouseEvent& event,
                                          CefBrowserHost::MouseButtonType button,
                                          bool mouseUp, int clickCount)
{
    _browser->GetHost()->SendMouseClickEvent(event, button, mouseUp, clickCount);
    return hasContent(glm::ivec2(event.x, event.y));
}

#ifdef WIN32
void BrowserInstance::sendTouchEvent(const CefTouchEvent& event) const{
    _browser->GetHost()->SendTouchEvent(event);
}
#endif // WIN32

bool BrowserInstance::sendMouseMoveEvent(const CefMouseEvent& event) {
    constexpr bool DidNotLeaveWindow = false;

    _browser->GetHost()->SendMouseMoveEvent(event, DidNotLeaveWindow);
    return false;
}

bool BrowserInstance::sendMouseWheelEvent(const CefMouseEvent& event,
                                          const glm::ivec2& delta)
{
    _browser->GetHost()->SendMouseWheelEvent(event, delta.x, delta.y);
    return hasContent(glm::ivec2(event.x, event.y));
}

void BrowserInstance::setZoom(float ratio) {
    const float dpiScaling = global::windowDelegate->dpiScaling().x;

    // Zooming in CEF is non-linear according to this:
    // https://www.magpcss.org/ceforum/viewtopic.php?f=6&t=11491
    _zoomLevel = glm::log(ratio * dpiScaling) / glm::log(1.2);
    _browser->GetHost()->SetZoomLevel(_zoomLevel);
}

void BrowserInstance::reloadBrowser() {
    _browser->Reload();
}

void BrowserInstance::selectAll() {
    if (_browser->GetFocusedFrame()) {
        _browser->GetFocusedFrame()->SelectAll();
    }
}

bool BrowserInstance::hasContent(const glm::ivec2& pos) const {
    return _renderHandler->hasContent(pos.x, pos.y);
}

} // namespace openspace
