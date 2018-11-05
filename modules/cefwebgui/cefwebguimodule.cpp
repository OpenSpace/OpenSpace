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
#include "cefwebguimodule.h"

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/cefwebgui/include/guirenderhandler.h>
#include <modules/cefwebgui/include/guikeyboardhandler.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "CefWebGui";
    
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether the browser should be enabled or not."
    };

    constexpr openspace::properties::Property::PropertyInfo VisibleInfo = {
        "Visible",
        "Is Visible",
        "This setting determines whether the browser should be visible or not."
    };

    constexpr openspace::properties::Property::PropertyInfo GuiUrlInfo = {
        "GuiUrl",
        "GUI URL",
        "The URL to load the Web GUI from"
    };
} // namespace

namespace openspace {

CefWebGuiModule::CefWebGuiModule()
    : OpenSpaceModule(CefWebGuiModule::Name)
    , _cefWebGuiEnabled(EnabledInfo, true)
    , _cefWebGuiVisible(VisibleInfo, true)
    , _guiUrl(GuiUrlInfo, "")
{
    addProperty(_cefWebGuiEnabled);
    addProperty(_cefWebGuiVisible);
    addProperty(_guiUrl);
}
    
void CefWebGuiModule::startOrStopGui() {
    WebBrowserModule* webBrowserModule =
        global::moduleEngine.module<WebBrowserModule>();
    
    const bool isGuiWindow =
        global::windowDelegate.hasGuiWindow() ?
        global::windowDelegate.isGuiWindow() :
        true;
    const bool isMaster = global::windowDelegate.isMaster();

    if (_cefWebGuiEnabled && isGuiWindow && isMaster) {
        LDEBUGC("WebBrowser", fmt::format("Loading GUI from {}", _guiUrl));
        
        if (!_guiInstance) {
            _guiInstance = std::make_shared<BrowserInstance>(
                new GUIRenderHandler,
                new GUIKeyboardHandler
            );
            _guiInstance->loadUrl(_guiUrl);
        }
        webBrowserModule->attachEventHandler(_guiInstance);
        webBrowserModule->addBrowser(_guiInstance);
    } else if (_guiInstance) {
        _guiInstance->close(true);
        webBrowserModule->removeBrowser(_guiInstance);
        webBrowserModule->detachEventHandler();
        _guiInstance.reset();
    }
}
    
void CefWebGuiModule::updateUrl() {
    if (_guiInstance) {
        _guiInstance->loadUrl(_guiUrl);
    }
}

void CefWebGuiModule::internalInitialize(const ghoul::Dictionary& configuration) {
    WebBrowserModule* webBrowserModule =
        global::moduleEngine.module<WebBrowserModule>();

    _webBrowserIsAvailable = webBrowserModule && webBrowserModule->isEnabled();

    if (!_webBrowserIsAvailable) {
        return;
    }

    _cefWebGuiEnabled.onChange([this]() {
        startOrStopGui();
    });

    _guiUrl.onChange([this]() {
        updateUrl();
    });

    _guiUrl = configuration.value<std::string>(GuiUrlInfo.identifier);

    _cefWebGuiEnabled = _webBrowserIsAvailable &&
        configuration.hasValue<bool>(EnabledInfo.identifier) &&
        configuration.value<bool>(EnabledInfo.identifier);

    _cefWebGuiVisible = _webBrowserIsAvailable &&
        configuration.hasValue<bool>(VisibleInfo.identifier) &&
        configuration.value<bool>(VisibleInfo.identifier);

    global::callback::initializeGL.push_back([this]() {
        startOrStopGui();
    });

    global::callback::draw2D.push_back([this](){
        const bool isGuiWindow =
            global::windowDelegate.hasGuiWindow() ?
            global::windowDelegate.isGuiWindow() :
            true;
        const bool isMaster = global::windowDelegate.isMaster();


        if (isGuiWindow && isMaster && _guiInstance) {
            if (global::windowDelegate.windowHasResized()) {
                _guiInstance->reshape(global::windowDelegate.currentWindowSize());
            }
            if (_cefWebGuiVisible) {
                _guiInstance->draw();
            }
        }
    });

    global::callback::deinitializeGL.push_back([this]() {
        _cefWebGuiEnabled = false;
        startOrStopGui();
    });
}

} // namespace openspace
