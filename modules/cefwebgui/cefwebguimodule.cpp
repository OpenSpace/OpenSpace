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
#include <ghoul/misc/dictionary.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "CefWebGui";
    
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not."
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
    , _guiUrl(GuiUrlInfo, "")
{
    addProperty(_cefWebGuiEnabled);
    addProperty(_guiUrl);
    
    _cefWebGuiEnabled.onChange([this]() {
        startOrStopGui();
    });
    
    _guiUrl.onChange([this]() {
        updateUrl();
    });
}
    
void CefWebGuiModule::startOrStopGui() {
    WebBrowserModule* webBrowserModule =
        global::moduleEngine.module<WebBrowserModule>();
    
    if (!webBrowserModule) {
        return;
    }
    
    if (_cefWebGuiEnabled) {
        LDEBUGC("WebBrowser", fmt::format("Loading GUI from {}", _guiUrl));
        
        if (!_guiInstance) {
            _guiInstance = std::make_shared<BrowserInstance>(new GUIRenderHandler);
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
    _guiUrl = configuration.value<std::string>("Url");

    global::callback::initializeGL.push_back([this]() {
        startOrStopGui();
    });

    global::callback::draw2D.push_back([this](){
        WindowDelegate& windowDelegate = global::windowDelegate;
        if (windowDelegate.isMaster() && _guiInstance) {
            if (windowDelegate.windowHasResized()) {
                _guiInstance->reshape(windowDelegate.currentWindowSize());
            }
            _guiInstance->draw();
        }
    });

    global::callback::deinitializeGL.push_back([this]() {
        _cefWebGuiEnabled = false;
        startOrStopGui();
    });
}

} // namespace openspace
