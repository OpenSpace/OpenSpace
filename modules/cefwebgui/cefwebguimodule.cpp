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

#include <openspace/engine/configuration.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/cefwebgui/include/guirenderhandler.h>

namespace {
    constexpr const char* _loggerCat = "CefWebGui";
} // namespace

namespace openspace {

CefWebGuiModule::CefWebGuiModule()
    : OpenSpaceModule(CefWebGuiModule::Name)
{}

void CefWebGuiModule::internalInitialize(const ghoul::Dictionary&) {
    _guiInstance = std::make_shared<BrowserInstance>(new GUIRenderHandler);
    _guiLocation = OsEng.configuration().cefWebGuiUrl;

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Initialize,
        [this]() {
            LDEBUGC("WebBrowser", fmt::format("Loading GUI from {}", _guiLocation));
            _guiInstance->loadUrl(_guiLocation);
            WebBrowserModule* webBrowserModule =
                OsEng.moduleEngine().module<WebBrowserModule>();

            if (webBrowserModule) {
                webBrowserModule->attachEventHandler(_guiInstance);
                webBrowserModule->addBrowser(_guiInstance);
            }
        }
    );
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Render,
        [this](){
            WindowWrapper& wrapper = OsEng.windowWrapper();
            if (wrapper.isMaster()) {
                if (wrapper.windowHasResized()) {
                    _guiInstance->reshape(wrapper.currentWindowSize());
                }

                _guiInstance->draw();
            }
        }
    );
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Deinitialize,
        [this]() {
            _guiInstance->close(true);
            WebBrowserModule* webBrowserModule =
                OsEng.moduleEngine().module<WebBrowserModule>();

            if (webBrowserModule) {
                webBrowserModule->removeBrowser(_guiInstance);
            }
            _guiInstance.reset();
        }
    );
}

} // namespace openspace
