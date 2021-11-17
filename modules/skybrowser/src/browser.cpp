/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/skybrowser/include/browser.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>
#include <apps\OpenSpace\ext\sgct\include\sgct\utils\box.h>

namespace {
    constexpr const char* _loggerCat = "Browser";

    const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser window."
    };
    const openspace::properties::Property::PropertyInfo UrlInfo = {
        "Url",
        "URL",
        "The URL to load"
    };

    const openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reload the web browser"
    };

} // namespace

namespace openspace {

    void Browser::RenderHandler::draw() {}

    void Browser::RenderHandler::render() {}

    void Browser::RenderHandler::setTexture(GLuint t) {
        _texture = t;
    }


    Browser::Browser(const ghoul::Dictionary& dictionary)
        : _url(UrlInfo)
        , _dimensions(DimensionsInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(3000.f))
        , _reload(ReloadInfo)
    { 
        if (dictionary.hasValue<std::string>(UrlInfo.identifier)) {
            _url = dictionary.value<std::string>(UrlInfo.identifier);
        }

        glm::vec2 windowDimensions = global::windowDelegate->currentSubwindowSize();
        _dimensions = windowDimensions;

        _url.onChange([this]() { _isUrlDirty = true; });
        _dimensions.onChange([this]() { _isDimensionsDirty = true; });
        _reload.onChange([this]() { _browserInstance->reloadBrowser(); });

        // Create browser and render handler
        _renderHandler = new RenderHandler();
        _keyboardHandler = new WebKeyboardHandler();
        _browserInstance = std::make_unique<BrowserInstance>(
            _renderHandler,
            _keyboardHandler
            );

        WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
        if (webBrowser) {
            webBrowser->addBrowser(_browserInstance.get());
        } 
    }

    Browser::~Browser() {
        // Delete
        _browserInstance.reset();
        _texture.reset();
    }

    bool Browser::initializeGL() {
        _texture = std::make_unique<ghoul::opengl::Texture>(
            glm::uvec3(_dimensions.value(), 1.0f)
            );

        _renderHandler->setTexture(*_texture);

        _browserInstance->initialize();
        _browserInstance->loadUrl(_url);
        return isReady();
    }


    bool Browser::deinitializeGL() {
        _renderHandler->setTexture(0);
        _texture = nullptr;

        std::string urlString;
        _url.getStringValue(urlString);
        LDEBUG(fmt::format("Deinitializing ScreenSpaceBrowser: {}", urlString));

        _browserInstance->close(true);

        WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
        if (webBrowser) {
            webBrowser->removeBrowser(_browserInstance.get());
            _browserInstance.reset();
        }
        else {
            LWARNING("Could not find WebBrowserModule");
        }

        return true;
    }

    void Browser::render() {
        if (!_renderHandler->isTextureReady()) {
            return;
        }
        _renderHandler->updateTexture();
        
    }

    void Browser::update() {

        if (_isUrlDirty) {
            _browserInstance->loadUrl(_url);
            _isUrlDirty = false;
        }

        if (_isDimensionsDirty) {
            _browserInstance->reshape(_dimensions.value());
            _isDimensionsDirty = false;
        }
    }

    bool Browser::isReady() const {
        return _texture.get();
    }

    void Browser::bindTexture() {
        _texture->bind();
    }

    glm::vec2 Browser::browserPixelDimensions() const {
        return _dimensions.value();
    }


} // namespace openspace
