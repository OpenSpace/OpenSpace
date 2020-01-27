/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/webbrowser/include/screenspacebrowser.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* KeyIdentifier = "Indentifier";
    constexpr const char* _loggerCat = "ScreenSpaceBrowser";

    const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser windows."
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

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::draw() {}

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::render() {}

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::setTexture(GLuint t) {
    _texture = t;
}

ScreenSpaceBrowser::ScreenSpaceBrowser(const ghoul::Dictionary &dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _url(UrlInfo)
    , _dimensions(DimensionsInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(3000.f))
    , _reload(ReloadInfo)
{

    std::string identifier;
    if (dictionary.hasKeyAndValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceBrowser";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(identifier);

    if (dictionary.hasKeyAndValue<std::string>(UrlInfo.identifier)) {
        _url = dictionary.value<std::string>(UrlInfo.identifier);
    }

    glm::vec2 windowDimensions = global::windowDelegate.currentSubwindowSize();
    _dimensions = windowDimensions;

    _renderHandler = new ScreenSpaceRenderHandler();
    _keyboardHandler = new WebKeyboardHandler();
    _browserInstance = std::make_unique<BrowserInstance>(
        _renderHandler,
        _keyboardHandler
    );

    _url.onChange([this]() { _isUrlDirty = true; });
    _dimensions.onChange([this]() { _isDimensionsDirty = true; });
    _reload.onChange([this]() { _browserInstance->reloadBrowser(); });

    addProperty(_url);
    addProperty(_dimensions);
    addProperty(_reload);

    WebBrowserModule* webBrowser = global::moduleEngine.module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->addBrowser(_browserInstance.get());
    }
}

bool ScreenSpaceBrowser::initializeGL() {
    _texture = std::make_unique<ghoul::opengl::Texture>(
         glm::uvec3(_dimensions.value(), 1.0f)
    );

    _renderHandler->setTexture(*_texture);

    createShaders();

    _browserInstance->initialize();
    _browserInstance->loadUrl(_url);
    return isReady();
}

bool ScreenSpaceBrowser::deinitializeGL() {
    _renderHandler->setTexture(0);
    _texture = nullptr;

    std::string urlString;
    _url.getStringValue(urlString);
    LDEBUG(fmt::format("Deinitializing ScreenSpaceBrowser: {}", urlString));

    _browserInstance->close(true);

    WebBrowserModule* webBrowser = global::moduleEngine.module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->removeBrowser(_browserInstance.get());
        _browserInstance.reset();
    }
    else {
        LWARNING("Could not find WebBrowserModule");
    }

    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceBrowser::render() {
    if (!_renderHandler->isTextureReady()) {
        return;
    }
    _renderHandler->updateTexture();
    draw(
        globalRotationMatrix() *
        translationMatrix() *
        localRotationMatrix() *
        scaleMatrix()
    );
}

void ScreenSpaceBrowser::update() {
    _objectSize = _texture->dimensions();

    if (_isUrlDirty) {
        _browserInstance->loadUrl(_url);
        _isUrlDirty = false;
    }

    if (_isDimensionsDirty) {
        _browserInstance->reshape(_dimensions.value());
        _isDimensionsDirty = false;
    }
}

bool ScreenSpaceBrowser::isReady() const {
    return _shader && _texture;
}

void ScreenSpaceBrowser::bindTexture() {
    _texture->bind();
}

} // namespace openspace
