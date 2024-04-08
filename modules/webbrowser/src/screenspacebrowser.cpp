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

#include <modules/webbrowser/include/screenspacebrowser.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "ScreenSpaceBrowser";

    constexpr openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser windows",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UrlInfo = {
        "Url",
        "URL",
        "The URL to load",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reload the web browser",
        openspace::properties::Property::Visibility::NoviceUser
    };

    struct [[codegen::Dictionary(ScreenSpaceBrowser)]] Parameters {
        std::optional<std::string> identifier;
        std::optional<std::string> url;
        std::optional<glm::vec2> dimensions;
    };
#include "screenspacebrowser_codegen.cpp"

} // namespace

namespace openspace {

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::draw() {}

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::render() {}

void ScreenSpaceBrowser::ScreenSpaceRenderHandler::setTexture(GLuint t) {
    _texture = t;
}

ScreenSpaceBrowser::ScreenSpaceBrowser(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _dimensions(DimensionsInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(3000.f))
    , _renderHandler(new ScreenSpaceRenderHandler)
    , _url(UrlInfo)
    , _reload(ReloadInfo)
    , _keyboardHandler(new WebKeyboardHandler)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceBrowser");
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(identifier);

    _url = p.url.value_or(_url);

    const glm::vec2 windowDimensions = global::windowDelegate->currentSubwindowSize();
    _dimensions = p.dimensions.value_or(windowDimensions);

    _browserInstance = std::make_unique<BrowserInstance>(
        _renderHandler.get(),
        _keyboardHandler.get()
    );

    _url.onChange([this]() { _isUrlDirty = true; });
    _dimensions.onChange([this]() { _isDimensionsDirty = true; });
    _reload.onChange([this]() { _browserInstance->reloadBrowser(); });

    addProperty(_url);
    addProperty(_dimensions);
    addProperty(_reload);

    WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->addBrowser(_browserInstance.get());
    }
}

bool ScreenSpaceBrowser::initializeGL() {
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(_dimensions.value(), 1),
        GL_TEXTURE_2D
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

    LDEBUG(std::format("Deinitializing ScreenSpaceBrowser: {}", _url.value()));

    _browserInstance->close(true);

    WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->removeBrowser(_browserInstance.get());
        _browserInstance.reset();
    }
    else {
        LWARNING("Could not find WebBrowserModule");
    }

    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceBrowser::render(float blackoutFactor) {
    if (!_renderHandler->isTextureReady()) {
        return;
    }

    _renderHandler->updateTexture();
    const glm::mat4 mat =
        globalRotationMatrix() *
        translationMatrix() *
        localRotationMatrix() *
        scaleMatrix();
    draw(mat, blackoutFactor);
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
