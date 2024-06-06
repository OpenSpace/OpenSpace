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

#include <modules/skybrowser/include/browser.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "Browser";

    constexpr openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser window.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UrlInfo = {
        "Url",
        "URL",
        "The URL to load.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reload the web browser.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(Browser)]] Parameters {
        // [[codegen::verbatim(DimensionsInfo.description)]]
        std::optional<glm::ivec2> dimensions;

        // [[codegen::verbatim(UrlInfo.description)]]
        std::optional<std::string> url;

        // [[codegen::verbatim(ReloadInfo.description)]]
        std::optional<bool> reload;
    };

#include "browser_codegen.cpp"
} // namespace

namespace openspace {

void Browser::RenderHandler::draw() {}

void Browser::RenderHandler::render() {}

void Browser::RenderHandler::setTexture(GLuint t) {
    _texture = t;
}

Browser::Browser(const ghoul::Dictionary& dictionary)
    : _browserDimensions(
        DimensionsInfo,
        glm::vec2(1000.f),
        glm::vec2(10.f),
        glm::vec2(3000.f)
    )
    , _url(UrlInfo)
    , _reload(ReloadInfo)
    , _renderHandler(new RenderHandler)
    , _keyboardHandler(new WebKeyboardHandler)
    , _browserInstance(
        std::make_unique<BrowserInstance>(_renderHandler.get(), _keyboardHandler.get())
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _url = p.url.value_or(_url);
    _url.onChange([this]() { _isUrlDirty = true; });

    _browserDimensions = p.dimensions.value_or(_browserDimensions);
    _browserDimensions.onChange([this]() { _isDimensionsDirty = true; });

    _reload.onChange([this]() { _shouldReload = true; });

    // Create browser and render handler
    _browserInstance = std::make_unique<BrowserInstance>(
        _renderHandler.get(),
        _keyboardHandler.get()
    );

    WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->addBrowser(_browserInstance.get());
    }
}

Browser::~Browser() {}

void Browser::initializeGL() {
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(glm::ivec2(_browserDimensions.value()), 1),
        GL_TEXTURE_2D
    );

    _renderHandler->setTexture(*_texture);

    _browserInstance->initialize();
    _browserInstance->loadUrl(_url);
    // Update the dimensions upon initialization. Do this with flag as it affects
    // derived classes as well
    _isDimensionsDirty = true;
}

void Browser::deinitializeGL() {
    _renderHandler->setTexture(0);

    _texture = nullptr;

    LDEBUG(std::format("Deinitializing browser '{}'", _url.value()));

    _browserInstance->close(true);

    WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->removeBrowser(_browserInstance.get());
        _browserInstance.reset();
    }
    else {
        LWARNING("Could not find WebBrowserModule");
    }
}

void Browser::render() {
    if (_renderHandler->isTextureReady()) {
        _renderHandler->updateTexture();
    }
}

void Browser::update() {
    if (_isUrlDirty) {
        _browserInstance->loadUrl(_url);
        _isUrlDirty = false;
    }

    if (_isDimensionsDirty) {
        updateBrowserDimensions();
    }

    if (_shouldReload) {
        _browserInstance->reloadBrowser();
        _shouldReload = false;
    }
}

bool Browser::isReady() const {
    return _texture.get();
}

// Updates the browser size to match the size of the texture
void Browser::updateBrowserSize() {
    _browserDimensions = _texture->dimensions();
}

void Browser::reload() {
    _reload.trigger();
}

void Browser::setRatio(float ratio) {
    const float relativeRatio = ratio / browserRatio();
    const float newX = static_cast<float>(_browserDimensions.value().x) * relativeRatio;
    _browserDimensions = {
        static_cast<int>(std::floor(newX)),
        _browserDimensions.value().y
    };
    _isDimensionsDirty = true;
}

float Browser::browserRatio() const {
    return static_cast<float>(_texture->dimensions().x) /
           static_cast<float>(_texture->dimensions().y);
}

void Browser::updateBrowserDimensions() {
    const glm::ivec2 dim = _browserDimensions;
    if (dim.x > 0 && dim.y > 0) {
        _texture->setDimensions(glm::uvec3(_browserDimensions.value(), 1));
        _browserInstance->reshape(dim);
        _isDimensionsDirty = false;
    }
}

void Browser::executeJavascript(const std::string& script) const {
    // Make sure that the browser has a main frame
    const bool browserExists = _browserInstance && _browserInstance->getBrowser();
    const bool frameIsLoaded =
        browserExists && _browserInstance->getBrowser()->GetMainFrame();

    if (frameIsLoaded) {
        const CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
        frame->ExecuteJavaScript(script, frame->GetURL(), 0);
    }
}

} // namespace openspace
