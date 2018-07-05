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

#include <modules/webbrowser/include/screenspacebrowser.h>

#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* KeyIdentifier = "Indentifier";
    constexpr const char* KeyUrl = "URL";
    constexpr const char* _loggerCat = "ScreenSpaceBrowser";

    const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser windows."
    };
    const openspace::properties::Property::PropertyInfo UrlInfo = {
        "URL",
        "url",
        "The URL to load"
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
{
    if (dictionary.hasKey(KeyIdentifier)) {
        setIdentifier(dictionary.value<std::string>(KeyIdentifier));
    } else {
        static int id = 0;
        setIdentifier("ScreenSpaceBrowser " + std::to_string(id));
        ++id;
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyUrl)) {
        _url = dictionary.value<std::string>(KeyUrl);
    }

    glm::vec2 windowDimensions = OsEng.windowWrapper().currentSubwindowSize();
    _dimensions = windowDimensions;

    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(windowDimensions, 1.0f)
    );

    _renderHandler = new ScreenSpaceRenderHandler();
    _browserInstance = std::make_shared<BrowserInstance>(_renderHandler);

    _url.onChange([this]() { _isUrlDirty = true; });
    _dimensions.onChange([this]() { _isDimensionsDirty = true; });

    addProperty(_url);
    addProperty(_dimensions);

    WebBrowserModule* webBrowser = OsEng.moduleEngine().module<WebBrowserModule>();
    if (webBrowser) {
        webBrowser->addBrowser(_browserInstance);
    }
}

bool ScreenSpaceBrowser::initialize() {
    _originalViewportSize = OsEng.windowWrapper().currentWindowSize();
    _renderHandler->setTexture(*_texture);

    createPlane();
    // Load a special version of the regular ScreenRenderable shaders. This mirrors the
    // image along the Y axis since the image produced by CEF was flipped.
    //createShaders("${MODULE_WEBBROWSER}/shaders/");

    createShaders();

    _browserInstance->loadUrl(_url);

    return isReady();
}

bool ScreenSpaceBrowser::deinitialize() {
    std::string urlString;
    _url.getStringValue(urlString);
    LDEBUG(fmt::format("Deinitializing ScreenSpaceBrowser: {}", urlString));

    _browserInstance->close(true);

    WebBrowserModule* webBrowser = OsEng.moduleEngine().module<WebBrowserModule>();
    if (webBrowser != nullptr) {
        webBrowser->removeBrowser(_browserInstance);
        _browserInstance.reset();
        return true;
    }

    LWARNING("Could not find WebBrowserModule");
    return false;
}

void ScreenSpaceBrowser::render() {
    draw(rotationMatrix() * translationMatrix() * scaleMatrix());
}

void ScreenSpaceBrowser::update() {
    if (_isUrlDirty) {
        _browserInstance->loadUrl(_url);
        _isUrlDirty = false;
    }

    if (_isDimensionsDirty) {
        _browserInstance->reshape(_dimensions.value());
        _originalViewportSize = _dimensions.value();
        _isDimensionsDirty = false;
    }
}

bool ScreenSpaceBrowser::isReady() const {
    return _shader && _texture;
}

} // namespace openspace
