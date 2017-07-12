/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "include/screen_space_browser.h"


namespace {
const char* KeyName = "Name";
const char* KeyUrl = "URL";
const std::string _loggerCat = "ScreenSpaceBrowser";
} // namespace

namespace openspace {

ScreenSpaceBrowser::ScreenSpaceBrowser(const ghoul::Dictionary &dictionary)
        : ScreenSpaceRenderable(dictionary)
        , urlIsDirty(false)
        , dimensionsAreDirty(false)
        , url("url", "URL", "")
        , dimensions("dimensions",
                     "Browser Dimensions",
                     glm::vec2(0.0f),
                     glm::vec2(0.0f),
                     glm::vec2(3000.0f))
{
    if (dictionary.hasKey(KeyName)) {
        setName(dictionary.value<std::string>(KeyName));
    } else {
        static int id = 0;
        setName("ScreenSpaceBrowser " + std::to_string(id));
        ++id;
    }

    std::string urlToLoad;
    if (dictionary.getValue(KeyUrl, urlToLoad)) {
        url = dictionary.value<std::string>(KeyUrl);
    }

    glm::vec2 windowDimensions = OsEng.windowWrapper().currentWindowSize();
    dimensions = windowDimensions;

    _texture = std::make_unique<ghoul::opengl::Texture>(glm::uvec3(windowDimensions, 1.0f));
    renderHandler = new ScreenSpaceRenderHandler();
    browserInstance = std::make_unique<BrowserInstance>(renderHandler);

    url.onChange([this]() { urlIsDirty = true; });
    dimensions.onChange([this]() { dimensionsAreDirty = true; });

    addProperty(url);
    addProperty(dimensions);
}

bool ScreenSpaceBrowser::initialize() {
    _originalViewportSize = OsEng.windowWrapper().currentWindowSize();
    renderHandler->setTexture((GLuint) *_texture);

    createPlane();
    // Load a special version of the regular ScreenRenderable shaders. This mirrors the
    // image along the Y axis since the image produced by CEF was flipped.
    createShaders("${MODULE_WEBBROWSER}/shaders/");
    browserInstance->load(url);

    return isReady();
}

bool ScreenSpaceBrowser::deinitialize() {
    return true;
}

void ScreenSpaceBrowser::render() {
    draw(rotationMatrix() * translationMatrix() * scaleMatrix());
}

void ScreenSpaceBrowser::update() {
    if (urlIsDirty) {
        browserInstance->load(url);
        urlIsDirty = false;
    }

    if (dimensionsAreDirty) {
        browserInstance->reshape(dimensions.value());
        _originalViewportSize = dimensions.value();
        dimensionsAreDirty = false;
    }
}

bool ScreenSpaceBrowser::isReady() const {
    return _shader && _texture;
}

} // namespace openspace