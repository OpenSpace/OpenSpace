/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <modules/skybrowser/include/screenspaceskybrowser.h>

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> 
#include <ghoul/opengl/texture.h>
#include <optional>
#include <glm/gtx/color_space.hpp> 
#include <random>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyBrowser";

    constexpr const openspace::properties::Property::PropertyInfo TextureQualityInfo = {
        "TextureQuality",
        "Quality of Texture",
        "A parameter to set the resolution of the texture. 1 is full resolution and "
        "slower frame rate. Lower value means lower resolution of texture and faster "
        "frame rate."
    };

    constexpr const openspace::properties::Property::PropertyInfo RenderCopyInfo = {
        "RenderCopy",
        "RAE Position Of A Copy Of The Sky Browser",
        "Render a copy of this sky browser at an additional position. This copy will not "
        "be interactive. The position is in RAE (Radius, Azimuth, Elevation) coordinates."
    };

    constexpr const openspace::properties::Property::PropertyInfo RenderOnMasterInfo = {
        "RenderOnlyOnMaster",
        "Render Only On Master",
        "Render the interactive sky browser only on the master node (this setting won't "
        "affect the copies). This setting allows mouse interactions in a dome environment."
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {
        // [[codegen::verbatim(TextureQualityInfo.description)]]
        std::optional<float> textureQuality;

        // [[codegen::verbatim(RenderOnMasterInfo.description)]]
        std::optional<bool> renderOnlyOnMaster;
    };

#include "screenspaceskybrowser_codegen.cpp"
} // namespace

glm::ivec3 randomBorderColor(glm::ivec3 highlight) {
    // Generate a random border color with sufficient lightness and a n
    std::random_device rd;
    // Hue is in the unit degrees [0, 360]
    std::uniform_real_distribution<float> hue(0.f, 360.f);
   
    // Value in saturation are in the unit percent [0,1]
    float value = 0.9f; // Brightness
    float saturation = 0.5f;
    glm::ivec3 rgbColor;
    glm::ivec3 highlighted;
    do {
        glm::vec3 hsvColor = glm::vec3(hue(rd), saturation, value);
        rgbColor = glm::ivec3(glm::rgbColor(hsvColor) * 255.f);
        highlighted = rgbColor + highlight;
    } while (highlighted.x < 255 && highlighted.y < 255 && highlighted.z < 255);
   
    return rgbColor;
}

namespace openspace {

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , WwtCommunicator(dictionary)
    , _textureQuality(TextureQualityInfo, 0.5f, 0.25f, 1.f)
    , _renderOnlyOnMaster(RenderOnMasterInfo, false)
{
    _identifier = makeUniqueIdentifier(_identifier);

    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _textureQuality = p.textureQuality.value_or(_textureQuality);
    _renderOnlyOnMaster = p.renderOnlyOnMaster.value_or(_renderOnlyOnMaster);

    addProperty(_url);
    addProperty(_browserPixeldimensions);
    addProperty(_reload);
    addProperty(_textureQuality);
    addProperty(_renderOnlyOnMaster);

    _textureQuality.onChange([this]() {
        _textureDimensionsIsDirty = true;
    });        

    // Ensure that the browser is placed at the z-coordinate of the screen space plane
    glm::vec2 screenPosition = _cartesianPosition.value();
    _cartesianPosition = glm::vec3(screenPosition, skybrowser::ScreenSpaceZ);

    if (global::windowDelegate->isMaster()) {
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        _borderColor = randomBorderColor(module->highlight());
    } 
    _scale = _size.y * 0.5;
}

ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module && module->getPair(identifier())) {
        module->removeTargetBrowserPair(identifier());
    }
}

bool ScreenSpaceSkyBrowser::initializeGL() {
    WwtCommunicator::initializeGL();
    ScreenSpaceRenderable::initializeGL();
    updateTextureResolution();
    return true;
}

glm::dvec2 ScreenSpaceSkyBrowser::fineTuneVector(glm::dvec2 drag) {
    // Fine tuning of target
    glm::dvec2 wwtFov = fieldsOfView();
    glm::dvec2 openSpaceFOV = skybrowser::fovWindow();

    glm::dvec2 browserDim = screenSpaceDimensions();
    glm::dvec2 angleResult = wwtFov * (drag / browserDim);
    glm::dvec2 resultRelativeOs = angleResult / openSpaceFOV;

    // Convert to screen space coordinate system
    glm::dvec2 convertToScreenSpace{ (2.f * skybrowser::windowRatio()), 2.f };
    glm::dvec2 result = - convertToScreenSpace * resultRelativeOs;
    return result;
}
    
void ScreenSpaceSkyBrowser::setIdInBrowser() {
    WwtCommunicator::setIdInBrowser(identifier());
}

void ScreenSpaceSkyBrowser::updateTextureResolution() {
    // Scale texture depending on the height of the window
    // Set texture size to the actual pixel size it covers
    glm::vec2 pixels = glm::vec2(global::windowDelegate->currentSubwindowSize());
           
    // If the scale is 1, it covers half the window. Hence multiplication with 2
    float newResY = pixels.y * 2.f * _scale; 
    float ratio = _size.x / _size.y;
    float newResX = newResY * ratio;
    glm::vec2 newSize = glm::vec2(newResX , newResY) * _textureQuality.value();

    _browserPixeldimensions = glm::ivec2(newSize);
    _texture->setDimensions(glm::ivec3(newSize, 1));
    _objectSize = glm::ivec3(_texture->dimensions());
}

void ScreenSpaceSkyBrowser::addRenderCopy(const glm::vec3& raePosition, int nCopies) {
    int start = _renderCopies.size();
    for (int i = 0; i < nCopies; i++) {
        openspace::properties::Property::PropertyInfo info = RenderCopyInfo;
        float azimuth = i * glm::two_pi<float>() / nCopies;
        glm::vec3 position = raePosition + glm::vec3(0.f, azimuth, 0.f);
        std::string id = "RenderCopy" + std::to_string(start + i);
        info.identifier = id.c_str();
        _renderCopies.push_back(
            std::make_unique<properties::Vec3Property>(
                info, 
                position,
                glm::vec3(0.f, -glm::pi<float>(), -glm::half_pi<float>()),
                glm::vec3(10.f, glm::pi<float>(), glm::half_pi<float>())
            )
        );
        addProperty(_renderCopies.back().get());
    }
}

void ScreenSpaceSkyBrowser::removeRenderCopy() {
    if (_renderCopies.size() > 0) {
        removeProperty(_renderCopies.back().get());
        _renderCopies.pop_back();
    }
}

std::vector<std::pair<std::string, glm::dvec3>>  ScreenSpaceSkyBrowser::renderCopies()
{
    std::vector<std::pair<std::string, glm::dvec3>>  vec;
    std::for_each(
        _renderCopies.begin(),
        _renderCopies.end(),
        [&](const std::unique_ptr<properties::Vec3Property>& copy) {
            std::pair<std::string, glm::dvec3> pair = { 
                copy.get()->identifier(), 
                glm::dvec3(copy.get()->value()) 
            };
            vec.push_back(pair);
        });
    return vec;
}

void ScreenSpaceSkyBrowser::moveRenderCopy(int i, glm::vec3 raePosition) {
    if (i < _renderCopies.size() && i >= 0) {
        _renderCopies[i].get()->set(raePosition);
    }
}

bool ScreenSpaceSkyBrowser::deinitializeGL() {
    ScreenSpaceRenderable::deinitializeGL();
    WwtCommunicator::deinitializeGL();
       
    return true;
}

void ScreenSpaceSkyBrowser::render() {
    WwtCommunicator::render();

    // If the sky browser only should be rendered on master, don't use the
    // global rotation
    if (_renderOnlyOnMaster && global::windowDelegate->isMaster()) {
        draw(
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }
    else if(!_renderOnlyOnMaster) {
        draw(
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }

    // Render a copy that is not interactive
    for (const std::unique_ptr<properties::Vec3Property>& copy : _renderCopies) {
        glm::vec3 spherical = sphericalToCartesian(raeToSpherical(copy.get()->value()));
        glm::mat4 localRotation = glm::inverse(glm::lookAt(
            glm::vec3(0.f),
            glm::normalize(spherical),
            glm::vec3(0.f, 1.f, 0.f)
        ));
        draw(
            globalRotationMatrix() *
            glm::translate(glm::mat4(1.f), spherical) *
            localRotation *
            scaleMatrix()
        );
    }
}

void ScreenSpaceSkyBrowser::update() {
    // Texture of window is 1x1 when minimized
    bool isWindow = global::windowDelegate->currentSubwindowSize() != glm::ivec2(1);
    bool isWindowResized = global::windowDelegate->windowHasResized();
    if ((isWindowResized && isWindow) || _textureDimensionsIsDirty) {
        updateTextureResolution();
        _textureDimensionsIsDirty = false;
    }
    if (_sizeIsDirty) {
        updateScreenSpaceSize();
        _sizeIsDirty = false;
    }

    WwtCommunicator::update();
    ScreenSpaceRenderable::update();
}

void ScreenSpaceSkyBrowser::setVerticalFovWithScroll(float scroll) {
    // Make scroll more sensitive the smaller the FOV
    double x = _verticalFov;
    double zoomFactor = atan(x / 50.0) + exp(x / 40.0) - 0.99999999999999999999999999999;
    double zoom = scroll > 0.0 ? -zoomFactor : zoomFactor;
    _verticalFov = std::clamp(_verticalFov + zoom, 0.0, 70.0);
}

void ScreenSpaceSkyBrowser::bindTexture() {
    _texture->bind();
}

glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
    // To ensure the plane has the right ratio
    // The _scale tells us how much of the windows height the
    // browser covers: e.g. a browser that covers 0.25 of the
    // height of the window will have scale = 0.25

    glm::mat4 scale = glm::scale(
        glm::mat4(1.f),
        glm::vec3(browserRatio() * _scale, _scale, 1.f)
    );
    return scale;
}

void ScreenSpaceSkyBrowser::setOpacity(float opacity) {
    _opacity = opacity;
}

void ScreenSpaceSkyBrowser::setScreenSpaceSize(const glm::vec2& newSize) {   
    _size = newSize;
    _sizeIsDirty = true;
}

void ScreenSpaceSkyBrowser::updateScreenSpaceSize() {
    _scale = abs(_size.y) * 0.5f;
    updateTextureResolution();
}

float ScreenSpaceSkyBrowser::opacity() const {
    return _opacity;
}

glm::vec2 ScreenSpaceSkyBrowser::size() const {
    return _size;
}
} // namespace openspace
