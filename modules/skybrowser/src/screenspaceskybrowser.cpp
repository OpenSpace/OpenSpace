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
    constexpr std::string_view _loggerCat = "ScreenSpaceSkyBrowser";

    constexpr openspace::properties::Property::PropertyInfo TextureQualityInfo = {
        "TextureQuality",
        "Quality of Texture",
        "A parameter to set the resolution of the texture. 1 is full resolution and "
        "slower frame rate. Lower value means lower resolution of texture and faster "
        "frame rate"
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayCopyInfo = {
        "DisplayCopy",
        "Display Copy Position",
        "Display a copy of this sky browser at an additional position. This copy will not "
        "be interactive. The position is in RAE (Radius, Azimuth, Elevation) coordinates "
        "or Cartesian, depending on if the browser uses RAE or Cartesian coordinates"
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayCopyShowInfo = {
        "ShowDisplayCopy",
        "Show Display Copy",
        "Show the display copy"
    };

    constexpr openspace::properties::Property::PropertyInfo IsHiddenInfo = {
        "IsHidden",
        "Is Hidden",
        "If checked, the browser will be not be displayed. If it is not checked, it will "
        "be"
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {
        // [[codegen::verbatim(TextureQualityInfo.description)]]
        std::optional<float> textureQuality;

        // [[codegen::verbatim(IsHiddenInfo.description)]]
        std::optional<bool> isHidden;
    };

#include "screenspaceskybrowser_codegen.cpp"

    glm::ivec3 randomBorderColor() {
        // Generate a random border color with sufficient lightness and a n
        std::random_device rd;
        // Hue is in the unit degrees [0, 360]
        std::uniform_real_distribution<float> hue(0.f, 360.f);

        // Value in saturation are in the unit percent [0,1]
        float value = 0.9f; // Brightness
        float saturation = 0.5f;
        glm::vec3 hsvColor = glm::vec3(hue(rd), saturation, value);
        glm::ivec3 rgbColor = glm::ivec3(glm::rgbColor(hsvColor) * 255.f);

        return rgbColor;
    }
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceSkyBrowser::Documentation() {
    return codegen::doc<Parameters>("skybrowser_screenspaceskybrowser");
}

ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , WwtCommunicator(dictionary)
    , _textureQuality(TextureQualityInfo, 0.5f, 0.25f, 1.f)
    , _isHidden(IsHiddenInfo, true)
{
    _identifier = makeUniqueIdentifier(_identifier);

    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _textureQuality = p.textureQuality.value_or(_textureQuality);
    _isHidden = p.isHidden.value_or(_isHidden);

    addProperty(_isHidden);
    addProperty(_url);
    addProperty(_browserDimensions);
    addProperty(_reload);
    addProperty(_textureQuality);
    addProperty(_verticalFov);

    _textureQuality.onChange([this]() { _textureDimensionsIsDirty = true; });

    if (global::windowDelegate->isMaster()) {
        _borderColor = randomBorderColor();
    }

    _scale.onChange([this]() {
        updateTextureResolution();
        _borderRadiusTimer = 0;
    });

    _useRadiusAzimuthElevation.onChange(
        [this]() {
            std::for_each(
                _displayCopies.begin(),
                _displayCopies.end(),
                [this](std::unique_ptr<properties::Vec3Property>& copy) {
                    if (_useRadiusAzimuthElevation) {
                        *copy = sphericalToRae(cartesianToSpherical(copy->value()));

                    }
                    else {
                        *copy = sphericalToCartesian(raeToSpherical(copy->value()));
                    }
                });
        });
}

ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module && module->pair(identifier())) {
        module->removeTargetBrowserPair(identifier());
    }
}

bool ScreenSpaceSkyBrowser::initializeGL() {
    WwtCommunicator::initializeGL();
    ScreenSpaceRenderable::initializeGL();
    updateTextureResolution();
    return true;
}

glm::dvec2 ScreenSpaceSkyBrowser::fineTuneVector(const glm::dvec2& drag) {
    // Fine tuning of target
    glm::dvec2 wwtFov = fieldsOfView();
    glm::dvec2 openSpaceFOV = skybrowser::fovWindow();

    glm::dvec2 browserDim = screenSpaceDimensions();
    glm::dvec2 angleResult = wwtFov * (drag / browserDim);
    glm::dvec2 resultRelativeOs = angleResult / openSpaceFOV;

    // Convert to screen space coordinate system
    glm::dvec2 convertToScreenSpace = glm::dvec2((2.f * skybrowser::windowRatio()), 2.f);
    glm::dvec2 result = -convertToScreenSpace * resultRelativeOs;
    return result;
}

bool ScreenSpaceSkyBrowser::isInitialized() const {
    return _isInitialized;
}

void ScreenSpaceSkyBrowser::setIdInBrowser() const {
    int currentNode = global::windowDelegate->currentNode();
    WwtCommunicator::setIdInBrowser(fmt::format("{}_{}", identifier(), currentNode));
}

void ScreenSpaceSkyBrowser::setIsInitialized(bool isInitialized) {
    _isInitialized = isInitialized;
}

void ScreenSpaceSkyBrowser::updateTextureResolution() {
    // Scale texture depending on the height of the window
    // Set texture size to the actual pixel size it covers
    glm::vec2 pixels = glm::vec2(global::windowDelegate->currentSubwindowSize());

    // If the scale is 1, it covers half the window. Hence multiplication with 2
    float newResY = pixels.y * 2.f * _scale;
    float newResX = newResY * _ratio;
    glm::vec2 newSize = glm::vec2(newResX , newResY) * _textureQuality.value();

    _browserDimensions = glm::ivec2(newSize);
    _texture->setDimensions(glm::ivec3(newSize, 1));
    _objectSize = glm::ivec3(_texture->dimensions());
    _radiusIsDirty = true;
}

void ScreenSpaceSkyBrowser::addDisplayCopy(const glm::vec3& raePosition, int nCopies) {
    size_t start = _displayCopies.size();
    for (int i = 0; i < nCopies; i++) {
        openspace::properties::Property::PropertyInfo info = DisplayCopyInfo;
        float azimuth = i * glm::two_pi<float>() / nCopies;
        glm::vec3 position = raePosition + glm::vec3(0.f, azimuth, 0.f);
        std::string idDisplayCopy = "DisplayCopy" + std::to_string(start + i);
        info.identifier = idDisplayCopy.c_str();
        _displayCopies.push_back(
            std::make_unique<properties::Vec3Property>(
                info,
                position,
                glm::vec3(-4.f, -4.f, -10.f),
                glm::vec3(4.f, 4.f, glm::half_pi<float>())
            )
        );
        openspace::properties::Property::PropertyInfo showInfo = DisplayCopyShowInfo;
        std::string idDisplayCopyVisible = "ShowDisplayCopy" + std::to_string(start + i);
        showInfo.identifier = idDisplayCopyVisible.c_str();
        _showDisplayCopies.push_back(
            std::make_unique<properties::BoolProperty>(
                showInfo,
                true
                )
        );
        addProperty(_displayCopies.back().get());
        addProperty(_showDisplayCopies.back().get());
    }
}

void ScreenSpaceSkyBrowser::removeDisplayCopy() {
    if (!_displayCopies.empty()) {
        removeProperty(_displayCopies.back().get());
        removeProperty(_showDisplayCopies.back().get());
        _displayCopies.pop_back();
        _showDisplayCopies.pop_back();
    }
}

std::vector<std::pair<std::string, glm::dvec3>>
ScreenSpaceSkyBrowser::displayCopies() const
{
    std::vector<std::pair<std::string, glm::dvec3>> vec;
    using vec3Property = std::unique_ptr<properties::Vec3Property>;
    for (const vec3Property& copy : _displayCopies) {
        vec.push_back({ copy->identifier(), copy->value() });
    }
    return vec;
}

std::vector<std::pair<std::string, bool>>
ScreenSpaceSkyBrowser::showDisplayCopies() const
{
    std::vector<std::pair<std::string, bool>> vec;
    using boolProperty = std::unique_ptr<properties::BoolProperty>;
    for (const boolProperty& copy : _showDisplayCopies) {
        vec.push_back({copy->identifier(), copy->value()});
    }
    return vec;
}

bool ScreenSpaceSkyBrowser::deinitializeGL() {
    ScreenSpaceRenderable::deinitializeGL();
    WwtCommunicator::deinitializeGL();
    return true;
}

void ScreenSpaceSkyBrowser::render() {
    WwtCommunicator::render();

    if (!_isHidden) {
        draw(
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }

    // Render the display copies
    for (size_t i = 0; i < _displayCopies.size(); i++) {
        if (_showDisplayCopies[i]->value()) {
            glm::vec3 coordinates = _displayCopies[i]->value();
            if (_useRadiusAzimuthElevation) {
                coordinates = sphericalToCartesian(raeToSpherical(coordinates));
            }
            glm::mat4 localRotation = glm::mat4(1.f);
            if (_faceCamera) {
                localRotation = glm::inverse(glm::lookAt(
                    glm::vec3(0.f),
                    glm::normalize(coordinates),
                    glm::vec3(0.f, 1.f, 0.f)
                ));
            }

            draw(
                globalRotationMatrix() *
                glm::translate(glm::mat4(1.f), coordinates) *
                localRotation *
                scaleMatrix()
            );
        }
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
    if (_ratioIsDirty) {
        updateTextureResolution();
        _ratioIsDirty = false;
    }
    if (_shouldReload) {
        _isInitialized = false;
    }
    // After the texture has been updated, wait a little bit before updating the border
    // radius so the browser has time to update its size
    if (_radiusIsDirty && _isInitialized && _borderRadiusTimer == RadiusTimeOut) {
        setBorderRadius(_borderRadius);
        _radiusIsDirty = false;
        _borderRadiusTimer = -1;
    }
    _borderRadiusTimer++;

    ScreenSpaceRenderable::update();
    WwtCommunicator::update();
}

double ScreenSpaceSkyBrowser::setVerticalFovWithScroll(float scroll) {
    // Make scroll more sensitive the smaller the FOV
    double x = _verticalFov;
    double zoomFactor = atan(x / 50.0) + exp(x / 40.0) - 0.99999999999999999999999999999;
    double zoom = scroll > 0.0 ? zoomFactor : -zoomFactor;
    _verticalFov = std::clamp(_verticalFov + zoom, 0.0, 70.0);
    return _verticalFov;
}

void ScreenSpaceSkyBrowser::bindTexture() {
    _texture->bind();
}

glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
    // To ensure the plane has the right ratio
    // The _scale tells us how much of the windows height the browser covers: e.g. a
    // browser that covers 0.25 of the height of the window will have scale = 0.25

    glm::mat4 scale = glm::scale(
        glm::mat4(1.f),
        glm::vec3(browserRatio() * _scale, _scale, 1.f)
    );
    return scale;
}

void ScreenSpaceSkyBrowser::setOpacity(float opacity) {
    _opacity = opacity;
}

void ScreenSpaceSkyBrowser::setRatio(float ratio) {
    _ratio = ratio;
    _ratioIsDirty = true;
}

float ScreenSpaceSkyBrowser::opacity() const {
    return _opacity;
}

} // namespace openspace
