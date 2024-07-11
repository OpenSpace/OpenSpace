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
    constexpr openspace::properties::Property::PropertyInfo TextureQualityInfo = {
        "TextureQuality",
        "Quality of Texture",
        "A parameter to set the resolution of the texture. 1 is full resolution and "
        "slower frame rate. Lower value means lower resolution of texture and faster "
        "frame rate.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayCopyInfo = {
        "DisplayCopy",
        "Display Copy Position",
        "Display a copy of this sky browser at an additional position. This copy will "
        "not be interactive. The position is in RAE (Radius, Azimuth, Elevation) "
        "coordinates or Cartesian, depending on if the browser uses RAE or Cartesian "
        "coordinates.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayCopyShowInfo = {
        "ShowDisplayCopy",
        "Show Display Copy",
        "Show the display copy.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IsHiddenInfo = {
        "IsHidden",
        "Is Hidden",
        "If checked, the browser will be not be displayed. If it is not checked, it will "
        "be.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointSpacecraftInfo = {
        "PointSpacecraft",
        "Point Spacecraft",
        "If checked, spacecrafts will point towards the coordinate of an image upon "
        "selection.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UpdateDuringAnimationInfo = {
        "UpdateDuringTargetAnimation",
        "Update During Target Animation",
        "If checked, the sky browser display copy will update its coordinates while "
        "the target is animating.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo VerticalFovInfo = {
        "VerticalFov",
        "Vertical Field Of View",
        "The vertical field of view of the target.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `ScreenSpaceRenderable` is used to display a screen space window showing the
    // integrated World Wide Telescope view. The view will be dynamically updated when
    // interacting with the view or with images in the SkyBrowser panel.
    //
    // A `ScreenSpaceSkyBrowser` should not be created from a `.asset` file, but is rather
    // created from interacting with the SkyBrowser user interface panel. If created in
    // an asset, it requires some extra scripting to work with the SkyBrowser feature.
    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {
        // [[codegen::verbatim(TextureQualityInfo.description)]]
        std::optional<float> textureQuality;

        // [[codegen::verbatim(IsHiddenInfo.description)]]
        std::optional<bool> isHidden;

        // [[codegen::verbatim(PointSpacecraftInfo.description)]]
        std::optional<bool> pointSpacecraft;

        // [[codegen::verbatim(UpdateDuringAnimationInfo.description)]]
        std::optional<bool> updateDuringTargetAnimation;

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<double> verticalFov;
    };

#include "screenspaceskybrowser_codegen.cpp"

    glm::ivec3 randomBorderColor() {
        // Generate a random border color with sufficient lightness and a n
        std::random_device rd;
        // Hue is in the unit degrees [0, 360]
        std::uniform_real_distribution<float> hue(0.f, 360.f);

        // Value in saturation are in the unit percent [0,1]
        constexpr float Value = 0.9f; // Brightness
        constexpr float Saturation = 0.5f;
        const glm::vec3 hsvColor = glm::vec3(hue(rd), Saturation, Value);
        const glm::ivec3 rgbColor = glm::ivec3(glm::rgbColor(hsvColor) * 255.f);
        return rgbColor;
    }
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceSkyBrowser::Documentation() {
    return codegen::doc<Parameters>("skybrowser_screenspaceskybrowser");
}

ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _wwtCommunicator(dictionary)
    , _verticalFov(VerticalFovInfo, 10.0, 0.00000000001, 70.0)
    , _textureQuality(TextureQualityInfo, 1.f, 0.25f, 1.f)
    , _isHidden(IsHiddenInfo, true)
    , _isPointingSpacecraft(PointSpacecraftInfo, false)
    , _updateDuringTargetAnimation(UpdateDuringAnimationInfo, false)
{
    _identifier = makeUniqueIdentifier(_identifier);

    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _textureQuality = p.textureQuality.value_or(_textureQuality);
    _isHidden = p.isHidden.value_or(_isHidden);
    _isPointingSpacecraft = p.pointSpacecraft.value_or(_isPointingSpacecraft);
    _updateDuringTargetAnimation = p.updateDuringTargetAnimation.value_or(
        _updateDuringTargetAnimation
    );
    // Handle target dimension property
    _verticalFov = p.verticalFov.value_or(_verticalFov);
    _verticalFov.setReadOnly(true);

    addProperty(_isHidden);
    addProperty(_textureQuality);
    addProperty(_verticalFov);
    addProperty(_isPointingSpacecraft);
    addProperty(_updateDuringTargetAnimation);

    _textureQuality.onChange([this]() {
        _wwtCommunicator.updateBrowserDimensions();
    });

    if (global::windowDelegate->isMaster()) {
        _wwtBorderColor = randomBorderColor();
    }

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

    addPropertySubOwner(_wwtCommunicator);
    _wwtCommunicator.property("Reload")->onChange([this]() {
        _isImageCollectionLoaded = false;
        _isInitialized = false;
    });

    _wwtCommunicator.property("Dimensions")->onChange([this]() {
        updateTextureResolution();
    });

    _lastUpdateTime = std::chrono::system_clock::now();
    _objectSize = glm::ivec3(_wwtCommunicator.browserDimensions(), 1);
}

ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module && module->pair(identifier())) {
        module->removeTargetBrowserPair(identifier());
    }
}

bool ScreenSpaceSkyBrowser::initializeGL() {
    _wwtCommunicator.initializeGL();
    ScreenSpaceRenderable::initializeGL();
    return true;
}

glm::dvec2 ScreenSpaceSkyBrowser::fineTuneVector(const glm::dvec2& drag) {
    // Fine tuning of target
    const glm::dvec2 wwtFov = fieldsOfView();
    const glm::dvec2 openSpaceFOV = skybrowser::fovWindow();

    const glm::dvec2 browserDim = screenSpaceDimensions();
    const glm::dvec2 angleResult = wwtFov * (drag / browserDim);
    const glm::dvec2 resultRelativeOs = angleResult / openSpaceFOV;

    // Convert to screen space coordinate system
    const glm::dvec2 screenSpace = glm::dvec2((2.f * skybrowser::windowRatio()), 2.f);
    const glm::dvec2 result = -screenSpace * resultRelativeOs;
    return result;
}

bool ScreenSpaceSkyBrowser::isInitialized() const {
    return _isInitialized;
}

bool ScreenSpaceSkyBrowser::isPointingSpacecraft() const {
    return _isPointingSpacecraft;
}

bool ScreenSpaceSkyBrowser::shouldUpdateWhileTargetAnimates() const {
    return _updateDuringTargetAnimation;
}

void ScreenSpaceSkyBrowser::setIdInBrowser() const {
    int currentNode = global::windowDelegate->currentNode();
    _wwtCommunicator.setIdInBrowser(std::format("{}_{}", identifier(), currentNode));
}

void ScreenSpaceSkyBrowser::setIsInitialized(bool isInitialized) {
    _isInitialized = isInitialized;
}

void ScreenSpaceSkyBrowser::setPointSpaceCraft(bool shouldPoint) {
    _isPointingSpacecraft = shouldPoint;
}

void ScreenSpaceSkyBrowser::updateTextureResolution() {
    // Check if texture quality has changed. If it has, adjust accordingly
    if (std::abs(_textureQuality.value() - _lastTextureQuality) > glm::epsilon<float>()) {
        const float diffTextureQuality = _textureQuality / _lastTextureQuality;
        const glm::vec2 res = glm::vec2(_wwtCommunicator.browserDimensions()) * diffTextureQuality;
        _wwtCommunicator.setBrowserDimensions(glm::ivec2(res));
        _lastTextureQuality = _textureQuality.value();
    }
    _objectSize = glm::ivec3(_wwtCommunicator.browserDimensions(), 1);

    // The radius has to be updated when the texture resolution has changed
    _radiusIsDirty = true;
    _borderRadiusTimer = 0;
}

void ScreenSpaceSkyBrowser::updateBorderColor() {
    _wwtCommunicator.setBorderColor(glm::ivec3(_borderColor.value()));
}

void ScreenSpaceSkyBrowser::addDisplayCopy(const glm::vec3& raePosition, int nCopies) {
    const size_t start = _displayCopies.size();
    for (int i = 0; i < nCopies; i++) {
        openspace::properties::Property::PropertyInfo info = DisplayCopyInfo;
        const float azimuth = i * glm::two_pi<float>() / nCopies;
        const glm::vec3 position = raePosition + glm::vec3(0.f, azimuth, 0.f);
        // @TODO(abock) I think the lifetime for this string is a bit tricky. I don't
        // think it will live long enough to be actually usable
        const std::string idDisplayCopy = "DisplayCopy" + std::to_string(start + i);
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
        // @TODO(abock) I think the lifetime for this string is a bit tricky. I don't
        // think it will live long enough to be actually usable
        const std::string idDispCpyVis = "ShowDisplayCopy" + std::to_string(start + i);
        showInfo.identifier = idDispCpyVis.c_str();
        _showDisplayCopies.push_back(
            std::make_unique<properties::BoolProperty>(showInfo, true)
        );
        addProperty(_displayCopies.back().get());
        addProperty(_showDisplayCopies.back().get());
    }
}

std::vector<std::string> ScreenSpaceSkyBrowser::selectedImages() const {
    std::vector<std::string> selectedImagesVector;
    selectedImagesVector.resize(_selectedImages.size());
    std::transform(
        _selectedImages.cbegin(),
        _selectedImages.cend(),
        selectedImagesVector.begin(),
        [](const std::pair<std::string, double>& image) { return image.first; }
    );
    return selectedImagesVector;
}


void ScreenSpaceSkyBrowser::setBorderColor(glm::ivec3 color) {
    _wwtBorderColor = std::move(color);
    _borderColorIsDirty = true;
    _borderColor = color;
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
    vec.reserve(_displayCopies.size());
    for (const std::unique_ptr<properties::Vec3Property>& copy : _displayCopies) {
        vec.emplace_back(copy->identifier(), copy->value());
    }
    return vec;
}

std::vector<std::pair<std::string, bool>>
ScreenSpaceSkyBrowser::showDisplayCopies() const
{
    std::vector<std::pair<std::string, bool>> vec;
    vec.reserve(_showDisplayCopies.size());
    for (const std::unique_ptr<properties::BoolProperty>& copy : _showDisplayCopies) {
        vec.emplace_back(copy->identifier(), copy->value());
    }
    return vec;
}

bool ScreenSpaceSkyBrowser::deinitializeGL() {
    ScreenSpaceRenderable::deinitializeGL();
    _wwtCommunicator.deinitializeGL();
    return true;
}

void ScreenSpaceSkyBrowser::render(const RenderData& renderData) {
    _wwtCommunicator.render();

    if (!_isHidden) {
        const glm::mat4 mat =
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix();
        draw(mat, renderData);
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

            const glm::mat4 mat =
                globalRotationMatrix() *
                glm::translate(glm::mat4(1.f), coordinates) *
                localRotation *
                scaleMatrix();
            draw(mat, renderData);
        }
    }
}

glm::dvec2 ScreenSpaceSkyBrowser::fieldsOfView() const {
    const double vFov = _verticalFov;
    const double hFov = vFov * _wwtCommunicator.browserRatio();
    return glm::dvec2(hFov, vFov);
}

float ScreenSpaceSkyBrowser::browserRatio() const {
    return _wwtCommunicator.browserRatio();
}

void ScreenSpaceSkyBrowser::selectImage(const std::string& url) {
    // Ensure there are no duplicates
    auto it = findSelectedImage(url);

    if (it == _selectedImages.end()) {
        // Push newly selected image to front
        _selectedImages.emplace_front(url, 1.0);

        // If wwt has not loaded the collection yet, wait with passing the message
        if (_isImageCollectionLoaded) {
            _wwtCommunicator.addImageLayerToWwt(url);
        }
    }
}

void ScreenSpaceSkyBrowser::update() {
    // After the texture has been updated, wait a little bit before updating the border
    // radius so the browser has time to update its size
    if (_radiusIsDirty && _isInitialized && _borderRadiusTimer == RadiusTimeOut) {
        _wwtCommunicator.setBorderRadius(_borderRadius);
        _radiusIsDirty = false;
        _borderRadiusTimer = -1;
    }
    _borderRadiusTimer++;

    // Cap how messages are passed
    const std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    const std::chrono::system_clock::duration timeSinceLastUpdate = now - _lastUpdateTime;

    if (timeSinceLastUpdate > TimeUpdateInterval) {
        if (_equatorialAimIsDirty) {
            _wwtCommunicator.setAim(_equatorialAim, _verticalFov, _targetRoll);
            _equatorialAimIsDirty = false;
        }
        if (_borderColorIsDirty) {
            _wwtCommunicator.setBorderColor(glm::ivec3(_borderColor.value()));
            _borderColorIsDirty = false;
        }
        _lastUpdateTime = std::chrono::system_clock::now();
    }
    _wwtCommunicator.update();

    ScreenSpaceRenderable::update();

}

double ScreenSpaceSkyBrowser::setVerticalFovWithScroll(float scroll) {
    // Make scroll more sensitive the smaller the FOV
    const double x = _verticalFov;
    const double zoomFactor =
        atan(x / 50.0) + exp(x / 40.0) - 0.99999999999999999999999999999;
    const double zoom = scroll > 0.0 ? zoomFactor : -zoomFactor;
    _verticalFov = std::clamp(_verticalFov + zoom, 0.0, 70.0);

    return _verticalFov;
}

void ScreenSpaceSkyBrowser::bindTexture() {
    _wwtCommunicator.bindTexture();
}

glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
    // To ensure the plane has the right ratio
    // The _scale tells us how much of the windows height the browser covers: e.g. a
    // browser that covers 0.25 of the height of the window will have scale = 0.25

    glm::mat4 scale = glm::scale(
        glm::mat4(1.f),
        glm::vec3(_wwtCommunicator.browserRatio() * _scale, _scale, 1.f)
    );
    return scale;
}

float ScreenSpaceSkyBrowser::opacity() const noexcept {
    return _opacity;
}

glm::ivec3 ScreenSpaceSkyBrowser::borderColor() const {
    return _wwtBorderColor;
}

void ScreenSpaceSkyBrowser::removeSelectedImage(const std::string& imageUrl) {
    // Remove from selected list
    auto it = findSelectedImage(imageUrl);
    if (it != _selectedImages.end()) {
        _selectedImages.erase(it);
        _wwtCommunicator.removeSelectedImage(imageUrl);
    }
}

void ScreenSpaceSkyBrowser::hideChromeInterface() {
    _wwtCommunicator.hideChromeInterface();
}

void ScreenSpaceSkyBrowser::addImageLayerToWwt(std::string imageUrl) {
    _wwtCommunicator.addImageLayerToWwt(imageUrl);
}

void ScreenSpaceSkyBrowser::reload() {
    _wwtCommunicator.reload();
}

void ScreenSpaceSkyBrowser::setBorderRadius(double radius) {
    _wwtCommunicator.setBorderRadius(radius);
}

void ScreenSpaceSkyBrowser::setRatio(float ratio) {
    _wwtCommunicator.setRatio(ratio);
}

std::vector<double> ScreenSpaceSkyBrowser::opacities() const {
    std::vector<double> opacities;
    opacities.resize(_selectedImages.size());
    std::transform(
        _selectedImages.cbegin(),
        _selectedImages.cend(),
        opacities.begin(),
        [](const std::pair<std::string, double>& image) { return image.second; }
    );
    return opacities;
}

double ScreenSpaceSkyBrowser::borderRadius() const {
    return _borderRadius;
}

void ScreenSpaceSkyBrowser::setTargetRoll(double roll) {
    _targetRoll = roll;
}

void ScreenSpaceSkyBrowser::setVerticalFov(double vfov) {
    _verticalFov = vfov;
    _equatorialAimIsDirty = true;
}

void ScreenSpaceSkyBrowser::setEquatorialAim(glm::dvec2 equatorial) {
    _equatorialAim = std::move(equatorial);
    _equatorialAimIsDirty = true;
}

void ScreenSpaceSkyBrowser::loadImageCollection(const std::string& collection) {
    if (!_isImageCollectionLoaded) {
        _wwtCommunicator.loadImageCollection(collection);
    }
}

SelectedImageDeque::iterator ScreenSpaceSkyBrowser::findSelectedImage(
    const std::string& imageUrl)
{
    auto it = std::find_if(
        _selectedImages.begin(),
        _selectedImages.end(),
        [imageUrl](const std::pair<std::string, double>& pair) {
            return pair.first == imageUrl;
        }
    );
    return it;
}

glm::dvec2 ScreenSpaceSkyBrowser::equatorialAim() const {
    return _equatorialAim;
}

bool ScreenSpaceSkyBrowser::isImageCollectionLoaded() const {
    return _isImageCollectionLoaded;
}

void ScreenSpaceSkyBrowser::setImageOpacity(const std::string& imageUrl, float opacity) {
    auto it = findSelectedImage(imageUrl);
    it->second = opacity;
    _wwtCommunicator.setImageOpacity(imageUrl, opacity);
}

void ScreenSpaceSkyBrowser::setImageCollectionIsLoaded(bool isLoaded) {
    _isImageCollectionLoaded = isLoaded;
}

double ScreenSpaceSkyBrowser::verticalFov() const {
    return _verticalFov;
}

void ScreenSpaceSkyBrowser::setImageOrder(const std::string& imageUrl, int order) {
    // Find in selected images list
    auto current = findSelectedImage(imageUrl);
    const int currentIndex = static_cast<int>(
        std::distance(_selectedImages.begin(), current)
        );

    std::deque<std::pair<std::string, double>> newDeque;

    for (int i = 0; i < static_cast<int>(_selectedImages.size()); i++) {
        if (i == currentIndex) {
            continue;
        }
        else if (i == order) {
            if (order < currentIndex) {
                newDeque.push_back(*current);
                newDeque.push_back(_selectedImages[i]);
            }
            else {
                newDeque.push_back(_selectedImages[i]);
                newDeque.push_back(*current);
            }
        }
        else {
            newDeque.push_back(_selectedImages[i]);
        }
    }

    _selectedImages = newDeque;
    const int reverseOrder = static_cast<int>(_selectedImages.size()) - order - 1;
    _wwtCommunicator.setImageOrder(imageUrl, reverseOrder);
}

} // namespace openspace
