/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <modules/webbrowser/include/webkeyboardhandler.h>
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
    : ScreenSpaceBrowser(dictionary)
    , _isHidden(IsHiddenInfo, true)
    , _isPointingSpacecraft(PointSpacecraftInfo, false)
    , _updateDuringTargetAnimation(UpdateDuringAnimationInfo, true)
    , _verticalFov(VerticalFovInfo, 10.0, 0.00000000001, 70.0)
    , _wwtCommunicator(_browserInstance.get())

{
    _identifier = makeUniqueIdentifier(_identifier);

    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _isHidden = p.isHidden.value_or(_isHidden);
    _isPointingSpacecraft = p.pointSpacecraft.value_or(_isPointingSpacecraft);
    _updateDuringTargetAnimation = p.updateDuringTargetAnimation.value_or(
        _updateDuringTargetAnimation
    );

    _verticalFov = p.verticalFov.value_or(_verticalFov);
    _verticalFov.setReadOnly(true);

    addProperty(_isHidden);
    addProperty(_verticalFov);
    addProperty(_isPointingSpacecraft);
    addProperty(_updateDuringTargetAnimation);

    _reload.onChange([this]() {
        _wwtCommunicator.setImageCollectionIsLoaded(false);
        _isInitialized = false;
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
}

ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module && module->pair(identifier())) {
        module->removeTargetBrowserPair(identifier());
    }
}

void ScreenSpaceSkyBrowser::updateBorderColor() {
    _borderColorIsDirty = true;
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

double ScreenSpaceSkyBrowser::verticalFov() const {
    return _verticalFov;
}

void ScreenSpaceSkyBrowser::setIsInitialized(bool isInitialized) {
    _isInitialized = isInitialized;
}

void ScreenSpaceSkyBrowser::setPointSpaceCraft(bool shouldPoint) {
    _isPointingSpacecraft = shouldPoint;
}

void ScreenSpaceSkyBrowser::updateTextureResolution() {
    _objectSize = glm::ivec3(_dimensions.value(), 1);

    // The radius has to be updated when the texture resolution has changed
    _radiusIsDirty = true;
    _isDimensionsDirty = false;
    _borderRadiusTimer = 0;
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

ghoul::Dictionary ScreenSpaceSkyBrowser::data() const {
    ghoul::Dictionary res;
    std::vector<int> color = { _wwtBorderColor.r, _wwtBorderColor.g, _wwtBorderColor.b };

    res.setValue("fov", verticalFov());
    res.setValue("roll", _targetRoll);
    res.setValue("isFacingCamera", isFacingCamera());
    res.setValue("isUsingRae", isUsingRaeCoords());
    res.setValue("scale", static_cast<double>(scale()));
    res.setValue("ratio", browserRatio());
    res.setValue("borderRadius", borderRadius());
    res.setValue("opacities", _wwtCommunicator.opacities());
    res.setValue("color", color);

    std::vector<std::pair<std::string, glm::dvec3>> copies = displayCopies();
    std::vector<std::pair<std::string, bool>> showCopies = showDisplayCopies();
    ghoul::Dictionary copiesData;
    for (size_t i = 0; i < copies.size(); i++) {
        ghoul::Dictionary copy;
        copy.setValue("position", copies[i].second);
        copy.setValue("show", showCopies[i].second);
        copy.setValue("idShowProperty", showCopies[i].first);
        copiesData.setValue(copies[i].first, copy);
    }
    // Set table for the current target
    res.setValue("displayCopies", copiesData);
    return res;
}

WwtCommunicator* ScreenSpaceSkyBrowser::worldWideTelescope() {
    return &_wwtCommunicator;
}

void ScreenSpaceSkyBrowser::render(const RenderData& renderData) {
    if (!_isHidden) {
        ScreenSpaceBrowser::render(renderData);
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
            draw(mat, renderData, true);
        }
    }
}

glm::ivec3 ScreenSpaceSkyBrowser::borderColor() const {
    return _wwtBorderColor;
}

double ScreenSpaceSkyBrowser::borderRadius() const {
    return _borderRadius;
}

void ScreenSpaceSkyBrowser::setTargetRoll(double roll) {
    _equatorialAimIsDirty = true;
    _targetRoll = roll;
}

glm::dvec2 ScreenSpaceSkyBrowser::fieldsOfView() const {
    const double vFov = verticalFov();
    const double hFov = vFov * browserRatio();
    return glm::dvec2(hFov, vFov);
}

glm::dvec2 ScreenSpaceSkyBrowser::equatorialAim() const {
    return _equatorialAim;
}

void ScreenSpaceSkyBrowser::setVerticalFov(double vfov) {
    _equatorialAimIsDirty = true;
    _verticalFov = vfov;
}

void ScreenSpaceSkyBrowser::setEquatorialAim(glm::dvec2 equatorial) {
    _equatorialAim = std::move(equatorial);
    _equatorialAimIsDirty = true;
}

void ScreenSpaceSkyBrowser::setBorderColor(glm::ivec3 color) {
    _wwtBorderColor = std::move(color);
    _borderColorIsDirty = true;
}

double ScreenSpaceSkyBrowser::browserRatio() const {
    if (_dimensions.value().y > 0) {
        return static_cast<double>(_dimensions.value().x) /
            static_cast<double>(_dimensions.value().y);
    }
    else {
        return std::numeric_limits<double>::max();
    }
}

void ScreenSpaceSkyBrowser::reload() {
    _browserInstance->reloadBrowser();
}

void ScreenSpaceSkyBrowser::update() {
    // Cap how messages are passed
    const std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    const std::chrono::system_clock::duration timeSinceLastUpdate = now - _lastUpdateTime;

    if (timeSinceLastUpdate > TimeUpdateInterval) {
        if (_equatorialAimIsDirty) {
            _wwtCommunicator.setAim(_equatorialAim, _verticalFov, _targetRoll);
            _equatorialAimIsDirty = false;
        }
        if (_borderColorIsDirty) {
            _wwtCommunicator.setBorderColor(_wwtBorderColor);
            _borderColorIsDirty = false;
        }
        _lastUpdateTime = std::chrono::system_clock::now();
    }

    // Check for dirty flags
    if (_isDimensionsDirty) {
        updateTextureResolution();
    }
    // After the texture has been updated, wait a little bit before updating the border
    // radius so the browser has time to update its size
    if (_radiusIsDirty && _isInitialized && _borderRadiusTimer > RadiusTimeOut) {
        _wwtCommunicator.setBorderRadius(_borderRadius);
        _radiusIsDirty = false;
        _borderRadiusTimer = -1;
    }
    _borderRadiusTimer++;

    ScreenSpaceBrowser::update();
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

void ScreenSpaceSkyBrowser::setBorderRadius(double radius) {
    _borderRadius = radius;
    _radiusIsDirty = true;
}

void ScreenSpaceSkyBrowser::setRatio(double ratio) {
    _radiusIsDirty = true;
    _isDimensionsDirty = true;
    _dimensions = glm::uvec2(
        static_cast<unsigned int>(_dimensions.value().y * ratio),
        _dimensions.value().y
    );
}

} // namespace openspace
