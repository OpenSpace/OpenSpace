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

#include <modules/skybrowser/include/targetbrowserpair.h>

#include <modules/skybrowser/include/renderableskytarget.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/constants.hpp>
#include <functional>
#include <chrono>
#include <glm/gtx/color_space.hpp>
#include <random>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "TargetBrowserPair";

    void aimTargetGalactic(std::string_view id, const glm::dvec3& direction) {
        const glm::dvec3 positionCelestial = glm::normalize(direction) *
            openspace::skybrowser::CelestialSphereRadius;

        const std::string script = std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Translation.Position', {});",
            id, ghoul::to_string(positionCelestial)
        );
        openspace::global::scriptEngine->queueScript(
            script,
            openspace::scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            openspace::scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

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

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Determines if this target browser pair is enabled or not. If it is enabled, "
        "the target and the browser that the pair controls is enabled.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the border of the sky browser and the line of the target.",
        openspace::properties::Property::Visibility::NoviceUser
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

    constexpr openspace::properties::Property::PropertyInfo BrowserInfo = {
        "Browser",
        "Sky Browser",
        "The identifier of the sky browser of this pair.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TargetInfo = {
        "Target",
        "Sky Target",
        "The identifier of the sky target of this pair.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo VerticalFovInfo = {
        "VerticalFov",
        "Vertical Field Of View",
        "The vertical field of view of the target.",
        openspace::properties::Property::Visibility::User
    };
    
    constexpr openspace::properties::Property::PropertyInfo StopAnimationsInfo = {
        "StopAnimations",
        "Stop Animations",
        "Stops all animations for the sky browser and target of this pair.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BorderRadiusInfo = {
        "BorderRadius",
        "Border Radius",
        "The border radius of this Sky Browser.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RatioInfo = {
        "Ratio",
        "Ratio",
        "The ratio of the dimensions of the sky browser and target. This is defined as "
        "width divided by height.",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo EquatorialAimInfo = {
        "EquatorialAim",
        "Equatorial Aim",
        "The equatorial aim (Ra, dec) of the sky browser and target.",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(TargetBrowserPair)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(PointSpacecraftInfo.description)]]
        std::optional<bool> pointSpacecraft;

        // [[codegen::verbatim(UpdateDuringAnimationInfo.description)]]
        std::optional<bool> updateDuringTargetAnimation;

        // [[codegen::verbatim(BrowserInfo.description)]]
        std::string browser;

        // [[codegen::verbatim(TargetInfo.description)]]
        std::string target;

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<double> verticalFov;

        // [[codegen::verbatim(BorderRadiusInfo.description)]]
        std::optional<double> borderRadius;

        // [[codegen::verbatim(RatioInfo.description)]]
        std::optional<float> ratio;

        // [[codegen::verbatim(EquatorialAimInfo.description)]]
        std::optional<glm::dvec2> equatorialAim;
    };

#include "targetbrowserpair_codegen.cpp"
} // namespace

namespace openspace {

TargetBrowserPair::TargetBrowserPair(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "TargetBrowserPair" })
	, _targetId(TargetInfo)
	, _browserId(BrowserInfo)
	, _enabled(EnabledInfo, true)
    , _color(ColorInfo)
    , _borderRadius(BorderRadiusInfo, 0.0, 0.0, 1.0)
    , _isPointingSpacecraft(PointSpacecraftInfo, false)
    , _updateDuringTargetAnimation(UpdateDuringAnimationInfo, false)
    , _verticalFov(VerticalFovInfo, 10.0, 0.00000000001, 70.0)
    , _stopAnimations(StopAnimationsInfo)
    , _ratio(RatioInfo)
    , _equatorialAim(
        EquatorialAimInfo, glm::dvec2(0.0), glm::dvec2(0.0, -90.0), glm::dvec2(360.0, 90.0)
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _targetNode = global::renderEngine->scene()->sceneGraphNode(p.target);
    _browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
        global::renderEngine->screenSpaceRenderable(p.browser)
        );

    ghoul_assert(_browser, "Sky browser is null pointer");
    ghoul_assert(_targetNode, "Sky target is null pointer");

    setIdentifier(std::format("{}Pair", _browser->identifier()));

    _verticalFov = p.verticalFov.value_or(_verticalFov);
    _borderRadius = p.verticalFov.value_or(_borderRadius);
    _ratio = p.ratio.value_or(_ratio);
    _equatorialAim = p.equatorialAim.value_or(_equatorialAim);

    _targetRenderable = dynamic_cast<RenderableSkyTarget*>(_targetNode->renderable());
    
    _isPointingSpacecraft = p.pointSpacecraft.value_or(_isPointingSpacecraft);
    _updateDuringTargetAnimation = p.updateDuringTargetAnimation.value_or(
        _updateDuringTargetAnimation
    );

    addProperty(_isPointingSpacecraft);
    addProperty(_updateDuringTargetAnimation);
    addProperty(_ratio);
    addProperty(_color);
    addProperty(_borderRadius);
    addProperty(_enabled);
    addProperty(_stopAnimations);
    addProperty(_equatorialAim);

    _borderRadius.onChange([this]() {
        _browser->property("BorderRadius")->set(_borderRadius.value());
        _targetRenderable->property("BorderRadius")->set(_borderRadius.value());
        });

    _color.onChange([this]() {
        _browser->setBorderColor(glm::ivec3(_color.value() * 255.f));
		_targetRenderable->setColor(_color.value());
        });

    _verticalFov.onChange([this]() {
        _browser->property("VerticalFov")->set(_verticalFov.value());
        _targetRenderable->property("VerticalFov")->set(_verticalFov.value());
        });

    _enabled.onChange([this]() {
        _browser->property("Enabled")->set(_enabled.value());
        _targetRenderable->property("Enabled")->set(_enabled.value());
        });

    _ratio.onChange([this]() {
        _browser->property("Ratio")->set(_ratio.value());
        _targetRenderable->property("Ratio")->set(_ratio.value());
        });

    _stopAnimations.onChange([this]() {
        _fovAnimation.stop();
        _targetAnimation.stop();
        });

    _equatorialAim.onChange([this]() {
        glm::dvec3 aimGalactic = skybrowser::equatorialToGalactic(
            skybrowser::sphericalToCartesian(_equatorialAim.value())
        );
        aimTargetGalactic(_targetNode->identifier(), aimGalactic);
        _browser->property("EquatorialAim")->set(_equatorialAim.value());
        });

	if (global::windowDelegate->isMaster()) {
		_color = glm::vec3(randomBorderColor()) / 255.f;
	}
    _browser->setAsPaired();
}

void TargetBrowserPair::setImageOrder(const std::string& imageUrl, int order) {
    _browser->setImageOrder(imageUrl, order);
}

void TargetBrowserPair::startFinetuningTarget() {
    _startTargetPosition = _targetNode->worldPosition();
}

// The fine tune of the target is a way to "drag and drop" the target with click
// drag on the sky browser window. This is to be able to drag the target around when it
// has a very small field of view
void TargetBrowserPair::fineTuneTarget(const glm::vec2& translation) {
    const glm::dvec2 percentage = glm::dvec2(translation);
    const glm::dvec3 right = _targetRenderable->rightVector() * percentage.x;
    const glm::dvec3 up = _targetRenderable->upVector() * percentage.y;

    const glm::dvec3 newPosition = _startTargetPosition - (right - up);
    _equatorialAim = skybrowser::cartesianToSpherical(
        skybrowser::galacticToEquatorial(newPosition)
    );
}

void TargetBrowserPair::synchronizeAim() {
    const bool shouldUpdate =
        _updateDuringTargetAnimation ||
        !_targetAnimation.isAnimating();
    if (shouldUpdate && _browser->isInitialized()) {
        _browser->setTargetRoll(targetRoll());
    }
}

void TargetBrowserPair::setEnabled(bool enable) {
    _enabled = enable;
}

bool TargetBrowserPair::isEnabled() const {
    return _enabled;
}

void TargetBrowserPair::initialize() {
    _targetRenderable->setColor(_color.value());
    const glm::vec2 dim = _browser->screenSpaceDimensions();
    _targetRenderable->setRatio(dim.x / dim.y);
    _browser->setBorderColor(_browser->borderColor());
    _browser->hideChromeInterface();
    _browser->property("EquatorialAim")->set(_equatorialAim.value());
    _browser->setIsInitialized(true);
}

void TargetBrowserPair::reloadDisplayCopies() {
    _browser->setIsInitialized(false);
    _browser->setImageCollectionIsLoaded(false);
    _browser->reload();
}

glm::ivec3 TargetBrowserPair::borderColor() const {
    return _color.value() * 255.f;
}

glm::dvec3 TargetBrowserPair::targetDirectionGalactic() const {
    return glm::normalize(_targetNode->worldPosition());
}

std::string TargetBrowserPair::browserId() const {
    return _browser->identifier();
}

std::string TargetBrowserPair::targetRenderableId() const {
    return _targetRenderable->identifier();
}

std::string TargetBrowserPair::targetNodeId() const {
    return _targetNode->identifier();
}

bool TargetBrowserPair::pointSpaceCraft() const {
    return _isPointingSpacecraft;
}

std::vector<std::string> TargetBrowserPair::selectedImages() const {
    return _browser->selectedImages();
}

void TargetBrowserPair::selectImage(const ImageData& image) {
    // Load image into browser
    _browser->selectImage(image.imageUrl);

    // If the image has coordinates, move the target
    if (image.hasCelestialCoords) {
        // Animate the target to the image coordinate position
        const glm::dvec3 galactic = skybrowser::equatorialToGalactic(
            image.equatorialCartesian
        );
        startAnimation(galactic * skybrowser::CelestialSphereRadius, image.fov);
    }
}

void TargetBrowserPair::addImageLayerToWwt(const std::string& imageUrl) {
    _browser->addImageLayerToWwt(imageUrl);
}

void TargetBrowserPair::removeSelectedImage(const std::string& imageUrl) {
    _browser->removeSelectedImage(imageUrl);
}

void TargetBrowserPair::loadImageCollection(const std::string& collection) {
    _browser->loadImageCollection(collection);
}

void TargetBrowserPair::setImageOpacity(const std::string& imageUrl, float opacity) {
    _browser->setImageOpacity(imageUrl, opacity);
}

void TargetBrowserPair::sendIdToBrowser() const {
    _browser->setIdInBrowser();
}
std::vector<std::pair<std::string, glm::dvec3>> TargetBrowserPair::displayCopies() const {
    return _browser->displayCopies();
}

void TargetBrowserPair::setVerticalFov(double vfov) {
    _verticalFov = vfov;
}

void TargetBrowserPair::setEquatorialAim(const glm::dvec2& aim) {
    _equatorialAim = aim;
}

void TargetBrowserPair::setBorderColor(const glm::ivec3& color) {
    _color = glm::vec3(color) / 255.f;
}

void TargetBrowserPair::setBorderRadius(double radius) {
    _borderRadius = radius;
}

void TargetBrowserPair::setBrowserRatio(float ratio) {
    _ratio = ratio;
}

void TargetBrowserPair::setVerticalFovWithScroll(float scroll) {
    // Make scroll more sensitive the smaller the FOV
    const double x = _verticalFov;
    const double zoomFactor =
        atan(x / 50.0) + exp(x / 40.0) - 0.99999999999999999999999999999;
    const double zoom = scroll > 0.0 ? zoomFactor : -zoomFactor;
    _verticalFov = std::clamp(_verticalFov + zoom, 0.0, 70.0);
}

void TargetBrowserPair::setImageCollectionIsLoaded(bool isLoaded) {
    _browser->setImageCollectionIsLoaded(isLoaded);
}

void TargetBrowserPair::setPointSpaceCraft(bool shouldPoint) {
    _isPointingSpacecraft = shouldPoint;
}

void TargetBrowserPair::incrementallyAnimateToCoordinate() {
    // Animate the target before the field of view starts to animate
    if (_targetAnimation.isAnimating()) {
        _equatorialAim = skybrowser::cartesianToSpherical(
            skybrowser::galacticToEquatorial(_targetAnimation.newValue()
            ));
    }
    else if (!_targetAnimation.isAnimating() && _targetIsAnimating) {
        // Set the finished position
        _equatorialAim = skybrowser::cartesianToSpherical(
            skybrowser::galacticToEquatorial(_targetAnimation.newValue()
            ));
        _fovAnimation.start();
        _targetIsAnimating = false;
        _fovIsAnimating = true;
    }
    // After the target has animated to its position, animate the field of view
    if (_fovAnimation.isAnimating()) {
        _verticalFov = _fovAnimation.newValue();
    }
    else if (!_fovAnimation.isAnimating() && _fovIsAnimating) {
        // Set the finished field of view
        _verticalFov = _fovAnimation.newValue();
        _fovIsAnimating = false;
    }
}

void TargetBrowserPair::startFading(float goal, float fadeTime) {
    const std::string script = std::format(
        "openspace.setPropertyValueSingle('Scene.{0}.Renderable.Fade', {2}, {3});"
        "openspace.setPropertyValueSingle('ScreenSpace.{1}.Fade', {2}, {3});",
        _targetNode->identifier(), _browser->identifier(), goal, fadeTime
    );

    global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

void TargetBrowserPair::stopAnimations() {
    _stopAnimations.trigger();
}

void TargetBrowserPair::startAnimation(glm::dvec3 galacticCoords, double fovEnd) {
    using namespace skybrowser;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const double fovSpeed = module->browserAnimationSpeed();
    // The speed is given degrees /sec
    const double fovTime = std::abs(_verticalFov - fovEnd) / fovSpeed;
    // Fov animation
    _fovAnimation = skybrowser::Animation(_verticalFov.value(), fovEnd, fovTime);

    // Target animation
    const glm::dvec3 start = glm::normalize(_targetNode->worldPosition()) *
        skybrowser::CelestialSphereRadius;
    const double targetSpeed = module->targetAnimationSpeed();
    const double angle = angleBetweenVectors(start, galacticCoords);
    _targetAnimation = Animation(start, std::move(galacticCoords), angle / targetSpeed);
    _targetAnimation.start();
    _targetIsAnimating = true;
}

void TargetBrowserPair::centerTargetOnScreen() {
    // Get camera direction in celestial spherical coordinates
    const glm::dvec3 viewDirection = skybrowser::cameraDirectionGalactic();
    // Keep the current fov
    const double currentFov = _verticalFov;
    startAnimation(viewDirection, currentFov);
}

double TargetBrowserPair::targetRoll() const {
    const glm::dvec3 normal = glm::normalize(
        _targetNode->worldPosition() -
        global::navigationHandler->camera()->positionVec3()
    );
    const glm::dvec3 right = _targetRenderable->rightVector();
    const glm::dvec3 up = glm::normalize(glm::cross(right, normal));
    return skybrowser::targetRoll(up, normal);
}

bool TargetBrowserPair::isFacingCamera() const {
    return _browser->isFacingCamera();
}

bool TargetBrowserPair::isUsingRadiusAzimuthElevation() const {
    return _browser->isUsingRaeCoords();
}

ScreenSpaceSkyBrowser* TargetBrowserPair::browser() const {
    return _browser;
}

} // namespace openspace
