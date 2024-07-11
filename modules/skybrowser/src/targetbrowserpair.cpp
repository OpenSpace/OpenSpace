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

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The thickness of the line of the target. The larger number, the thicker line.",
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
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TargetInfo = {
        "Target",
        "Sky Target",
        "The identifier of the sky target of this pair.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(TargetBrowserPair)]] Parameters {
        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<float> color;

        // [[codegen::verbatim(PointSpacecraftInfo.description)]]
        std::optional<bool> pointSpacecraft;

        // [[codegen::verbatim(UpdateDuringAnimationInfo.description)]]
        std::optional<bool> updateDuringTargetAnimation;

        // [[codegen::verbatim(BrowserInfo.description)]]
        std::string browser;

        // [[codegen::verbatim(TargetInfo.description)]]
        std::string target;
    };

#include "targetbrowserpair_codegen.cpp"
} // namespace

namespace openspace {

TargetBrowserPair::TargetBrowserPair(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "TargetBrowserPair" })
	, _targetId(TargetInfo)
	, _browserId(BrowserInfo)
	, _lineWidth(LineWidthInfo)
    , _color(ColorInfo)
    , _isPointingSpacecraft(PointSpacecraftInfo, false)
    , _updateDuringTargetAnimation(UpdateDuringAnimationInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _targetNode = global::renderEngine->scene()->sceneGraphNode(p.target);
    _browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
        global::renderEngine->screenSpaceRenderable(p.browser)
        );

    ghoul_assert(_browser, "Sky browser is null pointer");
    ghoul_assert(_targetNode, "Sky target is null pointer");

    setIdentifier(std::format("{}Pair", _browser->identifier()));

    _targetRenderable = dynamic_cast<RenderableSkyTarget*>(_targetNode->renderable());
    
    _isPointingSpacecraft = p.pointSpacecraft.value_or(_isPointingSpacecraft);
    _updateDuringTargetAnimation = p.updateDuringTargetAnimation.value_or(
        _updateDuringTargetAnimation
    );

    addProperty(_isPointingSpacecraft);
    addProperty(_updateDuringTargetAnimation);
    addProperty(_lineWidth);
    addProperty(_color);

    _lineWidth.onChange([this]() {
        _targetRenderable->property("LineWidth")->set(_lineWidth.value());
        });

    _color.onChange([this]() {
        _browser->setBorderColor(glm::ivec3(_color.value() * 255.f));
		_targetRenderable->setColor(glm::ivec3(_color.value() * 255.f));
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
    aimTargetGalactic(_targetNode->identifier(), newPosition);
}

void TargetBrowserPair::synchronizeAim() {
    const bool shouldUpdate =
        _updateDuringTargetAnimation ||
        !_targetAnimation.isAnimating();
    if (shouldUpdate && _browser->isInitialized()) {
        _browser->setEquatorialAim(targetDirectionEquatorial());
        _browser->setTargetRoll(targetRoll());
        _targetRenderable->setVerticalFov(_browser->verticalFov());
    }
}

void TargetBrowserPair::setEnabled(bool enable) {
    _browser->setEnabled(enable);
    _targetRenderable->property("Enabled")->set(enable);
}

bool TargetBrowserPair::isEnabled() const {
    return _targetRenderable->isEnabled() || _browser->isEnabled();
}

void TargetBrowserPair::initialize() {
    _targetRenderable->setColor(_browser->borderColor());
    const glm::vec2 dim = _browser->screenSpaceDimensions();
    _targetRenderable->setRatio(dim.x / dim.y);
    _browser->setBorderColor(_browser->borderColor());
    _browser->hideChromeInterface();
    _browser->setIsInitialized(true);
}

glm::ivec3 TargetBrowserPair::borderColor() const {
    return _browser->borderColor();
}

glm::dvec2 TargetBrowserPair::targetDirectionEquatorial() const {
    const glm::dvec3 cartesian = skybrowser::galacticToEquatorial(
        glm::normalize(_targetNode->worldPosition())
    );
    return skybrowser::cartesianToSpherical(cartesian);
}

glm::dvec3 TargetBrowserPair::targetDirectionGalactic() const {
    return glm::normalize(_targetNode->worldPosition());
}

std::string TargetBrowserPair::browserGuiName() const {
    return _browser->guiName();
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

double TargetBrowserPair::verticalFov() const {
    return _browser->verticalFov();
}

std::vector<std::string> TargetBrowserPair::selectedImages() const {
    return _browser->selectedImages();
}

nlohmann::json TargetBrowserPair::dataAsDictionary() const {
    const glm::dvec2 spherical = targetDirectionEquatorial();
    const glm::dvec3 cartesian = skybrowser::sphericalToCartesian(spherical);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::vector<std::string> selectedImagesIndices;

    for (const std::string& imageUrl : selectedImages()) {
        const bool imageExists = module->wwtDataHandler().image(imageUrl).has_value();
        ghoul_assert(imageExists, "Image doesn't exist in the wwt catalog!");
        selectedImagesIndices.push_back(
            module->wwtDataHandler().image(imageUrl)->identifier
        );
    }
    glm::ivec3 color = borderColor();
    std::vector<double> cartesianJson = { cartesian.x, cartesian.y, cartesian.z };
    std::vector<int> colorJson= { color.r, color.g, color.b };

    nlohmann::json result = {
        { "id", browserId() },
        { "targetId", targetNodeId() },
        { "name", browserGuiName() },
        { "fov", static_cast<double>(verticalFov()) },
        { "ra", spherical.x },
        { "dec", spherical.y },
        { "roll", targetRoll() },
        { "ratio", static_cast<double>(_browser->browserRatio()) },
        { "isFacingCamera", isFacingCamera() },
        { "isUsingRae", isUsingRadiusAzimuthElevation() },
        { "selectedImages", selectedImagesIndices },
        { "scale", static_cast<double>(_browser->scale()) },
        { "opacities", _browser->opacities() },
        { "borderRadius", _browser->borderRadius() },
        { "cartesianDirection", cartesianJson },
        { "color", colorJson }
    };

    std::vector<std::pair<std::string, glm::dvec3>> copies = displayCopies();
    std::vector<std::pair<std::string, bool>> showCopies = _browser->showDisplayCopies();
    nlohmann::json copiesData = nlohmann::json::object();
    for (size_t i = 0; i < copies.size(); i++) {
        nlohmann::json position = {
            copies[i].second.x, copies[i].second.y, copies[i].second.z
        };
        nlohmann::json copy = {
            { "position", position },
            { "show", showCopies[i].second },
            { "idShowProperty", showCopies[i].first }
        };
        copiesData[copies[i].first] = copy;
    }
    // Set table for the current target
    result["displayCopies"] = copiesData;

    return result;
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

void TargetBrowserPair::hideChromeInterface() {
    _browser->hideChromeInterface();
}

void TargetBrowserPair::sendIdToBrowser() const {
    _browser->setIdInBrowser();
}
std::vector<std::pair<std::string, glm::dvec3>> TargetBrowserPair::displayCopies() const {
    return _browser->displayCopies();
}

void TargetBrowserPair::setVerticalFov(double vfov) {
    _browser->setVerticalFov(vfov);
    _targetRenderable->setVerticalFov(vfov);
}

void TargetBrowserPair::setEquatorialAim(const glm::dvec2& aim) {
    aimTargetGalactic(
        _targetNode->identifier(),
        skybrowser::equatorialToGalactic(skybrowser::sphericalToCartesian(aim))
    );
    _browser->setEquatorialAim(aim);
}

void TargetBrowserPair::setBorderColor(const glm::ivec3& color) {
    _targetRenderable->setColor(color);
    _browser->setBorderColor(color);
}

void TargetBrowserPair::setBorderRadius(double radius) {
    _browser->setBorderRadius(radius);
    _targetRenderable->setBorderRadius(radius);
}

void TargetBrowserPair::setBrowserRatio(float ratio) {
    _browser->setRatio(ratio);
    _targetRenderable->setRatio(ratio);
}

void TargetBrowserPair::setVerticalFovWithScroll(float scroll) {
    const double fov = _browser->setVerticalFovWithScroll(scroll);
    _targetRenderable->setVerticalFov(fov);
}

void TargetBrowserPair::setImageCollectionIsLoaded(bool isLoaded) {
    _browser->setImageCollectionIsLoaded(isLoaded);
}

void TargetBrowserPair::applyRoll() {
    _targetRenderable->applyRoll();
}

void TargetBrowserPair::setPointSpaceCraft(bool shouldPoint) {
    _isPointingSpacecraft = shouldPoint;
}

void TargetBrowserPair::incrementallyAnimateToCoordinate() {
    // Animate the target before the field of view starts to animate
    if (_targetAnimation.isAnimating()) {
        aimTargetGalactic(_targetNode->identifier(), _targetAnimation.newValue());
    }
    else if (!_targetAnimation.isAnimating() && _targetIsAnimating) {
        // Set the finished position
        aimTargetGalactic(_targetNode->identifier(), _targetAnimation.newValue());
        _fovAnimation.start();
        _targetIsAnimating = false;
        _fovIsAnimating = true;
    }
    // After the target has animated to its position, animate the field of view
    if (_fovAnimation.isAnimating()) {
        _browser->setVerticalFov(_fovAnimation.newValue());
        _targetRenderable->setVerticalFov(_browser->verticalFov());
    }
    else if (!_fovAnimation.isAnimating() && _fovIsAnimating) {
        // Set the finished field of view
        setVerticalFov(_fovAnimation.newValue());
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
    _fovAnimation.stop();
    _targetAnimation.stop();
}

void TargetBrowserPair::startAnimation(glm::dvec3 galacticCoords, double fovEnd) {
    using namespace skybrowser;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const double fovSpeed = module->browserAnimationSpeed();
    // The speed is given degrees /sec
    const double fovTime = std::abs(_browser->verticalFov() - fovEnd) / fovSpeed;
    // Fov animation
    _fovAnimation = skybrowser::Animation(_browser->verticalFov(), fovEnd, fovTime);

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
    const double currentFov = verticalFov();
    startAnimation(viewDirection, currentFov);
}

double TargetBrowserPair::targetRoll() const {
    // To remove the lag effect when moving the camera while having a locked
    // target, send the locked coordinates to wwt
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
