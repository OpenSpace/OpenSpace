#include <modules/skybrowser/include/screenspaceskybrowser.h>

#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/opengl/texture.h>
#include <glm/gtx/color_space.hpp>
#include <optional>
#include <random>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyBrowser";

    constexpr const openspace::properties::Property::PropertyInfo BorderColorInfo =
    {
        "BorderColor",
        "Border Color",
        "The color of the border of the sky browser."
    };
    constexpr const openspace::properties::Property::PropertyInfo VerticalFovInfo =
    {
        "VerticalFieldOfView",
        "Vertical Field Of View",
        "The vertical field of view in degrees."
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<float> verticalFov;

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::ivec3> borderColor;
    };

#include "ScreenSpaceSkyBrowser_codegen.cpp"
} // namespace

namespace openspace {

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary),
        WwtCommunicator(dictionary)
    {
        // Set a unique identifier
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyBrowser22";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        // Make the color property display a color picker in the GUI
        _borderColor.setViewOption(properties::Property::ViewOptions::Color, true);

        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _verticalFov = p.verticalFov.value_or(_verticalFov);
        _borderColor = p.borderColor.value_or(_borderColor);

        addProperty(_url);
        addProperty(_dimensions);
        addProperty(_reload);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_equatorialAim);

        glm::vec2 screenPosition = _cartesianPosition.value();
        _cartesianPosition.setValue(glm::vec3(screenPosition, skybrowser::ScreenSpaceZ));

        // Generate a random border color
        std::random_device rd;
        std::uniform_int_distribution<int> hue(0, 255);
        int value = 220;
        int saturation = 128;
        glm::ivec3 hsvColor = glm::ivec3(hue(rd), saturation , value);
        _borderColor = glm::rgbColor(hsvColor);
    }

    ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {

    }

    bool ScreenSpaceSkyBrowser::initializeGL() {

        Browser::initializeGL();
        ScreenSpaceRenderable::initializeGL();
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
        glm::dvec2 convertToScreenSpace{ (2 * skybrowser::windowRatio()), 2.f };
        glm::dvec2 result = - convertToScreenSpace * resultRelativeOs;
        return result;
    }
    
    void ScreenSpaceSkyBrowser::setIdInBrowser() {
        WwtCommunicator::setIdInBrowser(identifier());
    }

    bool ScreenSpaceSkyBrowser::deinitializeGL() {
        ScreenSpaceRenderable::deinitializeGL();
        Browser::deinitializeGL();
        return true;
    }

    bool ScreenSpaceSkyBrowser::isAnimated()
    {
        return _isFovAnimated;
    }

    void ScreenSpaceSkyBrowser::startFovAnimation(float fov)
    {
        _isFovAnimated = true;
        _endVfov = fov;
    }

    void ScreenSpaceSkyBrowser::incrementallyAnimateToFov(float deltaTime)
    {
        // If distance too large, keep animating. Else, stop animation
        float diff = verticalFov() - _endVfov;
        const bool shouldAnimate = abs(diff) > _fovDiff;

        if (shouldAnimate) {
            setVerticalFovWithScroll(diff);
        }
        else {
            _isFovAnimated = false;
        }
    }

    void ScreenSpaceSkyBrowser::render() {
        Browser::render();

        draw(
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }

    void ScreenSpaceSkyBrowser::update() {
        
        _objectSize = _texture->dimensions();

        if (global::windowDelegate->windowHasResized()) {
            // change the resolution of the texture
        }
        
        WwtCommunicator::update();
        ScreenSpaceRenderable::update();
    }

    void ScreenSpaceSkyBrowser::setVerticalFovWithScroll(float scroll) {
        // Cap how often the zoom is allowed to update
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
        std::chrono::system_clock::duration timeSinceLastUpdate = now - _lastUpdateTime;

        if (timeSinceLastUpdate > _timeUpdateInterval) {
            // Make scroll more sensitive the smaller the FOV
            float x = _verticalFov;
            float zoomFactor = atan(x / 50.0) + exp(x / 40) - 0.999999;
            float zoom = scroll > 0.0 ? -zoomFactor : zoomFactor;
            _verticalFov = std::clamp(_verticalFov + zoom, 0.001f, 70.0f);
            _lastUpdateTime = std::chrono::system_clock::now();
        }
    }

    void ScreenSpaceSkyBrowser::bindTexture()
    {
        _texture->bind();
    }

    glm::ivec2 ScreenSpaceSkyBrowser::isOnResizeArea(glm::vec2 coord) {
        glm::ivec2 resizePosition = glm::ivec2{ 0 };
        // Make sure coordinate is on browser
        if (!coordIsInsideCornersScreenSpace(coord)) return resizePosition;

        // TO DO: turn this into a vector and use prettier vector arithmetic
        float resizeAreaY = screenSpaceDimensions().y * _resizeAreaPercentage;
        float resizeAreaX = screenSpaceDimensions().x * _resizeAreaPercentage;

        const bool isOnTop = coord.y > upperRightCornerScreenSpace().y - resizeAreaY;
        const bool isOnBottom = coord.y < lowerLeftCornerScreenSpace().y + resizeAreaY;
        const bool isOnRight = coord.x > upperRightCornerScreenSpace().x - resizeAreaX;
        const bool isOnLeft = coord.x < lowerLeftCornerScreenSpace().x + resizeAreaX;

        resizePosition.x = isOnRight ? 1 : isOnLeft ? -1 : 0;
        resizePosition.y = isOnTop ? 1 : isOnBottom ? -1 : 0;

        return  resizePosition;
    }
    // Scales the ScreenSpaceBrowser to a new ratio
    void ScreenSpaceSkyBrowser::setScale(glm::vec2 scalingFactor) {

        // Scale on the y axis, this is to ensure that _scale = 1 is
        // equal to the height of the window
        setScale(abs(scalingFactor.y));
        // Resize the dimensions of the texture on the x axis
        glm::vec2 newSize = abs(scalingFactor) * _originalDimensions;
        _texture->setDimensions(glm::ivec3(newSize, 1));
        _objectSize = _texture->dimensions();
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

    void ScreenSpaceSkyBrowser::saveResizeStartSize() {
        _originalDimensions = _dimensions.value();
        _originalScale = _scale.value();
    }

    void ScreenSpaceSkyBrowser::setCallbackEquatorialAim(
        std::function<void(glm::dvec3, bool)> function)
    {
        _equatorialAim.onChange([this, f = std::move(function)]() {
            f(skybrowser::sphericalToCartesian(_equatorialAim), false);
        });

    }
    void ScreenSpaceSkyBrowser::setCallbackVerticalFov(
        std::function<void(float)> function)
    {
        _verticalFov.onChange([this, f = std::move(function)]() {
            f(_verticalFov.value());
        });

    }
    void ScreenSpaceSkyBrowser::setCallbackDimensions(
        std::function<void(const glm::vec2&)> function)
    {
        _dimensions.onChange([this, f = std::move(function)]() {
            f(_dimensions);
        });

    }
    void ScreenSpaceSkyBrowser::setCallbackBorderColor(
        std::function<void(const glm::ivec3&)> function) {
        _borderColor.onChange([this, f = std::move(function)]() {
            f(_borderColor.value());
        });
    }
    void ScreenSpaceSkyBrowser::setCallbackEnabled(std::function<void(bool)> function)
    {
        _enabled.onChange([this, f = std::move(function)]() {
            f(_enabled);
        });
    }
    void ScreenSpaceSkyBrowser::setScale(float scalingFactor) {
        _scale = _originalScale * scalingFactor;
    }

    void ScreenSpaceSkyBrowser::setOpacity(float opacity)
    {
        _opacity = opacity;
    }

    float ScreenSpaceSkyBrowser::opacity() const {
        return _opacity;
    }
}
