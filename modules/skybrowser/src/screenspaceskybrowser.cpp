#include <modules/skybrowser/include/screenspaceskybrowser.h>

#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/opengl/texture.h>
#include <glm/gtx/color_space.hpp> // For hsv color
#include <optional>
#include <random>


#include <glm/gtx/string_cast.hpp>
#pragma optimize("", off)
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
    constexpr const openspace::properties::Property::PropertyInfo AnimationSpeedInfo =
    {
        "AnimationSpeed",
        "Field Of View Animation Speed",
        "The factor which is multiplied with the animation of the field of view."
    };
    constexpr const openspace::properties::Property::PropertyInfo TextureQualityInfo =
    {
        "TextureQuality",
        "Quality of Texture",
        "A parameter to set the resolution of the texture. 1 is full resolution and "
        "slower framerate. Lower value means lower resolution of texture and faster "
        "frame rate."
    };

    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<float> verticalFov;

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::ivec3> borderColor;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<double> animationSpeed;

        // [[codegen::verbatim(TextureQualityInfo.description)]]
        std::optional<double> textureQuality;
    };

#include "ScreenSpaceSkyBrowser_codegen.cpp"
} // namespace

namespace openspace {

    glm::ivec3 randomBorderColor() {
        // Generate a random border color with sufficient lightness and a n
        std::random_device rd;
        // Hue is in the unit degrees [0, 360]
        std::uniform_real_distribution<float> hue(0.f, 360.f);
        // Value in saturation are in the unit percent [0,1]
        float value = 0.95f; // Brightness
        float saturation = 0.5f;
        glm::vec3 hsvColor = glm::vec3(hue(rd), saturation, value);
        glm::ivec3 rgbColor = glm::ivec3(glm::rgbColor(hsvColor) * 255.f);
        return rgbColor;
    }

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , WwtCommunicator(dictionary)
        , _animationSpeed(AnimationSpeedInfo, 5.0, 0.1, 10.0)
        , _textureQuality(TextureQualityInfo, 1.f, 0.25f, 1.f)
    {
        // Set a unique identifier
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyBrowser";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        // Make the color property display a color picker in the GUI
        _borderColor.setViewOption(properties::Property::ViewOptions::Color, true);

        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _verticalFov = p.verticalFov.value_or(_verticalFov);
        _borderColor = p.borderColor.value_or(randomBorderColor());
        _textureQuality = p.textureQuality.value_or(_textureQuality);

        addProperty(_url);
        addProperty(_dimensions);
        addProperty(_reload);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_equatorialAim);
        addProperty(_textureQuality);

        _textureQuality.onChange([this]() {
            updateTextureResolution();
            });

        // Ensure that the browser is placed at the z-coordinate of the screen space plane
        glm::vec2 screenPosition = _cartesianPosition.value();
        _cartesianPosition.setValue(glm::vec3(screenPosition, skybrowser::ScreenSpaceZ));
    }

    ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {

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
        glm::dvec2 convertToScreenSpace{ (2 * skybrowser::windowRatio()), 2.f };
        glm::dvec2 result = - convertToScreenSpace * resultRelativeOs;
        return result;
    }
    
    void ScreenSpaceSkyBrowser::setIdInBrowser() {
        WwtCommunicator::setIdInBrowser(identifier());
    }

    void ScreenSpaceSkyBrowser::updateTextureResolution()
    {
        
        // Scale texture depending on the height of the window
        // Set texture size to the actual pixel size it covers
        glm::vec2 pixels = glm::vec2(global::windowDelegate->currentSubwindowSize());
        glm::vec2 browserDim = screenSpaceDimensions();
        float newResY = pixels.y * browserDim.y;
        float newResX = newResY * (_dimensions.value().x / _dimensions.value().y);
        glm::vec2 newSize = glm::vec2(newResX , newResY) * _textureQuality.value();

        _dimensions = glm::ivec2(newSize);
        _texture->setDimensions(glm::ivec3(newSize, 1));
    }

    bool ScreenSpaceSkyBrowser::deinitializeGL() {
        ScreenSpaceRenderable::deinitializeGL();
        WwtCommunicator::deinitializeGL();
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
        float diff = _endVfov - verticalFov();
        bool shouldAnimate = abs(diff) > FovThreshold;

        if (shouldAnimate) {
            float delta = _animationSpeed * (diff * deltaTime);
            _verticalFov = std::clamp(_verticalFov + delta, 0.0001f, 70.0f);
        }
        else {
            _isFovAnimated = false;
        }
    }

    void ScreenSpaceSkyBrowser::render() {
        WwtCommunicator::render();

        draw(
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }

    void ScreenSpaceSkyBrowser::update() {
        
        if (global::windowDelegate->windowHasResized()) {
            // Change the resolution of the texture
            updateTextureResolution();
        }

        _objectSize = _texture->dimensions();
        
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
        if (!intersection(coord)) return resizePosition;

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

    void ScreenSpaceSkyBrowser::setScale(float scalingFactor) {
        _scale = _originalScale * scalingFactor;
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
        _originalDimensions = _dimensions;
        _originalScale = _scale;
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


    void ScreenSpaceSkyBrowser::setOpacity(float opacity)
    {
        _opacity = opacity;
    }

    float ScreenSpaceSkyBrowser::opacity() const {
        return _opacity;
    }
}
