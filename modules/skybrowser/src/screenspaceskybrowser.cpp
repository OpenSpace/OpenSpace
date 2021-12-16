#include <modules/skybrowser/include/screenspaceskybrowser.h>

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/opengl/texture.h>
#include <optional>

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
    constexpr const openspace::properties::Property::PropertyInfo SizeInfo =
    {
        "Size",
        "Screen space size of the sky browser",
        "The size of the sky browser determines how large is on screen. The y parameter "
        "determines the percentage of the screen the browser will cover in the "
        "y-direction. The x parameter determines how large the sky browser will be in "
        "the x direction."
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

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , WwtCommunicator(dictionary)
        , _animationSpeed(AnimationSpeedInfo, 5.0, 0.1, 10.0)
        , _textureQuality(TextureQualityInfo, 1.f, 0.25f, 1.f)
        , _size(SizeInfo, glm::vec2(0.5f), glm::vec2(0.f), glm::vec2(2.f))
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
        _borderColor = p.borderColor.value_or(_borderColor);
        _textureQuality = p.textureQuality.value_or(_textureQuality);

        addProperty(_url);
        addProperty(_browserPixeldimensions);
        addProperty(_reload);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_equatorialAim);
        addProperty(_textureQuality);
        addProperty(_size);

        _textureQuality.onChange([this]() {
            _textureDimensionsIsDirty = true;
            });        
        _size.onChange([this]() {
            setScreenSpaceSize(_size);
            });

        // Ensure that the browser is placed at the z-coordinate of the screen space plane
        glm::vec2 screenPosition = _cartesianPosition.value();
        _cartesianPosition.setValue(glm::vec3(screenPosition, skybrowser::ScreenSpaceZ));
        
    }

    ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        if (module->getPair(identifier())) {
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
           
        // If the scale is 1, it covers half the window. Hence multiplication with 2
        float newResY = pixels.y * 2.f * _scale; 
        float newResX = newResY * (_browserPixeldimensions.value().x / _browserPixeldimensions.value().y);
        glm::vec2 newSize = glm::vec2(newResX , newResY) * _textureQuality.value();

        _browserPixeldimensions = glm::ivec2(newSize);
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
        // Texture of window is 1x1 when minimized
        bool isWindow = global::windowDelegate->currentSubwindowSize() != glm::ivec2(1);
        bool isWindowResized = global::windowDelegate->windowHasResized();
        if ((isWindowResized && isWindow) || _textureDimensionsIsDirty) {
            updateTextureResolution();
            _textureDimensionsIsDirty = false;
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

    void ScreenSpaceSkyBrowser::setCallbackEquatorialAim(
        std::function<void(const glm::dvec2&)> function)
    {
        _equatorialAim.onChange([this, f = std::move(function)]() {
            f(_equatorialAim);
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
        _browserPixeldimensions.onChange([this, f = std::move(function)]() {
            f(_browserPixeldimensions);
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

    void ScreenSpaceSkyBrowser::setScreenSpaceSize(const glm::vec2& newSize)
    {
        _scale = abs(newSize.y) * 0.5f;
        glm::vec2 newSizeRelToOld = abs((screenSpaceDimensions() + newSize) /
            screenSpaceDimensions());
        glm::vec2 newDimensions = newSizeRelToOld * glm::vec2(_texture->dimensions());
        // Scale the browser
        // Scale on the y axis, this is to ensure that _scale = 1 is
        // equal to the height of the window
        _browserPixeldimensions = glm::ivec2(newDimensions);
        _texture->setDimensions(glm::ivec3(newDimensions, 1));
        _objectSize = _texture->dimensions();

        updateTextureResolution();
    }

    float ScreenSpaceSkyBrowser::opacity() const {
        return _opacity;
    }
}
