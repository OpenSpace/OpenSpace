#include <modules/skybrowser/include/ScreenSpaceSkyBrowser2.h>

#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyBrowser2";

    constexpr const openspace::properties::Property::PropertyInfo BrowserDimensionInfo =
    {
        "BrowserDimensions",
        "Browser Dimensions",
        "The pixel dimensions of the sky browser."
    };
    constexpr const openspace::properties::Property::PropertyInfo TargetIdInfo =
    {
        "TargetId",
        "Target Id",
        "The identifier of the target. It is used to synchronize the sky browser and the"
        "sky target."
    };
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

    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser2)]] Parameters {

        // [[codegen::verbatim(BrowserDimensionInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<float> verticalFov;

        // [[codegen::verbatim(TargetIdInfo.description)]]
        std::optional<std::string> targetId;

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::ivec3> borderColor;
    };

#include "ScreenSpaceSkyBrowser2_codegen.cpp"
} // namespace

namespace openspace {

    ScreenSpaceSkyBrowser2::ScreenSpaceSkyBrowser2(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary),
        WwtCommunicator(dictionary)
        , _skyTargetId(TargetIdInfo)
    {
        // Make the color property display a color picker in the GUI
        _borderColor.setViewOption("Color", true);

        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _verticalFov = p.verticalFov.value_or(_verticalFov);
        _borderColor = p.borderColor.value_or(_borderColor);
        _skyTargetId = p.targetId.value_or(_skyTargetId);

        addProperty(_dimensions);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_skyTargetId);

        _verticalFov.onChange([&]() {
            if (_skyTarget) {
                _skyTarget->setScale(_verticalFov);
            }
            });
        _borderColor.onChange([&]() {
            setWebpageBorderColor(_borderColor.value());
            });
        _skyTargetId.onChange([&]() {
            connectToSkyTarget();
            });

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

        glm::vec2 screenPosition = _cartesianPosition.value();
        _cartesianPosition.setValue(glm::vec3(screenPosition, skybrowser::ScreenSpaceZ));

        // Always make sure that the target and browser are visible together
        _enabled.onChange([&]() {
            if (_skyTarget) {
                _skyTarget->property("Enabled")->set(_enabled.value());
            }
        });

        // Ensure the color of the border is bright enough.
        // Make sure the RGB color at least is 50% brightness
        // By making each channel 50% bright in general
        // 222 = sqrt(3*(0.5*256)^2)
        while (glm::length(glm::vec3(_borderColor.value())) < 222.f) {
            _borderColor = glm::vec3(rand() % 256, rand() % 256, rand() % 256);
        }
    }

    ScreenSpaceSkyBrowser2::~ScreenSpaceSkyBrowser2() {
        // Set flag to false so the thread can exit
        _isSyncedWithWwt = false;
        if (_wwtMessages.joinable()) {
            _wwtMessages.join();
            LINFO("Joined thread");
        }
    }

    bool ScreenSpaceSkyBrowser2::initializeGL() {

        return Browser::initializeGL() && ScreenSpaceRenderable::initializeGL();
    }


    void ScreenSpaceSkyBrowser2::startSyncingWithWwt() {
        // If the camera is already synced, the browser is already syncing
        if (!_isSyncedWithWwt) {
            _isSyncedWithWwt = true;
            // Set border color
            setWebpageBorderColor(_borderColor.value());
            // Track target
            syncWwtView();
        }
    }

    glm::dvec2 ScreenSpaceSkyBrowser2::fineTuneVector(glm::dvec2 drag) {
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
    /*
    void ScreenSpaceBrowser::setXCallback(std::function<void()> callback) {
        _originalDimensions.onChange(std::move(callback));
    }
    */
    bool ScreenSpaceSkyBrowser2::deinitializeGL() {
        // Set flag to false so the thread can exit
        _isSyncedWithWwt = false;
        if (_wwtMessages.joinable()) {
            _wwtMessages.join();
            LINFO("Joined thread");
        }
        return Browser::deinitializeGL() && ScreenSpaceRenderable::deinitializeGL();
    }

    bool ScreenSpaceSkyBrowser2::connectToSkyTarget() {
        _skyTarget = dynamic_cast<ScreenSpaceSkyTarget*>(
            global::renderEngine->screenSpaceRenderable(_skyTargetId.value()));
        return _skyTarget;
    }

    bool ScreenSpaceSkyBrowser2::isAnimated()
    {
        return _isFovAnimated;
    }

    void ScreenSpaceSkyBrowser2::startFovAnimation(float fov)
    {
        _isFovAnimated = true;
        _endVfov = fov;
    }

    void ScreenSpaceSkyBrowser2::incrementallyAnimateToFov(float deltaTime)
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

    void ScreenSpaceSkyBrowser2::render() {
        Browser::render();

        draw(
            globalRotationMatrix() *
            translationMatrix() *
            localRotationMatrix() *
            scaleMatrix()
        );
    }

    void ScreenSpaceSkyBrowser2::update() {
        Browser::update();
        ScreenSpaceRenderable::update();
    }

    ScreenSpaceSkyTarget* ScreenSpaceSkyBrowser2::getSkyTarget() {
        return _skyTarget;
    }

    void ScreenSpaceSkyBrowser2::setVerticalFovWithScroll(float scroll) {
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

    void ScreenSpaceSkyBrowser2::syncWwtView() {

        // Start a thread to enable user interaction while sending the calls to WWT
        _wwtMessages = std::thread([&] {
            while (_isSyncedWithWwt) {
                if (_skyTarget) {
                    // Message WorldWide Telescope current view
                    glm::dvec3 cartesian = _skyTarget->directionEquatorial();

                    ghoul::Dictionary message = wwtmessage::moveCamera(
                        skybrowser::cartesianToSpherical(cartesian),
                        _verticalFov,
                        skybrowser::cameraRoll()
                    );
                    sendMessageToWwt(message);
                }

                // Sleep so we don't bombard WWT with too many messages
                std::this_thread::sleep_for(std::chrono::milliseconds(50));
            }
        });

    }

    glm::ivec2 ScreenSpaceSkyBrowser2::isOnResizeArea(glm::vec2 coord) {
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
    void ScreenSpaceSkyBrowser2::setScale(glm::vec2 scalingFactor) {

        // Scale on the y axis, this is to ensure that _scale = 1 is
        // equal to the height of the window
        setScale(abs(scalingFactor.y));
        // Resize the dimensions of the texture on the x axis
        glm::vec2 newSize = abs(scalingFactor) * _originalDimensions;
        _texture->setDimensions(glm::ivec3(newSize, 1));
        _objectSize = _texture->dimensions();
    }

    glm::mat4 ScreenSpaceSkyBrowser2::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale tells us how much of the windows height the
        // browser covers: e.g. a browser that covers 0.25 of the
        // height of the window will have scale = 0.25

        float textureRatio = static_cast<float>(_texture->dimensions().x) /
                             static_cast<float>(_texture->dimensions().y);

        glm::mat4 scale = glm::scale(
            glm::mat4(1.f),
            glm::vec3(textureRatio * _scale, _scale, 1.f)
        );
        return scale;
    }

    void ScreenSpaceSkyBrowser2::saveResizeStartSize() {
        _originalDimensions = _dimensions.value();
        _originalScale = _scale.value();
    }

    // Updates the browser size to match the size of the texture
    void ScreenSpaceSkyBrowser2::updateBrowserSize() {
        _dimensions = _texture->dimensions();
    }

    void ScreenSpaceSkyBrowser2::setScale(float scalingFactor) {
        _scale = _originalScale * scalingFactor;
    }

    properties::FloatProperty& ScreenSpaceSkyBrowser2::getOpacity() {
        return _opacity;
    }

    void ScreenSpaceSkyBrowser2::bindTexture() {
        _texture->bind();
    }
}
