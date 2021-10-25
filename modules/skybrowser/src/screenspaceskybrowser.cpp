#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/camera.h>
#include <openspace/scene/scene.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <glm/gtx/matrix_decompose.hpp>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/rotate_vector.hpp >
#include <thread>
#include <chrono> // Milliseconds
#include <optional>
#include <openspace/util/coordinateconversion.h> // to adjust camera angle

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyBrowser";

    constexpr const openspace::properties::Property::PropertyInfo BrowserDimensionInfo =
    {
        "BrowserDimensions",
        "Browser Dimensions",
        "The pixel dimensions of the sky browser."
    };
    constexpr const openspace::properties::Property::PropertyInfo VerticalFovInfo =
    {
        "VerticalFieldOfView",
        "Vertical Field Of View",
        "The vertical field of view in degrees."
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
        "The color of the border of the sky browser as well as the sky target."
    };


    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(BrowserDimensionInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<float> verticalFov;

        // [[codegen::verbatim(TargetIdInfo.description)]]
        std::optional<std::string> targetId;

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::vec3> borderColor;
    };

#include "screenspaceskybrowser_codegen.cpp"
} // namespace

namespace openspace {

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary) 
        : ScreenSpaceBrowser(dictionary)
        , _browserDimensions(BrowserDimensionInfo, _dimensions, glm::ivec2(0), glm::ivec2(300))
        , _verticalFov(VerticalFovInfo, 10.f, 0.1f, 70.f)
        , _borderColor(BorderColorInfo, glm::vec3(rand() % 256, rand() % 256, rand() % 256))
        , _skyTargetId(TargetIdInfo)
    {
        // Make the color property display a color picker in the GUI
        _borderColor.setViewOption("Color", true);

        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _browserDimensions = p.browserDimensions.value_or(_browserDimensions);
        _verticalFov = p.verticalFov.value_or(_verticalFov);
        _borderColor = p.borderColor.value_or(_borderColor);
        _skyTargetId = p.targetId.value_or(_skyTargetId);

        addProperty(_browserDimensions);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_skyTargetId);

        _browserDimensions.onChange([&]() {
            if (_skyTarget) {
                glm::vec2 dim = browserPixelDimensions();
                _skyTarget->setDimensions(dim);
            }
            });
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
            identifier = "ScreenSpaceSkyBrowser";
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
        while (glm::length(_borderColor.value()) < 222) {
            _borderColor = glm::vec3(rand() % 256, rand() % 256, rand() % 256);
        }
    }

    ScreenSpaceSkyBrowser::~ScreenSpaceSkyBrowser() {
        // Set flag to false so the thread can exit
        _syncViewWithWwt = false;
        if (_threadWwtMessages.joinable()) {
            _threadWwtMessages.join();
            LINFO("Joined thread");
        }
    }

    bool ScreenSpaceSkyBrowser::initializeGL() {
        return ScreenSpaceBrowser::initializeGL();
    }

    void ScreenSpaceSkyBrowser::sendIdToBrowser() {
        // Send ID to it's browser
        executeJavascript("setId('" + identifier() + "')");
    }

    void ScreenSpaceSkyBrowser::initializeBrowser() {
        // If the camera is already synced, the browser is already initialized
        if (!_syncViewWithWwt) {
            _syncViewWithWwt = true;
            // Set border color
            setWebpageBorderColor(_borderColor.value());
            // Connect to target if they haven't been connected
            if (!_skyTarget) {
                connectToSkyTarget();
            }
            // Track target
            syncWwtView();
        }
    }

    bool ScreenSpaceSkyBrowser::deinitializeGL() {
        // Set flag to false so the thread can exit
        _syncViewWithWwt = false;
        if (_threadWwtMessages.joinable()) {
            _threadWwtMessages.join();
            LINFO("Joined thread");
        }
        return ScreenSpaceBrowser::deinitializeGL();
    }   

    bool ScreenSpaceSkyBrowser::connectToSkyTarget() {
        _skyTarget = dynamic_cast<ScreenSpaceSkyTarget*>(
            global::renderEngine->screenSpaceRenderable(_skyTargetId.value()));
        return _skyTarget;
    }

    ScreenSpaceSkyTarget* ScreenSpaceSkyBrowser::getSkyTarget() {
        return _skyTarget;
    }

    bool ScreenSpaceSkyBrowser::hasLoadedImages() const {
        return _hasLoadedImages;
    }

    void ScreenSpaceSkyBrowser::setHasLoadedImages(bool isLoaded) {
        _hasLoadedImages = isLoaded;
    }

    void ScreenSpaceSkyBrowser::setVerticalFov(float vfov) {
        _verticalFov = vfov;
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

    void ScreenSpaceSkyBrowser::executeJavascript(std::string script) {
        // Make sure that the browser has a main frame
        bool browserExists = _browserInstance && _browserInstance->getBrowser();
        bool frameIsLoaded = browserExists && 
            _browserInstance->getBrowser()->GetMainFrame();

        if (frameIsLoaded) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }      
    }

    glm::ivec3 ScreenSpaceSkyBrowser::borderColor() const {
        return _borderColor.value();
    }

    float ScreenSpaceSkyBrowser::verticalFov() const {
        return _verticalFov.value();
    }

    void ScreenSpaceSkyBrowser::setWebpageBorderColor(glm::ivec3 color)  {
        std::string stringColor = std::to_string(color.x) + "," 
            + std::to_string(color.y) + "," + std::to_string(color.z);
        std::string script = "document.body.style.backgroundColor = 'rgb("
            + stringColor + ")';";
        executeJavascript(script);
    }

    void ScreenSpaceSkyBrowser::sendMessageToWwt(const ghoul::Dictionary& msg) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        executeJavascript(script);
    }

    void ScreenSpaceSkyBrowser::syncWwtView() {

        // Start a thread to enable user interaction while sending the calls to WWT
        _threadWwtMessages = std::thread([&] {
            while (_syncViewWithWwt) {
                if (_skyTarget) {                    
                    // Message WorldWide Telescope current view
                    glm::dvec3 cartesian = _skyTarget->targetDirectionEquatorial();

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
        
    //
    //void ScreenSpaceSkyBrowser::translate(glm::vec2 translation) {
    //    glm::vec3 position = _cartesianPosition;     
    //    _cartesianPosition =glm::translate(glm::mat4(1.f), glm::vec3(translation, 0.0f)) * glm::vec4(position, 1.0f);
    //}

    glm::vec2 ScreenSpaceSkyBrowser::isOnResizeArea(glm::vec2 screenSpaceCoord) {
        glm::vec2 resizePosition = glm::vec2{ 0 };
        // Make sure coordinate is on browser
        if (!coordIsInsideCornersScreenSpace(screenSpaceCoord)) return resizePosition;

        float resizeAreaY = screenSpaceDimensions().y * _resizeAreaPercentage;
        float resizeAreaX = screenSpaceDimensions().x * _resizeAreaPercentage;

        bool isOnTop = screenSpaceCoord.y > upperRightCornerScreenSpace().y - resizeAreaY;
        bool isOnBottom = screenSpaceCoord.y < lowerLeftCornerScreenSpace().y + resizeAreaY;
        bool isOnRight = screenSpaceCoord.x > upperRightCornerScreenSpace().x - resizeAreaX;
        bool isOnLeft = screenSpaceCoord.x < lowerLeftCornerScreenSpace().x + resizeAreaX;

        resizePosition.x = isOnRight ? 1.f : isOnLeft ? -1.f : 0.f;
        resizePosition.y = isOnTop ? 1.f : isOnBottom ? -1.f : 0.f;

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
        _browserDimensions = newSize;
    }

    glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale tells us how much of the windows height the
        // browser covers: e.g. a browser that covers 0.25 of the 
        // height of the window will have scale = 0.25

        float textureRatio =
            static_cast<float>(_texture->dimensions().x) / static_cast<float>(_texture->dimensions().y);

        glm::mat4 scale = glm::scale(
            glm::mat4(1.f),
            glm::vec3(textureRatio * _scale, _scale, 1.f)
        );
        return scale;
    }

    void ScreenSpaceSkyBrowser::saveResizeStartSize() {
        _originalDimensions = _dimensions.value();
        _originalScale = _scale.value();
    }

    // Updates the browser size to match the size of the texture
    void ScreenSpaceSkyBrowser::updateBrowserSize() {
        _dimensions = _texture->dimensions();
    }
    void ScreenSpaceSkyBrowser::setScale(float scalingFactor) {
        _scale = _originalScale * scalingFactor;
    }

    glm::vec2 ScreenSpaceSkyBrowser::browserPixelDimensions() const {
        return _browserDimensions.value();
    }

    properties::FloatProperty& ScreenSpaceSkyBrowser::getOpacity() {
        return _opacity;
    }

    std::deque<int>& ScreenSpaceSkyBrowser::getSelectedImages() {
        return _selectedImages;
    }

    void ScreenSpaceSkyBrowser::addSelectedImage(ImageData& image, int i) {
        // Ensure there are no duplicates
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        bool found = it != std::end(_selectedImages);
        if (!found) {
            // Push newly selected image to front
            _selectedImages.push_front(i);
            // Index of image is used as layer ID as it is unique in the image data set
            sendMessageToWwt(wwtmessage::addImage(std::to_string(i), image.imageUrl));
            sendMessageToWwt(wwtmessage::setImageOpacity(std::to_string(i), 1.0));
        }
    }

    void ScreenSpaceSkyBrowser::removeSelectedImage(ImageData& image, int i) {
        // Remove from selected list
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        bool found = it != std::end(_selectedImages);
        if (found) {
            _selectedImages.erase(it);
            sendMessageToWwt(wwtmessage::removeImage(std::to_string(i)));
        }
    }

    void ScreenSpaceSkyBrowser::setImageOrder(int i, int order, int version) {

        // Find the selected image
        auto selected = std::find(
            std::begin(_selectedImages), 
            std::end(_selectedImages), 
            i
        );

        // Find the target for the swap
        auto target = std::begin(_selectedImages) + order;

        // Make sure the selected and the target placement was found in the list
        bool foundSelected = selected != std::end(_selectedImages);
        bool foundTarget = target != std::end(_selectedImages);

        if (foundSelected && foundTarget) {
            // Swap the two images
            std::iter_swap(selected, target);
        }

        // The images in the selected list are displayed in the reverse order from how the 
        // WorldWide Telescope application sees them 
        int reverseOrder = _selectedImages.size() - order - 1;
        ghoul::Dictionary message = wwtmessage::setLayerOrder(
            std::to_string(i), 
            reverseOrder, 
            version
        );
        sendMessageToWwt(message);
    }
}
