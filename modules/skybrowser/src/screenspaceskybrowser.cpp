#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/skybrowsermodule.h>
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
        "Browser Dimensions Info",
        "Set the dimensions of the SkyTarget according to the SkyBrowser ratio "
    };
    constexpr const openspace::properties::Property::PropertyInfo ZoomInfo =
    {
        "Zoom",
        "Zoom Info",
        "tjobidabidobidabidopp plupp"
    };
    constexpr const openspace::properties::Property::PropertyInfo TargetIDInfo =
    {
        "TargetID",
        "Target Info",
        "tjobidabidobidabidopp plupp"
    };
    constexpr const openspace::properties::Property::PropertyInfo BorderColorInfo =
    {
        "BorderColor",
        "Border color Info",
        "tjobidabidobidabidopp plupp"
    };


    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(BrowserDimensionInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(ZoomInfo.description)]]
        std::optional<float> zoom;

        // [[codegen::verbatim(TargetIDInfo.description)]]
        std::optional<std::string> targetID;

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::ivec3> borderColor;
    };

#include "screenspaceskybrowser_codegen.cpp"
} // namespace

namespace openspace {

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary) 
        : ScreenSpaceBrowser(dictionary)
        , _browserDimensions(BrowserDimensionInfo, _dimensions, glm::ivec2(0), glm::ivec2(300))
        , _vfieldOfView(ZoomInfo, 10.f, 0.1f, 70.f)
        , _borderColor(BorderColorInfo, glm::ivec3(rand() % 256, rand() % 256, rand() % 256))
        , _skyTargetID(TargetIDInfo)
        , _camIsSyncedWWT(true)
        , _skyTarget(nullptr)
    {
        // Ensure the color of the border is bright enough.
        // Make sure the RGB color at least is 50% brightness
        // By making each channel 50% bright in general
        // 222 = sqrt(3*(0.5*256)^2)
        while (glm::length(_borderColor.value()) < 222) {
            _borderColor = glm::ivec3(rand() % 256, rand() % 256, rand() % 256);
        }
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _browserDimensions = p.browserDimensions.value_or(_browserDimensions);
        _browserDimensions.onChange([&]() { 
            if (!_skyTarget) {
                setConnectedTarget();
            }
            else {
                glm::vec2 dim = getBrowserPixelDimensions();
                _skyTarget->setDimensions(dim);
            }
            });
        addProperty(_browserDimensions);

        _vfieldOfView = p.zoom.value_or(_vfieldOfView);
        addProperty(_vfieldOfView);

        _skyTargetID = p.targetID.value_or(_skyTargetID);
        addProperty(_skyTargetID);

        _skyTargetID.onChange([&]() {
            setConnectedTarget();
            });
        
        _vfieldOfView.onChange([&]() {
            if (_skyTarget) {
                _skyTarget->updateFOV(_vfieldOfView);
            }
            });
        
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyBrowser";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        _cartesianPosition.setValue(glm::dvec3(_cartesianPosition.value().x, _cartesianPosition.value().y, skybrowser::SCREENSPACE_Z));

        // Always make sure that the target and browser are visible together
        _enabled.onChange([&]() {
            if (_skyTarget) {
                _skyTarget->property("Enabled")->set(_enabled.value());
            }
        });
    }

    bool ScreenSpaceSkyBrowser::initializeGL() {
        global::moduleEngine->module<SkyBrowserModule>()->addRenderable(this);
        setConnectedTarget();
        if (_skyTarget) {    
            _skyTarget->setDimensions(getBrowserPixelDimensions());
        }

        WWTfollowCamera();

        return ScreenSpaceBrowser::initializeGL();
    }

    bool ScreenSpaceSkyBrowser::deinitializeGL() {
        // Set flag to false so the thread can exit
        _camIsSyncedWWT = false;
        if (_threadWWTMessages.joinable()) {
            _threadWWTMessages.join();
            LINFO("Joined thread");
        }
        return ScreenSpaceBrowser::deinitializeGL();
    }   

    bool ScreenSpaceSkyBrowser::setConnectedTarget() {
        setBorderColor(_borderColor.value());
        _skyTarget = dynamic_cast<ScreenSpaceSkyTarget*>(global::renderEngine->screenSpaceRenderable(_skyTargetID.value()));
        if (_skyTarget) {
            _skyTarget->setBorderColor(_borderColor.value());
            _skyTarget->updateFOV(_vfieldOfView.value());
            _skyTarget->setDimensions(getBrowserPixelDimensions());
        }
        return _skyTarget != nullptr;
    }

    ScreenSpaceSkyTarget* ScreenSpaceSkyBrowser::getSkyTarget() {
        return _skyTarget;
    }

    float ScreenSpaceSkyBrowser::fieldOfView() const {
        return _vfieldOfView;
    }

    void ScreenSpaceSkyBrowser::setVerticalFieldOfView(float fov) {
        _vfieldOfView = fov;
    }

    void ScreenSpaceSkyBrowser::scrollZoom(float scroll) {
        // Cap how often the zoom is allowed to update
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
        if (now - _lastUpdateTime > TimeUpdateInterval) {
            // Make scroll more sensitive the smaller the FOV
            float x = _vfieldOfView;
            float zoomFactor = atan(x / 50.0) + exp(x / 40) - 0.999999;
            float zoom = scroll > 0.0 ? -zoomFactor : zoomFactor;
            _vfieldOfView = std::clamp(_vfieldOfView + zoom, 0.001f, 70.0f);
            _lastUpdateTime = std::chrono::system_clock::now();
        }
    }

    void ScreenSpaceSkyBrowser::executeJavascript(std::string script) const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetMainFrame()) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }      
    }

    glm::ivec3 ScreenSpaceSkyBrowser::getColor() {
        return _borderColor.value();
    }

    void ScreenSpaceSkyBrowser::setBorderColor(glm::ivec3 col)  {
        std::string stringColor = std::to_string(col.x) + "," + std::to_string(col.y) + "," + std::to_string(col.z);
        std::string script = "document.body.style.backgroundColor = 'rgb(" + stringColor + ")';";
        executeJavascript(script);
    }

    bool ScreenSpaceSkyBrowser::sendMessageToWWT(const ghoul::Dictionary& msg) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        executeJavascript(script);
        return true;
    }


    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const double fov, const bool moveInstantly) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;

        glm::dvec3 camUpJ2000 = skybrowser::galacticCartesianToJ2000Cartesian(global::navigationHandler->camera()->lookUpVectorWorldSpace());
        glm::dvec3 camForwardJ2000 = skybrowser::galacticCartesianToJ2000Cartesian(global::navigationHandler->camera()->viewDirectionWorldSpace());     
        double angle = glm::degrees(atan2(glm::dot(glm::cross(camUpJ2000, skybrowser::NORTH_POLE), camForwardJ2000), glm::dot(skybrowser::NORTH_POLE, camUpJ2000)));

        msg.setValue("event", "center_on_coordinates"s);
        msg.setValue("ra", celestCoords.x);
        msg.setValue("dec", celestCoords.y);
        msg.setValue("fov", fov);
        msg.setValue("roll", angle);
        msg.setValue("instant", moveInstantly);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForLoadingWWTImgColl(const std::string& url) const {
        // https://docs.worldwidetelescope.org/data-guide/1/data-file-formats/collections/sample-blank-collection.wtml
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "load_image_collection"s);
        msg.setValue("url", url);
        msg.setValue("loadChildFolders", true);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForSettingForegroundWWT(const std::string& name) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "set_foreground_by_name"s);
        msg.setValue("name", name);

        return msg;
    }  
    
    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForAddingImageLayerWWT(const std::string& url) {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_create"s);
        msg.setValue("id", std::to_string(imageId));
        msg.setValue("name", url);
        // Update ID to ensure that all ID's are unique
        imageId++;

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForRemovingImageLayerWWT(int id) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_remove"s);
        msg.setValue("id", std::to_string(id));

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForSettingOpacityLayerWWT(int id, double opacity) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "image_layer_set"s);
        msg.setValue("id", std::to_string(id));
        msg.setValue("setting", "opacity"s);
        msg.setValue("value", opacity);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForSettingForegroundOpacityWWT(double val) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "set_foreground_opacity"s);
        msg.setValue("value", val);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForPausingWWTTime() const {

        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "pause_time"s);

        return msg;
    }

    void ScreenSpaceSkyBrowser::sendMouseEvent(CefStructBase<CefMouseEventTraits> event, int x, int y)  const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        LINFO(std::to_string(_objectSize.x) + "  " + std::to_string(_objectSize.y));
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetHost()) {

            //_browserInstance->getBrowser()->GetHost()->SendMouseWheelEvent(event, x, y);
            //LINFOC(_loggerCat, "Sending scroll");

        }
    }

    void ScreenSpaceSkyBrowser::WWTfollowCamera() {

        // Start a thread to enable user interaction while sending the calls to WWT
        _threadWWTMessages = std::thread([&] {
            while (_camIsSyncedWWT) {
                if (_skyTarget) {
                    ghoul::Dictionary message = createMessageForMovingWWTCamera(_skyTarget->getTargetDirectionCelestial(), _vfieldOfView);
                    sendMessageToWWT(message);
                }
                
                // Sleep so we don't bombard WWT with too many messages
                std::this_thread::sleep_for(std::chrono::milliseconds(50));
            }
        });

    }
        
    /*
    void ScreenSpaceSkyBrowser::translate(glm::vec2 translation) {
        glm::vec3 position = _cartesianPosition;     
        _cartesianPosition = glm::translate(glm::mat4(1.f), glm::vec3(translation, 0.0f)) * glm::vec4(position, 1.0f);
    }*/

    glm::vec2 ScreenSpaceSkyBrowser::coordIsOnResizeArea(glm::vec2 coord) {
        glm::vec2 resizePosition = glm::vec2{ 0 };
        // Make sure coord is on browser
        if (!coordIsInsideCornersScreenSpace(coord)) return resizePosition;

        float resizeButtonSize = 0.1f;
       
        bool isOnTop = coord.y > getUpperRightCornerScreenSpace().y - (getScreenSpaceDimensions().y * resizeButtonSize);
        bool isOnBottom = coord.y < getLowerLeftCornerScreenSpace().y + (getScreenSpaceDimensions().y * resizeButtonSize);
        bool isOnRight = coord.x > getUpperRightCornerScreenSpace().x - (getScreenSpaceDimensions().x * resizeButtonSize);
        bool isOnLeft = coord.x < getLowerLeftCornerScreenSpace().x + (getScreenSpaceDimensions().x * resizeButtonSize);

        resizePosition.x = isOnRight ? 1.f : isOnLeft ? -1.f : 0.f;
        resizePosition.y = isOnTop ? 1.f : isOnBottom ? -1.f : 0.f;

        return  resizePosition;
    }
    // Scales the ScreenSpaceBrowser to a new ratio
    void ScreenSpaceSkyBrowser::scale(glm::vec2 scalingFactor) {       

        // Scale on the y axis, this is to ensure that _scale = 1 is
        // equal to the height of the window
        scale(abs(scalingFactor.y)); 
        // Resize the dimensions of the texture on the x axis
        glm::vec2 newSize = abs(scalingFactor) * _startDimensionsSize;
        _texture->setDimensions(glm::ivec3(newSize, 1));
        _objectSize = _texture->dimensions();
        _browserDimensions = newSize;
    }

    glm::mat4 ScreenSpaceSkyBrowser::scaleMatrix() {
        // To ensure the plane has the right ratio
        // The _scale tells us how much of the windows height the
        // browser covers: eg a browser that covers 0.25 of the 
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
        _startDimensionsSize = glm::vec2(_dimensions.value().x, _dimensions.value().y);
        _startScale = _scale.value();
    }

    // Updates the browser size to match the size of the texture
    void ScreenSpaceSkyBrowser::updateBrowserSize() {
        _dimensions = _texture->dimensions();
    }
    void ScreenSpaceSkyBrowser::scale(float scalingFactor) {
        _scale = _startScale * scalingFactor;
    }

    glm::vec2 ScreenSpaceSkyBrowser::getBrowserPixelDimensions() {
        return _browserDimensions.value();
    }
}
