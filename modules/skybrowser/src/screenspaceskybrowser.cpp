#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
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
#include <glm/gtx/string_cast.hpp>
#include <thread>
#include <chrono> // Milliseconds

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


    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(BrowserDimensionInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(ZoomInfo.description)]]
        std::optional<float> zoom;

        // [[codegen::verbatim(TargetIDInfo.description)]]
        std::optional<std::string> targetID;
    };

#include "screenspaceskybrowser_codegen.cpp"
} // namespace

namespace openspace {

    ScreenSpaceSkyBrowser::ScreenSpaceSkyBrowser(const ghoul::Dictionary& dictionary) 
        : ScreenSpaceBrowser(dictionary)
        , _browserDimensions(BrowserDimensionInfo, _dimensions, glm::ivec2(0.f), glm::ivec2(300.f))
        , _fieldOfView(ZoomInfo, 50.f, 0.1f, 70.f)
        , _skyTargetID(TargetIDInfo)
        , _camIsSyncedWWT(true)
        , _skyTarget(nullptr)
        , _borderColor(220, 220, 220)
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _browserDimensions = p.browserDimensions.value_or(_browserDimensions);
        _browserDimensions.onChange([&]() { 
            if (!_skyTarget) {
                setConnectedTarget();
            }
            else {
                _skyTarget->setDimensions(getScreenSpaceBrowserDimension());
            }
            });
        addProperty(_browserDimensions);

        _fieldOfView = p.zoom.value_or(_fieldOfView);
        addProperty(_fieldOfView);

        _skyTargetID = p.targetID.value_or(_skyTargetID);
        addProperty(_skyTargetID);

        _skyTargetID.onChange([&]() {
            setConnectedTarget();
            });
        
        _fieldOfView.onChange([&]() {
            if (_skyTarget) {
                _skyTarget->updateFOV(_fieldOfView);
                float scaleWhenFovIs10 = static_cast<float>(10.f / global::windowDelegate->getHorizFieldOfView());
                _skyTarget->setScale(std::max(static_cast<float>(_fieldOfView / global::windowDelegate->getHorizFieldOfView()), scaleWhenFovIs10));
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
        // The projection plane seems to be located at z = -2.1 so at that place the ScreenSpaceRenderables behaves like
        // they are in screen space
        _cartesianPosition.setValue(glm::vec3(_cartesianPosition.value().x, _cartesianPosition.value().y, -2.1f));

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
            _skyTarget->setDimensions(getScreenSpaceBrowserDimension());
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
        _skyTarget = dynamic_cast<ScreenSpaceSkyTarget*>(global::renderEngine->screenSpaceRenderable(_skyTargetID.value()));
        return _skyTarget != nullptr;
    }

    float ScreenSpaceSkyBrowser::fieldOfView() const {
        return _fieldOfView;
    }

    void ScreenSpaceSkyBrowser::scrollZoom(float scroll) {
        float zoom = scroll > 0.0 ? -log(_fieldOfView + 1.1f) : log(_fieldOfView + 1.1f);
        _fieldOfView = std::clamp(_fieldOfView + zoom, 0.001f, 70.0f);
    }

    void ScreenSpaceSkyBrowser::executeJavascript(std::string& script) const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetMainFrame()) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }      
    }

    glm::ivec3 ScreenSpaceSkyBrowser::getColor() {
        return _borderColor;
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


    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov, const bool moveInstantly) const {
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "center_on_coordinates"s);
        msg.setValue("ra", static_cast<double>(celestCoords[0]));
        msg.setValue("dec", static_cast<double>(celestCoords[1]));
        msg.setValue("fov", static_cast<double>(fov));
        msg.setValue("instant", moveInstantly);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForLoadingWWTImgColl(const std::string& url) const {
        // https://docs.worldwidetelescope.org/data-guide/1/data-file-formats/collections/sample-blank-collection.wtml
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "load_image_collection"s);
        msg.setValue("url", url);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForSettingForegroundWWT(const std::string& name) const {
        // https://docs.worldwidetelescope.org/data-guide/1/data-file-formats/collections/sample-blank-collection.wtml
        using namespace std::string_literals;
        ghoul::Dictionary msg;
        msg.setValue("event", "set_foreground_by_name"s);
        msg.setValue("name", name);

        return msg;
    }

    ghoul::Dictionary ScreenSpaceSkyBrowser::createMessageForSettingForegroundOpacityWWT(double val) const {
        // https://docs.worldwidetelescope.org/data-guide/1/data-file-formats/collections/sample-blank-collection.wtml
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

                // Get camera view direction and orthogonal coordinate system of camera view direction
                glm::vec3 viewDirection = global::navigationHandler->camera()->viewDirectionWorldSpace();
                glm::vec3 upDirection = global::navigationHandler->camera()->lookUpVectorWorldSpace();
                glm::vec3 sideDirection = glm::cross(upDirection, viewDirection);

                glm::vec2 angleOffset = _skyTarget ? _skyTarget->getAnglePosition() : glm::vec2(0);
                // Change view if target is moved
                glm::vec3 targetDirection = glm::rotate(viewDirection, angleOffset.x, upDirection);
                targetDirection = glm::rotate(targetDirection, angleOffset.y, sideDirection);

                // Convert to celestial coordinates
                glm::dvec2 celestCoords = convertGalacticToCelestial(targetDirection);
                ghoul::Dictionary message = createMessageForMovingWWTCamera(celestCoords, _fieldOfView);

                // Sleep so we don't bombard WWT with too many messages
                std::this_thread::sleep_for(std::chrono::milliseconds(50));
                sendMessageToWWT(message);
                
            }
        });

    }

    glm::dvec2 ScreenSpaceSkyBrowser::convertGalacticToCelestial(glm::dvec3 rGal) const {          
        // Used the math from this website: https://gea.esac.esa.int/archive/documentation/GD -->
        // R2/Data_processing/chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
        const glm::dmat3 conversionMatrix = glm::dmat3({
          -0.0548755604162154,  0.4941094278755837, -0.8676661490190047, // col 0
          -0.8734370902348850, -0.4448296299600112, -0.1980763734312015, // col 1
          -0.4838350155487132,  0.7469822444972189,  0.4559837761750669  // col 2
            });

        glm::dvec3 rICRS = glm::transpose(conversionMatrix) * rGal;
        float ra = atan2(rICRS[1], rICRS[0]);
        float dec = atan2(rICRS[2], glm::sqrt((rICRS[0] * rICRS[0]) + (rICRS[1] * rICRS[1])));

        ra = ra > 0 ? ra : ra + (2 * glm::pi<float>());

        return glm::dvec2(glm::degrees(ra), glm::degrees(dec));
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

    glm::vec2 ScreenSpaceSkyBrowser::getScreenSpaceBrowserDimension() {
        return _browserDimensions.value();
    }
}
