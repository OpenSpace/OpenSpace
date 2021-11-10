#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/include/renderableskybrowser.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/webbrowser/webbrowsermodule.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <modules/webbrowser/include/browserinstance.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/globals.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <deque>

namespace {

    constexpr const char* _loggerCat = "RenderableSkyBrowser";

    const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser windows."
    };
    const openspace::properties::Property::PropertyInfo UrlInfo = {
        "Url",
        "URL",
        "The URL to load"
    };

    const openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reload the web browser"
    };


    struct [[codegen::Dictionary(RenderableSkyBrowser)]] Parameters {

        // [[codegen::verbatim(DimensionsInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(UrlInfo.description)]]
        std::optional<std::string> url;
    };

#include "renderableskybrowser_codegen.cpp"
} // namespace

namespace openspace {

    void RenderableSkyBrowser::ScreenSpaceRenderHandler::draw() {}

    void RenderableSkyBrowser::ScreenSpaceRenderHandler::render() {}

    void RenderableSkyBrowser::ScreenSpaceRenderHandler::setTexture(GLuint t) {
        _texture = t;
    }


    RenderableSkyBrowser::RenderableSkyBrowser(const ghoul::Dictionary& dictionary)
        : RenderablePlane(dictionary)
        , _url(UrlInfo)
        , _dimensions(DimensionsInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(3000.f))
        , _reload(ReloadInfo)
        , _verticalFov(70.f)
        , _isSyncedWithWwt(false)
    {
        // Handle target dimension property
        const Parameters p = codegen::bake<Parameters>(dictionary);
        _url = p.url.value_or(_url);

        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "RenderableSkyBrowser";
        }
        setIdentifier(identifier);

        // Ensure the texture is a square for now
        // Maybe change later
        glm::vec2 windowDimensions = global::windowDelegate->currentSubwindowSize();
        float maxDimension = std::max(windowDimensions.x, windowDimensions.y);
        _dimensions = { maxDimension, maxDimension };

        // Create browser and render handler
        _renderHandler = new ScreenSpaceRenderHandler();
        _keyboardHandler = new WebKeyboardHandler();
        _browserInstance = std::make_unique<BrowserInstance>(
            _renderHandler,
            _keyboardHandler
            );

        _url.onChange([this]() { _isUrlDirty = true; });
        _dimensions.onChange([this]() { _isDimensionsDirty = true; });
        _reload.onChange([this]() { _browserInstance->reloadBrowser(); });

        addProperty(_url);
        addProperty(_dimensions);
        addProperty(_reload);

        WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
        if (webBrowser) {
            webBrowser->addBrowser(_browserInstance.get());
        }
    }

    void RenderableSkyBrowser::initializeGL() {
        RenderablePlane::initializeGL();
        _texture = std::make_unique<ghoul::opengl::Texture>(
            glm::uvec3(_dimensions.value(), 1.0f)
            );
        _texture->setDimensions(glm::ivec3(_dimensions.value(), 1));
        
        _renderHandler->setTexture(*_texture);
        // The browser gets by default the size of the OpenSpace window, so it needs to 
        // be resized
        _browserInstance->initialize();
        _browserInstance->loadUrl(_url);
        _browserInstance->reshape(_dimensions.value());
    }

    void RenderableSkyBrowser::deinitializeGL() {
        RenderablePlane::deinitializeGL();
        _renderHandler->setTexture(0);
        _texture = nullptr;

        std::string urlString;
        _url.getStringValue(urlString);
        LDEBUG(fmt::format("Deinitializing RenderableSkyBrowser: {}", urlString));

        _browserInstance->close(true);

        WebBrowserModule* webBrowser = global::moduleEngine->module<WebBrowserModule>();
        if (webBrowser) {
            webBrowser->removeBrowser(_browserInstance.get());
            _browserInstance.reset();
        }
        else {
            LWARNING("Could not find WebBrowserModule");
        }

        RenderablePlane::deinitializeGL();
    }

    void RenderableSkyBrowser::update(const UpdateData& data) {
        RenderablePlane::update(data);
        _renderHandler->updateTexture();

        if (_isUrlDirty) {
            _browserInstance->loadUrl(_url);
            _isUrlDirty = false;
        }

        if (_isDimensionsDirty) {
            _browserInstance->reshape(_dimensions.value());
            _isDimensionsDirty = false;
        }
    }

    void RenderableSkyBrowser::bindTexture() {
        if (_texture) {
            _texture->bind();
        }
        else {
            glBindTexture(GL_TEXTURE_2D, 0);
        }
    }

    void RenderableSkyBrowser::executeJavascript(std::string script) const {
        //LINFOC(_loggerCat, "Executing javascript " + script);
        bool isBrowserReady = _browserInstance && _browserInstance->getBrowser();
        bool isMainFrameReady = _browserInstance->getBrowser()->GetMainFrame();
        if (isBrowserReady && isMainFrameReady) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }
    }

    bool RenderableSkyBrowser::sendMessageToWwt(const ghoul::Dictionary& msg) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        executeJavascript(script);
        return true;
    }

    void RenderableSkyBrowser::displayImage(const ImageData& image, const int i) {
        ghoul::Dictionary msg = wwtmessage::moveCamera(
            image.equatorialSpherical,
            image.fov,
            0.0
        );
        sendMessageToWwt(msg);
        _verticalFov = image.fov;
        // Add to selected images if there are no duplicates
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        if (it == std::end(_selectedImages)) {
            // Push newly selected image to front
            _selectedImages.push_front(i);
            // Create image layer and center WWT app on the image
            sendMessageToWwt(wwtmessage::addImage(std::to_string(i), image.imageUrl));
            LINFO("Image has been loaded to " + identifier());
        }
    }

    void RenderableSkyBrowser::removeSelectedImage(const ImageData& image, int i) {
        // Remove from selected list
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        if (it != std::end(_selectedImages)) {
            _selectedImages.erase(it);
            sendMessageToWwt(wwtmessage::removeImage(std::to_string(i)));
        }
    }

    void RenderableSkyBrowser::setIdInBrowser(std::string id) {
        // Send ID to it's browser
        executeJavascript("setId('" + id + "')");
    }

    float RenderableSkyBrowser::verticalFov() const {
        return _verticalFov;
    }

    void RenderableSkyBrowser::syncWwtView() {
        // If the camera is already synced, the browser is already initialized
        if (!_isSyncedWithWwt) {
            _isSyncedWithWwt = true;
            // Start a thread to enable user interaction while sending the calls to WWT
            _threadWwtMessages = std::thread([&] {
                while (_isSyncedWithWwt) {

                    glm::dvec2 aim{ 0.0 };
                    // Send a message just to establish contact
                    ghoul::Dictionary msg = wwtmessage::moveCamera(
                        aim, 
                        _verticalFov, 
                        0.0
                    );
                    sendMessageToWwt(msg);

                    // Sleep so we don't bombard WWT with too many messages
                    std::this_thread::sleep_for(std::chrono::milliseconds(500));
                }
                });
        }
      
    }

    void RenderableSkyBrowser::stopSyncingWwtView() {
        _isSyncedWithWwt = false;

        if (_threadWwtMessages.joinable()) {
            _threadWwtMessages.join();
        }
    }

	void RenderableSkyBrowser::placeAt3dPosition(const ImageData& image)
	{
        std::string renderableId = dynamic_cast<SceneGraphNode*>(
            this)->renderable()->identifier();
        // Uris for properties
        std::string sizeUri = "Scene." + _identifier + "." + renderableId + ".Size";
        std::string positionUri = "Scene." + _identifier + ".Translation.Position";
        std::string rotationUri = "Scene." + _identifier + ".Rotation.Rotation";
        std::string cameraAim = "NavigationHandler.OrbitalNavigator.Aim";
        glm::dvec3 position = image.position3d * distanceconstants::Parsec;
        // Calculate the size of the plane with trigonometry
        // Calculate in equatorial coordinate system since the FOV is from Earth
        //  /|
        // /_|    Adjacent is the horizontal line, opposite the vertical 
        // \ |    Calculate for half the triangle first, then multiply with 2
        //  \|
        glm::dvec3 j2000 = skybrowser::galacticToEquatorial(position);
        double adjacent = glm::length(j2000);
        double opposite = 2 * adjacent * glm::tan(glm::radians(image.fov * 0.5));

        // Calculate rotation to make the plane face the solar system barycenter
        glm::dvec3 normal = glm::normalize(-position);
        glm::dvec3 newRight = glm::normalize(
            glm::cross(glm::dvec3(0.0, 0.0, 1.0), normal)
        );
        glm::dvec3 newUp = glm::cross(normal, newRight);
        // Face the Solar System Barycenter
        glm::dmat3 rotation = glm::dmat3(1.0);
        rotation[0] = newRight;
        rotation[1] = newUp;
        rotation[2] = normal;

        std::string setValue = "openspace.setPropertyValueSingle('";

        openspace::global::scriptEngine->queueScript(
            setValue + sizeUri + "', " + std::to_string(opposite) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            setValue + positionUri + "', " + ghoul::to_string(position) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            setValue + rotationUri + "', " + ghoul::to_string(rotation) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
	}

	std::deque<int>& RenderableSkyBrowser::getSelectedImages() {
        return _selectedImages;
    }

    void RenderableSkyBrowser::setImageLayerOrder(int i, int order) {
        // Remove from selected list
        auto current = std::find(
            std::begin(_selectedImages), 
            std::end(_selectedImages), 
            i
        );
        auto target = std::begin(_selectedImages) + order;

        // Make sure the image was found in the list
        if (current != std::end(_selectedImages) && target != std::end(_selectedImages)) {
            // Swap the two images
            std::iter_swap(current, target);
        }

        int reverseOrder = _selectedImages.size() - order - 1;
        ghoul::Dictionary message = wwtmessage::setLayerOrder(std::to_string(i),
            reverseOrder);
        sendMessageToWwt(message);
    }

    

} // namespace
