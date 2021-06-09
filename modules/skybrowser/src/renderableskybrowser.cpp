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
        , _fov(70.f)
        , _connectToWwt(true)
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
        if (_browserInstance && _browserInstance->getBrowser() && _browserInstance->getBrowser()->GetMainFrame()) {
            CefRefPtr<CefFrame> frame = _browserInstance->getBrowser()->GetMainFrame();
            frame->ExecuteJavaScript(script, frame->GetURL(), 0);
        }
    }

    bool RenderableSkyBrowser::sendMessageToWWT(const ghoul::Dictionary& msg) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        executeJavascript(script);
        return true;
    }

    void RenderableSkyBrowser::displayImage(ImageData& image, int i) {
        sendMessageToWWT(wwtmessage::moveCamera(image.celestCoords, image.fov, 0.0));
        _fov = image.fov;
        // Add to selected images if there are no duplicates
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        if (it == std::end(_selectedImages)) {
            // Push newly selected image to front
            _selectedImages.push_front(i);
            // Create image layer and center WWT app on the image
            sendMessageToWWT(wwtmessage::createImageLayer(std::to_string(i), image.imageUrl));
            LINFO("Image has been loaded to " + identifier());
        }
    }

    void RenderableSkyBrowser::removeSelectedImage(ImageData& image, int i) {
        // Remove from selected list
        auto it = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        if (it != std::end(_selectedImages)) {
            _selectedImages.erase(it);
            sendMessageToWWT(wwtmessage::removeImageLayer(std::to_string(i)));
        }
    }

    void RenderableSkyBrowser::setIdInBrowser(std::string id) {
        // Send ID to it's browser
        executeJavascript("setId('" + id + "')");
    }

    float RenderableSkyBrowser::fieldOfView() const {
        return _fov;
    }

    void RenderableSkyBrowser::connectToWwt() {

        // Start a thread to enable user interaction while sending the calls to WWT
        _threadWwtMessages = std::thread([&] {
            while (_connectToWwt) {

                glm::dvec2 aim { 0.0 };
                // Send a message just to establish contact
                ghoul::Dictionary message = wwtmessage::moveCamera(aim, _fov, 0.0);
                sendMessageToWWT(message);

                // Sleep so we don't bombard WWT with too many messages
                std::this_thread::sleep_for(std::chrono::milliseconds(500));
            }
        });
    }

    void RenderableSkyBrowser::stopConnectingToWwt() {
        _connectToWwt = false;

        if (_threadWwtMessages.joinable()) {
            _threadWwtMessages.join();
        }
    }

    std::deque<int>& RenderableSkyBrowser::selectedImages() {
        return _selectedImages;
    }

    void RenderableSkyBrowser::setImageLayerOrder(int i, int order, int version) {
        // Remove from selected list
        auto current = std::find(std::begin(_selectedImages), std::end(_selectedImages), i);
        auto target = std::begin(_selectedImages) + order;

        // Make sure the image was found in the list
        if (current != std::end(_selectedImages) && target != std::end(_selectedImages)) {
            // Swap the two images
            std::iter_swap(current, target);
        }

        int reverseOrder = _selectedImages.size() - order - 1;
        ghoul::Dictionary message = wwtmessage::setLayerOrder(std::to_string(i),
            reverseOrder, version);
        sendMessageToWWT(message);
    }

    

} // namespace
