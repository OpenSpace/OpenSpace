#ifndef __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

#include <modules/base/rendering/renderableplane.h>
#include <openspace/documentation/documentation.h>
#include <modules/webbrowser/include/webrenderhandler.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/triggerproperty.h>
#include <deque>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#endif // __clang__

#include <include/cef_client.h>

#ifdef __clang__
#pragma clang diagnostic pop
#endif // __clang__

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace ghoul::opengl { class Texture; }

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class BrowserInstance;
    class RenderHandler;
    class WebKeyboardHandler;
    class ImageData;

    class RenderableSkyBrowser : public RenderablePlane
    {
    public:
        static constexpr const char* KeyIdentifier = "Identifier";

        RenderableSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~RenderableSkyBrowser() = default;

        // Inherited from RenderablePlane
        void initializeGL() override;
        void deinitializeGL() override;
        void update(const UpdateData& data) override;

        // Web page communication
        void executeJavascript(std::string script) const;
        void setIdInBrowser(std::string id);

        // WorldWide Telescope communication
        void displayImage(const ImageData& image, int i);
        void removeSelectedImage(const ImageData& image, int i);
        bool sendMessageToWwt(const ghoul::Dictionary& msg);
        void syncWwtView();
        void stopSyncingWwtView();

        // Place
        void placeAt3dPosition(const ImageData& image);

        // Getters
        float verticalFov() const;
        std::deque<int>& getSelectedImages();
        
        // Setters
        void setImageLayerOrder(int i, int order);

    private:
        // Properties
        properties::Vec2Property _dimensions;
        properties::StringProperty _url;
        properties::TriggerProperty _reload;

        class ScreenSpaceRenderHandler : public WebRenderHandler {
        public:
            void draw() override;
            void render() override;

            void setTexture(GLuint t);
        };

        void bindTexture() override;

        // Browser variables       
        std::unique_ptr<BrowserInstance> _browserInstance;
        std::unique_ptr<ghoul::opengl::Texture> _texture;
        CefRefPtr<ScreenSpaceRenderHandler> _renderHandler;
        CefRefPtr<WebKeyboardHandler> _keyboardHandler;

        // Flags
        bool _isUrlDirty = false;
        bool _isDimensionsDirty = false;
        bool _syncViewWithWwt;
        
        float _verticalFov;
        std::thread _threadWwtMessages;
        std::deque<int> _selectedImages;
    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

