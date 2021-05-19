#ifndef __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

#include <modules/base/rendering/renderableplane.h>
#include <openspace/documentation/documentation.h>
#include <modules/webbrowser/include/webrenderhandler.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/triggerproperty.h>

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

    class RenderableSkyBrowser : public RenderablePlane
    {
    public:
        static constexpr const char* KeyIdentifier = "Identifier";

        RenderableSkyBrowser(const ghoul::Dictionary& dictionary);
        virtual ~RenderableSkyBrowser() = default;

        void initializeGL() override;
        void deinitializeGL() override;

        void update(const UpdateData& data) override;

        void executeJavascript(std::string script) const;
        bool sendMessageToWWT(const ghoul::Dictionary& msg);


    protected:

        properties::Vec2Property _dimensions;
        std::unique_ptr<BrowserInstance> _browserInstance;
        std::unique_ptr<ghoul::opengl::Texture> _texture;

    private:
        class ScreenSpaceRenderHandler : public WebRenderHandler {
        public:
            void draw() override;
            void render() override;

            void setTexture(GLuint t);
        };

        CefRefPtr<ScreenSpaceRenderHandler> _renderHandler;

        void bindTexture() override;

        properties::StringProperty _url;

        properties::TriggerProperty _reload;

        CefRefPtr<WebKeyboardHandler> _keyboardHandler;

        glm::ivec2 _objectSize = glm::ivec2(0);
        bool _isUrlDirty = false;
        bool _isDimensionsDirty = false;

    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___RENDERABLESKYBROWSER___H__

