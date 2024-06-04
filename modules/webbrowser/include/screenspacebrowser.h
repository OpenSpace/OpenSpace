/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_MODULE_WEBBROWSER___SCREEN_SPACE_BROWSER___H__
#define __OPENSPACE_MODULE_WEBBROWSER___SCREEN_SPACE_BROWSER___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <modules/webbrowser/include/webrenderhandler.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/uvec2property.h>
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

namespace openspace {

class BrowserInstance;
class ScreenSpaceRenderHandler;
class WebKeyboardHandler;

class ScreenSpaceBrowser : public ScreenSpaceRenderable {
public:
    ScreenSpaceBrowser(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceBrowser() override = default;

    bool initializeGL() override;
    bool deinitializeGL() override;

    void render(const RenderData& renderData) override;
    void update() override;
    bool isReady() const override;

    static documentation::Documentation Documentation();

protected:
    properties::UVec2Property _dimensions;
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

private:
    void bindTexture() override;

    properties::StringProperty _url;
    properties::TriggerProperty _reload;

    CefRefPtr<WebKeyboardHandler> _keyboardHandler;

    bool _isUrlDirty = false;
    bool _isDimensionsDirty = false;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___SCREEN_SPACE_BROWSER___H__
