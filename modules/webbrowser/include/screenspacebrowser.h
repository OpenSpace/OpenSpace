/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/properties/vector/vec2property.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_client.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace ghoul::opengl { class Texture; }

namespace openspace {

class BrowserInstance;
class ScreenSpaceRenderHandler;

class ScreenSpaceBrowser : public ScreenSpaceRenderable {
public:
    ScreenSpaceBrowser(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;
    void render() override;
    void update() override;
    bool isReady() const override;

private:
    class ScreenSpaceRenderHandler : public WebRenderHandler {
    public:
        void draw();
        void render();

        void setTexture(GLuint t);
    };

    properties::StringProperty _url;
    properties::Vec2Property _dimensions;
    CefRefPtr<ScreenSpaceRenderHandler> _renderHandler;
    std::shared_ptr<BrowserInstance> _browserInstance;
    std::unique_ptr<ghoul::opengl::Texture> _texture;

    bool _isUrlDirty = false;
    bool _isDimensionsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___SCREEN_SPACE_BROWSER___H__
