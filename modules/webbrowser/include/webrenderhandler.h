/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H
#define __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H

#include <vector>
#include <ghoul/glm.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#elif defined(__clang__)
//#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-variable"
#endif // _MSC_VER

#include <include/cef_render_handler.h>
#include <include/cef_display_handler.h>

#ifdef _MSC_VER
#pragma warning (pop)
#elif defined(__clang__)
//#pragma clang diagnostic pop
#endif // _MSC_VER

#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class WebRenderHandler : public CefRenderHandler {
public:
    using Pixel = glm::tvec4<char>;

    WebRenderHandler();

    virtual void draw(void) = 0;
    virtual void render() = 0;

    void reshape(int, int);

    void GetViewRect(CefRefPtr<CefBrowser> browser, CefRect &rect) override;

    // Regular OnPaint method. Uses CPU allocation
    void OnPaint(CefRefPtr<CefBrowser> browser, PaintElementType type,
        const RectList &dirtyRects, const void* buffer, int width, int height) override;

#ifdef WIN32
    // Used when the "shared_texture" flag is set to true for CEF. Uses a shared texture
    // from CEF that is allocated on another part of the GPU. Skip CPU allocationn for
    // better performance. Needs OpenGl 4.5 or higher.
    void OnAcceleratedPaint(CefRefPtr<CefBrowser> browser, PaintElementType type,
        const RectList& dirtyRects, const CefAcceleratedPaintInfo& info) override;
#endif // WIN32

    // Determines if the alpha value is > 0 at the specified pixel coordinates. Used in
    // the GUI to determine if the click is consumed
    bool hasContent(int x, int y);

    bool isTextureReady() const;
    void updateTexture();
    void bindTexture();

protected:
    GLuint _texture = 0;
    const bool _acceleratedRendering;

private:
    glm::ivec2 _windowSize = glm::ivec2(0);
    glm::ivec2 _browserBufferSize = glm::ivec2(0);

    /**
     * RGBA buffer from browser
     */
    std::vector<Pixel> _browserBuffer;
    bool _needsRepaint = true;
    bool _textureSizeIsDirty = true;
    bool _textureIsDirty = true;
    glm::ivec2 _lowerDirtyRectBound = glm::ivec2(0);
    glm::ivec2 _upperDirtyRectBound = glm::ivec2(0);

    IMPLEMENT_REFCOUNTING(WebRenderHandler);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H
