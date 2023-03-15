/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

    virtual void draw(void) = 0;
    virtual void render() = 0;

    void reshape(int, int);

    void GetViewRect(CefRefPtr<CefBrowser> browser, CefRect &rect) override;
    void OnPaint(CefRefPtr<CefBrowser> browser, PaintElementType type,
        const RectList &dirtyRects, const void* buffer, int width, int height) override;
    bool hasContent(int x, int y);

    bool isTextureReady() const;
    void updateTexture();

protected:
    GLuint _texture = 0;

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
