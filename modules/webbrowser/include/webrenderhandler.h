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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H
#define __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER

#include <include/cef_render_handler.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class WebRenderHandler : public CefRenderHandler {
public:
    virtual void draw(void) = 0;
    virtual void render() = 0;

    void reshape(int, int);

    bool GetViewRect(CefRefPtr<CefBrowser> browser, CefRect &rect) override;
    void OnPaint(CefRefPtr<CefBrowser> browser, PaintElementType type,
        const RectList &dirtyRects, const void* buffer, int width, int height) override;
    bool hasContent(int x, int y);

protected:
    int _width = 0;
    int _height = 0;
    GLuint _texture;

    /**
     * Alpha mask showing whether or not a pixel is filled with content
     *
     * Depending on what config you're running (Debug/Release), use different types here.
     * This is to increase performance on Debug, since VS is performing lots of extra
     * checks on vector<bool>.
     */
#if !(defined(NDEBUG) || defined(DEBUG))
    std::vector<char> _alphaMask;
#else
    std::vector<bool> _alphaMask;
#endif

    IMPLEMENT_REFCOUNTING(WebRenderHandler);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER__WEB_RENDER_HANDLER_H
