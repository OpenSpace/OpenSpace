/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_WEBGUI___GUI_RENDER_HANDLER___H__
#define __OPENSPACE_MODULE_WEBGUI___GUI_RENDER_HANDLER___H__

#include <memory>
#include <include/openspace/engine/openspaceengine.h>
#include <include/openspace/rendering/renderengine.h>
#include <include/openspace/engine/wrapper/windowwrapper.h>
#include <ghoul/opengl/opengl>
#include <fmt/format.h>
#include <include/cef_app.h>
#include <include/cef_render_handler.h>

namespace openspace {

class GUIRenderHandler : public CefRenderHandler {
public:
    GUIRenderHandler() {};

    void initialize();
    void initializeGL();
    void reshape(int, int);
    void draw(void);
    bool GetViewRect(CefRefPtr<CefBrowser> browser, CefRect &rect) override;
    void OnPaint(CefRefPtr<CefBrowser> browser, PaintElementType type, const RectList &dirtyRects, const void *buffer,
                 int width, int height) override;

private:
    int width = 0, height = 0;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    GLuint program, vao, vbo, texture, positionLoc;

    IMPLEMENT_REFCOUNTING(GUIRenderHandler);
};

} // namespace openspace

#endif  // __OPENSPACE_MODULE_WEBGUI___GUI_RENDER_HANDLER___H__
