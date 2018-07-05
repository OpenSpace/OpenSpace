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

#include <modules/webbrowser/include/webrenderhandler.h>

#include <ghoul/glm.h>

namespace openspace {

void WebRenderHandler::reshape(int w, int h) {
    _width = w;
    _height = h;
    _alphaMask.clear();
    _alphaMask.resize(w * h);
}

bool WebRenderHandler::GetViewRect(CefRefPtr<CefBrowser> browser, CefRect& rect) {
    rect = CefRect(0, 0, _width, _height);
    return true;
}

void WebRenderHandler::OnPaint(CefRefPtr<CefBrowser> browser,
                               CefRenderHandler::PaintElementType,
                               const CefRenderHandler::RectList& dirtyRects,
                               const void* buffer, int w, int h)
{
    glBindTexture(GL_TEXTURE_2D, _texture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        _width,
        _height,
        0,
        GL_BGRA_EXT,
        GL_UNSIGNED_BYTE,
        buffer
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    // copy alpha channel from buffer into the alpha mask
    const unsigned char* bf = reinterpret_cast<const unsigned char*>(buffer);
    for (int maskIndex = 0, bufferIndex = 3;
         maskIndex < w * h;
         bufferIndex += 4, ++maskIndex)
    {
        _alphaMask[maskIndex] = bf[bufferIndex] > 0;
    }
}

bool WebRenderHandler::hasContent(int x, int y) {
    int index = x + (_width * y);
    index = glm::clamp(index, 0, static_cast<int>(_alphaMask.size() - 1));
    return _alphaMask[index];
}

} // namespace openspace
