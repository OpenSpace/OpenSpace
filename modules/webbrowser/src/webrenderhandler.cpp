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

#include <modules/webbrowser/include/webrenderhandler.h>

#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

void WebRenderHandler::reshape(int w, int h) {
    if (w == _windowSize.x && h == _windowSize.y) {
        return;
    }
    _windowSize = glm::ivec2(w, h);
    _needsRepaint = true;
}

void WebRenderHandler::GetViewRect(CefRefPtr<CefBrowser>, CefRect& rect) {
    rect = CefRect(0, 0, _windowSize.x, _windowSize.y);
}

void WebRenderHandler::OnPaint(CefRefPtr<CefBrowser>, CefRenderHandler::PaintElementType,
                               const CefRenderHandler::RectList& dirtyRects,
                               const void* buffer, int w, int h)
{
    const size_t bufferSize = static_cast<size_t>(w * h);

    glm::ivec2 upperUpdatingRectBound = glm::ivec2(0, 0);
    glm::ivec2 lowerUpdatingRectBound = glm::ivec2(w, h);

    if (_needsRepaint || _browserBuffer.size() != bufferSize) {
        _browserBufferSize = glm::ivec2(w, h);
        _browserBuffer.resize(w * h, Pixel(0));
        _textureSizeIsDirty = true;
        _upperDirtyRectBound = upperUpdatingRectBound = glm::ivec2(w, h);
        _lowerDirtyRectBound = lowerUpdatingRectBound = glm::ivec2(0, 0);
    }

    for (const CefRect& r : dirtyRects) {
        lowerUpdatingRectBound = glm::min(lowerUpdatingRectBound, glm::ivec2(r.x, r.y));
        upperUpdatingRectBound = glm::max(
            upperUpdatingRectBound,
            glm::ivec2(r.x + r.width, r.y + r.height)
        );

    }

    const glm::ivec2 rectSize = upperUpdatingRectBound - lowerUpdatingRectBound;
    if (rectSize.x > 0 && rectSize.y > 0) {
        _textureIsDirty = true;
    }
    else {
        return;
    }

    // Copy the updated rectangle line by line.
    for (int y = lowerUpdatingRectBound.y; y < upperUpdatingRectBound.y; ++y) {
        const int lineOffset = y * w + lowerUpdatingRectBound.x;
        // Chromium stores image upside down compared to OpenGL, so we flip it:
        const int invLineOffset = (h - y - 1) * w + lowerUpdatingRectBound.x;
        const int rectWidth = upperUpdatingRectBound.x - lowerUpdatingRectBound.x;
        std::copy(
            reinterpret_cast<const Pixel*>(buffer) + lineOffset,
            reinterpret_cast<const Pixel*>(buffer) + lineOffset + rectWidth,
            _browserBuffer.data() + invLineOffset
        );
    }

    // Add the dirty rect bounds to the GPU texture dirty rect.
    _lowerDirtyRectBound = glm::min(lowerUpdatingRectBound, _lowerDirtyRectBound);
    _upperDirtyRectBound = glm::max(upperUpdatingRectBound, _upperDirtyRectBound);
    _needsRepaint = false;
}

void WebRenderHandler::updateTexture() {
    if (_needsRepaint) {
        return;
    }

    if (_textureSizeIsDirty) {
        glBindTexture(GL_TEXTURE_2D, _texture);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGBA,
            _browserBufferSize.x,
            _browserBufferSize.y,
            0,
            GL_BGRA_EXT,
            GL_UNSIGNED_BYTE,
            reinterpret_cast<char*>(_browserBuffer.data())
        );
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindTexture(GL_TEXTURE_2D, 0);
    }
    else if (_textureIsDirty) {
        glBindTexture(GL_TEXTURE_2D, _texture);
        glPixelStorei(GL_UNPACK_ROW_LENGTH, _browserBufferSize.x);

        glTexSubImage2D(
            GL_TEXTURE_2D,
            0,
            _lowerDirtyRectBound.x,
            _browserBufferSize.y - _upperDirtyRectBound.y,
            _upperDirtyRectBound.x - _lowerDirtyRectBound.x,
            _upperDirtyRectBound.y - _lowerDirtyRectBound.y,
            GL_BGRA_EXT,
            GL_UNSIGNED_BYTE,
            reinterpret_cast<char*>(
                _browserBuffer.data() +
                (_browserBufferSize.y - _upperDirtyRectBound.y) *
                _browserBufferSize.x + _lowerDirtyRectBound.x
            )
        );

        glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    _upperDirtyRectBound = glm::ivec2(0, 0);
    _lowerDirtyRectBound = glm::ivec2(_browserBufferSize.x, _browserBufferSize.y);
    _textureSizeIsDirty = false;
    _textureIsDirty = false;
}

bool WebRenderHandler::hasContent(int x, int y) {
    if (_browserBuffer.empty()) {
        return false;
    }
    int index = x + _browserBufferSize.x * (_browserBufferSize.y - y - 1);
    index = glm::clamp(index, 0, static_cast<int>(_browserBuffer.size() - 1));
    return _browserBuffer[index].a;
}

bool WebRenderHandler::isTextureReady() const {
    return !_needsRepaint;
}

} // namespace openspace
