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

#include <modules/webbrowser/include/webrenderhandler.h>
#include <modules/webbrowser/webbrowsermodule.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

WebRenderHandler::WebRenderHandler()
    : _acceleratedRendering(WebBrowserModule::canUseAcceleratedRendering())
{
    if (_acceleratedRendering) {
        glCreateTextures(GL_TEXTURE_2D, 1, &_texture);
    }
    else {
        glGenTextures(1, &_texture);
    }
}

void WebRenderHandler::reshape(int w, int h) {
    if (w == _windowSize.x && h == _windowSize.y) {
        return;
    }
    ghoul_assert(w > 0 && h > 0, std::format("Reshaped browser to {} x {}", w, h));
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
    // This should never happen - if accelerated rendering is on the OnAcceleratePaint
    // method should be called. But we instatiate the web render handler and the browser
    // instance in different places so room for error
    ghoul_assert(!_acceleratedRendering, "Accelerated rendering flag is turned on");

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

#ifdef WIN32
void WebRenderHandler::OnAcceleratedPaint(CefRefPtr<CefBrowser>,
                                          PaintElementType,
                                          const RectList& dirtyRects,
                                          const CefAcceleratedPaintInfo& info)
{
    // This should never happen - if accelerated rendering is off the OnPaint method
    // should be called. But we instatiate the web render handler and the browser instance
    // in different places so there is room for error
    ghoul_assert(_acceleratedRendering, "Accelerated rendering flag is turned off");

    if (dirtyRects.empty()) {
        return;
    }
    // When the window is minimized the texture is 1x1.
    // This prevents a hard crash when minimizing the window or when you resize the window
    // so that only the top bar is visible.
    // @TODO (ylvse 2024-08-20): minimizing window should be handled with the appropriate
    // function in the CefBrowser called WasHidden
    if (dirtyRects[0].height <= 1 || dirtyRects[0].width <= 1) {
        return;
    }
    // This function is called asynchronously after a reshape which means we have to check
    // for what we request. Validate the size. This prevents rendering a texture with the
    // wrong size (the look will be deformed)
    int newWidth = dirtyRects[0].width;
    int newHeight = dirtyRects[0].height;
    if (newWidth != _windowSize.x || newHeight != _windowSize.y) {
        return;
    }

    GLuint sharedTexture;
    GLuint memObj;
    // Create new texture that we can copy the shared texture into. Unfortunately
    // textures are immutable so we have to create a new one
    glCreateTextures(GL_TEXTURE_2D, 1, &sharedTexture);

    // Create the memory object handle
    glCreateMemoryObjectsEXT(1, &memObj);

    // The size of the texture we get from CEF. The CEF format is CEF_COLOR_TYPE_BGRA_8888
    // It has 4 bytes per pixel. The mem object requires this to be multiplied with 2
    int size = newWidth * newHeight * 8;

    // Cef uses the GL_HANDLE_TYPE_D3D11_IMAGE_EXT handle for their shared texture
    // Import the shared texture to the memory object
    glImportMemoryWin32HandleEXT(
        memObj, size, GL_HANDLE_TYPE_D3D11_IMAGE_EXT, info.shared_texture_handle
    );

    // @TODO (2025-02-17, abock) The following line will cause a crash on AMD cards if
    // accelerated rendering is enabled. I wasn't able to figure out why, but this is
    // the reason accelerated rendering is disabled for AMD.

    // Allocate immutable storage for the texture for the data from the memory object
    // Use GL_RGBA8 since it is 4 bytes
    glTextureStorageMem2DEXT(sharedTexture, 1, GL_RGBA8, newWidth, newHeight, memObj, 0);

    // Clean up the temporary allocations
    glDeleteTextures(1, &_texture);

    glDeleteMemoryObjectsEXT(1, &memObj);
    // Set the updated texture
    _texture = sharedTexture;
    _needsRepaint = false;
}
#endif // WIN32

void WebRenderHandler::updateTexture() {
    if (_acceleratedRendering || _needsRepaint) {
        return;
    }

    if (_textureSizeIsDirty) [[unlikely]] {
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
    // We don't have any content if we are querying outside the window size
    if (x < 0 || x > _windowSize.x || y < 0 || y > _windowSize.y) {
        return false;
    }

    if (_acceleratedRendering) {
        // To see the alpha value of the pixel we first have to get the texture from the
        // GPU. Use a PBO for better performance
        bool hasContent = false;
        GLuint pbo = 0;
        glGenBuffers(1, &pbo);
        glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo);

        // Allocate memory for the PBO (width * height * 4 bytes for RGBA)
        glBufferData(
            GL_PIXEL_PACK_BUFFER,
            _windowSize.x * _windowSize.y * 4,
            nullptr,
            GL_STREAM_READ
        );

        glBindTexture(GL_TEXTURE_2D, _texture);

        // Read the texture data into the PBO
        glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);

        // Map the PBO to the CPU memory space
        GLubyte* pixels = reinterpret_cast<GLubyte*>(
            glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY)
        );

        ghoul_assert(pixels, "Could not read pixels from the GPU for the cef gui.");
        if (pixels) {
            // Access the specific pixel data
            int index = (y * _windowSize.x + x) * 4;
            if (index < _windowSize.x* _windowSize.y * 4) {
                // The alpha value is at the fourth place (rgba)
                GLubyte a = pixels[index + 3];
                hasContent = a > 0;
            }
        }
        // Unmap the buffer
        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);

        // Unbind and delete the PBO
        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
        glDeleteBuffers(1, &pbo);
        return hasContent;
    }
    else {
        if (_browserBuffer.empty()) {
            return false;
        }
        int index = x + _browserBufferSize.x * (_browserBufferSize.y - y - 1);
        index = glm::clamp(index, 0, static_cast<int>(_browserBuffer.size() - 1));
        return _browserBuffer[index].a != 0;
    }
}

bool WebRenderHandler::isTextureReady() const {
    return !_needsRepaint;
}

void WebRenderHandler::bindTexture() {
    glBindTexture(GL_TEXTURE_2D, _texture);
}

} // namespace openspace
