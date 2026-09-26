/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___OPENGLSTATECACHE___H__
#define __GHOUL___OPENGLSTATECACHE___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <array>
#include <limits>
#include <vector>

namespace ghoul::opengl {

/**
 * This class works as cache for the OpenGL most common properties defining the current
 * GL's state. Be aware that not all OpenGL states are available for caching at this
 * moment. New states caching should be added as needed.
 */
class OpenGLStateCache {
public:
    /**
     * Currently, this class is a Singleton. In the future we will enable multiple
     * instances of the class when working with multiple OpenGL contexts.
     */
    static OpenGLStateCache* instance();

    OpenGLStateCache(const OpenGLStateCache&) = delete;
    OpenGLStateCache(OpenGLStateCache&&) = delete;
    OpenGLStateCache& operator=(const OpenGLStateCache&) = delete;
    OpenGLStateCache& operator=(OpenGLStateCache&&) = delete;

    bool isCacheInitialized() const;
    void loadCurrentGLState();
    void resetCachedStates() const;
    void resetBlendState() const;
    void resetDepthState() const;
    void resetLineState() const;
    void resetPolygonAndClippingState() const;
    void resetViewportState() const;
    void setDefaultFramebuffer(GLuint defaultFB);
    void setViewportState(const GLint viewportCoords[4]);
    void resetColorState() const;
    void setColorState(GLfloat clearColor[4], GLboolean clampColor = GL_FALSE);

    void viewport(GLint viewport[4]) const;

    GLuint defaultFramebuffer() const;

private:
    OpenGLStateCache() = default;
    ~OpenGLStateCache() = default;

    static OpenGLStateCache* _singleton;
    static GLint _maxAttachBuffers;

    bool _cacheInitialized = false;

    // Default Framebuffer (Initialized to max GLuint on purpose)
    // std::numeric_limits<GLuint>::max() is way bigger than the max number of OpenGL's
    // FrameBuffers names possible to create
    GLuint _defaultFramebuffer = std::numeric_limits<GLuint>::max();

    // Viewport
    std::array<GLint, 4> _viewport = { 0, 0, 0, 0 };

    // Polygon and Culling
    GLboolean _faceCullingEnabled = false;
    GLenum _faceToCull = GL_BACK;

    struct {
        GLboolean enabled = false;
        GLfloat factor = 0.f;
        GLfloat units = 0.f;
    } _polygonOffset;

    // Color
    std::array<GLfloat, 4> _colorClearValue = { 0.f, 0.f, 0.f, 0.f };
    GLboolean _clampColorEnabled;

    // Depth
    struct {
        GLboolean testEnabled = false;
        GLboolean maskEnabled = true;
        GLfloat clearValue = 1.f;
        GLenum function;
    } _depth;

    // Blending
    struct {
        GLboolean enabled = false;
        std::vector<GLboolean> enabledArray;
        GLenum equationRGB;
        GLenum equationAlpha;
        GLenum srcRGB;
        GLenum srcAlpha;
        GLenum destRGB;
        GLenum destAlpha;
    } _blending;

    // Line
    struct {
        GLboolean smoothEnabled = false;
        GLfloat width = 1.f;
    } _line;
};

} // namespace ghoul::opengl

#endif // __GHOUL___OPENGLSTATECACHE___H__
