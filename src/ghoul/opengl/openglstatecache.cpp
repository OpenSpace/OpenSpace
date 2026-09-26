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

#include <ghoul/opengl/openglstatecache.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>

namespace ghoul::opengl {

OpenGLStateCache* OpenGLStateCache::_singleton = nullptr;
GLint OpenGLStateCache::_maxAttachBuffers = 8;

OpenGLStateCache* OpenGLStateCache::instance() {
    if (!_singleton) {
        _singleton = new OpenGLStateCache;
        glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, &_maxAttachBuffers);
        _singleton->_blending.enabledArray.reserve(_maxAttachBuffers);
    }

    return _singleton;
}

bool OpenGLStateCache::isCacheInitialized() const {
    return _cacheInitialized;
}

void OpenGLStateCache::loadCurrentGLState() {
    // Viewport
    glGetIntegerv(GL_VIEWPORT, _viewport.data());

    // Polygon and Culling
    _faceCullingEnabled = glIsEnabled(GL_CULL_FACE);
    glGetIntegerv(GL_CULL_FACE_MODE, &_faceToCull);
    _polygonOffset.enabled = glIsEnabled(GL_POLYGON_OFFSET_FILL);
    glGetFloatv(GL_POLYGON_OFFSET_FACTOR, &_polygonOffset.factor);
    glGetFloatv(GL_POLYGON_OFFSET_UNITS, &_polygonOffset.units);

    // Color
    glGetFloatv(GL_COLOR_CLEAR_VALUE, _colorClearValue.data());
    glGetBooleanv(GL_CLAMP_READ_COLOR, &_clampColorEnabled);

    // Depth
    glGetFloatv(GL_DEPTH_CLEAR_VALUE, &_depth.clearValue);
    _depth.testEnabled = glIsEnabled(GL_DEPTH_TEST);
    glGetBooleanv(GL_DEPTH_WRITEMASK, &_depth.maskEnabled);
    glGetIntegerv(GL_DEPTH_FUNC, &_depth.function);

    // Blending
    _blending.enabled = glIsEnabled(GL_BLEND);

    if (_blending.enabledArray.empty()) {
        for (int i = 0; i < _maxAttachBuffers; i++) {
            _blending.enabledArray.push_back(glIsEnabledi(GL_BLEND, i));
        }
    }
    else {
        std::vector<GLboolean>::iterator blendBufferIt = _blending.enabledArray.begin();
        std::vector<GLboolean>::iterator endBlendBuffer = _blending.enabledArray.end();
        for (int i = 0; blendBufferIt < endBlendBuffer; blendBufferIt++, i++) {
            *blendBufferIt = glIsEnabledi(GL_BLEND, i);
        }
    }

    glGetIntegerv(GL_BLEND_EQUATION_RGB, &_blending.equationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &_blending.equationAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &_blending.srcRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &_blending.srcAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &_blending.destRGB);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &_blending.destAlpha);

    // Line
    _line.smoothEnabled = glIsEnabled(GL_LINE_SMOOTH);
    glGetFloatv(GL_LINE_WIDTH, &_line.width);

    _cacheInitialized = true;
}

void OpenGLStateCache::resetCachedStates() const {
    ghoul_assert(_cacheInitialized, "OpenGL State cache is not initialized");

    resetViewportState();
    resetColorState();
    resetBlendState();
    resetDepthState();
    resetLineState();
    resetPolygonAndClippingState();
}

void OpenGLStateCache::resetBlendState() const {
    if (_blending.enabled) {
        glEnable(GL_BLEND);
    }
    else {
        glDisable(GL_BLEND);
    }

    auto it  = _blending.enabledArray.cbegin();
    for (int i = 0; it < _blending.enabledArray.cend(); it++, i++) {
        if (*it) {
            glEnablei(GL_BLEND, i);
        }
        else {
            glDisablei(GL_BLEND, i);
        }
    }

    glBlendEquationSeparate(_blending.equationRGB, _blending.equationAlpha);
    glBlendFuncSeparate(
        _blending.srcRGB,
        _blending.destRGB,
        _blending.srcAlpha,
        _blending.destAlpha
    );
}

void OpenGLStateCache::resetDepthState() const {
    if (_depth.testEnabled) {
        glEnable(GL_DEPTH_TEST);
    }
    else {
        glDisable(GL_DEPTH_TEST);
    }

    if (_depth.maskEnabled) {
        glDepthMask(GL_TRUE);
    }
    else {
        glDepthMask(GL_FALSE);
    }

    glClearDepth(_depth.clearValue);
    glDepthFunc(_depth.function);
}

void OpenGLStateCache::resetLineState() const {
    if (_line.smoothEnabled) {
        glEnable(GL_LINE_SMOOTH);
    }
    else {
        glDisable(GL_LINE_SMOOTH);
    }
    glLineWidth(_line.width);
}

void OpenGLStateCache::resetPolygonAndClippingState() const {
    if (_faceCullingEnabled) {
        glEnable(GL_CULL_FACE);
    }
    else {
        glDisable(GL_CULL_FACE);
    }

    glCullFace(_faceToCull);

    if (_polygonOffset.enabled) {
        glEnable(GL_POLYGON_OFFSET_FILL);
    }
    else {
        glDisable(GL_POLYGON_OFFSET_FILL);
    }

    glPolygonOffset(_polygonOffset.factor, _polygonOffset.units);
}

void OpenGLStateCache::resetColorState() const {
    glClearColor(
        _colorClearValue[0],
        _colorClearValue[1],
        _colorClearValue[2],
        _colorClearValue[3]
    );
}

void OpenGLStateCache::setColorState(GLfloat clearColor[4], GLboolean clampColor) {
    ghoul_assert(clearColor != nullptr, "color must not be nullptr");

    if (!std::equal_to<>()(clearColor[0], _colorClearValue[0]) ||
        !std::equal_to<>()(clearColor[1], _colorClearValue[1]) ||
        !std::equal_to<>()(clearColor[2], _colorClearValue[2]) ||
        !std::equal_to<>()(clearColor[3], _colorClearValue[3]))
    {
        _colorClearValue[0] = clearColor[0];
        _colorClearValue[1] = clearColor[1];
        _colorClearValue[2] = clearColor[2];
        _colorClearValue[3] = clearColor[3];
    }

    if (clampColor != _clampColorEnabled) {
        _clampColorEnabled = clampColor;
        // glClampColor is weird as it requires a GLenum in the function definition, but
        // the OpenGL standard says that it only accepts GL_FALSE and GL_TRUE, which are
        // of type GLboolean *eye rolling*
        // GLenum(0) == GLboolen(0) == GL_FALSE
        glClampColor(GL_CLAMP_READ_COLOR, GLenum(0));
    }

    glClearColor(
        _colorClearValue[0],
        _colorClearValue[1],
        _colorClearValue[2],
        _colorClearValue[3]
    );
}

void OpenGLStateCache::resetViewportState() const {
    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);
}

void OpenGLStateCache::setDefaultFramebuffer(GLuint defaultFB) {
    ghoul_assert(
        defaultFB < std::numeric_limits<GLuint>::max(),
        "The default Framebuffer must be a valid number"
    );

    _defaultFramebuffer = defaultFB;
}

void OpenGLStateCache::setViewportState(const GLint viewportCoords[4])  {
    ghoul_assert(viewportCoords != nullptr, "viewportCoords must not be nullptr");

    if (viewportCoords[0] != _viewport[0] || viewportCoords[1] != _viewport[1] ||
        viewportCoords[2] != _viewport[2] || viewportCoords[3] != _viewport[3])
    {
        _viewport[0] = viewportCoords[0];
        _viewport[1] = viewportCoords[1];
        _viewport[2] = viewportCoords[2];
        _viewport[3] = viewportCoords[3];
    }

    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);
}

void OpenGLStateCache::viewport(GLint viewport[4]) const {
    ghoul_assert(viewport != nullptr, "viewport must not be nullptr");

    viewport[0] = _viewport[0];
    viewport[1] = _viewport[1];
    viewport[2] = _viewport[2];
    viewport[3] = _viewport[3];
}

GLuint OpenGLStateCache::defaultFramebuffer() const {
    return _defaultFramebuffer;
}

} // namespace ghoul::opengl
