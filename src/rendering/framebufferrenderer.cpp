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

#include <openspace/rendering/framebufferrenderer.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/deferredcaster.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/type_ptr.hpp>
#include <fstream>
#include <string>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "FramebufferRenderer";

    constexpr glm::vec4 PosBufferClearVal = glm::vec4(1e32, 1e32, 1e32, 1.f);

    constexpr std::array<const char*, 9> HDRUniformNames = {
        "hdrFeedingTexture", "blackoutFactor", "hdrExposure", "gamma",
        "Hue", "Saturation", "Value", "Viewport", "Resolution"
    };

    constexpr std::array<const char*, 4> FXAAUniformNames = {
        "renderedTexture", "inverseScreenSize", "Viewport", "Resolution"
    };

    constexpr std::array<const char*, 4> DownscaledVolumeUniformNames = {
        "downscaledRenderedVolume", "downscaledRenderedVolumeDepth", "viewport",
        "resolution"
    };

    constexpr std::string_view ExitFragmentShaderPath =
        "${SHADERS}/framebuffer/exitframebuffer.frag";
    constexpr std::string_view RaycastFragmentShaderPath =
        "${SHADERS}/framebuffer/raycastframebuffer.frag";
    constexpr std::string_view GetEntryInsidePath = "${SHADERS}/framebuffer/inside.glsl";
    constexpr std::string_view GetEntryOutsidePath =
        "${SHADERS}/framebuffer/outside.glsl";
    constexpr std::string_view RenderFragmentShaderPath =
        "${SHADERS}/framebuffer/renderframebuffer.frag";

    constexpr std::array<GLenum, 4> ColorAttachmentArray = {
       GL_COLOR_ATTACHMENT0,
       GL_COLOR_ATTACHMENT1,
       GL_COLOR_ATTACHMENT2,
       GL_COLOR_ATTACHMENT3
    };
} // namespace

namespace openspace {

//============================//
//=====  Reuse textures  =====//
//============================//
GLuint FramebufferRenderer::additionalColorTexture1() const {
    // Gives access to the currently NOT used pingPongTexture
    const int unusedPingPongIndex = _pingPongIndex == 0 ? 1 : 0;
    return _pingPongBuffers.colorTexture[unusedPingPongIndex];
}

GLuint FramebufferRenderer::additionalColorTexture2() const {
    // Gives access to the exitColorTexture
    return _exitColorTexture;
}

GLuint FramebufferRenderer::additionalColorTexture3() const {
    // Gives access to the fxaaTexture
    return _fxaaBuffers.fxaaTexture;
}

GLuint FramebufferRenderer::additionalDepthTexture() const {
    // Gives access to the exitDepthTexture
    return _exitDepthTexture;
}

//=============================//
//=====  Access G-buffer  =====//
//=============================//
GLuint FramebufferRenderer::gBufferColorTexture() const {
    // Gives access to the color texture of the G-buffer
    return _gBuffers.colorTexture;
}

GLuint FramebufferRenderer::gBufferPositionTexture() const {
    // Gives access to the position texture of the G-buffer
    return _gBuffers.positionTexture;
}

GLuint FramebufferRenderer::gBufferNormalTexture() const {
    // Gives access to the normal texture of the G-buffer
    return _gBuffers.normalTexture;
}

GLuint FramebufferRenderer::gBufferDepthTexture() const {
    // Gives access to the depth texture of the G-buffer
    return _gBuffers.depthTexture;
}

void FramebufferRenderer::initialize() {
    ZoneScoped;
    TracyGpuZone("Rendering initialize");

    LDEBUG("Initializing FramebufferRenderer");

    constexpr std::array<GLfloat, 12> VertexData = {
        // x     y
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(0);

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);

    // GBuffers
    glGenTextures(1, &_gBuffers.colorTexture);
    glGenTextures(1, &_gBuffers.depthTexture);
    glGenTextures(1, &_gBuffers.positionTexture);
    glGenTextures(1, &_gBuffers.normalTexture);
    glGenFramebuffers(1, &_gBuffers.framebuffer);


    // PingPong Buffers
    // The first pingpong buffer shares the color texture with the renderbuffer:
    _pingPongBuffers.colorTexture[0] = _gBuffers.colorTexture;
    glGenTextures(1, &_pingPongBuffers.colorTexture[1]);
    glGenFramebuffers(1, &_pingPongBuffers.framebuffer);

    // Exit framebuffer
    glGenTextures(1, &_exitColorTexture);
    glGenTextures(1, &_exitDepthTexture);
    glGenFramebuffers(1, &_exitFramebuffer);

    // FXAA Buffers
    glGenFramebuffers(1, &_fxaaBuffers.fxaaFramebuffer);
    glGenTextures(1, &_fxaaBuffers.fxaaTexture);

    // DownscaleVolumeRendering
    glGenFramebuffers(1, &_downscaleVolumeRendering.framebuffer);
    glGenTextures(1, &_downscaleVolumeRendering.colorTexture);
    glGenTextures(1, &_downscaleVolumeRendering.depthbuffer);

    // Allocate Textures/Buffers Memory
    updateResolution();

    updateRendererData();
    updateRaycastData();

    //==============================//
    //=====  GBuffers Buffers  =====//
    //==============================//
    glBindFramebuffer(GL_FRAMEBUFFER, _gBuffers.framebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _gBuffers.colorTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        _gBuffers.positionTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT2,
        _gBuffers.normalTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        _gBuffers.depthTexture,
        0
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _gBuffers.framebuffer, -1, "G-Buffer Main");
    }

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    //==============================//
    //=====  PingPong Buffers  =====//
    //==============================//
    glBindFramebuffer(GL_FRAMEBUFFER, _pingPongBuffers.framebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _pingPongBuffers.colorTexture[0],
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        _pingPongBuffers.colorTexture[1],
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        _gBuffers.depthTexture,
        0
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_FRAMEBUFFER,
            _pingPongBuffers.framebuffer,
            -1,
            "G-Buffer Ping-Pong"
        );
    }

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Ping pong buffer is not complete");
    }

    //======================================//
    //=====  Volume Rendering Buffers  =====//
    //======================================//
    // Builds Exit Framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _exitColorTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        _exitDepthTexture,
        0
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _exitFramebuffer, -1, "Exit");
    }

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Exit framebuffer is not complete");
    }

    //===================================//
    //==========  FXAA Buffers  =========//
    //===================================//
    glBindFramebuffer(GL_FRAMEBUFFER, _fxaaBuffers.fxaaFramebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _fxaaBuffers.fxaaTexture,
        0
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _fxaaBuffers.fxaaFramebuffer, -1, "FXAA");
    }

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("FXAA framebuffer is not complete");
    }

    //================================================//
    //=====  Downscale Volume Rendering Buffers  =====//
    //================================================//
    glBindFramebuffer(GL_FRAMEBUFFER, _downscaleVolumeRendering.framebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _downscaleVolumeRendering.colorTexture,
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        _downscaleVolumeRendering.depthbuffer,
        0
    );
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_FRAMEBUFFER,
            _downscaleVolumeRendering.framebuffer,
            -1,
            "Downscaled Volume"
        );
    }

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Downscale Volume Rendering framebuffer is not complete");
    }


    // JCC: Moved to here to avoid NVidia: "Program/shader state performance warning"
    // Building programs
    updateHDRAndFiltering();
    updateFXAA();
    updateDeferredcastData();
    updateDownscaledVolume();

    // Sets back to default FBO
    glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);

    ghoul::opengl::updateUniformLocations(
        *_hdrFilteringProgram,
        _hdrUniformCache,
        HDRUniformNames
    );
    ghoul::opengl::updateUniformLocations(
        *_fxaaProgram,
        _fxaaUniformCache,
        FXAAUniformNames
    );
    ghoul::opengl::updateUniformLocations(
        *_downscaledVolumeProgram,
        _writeDownscaledVolumeUniformCache,
        DownscaledVolumeUniformNames
    );

    global::raycasterManager->addListener(*this);
    global::deferredcasterManager->addListener(*this);

    // Set Default Rendering OpenGL State
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);
    glEnablei(GL_BLEND, 0);
    glDisablei(GL_BLEND, 1);
    glDisablei(GL_BLEND, 2);

    // glClampColor is weird as it requires a GLenum in the function definition, but
    // the OpenGL standard says that it only accepts GL_FALSE and GL_TRUE, which are
    // of type GLboolean *eye rolling*
    // GLenum(0) == GLboolen(0) == GL_FALSE
    glClampColor(GL_CLAMP_READ_COLOR, GLenum(0));

    glEnable(GL_DEPTH_TEST);

    // Default GL State for Blending
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Save State in Cache
    global::renderEngine->openglStateCache().loadCurrentGLState();
}

void FramebufferRenderer::deinitialize() {
    ZoneScoped;
    TracyGpuZone("Renderer deinitialize");

    LINFO("Deinitializing FramebufferRenderer");

    glDeleteFramebuffers(1, &_gBuffers.framebuffer);
    glDeleteFramebuffers(1, &_exitFramebuffer);
    glDeleteFramebuffers(1, &_fxaaBuffers.fxaaFramebuffer);
    glDeleteFramebuffers(1, &_pingPongBuffers.framebuffer);
    glDeleteFramebuffers(1, &_downscaleVolumeRendering.framebuffer);

    glDeleteTextures(1, &_gBuffers.colorTexture);
    glDeleteTextures(1, &_gBuffers.depthTexture);

    glDeleteTextures(1, &_fxaaBuffers.fxaaTexture);
    glDeleteTextures(1, &_gBuffers.positionTexture);
    glDeleteTextures(1, &_gBuffers.normalTexture);
    glDeleteTextures(1, &_downscaleVolumeRendering.colorTexture);
    glDeleteTextures(1, &_downscaleVolumeRendering.depthbuffer);

    glDeleteTextures(1, &_pingPongBuffers.colorTexture[1]);

    glDeleteTextures(1, &_exitColorTexture);
    glDeleteTextures(1, &_exitDepthTexture);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    global::raycasterManager->removeListener(*this);
    global::deferredcasterManager->removeListener(*this);
}

void FramebufferRenderer::raycastersChanged(VolumeRaycaster&,
                                            RaycasterListener::IsAttached)
{
    _dirtyRaycastData = true;
}

void FramebufferRenderer::deferredcastersChanged(Deferredcaster&,
                                                 DeferredcasterListener::IsAttached)
{
    _dirtyDeferredcastData = true;
}

void FramebufferRenderer::applyTMO(float blackoutFactor, const glm::ivec4& viewport) {
    ZoneScoped;
    TracyGpuZone("applyTMO");

    _hdrFilteringProgram->activate();

    ghoul::opengl::TextureUnit hdrFeedingTextureUnit;
    hdrFeedingTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _pingPongBuffers.colorTexture[_pingPongIndex]);

    _hdrFilteringProgram->setUniform(
        _hdrUniformCache.hdrFeedingTexture,
        hdrFeedingTextureUnit
    );

    _hdrFilteringProgram->setUniform(_hdrUniformCache.blackoutFactor, blackoutFactor);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.hdrExposure, _hdrExposure);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.gamma, _gamma);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.Hue, _hue);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.Saturation, _saturation);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.Value, _value);
    _hdrFilteringProgram->setUniform(_hdrUniformCache.Viewport, glm::vec4(viewport));
    _hdrFilteringProgram->setUniform(_hdrUniformCache.Resolution, glm::vec2(_resolution));

    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);

    _hdrFilteringProgram->deactivate();
}

void FramebufferRenderer::applyFXAA(const glm::ivec4& viewport) {
    _fxaaProgram->activate();

    ghoul::opengl::TextureUnit renderedTextureUnit;
    renderedTextureUnit.activate();
    glBindTexture(
        GL_TEXTURE_2D,
        _fxaaBuffers.fxaaTexture
    );

    _fxaaProgram->setUniform(
        _fxaaUniformCache.renderedTexture,
        renderedTextureUnit
    );

    const glm::vec2 invScreenSize = glm::vec2(1.f / _resolution.x, 1.f / _resolution.y);
    _fxaaProgram->setUniform(_fxaaUniformCache.inverseScreenSize, invScreenSize);
    _fxaaProgram->setUniform(_fxaaUniformCache.Viewport, glm::vec4(viewport));
    _fxaaProgram->setUniform(_fxaaUniformCache.Resolution, glm::vec2(_resolution));

    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);

    _fxaaProgram->deactivate();
}

void FramebufferRenderer::updateDownscaleTextures() const {
    const float cdf = _downscaleVolumeRendering.currentDownscaleFactor;

    glBindTexture(GL_TEXTURE_2D, _downscaleVolumeRendering.colorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        static_cast<GLsizei>(glm::max(_resolution.x * cdf, 1.f)),
        static_cast<GLsizei>(glm::max(_resolution.y * cdf, 1.f)),
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    constexpr float VolumeBorderColor[] = { 0.f, 0.f, 0.f, 1.f };
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, VolumeBorderColor);

    glBindTexture(GL_TEXTURE_2D, _downscaleVolumeRendering.depthbuffer);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        static_cast<GLsizei>(glm::max(_resolution.x * cdf, 1.f)),
        static_cast<GLsizei>(glm::max(_resolution.y * cdf, 1.f)),
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}

void FramebufferRenderer::writeDownscaledVolume(const glm::ivec4& viewport) {
    _downscaledVolumeProgram->activate();

    ghoul::opengl::TextureUnit downscaledTextureUnit;
    downscaledTextureUnit.activate();
    glBindTexture(
        GL_TEXTURE_2D,
        _downscaleVolumeRendering.colorTexture
    );

    _downscaledVolumeProgram->setUniform(
        _writeDownscaledVolumeUniformCache.downscaledRenderedVolume,
        downscaledTextureUnit
    );

    ghoul::opengl::TextureUnit downscaledDepthUnit;
    downscaledDepthUnit.activate();
    glBindTexture(
        GL_TEXTURE_2D,
        _downscaleVolumeRendering.depthbuffer
    );

    _downscaledVolumeProgram->setUniform(
        _writeDownscaledVolumeUniformCache.downscaledRenderedVolumeDepth,
        downscaledDepthUnit
    );

    _downscaledVolumeProgram->setUniform(
        _writeDownscaledVolumeUniformCache.viewport,
        static_cast<float>(viewport[0]),
        static_cast<float>(viewport[1]),
        static_cast<float>(viewport[2]),
        static_cast<float>(viewport[3])
    );
    _downscaledVolumeProgram->setUniform(
        _writeDownscaledVolumeUniformCache.resolution,
        glm::vec2(_resolution)
    );


    glEnablei(GL_BLEND, 0);

    // (abock, 2021-04-30) I changed the blend function as the front-to-back blending
    // didn't work for the milkyway volume and was causing the edges of the milkyway to
    // disappear
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBlendFunc(GL_ONE, GL_ONE);

    glDisable(GL_DEPTH_TEST);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    glEnable(GL_DEPTH_TEST);

    _downscaledVolumeProgram->deactivate();

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void FramebufferRenderer::update() {
    if (_dirtyResolution) {
        updateResolution();
    }

    if (_dirtyRaycastData) {
        updateRaycastData();
    }

    if (_dirtyDeferredcastData) {
        updateDeferredcastData();
    }

    if (_hdrFilteringProgram->isDirty()) {
        _hdrFilteringProgram->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_hdrFilteringProgram,
            _hdrUniformCache,
            HDRUniformNames
        );
    }

    if (_fxaaProgram->isDirty()) {
        _fxaaProgram->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_fxaaProgram,
            _fxaaUniformCache,
            FXAAUniformNames
        );
    }

    if (_downscaledVolumeProgram->isDirty()) {
        _downscaledVolumeProgram->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_downscaledVolumeProgram,
            _writeDownscaledVolumeUniformCache,
            DownscaledVolumeUniformNames
        );
    }

    using K = VolumeRaycaster*;
    using V = std::unique_ptr<ghoul::opengl::ProgramObject>;
    for (const std::pair<const K, V>& program : _exitPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<const K, V>& program : _raycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<const K, V>& program : _insideRaycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<
            Deferredcaster* const,
            std::unique_ptr<ghoul::opengl::ProgramObject>
        >& program : _deferredcastPrograms)
    {
        if (program.second && program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }
}

void FramebufferRenderer::updateResolution() {
    ZoneScoped;
    TracyGpuZone("Renderer updateResolution");

    glBindTexture(GL_TEXTURE_2D, _gBuffers.colorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _gBuffers.colorTexture, -1, "G-Buffer Color");
    }

    glBindTexture(GL_TEXTURE_2D, _gBuffers.positionTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _gBuffers.positionTexture, -1, "G-Buffer Position");
    }

    glBindTexture(GL_TEXTURE_2D, _gBuffers.normalTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _gBuffers.normalTexture, -1, "G-Buffer Normal");
    }

    glBindTexture(GL_TEXTURE_2D, _gBuffers.depthTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _gBuffers.depthTexture, -1, "G-Buffer Depth");
    }

    glBindTexture(GL_TEXTURE_2D, _pingPongBuffers.colorTexture[1]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_TEXTURE,
            _pingPongBuffers.colorTexture[1],
            -1,
            "G-Buffer Color Ping-Pong"
        );
    }

    // FXAA
    glBindTexture(GL_TEXTURE_2D, _fxaaBuffers.fxaaTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _fxaaBuffers.fxaaTexture, -1, "FXAA");
    }

    const float cdf = _downscaleVolumeRendering.currentDownscaleFactor;

    // Downscale Volume Rendering
    glBindTexture(GL_TEXTURE_2D, _downscaleVolumeRendering.colorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        static_cast<GLsizei>(glm::max(_resolution.x * cdf, 1.f)),
        static_cast<GLsizei>(glm::max(_resolution.y * cdf, 1.f)),
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    constexpr std::array<float, 4> VolumeBorderColor = { 0.f, 0.f, 0.f, 1.f };
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, VolumeBorderColor.data());
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_TEXTURE,
            _downscaleVolumeRendering.colorTexture,
            -1,
            "Downscaled Volume Color"
        );
    }

    glBindTexture(GL_TEXTURE_2D, _downscaleVolumeRendering.depthbuffer);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        static_cast<GLsizei>(glm::max(_resolution.x * cdf, 1.f)),
        static_cast<GLsizei>(glm::max(_resolution.y * cdf, 1.f)),
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(
            GL_TEXTURE,
            _downscaleVolumeRendering.depthbuffer,
            -1,
            "Downscaled Volume Depth"
        );
    }

    // Volume Rendering Textures
    glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_SHORT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _exitColorTexture, -1, "Exit color");
    }

    glBindTexture(GL_TEXTURE_2D, _exitDepthTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_TEXTURE, _exitDepthTexture, -1, "Exit depth");
    }

    _dirtyResolution = false;
}

void FramebufferRenderer::updateRaycastData() {
    ZoneScoped;

    _raycastData.clear();
    _exitPrograms.clear();
    _raycastPrograms.clear();
    _insideRaycastPrograms.clear();

    const std::vector<VolumeRaycaster*>& raycasters =
        global::raycasterManager->raycasters();

    int nextId = 0;
    for (VolumeRaycaster* raycaster : raycasters) {
        ZoneScopedN("raycaster");

        RaycastData data = { .id = nextId++, .namespaceName = "Helper" };

        const std::string& vsPath = raycaster->boundsVertexShaderPath();
        std::string fsPath = raycaster->boundsFragmentShaderPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        dict.setValue("fragmentPath", std::move(fsPath));
        dict.setValue("id", data.id);

        std::string helperPath = raycaster->helperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", std::move(helperPath));
        }
        dict.setValue("helperPaths", std::move(helpersDict));
        dict.setValue("raycastPath", raycaster->raycasterPath());

        _raycastData[raycaster] = data;

        try {
            _exitPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                std::format("Volume {} exit", data.id),
                absPath(vsPath),
                absPath(ExitFragmentShaderPath),
                dict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary outsideDict = dict;
            outsideDict.setValue("getEntryPath", std::string(GetEntryOutsidePath));
            _raycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                std::format("Volume {} raycast", data.id),
                absPath(vsPath),
                absPath(RaycastFragmentShaderPath),
                outsideDict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary insideDict = dict;
            insideDict.setValue("getEntryPath", std::string(GetEntryInsidePath));
            _insideRaycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                std::format("Volume {} inside raycast", data.id),
                absPath("${SHADERS}/framebuffer/resolveframebuffer.vert"),
                absPath(RaycastFragmentShaderPath),
                insideDict
            );
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    _dirtyRaycastData = false;
}

void FramebufferRenderer::updateDeferredcastData() {
    _deferredcastData.clear();
    _deferredcastPrograms.clear();

    const std::vector<Deferredcaster*>& deferredcasters =
        global::deferredcasterManager->deferredcasters();
    int nextId = 0;
    for (Deferredcaster* caster : deferredcasters) {
        DeferredcastData data = { .id = nextId++, .namespaceName = "HELPER" };

        const std::filesystem::path vsPath = caster->deferredcastVSPath();
        const std::filesystem::path fsPath = caster->deferredcastFSPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        //dict.setValue("fragmentPath", fsPath);
        dict.setValue("id", data.id);
        const std::filesystem::path helperPath = caster->helperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", helperPath.string());
        }
        dict.setValue("helperPaths", helpersDict);

        _deferredcastData[caster] = data;

        try {
            _deferredcastPrograms[caster] = ghoul::opengl::ProgramObject::Build(
                std::format("Deferred {} raycast", data.id),
                vsPath,
                fsPath,
                dict
            );

            caster->initializeCachedVariables(*_deferredcastPrograms[caster]);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    _dirtyDeferredcastData = false;
}


void FramebufferRenderer::updateHDRAndFiltering() {
    ZoneScoped;

    _hdrFilteringProgram = ghoul::opengl::ProgramObject::Build(
        "HDR and Filtering Program",
        absPath("${SHADERS}/framebuffer/hdrAndFiltering.vert"),
        absPath("${SHADERS}/framebuffer/hdrAndFiltering.frag")
    );
}

void FramebufferRenderer::updateFXAA() {
    ZoneScoped;

    _fxaaProgram = ghoul::opengl::ProgramObject::Build(
        "FXAA Program",
        absPath("${SHADERS}/framebuffer/fxaa.vert"),
        absPath("${SHADERS}/framebuffer/fxaa.frag")
    );
}

void FramebufferRenderer::updateDownscaledVolume() {
    ZoneScoped;

    _downscaledVolumeProgram = ghoul::opengl::ProgramObject::Build(
        "Write Downscaled Volume Program",
        absPath("${SHADERS}/framebuffer/mergeDownscaledVolume.vert"),
        absPath("${SHADERS}/framebuffer/mergeDownscaledVolume.frag")
    );
}

void FramebufferRenderer::render(Scene* scene, Camera* camera, float blackoutFactor) {
    ZoneScoped;
    TracyGpuZone("FramebufferRenderer");

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);
    global::renderEngine->openglStateCache().setDefaultFramebuffer(_defaultFBO);

    std::array<GLint, 4> vp = {};
    glGetIntegerv(GL_VIEWPORT, vp.data());
    global::renderEngine->openglStateCache().setViewportState(vp.data());
    const glm::ivec4 viewport = glm::ivec4(vp[0], vp[1], vp[2], vp[3]);

    // Reset Render Pipeline State
    global::renderEngine->openglStateCache().resetCachedStates();

    _pingPongIndex = 0;

    if (!scene || !camera) {
        return;
    }

    {
        // deferred g-buffer
        ZoneScopedN("Deferred G-Buffer");
        TracyGpuZone("Deferred G-Buffer");

        glBindFramebuffer(GL_FRAMEBUFFER, _gBuffers.framebuffer);
        glDrawBuffers(3, ColorAttachmentArray.data());
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearBufferfv(GL_COLOR, 1, glm::value_ptr(PosBufferClearVal));
    }

    RenderData data = {
        .camera = *camera,
        .time = global::timeManager->time(),
        .renderBinMask = 0
    };
    RendererTasks tasks;

    {
        TracyGpuZone("Background")
        const ghoul::GLDebugGroup group("Background");
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Background);
        scene->render(data, tasks);
    }

    {
        TracyGpuZone("Opaque")
        const ghoul::GLDebugGroup group("Opaque");
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Opaque);
        scene->render(data, tasks);
    }

    {
        TracyGpuZone("PreDeferredTransparent")
        const ghoul::GLDebugGroup group("PreDeferredTransparent");
        data.renderBinMask = static_cast<int>(
            Renderable::RenderBin::PreDeferredTransparent
        );
        scene->render(data, tasks);
    }

    // Run Volume Tasks
    {
        TracyGpuZone("Raycaster Tasks")
        const ghoul::GLDebugGroup group("Raycaster Tasks");
        performRaycasterTasks(tasks.raycasterTasks, viewport);
    }

    if (!tasks.deferredcasterTasks.empty()) {
        TracyGpuZone("Deferred Caster Tasks")
        const ghoul::GLDebugGroup group("Deferred Caster Tasks");

        // We use ping pong rendering in order to be able to render multiple deferred
        // tasks at same time (e.g. more than 1 ATM being seen at once) to the same final
        // buffer
        glBindFramebuffer(GL_FRAMEBUFFER, _pingPongBuffers.framebuffer);
        glDrawBuffers(1, &ColorAttachmentArray[_pingPongIndex]);

        performDeferredTasks(tasks.deferredcasterTasks, viewport);
    }

    glDrawBuffers(1, &ColorAttachmentArray[_pingPongIndex]);
    glEnablei(GL_BLEND, 0);

    {
        TracyGpuZone("Overlay")
        const ghoul::GLDebugGroup group("Overlay");
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Overlay);
        scene->render(data, tasks);
    }

    {
        TracyGpuZone("PostDeferredTransparent")
        const ghoul::GLDebugGroup group("PostDeferredTransparent");
        data.renderBinMask = static_cast<int>(
            Renderable::RenderBin::PostDeferredTransparent
        );
        scene->render(data, tasks);
    }

    {
        TracyGpuZone("Sticker")
        const ghoul::GLDebugGroup group("Sticker");
        data.renderBinMask = static_cast<int>(
            Renderable::RenderBin::Sticker
        );
        scene->render(data, tasks);
    }

    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    // Disabling depth test for filtering and hdr
    glDisable(GL_DEPTH_TEST);

    if (_enableFXAA) {
        glBindFramebuffer(GL_FRAMEBUFFER, _fxaaBuffers.fxaaFramebuffer);
        glDrawBuffers(1, ColorAttachmentArray.data());
        glDisable(GL_BLEND);

    }
    else {
        // When applying the TMO, the result is saved to the default FBO to be displayed
        // by the Operating System. Also, the resolve procedure is executed in this step.
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    }

    {
        // Apply the selected TMO on the results and resolve the result to the default FBO
        TracyGpuZone("Apply TMO");
        const ghoul::GLDebugGroup group("Apply TMO");

        applyTMO(blackoutFactor, viewport);
    }

    if (_enableFXAA) {
        TracyGpuZone("Apply FXAA")
        const ghoul::GLDebugGroup group("Apply FXAA");
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
        applyFXAA(viewport);
    }
}

void FramebufferRenderer::performRaycasterTasks(const std::vector<RaycasterTask>& tasks,
                                                const glm::ivec4& viewport)
{
    ZoneScoped;

    for (const RaycasterTask& raycasterTask : tasks) {
        TracyGpuZone("Raycaster");

        VolumeRaycaster* raycaster = raycasterTask.raycaster;

        glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        //global::renderEngine->openglStateCache().viewport(glm::value_ptr(viewport));

        ghoul::opengl::ProgramObject* exitProgram = _exitPrograms[raycaster].get();
        if (exitProgram) {
            exitProgram->activate();
            raycaster->renderExitPoints(raycasterTask.renderData, *exitProgram);
            exitProgram->deactivate();
        }

        if (raycaster->downscaleRender() < 1.f) {
            glBindFramebuffer(GL_FRAMEBUFFER, _downscaleVolumeRendering.framebuffer);
            const float s = raycaster->downscaleRender();
            const std::array<GLint, 4> newVP = {
                static_cast<GLint>(viewport[0] * s),
                static_cast<GLint>(viewport[1] * s),
                static_cast<GLint>(viewport[2] * s),
                static_cast<GLint>(viewport[3] * s)
            };
            global::renderEngine->openglStateCache().setViewportState(newVP.data());

            if (_downscaleVolumeRendering.currentDownscaleFactor != s) {
                _downscaleVolumeRendering.currentDownscaleFactor = s;
                updateDownscaleTextures();
            }
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        }
        else {
            glBindFramebuffer(GL_FRAMEBUFFER, _gBuffers.framebuffer);
        }

        glm::vec3 cameraPosition = glm::vec3(0.f);
        const bool isCameraInside = raycaster->isCameraInside(
            raycasterTask.renderData,
            cameraPosition
        );
        ghoul::opengl::ProgramObject* raycastProgram = nullptr;

        if (isCameraInside) {
            raycastProgram = _insideRaycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            }
            else {
                raycastProgram = _insideRaycastPrograms[raycaster].get();
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            }
        }
        else {
            raycastProgram = _raycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
            }
            else {
                raycastProgram = _raycastPrograms[raycaster].get();
                raycastProgram->activate();
            }
        }

        if (raycastProgram) {
            raycastProgram->setUniform("rayCastSteps", raycaster->maxSteps());

            raycaster->preRaycast(_raycastData[raycaster], *raycastProgram);

            ghoul::opengl::TextureUnit exitColorTextureUnit;
            exitColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
            raycastProgram->setUniform("exitColorTexture", exitColorTextureUnit);

            ghoul::opengl::TextureUnit exitDepthTextureUnit;
            exitDepthTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _exitDepthTexture);
            raycastProgram->setUniform("exitDepthTexture", exitDepthTextureUnit);

            ghoul::opengl::TextureUnit mainDepthTextureUnit;
            mainDepthTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _gBuffers.depthTexture);
            raycastProgram->setUniform("mainDepthTexture", mainDepthTextureUnit);

            if (raycaster->downscaleRender() < 1.f) {
                const float scaleDown = raycaster->downscaleRender();
                raycastProgram->setUniform(
                    "windowSize",
                    glm::vec2(_resolution.x * scaleDown, _resolution.y * scaleDown)
                );
            }
            else {
                raycastProgram->setUniform(
                    "windowSize",
                    static_cast<glm::vec2>(_resolution)
                );
            }

            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);
            if (isCameraInside) {
                glBindVertexArray(_screenQuad);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);
            }
            else {
                raycaster->renderEntryPoints(raycasterTask.renderData, *raycastProgram);
            }
            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            raycaster->postRaycast(_raycastData[raycaster], *raycastProgram);
            raycastProgram->deactivate();
        }
        else {
            LWARNING("Raycaster is not attached when trying to perform raycaster task");
        }

        if (raycaster->downscaleRender() < 1.f) {
            global::renderEngine->openglStateCache().setViewportState(
                glm::value_ptr(viewport)
            );
            glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _gBuffers.framebuffer);
            writeDownscaledVolume(viewport);
        }
    }
}

void FramebufferRenderer::performDeferredTasks(
                                             const std::vector<DeferredcasterTask>& tasks,
                                                               const glm::ivec4& viewport)
{
    ZoneScoped;

    for (const DeferredcasterTask& deferredcasterTask : tasks) {
        TracyGpuZone("Deferredcaster");

        Deferredcaster* deferredcaster = deferredcasterTask.deferredcaster;

        ghoul::opengl::ProgramObject* deferredcastProgram =
            _deferredcastPrograms[deferredcaster].get();

        if (deferredcastProgram) {
            _pingPongIndex = _pingPongIndex == 0 ? 1 : 0;
            const int fromIndex = _pingPongIndex == 0 ? 1 : 0;
            glDrawBuffers(1, &ColorAttachmentArray[_pingPongIndex]);
            glDisablei(GL_BLEND, 0);
            glDisablei(GL_BLEND, 1);

            deferredcastProgram->activate();

            // adding G-Buffer
            ghoul::opengl::TextureUnit mainDColorTextureUnit;
            mainDColorTextureUnit.activate();
            glBindTexture(
                GL_TEXTURE_2D,
                _pingPongBuffers.colorTexture[fromIndex]
            );
            deferredcastProgram->setUniform(
                "mainColorTexture",
                mainDColorTextureUnit
            );

            deferredcastProgram->setUniform(
                "viewport",
                static_cast<float>(viewport[0]),
                static_cast<float>(viewport[1]),
                static_cast<float>(viewport[2]),
                static_cast<float>(viewport[3])
            );
            deferredcastProgram->setUniform("resolution", glm::vec2(_resolution));


            ghoul::opengl::TextureUnit mainPositionTextureUnit;
            mainPositionTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _gBuffers.positionTexture);
            deferredcastProgram->setUniform(
                "mainPositionTexture",
                mainPositionTextureUnit
            );

            ghoul::opengl::TextureUnit mainNormalTextureUnit;
            mainNormalTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _gBuffers.normalTexture);
            deferredcastProgram->setUniform(
                "mainNormalTexture",
                mainNormalTextureUnit
            );

            deferredcaster->preRaycast(
                deferredcasterTask.renderData,
                _deferredcastData[deferredcaster],
                *deferredcastProgram
            );

            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            deferredcaster->postRaycast(
                deferredcasterTask.renderData,
                _deferredcastData[deferredcaster],
                *deferredcastProgram
            );

            deferredcastProgram->deactivate();
        }
        else {
            LWARNING(
                "Deferredcaster is not attached when trying to perform deferred task"
            );
        }
    }
}

void FramebufferRenderer::setResolution(glm::ivec2 res) {
    _resolution = std::move(res);
    _dirtyResolution = true;
}

void FramebufferRenderer::setDisableHDR(bool disable) {
    _disableHDR = disable;
}

void FramebufferRenderer::setHDRExposure(float hdrExposure) {
    ghoul_assert(hdrExposure > 0.f, "HDR exposure must be greater than zero");
    _hdrExposure = hdrExposure;
    updateRendererData();
}

void FramebufferRenderer::setGamma(float gamma) {
    ghoul_assert(gamma > 0.f, "Gamma value must be greater than zero");
    _gamma = gamma;
}

void FramebufferRenderer::setHueValueSaturation(float hue, float value, float saturation)
{
    _hue = hue;
    _value = value;
    _saturation = saturation;
}

void FramebufferRenderer::enableFXAA(bool enable) {
    _enableFXAA = enable;
}

void FramebufferRenderer::updateRendererData() {
    ZoneScoped;

    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));
    dict.setValue("hdrExposure", std::to_string(_hdrExposure));
    dict.setValue("disableHDR", std::to_string(_disableHDR));
    _rendererData = dict;
    global::renderEngine->setRendererData(dict);
}

} // namespace openspace
