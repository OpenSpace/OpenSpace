/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/performance/performancemeasurement.h>
#include <openspace/rendering/deferredcaster.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <string>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "FramebufferRenderer";

    constexpr const std::array<const char*, 7> HDRUniformNames = {
        "hdrFeedingTexture", "blackoutFactor", "hdrExposure", "gamma", 
        "Hue", "Saturation", "Value"
    };

    constexpr const std::array<const char*, 2> FXAAUniformNames = {
        "renderedTexture", "inverseScreenSize"
    };

    constexpr const char* ExitFragmentShaderPath =
        "${SHADERS}/framebuffer/exitframebuffer.frag";
    constexpr const char* RaycastFragmentShaderPath =
        "${SHADERS}/framebuffer/raycastframebuffer.frag";
    constexpr const char* GetEntryInsidePath = "${SHADERS}/framebuffer/inside.glsl";
    constexpr const char* GetEntryOutsidePath = "${SHADERS}/framebuffer/outside.glsl";
    constexpr const char* RenderFragmentShaderPath =
        "${SHADERS}/framebuffer/renderframebuffer.frag";

    const GLenum ColorAttachment0Array[1] = {
        GL_COLOR_ATTACHMENT0
    };

    const GLenum ColorAttachment1Array[1] = {
       GL_COLOR_ATTACHMENT1
    };

    const GLenum ColorAttachment01Array[2] = {
       GL_COLOR_ATTACHMENT0,
       GL_COLOR_ATTACHMENT1
    };

    const GLenum ColorAttachment03Array[2] = {
       GL_COLOR_ATTACHMENT0,
       GL_COLOR_ATTACHMENT3
    };

    const GLenum ColorAttachment012Array[3] = {
       GL_COLOR_ATTACHMENT0,
       GL_COLOR_ATTACHMENT1,
       GL_COLOR_ATTACHMENT2
    };

    const GLenum ColorAttachment0123Array[4] = {
       GL_COLOR_ATTACHMENT0,
       GL_COLOR_ATTACHMENT1,
       GL_COLOR_ATTACHMENT2,
       GL_COLOR_ATTACHMENT3
    };

    void saveTextureToMemory(GLenum attachment, int width, int height,
                             std::vector<double>& memory)
    {
        memory.clear();
        memory.resize(width * height * 3);

        std::vector<float> tempMemory(width * height * 3);

        if (attachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(attachment);
            glReadPixels(0, 0, width, height, GL_RGB, GL_FLOAT, tempMemory.data());

        }
        else {
            glReadPixels(
                0,
                0,
                width,
                height,
                GL_DEPTH_COMPONENT,
                GL_FLOAT,
                tempMemory.data()
            );
        }

        for (int i = 0; i < width * height * 3; ++i) {
            memory[i] = static_cast<double>(tempMemory[i]);
        }
    }

} // namespace

namespace openspace {

void FramebufferRenderer::initialize() {
    LDEBUG("Initializing FramebufferRenderer");

    const GLfloat vertexData[] = {
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

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, nullptr);
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

    // HDR / Filtering Buffers
    glGenFramebuffers(1, &_hdrBuffers.hdrFilteringFramebuffer);
    glGenTextures(1, &_hdrBuffers.hdrFilteringTexture);

    // FXAA Buffers
    glGenFramebuffers(1, &_fxaaBuffers.fxaaFramebuffer);
    glGenTextures(1, &_fxaaBuffers.fxaaTexture);

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

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Exit framebuffer is not complete");
    }

    //===================================//
    //=====  HDR/Filtering Buffers  =====//
    //===================================//
    glBindFramebuffer(GL_FRAMEBUFFER, _hdrBuffers.hdrFilteringFramebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        _hdrBuffers.hdrFilteringTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("HDR/Filtering framebuffer is not complete");
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

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("FXAA framebuffer is not complete");
    }

    // JCC: Moved to here to avoid NVidia: "Program/shader state performance warning"
    // Building programs
    updateHDRAndFiltering();
    updateFXAA();
    updateDeferredcastData();

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

    global::raycasterManager.addListener(*this);
    global::deferredcasterManager.addListener(*this);

    // Default GL State for Blending
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void FramebufferRenderer::deinitialize() {
    LINFO("Deinitializing FramebufferRenderer");

    glDeleteFramebuffers(1, &_gBuffers.framebuffer);
    glDeleteFramebuffers(1, &_exitFramebuffer);
    glDeleteFramebuffers(1, &_hdrBuffers.hdrFilteringFramebuffer);
    glDeleteFramebuffers(1, &_fxaaBuffers.fxaaFramebuffer);
    glDeleteFramebuffers(1, &_pingPongBuffers.framebuffer);

    glDeleteTextures(1, &_gBuffers.colorTexture);
    glDeleteTextures(1, &_gBuffers.depthTexture);

    glDeleteTextures(1, &_hdrBuffers.hdrFilteringTexture);
    glDeleteTextures(1, &_fxaaBuffers.fxaaTexture);
    glDeleteTextures(1, &_gBuffers.positionTexture);
    glDeleteTextures(1, &_gBuffers.normalTexture);
    
    glDeleteTextures(1, &_pingPongBuffers.colorTexture[1]);

    glDeleteTextures(1, &_exitColorTexture);
    glDeleteTextures(1, &_exitDepthTexture);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    global::raycasterManager.removeListener(*this);
    global::deferredcasterManager.removeListener(*this);
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

void FramebufferRenderer::applyTMO(float blackoutFactor) {
    const bool doPerformanceMeasurements = global::performanceManager.isEnabled();
    std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
    
    if (doPerformanceMeasurements) {
        perfInternal = std::make_unique<performance::PerformanceMeasurement>(
            "FramebufferRenderer::render::TMO"
            );
    }
    _hdrFilteringProgram->activate();

    ghoul::opengl::TextureUnit hdrFeedingTextureUnit;
    hdrFeedingTextureUnit.activate();
    glBindTexture(
        GL_TEXTURE_2D,
        _pingPongBuffers.colorTexture[_pingPongIndex]
    );
    
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

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _hdrFilteringProgram->deactivate();
}

void FramebufferRenderer::applyFXAA() {
    const bool doPerformanceMeasurements = global::performanceManager.isEnabled();
    std::unique_ptr<performance::PerformanceMeasurement> perfInternal;

    if (doPerformanceMeasurements) {
        perfInternal = std::make_unique<performance::PerformanceMeasurement>(
            "FramebufferRenderer::render::FXAA"
            );
    }

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

    glm::vec2 inverseScreenSize(1.f/_resolution.x, 1.f/_resolution.y);
    _fxaaProgram->setUniform(_fxaaUniformCache.inverseScreenSize, inverseScreenSize);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _fxaaProgram->deactivate();
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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // HDR / Filtering
    glBindTexture(GL_TEXTURE_2D, _hdrBuffers.hdrFilteringTexture);
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
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
        GL_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    _dirtyResolution = false;
}

void FramebufferRenderer::updateRaycastData() {
    _raycastData.clear();
    _exitPrograms.clear();
    _raycastPrograms.clear();
    _insideRaycastPrograms.clear();

    const std::vector<VolumeRaycaster*>& raycasters =
        global::raycasterManager.raycasters();

    int nextId = 0;
    for (VolumeRaycaster* raycaster : raycasters) {
        RaycastData data = { nextId++, "Helper" };

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
                "Volume " + std::to_string(data.id) + " exit",
                absPath(vsPath),
                absPath(ExitFragmentShaderPath),
                dict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary outsideDict = dict;
            outsideDict.setValue("getEntryPath", GetEntryOutsidePath);
            _raycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " raycast",
                absPath(vsPath),
                absPath(RaycastFragmentShaderPath),
                outsideDict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary insideDict = dict;
            insideDict.setValue("getEntryPath", GetEntryInsidePath);
            _insideRaycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " inside raycast",
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
        global::deferredcasterManager.deferredcasters();
    int nextId = 0;
    for (Deferredcaster* caster : deferredcasters) {
        DeferredcastData data = { nextId++, "HELPER" };

        std::string vsPath = caster->deferredcastVSPath();
        std::string fsPath = caster->deferredcastFSPath();
        std::string deferredShaderPath = caster->deferredcastPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        //dict.setValue("fragmentPath", fsPath);
        dict.setValue("id", data.id);
        std::string helperPath = caster->helperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", helperPath);
        }
        dict.setValue("helperPaths", helpersDict);

        _deferredcastData[caster] = data;

        try {
            _deferredcastPrograms[caster] = ghoul::opengl::ProgramObject::Build(
                "Deferred " + std::to_string(data.id) + " raycast",
                absPath(vsPath),
                absPath(deferredShaderPath),
                dict
            );

            _deferredcastPrograms[caster]->setIgnoreSubroutineUniformLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );
            _deferredcastPrograms[caster]->setIgnoreUniformLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );

            caster->initializeCachedVariables(*_deferredcastPrograms[caster]);
        }
        catch (ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    _dirtyDeferredcastData = false;
}


void FramebufferRenderer::updateHDRAndFiltering() {
    _hdrFilteringProgram = ghoul::opengl::ProgramObject::Build(
        "HDR and Filtering Program",
        absPath("${SHADERS}/framebuffer/hdrAndFiltering.vert"),
        absPath("${SHADERS}/framebuffer/hdrAndfiltering.frag")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_hdrFilteringProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_hdrFilteringProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateFXAA() {
    _fxaaProgram = ghoul::opengl::ProgramObject::Build(
        "FXAA Program",
        absPath("${SHADERS}/framebuffer/fxaa.vert"),
        absPath("${SHADERS}/framebuffer/fxaa.frag")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_fxaaProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_fxaaProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::render(Scene* scene, Camera* camera, float blackoutFactor) {
    // Set OpenGL default rendering state
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);
    glEnablei(GL_BLEND, 0);
    glDisablei(GL_BLEND, 1);
    glDisablei(GL_BLEND, 2);
    
    glClampColor(GL_CLAMP_READ_COLOR, GL_FALSE);

    glEnable(GL_DEPTH_TEST);

    _pingPongIndex = 0;
    
    // Measurements cache variable
    const bool doPerformanceMeasurements = global::performanceManager.isEnabled();

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (doPerformanceMeasurements) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "FramebufferRenderer::render"
        );
    }

    if (!scene || !camera) {
        return;
    }    

    // deferred g-buffer
    glBindFramebuffer(GL_FRAMEBUFFER, _gBuffers.framebuffer);
    glDrawBuffers(3, ColorAttachment012Array);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    Time time = global::timeManager.time();

    RenderData data = {
        *camera,
        std::move(time),
        doPerformanceMeasurements,
        0,
        {}
    };
    RendererTasks tasks;

    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Background);
    scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Opaque);
    scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Transparent);
    scene->render(data, tasks);

    // Run Volume Tasks
    {
        std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
        if (doPerformanceMeasurements) {
            perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                "FramebufferRenderer::render::raycasterTasks"
            );
        }
        performRaycasterTasks(tasks.raycasterTasks);
    }

    if (!tasks.deferredcasterTasks.empty()) {
        // We use ping pong rendering in order to be able to
        // render to the same final buffer, multiple 
        // deferred tasks at same time (e.g. more than 1 ATM being seen at once)
        glBindFramebuffer(GL_FRAMEBUFFER, _pingPongBuffers.framebuffer);
        glDrawBuffers(1, &ColorAttachment01Array[_pingPongIndex]);

        std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
        if (doPerformanceMeasurements) {
            perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                "FramebufferRenderer::render::deferredTasks"
                );
        }
        performDeferredTasks(tasks.deferredcasterTasks);
    }
    
    glDrawBuffers(1, &ColorAttachment01Array[_pingPongIndex]);
    glEnablei(GL_BLEND, 0);

    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Overlay);
    scene->render(data, tasks);

    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    // Disabling depth test for filtering and hdr
    glDisable(GL_DEPTH_TEST);
    
    if (_enableFXAA) {
        glBindFramebuffer(GL_FRAMEBUFFER, _fxaaBuffers.fxaaFramebuffer);
    }
    else {
        // When applying the TMO, the result is saved to the default FBO to be displayed
        // by the Operating System. Also, the resolve procedure is executed in this step.
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    }
    
    glViewport(0, 0, _resolution.x, _resolution.y);
    
    // Apply the selected TMO on the results and resolve the result for the default FBO
    applyTMO(blackoutFactor);

    if (_enableFXAA) {
        glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
        applyFXAA();
    }
    
}

void FramebufferRenderer::performRaycasterTasks(const std::vector<RaycasterTask>& tasks) {
    for (const RaycasterTask& raycasterTask : tasks) {
        VolumeRaycaster* raycaster = raycasterTask.raycaster;

        glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        ghoul::opengl::ProgramObject* exitProgram = _exitPrograms[raycaster].get();
        if (exitProgram) {
            exitProgram->activate();
            raycaster->renderExitPoints(raycasterTask.renderData, *exitProgram);
            exitProgram->deactivate();
        }

        glBindFramebuffer(GL_FRAMEBUFFER, _gBuffers.framebuffer);
        glm::vec3 cameraPosition;
        bool isCameraInside = raycaster->isCameraInside(
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

            raycastProgram->setUniform("windowSize", static_cast<glm::vec2>(_resolution));

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
    }
}

void FramebufferRenderer::performDeferredTasks(
                                             const std::vector<DeferredcasterTask>& tasks
                                              )
{   
    for (const DeferredcasterTask& deferredcasterTask : tasks) {
        Deferredcaster* deferredcaster = deferredcasterTask.deferredcaster;

        ghoul::opengl::ProgramObject* deferredcastProgram = nullptr;

        if (deferredcastProgram != _deferredcastPrograms[deferredcaster].get()
            || deferredcastProgram == nullptr)
        {
            deferredcastProgram = _deferredcastPrograms[deferredcaster].get();
        }

        if (deferredcastProgram) {
            _pingPongIndex = _pingPongIndex == 0 ? 1 : 0;
            int fromIndex = _pingPongIndex == 0 ? 1 : 0;
            glDrawBuffers(1, &ColorAttachment01Array[_pingPongIndex]);
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
    _disableHDR = std::move(disable);
}

void FramebufferRenderer::setHDRExposure(float hdrExposure) {
    ghoul_assert(hdrExposure > 0.f, "HDR exposure must be greater than zero");
    _hdrExposure = std::move(hdrExposure);
    updateRendererData();
}

void FramebufferRenderer::setGamma(float gamma) {
    ghoul_assert(gamma > 0.f, "Gamma value must be greater than zero");
    _gamma = std::move(gamma);
}

void FramebufferRenderer::setHue(float hue) {
    _hue = std::move(hue);
}

void FramebufferRenderer::setValue(float value) {
    _value = std::move(value);
}

void FramebufferRenderer::setSaturation(float sat) {
    _saturation = std::move(sat);
}

void FramebufferRenderer::enableFXAA(bool enable) {
    _enableFXAA = std::move(enable);
}

void FramebufferRenderer::updateRendererData() {
    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));
    dict.setValue("hdrExposure", std::to_string(_hdrExposure));
    dict.setValue("disableHDR", std::to_string(_disableHDR));
    _rendererData = dict;
    global::renderEngine.setRendererData(dict);
}

} // namespace openspace
