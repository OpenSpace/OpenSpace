﻿/*****************************************************************************************
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

#include <openspace/rendering/framebufferrenderer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/performance/performancemeasurement.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/deferredcaster.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/rendering/raycastermanager.h>

#include <openspace/performance/performancemeasurement.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

#include <string>
#include <vector>

//#define _OLD_RENDERING_
#define _NEW_RENDERING_

namespace {
    const char* _loggerCat = "FramebufferRenderer";
    const char* ExitFragmentShaderPath = "${SHADERS}/framebuffer/exitframebuffer.frag";
    const char* RaycastFragmentShaderPath = "${SHADERS}/framebuffer/raycastframebuffer.frag";
    const char* GetEntryInsidePath = "${SHADERS}/framebuffer/inside.glsl";
    const char* GetEntryOutsidePath = "${SHADERS}/framebuffer/outside.glsl";
    const char* RenderFragmentShaderPath = "${SHADERS}/framebuffer/renderframebuffer.frag";
} // namespace

namespace openspace {

FramebufferRenderer::FramebufferRenderer()
    : _camera(nullptr)
    , _scene(nullptr)
    , _resolution(glm::vec2(0))
    , _hdrExposure(0.4f)
    , _hdrBackground(2.8f)
    , _gamma(2.2f)
{}

FramebufferRenderer::~FramebufferRenderer() {}

void FramebufferRenderer::initialize() {
    LINFO("Initializing FramebufferRenderer");

    const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = {
        //      x      y     s     t
        -size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f,
        -size,  size, 0.0f, 1.0f,
        -size, -size, 0.0f, 1.0f,
        size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f
    };
    
    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 4,
        reinterpret_cast<void*>(0)
    );
    glEnableVertexAttribArray(0);

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    // Main framebuffer
    glGenTextures(1, &_mainColorTexture);
    glGenTextures(1, &_mainDepthTexture);
    glGenFramebuffers(1, &_mainFramebuffer);

    // Exit framebuffer
    glGenTextures(1, &_exitColorTexture);
    glGenTextures(1, &_exitDepthTexture);
    glGenFramebuffers(1, &_exitFramebuffer);

    // Deferred framebuffer
    glGenTextures(1, &_deferredColorTexture);
    glGenTextures(1, &_mainOtherDataTexture);
    glGenTextures(1, &_mainPositionTexture);
    glGenTextures(1, &_mainNormalTexture);
    glGenFramebuffers(1, &_deferredFramebuffer);

    updateResolution();
    updateRendererData();
    updateRaycastData();
    
    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture, 0);
    // G-buffer
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D_MULTISAMPLE, _mainOtherDataTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT3, GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture, 0);
        
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _exitColorTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        _exitDepthTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Exit framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, _deferredFramebuffer);
#ifdef _OLD_RENDERING_
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, _deferredColorTexture, 0);
#endif
#ifdef _NEW_RENDERING_
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _deferredColorTexture, 0);
#endif

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Deferred framebuffer is not complete");
    }

    // JCC: Moved to here to avoid NVidia: "Program/shader state performance warning"
    updateHDRData();
    updateDeferredcastData();

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

    try {
        _resolveProgram = ghoul::opengl::ProgramObject::Build(
            "Framebuffer Resolve",
            "${SHADERS}/framebuffer/resolveframebuffer.vert",
            "${SHADERS}/framebuffer/resolveframebuffer.frag"
        );
    } catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    OsEng.renderEngine().raycasterManager().addListener(*this);
    OsEng.renderEngine().deferredcasterManager().addListener(*this);
}

void FramebufferRenderer::deinitialize() {
    LINFO("Deinitializing FramebufferRenderer");

    glDeleteFramebuffers(1, &_mainFramebuffer);
    glDeleteFramebuffers(1, &_exitFramebuffer);
    glDeleteFramebuffers(1, &_deferredFramebuffer);

    glDeleteTextures(1, &_mainColorTexture);
    glDeleteTextures(1, &_mainDepthTexture);

    // DEBUG: deferred g-buffer
    glDeleteTextures(1, &_deferredColorTexture);
    glDeleteTextures(1, &_mainOtherDataTexture);
    glDeleteTextures(1, &_mainPositionTexture);
    glDeleteTextures(1, &_mainNormalTexture);

    glDeleteTextures(1, &_exitColorTexture);
    glDeleteTextures(1, &_exitDepthTexture);


    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    OsEng.renderEngine().raycasterManager().removeListener(*this);
    OsEng.renderEngine().deferredcasterManager().removeListener(*this);
}

void FramebufferRenderer::raycastersChanged(VolumeRaycaster&, bool) {
    _dirtyRaycastData = true;
}

void FramebufferRenderer::deferredcastersChanged(Deferredcaster& deferredcaster, bool attached) {
    (void) deferredcaster;
    (void) attached;
    _dirtyDeferredcastData = true;
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

    // If the resolve dictionary changed (or a file changed on disk)
    // then rebuild the resolve program.
    if (_hdrBackGroundProgram && _hdrBackGroundProgram->isDirty()) {
        try {
            _hdrBackGroundProgram->rebuildFromFile();
        } catch (const ghoul::RuntimeError& error) {
            LERRORC(error.component, error.message);
        }
    }

    if (_resolveProgram->isDirty()) {
        try {
            _resolveProgram->rebuildFromFile();
        }
        catch (const ghoul::RuntimeError& error) {
            LERRORC(error.component, error.message);
        }
    }

    for (auto& program : _exitPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (auto& program : _raycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (auto& program : _insideRaycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (auto &program : _deferredcastPrograms) {
        if (program.second && program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (ghoul::RuntimeError e) {
                LERROR(e.message);
            }
        }
    }
}

void FramebufferRenderer::updateResolution() {
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true
    );

    // G-buffer
#ifdef _OLD_RENDERING_
    
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _deferredColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);    
#endif
#ifdef _NEW_RENDERING_
    glBindTexture(GL_TEXTURE_2D, _deferredColorTexture);

    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);    
#endif

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainOtherDataTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_DEPTH_COMPONENT32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true
    );

    glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
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
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
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
        OsEng.renderEngine().raycasterManager().raycasters();
    int nextId = 0;
    for (auto& raycaster : raycasters) {
        RaycastData data;
        data.id = nextId++;
        data.namespaceName = "HELPER";

        std::string vsPath = raycaster->getBoundsVsPath();
        std::string fsPath = raycaster->getBoundsFsPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        dict.setValue("fragmentPath", fsPath);
        dict.setValue("id", data.id);
        std::string helperPath = raycaster->getHelperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", helperPath);
        }
        dict.setValue("helperPaths", helpersDict);
        dict.setValue("raycastPath", raycaster->getRaycastPath());

        _raycastData[raycaster] = data;

        try {
            _exitPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " exit",
                vsPath,
                ExitFragmentShaderPath,
                dict
            );
        } catch (ghoul::RuntimeError e) {
            LERROR(e.message);
        }
        try {
            ghoul::Dictionary outsideDict = dict;
            outsideDict.setValue("getEntryPath", GetEntryOutsidePath);
            _raycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " raycast",
                vsPath,
                RaycastFragmentShaderPath,
                outsideDict
            );
        } catch (ghoul::RuntimeError e) {
            LERROR(e.message);
        }
        try {
            ghoul::Dictionary insideDict = dict;
            insideDict.setValue("getEntryPath", GetEntryInsidePath);
            _insideRaycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " inside raycast",
                "${SHADERS}/framebuffer/resolveframebuffer.vert",
                RaycastFragmentShaderPath,
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
    
    const std::vector<Deferredcaster*>& deferredcasters = OsEng.renderEngine().deferredcasterManager().deferredcasters();
    int nextId = 0;
    for (auto &deferredcaster : deferredcasters) {
        DeferredcastData data;
        data.id = nextId++;
        data.namespaceName = "HELPER";

        std::string vsPath = deferredcaster->getDeferredcastVSPath();
        std::string fsPath = deferredcaster->getDeferredcastFSPath();
        std::string deferredShaderPath = deferredcaster->getDeferredcastPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        //dict.setValue("fragmentPath", fsPath);
        dict.setValue("id", data.id);
        std::string helperPath = deferredcaster->getHelperPath();
        ghoul::Dictionary helpersDict;
        if (helperPath != "") {
            helpersDict.setValue("0", helperPath);
        }
        dict.setValue("helperPaths", helpersDict);
        //dict.setValue("deferredcastPath", deferredcaster->getDeferredcastPath());

        _deferredcastData[deferredcaster] = data;

        try {
            ghoul::Dictionary deferredDict = dict;
            //deferredDict.setValue("getEntryPath", GetEntryOutsidePath);
            _deferredcastPrograms[deferredcaster] = ghoul::opengl::ProgramObject::Build(
                "Deferred " + std::to_string(data.id) + " raycast",
                vsPath,
                deferredShaderPath,
                deferredDict);
            using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
            _deferredcastPrograms[deferredcaster]->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
            _deferredcastPrograms[deferredcaster]->setIgnoreUniformLocationError(IgnoreError::Yes);
        }
        catch (ghoul::RuntimeError e) {
            LERROR(e.message);
        }

    }
    _dirtyDeferredcastData = false;
}

void FramebufferRenderer::updateHDRData() {
    try {
        _hdrBackGroundProgram = ghoul::opengl::ProgramObject::Build(
            "HDR Background Control",
            "${SHADERS}/framebuffer/hdrBackground.vert",
            "${SHADERS}/framebuffer/hdrBackground.frag"
        );
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _hdrBackGroundProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _hdrBackGroundProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }
}

void FramebufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {
    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (doPerformanceMeasurements) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "FramebufferRenderer::render",
            OsEng.renderEngine().performanceManager()
        );
    }
    
    if (!_scene || !_camera) {
        return;
    }

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    Time time = OsEng.timeManager().time();

    RenderData data = { *_camera, psc(), time, doPerformanceMeasurements, 0 };
    RendererTasks tasks;

    // Capture standard fbo
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    // deferred g-buffer
    GLenum textureBuffers[4] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3 };
    glDrawBuffers(4, textureBuffers);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Background);
    _scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Opaque);
    _scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Transparent);
    _scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Overlay);
    _scene->render(data, tasks);

    for (const RaycasterTask& raycasterTask : tasks.raycasterTasks) {
        VolumeRaycaster* raycaster = raycasterTask.raycaster;

        glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        ghoul::opengl::ProgramObject* exitProgram = _exitPrograms[raycaster].get(); 
        if (exitProgram) {
            exitProgram->activate();
            raycaster->renderExitPoints(raycasterTask.renderData, *exitProgram);
            exitProgram->deactivate();
        }

        glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
        glm::vec3 cameraPosition;
        bool cameraIsInside = raycaster->cameraIsInside(
            raycasterTask.renderData,
            cameraPosition
        );
        ghoul::opengl::ProgramObject* raycastProgram = nullptr;

        if (cameraIsInside) {
            raycastProgram = _insideRaycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            } else {
                raycastProgram = _insideRaycastPrograms[raycaster].get();
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            }
        } else {
            raycastProgram = _raycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
            } else {
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
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
            raycastProgram->setUniform("mainDepthTexture", mainDepthTextureUnit);

            raycastProgram->setUniform("nAaSamples", _nAaSamples);
            raycastProgram->setUniform("windowSize", glm::vec2(_resolution));


            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);
            if (cameraIsInside) {
                glBindVertexArray(_screenQuad);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);
            } else {
                raycaster->renderEntryPoints(raycasterTask.renderData, *raycastProgram);
            }
            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            raycaster->postRaycast(_raycastData[raycaster], *raycastProgram);
            raycastProgram->deactivate();
        } else {
            LWARNING("Raycaster is not attached when trying to perform raycaster task");
        }
    }

    // g-buffer
    if (!tasks.deferredcasterTasks.empty()) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _deferredFramebuffer);
        GLenum dBuffer[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, dBuffer);
        glClear(GL_COLOR_BUFFER_BIT);

        // HDR Image Control and Resolve
        _hdrBackGroundProgram->activate();

        ghoul::opengl::TextureUnit mainColorTextureUnit;
        mainColorTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

        _hdrBackGroundProgram->setUniform("mainColorTexture", mainColorTextureUnit);
        _hdrBackGroundProgram->setUniform("nAaSamples", _nAaSamples);
        _hdrBackGroundProgram->setUniform("exposure", _hdrExposure);
        _hdrBackGroundProgram->setUniform("backgroundExposure", _hdrBackground);
        _hdrBackGroundProgram->setUniform("gamma", _gamma);

        glDisable(GL_DEPTH_TEST);
        glDepthMask(false);

        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        glDepthMask(true);
        glEnable(GL_DEPTH_TEST);

        _hdrBackGroundProgram->deactivate();        
    }
    
    for (const DeferredcasterTask& deferredcasterTask : tasks.deferredcasterTasks) {

        Deferredcaster* deferredcaster = deferredcasterTask.deferredcaster;

        ghoul::opengl::ProgramObject* deferredcastProgram = nullptr;

        if (deferredcastProgram != _deferredcastPrograms[deferredcaster].get() 
            || deferredcastProgram == nullptr) {
            deferredcastProgram = _deferredcastPrograms[deferredcaster].get();            
        }        

        if (deferredcastProgram) {
            
            deferredcastProgram->activate();

            // adding G-Buffer
            ghoul::opengl::TextureUnit mainDColorTextureUnit;
            mainDColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);
            deferredcastProgram->setUniform("mainColorTexture", mainDColorTextureUnit);

            ghoul::opengl::TextureUnit otherDataTextureUnit;
            otherDataTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainOtherDataTexture);
            deferredcastProgram->setUniform("otherDataTexture", otherDataTextureUnit);

            ghoul::opengl::TextureUnit mainPositionTextureUnit;
            mainPositionTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture);
            deferredcastProgram->setUniform("mainPositionTexture", mainPositionTextureUnit);

            ghoul::opengl::TextureUnit mainNormalTextureUnit;
            mainNormalTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture);
            deferredcastProgram->setUniform("mainNormalTexture", mainNormalTextureUnit);

       
            deferredcastProgram->setUniform("nAaSamples", _nAaSamples);
       
            deferredcaster->preRaycast(deferredcasterTask.renderData, 
                                       _deferredcastData[deferredcaster], 
                                       *deferredcastProgram);
            
            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);
            
            deferredcaster->postRaycast(deferredcasterTask.renderData,
                _deferredcastData[deferredcaster],
                *deferredcastProgram);

            deferredcastProgram->deactivate();                        

        } else {
            LWARNING("Deferredcaster is not attached when trying to perform deferred task");
        }
    }
    
#ifdef _NEW_RENDERING_    
    if (!tasks.deferredcasterTasks.empty()) {
        glBindFramebuffer(GL_READ_FRAMEBUFFER, _deferredFramebuffer);        
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, defaultFbo);
        GLenum dBuffer[] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, dBuffer);
        glReadBuffer(GL_COLOR_ATTACHMENT0);
        glBlitFramebuffer(0, 0, GLsizei(_resolution.x), GLsizei(_resolution.y),
            0, 0, GLsizei(_resolution.x), GLsizei(_resolution.y),
            GL_COLOR_BUFFER_BIT, GL_NEAREST);
        
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    } else {
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
        _resolveProgram->activate();

        ghoul::opengl::TextureUnit mainColorTextureUnit;
        mainColorTextureUnit.activate();

        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);
        _resolveProgram->setUniform("mainColorTexture", mainColorTextureUnit);
        _resolveProgram->setUniform("blackoutFactor", blackoutFactor);
        _resolveProgram->setUniform("nAaSamples", _nAaSamples);
        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        _resolveProgram->deactivate();
    }
#endif
#ifdef _OLD_RENDERING_    
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    _resolveProgram->activate();

    ghoul::opengl::TextureUnit mainColorTextureUnit;
    mainColorTextureUnit.activate();

    if (tasks.deferredcasterTasks.size()) {
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _deferredColorTexture);        
    }
    else {
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);        
    }
    _resolveProgram->setUniform("mainColorTexture", mainColorTextureUnit);
    _resolveProgram->setUniform("blackoutFactor", blackoutFactor);
    _resolveProgram->setUniform("nAaSamples", _nAaSamples);
    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _resolveProgram->deactivate();
#endif    

}

void FramebufferRenderer::setScene(Scene* scene) {
    _scene = scene;
}

void FramebufferRenderer::setCamera(Camera* camera) {
    _camera = camera;
}

void FramebufferRenderer::setResolution(glm::ivec2 res) {
    _resolution = res;
    _dirtyResolution = true;
}

void FramebufferRenderer::setNAaSamples(const int nAaSamples) {
    _nAaSamples = nAaSamples;
    if (_nAaSamples == 0) {
        _nAaSamples = 1;
    }
    if (_nAaSamples > 8) {
        LERROR("Framebuffer renderer does not support more than 8 MSAA samples.");
        _nAaSamples = 8;
    }
    _dirtyResolution = true;
}

void FramebufferRenderer::setHDRExposure(const float hdrExposure) {
    _hdrExposure = hdrExposure;
    if (_hdrExposure < 0.0f) {
        LERROR("HDR Exposure constant must be greater than zero.");
        _hdrExposure = 1.0f;
    }
}

void FramebufferRenderer::setHDRBackground(const float hdrBackground) {
    _hdrBackground = hdrBackground;
    if (_hdrBackground < 0.0f) {
        LERROR("HDR Background constant must be greater than zero.");
        _hdrBackground = 1.0f;    
    }
}

void FramebufferRenderer::setGamma(const float gamma) {
    _gamma = gamma;
    if (_gamma < 0.0f) {
        LERROR("Gamma value must be greater than zero.");
        _gamma = 2.2f;
    }
}

float FramebufferRenderer::hdrBackground() const {
    return _hdrBackground;
}

void FramebufferRenderer::updateRendererData() {
    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));

    _rendererData = dict;

    OsEng.renderEngine().setRendererData(dict);
}

} // namespace openspace
