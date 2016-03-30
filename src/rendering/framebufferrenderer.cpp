/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <string>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/rendering/raycastermanager.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/textureunit.h>
#include <vector>

#include <ghoul/opengl/programobject.h>

namespace {
	const std::string _loggerCat = "FramebufferRenderer";
    const std::string ExitFragmentShaderPath = "${SHADERS}/framebuffer/exitframebuffer.frag";
    const std::string RaycastFragmentShaderPath = "${SHADERS}/framebuffer/raycastframebuffer.frag";
    const std::string RenderFragmentShaderPath = "${SHADERS}/framebuffer/renderframebuffer.frag";
}

namespace openspace {

FramebufferRenderer::FramebufferRenderer()
    : _camera(nullptr)
    , _scene(nullptr)
    , _resolution(glm::vec2(0)) {
}

FramebufferRenderer::~FramebufferRenderer() {}

void FramebufferRenderer::initialize() {
    LINFO("Initializing FramebufferRenderer");

    const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = {
        //	  x      y     s     t
        -size, -size, 0.0f, 1.0f,
        size,	size, 0.0f, 1.0f,
        -size,  size, 0.0f, 1.0f,
        -size, -size, 0.0f, 1.0f,
        size, -size, 0.0f, 1.0f,
        size,	size, 0.0f, 1.0f
    };
    
    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, reinterpret_cast<void*>(0));
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

    updateResolution();
    updateRendererData();
    updateRaycastData();

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture, 0);

    glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _exitColorTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, _exitDepthTexture, 0);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

    try {
        _resolveProgram = ghoul::opengl::ProgramObject::Build("Framebuffer Resolve",
            "${SHADERS}/framebuffer/resolveframebuffer.vert",
            "${SHADERS}/framebuffer/resolveframebuffer.frag");
    } catch (ghoul::RuntimeError e) {
        LERROR(e.message);
    }

    OsEng.renderEngine().raycasterManager().addListener(*this);

    _nAaSamples = OsEng.windowWrapper().currentNumberOfAaSamples();
    if (_nAaSamples > 8) {
        LERROR("Framebuffer renderer does not support more than 8 MSAA samples.");
        _nAaSamples = 8;
    }
}

void FramebufferRenderer::deinitialize() {
    LINFO("Deinitializing FramebufferRenderer");

    glDeleteFramebuffers(1, &_mainFramebuffer);
    glDeleteFramebuffers(1, &_exitFramebuffer);

    glDeleteTextures(1, &_mainColorTexture);
    glDeleteTextures(1, &_mainDepthTexture);
    glDeleteTextures(1, &_exitColorTexture);
    glDeleteTextures(1, &_exitDepthTexture);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    OsEng.renderEngine().raycasterManager().removeListener(*this);
}

void FramebufferRenderer::raycastersChanged(VolumeRaycaster& raycaster, bool attached) {
    (void) raycaster;
    (void) attached;
    _dirtyRaycastData = true;
}

void FramebufferRenderer::update() {
    if (_dirtyResolution) {
        updateResolution();
    }

    if (_dirtyRaycastData) {
        updateRaycastData();
    }

    // If the resolve dictionary changed (or a file changed on disk)
    // then rebuild the resolve program.
    if (_resolveProgram->isDirty()) {
        try {
            _resolveProgram->rebuildFromFile();
        } catch (ghoul::RuntimeError& error) {
            LERROR(error.message);
        }
    }

    for (auto &program : _exitPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (ghoul::RuntimeError e) {
                LERROR(e.message);
            }
        }
    }

    for (auto &program : _raycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (ghoul::RuntimeError e) {
                LERROR(e.message);
            }
        }
    }
}

void FramebufferRenderer::updateResolution() {
    int nSamples = OsEng.windowWrapper().currentNumberOfAaSamples();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        nSamples,
        GL_RGBA,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        nSamples,
        GL_DEPTH_COMPONENT32F,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        true);

    glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        GLsizei(_resolution.x),
        GLsizei(_resolution.y),
        0,
        GL_RGBA,
        GL_BYTE,
        nullptr);

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

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
        nullptr);

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}

void FramebufferRenderer::updateRaycastData() {
    _raycastData.clear();
    _exitPrograms.clear();
    _raycastPrograms.clear();

    const std::vector<VolumeRaycaster*>& raycasters = OsEng.renderEngine().raycasterManager().raycasters();
    int nextId = 0;
    for (auto &raycaster : raycasters) {
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
        if (helperPath != "") {
            helpersDict.setValue("0", helperPath);
        }
        dict.setValue("helperPaths", helpersDict);
        dict.setValue("raycastPath", raycaster->getRaycastPath());

        _raycastData[raycaster] = data;

        try {
            _exitPrograms[raycaster] = ghoul::opengl::ProgramObject::Build("Volume " + std::to_string(data.id) + " exit", vsPath, ExitFragmentShaderPath, dict);
        }
        catch (ghoul::RuntimeError e) {
            LERROR(e.message);
        }
        try {
            _raycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build("Volume " + std::to_string(data.id) + " raycast", vsPath, RaycastFragmentShaderPath, dict);
        } catch (ghoul::RuntimeError e) {
            LERROR(e.message);
        }
    }
    _dirtyRaycastData = false;
}

void FramebufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


    if (_scene == nullptr) return;
    if (_camera == nullptr) return;

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    RenderData data = { *_camera, psc(), doPerformanceMeasurements };
    RendererTasks tasks;

    // Capture standard fbo
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // bind new fbo A with color and depth buffer.
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

        ghoul::opengl::ProgramObject* raycastProgram = _raycastPrograms[raycaster].get();
        if (raycastProgram) {
            raycastProgram->activate();
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

            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);
            raycaster->renderEntryPoints(raycasterTask.renderData, *raycastProgram);
            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            raycaster->postRaycast(_raycastData[raycaster], *raycastProgram);
            raycastProgram->deactivate();
        } else {
            LWARNING("Raycaster is not attached when trying to perform raycaster task");
        }
    }

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

    _resolveProgram->deactivate();
}

void FramebufferRenderer::setScene(Scene* scene) {
    _scene = scene;
}

void FramebufferRenderer::setCamera(Camera* camera) {
    _camera = camera;
}

void FramebufferRenderer::setResolution(glm::ivec2 res) {
    _resolution = res;
}

void FramebufferRenderer::updateRendererData() {
    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", RenderFragmentShaderPath);
    dict.setValue("windowWidth", _resolution.x);
    dict.setValue("windowHeight", _resolution.y);

    _rendererData = dict;

    OsEng.renderEngine().setRendererData(dict);
}


}

