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

#ifdef OPENSPACE_WITH_ABUFFER_RENDERER

#include <openspace/rendering/abufferrenderer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/performance/performancemeasurement.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/rendering/deferredcaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* _loggerCat = "ABufferRenderer";
    constexpr const char* BoundsFragmentShaderPath =
        "${SHADERS}/abuffer/boundsabuffer.frag";
    constexpr const char* RenderFragmentShaderPath =
        "${SHADERS}/abuffer/renderabuffer.frag";

    constexpr int MaxRaycasters = 32;
    constexpr int MaxLayers = 32;
    constexpr int MaxAverageLayers = 8;
} // namespace

namespace openspace {

void ABufferRenderer::initialize() {
    LINFO("Initializing ABufferRenderer");
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

    glGenTextures(1, &_anchorPointerTexture);
    glGenBuffers(1, &_anchorPointerTextureInitializer);
    glGenBuffers(1, &_atomicCounterBuffer);
    glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, _atomicCounterBuffer);
    glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), nullptr, GL_DYNAMIC_COPY);
    glGenBuffers(1, &_fragmentBuffer);
    glGenTextures(1, &_fragmentTexture);

    glGenTextures(1, &_mainColorTexture);
    glGenTextures(1, &_mainDepthTexture);
    glGenFramebuffers(1, &_mainFramebuffer);

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    updateResolution();
    updateRendererData();
    updateRaycastData();
    updateResolveDictionary();
    updateMSAASamplingPattern();

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainColorTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainDepthTexture,
        0
    );

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

    try {
        ghoul::Dictionary dict;
        dict.setValue("resolveData", _resolveDictionary);
        dict.setValue("rendererData", _rendererData);

        _resolveProgram = ghoul::opengl::ProgramObject::Build(
            "ABuffer Resolve",
            absPath("${SHADERS}/abuffer/resolveabuffer.vert"),
            absPath("${SHADERS}/abuffer/resolveabuffer.frag"),
            dict
        );
    } catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    global::raycasterManager.addListener(*this);
}

void ABufferRenderer::deinitialize() {
    LINFO("Deinitializing ABufferRenderer");
    glDeleteBuffers(1, &_fragmentBuffer);
    glDeleteTextures(1, &_fragmentTexture);

    glDeleteTextures(1, &_anchorPointerTexture);
    glDeleteBuffers(1, &_anchorPointerTextureInitializer);
    glDeleteBuffers(1, &_atomicCounterBuffer);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    global::raycasterManager.removeListener(*this);
}

void ABufferRenderer::raycastersChanged(VolumeRaycaster&, IsAttached) {
    _dirtyRaycastData = true;
}

void ABufferRenderer::update() {
    PerfMeasure("ABufferRenderer::update");

    // Make sure that the fragment buffer has the correct resoliution
    // according to the output render buffer size
    if (_dirtyResolution) {
        updateResolution();
        updateMSAASamplingPattern();
    }

    // Make sure that the renderengine gets the correct render data
    // to feed into all render programs.
    // This will trigger a recompilation of all the shader programs
    // involved in rendering geometries.
    if (_dirtyRendererData) {
        updateRendererData();
    }

    // Make sure that all raycaster data is up to date.
    if (_dirtyRaycastData) {
        updateRaycastData();
    }

    // Make sure that the resolve dictionary is up to date.
    // The resolve dictionary contains information for all
    // ray casters, including shader include paths.

    if (_dirtyResolveDictionary) {
        updateResolveDictionary();
        ghoul::Dictionary dict;
        dict.setValue("resolveData", _resolveDictionary);
        dict.setValue("rendererData", _rendererData);
        _resolveProgram->setDictionary(dict);
    }

    // If the resolve dictionary changed (or a file changed on disk)
    // then rebuild the resolve program.
    if (_resolveProgram->isDirty()) {
        try {
            _resolveProgram->rebuildFromFile();
        } catch (const ghoul::RuntimeError& error) {
            LERRORC(error.component, error.message);
        }
    }
    using K = VolumeRaycaster* const;
    using V = std::unique_ptr<ghoul::opengl::ProgramObject>;
    for (std::pair<K, V>& program : _boundsPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }
}

void ABufferRenderer::updateMSAASamplingPattern() {
    // @CLEANUP(abock): This should probably be merged with the same code from the
    //                  framebuffer renderer?
    LINFO("Updating MSAA Sampling Pattern");

    constexpr const int GridSize = 32;
    GLfloat step = 2.f / static_cast<GLfloat>(GridSize);
    GLfloat sizeX = -1.f;
    GLfloat sizeY = 1.f;

    constexpr const int NVertex = 4 * 6;
    // openPixelSizeVertexData
    GLfloat vertexData[GridSize * GridSize * NVertex];

    for (int y = 0; y < GridSize; ++y) {
        for (int x = 0; x < GridSize; ++x) {
            vertexData[y * GridSize * NVertex + x * NVertex] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 1] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 2] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 3] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 4] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 5] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 6] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 7] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 8] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 9] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 10] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 11] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 12] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 13] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 14] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 15] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 16] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 17] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 18] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 19] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 20] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 21] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 22] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 23] = 1.f;

            sizeX += step;
        }
        sizeX = -1.f;
        sizeY -= step;
    }

    GLuint pixelSizeQuadVAO = 0;
    glGenVertexArrays(1, &pixelSizeQuadVAO);
    glBindVertexArray(pixelSizeQuadVAO);

    GLuint pixelSizeQuadVBO = 0;
    glGenBuffers(1, &pixelSizeQuadVBO);
    glBindBuffer(GL_ARRAY_BUFFER, pixelSizeQuadVBO);

    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(GLfloat) * GridSize * GridSize * NVertex,
        vertexData,
        GL_STATIC_DRAW
    );

    // Position
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, nullptr);
    glEnableVertexAttribArray(0);

    // Saves current state
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    // Main framebuffer

    GLuint pixelSizeTexture = 0;
    glGenTextures(1, &pixelSizeTexture);
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, pixelSizeTexture);

    constexpr const GLsizei OnePixel = 1;
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        OnePixel,
        OnePixel,
        true
    );

    glViewport(0, 0, OnePixel, OnePixel);

    GLuint pixelSizeFramebuffer = 0;
    glGenFramebuffers(1, &pixelSizeFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, pixelSizeFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D_MULTISAMPLE,
        pixelSizeTexture,
        0
    );

    GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, textureBuffers);

    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("MSAA Sampling pattern framebuffer is not complete");
        return;
    }

    std::unique_ptr<ghoul::opengl::ProgramObject> pixelSizeProgram =
        ghoul::opengl::ProgramObject::Build(
            "OnePixel MSAA",
            absPath("${SHADERS}/framebuffer/pixelSizeMSAA.vert"),
            absPath("${SHADERS}/framebuffer/pixelSizeMSAA.frag")
        );

    pixelSizeProgram->activate();

    // Draw sub-pixel grid
    glEnable(GL_SAMPLE_SHADING);
    glBindVertexArray(pixelSizeQuadVAO);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(false);
    glDrawArrays(GL_TRIANGLES, 0, GridSize * GridSize * 6);
    glBindVertexArray(0);
    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_SAMPLE_SHADING);

    pixelSizeProgram->deactivate();

    // Now we render the Nx1 quad strip
    GLuint nOneStripFramebuffer = 0;
    GLuint nOneStripVAO = 0;
    GLuint nOneStripVBO = 0;
    GLuint nOneStripTexture = 0;

    sizeX = -1.f;
    step = 2.f / static_cast<GLfloat>(_nAaSamples);

    std::vector<GLfloat> nOneStripVertexData(_nAaSamples * (NVertex + 12));

    for (int x = 0; x < _nAaSamples; ++x) {
        nOneStripVertexData[x * (NVertex + 12)] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 1] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 2] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 3] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 4] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 5] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 6] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 7] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 8] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 9] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 10] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 11] = 1.f;

        nOneStripVertexData[x * (NVertex + 12) + 12] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 13] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 14] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 15] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 16] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 17] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 18] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 19] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 20] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 21] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 22] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 23] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 24] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 25] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 26] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 27] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 28] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 29] = 1.f;

        nOneStripVertexData[x * (NVertex + 12) + 30] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 31] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 32] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 33] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 34] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 35] = 1.f;

        sizeX += step;
    }

    glGenVertexArrays(1, &nOneStripVAO);
    glBindVertexArray(nOneStripVAO);
    glGenBuffers(1, &nOneStripVBO);
    glBindBuffer(GL_ARRAY_BUFFER, nOneStripVBO);
    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(GLfloat) * _nAaSamples * (NVertex + 12),
        nOneStripVertexData.data(),
        GL_STATIC_DRAW
    );

    // position
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        nullptr
    );
    glEnableVertexAttribArray(0);

    // texture coords
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 4)
    );
    glEnableVertexAttribArray(1);

    // fbo texture buffer
    glGenTextures(1, &nOneStripTexture);
    glBindTexture(GL_TEXTURE_2D, nOneStripTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _nAaSamples,
        OnePixel,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glGenFramebuffers(1, &nOneStripFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, nOneStripFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        nOneStripTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("nOneStrip framebuffer is not complete");
    }

    glViewport(0, 0, _nAaSamples, OnePixel);

    std::unique_ptr<ghoul::opengl::ProgramObject> nOneStripProgram =
        ghoul::opengl::ProgramObject::Build(
            "OneStrip MSAA",
            absPath("${SHADERS}/framebuffer/nOneStripMSAA.vert"),
            absPath("${SHADERS}/framebuffer/nOneStripMSAA.frag")
        );

    nOneStripProgram->activate();

    ghoul::opengl::TextureUnit pixelSizeTextureUnit;
    pixelSizeTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, pixelSizeTexture);
    nOneStripProgram->setUniform("pixelSizeTexture", pixelSizeTextureUnit);

    // render strip
    glDrawBuffers(1, textureBuffers);

    glClearColor(0.f, 1.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindVertexArray(nOneStripVAO);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(false);
    for (int sample = 0; sample < _nAaSamples; ++sample) {
        nOneStripProgram->setUniform("currentSample", sample);
        glDrawArrays(GL_TRIANGLES, sample * 6, 6);
    }
    /*nOneStripProgram->setUniform("currentSample", 0);
    glDrawArrays(GL_TRIANGLES, 0, 6 * _nAaSamples);*/
    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);
    glBindVertexArray(0);

    saveTextureToMemory(GL_COLOR_ATTACHMENT0, _nAaSamples, 1, _mSAAPattern);
    // Convert back to [-1, 1] range and then scales to the current viewport size:
    for (int d = 0; d < _nAaSamples; ++d) {
        _mSAAPattern[d * 3] = (2.0 * _mSAAPattern[d * 3] - 1.0) /
                              static_cast<double>(viewport[2]);
        _mSAAPattern[(d * 3) + 1] = (2.0 * _mSAAPattern[(d * 3) + 1] - 1.0) /
                                    static_cast<double>(viewport[3]);
        _mSAAPattern[(d * 3) + 2] = 0.0;
    }

    nOneStripProgram->deactivate();

    // Restores default state
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    // Deletes unused buffers
    glDeleteFramebuffers(1, &pixelSizeFramebuffer);
    glDeleteTextures(1, &pixelSizeTexture);
    glDeleteBuffers(1, &pixelSizeQuadVBO);
    glDeleteVertexArrays(1, &pixelSizeQuadVAO);

    glDeleteFramebuffers(1, &nOneStripFramebuffer);
    glDeleteTextures(1, &nOneStripTexture);
    glDeleteBuffers(1, &nOneStripVBO);
    glDeleteVertexArrays(1, &nOneStripVAO);
}

void ABufferRenderer::render(Scene* scene, Camera* camera, float blackoutFactor) {
    PerfMeasure("ABufferRenderer::render");

    if (!scene || !camera) {
        return;
    }

    ghoul::opengl::TextureUnit mainColorTextureUnit;
    mainColorTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    ghoul::opengl::TextureUnit mainDepthTextureUnit;
    mainDepthTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Reset
    clear();
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);

    // Step 1: Render geometries to the fragment buffer

    // Bind head-pointer image for read-write
    glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
    glBindImageTexture(0, _anchorPointerTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32UI);

    // Render the scene to the fragment buffer. Collect renderer tasks (active raycasters)
    int renderBinMask = static_cast<int>(Renderable::RenderBin::Background) |
        static_cast<int>(Renderable::RenderBin::Opaque) |
        static_cast<int>(Renderable::RenderBin::Transparent) |
        static_cast<int>(Renderable::RenderBin::Overlay);

    Time time = global::timeManager.time();
    RenderData data{ *camera, psc(), time, doPerformanceMeasurements, renderBinMask, {} };
    RendererTasks tasks;
    scene->render(data, tasks);
    _blackoutFactor = blackoutFactor;

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);


    // Step 2: Perform raycasting tasks requested by the scene
    for (const RaycasterTask& raycasterTask : tasks.raycasterTasks) {
        VolumeRaycaster* raycaster = raycasterTask.raycaster;
        ghoul::opengl::ProgramObject* program = _boundsPrograms[raycaster].get();
        if (program) {
            program->activate();
            program->setUniform("_exit_", false);
            raycaster->renderEntryPoints(raycasterTask.renderData, *program);
            program->setUniform("_exit_", true);
            raycaster->renderExitPoints(raycasterTask.renderData, *program);
            program->deactivate();
        }
        else {
            LWARNING("Raycaster is not attached when trying to perform raycaster task");
        }
    }


    // Step 3: Resolve the buffer
    _resolveProgram->activate();

    // TEMPORARY GAMMA CORRECTION.

    glm::vec3 cameraPos = data.camera.position().vec3();
    float maxComponent = std::max(
        std::max(std::abs(cameraPos.x), std::abs(cameraPos.y)), std::abs(cameraPos.z)
    );
    float logDistance = std::log(glm::length(cameraPos / maxComponent) * maxComponent)
        / std::log(10.f);

    const float minLogDist = 15.f;
    const float maxLogDist = 20.f;

    float t = (logDistance - minLogDist) / (maxLogDist - minLogDist);
    t = glm::clamp(t, 0.0f, 1.0f);
    const float gamma = 1.f * (1.f - t) + 2.2f * t;

    _resolveProgram->setUniform("gamma", gamma);

    // END TEMPORARY GAMMA CORRECTION.

    _resolveProgram->setUniform("mainColorTexture", mainColorTextureUnit.unitNumber());
    _resolveProgram->setUniform("mainDepthTexture", mainDepthTextureUnit.unitNumber());
    _resolveProgram->setUniform("blackoutFactor", _blackoutFactor);
    _resolveProgram->setUniform("nAaSamples", _nAaSamples);

    for (const RaycasterTask& raycasterTask : tasks.raycasterTasks) {
        preRaycast(raycasterTask);
    }

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    for (const RaycasterTask& raycasterTask : tasks.raycasterTasks) {
        postRaycast(raycasterTask);
    }

    _resolveProgram->deactivate();
}

void ABufferRenderer::preRaycast(const RaycasterTask& raycasterTask) {
    VolumeRaycaster& raycaster = *raycasterTask.raycaster;
    const RaycastData& raycastData = _raycastData[&raycaster];
    const RenderData& renderData = raycasterTask.renderData;

    raycaster.preRaycast(raycastData, *_resolveProgram);

    glm::vec3 localCameraPosition;
    bool isCameraInside = raycaster.isCameraInside(renderData, localCameraPosition);
    int uniformIndex = raycastData.id + 1; // uniforms are indexed from 1 (not from 0)
    _resolveProgram->setUniform(
        "insideRaycaster" + std::to_string(uniformIndex),
        isCameraInside
    );
    if (isCameraInside) {
        _resolveProgram->setUniform(
            "cameraPosInRaycaster" + std::to_string(uniformIndex),
            localCameraPosition
        );
    }
}

void ABufferRenderer::postRaycast(const RaycasterTask& raycasterTask) {
    VolumeRaycaster& raycaster = *raycasterTask.raycaster;
    const RaycastData& raycastData = _raycastData[&raycaster];

    raycaster.postRaycast(raycastData, *_resolveProgram);
}

void ABufferRenderer::setResolution(glm::ivec2 res) {
    if (res != _resolution) {
        _resolution = std::move(res);
        _dirtyResolution = true;
    }
}

void ABufferRenderer::setNAaSamples(int nAaSamples) {
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

void ABufferRenderer::setHDRExposure(float hdrExposure) {
    _hdrExposure = hdrExposure;
    if (_hdrExposure < 0.f) {
        LERROR("HDR Exposure constant must be greater than zero.");
        _hdrExposure = 1.0;
    }
}

void ABufferRenderer::setHDRBackground(float hdrBackground) {
    _hdrBackground = hdrBackground;
    if (_hdrBackground < 0.f) {
        LERROR("HDR Background constant must be greater than zero.");
        _hdrBackground = 1.0;
    }
}


void ABufferRenderer::setGamma(float gamma) {
    _gamma = gamma;
    if (_gamma < 0.f) {
        LERROR("Gamma value must be greater than zero.");
        _gamma = 2.2f;
    }
}

float ABufferRenderer::hdrBackground() const {
    return _hdrBackground;
}

int ABufferRenderer::nAaSamples() const {
    return _nAaSamples;
}

const std::vector<double>& ABufferRenderer::mSSAPattern() const {
    return _mSAAPattern;
}

void ABufferRenderer::clear() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
    glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);

    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R32UI,
        _resolution.x,
        _resolution.y,
        0,
        GL_RED_INTEGER,
        GL_UNSIGNED_INT,
        nullptr
    );
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    static const GLuint zero = 1;
    glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
    glBufferSubData(GL_ATOMIC_COUNTER_BUFFER, 0, sizeof(zero), &zero);
    glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, 0);
}

void ABufferRenderer::updateResolution() {
    PerfMeasure("ABufferRenderer::updateResolution");

    int totalPixels = _resolution.x * _resolution.y;
    glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R32UI,
        _resolution.x,
        _resolution.y,
        0,
        GL_RED_INTEGER,
        GL_UNSIGNED_INT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
    glBufferData(
        GL_PIXEL_UNPACK_BUFFER,
        totalPixels * sizeof(GLuint),
        nullptr,
        GL_STATIC_DRAW
    );

    GLuint* data = reinterpret_cast<GLuint*>(
        glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY)
    );
    memset(data, 0x00, totalPixels * sizeof(GLuint));
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
    glBufferData(
        GL_TEXTURE_BUFFER,
        MaxAverageLayers*totalPixels * sizeof(GLuint) * 4,
        nullptr,
        GL_DYNAMIC_COPY
    );

    glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA,
        _resolution.x,
        _resolution.y,
        true
    );

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_DEPTH_COMPONENT32F,
        _resolution.x,
        _resolution.y,
        true
    );

    _dirtyResolution = false;
}

void ABufferRenderer::updateResolveDictionary() {
    ghoul::Dictionary dict;
    ghoul::Dictionary raycastersDict;

    for (const std::pair<VolumeRaycaster*, RaycastData>& raycastPair : _raycastData) {
        ghoul::Dictionary innerDict;
        int id = raycastPair.second.id;
        std::string namespaceName = raycastPair.second.namespaceName;
        std::string raycastPath = raycastPair.first->raycasterPath();

        innerDict.setValue("id", id);
        innerDict.setValue("namespace", namespaceName);
        innerDict.setValue("bitmask", 1 << id);
        innerDict.setValue("raycastPath", raycastPath);

        raycastersDict.setValue(std::to_string(id), innerDict);
    }

    dict.setValue("raycasters", raycastersDict);

    ghoul::Dictionary helperPathsDict;
    for (size_t i = 0; i < _helperPaths.size(); ++i) {
        helperPathsDict.setValue(std::to_string(i), _helperPaths[i]);
    }

    dict.setValue("helperPaths", helperPathsDict);
    dict.setValue("raycastingEnabled", !_raycastData.empty());
    dict.setValue("storeSorted", true);
    dict.setValue("nRaycasters", static_cast<unsigned long long>(_raycastData.size()));

    _resolveDictionary = dict;

    global::renderEngine.setResolveData(dict);

    _dirtyResolveDictionary = false;
}

void ABufferRenderer::updateRaycastData() {
    PerfMeasure("ABufferRenderer::updateRaycastData");

    _raycastData.clear();
    _boundsPrograms.clear();
    _helperPaths.clear();

    const std::vector<VolumeRaycaster*>& raycasters =
        global::renderEngine.raycasterManager().raycasters();

    std::map<std::string, int> namespaceIndices;
    // raycaster ids are positive integers starting at 0. (for raycasters,
    // fragment type is id+1)
    int nextId = 0;
    int nextNamespaceIndex = 0;

    for (VolumeRaycaster* raycaster : raycasters) {
        if (nextId > MaxRaycasters) {
            int nIgnored = MaxRaycasters - static_cast<int>(raycasters.size());
            LWARNING(fmt::format(
                "ABufferRenderer does not support more than 32 raycasters. "
                "Ignoring {} raycasters",
                nIgnored
            ));
            break;
        }

        RaycastData data;
        data.id = nextId++;

        std::string helperPath = raycaster->helperPath();
        // Each new helper path generates a new namespace,
        // to avoid glsl name collisions between raycaster implementaitons.
        // Assign a new namespace or find an already created index.

        if (helperPath.empty()) {
            data.namespaceName = "NAMESPACE_" + std::to_string(nextNamespaceIndex++);
        } else {
            auto iter = namespaceIndices.find(helperPath);
            if (iter == namespaceIndices.end()) {
                int namespaceIndex = nextNamespaceIndex++;
                data.namespaceName = std::to_string(namespaceIndex);
                namespaceIndices[helperPath] = namespaceIndex;
                _helperPaths.push_back(helperPath);
            }
            else {
                data.namespaceName = "NAMESPACE_" + std::to_string(iter->second);
            }
        }

        _raycastData[raycaster] = data;
        std::string vsPath = raycaster->boundsVertexShaderPath();
        std::string fsPath = raycaster->boundsFragmentShaderPath();
        ghoul::Dictionary dict;

        // set path to the current renderer's main fragment shader
        dict.setValue("rendererData", _rendererData);
        // parameterize the main fragment shader program with specific contents.
        // fsPath should point to a shader file defining a Fragment getFragment() function
        // instead of a void main() setting glFragColor, glFragDepth, etc.
        dict.setValue("fragmentPath", fsPath);
        dict.setValue("fragmentType", data.id + 1);
        try {
            _boundsPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " bounds",
                vsPath,
                BoundsFragmentShaderPath,
                dict
            );
        }
        catch (ghoul::RuntimeError& error) {
            LERRORC(error.component, error.message);
        }
    }

    _dirtyRaycastData = false;
    _dirtyResolveDictionary = true;
}

void ABufferRenderer::updateRendererData() {
    PerfMeasure("ABufferRenderer::updateRendererData");

    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));
    dict.setValue("maxLayers", MaxLayers);
    dict.setValue("maxTotalFragments", MaxLayers * _resolution.x * _resolution.y);

    _rendererData = dict;

    global::renderEngine.setRendererData(dict);
    _dirtyRendererData = false;
}

void ABufferRenderer::saveTextureToMemory(GLenum color_buffer_attachment, int width,
                                          int height, std::vector<double>& memory) const
{
    memory.clear();

    memory.reserve(width * height * 3);
    std::vector<float> tmpMemory(width * height * 3);

    if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
        glReadBuffer(color_buffer_attachment);
        glReadPixels(0, 0, width, height, GL_RGB, GL_FLOAT, tmpMemory.data());

    }
    else {
        glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, tmpMemory.data());
    }

    for (int i = 0; i < width * height * 3; ++i) {
        memory[i] = static_cast<double>(tmpMemory[i]);
    }
}

} // namespace openspace

#endif // OPENSPACE_WITH_ABUFFER_RENDERER
