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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

#include <string>
#include <vector>
#include <sstream>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "FramebufferRenderer";
    constexpr const char* ExitFragmentShaderPath =
        "${SHADERS}/framebuffer/exitframebuffer.frag";
    constexpr const char* RaycastFragmentShaderPath =
        "${SHADERS}/framebuffer/raycastframebuffer.frag";
    constexpr const char* GetEntryInsidePath = "${SHADERS}/framebuffer/inside.glsl";
    constexpr const char* GetEntryOutsidePath = "${SHADERS}/framebuffer/outside.glsl";
    constexpr const char* RenderFragmentShaderPath =
        "${SHADERS}/framebuffer/renderframebuffer.frag";
} // namespace

namespace openspace {
    void saveTextureToPPMFile(GLenum color_buffer_attachment,
        const std::string & fileName, int width, int height);

    void saveTextureToMemory(GLenum color_buffer_attachment,
        int width, int height, std::vector<double> & memory);


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
        LDEBUG("Initializing FramebufferRenderer");

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
            nullptr
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
        glGenTextures(1, &_mainPositionTexture);
        glGenTextures(1, &_mainNormalTexture);
        glGenFramebuffers(1, &_deferredFramebuffer);

        updateResolution();
        updateRendererData();
        updateRaycastData();

        glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D_MULTISAMPLE,
            _mainColorTexture,
            0
        );
        // G-buffer
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT1,
            GL_TEXTURE_2D_MULTISAMPLE,
            _mainPositionTexture,
            0
        );
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT2,
            GL_TEXTURE_2D_MULTISAMPLE,
            _mainNormalTexture,
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
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            _deferredColorTexture,
            0
        );

        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Deferred framebuffer is not complete");
        }

        // JCC: Moved to here to avoid NVidia: "Program/shader state performance warning"
        updateHDRData();
        updateDeferredcastData();
        _dirtyMsaaSamplingPattern = true;

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

        _resolveProgram = ghoul::opengl::ProgramObject::Build(
            "Framebuffer Resolve",
            absPath("${SHADERS}/framebuffer/resolveframebuffer.vert"),
            absPath("${SHADERS}/framebuffer/resolveframebuffer.frag")
        );

        _uniformCache.mainColorTexture = _resolveProgram->uniformLocation("mainColorTexture");
        _uniformCache.blackoutFactor = _resolveProgram->uniformLocation("blackoutFactor");
        _uniformCache.nAaSamples = _resolveProgram->uniformLocation("nAaSamples");

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

    void FramebufferRenderer::deferredcastersChanged(Deferredcaster& /*deferredcaster*/,
        isAttached /*isAttached*/)
    {
        _dirtyDeferredcastData = true;
    }

    void FramebufferRenderer::update() {
        // If the resolve dictionary changed (or a file changed on disk)
        // then rebuild the resolve program.
        if (_hdrBackGroundProgram && _hdrBackGroundProgram->isDirty()) {
            _hdrBackGroundProgram->rebuildFromFile();
        }

        if (_resolveProgram->isDirty()) {
            _resolveProgram->rebuildFromFile();

            _uniformCache.mainColorTexture = _resolveProgram->uniformLocation(
                "mainColorTexture"
            );
            _uniformCache.blackoutFactor = _resolveProgram->uniformLocation("blackoutFactor");
            _uniformCache.nAaSamples = _resolveProgram->uniformLocation("nAaSamples");
        }

        for (RaycasterProgObjMap::value_type & program : _exitPrograms) {
            if (program.second->isDirty()) {
                try {
                    program.second->rebuildFromFile();
                } 
                catch (const ghoul::RuntimeError& e) {
                    LERRORC(e.component, e.message);
                }
            }
        }

        for (RaycasterProgObjMap::value_type & program : _raycastPrograms) {
            if (program.second->isDirty()) {
                try {
                    program.second->rebuildFromFile();
                } 
                catch (const ghoul::RuntimeError& e) {
                    LERRORC(e.component, e.message);
                }
            }
        }

        for (RaycasterProgObjMap::value_type & program : _insideRaycastPrograms) {
            if (program.second->isDirty()) {
                try {
                    program.second->rebuildFromFile();
                } 
                catch (const ghoul::RuntimeError& e) {
                    LERRORC(e.component, e.message);
                }
            }
        }

        for (DeferredcasterProgObjMap::value_type &program : _deferredcastPrograms) {
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
        for (VolumeRaycaster* raycaster : raycasters) {
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
            } 
            catch (ghoul::RuntimeError e) {
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
            } 
            catch (ghoul::RuntimeError e) {
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
            OsEng.renderEngine().deferredcasterManager().deferredcasters();
        int nextId = 0;
        for (Deferredcaster * caster : deferredcasters) {
            DeferredcastData data;
            data.id = nextId++;
            data.namespaceName = "HELPER";

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
            //dict.setValue("deferredcastPath", caster->getDeferredcastPath());

            _deferredcastData[caster] = data;

            try {
                ghoul::Dictionary deferredDict = dict;
                //deferredDict.setValue("getEntryPath", GetEntryOutsidePath);
                _deferredcastPrograms[caster] = ghoul::opengl::ProgramObject::Build(
                    "Deferred " + std::to_string(data.id) + " raycast",
                    absPath(vsPath),
                    absPath(deferredShaderPath),
                    deferredDict);
                using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
                _deferredcastPrograms[caster]->setIgnoreSubroutineUniformLocationError(
                    IgnoreError::Yes
                );
                _deferredcastPrograms[caster]->setIgnoreUniformLocationError(
                    IgnoreError::Yes
                );

                caster->initializeCachedVariables(*_deferredcastPrograms[caster]);
            }
            catch (ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
        _dirtyDeferredcastData = false;
    }

    void FramebufferRenderer::updateHDRData() {
        _hdrBackGroundProgram = ghoul::opengl::ProgramObject::Build(
            "HDR Background Control",
            absPath("${SHADERS}/framebuffer/hdrBackground.vert"),
            absPath("${SHADERS}/framebuffer/hdrBackground.frag")
        );
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _hdrBackGroundProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _hdrBackGroundProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
    }

    void FramebufferRenderer::updateMSAASamplingPattern() {
        LDEBUG("Updating MSAA Sampling Pattern");

        const int GRIDSIZE = 32;
        GLfloat step = 2.f / static_cast<GLfloat>(GRIDSIZE);
        GLfloat sizeX = -1.f,
            sizeY = 1.f;

        const int NVERTEX = 4 * 6;
        // openPixelSizeVertexData
        GLfloat vertexData[GRIDSIZE * GRIDSIZE * NVERTEX];

        for (int y = 0; y < GRIDSIZE; ++y) {
            for (int x = 0; x < GRIDSIZE; ++x) {
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX] = sizeX;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 1] = sizeY - step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 2] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 3] = 1.f;

                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 4] = sizeX + step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 5] = sizeY;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 6] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 7] = 1.f;

                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 8] = sizeX;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 9] = sizeY;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 10] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 11] = 1.f;

                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 12] = sizeX;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 13] = sizeY - step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 14] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 15] = 1.f;

                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 16] = sizeX + step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 17] = sizeY - step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 18] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 19] = 1.f;

                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 20] = sizeX + step;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 21] = sizeY;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 22] = 0.f;
                vertexData[y * GRIDSIZE * NVERTEX + x * NVERTEX + 23] = 1.f;

                sizeX += step;
            }
            sizeX = -1.f;
            sizeY -= step;
        }

        GLuint pixelSizeQuadVAO = 0;
        GLuint pixelSizeQuadVBO = 0;

        glGenVertexArrays(1, &pixelSizeQuadVAO);
        glBindVertexArray(pixelSizeQuadVAO);

        glGenBuffers(1, &pixelSizeQuadVBO);
        glBindBuffer(GL_ARRAY_BUFFER, pixelSizeQuadVBO);

        glBufferData(
            GL_ARRAY_BUFFER,
            sizeof(GLfloat) * GRIDSIZE * GRIDSIZE * NVERTEX,
            vertexData,
            GL_STATIC_DRAW
        );

        // Position
        glVertexAttribPointer(
            0,
            4,
            GL_FLOAT,
            GL_FALSE,
            0,
            nullptr
        );
        glEnableVertexAttribArray(0);

        // Saves current state
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

        glm::ivec4 viewport = OsEng.windowWrapper().viewportPixelCoordinates();

        // Main framebuffer
        GLuint pixelSizeTexture = 0;
        GLuint pixelSizeFramebuffer = 0;

        glGenTextures(1, &pixelSizeTexture);
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, pixelSizeTexture);

        const GLsizei ONEPIXEL = 1;
        glTexImage2DMultisample(
            GL_TEXTURE_2D_MULTISAMPLE,
            _nAaSamples,
            GL_RGBA32F,
            ONEPIXEL,
            ONEPIXEL,
            true
        );

        glViewport(0, 0, ONEPIXEL, ONEPIXEL);

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

        std::unique_ptr<ghoul::opengl::ProgramObject> pixelSizeProgram = nullptr;
        try {
            pixelSizeProgram = ghoul::opengl::ProgramObject::Build(
                "OnePixel MSAA",
                absPath("${SHADERS}/framebuffer/pixelSizeMSAA.vert"),
                absPath("${SHADERS}/framebuffer/pixelSizeMSAA.frag")

            );
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }

        pixelSizeProgram->activate();

        // Draw sub-pixel grid
        glEnable(GL_SAMPLE_SHADING);
        glBindVertexArray(pixelSizeQuadVAO);
        glDisable(GL_DEPTH_TEST);
        glDepthMask(false);
        glDrawArrays(GL_TRIANGLES, 0, GRIDSIZE * GRIDSIZE * 6);
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

        GLfloat * nOneStripVertexData = new GLfloat[_nAaSamples * (NVERTEX + 12)];

        for (int x = 0; x < _nAaSamples; ++x) {
            nOneStripVertexData[x * (NVERTEX + 12)] = sizeX;
            nOneStripVertexData[x * (NVERTEX + 12) + 1] = -1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 2] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 3] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 4] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 5] = 0.f;

            nOneStripVertexData[x * (NVERTEX + 12) + 6] = sizeX + step;
            nOneStripVertexData[x * (NVERTEX + 12) + 7] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 8] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 9] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 10] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 11] = 1.f;

            nOneStripVertexData[x * (NVERTEX + 12) + 12] = sizeX;
            nOneStripVertexData[x * (NVERTEX + 12) + 13] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 14] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 15] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 16] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 17] = 0.f;

            nOneStripVertexData[x * (NVERTEX + 12) + 18] = sizeX;
            nOneStripVertexData[x * (NVERTEX + 12) + 19] = -1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 20] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 21] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 22] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 23] = 0.f;

            nOneStripVertexData[x * (NVERTEX + 12) + 24] = sizeX + step;
            nOneStripVertexData[x * (NVERTEX + 12) + 25] = -1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 26] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 27] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 28] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 29] = 1.f;

            nOneStripVertexData[x * (NVERTEX + 12) + 30] = sizeX + step;
            nOneStripVertexData[x * (NVERTEX + 12) + 31] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 32] = 0.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 33] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 34] = 1.f;
            nOneStripVertexData[x * (NVERTEX + 12) + 35] = 1.f;

            sizeX += step;
        }

        glGenVertexArrays(1, &nOneStripVAO);
        glBindVertexArray(nOneStripVAO);
        glGenBuffers(1, &nOneStripVBO);
        glBindBuffer(GL_ARRAY_BUFFER, nOneStripVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            sizeof(GLfloat) * _nAaSamples * (NVERTEX + 12),
            nOneStripVertexData,
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
        delete[] nOneStripVertexData;

        // fbo texture buffer
        glGenTextures(1, &nOneStripTexture);
        glBindTexture(GL_TEXTURE_2D, nOneStripTexture);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGBA32F,
            _nAaSamples,
            ONEPIXEL,
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

        glViewport(0, 0, _nAaSamples, ONEPIXEL);

        std::unique_ptr<ghoul::opengl::ProgramObject> nOneStripProgram = nullptr;
        nOneStripProgram = ghoul::opengl::ProgramObject::Build(
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
        glDepthMask(true);
        glEnable(GL_DEPTH_TEST);
        glBindVertexArray(0);

        saveTextureToMemory(GL_COLOR_ATTACHMENT0, _nAaSamples, 1, _mSAAPattern);
        // Convert back to [-1, 1] range and then scale for the current viewport size:
        for (int d = 0; d < _nAaSamples; ++d) {
            _mSAAPattern[d * 3] = (2.0 * _mSAAPattern[d * 3] - 1.0) /
                static_cast<double>(viewport[1]);
            _mSAAPattern[(d * 3) + 1] = (2.0 * _mSAAPattern[(d * 3) + 1] - 1.0) /
                static_cast<double>(viewport[3]);
            _mSAAPattern[(d * 3) + 2] = 0.0;
        }

        nOneStripProgram->deactivate();

        // Restores default state
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
        glViewport(viewport[0], viewport[2], viewport[1], viewport[3]);

        // Deletes unused buffers
        glDeleteFramebuffers(1, &pixelSizeFramebuffer);
        glDeleteTextures(1, &pixelSizeTexture);
        glDeleteBuffers(1, &pixelSizeQuadVBO);
        glDeleteVertexArrays(1, &pixelSizeQuadVAO);

        glDeleteFramebuffers(1, &nOneStripFramebuffer);
        glDeleteTextures(1, &nOneStripTexture);
        glDeleteBuffers(1, &nOneStripVBO);
        glDeleteVertexArrays(1, &nOneStripVAO);

        _dirtyMsaaSamplingPattern = false;
    }

    void FramebufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {
        if (_dirtyResolution) {
            updateResolution();
        }

        if (_dirtyMsaaSamplingPattern) {
            updateMSAASamplingPattern();
        }

        if (_dirtyRaycastData) {
            updateRaycastData();
        }

        if (_dirtyDeferredcastData) {
            updateDeferredcastData();
        }

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

        // Capture standard fbo
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

        glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
        glEnable(GL_DEPTH_TEST);

        // deferred g-buffer
        GLenum textureBuffers[3] = {
            GL_COLOR_ATTACHMENT0,
            GL_COLOR_ATTACHMENT1,
            GL_COLOR_ATTACHMENT2,
        };
        glDrawBuffers(3, textureBuffers);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        glEnablei(GL_BLEND, 0);
        glDisablei(GL_BLEND, 1);
        glDisablei(GL_BLEND, 2);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        Time time = OsEng.timeManager().time();

        RenderData data = { *_camera, psc(), time, doPerformanceMeasurements, 0,{} };
        RendererTasks tasks;

        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Background);
        _scene->render(data, tasks);
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Opaque);
        _scene->render(data, tasks);
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Transparent);
        _scene->render(data, tasks);
        data.renderBinMask = static_cast<int>(Renderable::RenderBin::Overlay);
        _scene->render(data, tasks);

        {
            std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
            if (doPerformanceMeasurements) {
                perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                    "FramebufferRenderer::render::raycasterTasks",
                    OsEng.renderEngine().performanceManager()
                    );
            }
            performRaycasterTasks(tasks.raycasterTasks);
        }

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
        GLenum dBuffer[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, dBuffer);
        
        {
            std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
            if (doPerformanceMeasurements) {
                perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                    "FramebufferRenderer::render::deferredTasks",
                    OsEng.renderEngine().performanceManager()
                    );
            }
            performDeferredTasks(tasks.deferredcasterTasks);
        }
    
        if (tasks.deferredcasterTasks.empty()) {
            glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
            _resolveProgram->activate();

            ghoul::opengl::TextureUnit mainColorTextureUnit;
            mainColorTextureUnit.activate();

            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);
            _resolveProgram->setUniform(_uniformCache.mainColorTexture, mainColorTextureUnit);
            _resolveProgram->setUniform(_uniformCache.blackoutFactor, blackoutFactor);
            _resolveProgram->setUniform(_uniformCache.nAaSamples, _nAaSamples);
            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            _resolveProgram->deactivate();
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
                glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
                raycastProgram->setUniform("mainDepthTexture", mainDepthTextureUnit);

                raycastProgram->setUniform("nAaSamples", _nAaSamples);
                raycastProgram->setUniform("windowSize", _resolution);

                glDisable(GL_DEPTH_TEST);
                glDepthMask(false);
                if (cameraIsInside) {
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

    void FramebufferRenderer::performDeferredTasks(const std::vector<DeferredcasterTask>& tasks) {
        bool firstPaint = true;

        for (const DeferredcasterTask& deferredcasterTask : tasks) {

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
                deferredcastProgram->setUniform(
                    "mainColorTexture",
                    mainDColorTextureUnit
                );

                ghoul::opengl::TextureUnit mainPositionTextureUnit;
                mainPositionTextureUnit.activate();
                glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture);
                deferredcastProgram->setUniform(
                    "mainPositionTexture",
                    mainPositionTextureUnit
                );

                ghoul::opengl::TextureUnit mainNormalTextureUnit;
                mainNormalTextureUnit.activate();
                glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture);
                deferredcastProgram->setUniform(
                    "mainNormalTexture",
                    mainNormalTextureUnit
                );

                deferredcastProgram->setUniform("nAaSamples", _nAaSamples);
                // 48 = 16 samples * 3 coords
                deferredcastProgram->setUniform("msaaSamplePatter", &_mSAAPattern[0], 48);

                deferredcastProgram->setUniform("firstPaint", firstPaint);
                deferredcastProgram->setUniform("atmExposure", _hdrExposure);
                deferredcastProgram->setUniform("backgroundConstant", _hdrBackground);

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

                deferredcaster->postRaycast(deferredcasterTask.renderData,
                    _deferredcastData[deferredcaster],
                    *deferredcastProgram);

                deferredcastProgram->deactivate();

                if (firstPaint) {
                    firstPaint = false;
                }
            }
            else {
                LWARNING(
                    "Deferredcaster is not attached when trying to perform deferred task"
                );
            }
        }
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

    void FramebufferRenderer::setNAaSamples(int nAaSamples) {
        _nAaSamples = nAaSamples;
        if (_nAaSamples == 0) {
            _nAaSamples = 1;
        }
        if (_nAaSamples > 8) {
            LERROR("Framebuffer renderer does not support more than 8 MSAA samples.");
            _nAaSamples = 8;
        }
        _dirtyMsaaSamplingPattern = true;
    }

    void FramebufferRenderer::setHDRExposure(float hdrExposure) {
        _hdrExposure = hdrExposure;
        if (_hdrExposure < 0.0f) {
            LERROR("HDR Exposure constant must be greater than zero.");
            _hdrExposure = 1.0f;
        }
    }

    void FramebufferRenderer::setHDRBackground(float hdrBackground) {
        _hdrBackground = hdrBackground;
        if (_hdrBackground < 0.0f) {
            LERROR("HDR Background constant must be greater than zero.");
            _hdrBackground = 1.0f;
        }
    }

    void FramebufferRenderer::setGamma(float gamma) {
        _gamma = gamma;
        if (_gamma < 0.0f) {
            LERROR("Gamma value must be greater than zero.");
            _gamma = 2.2f;
        }
    }

    float FramebufferRenderer::hdrBackground() const {
        return _hdrBackground;
    }

    int FramebufferRenderer::nAaSamples() const {
        return _nAaSamples;
    }

    std::vector<double> FramebufferRenderer::mSSAPattern() const {
        return _mSAAPattern;
    }

    void FramebufferRenderer::updateRendererData() {
        ghoul::Dictionary dict;
        dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));
        _rendererData = dict;
        OsEng.renderEngine().setRendererData(dict);
    }

    void saveTextureToPPMFile(GLenum color_buffer_attachment,
        const std::string & fileName, int width, int height)
    {
        std::fstream ppmFile;

        ppmFile.open(fileName.c_str(), std::fstream::out);
        if (ppmFile.is_open()) {
            unsigned char* pixels = new unsigned char[width*height * 3];
            for (int t = 0; t < width*height * 3; ++t) {
                pixels[t] = 255;
            }

            if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
                glReadBuffer(color_buffer_attachment);
                glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

            }
            else {
                glReadPixels(
                    0,
                    0,
                    width,
                    height,
                    GL_DEPTH_COMPONENT,
                    GL_UNSIGNED_BYTE,
                    pixels
                );
            }

            ppmFile << "P3" << std::endl;
            ppmFile << width << " " << height << std::endl;
            ppmFile << "255" << std::endl;

            int k = 0;
            for (int i = 0; i < width; i++) {
                for (int j = 0; j < height; j++) {
                    ppmFile << static_cast<unsigned int>(pixels[k]) << " "
                        << static_cast<unsigned int>(pixels[k + 1]) << " "
                        << static_cast<unsigned int>(pixels[k + 2]) << " ";
                    k += 3;
                }
                ppmFile << std::endl;
            }
            delete[] pixels;

            ppmFile.close();
        }
    }

    void saveTextureToMemory(GLenum color_buffer_attachment,
        int width, int height, std::vector<double> & memory) {

        memory.clear();
        memory.resize(width * height * 3);

        std::vector<float> tempMemory(width*height * 3, 0.f);

        if (color_buffer_attachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(color_buffer_attachment);
            glReadPixels(0, 0, width, height, GL_RGB, GL_FLOAT, &tempMemory[0]);

        }
        else {
            glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, &tempMemory[0]);
        }

        for (auto i = 0; i < width*height * 3; ++i) {
            memory[i] = static_cast<double>(tempMemory[i]);
        }
    }

} // namespace openspace
