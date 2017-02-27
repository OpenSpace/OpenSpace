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

#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/raycastermanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <openspace/performance/performancemeasurement.h>


#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>



#include <string>
#include <iterator>

namespace {
    const std::string _loggerCat = "ABufferRenderer";
    const std::string BoundsFragmentShaderPath = "${SHADERS}/abuffer/boundsabuffer.frag";
    const std::string RenderFragmentShaderPath = "${SHADERS}/abuffer/renderabuffer.frag";
    const int MaxRaycasters = 32;
    const int MaxLayers = 32;
    const int MaxAverageLayers = 8;
}

namespace openspace {


ABufferRenderer::ABufferRenderer()
        : _camera(nullptr)
        , _scene(nullptr)
        , _resolution(glm::ivec2(0))
        , _dirtyResolution(true)
        , _dirtyRaycastData(true)
        , _dirtyRendererData(true)
        , _dirtyResolveDictionary(true)
        , _resolveProgram(nullptr) { }

ABufferRenderer::~ABufferRenderer() {}


void ABufferRenderer::initialize() {
    LINFO("Initializing ABufferRenderer");
    const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = {
        //      x      y     s     t
        -size, -size, 0.0f, 1.0f,
        size,    size, 0.0f, 1.0f, 
        -size,  size, 0.0f, 1.0f, 
        -size, -size, 0.0f, 1.0f, 
        size, -size, 0.0f, 1.0f, 
        size,    size, 0.0f, 1.0f,
    };


    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(0);

    glGenTextures(1, &_anchorPointerTexture);
    glGenBuffers(1, &_anchorPointerTextureInitializer);
    glGenBuffers(1, &_atomicCounterBuffer);
    glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, _atomicCounterBuffer);
    glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);
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

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture, 0);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);


    try {
        ghoul::Dictionary dict;
        dict.setValue("resolveData", _resolveDictionary);
        dict.setValue("rendererData", _rendererData);

        _resolveProgram = ghoul::opengl::ProgramObject::Build("ABuffer Resolve",
            "${SHADERS}/abuffer/resolveabuffer.vert",
            "${SHADERS}/abuffer/resolveabuffer.frag",
            dict);
    } catch (ghoul::RuntimeError e) {
        LERROR(e.message);
    }

    OsEng.renderEngine().raycasterManager().addListener(*this);
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

    OsEng.renderEngine().raycasterManager().removeListener(*this);
}

void ABufferRenderer::raycastersChanged(VolumeRaycaster& raycaster, bool attached) {
    (void) raycaster;
    (void) attached;
    _dirtyRaycastData = true;
}
    
void ABufferRenderer::update() {
    PerfMeasure("ABufferRenderer::update");
    
    // Make sure that the fragment buffer has the correct resoliution
    // according to the output render buffer size
    if (_dirtyResolution) {
        updateResolution();
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
        } catch (ghoul::RuntimeError& error) {
            LERROR(error.message);
        }
    }

    for (auto &program : _boundsPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            } catch (ghoul::RuntimeError e) {
                LERROR(e.message);
            }
        }
    }

}

    
void ABufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {
    PerfMeasure("ABufferRenderer::render");


    if (_scene == nullptr)
        return;
    if (_camera == nullptr)
        return;

    _mainColorTextureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _mainColorTextureUnit->activate();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    _mainDepthTextureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _mainDepthTextureUnit->activate();
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

    RenderData data{ *_camera, psc(), doPerformanceMeasurements, renderBinMask };
    RendererTasks tasks;
    _scene->render(data, tasks);

    _rendererTasks = std::make_unique<RendererTasks>(tasks);
    _renderData = std::make_unique<RenderData>(data);
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

    float gamma = 1.0;
    glm::vec3 cameraPos = data.camera.position().vec3();
    float maxComponent = std::max(std::max(std::abs(cameraPos.x), std::abs(cameraPos.y)), std::abs(cameraPos.z));
    float logDistance = std::log(glm::length(cameraPos / maxComponent) * maxComponent) / std::log(10);

    float minLogDist = 15;
    float maxLogDist = 20;

    float t = (logDistance - minLogDist) / (maxLogDist - minLogDist);
    t = glm::clamp(t, 0.0f, 1.0f);
    gamma = 1.0 * (1 - t) + 2.2 * t;

    _resolveProgram->setUniform("gamma", gamma);

    // END TEMPORARY GAMMA CORRECTION.

    preRaycast(*_resolveProgram);
    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    postRaycast(*_resolveProgram);

    _resolveProgram->deactivate();

    _mainColorTextureUnit = nullptr;
    _mainDepthTextureUnit = nullptr;
}


void ABufferRenderer::preRaycast(ghoul::opengl::ProgramObject& program) {

    program.setUniform("mainColorTexture", _mainColorTextureUnit->unitNumber());
    program.setUniform("mainDepthTexture", _mainDepthTextureUnit->unitNumber());
    
    for (const auto& raycastData : _raycastData) {
        raycastData.first->preRaycast(raycastData.second, program);

        glm::vec3 localCameraPosition;
        bool cameraIsInside = raycastData.first->cameraIsInside(*_renderData, localCameraPosition);
        int uniformIndex = raycastData.second.id + 1; // uniforms are indexed from 1 (not from 0)
        program.setUniform("insideRaycaster" + std::to_string(uniformIndex), cameraIsInside);
        if (cameraIsInside) {
            program.setUniform("cameraPosInRaycaster" + std::to_string(uniformIndex), localCameraPosition);
        }
    }

    // 3b: Set "global" uniforms, and start the resolve pass.
    program.setUniform("blackoutFactor", _blackoutFactor);
    program.setUniform("nAaSamples", _nAaSamples);
}

void ABufferRenderer::postRaycast(ghoul::opengl::ProgramObject& program) {
    for (const auto& raycastData : _raycastData) {
        raycastData.first->postRaycast(raycastData.second, program);
    }
}

void ABufferRenderer::setScene(Scene* scene) {
    _scene = scene;
}

void ABufferRenderer::setCamera(Camera* camera) {
    _camera = camera;
}

void ABufferRenderer::setResolution(glm::ivec2 res) {
    if (res != _resolution) {
        _resolution = res;
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

void ABufferRenderer::clear() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
    glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _resolution.x, _resolution.y, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);
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
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _resolution.x, _resolution.y, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

    GLuint* data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
    memset(data, 0x00, totalPixels * sizeof(GLuint));
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
    glBufferData(GL_TEXTURE_BUFFER, MaxAverageLayers*totalPixels*sizeof(GLuint) * 4, NULL, GL_DYNAMIC_COPY);

    glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA,
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
        true);

    
    _dirtyResolution = false;
}



void ABufferRenderer::updateResolveDictionary() {
    ghoul::Dictionary dict;

    ghoul::Dictionary raycastersDict;

    for (const auto &raycastPair : _raycastData) {
        ghoul::Dictionary innerDict;
        int id = raycastPair.second.id;
        std::string namespaceName = raycastPair.second.namespaceName;
        std::string raycastPath = raycastPair.first->getRaycastPath();

        innerDict.setValue("id", id);
        innerDict.setValue("namespace", namespaceName);
        innerDict.setValue("bitmask", 1 << id);
        innerDict.setValue("raycastPath", raycastPath);

        raycastersDict.setValue(std::to_string(id), innerDict);
    }

    dict.setValue("raycasters", raycastersDict);

    ghoul::Dictionary helperPathsDict;
    for (int i = 0; i < _helperPaths.size(); i++) {
        helperPathsDict.setValue(std::to_string(i), _helperPaths[i]);
    }

    dict.setValue("helperPaths", helperPathsDict);
    dict.setValue("raycastingEnabled", _raycastData.size() > 0);
    dict.setValue("storeSorted", true);
    dict.setValue("nRaycasters", static_cast<unsigned long long>(_raycastData.size()));

    _resolveDictionary = dict;

    OsEng.renderEngine().setResolveData(dict);

    _dirtyResolveDictionary = false;
}

void ABufferRenderer::updateRaycastData() {
    PerfMeasure("ABufferRenderer::updateRaycastData");

    _raycastData.clear();
    _boundsPrograms.clear();
    _helperPaths.clear();

    const std::vector<VolumeRaycaster*>& raycasters = OsEng.renderEngine().raycasterManager().raycasters();

    std::map<std::string, int> namespaceIndices;
    int nextId = 0; // raycaster ids are positive integers starting at 0. (for raycasters, fragment type is id+1)
    int nextNamespaceIndex = 0;

    for (auto &raycaster : raycasters) {
        if (nextId > MaxRaycasters) {
            int nIgnored = MaxRaycasters - raycasters.size();
            LWARNING("ABufferRenderer does not support more than 32 raycasters. Ignoring " << nIgnored << " raycasters");
            break;
        }

        RaycastData data;
        data.id = nextId++;

        std::string helperPath = raycaster->getHelperPath();
        // Each new helper path generates a new namespace,
        // to avoid glsl name collisions between raycaster implementaitons.
        // Assign a new namespace or find an already created index.

        if (helperPath == "") {
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
        std::string vsPath = raycaster->getBoundsVsPath();
        std::string fsPath = raycaster->getBoundsFsPath();
        ghoul::Dictionary dict;

        // set path to the current renderer's main fragment shader
        dict.setValue("rendererData", _rendererData);
        // parameterize the main fragment shader program with specific contents.
        // fsPath should point to a shader file defining a Fragment getFragment() function
        // instead of a void main() setting glFragColor, glFragDepth, etc.
        dict.setValue("fragmentPath", fsPath);
        dict.setValue("fragmentType", data.id + 1);
        try {
            _boundsPrograms[raycaster] = ghoul::opengl::ProgramObject::Build("Volume " + std::to_string(data.id) + " bounds", vsPath, BoundsFragmentShaderPath, dict);
        }
        catch (ghoul::RuntimeError& error) {
            LERROR(error.message);
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

    OsEng.renderEngine().setRendererData(dict);
    _dirtyRendererData = false;
}

}
