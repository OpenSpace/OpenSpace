/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/molecule/moleculemodule.h>

#include <modules/molecule/src/renderablemolecule.h>
#include <modules/molecule/src/renderablesimulationbox.h>
#include <modules/molecule/src/viamd/postprocessing.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/logging/logmanager.h>
#include <md_gl.h>
#include <string_view>

namespace {
    constexpr std::string_view ShaderOutputSnippet = R"(
layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_normal;

vec2 encode_normal (vec3 n) {
  float p = sqrt(n.z * 8 + 8);
  return n.xy / p + 0.5;
}

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
  out_normal = vec4(encode_normal(view_normal), 0, 0);
  out_color = color;
}
)";

    constexpr openspace::properties::Property::PropertyInfo SSAOEnabledInfo = {
        "SSAOEnabled",
        "Enable SSAO",
        "Enable SSAO"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAOIntensityInfo = {
        "SSAOIntensity",
        "SSAO Intensity",
        "SSAO Intensity"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAORadiusInfo = {
        "SSAORadius",
        "SSAO Radius",
        "SSAO Radius"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAOBiasInfo = {
        "SSAOHorizonBias",
        "SSAO Horizon Bias",
        "SSAO Horizon Bias"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAONormalBiasInfo = {
        "SSAONormalBias",
        "SSAO Normal Bias",
        "SSAO Normal Bias"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAO2EnabledInfo = {
        "SSAO2Enabled",
        "Enable SSAO",
        "Enable SSAO"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAO2IntensityInfo = {
        "SSAO2Intensity",
        "SSAO Intensity",
        "SSAO Intensity"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAO2RadiusInfo = {
        "SSAO2Radius",
        "SSAO Radius",
        "SSAO Radius"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAO2BiasInfo = {
        "SSAO2HorizonBias",
        "SSAO Horizon Bias",
        "SSAO Horizon Bias"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAO2NormalBiasInfo = {
        "SSAO2NormalBias",
        "SSAO Normal Bias",
        "SSAO Normal Bias"
    };

    constexpr openspace::properties::Property::PropertyInfo ExposureInfo = {
        "Exposure",
        "Exposure",
        "Exposure, Controls the Exposure setting for the tonemap"
    };

    constexpr openspace::properties::Property::PropertyInfo DOFEnabledInfo = {
        "DOFEnabled",
        "Enable DOF",
        "Enable DOF"
    };

    constexpr openspace::properties::Property::PropertyInfo DOFFocusDistanceInfo = {
        "DOFFocusDistance",
        "DOF Focus Distance",
        "DOF Focus Distance"
    };

    constexpr openspace::properties::Property::PropertyInfo DOFFocusRangeInfo = {
        "DOFFocusRange",
        "DOF Focus Range",
        "DOF Focus Range"
    };
} // namespace

namespace openspace {

MoleculeModule::MoleculeModule()
    : OpenSpaceModule(Name)
    , _shaders(new md_gl_shaders_t)
    , _ssaoEnabled(SSAOEnabledInfo, true)
    , _ssaoIntensity(SSAOIntensityInfo, 4.f, 0.f, 100.f)
    , _ssaoRadius(SSAORadiusInfo, 1.f, 0.1f, 10.f)
    , _ssaoHorizonBias(SSAOBiasInfo, 0.1f, 0.f, 1.0f)
    , _ssaoNormalBias(SSAONormalBiasInfo, 1.f, 0.f, 1.f)
    , _ssao2Enabled(SSAO2EnabledInfo, true)
    , _ssao2Intensity(SSAO2IntensityInfo, 4.f, 0.f, 100.f)
    , _ssao2Radius(SSAO2RadiusInfo, 10.f, 10.f, 1000.f)
    , _ssao2HorizonBias(SSAO2BiasInfo, 0.f, 0.f, 1.f)
    , _ssao2NormalBias(SSAO2NormalBiasInfo, 1.f, 0.f, 0.f)
    , _exposure(ExposureInfo, 0.3f, 0.1f, 10.f)
    , _dofEnabled(DOFEnabledInfo, false)
    , _dofFocusDistance(DOFFocusDistanceInfo, 0.5f, 0.f, 1.f)
    , _dofFocusRange(DOFFocusRangeInfo, 0.1f, 0.f, 10.f)
    , _threadPool(std::max(1U, std::thread::hardware_concurrency() - 1))
{
    addProperty(_ssaoEnabled);
    addProperty(_ssaoIntensity);
    addProperty(_ssaoRadius);
    addProperty(_ssaoHorizonBias);
    addProperty(_ssaoNormalBias);
    addProperty(_ssao2Enabled);
    addProperty(_ssao2Intensity);
    addProperty(_ssao2Radius);
    addProperty(_ssao2HorizonBias);
    addProperty(_ssao2NormalBias);
    addProperty(_exposure);
    addProperty(_dofEnabled);
    addProperty(_dofFocusDistance);
    addProperty(_dofFocusRange);
}

void MoleculeModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");
    fRenderable->registerClass<RenderableMolecule>("RenderableMolecule");
    fRenderable->registerClass<RenderableSimulationBox>("RenderableSimulationBox");

    global::callback::postSyncPreDraw->push_back([this]() { preDraw(); });
    global::callback::render->push_back([this]() { render(); });
}

void MoleculeModule::internalInitializeGL() {
    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();

    glGenTextures(1, &_colorTex);
    glBindTexture(GL_TEXTURE_2D, _colorTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA8,
        size.x,
        size.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _colorTex,
        0
    );

    glGenTextures(1, &_normalTex);
    glBindTexture(GL_TEXTURE_2D, _normalTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16,
        size.x,
        size.y,
        0,
        GL_RG,
        GL_UNSIGNED_SHORT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        _normalTex,
        0
    );

    glGenTextures(1, &_depthTex);
    glBindTexture(GL_TEXTURE_2D, _depthTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        size.x,
        size.y,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        _depthTex,
        0
    );
    glBindTexture(GL_TEXTURE_2D, 0);

    md_gl_initialize();
    md_gl_shaders_init(_shaders.get(), ShaderOutputSnippet.data());

    postprocessing::initialize(size.x, size.y);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void MoleculeModule::internalDeinitializeGL() {
    glDeleteTextures(1, &_depthTex);
    glDeleteTextures(1, &_normalTex);
    glDeleteTextures(1, &_colorTex);
    glDeleteFramebuffers(1, &_fbo);
    _fbo = 0;
    _colorTex = 0;
    _normalTex = 0;
    _depthTex = 0;
    postprocessing::shutdown();
    
    md_gl_shaders_free(_shaders.get());
    md_gl_shutdown();
}

GLuint MoleculeModule::fbo() const {
    return _fbo;
}

GLuint MoleculeModule::colorTexture() const {
    return _colorTex;
}

GLuint MoleculeModule::normalTexture() const {
    return _normalTex;
}

GLuint MoleculeModule::depthTexture() const {
    return _depthTex;
}

const md_gl_shaders_t& MoleculeModule::shaders() const {
    return *_shaders;
}

ThreadPool& MoleculeModule::threadPool() {
    return _threadPool;
}

void MoleculeModule::setViewMatrix(glm::mat4 v) {
    _viewMatrix = std::move(v);
}

void MoleculeModule::setProjectionMatrix(glm::mat4 p) {
    _projectionMatrix = std::move(p);
}

void MoleculeModule::preDraw() {
    // Check if resized
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();
    if (size.x == _width && size.y == _height) {
        return;
    }

    _width = size.x;
    _height = size.y;
    glBindTexture(GL_TEXTURE_2D, _colorTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA8,
        size.x,
        size.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glBindTexture(GL_TEXTURE_2D, _normalTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16,
        size.x,
        size.y,
        0,
        GL_RG,
        GL_UNSIGNED_SHORT,
        nullptr
    );
    glBindTexture(GL_TEXTURE_2D, _depthTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        size.x,
        size.y,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );
    glBindTexture(GL_TEXTURE_2D, 0);
    postprocessing::initialize(size.x, size.y);
}

void MoleculeModule::render() {
    GLint lastFbo;
    GLint lastDrawBufferCount = 0;
    GLenum lastDrawBuffers[8];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);
    for (int i = 0; i < ARRAY_SIZE(lastDrawBuffers); ++i) {
        GLint drawBuf;
        glGetIntegerv(GL_DRAW_BUFFER0+i, &drawBuf);
        if (!drawBuf) {
            break;
        }
        lastDrawBuffers[lastDrawBufferCount++] = static_cast<GLenum>(drawBuf);
    }

    postprocessing::Settings settings;
    settings.background.enabled = false;
    settings.background = { 0, 0, 0 };    
    settings.ambientOcclusion[0].enabled = _ssaoEnabled;
    settings.ambientOcclusion[0].intensity = _ssaoIntensity;
    settings.ambientOcclusion[0].radius = _ssaoRadius;
    settings.ambientOcclusion[0].horizonBias = _ssaoHorizonBias;
    settings.ambientOcclusion[0].normalBias = _ssaoNormalBias;
    settings.ambientOcclusion[1].enabled = _ssao2Enabled;
    settings.ambientOcclusion[1].intensity = _ssao2Intensity;
    settings.ambientOcclusion[1].radius = _ssao2Radius;
    settings.ambientOcclusion[1].horizonBias = _ssao2HorizonBias;
    settings.ambientOcclusion[1].normalBias = _ssao2NormalBias;
    settings.bloom.enabled = false;
    settings.depthOfField.enabled = _dofEnabled;
    settings.depthOfField.focusDepth = _dofFocusDistance;
    settings.depthOfField.focusScale = _dofFocusRange;
    settings.temporalReprojection.enabled = false;
    settings.tonemapping.enabled = true;
    settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
    settings.tonemapping.exposure = _exposure;
    settings.fxaa.enabled = true;
    settings.inputTextures.depth = _depthTex;
    settings.inputTextures.color = _colorTex;
    settings.inputTextures.normal = _normalTex;

    mat4_t V, P;
    std::memcpy(&V, &_viewMatrix, sizeof(mat4_t));
    std::memcpy(&P, &_projectionMatrix, sizeof(mat4_t));

    postprocessing::postprocess(settings, V, P);
    
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0, 0, 0, 1);
    glClearDepth(1.0);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
    glDrawBuffers(lastDrawBufferCount, lastDrawBuffers);
}

std::vector<documentation::Documentation> MoleculeModule::documentations() const {
    return {
        RenderableMolecule::Documentation(),
        RenderableSimulationBox::Documentation()
    };
}

} // namespace openspace
