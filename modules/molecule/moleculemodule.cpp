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

#include <modules/molecule/renderablemolecule.h>
#include <modules/molecule/renderablesimulationbox.h>
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
#include <modules/molecule/mol/viamd/postprocessing.h>

namespace {
constexpr const char* shader_output_snippet = R"(
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
constexpr const char* _loggerCat = "MoleculeModule";

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

}

namespace openspace {

MoleculeModule::MoleculeModule() :
    OpenSpaceModule(Name),
    _ssaoEnabled(SSAOEnabledInfo, true),
    _ssaoIntensity(SSAOIntensityInfo, 4.f, 0.f, 100.f),
    _ssaoRadius(SSAORadiusInfo, 1.f, 0.1f, 10.f),
    _ssaoHorizonBias(SSAOBiasInfo, 0.1f, 0.0f, 1.0f),
    _ssaoNormalBias(SSAONormalBiasInfo, 1.0f, 0.0f, 1.0f),
    _ssao2Enabled(SSAO2EnabledInfo, true),
    _ssao2Intensity(SSAO2IntensityInfo, 4.f, 0.f, 100.f),
    _ssao2Radius(SSAO2RadiusInfo, 10.f, 10.f, 1000.f),
    _ssao2HorizonBias(SSAO2BiasInfo, 0.0f, 0.0f, 1.0f),
    _ssao2NormalBias(SSAO2NormalBiasInfo, 1.0f, 0.0f, 0.0f),
    _exposure(ExposureInfo, 0.3f, 0.1f, 10.f),
    _dofEnabled(DOFEnabledInfo, false),
    _dofFocusDistance(DOFFocusDistanceInfo, 0.5f, 0.f, 1.f),
    _dofFocusRange(DOFFocusRangeInfo, 0.1f, 0.f, 10.f),
    _threadPool(std::max(1U, std::thread::hardware_concurrency() - 1))
{
    _shaders.reset(new md_gl_shaders_t());
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

MoleculeModule::~MoleculeModule() {
}

void MoleculeModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");
    fRenderable->registerClass<RenderableMolecule>("RenderableMolecule");
    fRenderable->registerClass<RenderableSimulationBox>("RenderableSimulationBox");

    // This is ugly, but I don't know if there is a prettier way to pass member functions
    global::callback::postSyncPreDraw->push_back([this]() { MoleculeModule::preDraw(); });
    global::callback::render->push_back([this]() { MoleculeModule::postDraw(); });
}

void MoleculeModule::internalInitializeGL() {
    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();

    glGenTextures(1, &_colorTex);
    glBindTexture(GL_TEXTURE_2D, _colorTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _colorTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenTextures(1, &_normalTex);
    glBindTexture(GL_TEXTURE_2D, _normalTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16, size.x, size.y, 0, GL_RG, GL_UNSIGNED_SHORT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, _normalTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenTextures(1, &_depthTex);
    glBindTexture(GL_TEXTURE_2D, _depthTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, _depthTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Mold Framebuffer is not complete");

    glGetError();

    md_gl_initialize();
    md_gl_shaders_init(_shaders.get(), shader_output_snippet);

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

void MoleculeModule::preDraw() {
    // Check if resized
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();
    if (size.x != _width || size.y != _height) {
        _width = size.x;
        _height = size.y;
        glBindTexture(GL_TEXTURE_2D, _colorTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
        glBindTexture(GL_TEXTURE_2D, _normalTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16, size.x, size.y, 0, GL_RG, GL_UNSIGNED_SHORT, nullptr);
        glBindTexture(GL_TEXTURE_2D, _depthTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
        glBindTexture(GL_TEXTURE_2D, 0);
        postprocessing::initialize(size.x, size.y);
    }
}

void MoleculeModule::postDraw() {
    GLint last_fbo;
    GLint  last_draw_buffer_count = 0;
    GLenum last_draw_buffers[8];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    for (int i = 0; i < ARRAY_SIZE(last_draw_buffers); ++i) {
        GLint draw_buf;
        glGetIntegerv(GL_DRAW_BUFFER0+i, &draw_buf);
        if (!draw_buf) {
            break;
        }
        last_draw_buffers[last_draw_buffer_count++] = (GLenum)draw_buf;
    }



    
    postprocessing::Settings settings;
    settings.background.enabled = false;
    settings.background = { 0, 0, 0 };    
    settings.ambient_occlusion[0].enabled = _ssaoEnabled;
    settings.ambient_occlusion[0].intensity = _ssaoIntensity;
    settings.ambient_occlusion[0].radius = _ssaoRadius;
    settings.ambient_occlusion[0].horizon_bias = _ssaoHorizonBias;
    settings.ambient_occlusion[0].normal_bias = _ssaoNormalBias;
    settings.ambient_occlusion[1].enabled = _ssao2Enabled;
    settings.ambient_occlusion[1].intensity = _ssao2Intensity;
    settings.ambient_occlusion[1].radius = _ssao2Radius;
    settings.ambient_occlusion[1].horizon_bias = _ssao2HorizonBias;
    settings.ambient_occlusion[1].normal_bias = _ssao2NormalBias;
    settings.bloom.enabled = false;
    settings.depth_of_field.enabled = _dofEnabled;
    settings.depth_of_field.focus_depth = _dofFocusDistance;
    settings.depth_of_field.focus_scale = _dofFocusRange;
    settings.temporal_reprojection.enabled = false;
    settings.tonemapping.enabled = true;
    settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
    settings.tonemapping.exposure = _exposure;
    settings.fxaa.enabled = true;
    settings.input_textures.depth = _depthTex;
    settings.input_textures.color = _colorTex;
    settings.input_textures.normal = _normalTex;

    mat4_t V, P;
    MEMCPY(&V, &_V, sizeof(mat4_t));
    MEMCPY(&P, &_P, sizeof(mat4_t));

    postprocessing::postprocess(settings, V, P);
    
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0, 0, 0, 1);
    glClearDepth(1.0);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glDrawBuffers(last_draw_buffer_count, last_draw_buffers);
}

std::vector<documentation::Documentation> MoleculeModule::documentations() const {
    return {
        RenderableMolecule::Documentation(),
    };
}

} // namespace openspace
