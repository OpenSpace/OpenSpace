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
#include <modules/molecule/src/postprocessing.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>
#include <md_gl.h>

namespace {
    using namespace openspace;

    // Defining the shaders here since we don't want to need to include MOLD header files
    // in the module header, which would mean that the core would need to know about them
    std::unique_ptr<md_gl_shaders_t> _shaders = nullptr;

    constexpr std::string_view ShaderOutputSnippet = R"(
layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_normal;

vec2 encode_normal(vec3 n) {
  float p = sqrt(n.z * 8 + 8);
  return n.xy / p + 0.5;
}

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
  out_normal = vec4(encode_normal(view_normal), 0, 0);
  out_color = color;
}
)";

    constexpr Property::PropertyInfo SSAOEnabledInfo = {
        "Enabled",
        "Enabled",
        "Determines whether this SSAO pass should be enabled or not."
    };

    constexpr Property::PropertyInfo SSAOIntensityInfo = {
        "Intensity",
        "Intensity",
        "Controls the strength of the ambient occlusion effect. Higher values darken "
        "occluded areas more strongly."
    };

    constexpr Property::PropertyInfo SSAORadiusInfo = {
        "Radius",
        "Radius",
        "Sets the sampling radius for occlusion. Larger values produce broader, smoother "
        "shading, while smaller values create tighter shadows."
    };

    constexpr Property::PropertyInfo SSAOBiasInfo = {
        "HorizonBias",
        "Horizon Bias",
        "" // @TODO Missing documentation
    };

    constexpr Property::PropertyInfo SSAONormalBiasInfo = {
        "NormalBias",
        "Normal Bias",
        "" // @TODO Missing documentation
    };

    constexpr Property::PropertyInfo ExposureInfo = {
        "Exposure",
        "Exposure",
        "Controls the Exposure setting for the tonemap."
    };
} // namespace

namespace openspace {

MoleculeModule::SSAO::SSAO(PropertyOwner::PropertyOwnerInfo info)
    : PropertyOwner(info)
    , enabled(SSAOEnabledInfo, true)
    , intensity(SSAOIntensityInfo, 7.5f, 0.f, 100.f)
    , radius(SSAORadiusInfo, 1.f, 0.1f, 1000.f)
    , horizonBias(SSAOBiasInfo, 0.1f, 0.f, 1.f)
    , normalBias(SSAONormalBiasInfo, 1.f, 0.f, 1.f)
{
    addProperty(enabled);
    addProperty(intensity);
    addProperty(radius);
    addProperty(horizonBias);
    addProperty(normalBias);
}

MoleculeModule::MoleculeModule()
    : OpenSpaceModule(Name)
    , _ssao({ "SSAO", "SSAO", "First SSAO pass" })
    , _ssao2({ "SSAO2", "SSAO 2", "Second SSAO pass" })
    , _exposure(ExposureInfo, 1.f, 0.1f, 10.f)
    , _threadPool(std::max(1U, std::thread::hardware_concurrency() - 1))
{
    // @TODO (2026-02-11, abock) These settings should be made configurable from the cfg
    // file at some point
    addPropertySubOwner(_ssao);
    _ssao2.radius = 10.f;
    _ssao2.horizonBias = 0.f;
    addPropertySubOwner(_ssao2);

    addProperty(_exposure);
}

void MoleculeModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");
    fRenderable->registerClass<RenderableMolecule>("RenderableMolecule");
    fRenderable->registerClass<RenderableSimulationBox>("RenderableSimulationBox");

    global::callback::postSyncPreDraw->push_back([this]() { preDraw(); });
    global::callback::render->push_back(
        [this](const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
               const glm::mat4& projectionMatrix)
        {
            render(sceneMatrix, viewMatrix, projectionMatrix);
        }
    );
}

void MoleculeModule::internalDeinitializeGL() {
    ghoul_assert(_initializeCounter == 0, "Renderable type did not deinitialize shaders");

    _depthTex = nullptr;
    _normalTex = nullptr;
    _colorTex = nullptr;
    glDeleteFramebuffers(1, &_fbo);
    _fbo = 0;
    postprocessing::shutdown();

    if (_shaders) {
        md_gl_shaders_free(_shaders.get());
        md_gl_shutdown();
    }
}

void MoleculeModule::initializeShaders() {
    _initializeCounter++;

    if (_initializeCounter > 1) {
        return;
    }

    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();

    _colorTex = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = glm::uvec3(size.x, size.y, 1),
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::RGBA,
            .dataType = GL_UNSIGNED_BYTE
        },
        ghoul::opengl::Texture::SamplerInit{
            .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge
        }
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_colorTex,
        0
    );

    _normalTex = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = glm::uvec3(size.x, size.y, 1),
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::RG,
            .dataType = GL_UNSIGNED_SHORT
        },
        ghoul::opengl::Texture::SamplerInit{
            .filter = ghoul::opengl::Texture::FilterMode::Nearest,
            .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge
        }
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        *_normalTex,
        0
    );

    _depthTex = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = glm::uvec3(size.x, size.y, 1),
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::DepthComponent,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge
        }
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        *_depthTex,
        0
    );

    md_gl_initialize();
    _shaders = std::make_unique<md_gl_shaders_t>();
    md_gl_shaders_init(_shaders.get(), ShaderOutputSnippet.data());

    postprocessing::initialize(size.x, size.y);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void MoleculeModule::deinitializeShaders() {
    _initializeCounter--;
}

GLuint MoleculeModule::fbo() const {
    return _fbo;
}

const md_gl_shaders_t& MoleculeModule::shaders() const {
    return *_shaders;
}

ThreadPool& MoleculeModule::threadPool() {
    return _threadPool;
}

void MoleculeModule::preDraw() {
    if (_initializeCounter == 0) {
        return;
    }

    // Check if resized
    const glm::ivec2 size = global::windowDelegate->currentWindowSize();
    if (size.x == _width && size.y == _height) {
        return;
    }

    _width = size.x;
    _height = size.y;
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);

    _colorTex->resize(glm::uvec3(size.x, size.y, 1));
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_colorTex,
        0
    );

    _normalTex->resize(glm::uvec3(size.x, size.y, 1));
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        *_normalTex,
        0
    );

    _depthTex->resize(glm::uvec3(size.x, size.y, 1));
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        *_depthTex,
        0
    );

    postprocessing::resize(size.x, size.y);
}

void MoleculeModule::render(const glm::mat4&, const glm::mat4& viewMatrix,
                            const glm::mat4& projectionMatrix)
{
    if (_initializeCounter == 0) {
        return;
    }

    GLint lastFbo;
    GLint lastDrawBufferCount = 0;
    std::array<GLenum, 8> lastDrawBuffers;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);
    for (int i = 0; i < static_cast<int>(lastDrawBuffers.size()); i++) {
        GLint drawBuf;
        glGetIntegerv(GL_DRAW_BUFFER0 + i, &drawBuf);
        if (!drawBuf) {
            break;
        }
        lastDrawBuffers[lastDrawBufferCount++] = static_cast<GLenum>(drawBuf);
    }

    postprocessing::Settings settings;
    settings.background.enabled = false;
    settings.ambientOcclusion[0].enabled = _ssao.enabled;
    settings.ambientOcclusion[0].intensity = _ssao.intensity;
    settings.ambientOcclusion[0].radius = _ssao.radius;
    settings.ambientOcclusion[0].horizonBias = _ssao.horizonBias;
    settings.ambientOcclusion[0].normalBias = _ssao.normalBias;
    settings.ambientOcclusion[1].enabled = _ssao2.enabled;
    settings.ambientOcclusion[1].intensity = _ssao2.intensity;
    settings.ambientOcclusion[1].radius = _ssao2.radius;
    settings.ambientOcclusion[1].horizonBias = _ssao2.horizonBias;
    settings.ambientOcclusion[1].normalBias = _ssao2.normalBias;
    settings.bloom.enabled = false;
    settings.depthOfField.enabled = false;
    settings.temporalReprojection.enabled = false;
    settings.tonemapping.enabled = true;
    settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
    settings.tonemapping.exposure = _exposure;
    settings.fxaa.enabled = true;
    settings.inputTextures.depth = _depthTex.get();
    settings.inputTextures.color = _colorTex.get();
    settings.inputTextures.normal = _normalTex.get();

    postprocessing::postprocess(settings, viewMatrix, projectionMatrix);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, _fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClearDepth(1.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
    glDrawBuffers(lastDrawBufferCount, lastDrawBuffers.data());
}

std::vector<Documentation> MoleculeModule::documentations() const {
    return {
        RenderableMolecule::Documentation(),
        RenderableSimulationBox::Documentation()
    };
}

} // namespace openspace
