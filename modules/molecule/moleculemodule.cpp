/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <glm/glm.hpp>

#include "mol/viamd/postprocessing.h"

namespace openspace {

constexpr const char* _loggerCat = "MoleculeModule";

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

MoleculeModule::MoleculeModule() : OpenSpaceModule(Name) {}

void MoleculeModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");
    fRenderable->registerClass<RenderableMolecule>("RenderableMolecule");
    fRenderable->registerClass<RenderableSimulationBox>("RenderableSimulationBox");

    // This is ugly, but I don't know if there is a prettier way to pass member functions
    auto x = [this]() { MoleculeModule::preDraw(); };
    global::callback::postSyncPreDraw->push_back(x);

    auto y = [this]() { MoleculeModule::postDraw(); };
    global::callback::postDraw->push_back(y);
}

void MoleculeModule::internalInitializeGL() {
    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glm::ivec2 size = global::windowDelegate->currentWindowSize();

    glGenTextures(1, &_colorTex);
    glBindTexture(GL_TEXTURE_2D, _colorTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _colorTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenTextures(1, &_normalTex);
    glBindTexture(GL_TEXTURE_2D, _normalTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16, size.x, size.y, 0, GL_RG, GL_UNSIGNED_SHORT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, _normalTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenTextures(1, &_depthTex);
    glBindTexture(GL_TEXTURE_2D, _depthTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, _depthTex, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        LERROR("Mold Framebuffer is not complete");

    md_gl_initialize();
    md_gl_shaders_init(&shaders, shader_output_snippet);

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
    
    md_gl_shaders_free(&shaders);
    md_gl_shutdown();
}

void MoleculeModule::preDraw() {
    GLint default_fbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &default_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0, 0, 0, 1);
    glClearDepth(1.0);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    glDrawBuffer(GL_BACK);
    glBindFramebuffer(GL_FRAMEBUFFER, default_fbo);
}

void MoleculeModule::postDraw() {
    
    postprocessing::Settings settings;
    settings.background_intensity = { 0, 0, 0 };
    settings.ambient_occlusion.enabled = _ssaoEnabled;
    settings.ambient_occlusion.intensity = _ssaoIntensity;
    settings.ambient_occlusion.radius = _ssaoRadius;
    settings.ambient_occlusion.bias = _ssaoBias;
    settings.bloom.enabled = false;
    settings.depth_of_field.enabled = false;
    settings.temporal_reprojection.enabled = false;
    settings.tonemapping.enabled = true;
    settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
    settings.tonemapping.exposure = _exposure;
    settings.input_textures.depth = _depthTex;
    settings.input_textures.color = _colorTex;
    settings.input_textures.normal = _normalTex;

    postprocessing::postprocess(settings, mat4_from_glm(projMatrix));
}

std::vector<documentation::Documentation> MoleculeModule::documentations() const {
    return {
        RenderableMolecule::Documentation(),
    };
}

} // namespace openspace
