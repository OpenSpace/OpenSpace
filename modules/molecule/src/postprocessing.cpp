/*-----------------------------------------------------------------------
  Copyright (c) 2014, NVIDIA. All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Neither the name of its contributors may be used to endorse
     or promote products derived from this software without specific
     prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------*/

// Shaders for HBAO are based on nVidias examples and are copyrighted as stated above

#include <modules/molecule/src/postprocessing.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/uniformcache.h>
#include <core/md_allocator.h>
#include <core/md_str.h>
#include <core/md_log.h>
#include <core/md_str_builder.h>
#include <core/md_vec_math.h>
#include <cfloat>
#include <cstdio>
#include <cstring>

namespace {
    // @TODO: Use half-res render targets for SSAO
    // @TODO: Use shared textures for all postprocessing operations
    // @TODO: Use some kind of unified pipeline for all post processing operations

    constexpr int VelocityTileSize = 8;

    struct {
        GLuint vao = 0;
        uint32_t texWidth = 0;
        uint32_t texHeight = 0;

        struct {
            GLuint fbo = 0;
            std::unique_ptr<ghoul::opengl::Texture> texColor[2];
            // These are dedicated and cannot be use as intermediate buffers by other
            // shaders
            std::unique_ptr<ghoul::opengl::Texture> texTemporalBuffer[2];
        } targets;

        struct {
            GLuint fbo = 0;
            std::unique_ptr<ghoul::opengl::Texture> texTilemax;
            std::unique_ptr<ghoul::opengl::Texture> texNeighbormax;
        } velocity;

        struct {
            GLuint fbo = 0;
            std::unique_ptr<ghoul::opengl::Texture> texture;
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(clipInfo, texDepth, isPerspective) uniforms;
        } linearDepth;

        struct {
            std::unique_ptr<ghoul::opengl::Texture> texRandom;
            GLuint uboHbaoData = 0;

            struct {
                GLuint fbo = 0;
                std::unique_ptr<ghoul::opengl::Texture> texture;
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texLinearDepth, texNormal, texRandom, tcScale,
                    isPerspective) uniforms;
            } hbao;

            struct {
                GLuint fbo = 0;
                std::unique_ptr<ghoul::opengl::Texture> texture;
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texLinearDepth, texAo, sharpness, invResDir,
                    tcScale) uniforms;
            } blur;
        } ssao;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(texDepth, texColor, texNormal, invProjMat, lightDir,
                lightCol) uniforms;
        } shading;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(texHalfRes, texColor, texDepth, texelSize, focusDepth,
                focusScale, time) uniforms;

            struct {
                GLuint fbo = 0;
                std::unique_ptr<ghoul::opengl::Texture> colorCoc;
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texDepth, texColor, focusPoint, focusScale) uniforms;
            } halfRes;
        } bokehDof;

        struct {
            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(tex) uniforms;
            } passthrough;

            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(tex, exposure, gamma) uniforms;
            } exposureGamma;

            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(tex, exposure, gamma) uniforms;
            } filmic;

            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(tex, exposure, gamma) uniforms;
            } aces;
        } tonemapping;

        struct {
            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texLinearDepth, texMain, texPrev, texVel, texVelNeighbormax,
                    texelSize, time, feedbackMin, feedbackMax, motionScale,
                    jitterUv) uniforms;
            } withMotionBlur;
            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texLinearDepth, texMain, texPrev, texVel, texelSize,
                    feedbackMin, feedbackMax, jitterUv) uniforms;
            } noMotionBlur;
        } temporal;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(tex, inverseScreenSize) uniforms;
        } fxaa;

        struct {
            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(tex) uniforms;
            } tex;

            struct {
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texColor, texDepth) uniforms;
            } texDepth;
        } blit;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(texVel, texVelTexelSize) uniforms;
        } blitTilemax;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(texVel, texVelTexelSize) uniforms;
        } blitNeighbormax;

        struct {
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(tex) uniforms;
        } sharpen;
    } glObj;

    constexpr float halton(int index, int base) {
        float f = 1;
        float r = 0;
        const float ifb = 1.f / base;
        while (index > 0) {
            f = f * ifb;
            r = r + f * static_cast<float>(index % base);
            index = static_cast<int>(index * ifb);
        }
        return r;
    }

    constexpr bool isOrthoProjMatrix(const mat4_t& P) {
        return P.elem[2][3] == 0.f;
    }

    constexpr bool isOrthoProjMatrix(const glm::mat4& P) {
        return P[2][3] == 0.f;
    }

    void blitTexture(const ghoul::opengl::Texture& tex) {
        ghoul::opengl::TextureUnit texUnit;
        texUnit.activate();
        glBindTexture(GL_TEXTURE_2D, tex);

        glObj.blit.tex.program->activate();
        glObj.blit.tex.program->setUniform(glObj.blit.tex.uniforms.tex, 0);

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
    }

    void blitTexture(const ghoul::opengl::Texture& tex,
                     const ghoul::opengl::Texture& depth)
    {
        ghoul::opengl::TextureUnit texUnit;
        texUnit.activate();
        glBindTexture(GL_TEXTURE_2D, tex);

        ghoul::opengl::TextureUnit depthUnit;
        depthUnit.activate();
        glBindTexture(GL_TEXTURE_2D, depth);

        glObj.blit.texDepth.program->activate();
        glObj.blit.texDepth.program->setUniform(
            glObj.blit.texDepth.uniforms.texColor,
            texUnit
        );
        glObj.blit.texDepth.program->setUniform(
            glObj.blit.texDepth.uniforms.texDepth,
            depthUnit
        );

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
    }

    struct HBAOData {
        float radiusToScreen = 0.f;
        float negInvR2 = 0.f;
        float nDotVBias = 0.f;
        float nInfluence = 0.f;

        float aoMultiplier = 0.f;
        float powExponent = 0.f;
        vec2_t invFullRes = { 0.f, 0.f };

        vec4_t projInfo = { 0.f, 0.f, 0.f, 0.f };

        // From Intel SSAO
        const std::array<vec4_t, 32> samplePattern = {
             0.78488064f, 0.56661671f, 1.5f, -0.126083f, 0.26022232f, -0.29575172f, 1.5f,
             -1.06403f, 0.10459357f, 0.08372527f, 1.11f, -2.730563f, -0.682868f,
             0.04963045f, 1.09f, -0.498827f, -0.13570161f, -0.64190155f, 1.25f,
             -0.532765f, -0.26193795f, -0.08205118f, 0.67f, -1.783245f, -0.61177456f,
             0.66664219f, 0.71f, -0.044234f, 0.43675563f, 0.25119025f, 0.61f, -1.167283f,
             0.07884444f, 0.86618668f, 0.64f, -0.459002f, -0.12790935f, -0.29869005f,
             0.6f, -1.729424f, -0.04031125f, 0.02413622f, 0.6f, -4.792042f, 0.16201244f,
             -0.52851415f, 0.79f, -1.067055f, -0.70991218f, 0.47301072f, 0.64f,
             -0.335236f, 0.03277707f, -0.2234969f, 0.6f, -1.982384f, 0.68921727f,
             0.36800742f, 0.63f, -0.266718f, 0.29251814f, 0.37775412f, 0.61f, -1.42252f,
             -0.12224089f, 0.96582592f, 0.6f, -0.426142f, 0.11071457f, -0.16131058f, 0.6f,
             -2.165947f, 0.46562141f, -0.59747696f, 0.6f, -0.18976f, -0.51548797f,
             0.11804193f, 0.6f, -1.2468f, 0.89141309f, -0.42090443f, 0.6f, 0.028192f,
             -0.3240253f, -0.01591529f, 0.6f, -1.543018f, 0.60771245f, 0.41635221f, 0.6f,
             -0.605411f, 0.02379565f, -0.08239821f, 0.6f, -3.809046f, 0.48951152f,
             -0.23657045f, 0.6f, -1.189011f, -0.17611565f, -0.81696892f, 0.6f, -0.513724f,
             -0.33930185f, -0.20732205f, 0.6f, -1.698047f, -0.91974425f, 0.05403209f,
             0.6f, 0.062246f, -0.15064627f, -0.14949332f, 0.6f, -1.896062f, 0.53180975f,
             -0.35210401f, 0.6f, -0.758838f, 0.41487166f, 0.81442589f, 0.6f, -0.505648f,
             -0.24106961f, -0.32721516f, 0.6f, -1.665244f
        };
    };
} // namespace

namespace postprocessing {

void initialize(int width, int height) {
    glGenVertexArrays(1, &glObj.vao);

    glObj.linearDepth.program = ghoul::opengl::ProgramObject::Build(
        "Linearize Depth",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/linearize_depth_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.linearDepth.program,
        glObj.linearDepth.uniforms
    );

    glObj.linearDepth.texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red,
        GL_R32F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::LinearMipMap,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.linearDepth.texture->uploadTexture();

    glGenFramebuffers(1, &glObj.linearDepth.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.linearDepth.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.linearDepth.texture,
        0
    );
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glObj.targets.texColor[0] = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_R11F_G11F_B10F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.targets.texColor[0]->uploadTexture();
    glObj.targets.texColor[1] = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_R11F_G11F_B10F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.targets.texColor[1]->uploadTexture();

    glObj.targets.texTemporalBuffer[0] = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_R11F_G11F_B10F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.targets.texTemporalBuffer[0]->uploadTexture();

    glObj.targets.texTemporalBuffer[1] = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_R11F_G11F_B10F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.targets.texTemporalBuffer[1]->uploadTexture();

    glGenFramebuffers(1, &glObj.targets.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.targets.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.targets.texColor[0],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        *glObj.targets.texColor[1],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT2,
        GL_TEXTURE_2D,
        *glObj.targets.texTemporalBuffer[0],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT3,
        GL_TEXTURE_2D,
        *glObj.targets.texTemporalBuffer[1],
        0
    );

    GLenum buffers[] = {
        GL_COLOR_ATTACHMENT0,
        GL_COLOR_ATTACHMENT1,
        GL_COLOR_ATTACHMENT2,
        GL_COLOR_ATTACHMENT3
    };
    glDrawBuffers(4, buffers);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glObj.texWidth = width;
    glObj.texHeight = height;

    glObj.sharpen.program = ghoul::opengl::ProgramObject::Build(
        "Sharpen",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/sharpen_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.sharpen.program, glObj.sharpen.uniforms);

    ghoul::Dictionary blur;
    blur.setValue("UseMotionBlur", 1);
    glObj.temporal.withMotionBlur.program = ghoul::opengl::ProgramObject::Build(
        "Temporal AA + Motion blur",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/temporal_aa_fs.glsl"),
        blur
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.temporal.withMotionBlur.program,
        glObj.temporal.withMotionBlur.uniforms
    );

    ghoul::Dictionary noBlur;
    noBlur.setValue("UseMotionBlur", 0);
    glObj.temporal.noMotionBlur.program = ghoul::opengl::ProgramObject::Build(
        "Temporal AA",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/temporal_aa_fs.glsl"),
        noBlur
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.temporal.noMotionBlur.program,
        glObj.temporal.noMotionBlur.uniforms
    );

    glObj.blit.tex.program = ghoul::opengl::ProgramObject::Build(
        "Blit Texture",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/blit_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.blit.tex.program,
        glObj.blit.tex.uniforms
    );

    glObj.blit.texDepth.program = ghoul::opengl::ProgramObject::Build(
        "Blit Texture with Depth",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/blit_depth_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.blit.texDepth.program,
        glObj.blit.texDepth.uniforms
    );

    glObj.shading.program = ghoul::opengl::ProgramObject::Build(
        "Deferred Shading",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/deferred_shading_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.shading.program, glObj.shading.uniforms);

    glObj.fxaa.program = ghoul::opengl::ProgramObject::Build(
        "FXAA",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/fxaa_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.fxaa.program, glObj.fxaa.uniforms);

    glObj.tonemapping.passthrough.program = ghoul::opengl::ProgramObject::Build(
        "Tonemap Passthrough",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/tonemap_passthrough_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.tonemapping.passthrough.program,
        glObj.tonemapping.passthrough.uniforms
    );

    glObj.tonemapping.exposureGamma.program = ghoul::opengl::ProgramObject::Build(
        "Tonemap Exposure Gamma",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/tonemap_exposure_gamma_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.tonemapping.exposureGamma.program,
        glObj.tonemapping.exposureGamma.uniforms
    );

    glObj.tonemapping.filmic.program = ghoul::opengl::ProgramObject::Build(
        "Tonemap Filmic",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/tonemap_filmic_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.tonemapping.filmic.program,
        glObj.tonemapping.filmic.uniforms
    );

    glObj.tonemapping.aces.program = ghoul::opengl::ProgramObject::Build(
        "Tonemap ACES",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/tonemap_aces_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.tonemapping.aces.program,
        glObj.tonemapping.aces.uniforms
    );

    glObj.bokehDof.halfRes.program = ghoul::opengl::ProgramObject::Build(
        "DOF prepass",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/dof_halfres_prepass_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.bokehDof.halfRes.program,
        glObj.bokehDof.halfRes.uniforms
    );

    glObj.bokehDof.halfRes.colorCoc = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width / 2, height / 2, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA16F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.bokehDof.halfRes.colorCoc->uploadTexture();
    
    glGenFramebuffers(1, &glObj.bokehDof.halfRes.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.bokehDof.halfRes.colorCoc,
        0
    );
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glObj.bokehDof.program = ghoul::opengl::ProgramObject::Build(
        "DOF Bokeh",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/dof_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.bokehDof.program,
        glObj.bokehDof.uniforms
    );


    ghoul::Dictionary tileSize;
    tileSize.setValue("TileSize", VelocityTileSize);
    glObj.blitTilemax.program = ghoul::opengl::ProgramObject::Build(
        "Tilemax",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/vel_tilemax_fs.glsl"),
        tileSize
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.blitTilemax.program,
        glObj.blitTilemax.uniforms
    );

    glObj.blitNeighbormax.program = ghoul::opengl::ProgramObject::Build(
        "Tilemax",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/vel_neighbormax_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.blitNeighbormax.program,
        glObj.blitNeighbormax.uniforms
    );

    glObj.velocity.texTilemax = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(
            glObj.texWidth / VelocityTileSize,
            glObj.texHeight / VelocityTileSize,
            1
        ),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RG,
        GL_RG16F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.velocity.texTilemax->uploadTexture();

    glObj.velocity.texNeighbormax = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(
            glObj.texWidth / VelocityTileSize,
            glObj.texHeight / VelocityTileSize,
            1
        ),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RG,
        GL_RG16F,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.velocity.texNeighbormax->uploadTexture();

    glGenFramebuffers(1, &glObj.velocity.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.velocity.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.velocity.texTilemax,
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        *glObj.velocity.texNeighbormax,
        0
    );

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glObj.ssao.hbao.program = ghoul::opengl::ProgramObject::Build(
        "SSAO Perspective",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/ssao_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.ssao.hbao.program,
        glObj.ssao.hbao.uniforms
    );

    glObj.ssao.blur.program = ghoul::opengl::ProgramObject::Build(
        "SSAO Blur",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/ssao_blur_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.ssao.blur.program,
        glObj.ssao.blur.uniforms
    );

    // Initialize random textures
    constexpr int AORandomTexSize = 4;
    constexpr int BufferSize = AORandomTexSize * AORandomTexSize;
    std::array<short, BufferSize * 4> buffer;

    for (int i = 0; i < BufferSize; i++) {
        constexpr int Scale = 1 << 15;
        float rand1 = halton(i + 1, 2);
        float rand2 = halton(i + 1, 3);
        float angle = rand1 * glm::two_pi<float>();

        buffer[i * 4 + 0] = static_cast<signed short>(Scale * std::cos(angle));
        buffer[i * 4 + 1] = static_cast<signed short>(Scale * std::sin(angle));
        buffer[i * 4 + 2] = static_cast<signed short>(Scale * rand2);
        buffer[i * 4 + 3] = 0;
    }

    glObj.ssao.texRandom = std::make_unique<ghoul::opengl::Texture>(
        buffer.data(),
        glm::uvec3(AORandomTexSize, AORandomTexSize, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA16_SNORM,
        GL_SHORT,
        ghoul::opengl::Texture::FilterMode::Nearest
    );
    glObj.ssao.texRandom->uploadTexture();

    glObj.ssao.hbao.texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red,
        GL_R8,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.ssao.hbao.texture->uploadTexture();

    glObj.ssao.blur.texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red,
        GL_R8,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
    glObj.ssao.blur.texture->uploadTexture();

    glGenFramebuffers(1, &glObj.ssao.hbao.fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, glObj.ssao.hbao.fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.ssao.hbao.texture,
        0
    );

    glGenFramebuffers(1, &glObj.ssao.blur.fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, glObj.ssao.blur.fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *glObj.ssao.blur.texture,
        0
    );
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    glGenBuffers(1, &glObj.ssao.uboHbaoData);
    glBindBuffer(GL_UNIFORM_BUFFER, glObj.ssao.uboHbaoData);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), nullptr, GL_DYNAMIC_DRAW);
}

void resize(int width, int height) {
    glObj.texWidth = width;
    glObj.texHeight = height;

    glObj.linearDepth.texture->resize(glm::uvec3(width, height, 1));
    glObj.targets.texColor[0]->resize(glm::uvec3(width, height, 1));
    glObj.targets.texColor[1]->resize(glm::uvec3(width, height, 1));
    glObj.targets.texTemporalBuffer[0]->resize(glm::uvec3(width, height, 1));
    glObj.targets.texTemporalBuffer[1]->resize(glm::uvec3(width, height, 1));
    glObj.ssao.hbao.texture->resize(glm::uvec3(width, height, 1));
    glObj.ssao.blur.texture->resize(glm::uvec3(width, height, 1));
    glObj.bokehDof.halfRes.colorCoc->resize(glm::uvec3(width / 2, height / 2, 1));
    glObj.velocity.texTilemax->resize(glm::uvec3(
        glObj.texWidth / VelocityTileSize,
        glObj.texHeight / VelocityTileSize,
        1
    ));
    glObj.velocity.texNeighbormax->resize(glm::uvec3(
        glObj.texWidth / VelocityTileSize,
        glObj.texHeight / VelocityTileSize,
        1
    ));
}

void shutdown() {
    glDeleteVertexArrays(1, &glObj.vao);
    glDeleteFramebuffers(1, &glObj.targets.fbo);
    glObj.targets.texColor[0] = nullptr;
    glObj.targets.texColor[1] = nullptr;
    glObj.targets.texTemporalBuffer[0] = nullptr;
    glObj.targets.texTemporalBuffer[1] = nullptr;
    glDeleteFramebuffers(1, &glObj.velocity.fbo);
    glObj.velocity.texTilemax = nullptr;
    glObj.velocity.texNeighbormax = nullptr;
    glDeleteFramebuffers(1, &glObj.linearDepth.fbo);
    glObj.linearDepth.program = nullptr;
    glObj.linearDepth.texture = nullptr;
    glObj.ssao.texRandom = nullptr;
    glDeleteBuffers(1, &glObj.ssao.uboHbaoData);
    glDeleteFramebuffers(1, &glObj.ssao.hbao.fbo);
    glObj.ssao.hbao.texture = nullptr;
    glObj.ssao.hbao.program = nullptr;
    glDeleteFramebuffers(1, &glObj.ssao.blur.fbo);
    glObj.ssao.blur.texture = nullptr;
    glObj.ssao.blur.program = nullptr;
    glObj.shading.program = nullptr;
    glObj.bokehDof.program = nullptr;
    glDeleteFramebuffers(1, &glObj.bokehDof.halfRes.fbo);
    glObj.bokehDof.halfRes.colorCoc = nullptr;
    glObj.bokehDof.halfRes.program = nullptr;
    glObj.tonemapping.passthrough.program = nullptr;
    glObj.tonemapping.exposureGamma.program = nullptr;
    glObj.tonemapping.filmic.program = nullptr;
    glObj.tonemapping.aces.program = nullptr;
    glObj.temporal.withMotionBlur.program = nullptr;
    glObj.temporal.noMotionBlur.program = nullptr;
    glObj.fxaa.program = nullptr;
    glObj.blit.tex.program = nullptr;
    glObj.blit.texDepth.program = nullptr;
    glObj.blitTilemax.program = nullptr;
    glObj.blitNeighbormax.program = nullptr;
    glObj.sharpen.program = nullptr;
}

void applySsao(const ghoul::opengl::Texture& linearDepthTex,
               const ghoul::opengl::Texture& normalTex, const glm::mat4& projMatrix,
               float intensity, float radius, float bias, float normalBias)
{
    const bool isOrtho = isOrthoProjMatrix(projMatrix);
    const float sharpness = 3.f / std::sqrt(radius);

    int lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);
    int lastViewport[4];
    glGetIntegerv(GL_VIEWPORT, lastViewport);

    int width = lastViewport[2];
    int height = lastViewport[3];

    vec4_t projInfo;
    float projScl;

    const float* projData = glm::value_ptr(projMatrix);
    const bool ortho = isOrthoProjMatrix(projMatrix);
    if (!ortho) {
        projInfo = {
            2.f / (projData[4 * 0 + 0]),                        // (x) * (R - L)/N
            2.f / (projData[4 * 1 + 1]),                        // (y) * (T - B)/N
            -(1.f - projData[4 * 2 + 0]) / projData[4 * 0 + 0], // L/N
            -(1.f + projData[4 * 2 + 1]) / projData[4 * 1 + 1]  // B/N
        };
        projScl = static_cast<float>(height) * projData[4 * 1 + 1] * 0.5f;
    }
    else {
        projInfo = {
            2.f / (projData[4 * 0 + 0]),                        // ((x) * R - L)
            2.f / (projData[4 * 1 + 1]),                        // ((y) * T - B)
            -(1.f + projData[4 * 3 + 0]) / projData[4 * 0 + 0], // L
            -(1.f - projData[4 * 3 + 1]) / projData[4 * 1 + 1]  // B
        };
        projScl = static_cast<float>(height) / projInfo.y;
    }

    HBAOData data = {
        .radiusToScreen = radius * 0.5f * projScl,
        .negInvR2 = -1.f / (radius * radius),
        .nDotVBias = std::clamp(bias, 0.f, 1.f - std::numeric_limits<float>::epsilon()),
        .nInfluence = std::clamp(normalBias, 0.f, 1.0f),
        .aoMultiplier = 1.f / (1.f - data.nDotVBias),
        .powExponent = std::max(intensity, 0.f),
        .invFullRes = {
            1.f / static_cast<float>(width),
            1.f / static_cast<float>(height)
        },
        .projInfo = projInfo
    };

    glBindBuffer(GL_UNIFORM_BUFFER, glObj.ssao.uboHbaoData);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), &data, GL_DYNAMIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.ssao.hbao.fbo);
    glViewport(0, 0, glObj.texWidth, glObj.texHeight);
    glClearColor(1.f, 1.f, 1.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);

    glViewport(0, 0, width, height);


    ghoul::opengl::TextureUnit linearDepthUnit;
    linearDepthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    ghoul::opengl::TextureUnit normalUnit;
    normalUnit.activate();
    glBindTexture(GL_TEXTURE_2D, normalTex);

    // HBAO
    {
        ghoul::opengl::ProgramObject& program = *glObj.ssao.hbao.program;
        auto& uniforms = glObj.ssao.hbao.uniforms;

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "HBAO");
        program.activate();

        ghoul::opengl::TextureUnit randomUnit;
        randomUnit.activate();
        glObj.ssao.texRandom->bind();

        glBindBufferBase(GL_UNIFORM_BUFFER, 0, glObj.ssao.uboHbaoData);
        glUniformBlockBinding(
            program,
            glGetUniformBlockIndex(program, "controlBuffer"),
            0
        );
        program.setUniform(uniforms.texLinearDepth, linearDepthUnit);
        program.setUniform(uniforms.texNormal, normalUnit);
        program.setUniform(uniforms.texRandom, randomUnit);
        program.setUniform(uniforms.isPerspective, isOrtho ? 0 : 1);
        program.setUniform(
            uniforms.tcScale,
            static_cast<float>(width) / static_cast<float>(glObj.texWidth),
            static_cast<float>(height) / static_cast<float>(glObj.texHeight)
        );

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glPopDebugGroup();
    }

    // Blur
    {
        ghoul::opengl::ProgramObject& program = *glObj.ssao.blur.program;
        auto& uniforms = glObj.ssao.blur.uniforms;

        program.activate();
        program.setUniform(uniforms.texLinearDepth, linearDepthUnit);
        program.setUniform(uniforms.texAo, normalUnit);
        program.setUniform(uniforms.sharpness, sharpness);
        program.setUniform(uniforms.invResDir, 1.f / glObj.texWidth, 0.f);
        program.setUniform(
            uniforms.tcScale,
            static_cast<float>(width) / static_cast<float>(glObj.texWidth),
            static_cast<float>(height) / static_cast<float>(glObj.texHeight)
        );

        // First pass
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 1st");
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.ssao.blur.fbo);
        glViewport(0, 0, width, height);
        glClearColor(0, 0, 0, 0);
        glClear(GL_COLOR_BUFFER_BIT);
        normalUnit.activate();
        glObj.ssao.hbao.texture->bind();
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glPopDebugGroup();

        // Second pass
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 2nd");
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
        glViewport(lastViewport[0], lastViewport[1], lastViewport[2], lastViewport[3]);
        normalUnit.activate();
        glObj.ssao.blur.texture->bind();
        program.setUniform(uniforms.invResDir, 0.f, 1.f / glObj.texHeight);

        glEnable(GL_BLEND);
        glBlendFunc(GL_ZERO, GL_SRC_COLOR);
        glColorMask(1, 1, 1, 0);

        glDrawArrays(GL_TRIANGLES, 0, 3);

        glDisable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_ONE);
        glColorMask(1, 1, 1, 1);

        glBindVertexArray(0);
        glPopDebugGroup();
    }
}

void applyTemporalAa(const ghoul::opengl::Texture& linearDepthTex,
                     const ghoul::opengl::Texture& colorTex,
                     const ghoul::opengl::Texture& velocityTex,
                     const ghoul::opengl::Texture& velocityNeighbormaxTex,
                     const vec2_t& currJitter, const vec2_t& prevJitter,
                     float feedbackMin, float feedbackMax, float motionScale, float time)
{
    static int target = 0;
    target = (target + 1) % 2;

    const int dstBuf = target;
    const int srcBuf = (target + 1) % 2;

    const glm::vec2 res = glm::vec2(
        static_cast<float>(glObj.texWidth),
        static_cast<float>(glObj.texHeight)
    );
    const glm::vec2 invRes = 1.f / res;
    const glm::vec4 texelSize = glm::vec4(invRes.x, invRes.y, res.x, res.y);
    const glm::vec2 currJitt = glm::vec2(currJitter.x, currJitter.y);
    const glm::vec2 jitterUvCurr = currJitt / res;
    const glm::vec2 prevJitt = glm::vec2(prevJitter.x, prevJitter.y);
    const glm::vec2 jitterUvPrev = prevJitt / res;
    const glm::vec4 jitterUv = glm::vec4(
        jitterUvCurr.x,
        jitterUvCurr.y,
        jitterUvPrev.x,
        jitterUvPrev.y
    );

    ghoul::opengl::TextureUnit linearDepthUnit;
    linearDepthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    colorTex.bind();

    ghoul::opengl::TextureUnit tempBufferUnit;
    tempBufferUnit.activate();
    glObj.targets.texTemporalBuffer[srcBuf]->bind();

    ghoul::opengl::TextureUnit velocityUnit;
    velocityUnit.activate();
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    int boundBuffer;
    glGetIntegerv(GL_DRAW_BUFFER, &boundBuffer);

    const GLenum drawBuffers[2] = {
        GL_COLOR_ATTACHMENT2 + dstBuf,   // tex_temporal_buffer[0 or 1]
        static_cast<GLenum>(boundBuffer) // assume that this is part of the same gbuffer
    };

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.targets.fbo);
    glViewport(0, 0, glObj.texWidth, glObj.texHeight);
    glDrawBuffers(2, drawBuffers);

    if (motionScale != 0.f) {
        ghoul::opengl::ProgramObject& program = *glObj.temporal.withMotionBlur.program;
        auto& uniforms = glObj.temporal.withMotionBlur.uniforms;

        ghoul::opengl::TextureUnit velocityNeighbormaxUnit;
        velocityNeighbormaxUnit.activate();
        velocityNeighbormaxTex.bind();

        program.activate();
        program.setUniform(uniforms.texLinearDepth, linearDepthUnit);
        program.setUniform(uniforms.texMain, colorUnit);
        program.setUniform(uniforms.texPrev, tempBufferUnit);
        program.setUniform(uniforms.texVel, velocityUnit);
        program.setUniform(uniforms.texVelNeighbormax, velocityNeighbormaxUnit);
        program.setUniform(uniforms.texelSize, texelSize);
        program.setUniform(uniforms.jitterUv, jitterUv);
        program.setUniform(uniforms.time, time);
        program.setUniform(uniforms.feedbackMin, feedbackMin);
        program.setUniform(uniforms.feedbackMax, feedbackMax);
        program.setUniform(uniforms.motionScale, motionScale);

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
    }
    else {
        ghoul::opengl::ProgramObject& program = *glObj.temporal.noMotionBlur.program;
        auto& uniforms = glObj.temporal.noMotionBlur.uniforms;

        program.activate();
        program.setUniform(uniforms.texLinearDepth, linearDepthUnit);
        program.setUniform(uniforms.texMain, colorUnit);
        program.setUniform(uniforms.texPrev, tempBufferUnit);
        program.setUniform(uniforms.texVel, velocityUnit);
        program.setUniform(uniforms.texelSize, texelSize);
        program.setUniform(uniforms.jitterUv, jitterUv);
        program.setUniform(uniforms.feedbackMin, feedbackMin);
        program.setUniform(uniforms.feedbackMax, feedbackMax);

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
    }
}

void postprocess(const Settings& settings, const glm::mat4& V, const glm::mat4& P) {
    ghoul_assert(settings.inputTextures.color, "No color texture provided");
    ghoul_assert(settings.inputTextures.depth, "No depth texture provided");
    ghoul_assert(settings.inputTextures.normal, "No normal texture provided");

    // For seeding noise
    static float time = 0.f;
    time = time + 0.016f;
    if (time > 100.f) {
        time -= 100.f;
    }

    static vec2_t prevJitter = { 0.f, 0.f };
    glm::vec3 L = V * glm::vec4(0.f, 0.f, 0.f, 1.f);
    glm::vec3 lightDir = glm::normalize(L);
    glm::vec3 lightColor = glm::vec3(5.f);
    glm::mat4 invP = glm::inverse(P);
    const float near = P[3][2] / (P[2][2] - 1.f);
    const float far = P[3][2] / (P[2][2] + 1.f);
    vec2_t jitter;
    if (isOrthoProjMatrix(P)) {
        jitter[0] = -P[3][0] * 0.5f;
        jitter[1] = -P[3][1] * 0.5f;
    }
    else {
        jitter[0] = P[2][0] * 0.5f;
        jitter[1] = P[2][1] * 0.5f;
    }
    bool isOrthographic = isOrthoProjMatrix(P);


    int lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);
    int lastViewport[4];
    glGetIntegerv(GL_VIEWPORT, lastViewport);
    int lastDrawBuffer;
    glGetIntegerv(GL_DRAW_BUFFER, &lastDrawBuffer);
    int lastBlend;
    glGetIntegerv(GL_BLEND, &lastBlend);
    int lastColormask[4];
    glGetIntegerv(GL_COLOR_WRITEMASK, lastColormask);
    int lastDepthmask;
    glGetIntegerv(GL_DEPTH_WRITEMASK, &lastDepthmask);

    int width = lastViewport[2];
    int height = lastViewport[3];

    if (width > static_cast<int>(glObj.texWidth) ||
        height > static_cast<int>(glObj.texHeight))
    {
        resize(width, height);
    }

    glViewport(0, 0, width, height);
    glBindVertexArray(glObj.vao);

    {
        ghoul::opengl::ProgramObject& program = *glObj.linearDepth.program;
        auto& uniforms = glObj.linearDepth.uniforms;

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Linearize Depth");
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.linearDepth.fbo);
        glViewport(0, 0, glObj.texWidth, glObj.texHeight);
        glClearColor(far, 0, 0, 0);
        glClear(GL_COLOR_BUFFER_BIT);
        glViewport(0, 0, width, height);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);

        ghoul::opengl::TextureUnit unit;
        unit.activate();
        settings.inputTextures.depth->bind();

        program.activate();
        program.setUniform(uniforms.texDepth, unit);
        program.setUniform(uniforms.clipInfo, glm::vec4(near * far, near - far, far, 0));
        program.setUniform(uniforms.isPerspective,isOrthographic ? 0 : 1);

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
    }

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glPopDebugGroup();

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Generate Linear Depth Mipmaps");
    glObj.linearDepth.texture->bind();
    glGenerateMipmap(GL_TEXTURE_2D);
    glPopDebugGroup();

    if (settings.temporalReprojection.enabled) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.velocity.fbo);
        glViewport(
            0,
            0,
            glObj.texWidth / VelocityTileSize,
            glObj.texHeight / VelocityTileSize
        );

        // Blit Tilemax
        {
            ghoul::opengl::ProgramObject& program = *glObj.blitTilemax.program;
            auto& uniforms = glObj.blitTilemax.uniforms;

            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Velocity: Tilemax");

            ghoul::opengl::TextureUnit velocityUnit;
            velocityUnit.activate();
            settings.inputTextures.velocity->bind();

            program.activate();
            program.setUniform(uniforms.texVel, velocityUnit);
            program.setUniform(
                uniforms.texVelTexelSize,
                glm::vec2(1.f / glObj.texWidth, 1.f / glObj.texHeight)
            );

            glDrawBuffer(GL_COLOR_ATTACHMENT0);
            glBindVertexArray(glObj.vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);
            glPopDebugGroup();
        }

        // Blit Neighbormax
        {
            ghoul::opengl::ProgramObject& program = *glObj.blitNeighbormax.program;
            auto& uniforms = glObj.blitNeighbormax.uniforms;

            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Velocity: Neighbormax");
            glDrawBuffer(GL_COLOR_ATTACHMENT1);

            ghoul::opengl::TextureUnit velocityUnit;
            velocityUnit.activate();
            glObj.velocity.texTilemax->bind();

            program.activate();
            program.setUniform(uniforms.texVel, velocityUnit);
            program.setUniform(
                uniforms.texVelTexelSize,
                glm::vec2(
                    1.f / (glObj.texWidth / VelocityTileSize),
                    1.f / (glObj.texHeight / VelocityTileSize)
                )
            );

            glBindVertexArray(glObj.vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);
            glPopDebugGroup();
        }
    }

    GLenum dstBuffer = GL_COLOR_ATTACHMENT1;
    ghoul::opengl::Texture* srcTexture = glObj.targets.texColor[0].get();

    auto swapTarget = [&dstBuffer, &srcTexture]() {
        dstBuffer =
            dstBuffer == GL_COLOR_ATTACHMENT0 ?
            GL_COLOR_ATTACHMENT1 :
            GL_COLOR_ATTACHMENT0;
        srcTexture =
            srcTexture == glObj.targets.texColor[0].get() ?
            glObj.targets.texColor[1].get() :
            glObj.targets.texColor[0].get();
    };

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.targets.fbo);
    glViewport(0, 0, width, height);

    if (settings.background.enabled) {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Clear HDR");
        glDrawBuffer(dstBuffer);
        glClearColor(
            settings.background.r,
            settings.background.g,
            settings.background.b,
            0.f
        );
        glClear(GL_COLOR_BUFFER_BIT);
        glPopDebugGroup();
    }
    else {
        GLenum drawBuffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, drawBuffers);
        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT);
        glDrawBuffer(dstBuffer);
    }

    // Shading
    {
        ghoul::opengl::ProgramObject& program = *glObj.shading.program;
        auto& uniforms = glObj.shading.uniforms;

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Shading");
        ghoul::opengl::TextureUnit depthUnit;
        depthUnit.activate();
        settings.inputTextures.depth->bind();

        ghoul::opengl::TextureUnit colorUnit;
        colorUnit.activate();
        settings.inputTextures.color->bind();

        ghoul::opengl::TextureUnit normalUnit;
        normalUnit.activate();
        settings.inputTextures.normal->bind();

        program.activate();
        program.setUniform(uniforms.texDepth, depthUnit);
        program.setUniform(uniforms.texColor, colorUnit);
        program.setUniform(uniforms.texNormal, normalUnit);
        program.setUniform(uniforms.invProjMat, invP);
        program.setUniform(uniforms.lightDir, lightDir);
        program.setUniform(uniforms.lightCol, lightColor);

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
        glPopDebugGroup();
    }

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "SSAO");
    if (settings.ambientOcclusion[0].enabled) {
        applySsao(
            *glObj.linearDepth.texture,
            *settings.inputTextures.normal,
            P,
            settings.ambientOcclusion[0].intensity,
            settings.ambientOcclusion[0].radius,
            settings.ambientOcclusion[0].horizonBias,
            settings.ambientOcclusion[0].normalBias
        );
    }
    if (settings.ambientOcclusion[1].enabled) {
        applySsao(
            *glObj.linearDepth.texture,
            *settings.inputTextures.normal,
            P,
            settings.ambientOcclusion[1].intensity,
            settings.ambientOcclusion[1].radius,
            settings.ambientOcclusion[1].horizonBias,
            settings.ambientOcclusion[1].normalBias
        );
    }
    glPopDebugGroup();

    if (settings.temporalReprojection.enabled) {
        swapTarget();
        glDrawBuffer(dstBuffer);
        const float feedbackMin = settings.temporalReprojection.feedbackMin;
        const float feedbackMax = settings.temporalReprojection.feedbackMax;
        const float motionScale =
            settings.temporalReprojection.motionBlur.enabled ?
            settings.temporalReprojection.motionBlur.motionScale :
            0.f;

        if (motionScale != 0.f) {
            glPushDebugGroup(
                GL_DEBUG_SOURCE_APPLICATION,
                1,
                -1,
                "Temporal AA + Motion Blur"
            );
        }
        else {
            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Temporal AA");
        }
        applyTemporalAa(
            *glObj.linearDepth.texture,
            *srcTexture,
            *settings.inputTextures.velocity,
            *glObj.velocity.texNeighbormax,
            jitter,
            prevJitter,
            feedbackMin,
            feedbackMax,
            motionScale,
            time
        );
        glPopDebugGroup();

        {
            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Sharpen");
            swapTarget();
            glDrawBuffer(dstBuffer);
            ghoul::opengl::TextureUnit srcUnit;
            srcUnit.activate();
            srcTexture->bind();

            glObj.sharpen.program->activate();
            glObj.sharpen.program->setUniform(glObj.sharpen.uniforms.tex, srcUnit);

            glBindVertexArray(glObj.vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);
            glPopDebugGroup();
        }
    }

    if (settings.inputTextures.emissive) {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Add Emissive");
        glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_ONE);
        blitTexture(*settings.inputTextures.emissive);
        glDisable(GL_BLEND);
        glPopDebugGroup();
    }

    if (settings.depthOfField.enabled) {
        swapTarget();
        glDrawBuffer(dstBuffer);
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "DOF");
        int prevFbo;
        glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &prevFbo);

        ghoul::opengl::TextureUnit linearDepthUnit;
        linearDepthUnit.activate();
        glObj.linearDepth.texture->bind();

        {
            ghoul::opengl::ProgramObject& program = *glObj.bokehDof.halfRes.program;
            auto& uniforms = glObj.bokehDof.halfRes.uniforms;

            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "DOF Prepass");
            int prevViewport[4];
            glGetIntegerv(GL_VIEWPORT, prevViewport);
            glViewport(0, 0, glObj.texWidth / 2, glObj.texHeight / 2);

            glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);

            ghoul::opengl::TextureUnit colorUnit;
            colorUnit.activate();
            srcTexture->bind();

            program.activate();
            program.setUniform(uniforms.texDepth, linearDepthUnit);
            program.setUniform(uniforms.texColor, colorUnit);
            program.setUniform(uniforms.focusPoint, settings.depthOfField.focusDepth);
            program.setUniform(uniforms.focusScale, settings.depthOfField.focusScale);

            glBindVertexArray(glObj.vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);

            glViewport(prevViewport[0], prevViewport[1], prevViewport[2], prevViewport[3]);
            glPopDebugGroup();
            glBindFramebuffer(GL_DRAW_FRAMEBUFFER, prevFbo);
        }

        {
            ghoul::opengl::ProgramObject& program = *glObj.bokehDof.program;
            auto& uniforms = glObj.bokehDof.uniforms;

            ghoul::opengl::TextureUnit colorCocUnit;
            colorCocUnit.activate();
            glObj.bokehDof.halfRes.colorCoc->bind();

            ghoul::opengl::TextureUnit srcUnit;
            srcUnit.activate();
            srcTexture->bind();

            program.activate();
            program.setUniform(uniforms.texHalfRes, colorCocUnit);
            program.setUniform(uniforms.texDepth, linearDepthUnit);
            program.setUniform(uniforms.texColor, srcUnit);
            program.setUniform(
                uniforms.texelSize,
                glm::vec2(1.f / glObj.texWidth, 1.f / glObj.texHeight)
            );
            program.setUniform(uniforms.focusDepth, settings.depthOfField.focusDepth);
            program.setUniform(uniforms.focusScale, settings.depthOfField.focusScale);
            program.setUniform(uniforms.time, time);

            glBindVertexArray(glObj.vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);
            glPopDebugGroup();
        }
    }

    // Tonemapping
    {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Tonemapping");
        swapTarget();
        glDrawBuffer(dstBuffer);
        const Tonemapping tonemapping =
            settings.tonemapping.enabled ?
            settings.tonemapping.mode :
            Tonemapping::Passthrough;
        ghoul::opengl::TextureUnit srcUnit;
        srcUnit.activate();
        srcTexture->bind();

        switch (tonemapping) {
            case Tonemapping::Passthrough:
                glObj.tonemapping.passthrough.program->activate();
                glObj.tonemapping.passthrough.program->setUniform(
                    glObj.tonemapping.passthrough.uniforms.tex,
                    srcUnit
                );
                break;
            case Tonemapping::ExposureGamma:
                glObj.tonemapping.exposureGamma.program->activate();
                glObj.tonemapping.exposureGamma.program->setUniform(
                    glObj.tonemapping.exposureGamma.uniforms.tex,
                    srcUnit
                );
                glObj.tonemapping.exposureGamma.program->setUniform(
                    glObj.tonemapping.exposureGamma.uniforms.exposure,
                    settings.tonemapping.exposure
                );
                glObj.tonemapping.exposureGamma.program->setUniform(
                    glObj.tonemapping.exposureGamma.uniforms.gamma,
                    settings.tonemapping.gamma
                );
                break;
            case Tonemapping::Filmic:
                glObj.tonemapping.filmic.program->activate();
                glObj.tonemapping.filmic.program->setUniform(
                    glObj.tonemapping.filmic.uniforms.tex,
                    srcUnit
                );
                glObj.tonemapping.filmic.program->setUniform(
                    glObj.tonemapping.filmic.uniforms.exposure,
                    settings.tonemapping.exposure
                );
                glObj.tonemapping.filmic.program->setUniform(
                    glObj.tonemapping.filmic.uniforms.gamma,
                    settings.tonemapping.gamma
                );
                break;
            case Tonemapping::ACES:
                glObj.tonemapping.aces.program->activate();
                glObj.tonemapping.aces.program->setUniform(
                    glObj.tonemapping.aces.uniforms.tex,
                    srcUnit
                );
                glObj.tonemapping.aces.program->setUniform(
                    glObj.tonemapping.aces.uniforms.exposure,
                    settings.tonemapping.exposure
                );
                glObj.tonemapping.aces.program->setUniform(
                    glObj.tonemapping.aces.uniforms.gamma,
                    settings.tonemapping.gamma
                );
                break;
        }

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
        glPopDebugGroup();
    }

    if (settings.inputTextures.postTonemap) {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Add Post Tonemap");
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blitTexture(*settings.inputTextures.postTonemap);
        glDisable(GL_BLEND);
        glPopDebugGroup();
    }

    prevJitter = jitter;

    if (settings.fxaa.enabled) {
        ghoul::opengl::ProgramObject& program = *glObj.fxaa.program;
        auto& uniforms = glObj.fxaa.uniforms;

        swapTarget();
        glDrawBuffer(dstBuffer);
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "FXAA");
        ghoul::opengl::TextureUnit unit;
        unit.activate();
        srcTexture->bind();

        program.activate();
        program.setUniform(uniforms.tex, unit);
        program.setUniform(uniforms.inverseScreenSize, 1.f / glm::vec2(width, height));

        glBindVertexArray(glObj.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
        glPopDebugGroup();
    }

    // Activate backbuffer or whatever was bound before
    {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "PostProcess Blit Result");

        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
        glViewport(lastViewport[0], lastViewport[1], lastViewport[2], lastViewport[3]);
        glDrawBuffer(static_cast<GLenum>(lastDrawBuffer));

        swapTarget();
        glDepthMask(0);
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blitTexture(*srcTexture, *settings.inputTextures.depth);
        glDisable(GL_BLEND);

        glPopDebugGroup();
    }

    // Reset rest of state
    glBlendFunc(GL_ONE, GL_ZERO);
    if (lastBlend) {
        glEnable(GL_BLEND);
    }
    else {
        glDisable(GL_BLEND);
    }

    glDepthMask(lastDepthmask);
    glColorMask(lastColormask[0], lastColormask[1], lastColormask[2], lastColormask[3]);
}

}  // namespace postprocessing
