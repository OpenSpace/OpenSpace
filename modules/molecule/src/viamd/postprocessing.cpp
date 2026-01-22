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

// Shaders for HBAO are based on nVidias examples and are copyright protected as stated above

#include <modules/molecule/src/viamd/postprocessing.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
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

    struct {
        uint32_t vao = 0;
        uint32_t texWidth = 0;
        uint32_t texHeight = 0;

        struct {
            uint32_t fbo = 0;
            uint32_t texColor[2] = { 0, 0 };
            // These are dedicated and cannot be use as intermediate buffers by other
            // shaders
            uint32_t texTemporalBuffer[2] = { 0, 0 };
        } targets;

        struct {
            uint32_t fbo = 0;
            uint32_t texTilemax = 0;
            uint32_t texNeighbormax = 0;
            int32_t texWidth = 0;
            int32_t texHeight = 0;
        } velocity;

        struct {
            uint32_t fbo = 0;
            uint32_t texture = 0;
            std::unique_ptr<ghoul::opengl::ProgramObject> program;
            UniformCache(clipInfo, texDepth, isPerspective) uniforms;
        } linearDepth;

        struct {
            uint32_t texRandom = 0;
            uint32_t uboHbaoData = 0;

            struct {
                uint32_t fbo = 0;
                uint32_t texture = 0;
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                UniformCache(texLinearDepth, texNormal, texRandom, tcScale,
                    isPerspective) uniforms;
            } hbao;

            struct {
                uint32_t fbo = 0;
                uint32_t texture = 0;
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
                uint32_t fbo = 0;
                uint32_t colorCoc = 0;
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
            r = r + f * (float)(index % base);
            index = (int)(index * ifb);
        }
        return r;
    }

    // Projection parameter extraction
    constexpr bool isOrthoProjMatrix(const mat4_t& P) {
        return P.elem[2][3] == 0.f;
    }

    constexpr vec2_t extractJitterUv(const mat4_t& P) {
        vec2_t jitter;
        if (isOrthoProjMatrix(P)) {
            jitter[0] = -P.elem[3][0] * 0.5f;
            jitter[1] = -P.elem[3][1] * 0.5f;
        }
        else {
            jitter[0] = P.elem[2][0] * 0.5f;
            jitter[1] = P.elem[2][1] * 0.5f;
        }
        return jitter;
    }

    vec2_t extractNearFar(const mat4_t& P) {
        vec2_t nearFar;
        nearFar[0] = P.elem[3][2] / (P[2][2] - 1.0f);
        nearFar[1] = P.elem[3][2] / (P[2][2] + 1.0f);
        return nearFar;
    }
} // namespace

namespace postprocessing {

namespace ssao {

struct HBAOData {
    float radiusToScreen;
    float negInvR2;
    float nDotVBias;
    float nInfluence;

    float aoMultiplier;
    float powExponent;
    vec2_t invFullRes;

    vec4_t projInfo;

    vec4_t samplePattern[32];
};

void setupUboHbaoData(uint32_t ubo, int width, int height, const mat4_t& projMat,
                      float intensity, float radius, float nDotVBias, float normalBias)
{
    // From Intel ASSAO
    constexpr float SAMPLE_PATTERN[] = {
        0.78488064f,  0.56661671f,  1.500000f, -0.126083f, 0.26022232f,  -0.29575172f, 1.500000f, -1.064030f, 0.10459357f,  0.08372527f,  1.110000f, -2.730563f, -0.68286800f, 0.04963045f,  1.090000f, -0.498827f,
        -0.13570161f, -0.64190155f, 1.250000f, -0.532765f, -0.26193795f, -0.08205118f, 0.670000f, -1.783245f, -0.61177456f, 0.66664219f,  0.710000f, -0.044234f, 0.43675563f,  0.25119025f,  0.610000f, -1.167283f,
        0.07884444f,  0.86618668f,  0.640000f, -0.459002f, -0.12790935f, -0.29869005f, 0.600000f, -1.729424f, -0.04031125f, 0.02413622f,  0.600000f, -4.792042f, 0.16201244f,  -0.52851415f, 0.790000f, -1.067055f,
        -0.70991218f, 0.47301072f,  0.640000f, -0.335236f, 0.03277707f,  -0.22349690f, 0.600000f, -1.982384f, 0.68921727f,  0.36800742f,  0.630000f, -0.266718f, 0.29251814f,  0.37775412f,  0.610000f, -1.422520f,
        -0.12224089f, 0.96582592f,  0.600000f, -0.426142f, 0.11071457f,  -0.16131058f, 0.600000f, -2.165947f, 0.46562141f,  -0.59747696f, 0.600000f, -0.189760f, -0.51548797f, 0.11804193f,  0.600000f, -1.246800f,
        0.89141309f,  -0.42090443f, 0.600000f, 0.028192f,  -0.32402530f, -0.01591529f, 0.600000f, -1.543018f, 0.60771245f,  0.41635221f,  0.600000f, -0.605411f, 0.02379565f,  -0.08239821f, 0.600000f, -3.809046f,
        0.48951152f,  -0.23657045f, 0.600000f, -1.189011f, -0.17611565f, -0.81696892f, 0.600000f, -0.513724f, -0.33930185f, -0.20732205f, 0.600000f, -1.698047f, -0.91974425f, 0.05403209f,  0.600000f, 0.062246f,
        -0.15064627f, -0.14949332f, 0.600000f, -1.896062f, 0.53180975f,  -0.35210401f, 0.600000f, -0.758838f, 0.41487166f,  0.81442589f,  0.600000f, -0.505648f, -0.24106961f, -0.32721516f, 0.600000f, -1.665244f};
    constexpr float METERS_TO_VIEWSPACE = 1.f;

    vec4_t projInfo;
    float projScl;

    const float* projData = &projMat.elem[0][0];
    const bool ortho = isOrthoProjMatrix(projMat);
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

    float r = radius * METERS_TO_VIEWSPACE;

    HBAOData data = {
        .radiusToScreen = r * 0.5f * projScl,
        .negInvR2 = -1.f / (r * r),
        .nDotVBias =
            std::clamp(nDotVBias, 0.f, 1.f - std::numeric_limits<float>::epsilon()),
        .nInfluence = std::clamp(normalBias, 0.f, 1.0f),
        .aoMultiplier = 1.f / (1.f - data.nDotVBias),
        .powExponent = std::max(intensity, 0.f),
        .invFullRes = {
            1.f / static_cast<float>(width),
            1.f / static_cast<float>(height)
        },
        .projInfo = projInfo
    };
    memcpy(&data.samplePattern, SAMPLE_PATTERN, sizeof(SAMPLE_PATTERN));

    glBindBuffer(GL_UNIFORM_BUFFER, ubo);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), &data, GL_DYNAMIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

void initialize(int width, int height) {
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

    glGenFramebuffers(1, &glObj.ssao.hbao.fbo);
    glGenFramebuffers(1, &glObj.ssao.blur.fbo);

    glGenTextures(1, &glObj.ssao.texRandom);
    glGenTextures(1, &glObj.ssao.hbao.texture);
    glGenTextures(1, &glObj.ssao.blur.texture);
    glGenBuffers(1, &glObj.ssao.uboHbaoData);


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

    glBindTexture(GL_TEXTURE_2D, glObj.ssao.texRandom);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16_SNORM,
        AORandomTexSize,
        AORandomTexSize,
        0,
        GL_RGBA,
        GL_SHORT,
        buffer.data()
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);


    glBindTexture(GL_TEXTURE_2D, glObj.ssao.hbao.texture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R8,
        width,
        height,
        0,
        GL_RED,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(GL_TEXTURE_2D, glObj.ssao.blur.texture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R8,
        width,
        height,
        0,
        GL_RED,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindFramebuffer(GL_FRAMEBUFFER, glObj.ssao.hbao.fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.ssao.hbao.texture,
        0
    );

    glBindFramebuffer(GL_FRAMEBUFFER, glObj.ssao.blur.fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.ssao.blur.texture,0
    );

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    glBindBuffer(GL_UNIFORM_BUFFER, glObj.ssao.uboHbaoData);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), nullptr, GL_DYNAMIC_DRAW);
}

void shutdown() {
    glDeleteFramebuffers(1, &glObj.ssao.hbao.fbo);
    glDeleteFramebuffers(1, &glObj.ssao.blur.fbo);

    glDeleteTextures(1, &glObj.ssao.texRandom);
    glDeleteTextures(1, &glObj.ssao.hbao.texture);
    glDeleteTextures(1, &glObj.ssao.blur.texture);

    glDeleteBuffers(1, &glObj.ssao.uboHbaoData);

    glObj.ssao.hbao.program = nullptr;
    glObj.ssao.blur.program = nullptr;
}

}  // namespace ssao

namespace shading {

void initialize() {
    glObj.shading.program = ghoul::opengl::ProgramObject::Build(
        "Deferred Shading",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/deferred_shading_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.shading.program, glObj.shading.uniforms);
}

void shutdown() {
    glObj.shading.program = nullptr;
}

}  // namespace shading

namespace tonemapping {

void initialize() {
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
}

void shutdown() {
    glObj.tonemapping.passthrough.program = nullptr;
    glObj.tonemapping.exposureGamma.program = nullptr;
    glObj.tonemapping.filmic.program = nullptr;
    glObj.tonemapping.aces.program = nullptr;
}

}  // namespace tonemapping

namespace dof {

void initialize(int32_t width, int32_t height) {
    glObj.bokehDof.halfRes.program = ghoul::opengl::ProgramObject::Build(
        "DOF prepass",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/dof_halfres_prepass_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.bokehDof.halfRes.program,
        glObj.bokehDof.halfRes.uniforms
    );

    glGenTextures(1, &glObj.bokehDof.halfRes.colorCoc);
    glBindTexture(GL_TEXTURE_2D, glObj.bokehDof.halfRes.colorCoc);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16F,
        width / 2,
        height / 2,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenFramebuffers(1, &glObj.bokehDof.halfRes.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.bokehDof.halfRes.colorCoc,
        0
    );
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    // DOF
    glObj.bokehDof.program = ghoul::opengl::ProgramObject::Build(
        "DOF Bokeh",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/dof_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(
        *glObj.bokehDof.program,
        glObj.bokehDof.uniforms
    );
}

void shutdown() {
    glObj.bokehDof.halfRes.program = nullptr;
    glObj.bokehDof.program = nullptr;
}

}  // namespace dof

namespace blit {

void initialize() {
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
}

void shutdown() {
    glObj.blit.tex.program = nullptr;
    glObj.blit.texDepth.program = nullptr;
}

}  // namespace blit

namespace velocity {

void initialize(int32_t width, int32_t height) {
    constexpr int VelTileSize = 8;

    ghoul::Dictionary tileSize;
    tileSize.setValue("TileSize", VelTileSize);
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

    glObj.velocity.texWidth = width / VelTileSize;
    glObj.velocity.texHeight = height / VelTileSize;

    glGenTextures(1, &glObj.velocity.texTilemax);
    glBindTexture(GL_TEXTURE_2D, glObj.velocity.texTilemax);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16F,
        glObj.velocity.texWidth,
        glObj.velocity.texHeight,
        0,
        GL_RG,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenTextures(1, &glObj.velocity.texNeighbormax);
    glBindTexture(GL_TEXTURE_2D, glObj.velocity.texNeighbormax);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16F,
        glObj.velocity.texWidth,
        glObj.velocity.texHeight,
        0,
        GL_RG,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenFramebuffers(1, &glObj.velocity.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.velocity.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.velocity.texTilemax,
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        glObj.velocity.texNeighbormax,
        0
    );

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
}

void shutdown() {
    glObj.blitTilemax.program = nullptr;
    glObj.blitNeighbormax.program = nullptr;
    glDeleteTextures(1, &glObj.velocity.texTilemax);
    glDeleteTextures(1, &glObj.velocity.texNeighbormax);
    glDeleteFramebuffers(1, &glObj.velocity.fbo);
}

}  // namespace velocity

namespace temporal {

void initialize() {
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
}

void shutdown() {
    glObj.temporal.withMotionBlur.program = nullptr;
    glObj.temporal.noMotionBlur.program = nullptr;
}

} // namespace temporal

namespace sharpen {

void initialize() {
    glObj.sharpen.program = ghoul::opengl::ProgramObject::Build(
        "Sharpen",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/sharpen_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.sharpen.program, glObj.sharpen.uniforms);
}

void sharpen(uint32_t texture) {
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, texture);

    glObj.sharpen.program->activate();
    glObj.sharpen.program->setUniform(glObj.sharpen.uniforms.tex, unit);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void shutdown() {
    glObj.sharpen.program = nullptr;
}

}   // namespace sharpen

namespace fxaa {

void initialize() {
    glObj.fxaa.program = ghoul::opengl::ProgramObject::Build(
        "FXAA",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/fxaa_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.fxaa.program, glObj.fxaa.uniforms);
}

void applyFxaa(uint32_t texture, int width, int height) {
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, texture);

    glObj.fxaa.program->activate();
    glObj.fxaa.program->setUniform(glObj.fxaa.uniforms.tex, unit);

    glObj.fxaa.program->setUniform(
        glObj.fxaa.uniforms.inverseScreenSize,
        1.f / glm::vec2(width, height)
    );

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void shutdown() {
    glObj.fxaa.program = nullptr;
}

}   // namespace fxaa

void initialize(int width, int height) {
    glGenVertexArrays(1, &glObj.vao);

    // LINEARIZE DEPTH
    glObj.linearDepth.program = ghoul::opengl::ProgramObject::Build(
        "Linearize Depth",
        absPath("${MODULE_MOLECULE}/shaders/quad_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/linearize_depth_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*glObj.linearDepth.program, glObj.linearDepth.uniforms);

    glGenTextures(1, &glObj.linearDepth.texture);
    glBindTexture(GL_TEXTURE_2D, glObj.linearDepth.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenFramebuffers(1, &glObj.linearDepth.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.linearDepth.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.linearDepth.texture,
        0
    );
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);


    // COLOR
    glGenTextures(2, glObj.targets.texColor);
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texColor[0]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texColor[1]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenTextures(2, glObj.targets.texTemporalBuffer);
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[0]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[1]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenFramebuffers(1, &glObj.targets.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.targets.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.targets.texColor[0],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D,
        glObj.targets.texColor[1],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT2,
        GL_TEXTURE_2D,
        glObj.targets.texTemporalBuffer[0],
        0
    );
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT3,
        GL_TEXTURE_2D,
        glObj.targets.texTemporalBuffer[1],
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

    ssao::initialize(width, height);
    dof::initialize(width, height);
    velocity::initialize(width, height);
    shading::initialize();
    tonemapping::initialize();
    temporal::initialize();
    blit::initialize();
    sharpen::initialize();
    fxaa::initialize();
}

void resize(int width, int height) {
    glObj.texWidth = width;
    glObj.texHeight = height;

    glBindTexture(GL_TEXTURE_2D, glObj.linearDepth.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, nullptr);

    glBindTexture(GL_TEXTURE_2D, glObj.targets.texColor[0]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );

    glBindTexture(GL_TEXTURE_2D, glObj.targets.texColor[1]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );

    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[0]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );

    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[1]);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R11F_G11F_B10F,
        width,
        height,
        0,
        GL_RGB,
        GL_FLOAT,
        nullptr
    );

    // ssao
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.hbao.texture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R8,
        width,
        height,
        0,
        GL_RED,
        GL_UNSIGNED_BYTE,
        nullptr
    );

    glBindTexture(GL_TEXTURE_2D, glObj.ssao.blur.texture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_R8,
        width,
        height,
        0,
        GL_RED,
        GL_UNSIGNED_BYTE,
        nullptr
    );

    // dof
    glBindTexture(GL_TEXTURE_2D, glObj.bokehDof.halfRes.colorCoc);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16F,
        width / 2,
        height / 2,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );

    // velocity
    constexpr int VelTileSize = 8;
    glObj.velocity.texWidth = width / VelTileSize;
    glObj.velocity.texHeight = height / VelTileSize;

    glBindTexture(GL_TEXTURE_2D, glObj.velocity.texTilemax);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16F,
        glObj.velocity.texWidth,
        glObj.velocity.texHeight,
        0,
        GL_RG,
        GL_FLOAT,
        nullptr
    );

    glBindTexture(GL_TEXTURE_2D, glObj.velocity.texNeighbormax);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RG16F,
        glObj.velocity.texWidth,
        glObj.velocity.texHeight,
        0,
        GL_RG,
        GL_FLOAT,
        nullptr
    );
}

void shutdown() {
    glObj.linearDepth.program = nullptr;

    ssao::shutdown();
    dof::shutdown();
    velocity::shutdown();
    shading::shutdown();
    tonemapping::shutdown();
    temporal::shutdown();
    blit::shutdown();
    sharpen::shutdown();
    fxaa::shutdown();

    glDeleteVertexArrays(1, &glObj.vao);
}

void computeLinearDepth(uint32_t depthTex, float nearPlane, float farPlane,
                        bool isOrthographic = false)
{
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, depthTex);

    glObj.linearDepth.program->activate();
    glObj.linearDepth.program->setUniform(glObj.linearDepth.uniforms.texDepth, unit);
    glObj.linearDepth.program->setUniform(
        glObj.linearDepth.uniforms.clipInfo,
        glm::vec4(nearPlane * farPlane, nearPlane - farPlane, farPlane, 0)
    );
    glObj.linearDepth.program->setUniform(glObj.linearDepth.uniforms.isPerspective, isOrthographic ? 0 : 1);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void applySsao(uint32_t linearDepthTex, uint32_t normalTex, const mat4_t& projMatrix,
               float intensity, float radius, float bias, float normalBias)
{
    const bool isOrtho = isOrthoProjMatrix(projMatrix);
    const float sharpness = 3.f / std::sqrt(radius);

    int last_fbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    int last_viewport[4];
    glGetIntegerv(GL_VIEWPORT, last_viewport);

    int width = last_viewport[2];
    int height = last_viewport[3];

    glBindVertexArray(glObj.vao);

    const vec2_t invRes = vec2_t{ 1.f / glObj.texWidth, 1.f / glObj.texHeight };
    ssao::setupUboHbaoData(
        glObj.ssao.uboHbaoData,
        width,
        height,
        projMatrix,
        intensity,
        radius,
        bias,
        normalBias
    );

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.ssao.hbao.fbo);
    glViewport(0, 0, glObj.texWidth, glObj.texHeight);
    glClearColor(1,1,1,1);
    glClear(GL_COLOR_BUFFER_BIT);

    glViewport(0, 0, width, height);

    // RENDER HBAO
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "HBAO");
    glObj.ssao.hbao.program->activate();

    ghoul::opengl::TextureUnit linearDepthUnit;
    linearDepthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    ghoul::opengl::TextureUnit normalUnit;
    normalUnit.activate();
    glBindTexture(GL_TEXTURE_2D, normalTex);

    ghoul::opengl::TextureUnit randomUnit;
    randomUnit.activate();
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.texRandom);

    glBindBufferBase(GL_UNIFORM_BUFFER, 0, glObj.ssao.uboHbaoData);
    glUniformBlockBinding(*glObj.ssao.hbao.program, glGetUniformBlockIndex(*glObj.ssao.hbao.program, "u_control_buffer"), 0);
    glObj.ssao.hbao.program->setUniform(
        glObj.ssao.hbao.uniforms.texLinearDepth,
        linearDepthUnit
    );
    glObj.ssao.hbao.program->setUniform(glObj.ssao.hbao.uniforms.texNormal, normalUnit);
    glObj.ssao.hbao.program->setUniform(glObj.ssao.hbao.uniforms.texRandom, randomUnit);
    glObj.ssao.hbao.program->setUniform(glObj.ssao.hbao.uniforms.isPerspective, isOrtho ? 0 : 1);
    glObj.ssao.hbao.program->setUniform(
        glObj.ssao.hbao.uniforms.tcScale,
        static_cast<float>(width) / static_cast<float>(glObj.texWidth),
        static_cast<float>(height) / static_cast<float>(glObj.texHeight)
    );

    glDrawArrays(GL_TRIANGLES, 0, 3);
    glPopDebugGroup();

    glObj.ssao.blur.program->activate();
    glObj.ssao.blur.program->setUniform(
        glObj.ssao.blur.uniforms.texLinearDepth,
        linearDepthUnit
    );
    glObj.ssao.blur.program->setUniform(glObj.ssao.blur.uniforms.texAo, normalUnit);
    glObj.ssao.blur.program->setUniform(glObj.ssao.blur.uniforms.sharpness, sharpness);
    glObj.ssao.blur.program->setUniform(glObj.ssao.blur.uniforms.invResDir, invRes.x, 0.f);
    glObj.ssao.blur.program->setUniform(
        glObj.ssao.blur.uniforms.tcScale,
        static_cast<float>(width) / static_cast<float>(glObj.texWidth),
        static_cast<float>(height) / static_cast<float>(glObj.texHeight)
    );

    // BLUR FIRST
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 1st");
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.ssao.blur.fbo);
    glViewport(0, 0, width, height);
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT);
    normalUnit.activate();
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.hbao.texture);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glPopDebugGroup();

    // BLUR SECOND
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 2nd");
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    normalUnit.activate();
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.blur.texture);
    glObj.ssao.blur.program->setUniform(
        glObj.ssao.blur.uniforms.invResDir,
        0.f,
        invRes.y
    );

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

void shadeDeferred(uint32_t depthTex, uint32_t colorTex, uint32_t normalTex,
                   const mat4_t& invProjMatrix, const vec3_t& lightDir,
                   const vec3_t& lightCol, float)
{
    ghoul::opengl::TextureUnit depthUnit;
    depthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, depthTex);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    glBindTexture(GL_TEXTURE_2D, colorTex);

    ghoul::opengl::TextureUnit normalUnit;
    normalUnit.activate();
    glBindTexture(GL_TEXTURE_2D, normalTex);

    glObj.shading.program->activate();
    glObj.shading.program->setUniform(glObj.shading.uniforms.texDepth, depthUnit);
    glObj.shading.program->setUniform(glObj.shading.uniforms.texColor, colorUnit);
    glObj.shading.program->setUniform(glObj.shading.uniforms.texNormal, normalUnit);
    glm::mat4 ipm;
    std::memcpy(glm::value_ptr(ipm), &invProjMatrix, 16 * sizeof(float));
    glObj.shading.program->setUniform(glObj.shading.uniforms.invProjMat, ipm);
    glObj.shading.program->setUniform(glObj.shading.uniforms.lightDir, lightDir.x, lightDir.y, lightDir.z);
    glObj.shading.program->setUniform(glObj.shading.uniforms.lightCol, lightCol.x, lightCol.y, lightCol.z);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void halfResColorCoc(uint32_t linearDepthTex, uint32_t colorTex, float focusPoint,
                     float focusScale)
{
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "DOF Prepass");
    int lastViewport[4];
    glGetIntegerv(GL_VIEWPORT, lastViewport);
    glViewport(0, 0, glObj.texWidth / 2, glObj.texHeight / 2);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);

    ghoul::opengl::TextureUnit linearDepthUnit;
    linearDepthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glObj.bokehDof.halfRes.program->activate();
    glObj.bokehDof.halfRes.program->setUniform(glObj.bokehDof.halfRes.uniforms.texDepth, linearDepthUnit);
    glObj.bokehDof.halfRes.program->setUniform(glObj.bokehDof.halfRes.uniforms.texColor, colorUnit);
    glObj.bokehDof.halfRes.program->setUniform(glObj.bokehDof.halfRes.uniforms.focusPoint, focusPoint);
    glObj.bokehDof.halfRes.program->setUniform(glObj.bokehDof.halfRes.uniforms.focusScale, focusScale);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glViewport(lastViewport[0], lastViewport[1], lastViewport[2], lastViewport[3]);
    glPopDebugGroup();
}

void applyDof(uint32_t linearDepthTex, uint32_t colorTex, float focusPoint,
              float focusScale, float time)
{
    int lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);

    halfResColorCoc(linearDepthTex, colorTex, focusPoint, focusScale);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);

    ghoul::opengl::TextureUnit colorCocUnit;
    colorCocUnit.activate();
    glBindTexture(GL_TEXTURE_2D, glObj.bokehDof.halfRes.colorCoc);

    ghoul::opengl::TextureUnit linearDepthUnit;
    linearDepthUnit.activate();
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glObj.bokehDof.program->activate();
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.texHalfRes, colorCocUnit);
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.texDepth, linearDepthUnit);
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.texColor, colorUnit);
    glObj.bokehDof.program->setUniform(
        glObj.bokehDof.uniforms.texelSize,
        1.f / glObj.texWidth,
        1.f / glObj.texHeight
    );
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.focusDepth, focusPoint);
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.focusScale, focusScale);
    glObj.bokehDof.program->setUniform(glObj.bokehDof.uniforms.time, time);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void applyTonemapping(uint32_t colorTex, Tonemapping tonemapping, float exposure,
                      float gamma)
{
    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    glBindTexture(GL_TEXTURE_2D, colorTex);

    switch (tonemapping) {
        case Tonemapping::Passthrough:
            glObj.tonemapping.passthrough.program->activate();
            glObj.tonemapping.passthrough.program->setUniform(glObj.tonemapping.passthrough.uniforms.tex, colorUnit);
            break;
        case Tonemapping::ExposureGamma:
            glObj.tonemapping.exposureGamma.program->activate();
            glObj.tonemapping.exposureGamma.program->setUniform(glObj.tonemapping.exposureGamma.uniforms.tex, colorUnit);
            glObj.tonemapping.exposureGamma.program->setUniform(glObj.tonemapping.exposureGamma.uniforms.exposure, exposure);
            glObj.tonemapping.exposureGamma.program->setUniform(glObj.tonemapping.exposureGamma.uniforms.gamma, gamma);
            break;
        case Tonemapping::Filmic:
            glObj.tonemapping.filmic.program->activate();
            glObj.tonemapping.filmic.program->setUniform(glObj.tonemapping.filmic.uniforms.tex, colorUnit);
            glObj.tonemapping.filmic.program->setUniform(glObj.tonemapping.filmic.uniforms.exposure, exposure);
            glObj.tonemapping.filmic.program->setUniform(glObj.tonemapping.filmic.uniforms.gamma, gamma);
            break;
        case Tonemapping::ACES:
            glObj.tonemapping.aces.program->activate();
            glObj.tonemapping.aces.program->setUniform(glObj.tonemapping.aces.uniforms.tex, colorUnit);
            glObj.tonemapping.aces.program->setUniform(glObj.tonemapping.aces.uniforms.exposure, exposure);
            glObj.tonemapping.aces.program->setUniform(glObj.tonemapping.aces.uniforms.gamma, gamma);
            break;
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void blitTilemax(uint32_t velocityTex, int texWidth, int texHeight) {
    ghoul::opengl::TextureUnit velocityUnit;
    velocityUnit.activate();
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    glObj.blitTilemax.program->activate();
    glObj.blitTilemax.program->setUniform(glObj.blitTilemax.uniforms.texVel, velocityUnit);
    glObj.blitTilemax.program->setUniform(glObj.blitTilemax.uniforms.texVelTexelSize, glm::vec2(1.f / texWidth, 1.f / texHeight));

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void blitNeighbormax(uint32_t velocityTex, int texWidth, int texHeight) {
    ghoul::opengl::TextureUnit velocityUnit;
    velocityUnit.activate();
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    glObj.blitNeighbormax.program->activate();
    glObj.blitNeighbormax.program->setUniform(glObj.blitNeighbormax.uniforms.texVel, velocityUnit);
    glObj.blitNeighbormax.program->setUniform(glObj.blitNeighbormax.uniforms.texVelTexelSize, glm::vec2(1.f / texWidth, 1.f / texHeight));

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void applyTemporalAa(uint32_t linearDepthTex, uint32_t colorTex, uint32_t velocityTex,
                     uint32_t velocityNeighbormaxTex, const vec2_t& currJitter,
                     const vec2_t& prevJitter, float feedbackMin, float feedbackMax,
                     float motionScale, float time)
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
    glBindTexture(GL_TEXTURE_2D, colorTex);

    ghoul::opengl::TextureUnit tempBufferUnit;
    tempBufferUnit.activate();
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[srcBuf]);

    ghoul::opengl::TextureUnit velocityUnit;
    velocityUnit.activate();
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    ghoul::opengl::TextureUnit velocityNeighbormaxUnit;
    velocityNeighbormaxUnit.activate();
    glBindTexture(GL_TEXTURE_2D, velocityNeighbormaxTex);

    int boundBuffer;
    glGetIntegerv(GL_DRAW_BUFFER, &boundBuffer);

    GLenum drawBuffers[2];
    // tex_temporal_buffer[0 or 1]
    drawBuffers[0] = GL_COLOR_ATTACHMENT2 + dstBuf;
    // assume that this is part of the same gbuffer
    drawBuffers[1] = static_cast<GLenum>(boundBuffer);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.targets.fbo);
    glViewport(0, 0, glObj.texWidth, glObj.texHeight);
    glDrawBuffers(2, drawBuffers);

    if (motionScale != 0.f) {
        glObj.temporal.withMotionBlur.program->activate();
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texLinearDepth, linearDepthUnit);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texMain, colorUnit);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texPrev, tempBufferUnit);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texVel, velocityUnit);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texVelNeighbormax, velocityNeighbormaxUnit);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.texelSize, texelSize);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.jitterUv, jitterUv);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.time, time);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.feedbackMin, feedbackMin);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.feedbackMax, feedbackMax);
        glObj.temporal.withMotionBlur.program->setUniform(glObj.temporal.withMotionBlur.uniforms.motionScale, motionScale);
    }
    else {
        glObj.temporal.noMotionBlur.program->activate();
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.texLinearDepth, linearDepthUnit);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.texMain, colorUnit);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.texPrev, tempBufferUnit);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.texVel, velocityUnit);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.texelSize, texelSize);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.jitterUv, jitterUv);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.feedbackMin, feedbackMin);
        glObj.temporal.noMotionBlur.program->setUniform(glObj.temporal.noMotionBlur.uniforms.feedbackMax, feedbackMax);
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void blitTexture(uint32_t tex, uint32_t depth) {
    ghoul::opengl::TextureUnit texUnit;
    texUnit.activate();
    glBindTexture(GL_TEXTURE_2D, tex);

    ghoul::opengl::TextureUnit depthUnit;

    if (depth) {
        depthUnit.activate();
        glBindTexture(GL_TEXTURE_2D, depth);
        glObj.blit.texDepth.program->activate();
        glObj.blit.texDepth.program->setUniform(glObj.blit.texDepth.uniforms.texColor, texUnit);
        glObj.blit.texDepth.program->setUniform(glObj.blit.texDepth.uniforms.texDepth, depthUnit);
    }
    else {
        glObj.blit.tex.program->activate();
        glObj.blit.tex.program->setUniform(glObj.blit.tex.uniforms.tex, 0);
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void postprocess(const Settings& settings, const mat4_t& V, const mat4_t& P) {
    // For seeding noise
    static float time = 0.f;
    time = time + 0.016f;
    if (time > 100.f) {
        time -= 100.f;
    }

    static vec2_t prevJitter = { 0.f, 0.f };
    vec3_t L = mat4_mul_vec3(V, vec3_set(0.f, 0.f, 0.f), 1.f);
    vec3_t lightDir = vec3_normalize(L);
    vec3_t lightCol = vec3_set1(5.f);
    mat4_t invP = mat4_inverse(P);
    vec2_t nearFar = extractNearFar(P);
    vec2_t jitter = extractJitterUv(P);
    bool ortho = isOrthoProjMatrix(P);


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

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Linearize Depth");
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.linearDepth.fbo);
    glViewport(0, 0, glObj.texWidth, glObj.texHeight);
    glClearColor(nearFar[1], 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT);
    glViewport(0, 0, width, height);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);
    computeLinearDepth(settings.inputTextures.depth, nearFar[0], nearFar[1], ortho);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glPopDebugGroup();

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Generate Linear Depth Mipmaps");
    glBindTexture(GL_TEXTURE_2D, glObj.linearDepth.texture);
    glGenerateMipmap(GL_TEXTURE_2D);
    glPopDebugGroup();

    if (settings.temporalReprojection.enabled) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.velocity.fbo);
        glViewport(0, 0, glObj.velocity.texWidth, glObj.velocity.texHeight);

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Velocity: Tilemax");
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        blitTilemax(settings.inputTextures.velocity, glObj.texWidth, glObj.texHeight);
        glPopDebugGroup();

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Velocity: Neighbormax");
        glDrawBuffer(GL_COLOR_ATTACHMENT1);
        blitNeighbormax(
            glObj.velocity.texTilemax,
            glObj.velocity.texWidth,
            glObj.velocity.texHeight
        );
        glPopDebugGroup();
    }

    GLenum dstBuffer = GL_COLOR_ATTACHMENT1;
    uint32_t srcTexture = glObj.targets.texColor[0];

    auto swapTarget = [&dstBuffer, &srcTexture]() {
        dstBuffer =
            dstBuffer == GL_COLOR_ATTACHMENT0 ?
            GL_COLOR_ATTACHMENT1 :
            GL_COLOR_ATTACHMENT0;
        srcTexture =
            srcTexture == glObj.targets.texColor[0] ?
            glObj.targets.texColor[1] :
            glObj.targets.texColor[0];
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

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Shade");
    shadeDeferred(
        settings.inputTextures.depth,
        settings.inputTextures.color,
        settings.inputTextures.normal,
        invP,
        lightDir,
        lightCol,
        time
    );
    glPopDebugGroup();

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "SSAO");
    if (settings.ambientOcclusion[0].enabled) {
        applySsao(
            glObj.linearDepth.texture,
            settings.inputTextures.normal,
            P,
            settings.ambientOcclusion[0].intensity,
            settings.ambientOcclusion[0].radius,
            settings.ambientOcclusion[0].horizonBias,
            settings.ambientOcclusion[0].normalBias
        );
    }
    if (settings.ambientOcclusion[1].enabled) {
        applySsao(
            glObj.linearDepth.texture,
            settings.inputTextures.normal,
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

        if (motionScale != 0.f)
            glPushDebugGroup(
                GL_DEBUG_SOURCE_APPLICATION,
                1,
                -1,
                "Temporal AA + Motion Blur"
            );
        else
            glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Temporal AA");
            applyTemporalAa(
                glObj.linearDepth.texture,
                srcTexture,
                settings.inputTextures.velocity,
                glObj.velocity.texNeighbormax,
                jitter,
                prevJitter,
                feedbackMin,
                feedbackMax,
                motionScale,
                time
            );
         glPopDebugGroup();

        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Sharpen");
        swapTarget();
        glDrawBuffer(dstBuffer);
        sharpen::sharpen(srcTexture);
        glPopDebugGroup();
    }

    if (settings.inputTextures.emissive) {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Add Emissive");
        glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_ONE);
        blitTexture(settings.inputTextures.emissive, 0);
        glDisable(GL_BLEND);
        glPopDebugGroup();
    }

    if (settings.depthOfField.enabled) {
        swapTarget();
        glDrawBuffer(dstBuffer);
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "DOF");
        applyDof(
            glObj.linearDepth.texture,
            srcTexture,
            settings.depthOfField.focusDepth,
            settings.depthOfField.focusScale,
            time
        );
        glPopDebugGroup();
    }

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Tonemapping");
    swapTarget();
    glDrawBuffer(dstBuffer);
    const Tonemapping tonemapper =
        settings.tonemapping.enabled ?
        settings.tonemapping.mode :
        Tonemapping::Passthrough;
    applyTonemapping(
        srcTexture,
        tonemapper,
        settings.tonemapping.exposure,
        settings.tonemapping.gamma
    );
    glPopDebugGroup();

    if (settings.inputTextures.postTonemap) {
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "Add Post Tonemap");
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blitTexture(settings.inputTextures.postTonemap, 0);
        glDisable(GL_BLEND);
        glPopDebugGroup();
    }

    prevJitter = jitter;

    if (settings.fxaa.enabled) {
        swapTarget();
        glDrawBuffer(dstBuffer);
        glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "FXAA");
        fxaa::applyFxaa(srcTexture, width, height);
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
        blitTexture(srcTexture, settings.inputTextures.depth);
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
