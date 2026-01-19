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

#include <modules/molecule/mol/viamd/postprocessing.h>

#include <modules/molecule/mol/viamd/postprocessing_shaders.inl>
#include <ghoul/opengl/ghoul_gl.h>
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
        uint32_t vShaderFsQuad = 0;
        uint32_t texWidth = 0;
        uint32_t texHeight = 0;

        struct {
            uint32_t fbo = 0;
            uint32_t texRgba8 = 0;
        } tmp;

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
            uint32_t programPersp = 0;
            uint32_t programOrtho = 0;
            struct {
                int clipInfo = -1;
                int texDepth = -1;
            } uniformLoc;
        } linearDepth;

        struct {
            uint32_t texRandom = 0;
            uint32_t uboHbaoData = 0;

            struct {
                uint32_t fbo = 0;
                uint32_t texture = 0;
                uint32_t programPersp = 0;
                uint32_t programOrtho = 0;
            } hbao;

            struct {
                uint32_t fbo = 0;
                uint32_t texture = 0;
                uint32_t program = 0;
            } blur;
        } ssao;

        struct {
            uint32_t program = 0;
            struct {
                int textureDepth = -1;
                int textureColor = -1;
                int textureNormal = -1;
                int invProjMat = -1;
                int lightDir = -1;
                int lightCol = -1;
                int time = -1;
            } uniformLoc;
        } shading;

        struct {
            uint32_t program = 0;
            struct {
                int texHalfRes = -1;
                int texColor = -1;
                int texDepth = -1;
                int pixelSize = -1;
                int focusPoint = -1;
                int focusScale = -1;
                int time = -1;
            } uniformLoc;

            struct {
                uint32_t fbo = 0;
                uint32_t program = 0;
                struct {
                    uint32_t colorCoc = 0;
                } tex;
                struct {
                    int texDepth = -1;
                    int texColor = -1;
                    int focusPoint = -1;
                    int focusScale = -1;
                } uniformLoc;
            } halfRes;
        } bokehDof;

        struct {
            uint32_t program = 0;
            struct {
                int mode = -1;
                int texColor = -1;
            } uniformLoc;

            struct {
                uint32_t program = 0;
                struct {
                    int texture = -1;
                } uniformLoc;
            } passthrough;

            struct {
                uint32_t program = 0;
                struct {
                    int texture = -1;
                    int exposure = -1;
                    int gamma = -1;
                } uniformLoc;
            } exposureGamma;

            struct {
                uint32_t program = 0;
                struct {
                    int texture = -1;
                    int exposure = -1;
                    int gamma = -1;
                } uniformLoc;
            } filmic;

            struct {
                uint32_t program = 0;
                struct {
                    int texture = -1;
                    int exposure = -1;
                    int gamma = -1;
                } uniformLoc;
            } aces;

            struct {
                uint32_t programForward = 0;
                uint32_t programInverse = 0;
                struct {
                    int texture = -1;
                } uniformLoc;
            } fastReversible;
        } tonemapping;

        struct {
            struct {
                uint32_t program = 0;
                struct {
                    int texLinearDepth = -1;
                    int texMain = -1;
                    int texPrev = -1;
                    int texVel = -1;
                    int texVelNeighbormax = -1;
                    int texelSize = -1;
                    int time = -1;
                    int feedbackMin = -1;
                    int feedbackMax = -1;
                    int motionScale = -1;
                    int jitterUv = -1;
                } uniformLoc;
            } withMotionBlur;
            struct {
                uint32_t program = 0;
                struct {
                    int texLinearDepth = -1;
                    int texMain = -1;
                    int texPrev = -1;
                    int texVel = -1;
                    int texelSize = -1;
                    int time = -1;
                    int feedbackMin = -1;
                    int feedbackMax = -1;
                    int motionScale = -1;
                    int jitterUv = -1;
                } uniformLoc;
            } noMotionBlur;
        } temporal;

        struct {
            uint32_t program = 0;
            struct {
                int tex = -1;
                int inverseScreenSize = -1;
            } uniformLoc;
        } fxaa;

        struct {
            uint32_t programTex = 0;
            uint32_t programTexDepth = 0;
            uint32_t programCol;
            struct {
                int texColor = -1;
                int texDepth = -1;
                int texture = -1;
                int color = -1;
            } uniformLoc;
        } blit;

        struct {
            uint32_t program = 0;
            struct {
                int texDepth = -1;
                int currClipToPrevClipMat = -1;
                int jitterUv = -1;
            } uniformLoc;
        } blitVelocity;

        struct {
            uint32_t program = 0;
            struct {
                int texVel = -1;
                int texVelTexelSize = -1;
            } uniformLoc;
        } blitTilemax;

        struct {
            uint32_t program = 0;
            struct {
                int texVel = -1;
                int texVelTexelSize = -1;
            } uniformLoc;
        } blitNeighbormax;

        struct {
            uint32_t program = 0;
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

    bool getShaderCompileError(char* buffer, int maxLength, GLuint shader) {
        GLint success = 0;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
        if (success == 0) {
            int length;
            glGetShaderInfoLog(shader, maxLength, &length, buffer);
        }
        return success == 0;
    }

    bool getProgramLinkError(char* buffer, int maxLength, GLuint program) {
        GLint success = 0;
        glGetProgramiv(program, GL_LINK_STATUS, &success);
        if (success == 0) {
            int length;
            glGetProgramInfoLog(program, maxLength, &length, buffer);
        }
        return success == 0;
    }

    uint32_t compileShaderFromSource(str_t src, GLenum type, str_t defines = {}) {
        ghoul_assert(
            type == GL_VERTEX_SHADER || type == GL_GEOMETRY_SHADER ||
            type == GL_FRAGMENT_SHADER || type == GL_COMPUTE_SHADER ||
            type == GL_TESS_CONTROL_SHADER || type == GL_TESS_EVALUATION_SHADER,
            "Wrong shader type"
        );

        uint32_t shader = glCreateShader(type);
        md_strb_t builder = {};
        md_strb_init(&builder, default_temp_allocator);

        // Skip to first # which should contain version
        while (src.len > 0 && src.ptr[0] != '#') {
            src.ptr++;
            src.len--;
        }

        str_t final_src = src;
        if (defines) {
            str_t version_str = {};

            if (str_equal_cstr_n(src, "#version ", 9)) {
                if (!str_extract_line(&version_str, &src)) {
                    MD_LOG_ERROR("Failed to extract version string!");
                    return 0;
                }
                md_strb_push_str(&builder, version_str);
                md_strb_push_char(&builder, '\n');
                md_strb_push_str(&builder, defines);
                md_strb_push_char(&builder, '\n');
            }
            else {
                md_strb_push_str(&builder, defines);
                md_strb_push_char(&builder, '\n');
            }
            md_strb_push_str(&builder, src);
            final_src = md_strb_to_str(&builder);
        }

        glShaderSource(shader, 1, &final_src.ptr, nullptr);
        md_strb_free(&builder);

        glCompileShader(shader);

        char buffer[1024];
        if (getShaderCompileError(buffer, sizeof(buffer), shader)) {
            MD_LOG_ERROR("%s\n", buffer);
            return 0;
        }

        return shader;
    }

    uint32_t setupProgramFromSource(str_t name, str_t shaderSrc, str_t defines = {}) {
        uint32_t shader = compileShaderFromSource(shaderSrc, GL_FRAGMENT_SHADER, defines);

        if (shader == 0) {
            return 0;
        }

        char buffer[1024];
        uint32_t program = glCreateProgram();

        glAttachShader(program, glObj.vShaderFsQuad);
        glAttachShader(program, shader);
        glLinkProgram(program);
        if (getProgramLinkError(buffer, sizeof(buffer), program)) {
            MD_LOG_ERROR("Error while linking %.*s program:\n%s", (int)name.len, name.ptr, buffer);
            glDeleteProgram(program);
            return 0;
        }

        glDetachShader(program, glObj.vShaderFsQuad);
        glDetachShader(program, shader);
        glDeleteShader(shader);
        return program;
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
    ghoul_assert(ubo, "Missing uniform bufffer object");

    // From intel ASSAO
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
            2.f / (projData[4 * 0 + 0]),                          // (x) * (R - L)/N
            2.f / (projData[4 * 1 + 1]),                          // (y) * (T - B)/N
            -(1.f - projData[4 * 2 + 0]) / projData[4 * 0 + 0],  // L/N
            -(1.f + projData[4 * 2 + 1]) / projData[4 * 1 + 1]   // B/N
        };
        projScl = float(height) * projData[4 * 1 + 1] * 0.5f;
    }
    else {
        projInfo = {
            2.f / (projData[4 * 0 + 0]),                          // ((x) * R - L)
            2.f / (projData[4 * 1 + 1]),                          // ((y) * T - B)
            -(1.f + projData[4 * 3 + 0]) / projData[4 * 0 + 0],  // L
            -(1.f - projData[4 * 3 + 1]) / projData[4 * 1 + 1]   // B
        };
        projScl = float(height) / projInfo.y;
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
        .invFullRes = {1.f / float(width), 1.f / float(height)},
        .projInfo = projInfo
    };
    memcpy(&data.samplePattern, SAMPLE_PATTERN, sizeof(SAMPLE_PATTERN));

    glBindBuffer(GL_UNIFORM_BUFFER, ubo);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), &data, GL_DYNAMIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

void initializeRndTex(uint32_t rndTex) {
    constexpr int AORandomTexSize = 4;
    constexpr int BufferSize = AORandomTexSize * AORandomTexSize;
    signed short buffer[BufferSize * 4];

    for (int i = 0; i < BufferSize; i++) {
        constexpr int Scale = 1 << 15;
        float rand1 = halton(i + 1, 2);
        float rand2 = halton(i + 1, 3);
        float angle = rand1 * glm::two_pi<float>();

        buffer[i * 4 + 0] = static_cast<signed short>(Scale * std::cos(angle));
        buffer[i * 4 + 1] = static_cast<signed short>(Scale * std::sin(angle));
        buffer[i * 4 + 2] = static_cast<signed short>(Scale * rand2);
        buffer[i * 4 + 3] = static_cast<signed short>(Scale * 0);
    }

    glBindTexture(GL_TEXTURE_2D, rndTex);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16_SNORM,
        AORandomTexSize,
        AORandomTexSize,
        0,
        GL_RGBA,
        GL_SHORT,
        buffer
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
}

void initialize(int width, int height) {
    ghoul_assert(glObj.ssao.hbao.fbo == 0, "Object already created");
    ghoul_assert(glObj.ssao.blur.fbo == 0, "Object already created");
    ghoul_assert(glObj.ssao.texRandom == 0, "Object already created");
    ghoul_assert(glObj.ssao.hbao.texture == 0, "Object already created");
    ghoul_assert(glObj.ssao.blur.texture == 0, "Object already created");
    ghoul_assert(glObj.ssao.uboHbaoData == 0, "Object already created");

    glObj.ssao.hbao.programPersp = setupProgramFromSource(STR("ssao persp"), f_shader_src_ssao, STR("#define AO_PERSPECTIVE 1"));
    glObj.ssao.hbao.programOrtho = setupProgramFromSource(STR("ssao ortho"), f_shader_src_ssao, STR("#define AO_PERSPECTIVE 0"));
    glObj.ssao.blur.program = setupProgramFromSource(STR("ssao blur"),  f_shader_src_ssao_blur);
    
    glGenFramebuffers(1, &glObj.ssao.hbao.fbo);
    glGenFramebuffers(1, &glObj.ssao.blur.fbo);

    glGenTextures(1, &glObj.ssao.texRandom);
    glGenTextures(1, &glObj.ssao.hbao.texture);
    glGenTextures(1, &glObj.ssao.blur.texture);
    glGenBuffers(1, &glObj.ssao.uboHbaoData);

    initializeRndTex(glObj.ssao.texRandom);

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

    glBindTexture(GL_TEXTURE_2D, 0);

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

    glDeleteProgram(glObj.ssao.hbao.programPersp);
    glDeleteProgram(glObj.ssao.hbao.programOrtho);
    glDeleteProgram(glObj.ssao.blur.program);
}

}  // namespace ssao

namespace shading {

void initialize() {
    glObj.shading.program = setupProgramFromSource(STR("deferred shading"), f_shader_src_deferred_shading);
    glObj.shading.uniformLoc.textureDepth = glGetUniformLocation(glObj.shading.program, "u_texture_depth");
    glObj.shading.uniformLoc.textureColor = glGetUniformLocation(glObj.shading.program, "u_texture_color");
    glObj.shading.uniformLoc.textureNormal = glGetUniformLocation(glObj.shading.program, "u_texture_normal");
    glObj.shading.uniformLoc.invProjMat = glGetUniformLocation(glObj.shading.program, "u_inv_proj_mat");
    glObj.shading.uniformLoc.lightDir = glGetUniformLocation(glObj.shading.program, "u_light_dir");
    glObj.shading.uniformLoc.lightCol = glGetUniformLocation(glObj.shading.program, "u_light_col");
    glObj.shading.uniformLoc.time = glGetUniformLocation(glObj.shading.program, "u_time");
}

void shutdown() {
    glDeleteProgram(glObj.shading.program);
}

}  // namespace shading

namespace tonemapping {

void initialize() {
    // PASSTHROUGH
    glObj.tonemapping.passthrough.program = setupProgramFromSource(STR("Passthrough"), f_shader_src_tonemap_passthrough);
    glObj.tonemapping.passthrough.uniformLoc.texture = glGetUniformLocation(glObj.tonemapping.passthrough.program, "u_texture");

    // EXPOSURE GAMMA
    glObj.tonemapping.exposureGamma.program = setupProgramFromSource(STR("Exposure Gamma"), f_shader_src_tonemap_exposure_gamma);
    glObj.tonemapping.exposureGamma.uniformLoc.texture = glGetUniformLocation(glObj.tonemapping.exposureGamma.program, "u_texture");
    glObj.tonemapping.exposureGamma.uniformLoc.exposure = glGetUniformLocation(glObj.tonemapping.exposureGamma.program, "u_exposure");
    glObj.tonemapping.exposureGamma.uniformLoc.gamma = glGetUniformLocation(glObj.tonemapping.exposureGamma.program, "u_gamma");

    // FILMIC (UNCHARTED)
    glObj.tonemapping.filmic.program = setupProgramFromSource(STR("Filmic"), f_shader_src_tonemap_filmic);
    glObj.tonemapping.filmic.uniformLoc.texture = glGetUniformLocation(glObj.tonemapping.filmic.program, "u_texture");
    glObj.tonemapping.filmic.uniformLoc.exposure = glGetUniformLocation(glObj.tonemapping.filmic.program, "u_exposure");
    glObj.tonemapping.filmic.uniformLoc.gamma = glGetUniformLocation(glObj.tonemapping.filmic.program, "u_gamma");

    // ACES
    glObj.tonemapping.aces.program = setupProgramFromSource(STR("ACES"), f_shader_src_tonemap_aces);
    glObj.tonemapping.aces.uniformLoc.texture = glGetUniformLocation(glObj.tonemapping.aces.program, "u_texture");
    glObj.tonemapping.aces.uniformLoc.exposure = glGetUniformLocation(glObj.tonemapping.aces.program, "u_exposure");
    glObj.tonemapping.aces.uniformLoc.gamma = glGetUniformLocation(glObj.tonemapping.aces.program, "u_gamma");
}

void shutdown() {
    glDeleteProgram(glObj.tonemapping.passthrough.program);
    glDeleteProgram(glObj.tonemapping.exposureGamma.program);
    glDeleteProgram(glObj.tonemapping.filmic.program);
    glDeleteProgram(glObj.tonemapping.aces.program);
}

}  // namespace tonemapping

namespace dof {

void initialize(int32_t width, int32_t height) {
    ghoul_assert(glObj.bokehDof.halfRes.tex.colorCoc == 0, "Object already created");
    ghoul_assert(glObj.bokehDof.halfRes.fbo == 0, "Object already created");

    glObj.bokehDof.halfRes.program = setupProgramFromSource(STR("DOF prepass"), f_shader_src_dof_halfres_prepass);
    glObj.bokehDof.halfRes.uniformLoc.texDepth = glGetUniformLocation(glObj.bokehDof.halfRes.program, "u_tex_depth");
    glObj.bokehDof.halfRes.uniformLoc.texColor = glGetUniformLocation(glObj.bokehDof.halfRes.program, "u_tex_color");
    glObj.bokehDof.halfRes.uniformLoc.focusPoint = glGetUniformLocation(glObj.bokehDof.halfRes.program, "u_focus_point");
    glObj.bokehDof.halfRes.uniformLoc.focusScale = glGetUniformLocation(glObj.bokehDof.halfRes.program, "u_focus_scale");

    glGenTextures(1, &glObj.bokehDof.halfRes.tex.colorCoc);
    glBindTexture(GL_TEXTURE_2D, glObj.bokehDof.halfRes.tex.colorCoc);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, width / 2, height / 2, 0, GL_RGBA, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenFramebuffers(1, &glObj.bokehDof.halfRes.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, glObj.bokehDof.halfRes.tex.colorCoc, 0);
    GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        MD_LOG_ERROR("Something went wrong when generating framebuffer for DOF");
    }
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    // DOF
    glObj.bokehDof.program = setupProgramFromSource(STR("Bokeh DOF"), f_shader_src_dof);
    glObj.bokehDof.uniformLoc.texHalfRes = glGetUniformLocation(glObj.bokehDof.program, "u_half_res");
    glObj.bokehDof.uniformLoc.texColor = glGetUniformLocation(glObj.bokehDof.program, "u_tex_color");
    glObj.bokehDof.uniformLoc.texDepth = glGetUniformLocation(glObj.bokehDof.program, "u_tex_depth");
    glObj.bokehDof.uniformLoc.pixelSize = glGetUniformLocation(glObj.bokehDof.program, "u_texel_size");
    glObj.bokehDof.uniformLoc.focusPoint = glGetUniformLocation(glObj.bokehDof.program, "u_focus_depth");
    glObj.bokehDof.uniformLoc.focusScale = glGetUniformLocation(glObj.bokehDof.program, "u_focus_scale");
    glObj.bokehDof.uniformLoc.time = glGetUniformLocation(glObj.bokehDof.program, "u_time");
}

void shutdown() {}

}  // namespace dof

namespace blit {

void initialize() {
    constexpr str_t f_shader_src_tex = STR(R"(
#version 150 core

uniform sampler2D u_texture;

out vec4 out_frag;

void main() {
  out_frag = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
}
)");

    glObj.blit.programTex = setupProgramFromSource(STR("blit texture"), f_shader_src_tex);
    glObj.blit.uniformLoc.texture = glGetUniformLocation(glObj.blit.programTex, "u_texture");

    constexpr str_t f_shader_src_tex_depth = STR(R"(
#version 150 core

uniform sampler2D u_tex_color;
uniform sampler2D u_tex_depth;

out vec4 out_frag;

void main() {
  float depth = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
  if (depth == 1.0) {
    out_frag = vec4(0,0,0,0);
  }
  else {
    out_frag = texelFetch(u_tex_color, ivec2(gl_FragCoord.xy), 0);
  }
}
)");
    glObj.blit.programTexDepth = setupProgramFromSource(STR("blit texture with depth"), f_shader_src_tex_depth);
    glObj.blit.uniformLoc.texColor = glGetUniformLocation(glObj.blit.programTexDepth, "u_tex_color");
    glObj.blit.uniformLoc.texDepth = glGetUniformLocation(glObj.blit.programTexDepth, "u_tex_depth");

    constexpr str_t f_shader_src_col = STR(R"(
#version 150 core

uniform vec4 u_color;
out vec4 out_frag;

void main() {
  out_frag = u_color;
}
)");
    glObj.blit.programCol = setupProgramFromSource(STR("blit color"), f_shader_src_col);
    glObj.blit.uniformLoc.color = glGetUniformLocation(glObj.blit.programCol, "u_color");
}

void shutdown() {
    glDeleteProgram(glObj.blit.programTex);
    glDeleteProgram(glObj.blit.programTexDepth);
    glDeleteProgram(glObj.blit.programCol);
}

}  // namespace blit

namespace velocity {

void initialize(int32_t width, int32_t height) {
    constexpr int VelTileSize = 8;

    ghoul_assert(glObj.velocity.texTilemax == 0, "Object already created");
    ghoul_assert(glObj.velocity.texNeighbormax == 0, "Object already created");
    ghoul_assert(glObj.velocity.fbo == 0, "Object already created");
    
    glObj.blitVelocity.program = setupProgramFromSource(STR("screen-space velocity"), f_shader_src_vel_blit);
    glObj.blitVelocity.uniformLoc.texDepth = glGetUniformLocation(glObj.blitVelocity.program, "u_tex_depth");
    glObj.blitVelocity.uniformLoc.currClipToPrevClipMat = glGetUniformLocation(glObj.blitVelocity.program, "u_curr_clip_to_prev_clip_mat");
    glObj.blitVelocity.uniformLoc.jitterUv = glGetUniformLocation(glObj.blitVelocity.program, "u_jitter_uv");

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
    std::string exp = std::format("#define TILE_SIZE {}", VelTileSize);
    str_t defines = str_from_cstr(exp.c_str());
    glObj.blitTilemax.program = setupProgramFromSource(STR("tilemax"), f_shader_src_vel_tilemax, defines);
    glObj.blitTilemax.uniformLoc.texVel = glGetUniformLocation(glObj.blitTilemax.program, "u_tex_vel");
    glObj.blitTilemax.uniformLoc.texVelTexelSize = glGetUniformLocation(glObj.blitTilemax.program, "u_tex_vel_texel_size");
#undef STRINGIFY
#undef TOSTRING
    glObj.blitNeighbormax.program = setupProgramFromSource(STR("neighbormax"), f_shader_src_vel_neighbormax);
    glObj.blitNeighbormax.uniformLoc.texVel = glGetUniformLocation(glObj.blitNeighbormax.program, "u_tex_vel");
    glObj.blitNeighbormax.uniformLoc.texVelTexelSize = glGetUniformLocation(glObj.blitNeighbormax.program, "u_tex_vel_texel_size");

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
    glBindTexture(GL_TEXTURE_2D, 0);

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
    glBindTexture(GL_TEXTURE_2D, 0);

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
    GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        MD_LOG_ERROR("Something went wrong in creating framebuffer for velocity");
    }
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
}

void shutdown() {
    glDeleteProgram(glObj.blitVelocity.program);
    glDeleteTextures(1, &glObj.velocity.texTilemax);
    glDeleteTextures(1, &glObj.velocity.texNeighbormax);
    glDeleteFramebuffers(1, &glObj.velocity.fbo);
}

}  // namespace velocity

namespace temporal {

void initialize() {
    glObj.temporal.withMotionBlur.program = setupProgramFromSource(STR("temporal aa + motion-blur"), f_shader_src_temporal);

    glObj.temporal.withMotionBlur.uniformLoc.texLinearDepth = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_tex_linear_depth");
    glObj.temporal.withMotionBlur.uniformLoc.texMain = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_tex_main");
    glObj.temporal.withMotionBlur.uniformLoc.texPrev = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_tex_prev");
    glObj.temporal.withMotionBlur.uniformLoc.texVel = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_tex_vel");
    glObj.temporal.withMotionBlur.uniformLoc.texVelNeighbormax = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_tex_vel_neighbormax");
    glObj.temporal.withMotionBlur.uniformLoc.texelSize = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_texel_size");
    glObj.temporal.withMotionBlur.uniformLoc.jitterUv = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_jitter_uv");
    glObj.temporal.withMotionBlur.uniformLoc.time = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_time");
    glObj.temporal.withMotionBlur.uniformLoc.feedbackMin = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_feedback_min");
    glObj.temporal.withMotionBlur.uniformLoc.feedbackMax = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_feedback_max");
    glObj.temporal.withMotionBlur.uniformLoc.motionScale = glGetUniformLocation(glObj.temporal.withMotionBlur.program, "u_motion_scale");

    glObj.temporal.noMotionBlur.program = setupProgramFromSource(STR("temporal aa"), f_shader_src_temporal, STR("#define USE_MOTION_BLUR 0\n"));
    glObj.temporal.noMotionBlur.uniformLoc.texLinearDepth = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_tex_linear_depth");
    glObj.temporal.noMotionBlur.uniformLoc.texMain = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_tex_main");
    glObj.temporal.noMotionBlur.uniformLoc.texPrev = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_tex_prev");
    glObj.temporal.noMotionBlur.uniformLoc.texVel = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_tex_vel");
    glObj.temporal.noMotionBlur.uniformLoc.texelSize = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_texel_size");
    glObj.temporal.noMotionBlur.uniformLoc.jitterUv = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_jitter_uv");
    glObj.temporal.noMotionBlur.uniformLoc.time = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_time");
    glObj.temporal.noMotionBlur.uniformLoc.feedbackMin = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_feedback_min");
    glObj.temporal.noMotionBlur.uniformLoc.feedbackMax = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_feedback_max");
    glObj.temporal.noMotionBlur.uniformLoc.motionScale = glGetUniformLocation(glObj.temporal.noMotionBlur.program, "u_motion_scale");
}

void shutdown() {}

} // namespace temporal

namespace sharpen {

void initialize() {
    constexpr str_t f_shader_src_sharpen = STR(
 R"(
#version 150 core

uniform sampler2D u_tex;
out vec4 out_frag;

void main() {
    vec3 cc = texelFetch(u_tex, ivec2(gl_FragCoord.xy), 0).rgb;
    vec3 cl = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2(-1, 0), 0).rgb;
    vec3 ct = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 0, 1), 0).rgb;
    vec3 cr = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 1, 0), 0).rgb;
    vec3 cb = texelFetch(u_tex, ivec2(gl_FragCoord.xy) + ivec2( 0,-1), 0).rgb;

    const float weight[2] = float[2](1.4, -0.1);
    out_frag = vec4(vec3(weight[0] * cc + weight[1] * (cl + ct + cr + cb)), 1.0);
})");
    glObj.sharpen.program = setupProgramFromSource(STR("sharpen"), f_shader_src_sharpen);
}

void sharpen(uint32_t texture) {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);

    glUseProgram(glObj.sharpen.program);
    glUniform1i(glGetUniformLocation(glObj.sharpen.program, "u_tex"), 0);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glUseProgram(0);
}

void shutdown() {
    glDeleteProgram(glObj.sharpen.program);
}

}   // namespace sharpen

namespace fxaa {

void initialize() {
    ghoul_assert(glObj.fxaa.program == 0, "Object already created");

    glObj.fxaa.program = setupProgramFromSource(STR("fxaa"), f_shader_src_fxaa);
    glObj.fxaa.uniformLoc.tex = glGetUniformLocation(glObj.fxaa.program, "tex");
    glObj.fxaa.uniformLoc.inverseScreenSize = glGetUniformLocation(
        glObj.fxaa.program,
        "inverseScreenSize"
    );
}

void applyFxaa(uint32_t texture, int width, int height) {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);

    glUseProgram(glObj.fxaa.program);
    glUniform1i(glObj.fxaa.uniformLoc.tex, 0);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);

    vec2_t inv_screen_size = { 1.f / width, 1.f / height };
    glUniform2fv(glObj.fxaa.uniformLoc.inverseScreenSize, 1, inv_screen_size.elem);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glUseProgram(0);
}

void shutdown() {
    glDeleteProgram(glObj.fxaa.program);
}

}   // namespace fxaa

void initialize(int width, int height) {
    static bool IsInitialized = false;

    if (IsInitialized) {
        return;
    }

    glGenVertexArrays(1, &glObj.vao);

    glObj.vShaderFsQuad = compileShaderFromSource(v_shader_src_fs_quad, GL_VERTEX_SHADER);

    // LINEARIZE DEPTH
    glObj.linearDepth.programPersp = setupProgramFromSource(
        STR("linearize depth persp"),
        f_shader_src_linearize_depth,
        STR("#define PERSPECTIVE 1")
    );
    glObj.linearDepth.programOrtho = setupProgramFromSource(
        STR("linearize depth ortho"),
        f_shader_src_linearize_depth,
        STR("#define PERSPECTIVE 0")
    );

    glGenTextures(1, &glObj.linearDepth.texture);
    glBindTexture(GL_TEXTURE_2D, glObj.linearDepth.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenFramebuffers(1, &glObj.linearDepth.fbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.linearDepth.fbo);
    glFramebufferTexture2D(
        GL_DRAW_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        glObj.linearDepth.texture,
        0
    );
    GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        MD_LOG_ERROR("Something went wrong in creating framebuffer for depth linearization");
    }
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glObj.linearDepth.uniformLoc.clipInfo = glGetUniformLocation(
        glObj.linearDepth.programPersp, "u_clip_info"
    );
    glObj.linearDepth.uniformLoc.texDepth = glGetUniformLocation(
        glObj.linearDepth.programPersp, "u_tex_depth"
    );

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
    glBindTexture(GL_TEXTURE_2D, 0);

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
    glBindTexture(GL_TEXTURE_2D, 0);

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

    status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        MD_LOG_ERROR("Something went wrong in creating framebuffer for targets");
    }

    GLenum buffers[] = {
        GL_COLOR_ATTACHMENT0,
        GL_COLOR_ATTACHMENT1,
        GL_COLOR_ATTACHMENT2,
        GL_COLOR_ATTACHMENT3
    };
    glDrawBuffers(4, buffers);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

    glGenFramebuffers(1, &glObj.tmp.fbo);

    glGenTextures(1, &glObj.tmp.texRgba8);
    glBindTexture(GL_TEXTURE_2D, glObj.tmp.texRgba8);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA8,
        width,
        height,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

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

    IsInitialized = true;
}

void shutdown() {
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
    glDeleteShader(glObj.vShaderFsQuad);
    glDeleteFramebuffers(1, &glObj.tmp.fbo);
    glDeleteTextures(1, &glObj.tmp.texRgba8);
}

void computeLinearDepth(uint32_t depthTex, float nearPlane, float farPlane,
                        bool orthographic = false)
{
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depthTex);

    if (orthographic) {
        glUseProgram(glObj.linearDepth.programOrtho);
    }
    else {
        glUseProgram(glObj.linearDepth.programPersp);
    }
    glUniform1i(glObj.linearDepth.uniformLoc.texDepth, 0);
    const vec4_t clip = { nearPlane * farPlane, nearPlane - farPlane, farPlane, 0 };
    glUniform4fv(glObj.linearDepth.uniformLoc.clipInfo, 1, &clip.x);

    // ASSUME THAT THE APPROPRIATE FS_QUAD VAO IS BOUND
    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void applySsao(uint32_t linearDepthTex, uint32_t normalTex, const mat4_t& projMatrix,
               float intensity, float radius, float bias, float normalBias)
{
    ghoul_assert(glIsTexture(linearDepthTex), "No texture");
    ghoul_assert(glIsTexture(normalTex), "No texture");

    const bool ortho = isOrthoProjMatrix(projMatrix);
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
    uint32_t program =
        ortho ? glObj.ssao.hbao.programOrtho : glObj.ssao.hbao.programPersp;

    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "HBAO");
    glUseProgram(program);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normalTex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.texRandom);

    glBindBufferBase(GL_UNIFORM_BUFFER, 0, glObj.ssao.uboHbaoData);
    glUniformBlockBinding(program, glGetUniformBlockIndex(program, "u_control_buffer"), 0);
    glUniform1i(glGetUniformLocation(program, "u_tex_linear_depth"), 0);
    glUniform1i(glGetUniformLocation(program, "u_tex_normal"), 1);
    glUniform1i(glGetUniformLocation(program, "u_tex_random"), 2);
    glUniform2f(
        glGetUniformLocation(program, "u_tc_scl"),
        static_cast<float>(width) / static_cast<float>(glObj.texWidth),
        static_cast<float>(height) / static_cast<float>(glObj.texHeight)
    );

    glDrawArrays(GL_TRIANGLES, 0, 3);
    glPopDebugGroup();

    glUseProgram(glObj.ssao.blur.program);

    glUniform1i(glGetUniformLocation(glObj.ssao.blur.program, "u_tex_linear_depth"), 0);
    glUniform1i(glGetUniformLocation(glObj.ssao.blur.program, "u_tex_ao"), 1);
    glUniform1f(glGetUniformLocation(glObj.ssao.blur.program, "u_sharpness"), sharpness);
    glUniform2f(glGetUniformLocation(glObj.ssao.blur.program, "u_inv_res_dir"), invRes.x, 0);
    glUniform2f(
        glGetUniformLocation(glObj.ssao.blur.program, "u_tc_scl"),
        static_cast<float>(width) / static_cast<float>(glObj.texWidth),
        static_cast<float>(height) / static_cast<float>(glObj.texHeight)
    );

    glActiveTexture(GL_TEXTURE1);

    // BLUR FIRST
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 1st");
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.ssao.blur.fbo);
    glViewport(0, 0, width, height);
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.hbao.texture);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glPopDebugGroup();

    // BLUR SECOND
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "BLUR 2nd");
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, glObj.ssao.blur.texture);
    glUniform2f(
        glGetUniformLocation(glObj.ssao.blur.program, "u_inv_res_dir"),
        0,
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
                   const vec3_t& lightCol, float time)
{
    ghoul_assert(glIsTexture(depthTex), "No texture");
    ghoul_assert(glIsTexture(colorTex), "No texture");
    ghoul_assert(glIsTexture(normalTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depthTex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, colorTex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, normalTex);

    glUseProgram(glObj.shading.program);
    glUniform1i(glObj.shading.uniformLoc.textureDepth, 0);
    glUniform1i(glObj.shading.uniformLoc.textureColor, 1);
    glUniform1i(glObj.shading.uniformLoc.textureNormal, 2);
    glUniformMatrix4fv(glObj.shading.uniformLoc.invProjMat, 1, GL_FALSE, &invProjMatrix.elem[0][0]);
    glUniform3f(glObj.shading.uniformLoc.lightDir, lightDir.x, lightDir.y, lightDir.z);
    glUniform3f(glObj.shading.uniformLoc.lightCol, lightCol.x, lightCol.y, lightCol.z);
    glUniform1f(glObj.shading.uniformLoc.time, time);
    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void halfResColorCoc(uint32_t linearDepthTex, uint32_t colorTex, float focusPoint,
                     float focusScale)
{
    glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, "DOF Prepass");
    int lastViewport[4];
    glGetIntegerv(GL_VIEWPORT, lastViewport);
    glViewport(0, 0, glObj.texWidth / 2, glObj.texHeight / 2);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, glObj.bokehDof.halfRes.fbo);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glUseProgram(glObj.bokehDof.halfRes.program);

    glUniform1i(glObj.bokehDof.halfRes.uniformLoc.texDepth, 0);
    glUniform1i(glObj.bokehDof.halfRes.uniformLoc.texColor, 1);
    glUniform1f(glObj.bokehDof.halfRes.uniformLoc.focusPoint, focusPoint);
    glUniform1f(glObj.bokehDof.halfRes.uniformLoc.focusScale, focusScale);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glViewport(lastViewport[0], lastViewport[1], lastViewport[2], lastViewport[3]);
    glPopDebugGroup();
}

void applyDof(uint32_t linearDepthTex, uint32_t colorTex, float focusPoint,
              float focusScale, float time)
{
    ghoul_assert(glIsTexture(linearDepthTex), "No texture");
    ghoul_assert(glIsTexture(colorTex), "No texture");

    int lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);

    halfResColorCoc(linearDepthTex, colorTex, focusPoint, focusScale);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, glObj.bokehDof.halfRes.tex.colorCoc);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glUseProgram(glObj.bokehDof.program);
    glUniform1i(glObj.bokehDof.uniformLoc.texHalfRes, 0);
    glUniform1i(glObj.bokehDof.uniformLoc.texDepth, 1);
    glUniform1i(glObj.bokehDof.uniformLoc.texColor, 2);
    glUniform2f(
        glObj.bokehDof.uniformLoc.pixelSize,
        1.f / glObj.texWidth,
        1.f / glObj.texHeight
    );
    glUniform1f(glObj.bokehDof.uniformLoc.focusPoint, focusPoint);
    glUniform1f(glObj.bokehDof.uniformLoc.focusScale, focusScale);
    glUniform1f(glObj.bokehDof.uniformLoc.time, time);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
    glActiveTexture(GL_TEXTURE0);
}

void applyTonemapping(uint32_t colorTex, Tonemapping tonemapping, float exposure,
                      float gamma)
{
    ghoul_assert(glIsTexture(colorTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    switch (tonemapping) {
        case Tonemapping::Passthrough:
            glUseProgram(glObj.tonemapping.passthrough.program);
            glUniform1i(glObj.tonemapping.passthrough.uniformLoc.texture, 0);
            break;
        case Tonemapping::ExposureGamma:
            glUseProgram(glObj.tonemapping.exposureGamma.program);
            glUniform1i(glObj.tonemapping.exposureGamma.uniformLoc.texture, 0);
            glUniform1f(glObj.tonemapping.exposureGamma.uniformLoc.exposure, exposure);
            glUniform1f(glObj.tonemapping.exposureGamma.uniformLoc.gamma, gamma);
            break;
        case Tonemapping::Filmic:
            glUseProgram(glObj.tonemapping.filmic.program);
            glUniform1i(glObj.tonemapping.filmic.uniformLoc.texture, 0);
            glUniform1f(glObj.tonemapping.filmic.uniformLoc.exposure, exposure);
            glUniform1f(glObj.tonemapping.filmic.uniformLoc.gamma, gamma);
            break;
        case Tonemapping::ACES:
            glUseProgram(glObj.tonemapping.aces.program);
            glUniform1i(glObj.tonemapping.aces.uniformLoc.texture, 0);
            glUniform1f(glObj.tonemapping.aces.uniformLoc.exposure, exposure);
            glUniform1f(glObj.tonemapping.aces.uniformLoc.gamma, gamma);
            break;
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void applyAaTonemapping(uint32_t colorTex) {
    ghoul_assert(glIsTexture(colorTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glUseProgram(glObj.tonemapping.fastReversible.programForward);
    glUniform1i(glObj.tonemapping.fastReversible.uniformLoc.texture, 0);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void applyInverseAaTonemapping(uint32_t colorTex) {
    ghoul_assert(glIsTexture(colorTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glUseProgram(glObj.tonemapping.fastReversible.programInverse);
    glUniform1i(glObj.tonemapping.fastReversible.uniformLoc.texture, 0);

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blitTilemax(uint32_t velocityTex, int texWidth, int texHeight) {
    ghoul_assert(glIsTexture(velocityTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    glUseProgram(glObj.blitTilemax.program);
    glUniform1i(glObj.blitTilemax.uniformLoc.texVel, 0);
    const vec2_t texelSize = { 1.f / texWidth, 1.f / texHeight };
    glUniform2fv(glObj.blitTilemax.uniformLoc.texVelTexelSize, 1, &texelSize.x);
    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blitNeighbormax(uint32_t velocityTex, int texWwidth, int texHeight) {
    ghoul_assert(glIsTexture(velocityTex), "No texture");

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    glUseProgram(glObj.blitNeighbormax.program);
    glUniform1i(glObj.blitNeighbormax.uniformLoc.texVel, 0);
    const vec2_t texelSize = { 1.f / texWwidth, 1.f / texHeight };
    glUniform2fv(glObj.blitNeighbormax.uniformLoc.texVelTexelSize, 1, &texelSize.x);
    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void applyTemporalAa(uint32_t linearDepthTex, uint32_t colorTex, uint32_t velocityTex,
                     uint32_t velocityNeighbormaxTex, const vec2_t& currJitter,
                     const vec2_t& prevJitter, float feedbackMin, float feedbackMax,
                     float motionScale, float time)
{
    ghoul_assert(glIsTexture(linearDepthTex), "No texture");
    ghoul_assert(glIsTexture(colorTex), "No texture");
    ghoul_assert(glIsTexture(velocityTex), "No texture");
    ghoul_assert(glIsTexture(velocityNeighbormaxTex), "No texture");

    static int target = 0;
    target = (target + 1) % 2;

    const int dstBuf = target;
    const int srcBuf = (target + 1) % 2;

    const vec2_t res = {
        static_cast<float>(glObj.texWidth),
        static_cast<float>(glObj.texHeight)
    };
    const vec2_t invRes = 1.f / res;
    const vec4_t texelSize = { invRes.x, invRes.y, res.x, res.y };
    const vec2_t jitterUvCurr = currJitter / res;
    const vec2_t jitterUvPrev = prevJitter / res;
    const vec4_t jitterUv = {
        jitterUvCurr.x,
        jitterUvCurr.y,
        jitterUvPrev.x,
        jitterUvPrev.y
    };

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linearDepthTex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, colorTex);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, glObj.targets.texTemporalBuffer[srcBuf]);

    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, velocityTex);

    glActiveTexture(GL_TEXTURE4);
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
        glUseProgram(glObj.temporal.withMotionBlur.program);

        glUniform1i(glObj.temporal.withMotionBlur.uniformLoc.texLinearDepth, 0);
        glUniform1i(glObj.temporal.withMotionBlur.uniformLoc.texMain, 1);
        glUniform1i(glObj.temporal.withMotionBlur.uniformLoc.texPrev, 2);
        glUniform1i(glObj.temporal.withMotionBlur.uniformLoc.texVel, 3);
        glUniform1i(glObj.temporal.withMotionBlur.uniformLoc.texVelNeighbormax, 4);

        glUniform4fv(glObj.temporal.withMotionBlur.uniformLoc.texelSize, 1, &texelSize.x);
        glUniform4fv(glObj.temporal.withMotionBlur.uniformLoc.jitterUv, 1, &jitterUv.x);
        glUniform1f(glObj.temporal.withMotionBlur.uniformLoc.time, time);
        glUniform1f(glObj.temporal.withMotionBlur.uniformLoc.feedbackMin, feedbackMin);
        glUniform1f(glObj.temporal.withMotionBlur.uniformLoc.feedbackMax, feedbackMax);
        glUniform1f(glObj.temporal.withMotionBlur.uniformLoc.motionScale, motionScale);
    }
    else {
        glUseProgram(glObj.temporal.noMotionBlur.program);

        glUniform1i(glObj.temporal.noMotionBlur.uniformLoc.texLinearDepth, 0);
        glUniform1i(glObj.temporal.noMotionBlur.uniformLoc.texMain, 1);
        glUniform1i(glObj.temporal.noMotionBlur.uniformLoc.texPrev, 2);
        glUniform1i(glObj.temporal.noMotionBlur.uniformLoc.texVel, 3);

        glUniform4fv(glObj.temporal.noMotionBlur.uniformLoc.texelSize, 1, &texelSize.x);
        glUniform4fv(glObj.temporal.noMotionBlur.uniformLoc.jitterUv, 1, &jitterUv.x);
        glUniform1f(glObj.temporal.noMotionBlur.uniformLoc.time, time);
        glUniform1f(glObj.temporal.noMotionBlur.uniformLoc.feedbackMin, feedbackMin);
        glUniform1f(glObj.temporal.noMotionBlur.uniformLoc.feedbackMax, feedbackMax);
        glUniform1f(glObj.temporal.noMotionBlur.uniformLoc.motionScale, motionScale);
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blitTexture(uint32_t tex, uint32_t depth) {
    ghoul_assert(glIsTexture(tex), "No texture");
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex);

    if (depth) {
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, depth);
        glUseProgram(glObj.blit.programTexDepth);
        glUniform1i(glObj.blit.uniformLoc.texColor, 0);
        glUniform1i(glObj.blit.uniformLoc.texDepth, 1);
    }
    else {
        glUseProgram(glObj.blit.programTex);
        glUniform1i(glObj.blit.uniformLoc.texture, 0);
    }

    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
    glActiveTexture(GL_TEXTURE0);
}

void blitColor(vec4_t color) {
    glUseProgram(glObj.blit.programCol);
    glUniform4fv(glObj.blit.uniformLoc.color, 1, &color.x);
    glBindVertexArray(glObj.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void postprocess(const Settings& settings, const mat4_t& V, const mat4_t& P) {
    ghoul_assert(glIsTexture(settings.inputTextures.depth), "No texture");
    ghoul_assert(glIsTexture(settings.inputTextures.color), "No texture");
    ghoul_assert(glIsTexture(settings.inputTextures.normal), "No texture");
    if (settings.temporalReprojection.enabled) {
        ghoul_assert(glIsTexture(settings.inputTextures.velocity), "No texture");
    }

    // For seeding noise
    static float time = 0.f;
    time = time + 0.016f;
    if (time > 100.f) {
        time -= 100.f;
    }

    static vec2_t prevJitter = { 0.f, 0.f };
    vec3_t L = mat4_mul_vec3(V, vec3_set(0.f, 0.f, 0.f), 1.0f);
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
        initialize(width, height);
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
        const float motion_scale =
            settings.temporalReprojection.motionBlur.enabled ?
            settings.temporalReprojection.motionBlur.motionScale :
            0.f;

        if (motion_scale != 0.f)
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
                motion_scale,
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
