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

#ifdef _MSC_VER
// Disable double to float truncation warnings from table
#pragma warning( disable : 4305 )
#endif

#include "postprocessing.h"

#include <core/md_allocator.h>
#include <core/md_str.h>
#include <core/md_log.h>
#include <core/md_str_builder.h>
#include <core/md_vec_math.h>

#include <stdio.h>
#include <string.h>
#include <float.h>

#include "postprocessing_shaders.inl"

#include <ghoul/opengl/ghoul_gl.h>

#define PUSH_GPU_SECTION(lbl) {if (&glPushDebugGroup) glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, lbl);}
#define POP_GPU_SECTION()     {if (&glPopDebugGroup) glPopDebugGroup();}

namespace postprocessing {

// @TODO: Use half-res render targets for SSAO
// @TODO: Use shared textures for all postprocessing operations
// @TODO: Use some kind of unified pipeline for all post processing operations

static struct {
    uint32_t vao = 0;
    uint32_t v_shader_fs_quad = 0;
    uint32_t tex_width = 0;
    uint32_t tex_height = 0;

    struct {
        uint32_t fbo = 0;
        uint32_t tex_rgba8 = 0;
    } tmp;

    struct {
        uint32_t fbo = 0;
        uint32_t tex_color[2] = {0, 0};
        uint32_t tex_temporal_buffer[2] = {0, 0};  // These are dedicated and cannot be use as intermediate buffers by other shaders
    } targets;

    struct {
        uint32_t fbo = 0;
        uint32_t tex_tilemax = 0;
        uint32_t tex_neighbormax = 0;
        int32_t tex_width = 0;
        int32_t tex_height = 0;
    } velocity;

    struct {
        uint32_t fbo = 0;
        uint32_t texture = 0;
        uint32_t program_persp = 0;
        uint32_t program_ortho = 0;
        struct {
            int clip_info = -1;
            int tex_depth = -1;
        } uniform_loc;
    } linear_depth;

    struct {
        uint32_t tex_random = 0;
        uint32_t ubo_hbao_data = 0;

        struct {
            uint32_t fbo = 0;
            uint32_t texture = 0;
            uint32_t program_persp = 0;
            uint32_t program_ortho = 0;
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
            int tex_half_res = -1;
            int tex_color = -1;
            int tex_depth = -1;
            int pixel_size = -1;
            int focus_point = -1;
            int focus_scale = -1;
            int time = -1;
        } uniform_loc;

        struct {
            uint32_t fbo = 0;
            uint32_t program = 0;
            struct {
                uint32_t color_coc = 0;
            } tex;
            struct {
                int tex_depth = -1;
                int tex_color = -1;
                int focus_point = -1;
                int focus_scale = -1;
            } uniform_loc;
        } half_res;
    } bokeh_dof;

    struct {
        uint32_t program = 0;
        struct {
            int mode = -1;
            int tex_color = -1;
        } uniform_loc;
    } tonemapping;

    struct {
        struct {
            uint32_t program = 0;
            struct {
                int tex_linear_depth = -1;
                int tex_main = -1;
                int tex_prev = -1;
                int tex_vel = -1;
                int tex_vel_neighbormax = -1;
                int texel_size = -1;
                int time = -1;
                int feedback_min = -1;
                int feedback_max = -1;
                int motion_scale = -1;
                int jitter_uv = -1;
            } uniform_loc;
        } with_motion_blur;
        struct {
            uint32_t program = 0;
            struct {
                int tex_linear_depth = -1;
                int tex_main = -1;
                int tex_prev = -1;
                int tex_vel = -1;
                int texel_size = -1;
                int time = -1;
                int feedback_min = -1;
                int feedback_max = -1;
                int motion_scale = -1;
                int jitter_uv = -1;
            } uniform_loc;
        } no_motion_blur;
    } temporal;

    struct {
        uint32_t program = 0;
        struct {
            int tex = -1;
            int inverseScreenSize = -1;
        } uniform_loc;
    } fxaa;
} gl;

static inline float halton(int index, int base) {
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
static inline bool is_ortho_proj_matrix(const mat4_t& P) { return P.elem[2][3] == 0.0f; }

static inline vec2_t extract_jitter_uv(const mat4_t& P) {
    vec2_t jitter;
    if (is_ortho_proj_matrix(P)) {
        jitter[0] = -P.elem[3][0] * 0.5f;
        jitter[1] = -P.elem[3][1] * 0.5f;
    }
    else {
        jitter[0] = P.elem[2][0] * 0.5f;
        jitter[1] = P.elem[2][1] * 0.5f;
    }
    return jitter;
}

static inline vec2_t extract_near_far(const mat4_t& P) {
    vec2_t near_far;
    near_far[0] = P.elem[3][2] / (P[2][2] - 1.0f);
    near_far[1] = P.elem[3][2] / (P[2][2] + 1.0f);
    return near_far;
}

static bool get_shader_compile_error(char* buffer, int max_length, GLuint shader) {
    GLint success = 0;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (success == 0) {
        int length;
        glGetShaderInfoLog(shader, max_length, &length, buffer);
        return true;
    } else {
        return false;
    }
}

static bool get_program_link_error(char* buffer, int max_length, GLuint program) {
    GLint success = 0;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (success == 0) {
        int length;
        glGetProgramInfoLog(program, max_length, &length, buffer);
        return true;
    } else {
        return false;
    }
}

static uint32_t compile_shader_from_source(str_t src, GLenum type, str_t defines = {}) {
    ASSERT(type == GL_VERTEX_SHADER || type == GL_GEOMETRY_SHADER || type == GL_FRAGMENT_SHADER || type == GL_COMPUTE_SHADER ||
        type == GL_TESS_CONTROL_SHADER || type == GL_TESS_EVALUATION_SHADER);

    uint32_t shader = glCreateShader(type);
    md_strb_t builder = {};
    md_strb_init(&builder, default_temp_allocator);

    // Skip to first # which should contain version
    while(src.len > 0 && src.ptr[0] != '#') {
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
    if (get_shader_compile_error(buffer, sizeof(buffer), shader)) {
        MD_LOG_ERROR("%s\n", buffer);
        return 0;
    }

    return shader;
}

static uint32_t setup_program_from_source(str_t name, str_t f_shader_src, str_t defines = {}) {
    uint32_t f_shader = compile_shader_from_source(f_shader_src, GL_FRAGMENT_SHADER, defines);
    uint32_t program = 0;

    if (f_shader) {
        char buffer[1024];
        program = glCreateProgram();

        glAttachShader(program, gl.v_shader_fs_quad);
        glAttachShader(program, f_shader);
        glLinkProgram(program);
        if (get_program_link_error(buffer, sizeof(buffer), program)) {
            MD_LOG_ERROR("Error while linking %.*s program:\n%s", (int)name.len, name.ptr, buffer);
            glDeleteProgram(program);
            return 0;
        }

        glDetachShader(program, gl.v_shader_fs_quad);
        glDetachShader(program, f_shader);
        glDeleteShader(f_shader);
    }

    return program;
}


namespace ssao {
#ifndef AO_RANDOM_TEX_SIZE
#define AO_RANDOM_TEX_SIZE 4
#endif

struct HBAOData {
    float radius_to_screen;
    float neg_inv_r2;
    float n_dot_v_bias;
    float n_influence;

    float ao_multiplier;
    float pow_exponent;
    vec2_t inv_full_res;

    vec4_t proj_info;

    vec4_t sample_pattern[32];
};

void setup_ubo_hbao_data(uint32_t ubo, int width, int height, const mat4_t& proj_mat, float intensity, float radius, float n_dot_v_bias, float normal_bias) {
    ASSERT(ubo);

    // From intel ASSAO
    static constexpr float SAMPLE_PATTERN[] = {
        0.78488064,  0.56661671,  1.500000, -0.126083, 0.26022232,  -0.29575172, 1.500000, -1.064030, 0.10459357,  0.08372527,  1.110000, -2.730563, -0.68286800, 0.04963045,  1.090000, -0.498827,
        -0.13570161, -0.64190155, 1.250000, -0.532765, -0.26193795, -0.08205118, 0.670000, -1.783245, -0.61177456, 0.66664219,  0.710000, -0.044234, 0.43675563,  0.25119025,  0.610000, -1.167283,
        0.07884444,  0.86618668,  0.640000, -0.459002, -0.12790935, -0.29869005, 0.600000, -1.729424, -0.04031125, 0.02413622,  0.600000, -4.792042, 0.16201244,  -0.52851415, 0.790000, -1.067055,
        -0.70991218, 0.47301072,  0.640000, -0.335236, 0.03277707,  -0.22349690, 0.600000, -1.982384, 0.68921727,  0.36800742,  0.630000, -0.266718, 0.29251814,  0.37775412,  0.610000, -1.422520,
        -0.12224089, 0.96582592,  0.600000, -0.426142, 0.11071457,  -0.16131058, 0.600000, -2.165947, 0.46562141,  -0.59747696, 0.600000, -0.189760, -0.51548797, 0.11804193,  0.600000, -1.246800,
        0.89141309,  -0.42090443, 0.600000, 0.028192,  -0.32402530, -0.01591529, 0.600000, -1.543018, 0.60771245,  0.41635221,  0.600000, -0.605411, 0.02379565,  -0.08239821, 0.600000, -3.809046,
        0.48951152,  -0.23657045, 0.600000, -1.189011, -0.17611565, -0.81696892, 0.600000, -0.513724, -0.33930185, -0.20732205, 0.600000, -1.698047, -0.91974425, 0.05403209,  0.600000, 0.062246,
        -0.15064627, -0.14949332, 0.600000, -1.896062, 0.53180975,  -0.35210401, 0.600000, -0.758838, 0.41487166,  0.81442589,  0.600000, -0.505648, -0.24106961, -0.32721516, 0.600000, -1.665244};
    constexpr float METERS_TO_VIEWSPACE = 1.f;

    vec4_t proj_info;
    float proj_scl;

    const float* proj_data = &proj_mat.elem[0][0];
    const bool ortho = is_ortho_proj_matrix(proj_mat);
    if (!ortho) {
        proj_info = {
            2.0f / (proj_data[4 * 0 + 0]),                          // (x) * (R - L)/N
            2.0f / (proj_data[4 * 1 + 1]),                          // (y) * (T - B)/N
            -(1.0f - proj_data[4 * 2 + 0]) / proj_data[4 * 0 + 0],  // L/N
            -(1.0f + proj_data[4 * 2 + 1]) / proj_data[4 * 1 + 1]   // B/N
        };
        proj_scl = float(height) * proj_data[4 * 1 + 1] * 0.5f;
    } else {
        proj_info = {
            2.0f / (proj_data[4 * 0 + 0]),                          // ((x) * R - L)
            2.0f / (proj_data[4 * 1 + 1]),                          // ((y) * T - B)
            -(1.0f + proj_data[4 * 3 + 0]) / proj_data[4 * 0 + 0],  // L
            -(1.0f - proj_data[4 * 3 + 1]) / proj_data[4 * 1 + 1]   // B
        };
        proj_scl = float(height) / proj_info.y;
    }

    float r = radius * METERS_TO_VIEWSPACE;

    HBAOData data;
    data.radius_to_screen = r * 0.5f * proj_scl;
    data.neg_inv_r2 = -1.f / (r * r);
    data.n_dot_v_bias = CLAMP(n_dot_v_bias, 0.f, 1.f - FLT_EPSILON);
    data.n_influence = CLAMP(normal_bias, 0.0f, 1.0f);
    data.ao_multiplier = 1.f / (1.f - data.n_dot_v_bias);
    data.pow_exponent = MAX(intensity, 0.f);
    data.inv_full_res = {1.f / float(width), 1.f / float(height)};
    data.proj_info = proj_info;
    memcpy(&data.sample_pattern, SAMPLE_PATTERN, sizeof(SAMPLE_PATTERN));

    glBindBuffer(GL_UNIFORM_BUFFER, ubo);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), &data, GL_DYNAMIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

void initialize_rnd_tex(uint32_t rnd_tex) {
    constexpr int buffer_size = AO_RANDOM_TEX_SIZE * AO_RANDOM_TEX_SIZE;
    signed short buffer[buffer_size * 4];

    for (int i = 0; i < buffer_size; i++) {
#define SCALE ((1 << 15))
        float rand1 = halton(i + 1, 2);
        float rand2 = halton(i + 1, 3);
        float angle = 2.f * 3.1415926535f * rand1;

        buffer[i * 4 + 0] = (signed short)(SCALE * cosf(angle));
        buffer[i * 4 + 1] = (signed short)(SCALE * sinf(angle));
        buffer[i * 4 + 2] = (signed short)(SCALE * rand2);
        buffer[i * 4 + 3] = (signed short)(SCALE * 0);
#undef SCALE
    }

    glBindTexture(GL_TEXTURE_2D, rnd_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16_SNORM, AO_RANDOM_TEX_SIZE, AO_RANDOM_TEX_SIZE, 0, GL_RGBA, GL_SHORT, buffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
}

float compute_sharpness(float radius) { return 30.f / sqrtf(radius); }

void initialize(int width, int height) {
    gl.ssao.hbao.program_persp = setup_program_from_source(STR("ssao persp"), f_shader_src_ssao, STR("#define AO_PERSPECTIVE 1"));
    gl.ssao.hbao.program_ortho = setup_program_from_source(STR("ssao ortho"), f_shader_src_ssao, STR("#define AO_PERSPECTIVE 0"));
    gl.ssao.blur.program       = setup_program_from_source(STR("ssao blur"),  f_shader_src_ssao_blur);
    
    if (!gl.ssao.hbao.fbo) glGenFramebuffers(1, &gl.ssao.hbao.fbo);
    if (!gl.ssao.blur.fbo) glGenFramebuffers(1, &gl.ssao.blur.fbo);

    if (!gl.ssao.tex_random) glGenTextures(1, &gl.ssao.tex_random);
    if (!gl.ssao.hbao.texture) glGenTextures(1, &gl.ssao.hbao.texture);
    if (!gl.ssao.blur.texture) glGenTextures(1, &gl.ssao.blur.texture);

    if (!gl.ssao.ubo_hbao_data) glGenBuffers(1, &gl.ssao.ubo_hbao_data);

    initialize_rnd_tex(gl.ssao.tex_random);

    glBindTexture(GL_TEXTURE_2D, gl.ssao.hbao.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, width, height, 0, GL_RED, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(GL_TEXTURE_2D, gl.ssao.blur.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, width, height, 0, GL_RED, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glBindTexture(GL_TEXTURE_2D, 0);

    glBindFramebuffer(GL_FRAMEBUFFER, gl.ssao.hbao.fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.ssao.hbao.texture, 0);

    glBindFramebuffer(GL_FRAMEBUFFER, gl.ssao.blur.fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.ssao.blur.texture, 0);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    glBindBuffer(GL_UNIFORM_BUFFER, gl.ssao.ubo_hbao_data);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), nullptr, GL_DYNAMIC_DRAW);
}

void shutdown() {
    if (gl.ssao.hbao.fbo) glDeleteFramebuffers(1, &gl.ssao.hbao.fbo);
    if (gl.ssao.blur.fbo) glDeleteFramebuffers(1, &gl.ssao.blur.fbo);

    if (gl.ssao.tex_random) glDeleteTextures(1, &gl.ssao.tex_random);
    if (gl.ssao.hbao.texture) glDeleteTextures(1, &gl.ssao.hbao.texture);
    if (gl.ssao.blur.texture) glDeleteTextures(1, &gl.ssao.blur.texture);

    if (gl.ssao.ubo_hbao_data) glDeleteBuffers(1, &gl.ssao.ubo_hbao_data);

    if (gl.ssao.hbao.program_persp) glDeleteProgram(gl.ssao.hbao.program_persp);
    if (gl.ssao.hbao.program_ortho) glDeleteProgram(gl.ssao.hbao.program_ortho);
    if (gl.ssao.blur.program) glDeleteProgram(gl.ssao.blur.program);
}

}  // namespace ssao

namespace shading {

static struct {
    uint32_t program = 0;
    struct {
        int texture_depth = -1;
        int texture_color = -1;
        int texture_normal = -1;
        int inv_proj_mat = -1;
        int light_dir = -1;
        int light_col = -1;
        int time = -1;
    } uniform_loc;
} shading;

void initialize() {
    shading.program = setup_program_from_source(STR("deferred shading"), f_shader_src_deferred_shading);
    shading.uniform_loc.texture_depth = glGetUniformLocation(shading.program, "u_texture_depth");
    shading.uniform_loc.texture_color = glGetUniformLocation(shading.program, "u_texture_color");
    shading.uniform_loc.texture_normal = glGetUniformLocation(shading.program, "u_texture_normal");
    shading.uniform_loc.inv_proj_mat = glGetUniformLocation(shading.program, "u_inv_proj_mat");
    shading.uniform_loc.light_dir   = glGetUniformLocation(shading.program, "u_light_dir");
    shading.uniform_loc.light_col   = glGetUniformLocation(shading.program, "u_light_col");
    shading.uniform_loc.time = glGetUniformLocation(shading.program, "u_time");
}

void shutdown() {
    if (shading.program) glDeleteProgram(shading.program);
}
}  // namespace shading

namespace tonemapping {

static struct {
    uint32_t program = 0;
    struct {
        int texture = -1;
    } uniform_loc;
} passthrough;

static struct {
    uint32_t program = 0;
    struct {
        int texture = -1;
        int exposure = -1;
        int gamma = -1;
    } uniform_loc;
} exposure_gamma;

static struct {
    uint32_t program = 0;
    struct {
        int texture = -1;
        int exposure = -1;
        int gamma = -1;
    } uniform_loc;
} filmic;

static struct {
    uint32_t program = 0;
    struct {
        int texture = -1;
        int exposure = -1;
        int gamma = -1;
    } uniform_loc;
} ACES;

static struct {
    uint32_t program_forward = 0;
    uint32_t program_inverse = 0;
    struct {
        int texture = -1;
    } uniform_loc;
} fast_reversible;

void initialize() {
    {
        // PASSTHROUGH
        passthrough.program = setup_program_from_source(STR("Passthrough"), f_shader_src_tonemap_passthrough);
        passthrough.uniform_loc.texture = glGetUniformLocation(passthrough.program, "u_texture");
    }
    {
        // EXPOSURE GAMMA
        exposure_gamma.program = setup_program_from_source(STR("Exposure Gamma"), f_shader_src_tonemap_exposure_gamma);
        exposure_gamma.uniform_loc.texture = glGetUniformLocation(exposure_gamma.program, "u_texture");
        exposure_gamma.uniform_loc.exposure = glGetUniformLocation(exposure_gamma.program, "u_exposure");
        exposure_gamma.uniform_loc.gamma = glGetUniformLocation(exposure_gamma.program, "u_gamma");
    }
    {
        // FILMIC (UNCHARTED)
        filmic.program = setup_program_from_source(STR("Filmic"), f_shader_src_tonemap_filmic);
        filmic.uniform_loc.texture = glGetUniformLocation(filmic.program, "u_texture");
        filmic.uniform_loc.exposure = glGetUniformLocation(filmic.program, "u_exposure");
        filmic.uniform_loc.gamma = glGetUniformLocation(filmic.program, "u_gamma");
    }
    {
        // ACES
        ACES.program = setup_program_from_source(STR("ACES"), f_shader_src_tonemap_aces);
        ACES.uniform_loc.texture = glGetUniformLocation(ACES.program, "u_texture");
        ACES.uniform_loc.exposure = glGetUniformLocation(ACES.program, "u_exposure");
        ACES.uniform_loc.gamma = glGetUniformLocation(ACES.program, "u_gamma");
    }

}

void shutdown() {
    if (passthrough.program) glDeleteProgram(passthrough.program);
    if (exposure_gamma.program) glDeleteProgram(exposure_gamma.program);
    if (filmic.program) glDeleteProgram(filmic.program);
    if (ACES.program) glDeleteProgram(ACES.program);
}

}  // namespace tonemapping

namespace dof {
void initialize(int32_t width, int32_t height) {
    {
        gl.bokeh_dof.half_res.program = setup_program_from_source(STR("DOF prepass"), f_shader_src_dof_halfres_prepass);
        if (gl.bokeh_dof.half_res.program) {
            gl.bokeh_dof.half_res.uniform_loc.tex_depth = glGetUniformLocation(gl.bokeh_dof.half_res.program, "u_tex_depth");
            gl.bokeh_dof.half_res.uniform_loc.tex_color = glGetUniformLocation(gl.bokeh_dof.half_res.program, "u_tex_color");
            gl.bokeh_dof.half_res.uniform_loc.focus_point = glGetUniformLocation(gl.bokeh_dof.half_res.program, "u_focus_point");
            gl.bokeh_dof.half_res.uniform_loc.focus_scale = glGetUniformLocation(gl.bokeh_dof.half_res.program, "u_focus_scale");
        }
    }

    if (!gl.bokeh_dof.half_res.tex.color_coc) {
        glGenTextures(1, &gl.bokeh_dof.half_res.tex.color_coc);
    }
    glBindTexture(GL_TEXTURE_2D, gl.bokeh_dof.half_res.tex.color_coc);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, width / 2, height / 2, 0, GL_RGBA, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (!gl.bokeh_dof.half_res.fbo) {
        glGenFramebuffers(1, &gl.bokeh_dof.half_res.fbo);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.bokeh_dof.half_res.fbo);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.bokeh_dof.half_res.tex.color_coc, 0);
        GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            MD_LOG_ERROR("Something went wrong when generating framebuffer for DOF");
        }
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }

    // DOF
    {
        gl.bokeh_dof.program = setup_program_from_source(STR("Bokeh DOF"), f_shader_src_dof);
        if (gl.bokeh_dof.program) {
            gl.bokeh_dof.uniform_loc.tex_color = glGetUniformLocation(gl.bokeh_dof.program, "u_half_res");
            gl.bokeh_dof.uniform_loc.tex_color = glGetUniformLocation(gl.bokeh_dof.program, "u_tex_color");
            gl.bokeh_dof.uniform_loc.tex_depth = glGetUniformLocation(gl.bokeh_dof.program, "u_tex_depth");
            gl.bokeh_dof.uniform_loc.pixel_size = glGetUniformLocation(gl.bokeh_dof.program, "u_texel_size");
            gl.bokeh_dof.uniform_loc.focus_point = glGetUniformLocation(gl.bokeh_dof.program, "u_focus_depth");
            gl.bokeh_dof.uniform_loc.focus_scale = glGetUniformLocation(gl.bokeh_dof.program, "u_focus_scale");
            gl.bokeh_dof.uniform_loc.time = glGetUniformLocation(gl.bokeh_dof.program, "u_time");
        }
    }
}

void shutdown() {}
}  // namespace dof

namespace blit {
static uint32_t program_tex_depth = 0;
static uint32_t program_tex = 0;
static uint32_t program_col = 0;
static int uniform_loc_tex_color = -1;
static int uniform_loc_tex_depth = -1;
static int uniform_loc_texture = -1;
static int uniform_loc_color = -1;

constexpr str_t f_shader_src_tex_depth = STR(R"(
#version 150 core

uniform sampler2D u_tex_color;
uniform sampler2D u_tex_depth;

out vec4 out_frag;

void main() {
    float depth = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
    if (depth == 1.0) {
        out_frag = vec4(0,0,0,0);
    } else {
        out_frag = texelFetch(u_tex_color, ivec2(gl_FragCoord.xy), 0);
    }
}
)");

constexpr str_t f_shader_src_tex = STR(R"(
#version 150 core

uniform sampler2D u_texture;

out vec4 out_frag;

void main() {
    out_frag = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
}
)");

constexpr str_t f_shader_src_col = STR(R"(
#version 150 core

uniform vec4 u_color;
out vec4 out_frag;

void main() {
	out_frag = u_color;
}
)");

void initialize() {
    program_tex = setup_program_from_source(STR("blit texture"), f_shader_src_tex);
    uniform_loc_texture = glGetUniformLocation(program_tex, "u_texture");

    program_tex_depth = setup_program_from_source(STR("blit texture with depth"), f_shader_src_tex_depth);
    uniform_loc_tex_color = glGetUniformLocation(program_tex_depth, "u_tex_color");
    uniform_loc_tex_depth = glGetUniformLocation(program_tex_depth, "u_tex_depth");

    program_col = setup_program_from_source(STR("blit color"), f_shader_src_col);
    uniform_loc_color = glGetUniformLocation(program_col, "u_color");
}

void shutdown() {
    if (program_tex) glDeleteProgram(program_tex);
    if (program_col) glDeleteProgram(program_col);
}
}  // namespace blit

namespace velocity {
#define VEL_TILE_SIZE 8

struct {
    uint32_t program = 0;
    struct {
		int tex_depth = -1;
        int curr_clip_to_prev_clip_mat = -1;
        int jitter_uv = -1;
    } uniform_loc;
} blit_velocity;

struct {
    uint32_t program = 0;
    struct {
        int tex_vel = -1;
        int tex_vel_texel_size = -1;
    } uniform_loc;
} blit_tilemax;

struct {
    uint32_t program = 0;
    struct {
        int tex_vel = -1;
        int tex_vel_texel_size = -1;
    } uniform_loc;
} blit_neighbormax;

void initialize(int32_t width, int32_t height) {
    {
        blit_velocity.program = setup_program_from_source(STR("screen-space velocity"), f_shader_src_vel_blit);
		blit_velocity.uniform_loc.tex_depth = glGetUniformLocation(blit_velocity.program, "u_tex_depth");
        blit_velocity.uniform_loc.curr_clip_to_prev_clip_mat = glGetUniformLocation(blit_velocity.program, "u_curr_clip_to_prev_clip_mat");
        blit_velocity.uniform_loc.jitter_uv = glGetUniformLocation(blit_velocity.program, "u_jitter_uv");

    }
    {
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
        str_t defines = STR("#define TILE_SIZE " TOSTRING(VEL_TILE_SIZE));
        blit_tilemax.program = setup_program_from_source(STR("tilemax"), f_shader_src_vel_tilemax, defines);
        blit_tilemax.uniform_loc.tex_vel = glGetUniformLocation(blit_tilemax.program, "u_tex_vel");
        blit_tilemax.uniform_loc.tex_vel_texel_size = glGetUniformLocation(blit_tilemax.program, "u_tex_vel_texel_size");
#undef STRINGIFY
#undef TOSTRING
    }
    {
        blit_neighbormax.program = setup_program_from_source(STR("neighbormax"), f_shader_src_vel_neighbormax);
        blit_neighbormax.uniform_loc.tex_vel = glGetUniformLocation(blit_neighbormax.program, "u_tex_vel");
        blit_neighbormax.uniform_loc.tex_vel_texel_size = glGetUniformLocation(blit_neighbormax.program, "u_tex_vel_texel_size");
    }

    if (!gl.velocity.tex_tilemax) {
        glGenTextures(1, &gl.velocity.tex_tilemax);
    }

    if (!gl.velocity.tex_neighbormax) {
        glGenTextures(1, &gl.velocity.tex_neighbormax);
    }

    gl.velocity.tex_width = width / VEL_TILE_SIZE;
    gl.velocity.tex_height = height / VEL_TILE_SIZE;

    glBindTexture(GL_TEXTURE_2D, gl.velocity.tex_tilemax);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16F, gl.velocity.tex_width, gl.velocity.tex_height, 0, GL_RG, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    glBindTexture(GL_TEXTURE_2D, gl.velocity.tex_neighbormax);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16F, gl.velocity.tex_width, gl.velocity.tex_height, 0, GL_RG, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (!gl.velocity.fbo) {
        glGenFramebuffers(1, &gl.velocity.fbo);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.velocity.fbo);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.velocity.tex_tilemax, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, gl.velocity.tex_neighbormax, 0);
        GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            MD_LOG_ERROR("Something went wrong in creating framebuffer for velocity");
        }
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }
}

void shutdown() {
    if (blit_velocity.program) glDeleteProgram(blit_velocity.program);
    if (gl.velocity.tex_tilemax) glDeleteTextures(1, &gl.velocity.tex_tilemax);
    if (gl.velocity.tex_neighbormax) glDeleteTextures(1, &gl.velocity.tex_neighbormax);
    if (gl.velocity.fbo) glDeleteFramebuffers(1, &gl.velocity.fbo);
}
}  // namespace velocity

namespace temporal {
void initialize() {
    {
        gl.temporal.with_motion_blur.program = setup_program_from_source(STR("temporal aa + motion-blur"), f_shader_src_temporal);
        gl.temporal.no_motion_blur.program   = setup_program_from_source(STR("temporal aa"), f_shader_src_temporal, STR("#define USE_MOTION_BLUR 0\n"));

        gl.temporal.with_motion_blur.uniform_loc.tex_linear_depth = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_tex_linear_depth");
        gl.temporal.with_motion_blur.uniform_loc.tex_main = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_tex_main");
        gl.temporal.with_motion_blur.uniform_loc.tex_prev = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_tex_prev");
        gl.temporal.with_motion_blur.uniform_loc.tex_vel = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_tex_vel");
        gl.temporal.with_motion_blur.uniform_loc.tex_vel_neighbormax = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_tex_vel_neighbormax");
        gl.temporal.with_motion_blur.uniform_loc.texel_size = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_texel_size");
        gl.temporal.with_motion_blur.uniform_loc.jitter_uv = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_jitter_uv");
        gl.temporal.with_motion_blur.uniform_loc.time = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_time");
        gl.temporal.with_motion_blur.uniform_loc.feedback_min = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_feedback_min");
        gl.temporal.with_motion_blur.uniform_loc.feedback_max = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_feedback_max");
        gl.temporal.with_motion_blur.uniform_loc.motion_scale = glGetUniformLocation(gl.temporal.with_motion_blur.program, "u_motion_scale");

        gl.temporal.no_motion_blur.uniform_loc.tex_linear_depth = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_tex_linear_depth");
        gl.temporal.no_motion_blur.uniform_loc.tex_main = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_tex_main");
        gl.temporal.no_motion_blur.uniform_loc.tex_prev = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_tex_prev");
        gl.temporal.no_motion_blur.uniform_loc.tex_vel = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_tex_vel");
        gl.temporal.no_motion_blur.uniform_loc.texel_size = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_texel_size");
        gl.temporal.no_motion_blur.uniform_loc.jitter_uv = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_jitter_uv");
        gl.temporal.no_motion_blur.uniform_loc.time = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_time");
        gl.temporal.no_motion_blur.uniform_loc.feedback_min = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_feedback_min");
        gl.temporal.no_motion_blur.uniform_loc.feedback_max = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_feedback_max");
        gl.temporal.no_motion_blur.uniform_loc.motion_scale = glGetUniformLocation(gl.temporal.no_motion_blur.program, "u_motion_scale");
    }
}

void shutdown() {}
};  // namespace temporal

namespace sharpen {
static uint32_t program = 0;
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
    program = setup_program_from_source(STR("sharpen"), f_shader_src_sharpen);
}

void sharpen(uint32_t in_texture) {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, in_texture);

    glUseProgram(program);
    glUniform1i(glGetUniformLocation(sharpen::program, "u_tex"), 0);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glUseProgram(0);
}

void shutdown() {
    if (program) glDeleteProgram(program);
}
}   // namespace sharpen

namespace fxaa {
    void initialize() {
        gl.fxaa.program = setup_program_from_source(STR("fxaa"), f_shader_src_fxaa);

        if (gl.fxaa.program) {
            gl.fxaa.uniform_loc.tex = glGetUniformLocation(gl.fxaa.program, "tex");
            gl.fxaa.uniform_loc.inverseScreenSize = glGetUniformLocation(gl.fxaa.program, "inverseScreenSize");
        }
    }

    void apply_fxaa(uint32_t in_texture, int width, int height) {
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, in_texture);

        glUseProgram(gl.fxaa.program);
        glUniform1i(gl.fxaa.uniform_loc.tex, 0);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, in_texture);

        vec2_t inv_screen_size = { 1.0f / (float)width, 1.0f / (float)height };
        glUniform2fv(gl.fxaa.uniform_loc.inverseScreenSize, 1, inv_screen_size.elem);

        glBindVertexArray(gl.vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);

        glUseProgram(0);
    }

    void shutdown() {
        if (gl.fxaa.program) glDeleteProgram(gl.fxaa.program);
    }
}   // namespace fxaa

void initialize(int width, int height) {
    if (!gl.vao) glGenVertexArrays(1, &gl.vao);

    gl.v_shader_fs_quad = compile_shader_from_source(v_shader_src_fs_quad, GL_VERTEX_SHADER);

    // LINEARIZE DEPTH
    gl.linear_depth.program_persp = setup_program_from_source(STR("linearize depth persp"), f_shader_src_linearize_depth, STR("#define PERSPECTIVE 1"));
    gl.linear_depth.program_ortho = setup_program_from_source(STR("linearize depth ortho"), f_shader_src_linearize_depth, STR("#define PERSPECTIVE 0"));

    if (!gl.linear_depth.texture) glGenTextures(1, &gl.linear_depth.texture);
    glBindTexture(GL_TEXTURE_2D, gl.linear_depth.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (!gl.linear_depth.fbo) {
        glGenFramebuffers(1, &gl.linear_depth.fbo);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.linear_depth.fbo);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.linear_depth.texture, 0);
        GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            MD_LOG_ERROR("Something went wrong in creating framebuffer for depth linearization");
        }
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }

    gl.linear_depth.uniform_loc.clip_info = glGetUniformLocation(gl.linear_depth.program_persp, "u_clip_info");
    gl.linear_depth.uniform_loc.tex_depth = glGetUniformLocation(gl.linear_depth.program_persp, "u_tex_depth");

    // COLOR
    if (!gl.targets.tex_color[0]) glGenTextures(2, gl.targets.tex_color);
    glBindTexture(GL_TEXTURE_2D, gl.targets.tex_color[0]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R11F_G11F_B10F, width, height, 0, GL_RGB, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, gl.targets.tex_color[1]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R11F_G11F_B10F, width, height, 0, GL_RGB, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (!gl.targets.tex_temporal_buffer[0]) glGenTextures(2, gl.targets.tex_temporal_buffer);
    glBindTexture(GL_TEXTURE_2D, gl.targets.tex_temporal_buffer[0]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R11F_G11F_B10F, width, height, 0, GL_RGB, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, gl.targets.tex_temporal_buffer[1]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R11F_G11F_B10F, width, height, 0, GL_RGB, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    if (!gl.targets.fbo) {
        glGenFramebuffers(1, &gl.targets.fbo);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.targets.fbo);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.targets.tex_color[0], 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, gl.targets.tex_color[1], 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D, gl.targets.tex_temporal_buffer[0], 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT3, GL_TEXTURE_2D, gl.targets.tex_temporal_buffer[1], 0);

        GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            MD_LOG_ERROR("Something went wrong in creating framebuffer for targets");
        }

        GLenum buffers[] = {GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3};
        glDrawBuffers(4, buffers);
        glClear(GL_COLOR_BUFFER_BIT);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }

    if (!gl.tmp.fbo) {
        glGenFramebuffers(1, &gl.tmp.fbo);
    }

    if (!gl.tmp.tex_rgba8) {
        glGenTextures(1, &gl.tmp.tex_rgba8);
        glBindTexture(GL_TEXTURE_2D, gl.tmp.tex_rgba8);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    gl.tex_width = width;
    gl.tex_height = height;

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

    if (gl.vao) glDeleteVertexArrays(1, &gl.vao);
    if (gl.v_shader_fs_quad) glDeleteShader(gl.v_shader_fs_quad);
    if (gl.tmp.fbo) glDeleteFramebuffers(1, &gl.tmp.fbo);
    if (gl.tmp.tex_rgba8) glDeleteTextures(1, &gl.tmp.tex_rgba8);
}

void compute_linear_depth(uint32_t depth_tex, float near_plane, float far_plane, bool orthographic = false) {
    const vec4_t clip_info {near_plane * far_plane, near_plane - far_plane, far_plane, 0};

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth_tex);

    if (orthographic)
        glUseProgram(gl.linear_depth.program_ortho);
    else
        glUseProgram(gl.linear_depth.program_persp);
    glUniform1i(gl.linear_depth.uniform_loc.tex_depth, 0);
    glUniform4fv(gl.linear_depth.uniform_loc.clip_info, 1, &clip_info.x);

    // ASSUME THAT THE APPROPRIATE FS_QUAD VAO IS BOUND
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
}

void apply_ssao(uint32_t linear_depth_tex, uint32_t normal_tex, const mat4_t& proj_matrix, float intensity, float radius, float bias, float normal_bias) {
    ASSERT(glIsTexture(linear_depth_tex));
    ASSERT(glIsTexture(normal_tex));

    const bool ortho = is_ortho_proj_matrix(proj_matrix);
    const float sharpness = ssao::compute_sharpness(radius);

    int last_fbo;
    int last_viewport[4];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);

    int width = last_viewport[2];
    int height = last_viewport[3];

    const vec2_t inv_res = vec2_t{1.f / gl.tex_width, 1.f / gl.tex_height};

    glBindVertexArray(gl.vao);

    ssao::setup_ubo_hbao_data(gl.ssao.ubo_hbao_data, width, height, proj_matrix, intensity, radius, bias, normal_bias);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.ssao.hbao.fbo);
    glViewport(0, 0, gl.tex_width, gl.tex_height);
    glClearColor(1,1,1,1);
    glClear(GL_COLOR_BUFFER_BIT);

    glViewport(0, 0, width, height);

    // RENDER HBAO
    uint32_t program = ortho ? gl.ssao.hbao.program_ortho : gl.ssao.hbao.program_persp;

    PUSH_GPU_SECTION("HBAO")
    glUseProgram(program);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linear_depth_tex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normal_tex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, gl.ssao.tex_random);

    glBindBufferBase(GL_UNIFORM_BUFFER, 0, gl.ssao.ubo_hbao_data);
    glUniformBlockBinding(program, glGetUniformBlockIndex(program, "u_control_buffer"), 0);
    glUniform1i(glGetUniformLocation(program, "u_tex_linear_depth"), 0);
    glUniform1i(glGetUniformLocation(program, "u_tex_normal"), 1);
    glUniform1i(glGetUniformLocation(program, "u_tex_random"), 2);

    glUniform2f(glGetUniformLocation(program, "u_tc_scl"), (float)width/(float)gl.tex_width, (float)height/(float)gl.tex_height);

    glDrawArrays(GL_TRIANGLES, 0, 3);

    POP_GPU_SECTION()

    glUseProgram(gl.ssao.blur.program);

    glUniform1i(glGetUniformLocation(gl.ssao.blur.program, "u_tex_linear_depth"), 0);
    glUniform1i(glGetUniformLocation(gl.ssao.blur.program, "u_tex_ao"), 1);
    glUniform1f(glGetUniformLocation(gl.ssao.blur.program, "u_sharpness"), sharpness);
    glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_inv_res_dir"), inv_res.x, 0);

    glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_tc_scl"), (float)width/(float)gl.tex_width, (float)height/(float)gl.tex_height);

    glActiveTexture(GL_TEXTURE1);

    // BLUR FIRST
    PUSH_GPU_SECTION("BLUR 1st")
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.ssao.blur.fbo);
    glViewport(0, 0, width, height);
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindTexture(GL_TEXTURE_2D, gl.ssao.hbao.texture);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    POP_GPU_SECTION()

    // BLUR SECOND
    PUSH_GPU_SECTION("BLUR 2nd")
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, gl.ssao.blur.texture);
    glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_inv_res_dir"), 0, inv_res.y);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ZERO, GL_SRC_COLOR);
    glColorMask(1, 1, 1, 0);

    glDrawArrays(GL_TRIANGLES, 0, 3);

    glDisable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);
    glColorMask(1, 1, 1, 1);

    glBindVertexArray(0);
    POP_GPU_SECTION()
}

void shade_deferred(uint32_t depth_tex, uint32_t color_tex, uint32_t normal_tex, const mat4_t& inv_proj_matrix, const vec3_t& light_dir, const vec3_t& light_col, float time) {
    ASSERT(glIsTexture(depth_tex));
    ASSERT(glIsTexture(color_tex));
    ASSERT(glIsTexture(normal_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth_tex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, color_tex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, normal_tex);

    glUseProgram(shading::shading.program);
    glUniform1i(shading::shading.uniform_loc.texture_depth, 0);
    glUniform1i(shading::shading.uniform_loc.texture_color, 1);
    glUniform1i(shading::shading.uniform_loc.texture_normal, 2);
    glUniformMatrix4fv(shading::shading.uniform_loc.inv_proj_mat, 1, GL_FALSE, &inv_proj_matrix.elem[0][0]);
    glUniform3f(shading::shading.uniform_loc.light_dir, light_dir.x, light_dir.y, light_dir.z);
    glUniform3f(shading::shading.uniform_loc.light_col, light_col.x, light_col.y, light_col.z);
    glUniform1f(shading::shading.uniform_loc.time, time);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void half_res_color_coc(uint32_t linear_depth_tex, uint32_t color_tex, float focus_point, float focus_scale) {
    PUSH_GPU_SECTION("DOF Prepass");
    int last_viewport[4];
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    glViewport(0, 0, gl.tex_width / 2, gl.tex_height / 2);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.bokeh_dof.half_res.fbo);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linear_depth_tex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glUseProgram(gl.bokeh_dof.half_res.program);

    glUniform1i(gl.bokeh_dof.half_res.uniform_loc.tex_depth, 0);
    glUniform1i(gl.bokeh_dof.half_res.uniform_loc.tex_color, 1);
    glUniform1f(gl.bokeh_dof.half_res.uniform_loc.focus_point, focus_point);
    glUniform1f(gl.bokeh_dof.half_res.uniform_loc.focus_scale, focus_scale);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    POP_GPU_SECTION();
}

void apply_dof(uint32_t linear_depth_tex, uint32_t color_tex, float focus_point, float focus_scale, float time) {
    ASSERT(glIsTexture(linear_depth_tex));
    ASSERT(glIsTexture(color_tex));

    const vec2_t pixel_size = vec2_t{1.f / gl.tex_width, 1.f / gl.tex_height};

    int last_fbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);

    half_res_color_coc(linear_depth_tex, color_tex, focus_point, focus_scale);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, gl.bokeh_dof.half_res.tex.color_coc);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, linear_depth_tex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glUseProgram(gl.bokeh_dof.program);
    glUniform1i(gl.bokeh_dof.uniform_loc.tex_half_res, 0);
    glUniform1i(gl.bokeh_dof.uniform_loc.tex_depth, 1);
    glUniform1i(gl.bokeh_dof.uniform_loc.tex_color, 2);
    glUniform2f(gl.bokeh_dof.uniform_loc.pixel_size, pixel_size.x, pixel_size.y);
    glUniform1f(gl.bokeh_dof.uniform_loc.focus_point, focus_point);
    glUniform1f(gl.bokeh_dof.uniform_loc.focus_scale, focus_scale);
    glUniform1f(gl.bokeh_dof.uniform_loc.time, time);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
    glActiveTexture(GL_TEXTURE0);
}

void apply_tonemapping(uint32_t color_tex, Tonemapping tonemapping, float exposure, float gamma) {
    ASSERT(glIsTexture(color_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    switch (tonemapping) {
    case Tonemapping::ExposureGamma:
        glUseProgram(tonemapping::exposure_gamma.program);
        glUniform1i(tonemapping::exposure_gamma.uniform_loc.texture, 0);
        glUniform1f(tonemapping::exposure_gamma.uniform_loc.exposure, exposure);
        glUniform1f(tonemapping::exposure_gamma.uniform_loc.gamma, gamma);
        break;
    case Tonemapping::Filmic:
        glUseProgram(tonemapping::filmic.program);
        glUniform1i(tonemapping::filmic.uniform_loc.texture, 0);
        glUniform1f(tonemapping::filmic.uniform_loc.exposure, exposure);
        glUniform1f(tonemapping::filmic.uniform_loc.gamma, gamma);
        break;
    case Tonemapping::ACES:
        glUseProgram(tonemapping::ACES.program);
        glUniform1i(tonemapping::ACES.uniform_loc.texture, 0);
        glUniform1f(tonemapping::ACES.uniform_loc.exposure, exposure);
        glUniform1f(tonemapping::ACES.uniform_loc.gamma, gamma);
        break;
    case Tonemapping::Passthrough:
    default:
        glUseProgram(tonemapping::passthrough.program);
        glUniform1i(tonemapping::passthrough.uniform_loc.texture, 0);
        break;
    }

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void apply_aa_tonemapping(uint32_t color_tex) {
    ASSERT(glIsTexture(color_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glUseProgram(tonemapping::fast_reversible.program_forward);
    glUniform1i(tonemapping::fast_reversible.uniform_loc.texture, 0);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void apply_inverse_aa_tonemapping(uint32_t color_tex) {
    ASSERT(glIsTexture(color_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glUseProgram(tonemapping::fast_reversible.program_inverse);
    glUniform1i(tonemapping::fast_reversible.uniform_loc.texture, 0);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blit_tilemax(uint32_t velocity_tex, int tex_width, int tex_height) {
    ASSERT(glIsTexture(velocity_tex));
    const vec2_t texel_size = {1.f / tex_width, 1.f / tex_height};

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, velocity_tex);

    glUseProgram(velocity::blit_tilemax.program);
    glUniform1i(velocity::blit_tilemax.uniform_loc.tex_vel, 0);
    glUniform2fv(velocity::blit_tilemax.uniform_loc.tex_vel_texel_size, 1, &texel_size.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blit_neighbormax(uint32_t velocity_tex, int tex_width, int tex_height) {
    ASSERT(glIsTexture(velocity_tex));
    const vec2_t texel_size = {1.f / tex_width, 1.f / tex_height};

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, velocity_tex);

    glUseProgram(velocity::blit_neighbormax.program);
    glUniform1i(velocity::blit_neighbormax.uniform_loc.tex_vel, 0);
    glUniform2fv(velocity::blit_neighbormax.uniform_loc.tex_vel_texel_size, 1, &texel_size.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void apply_temporal_aa(uint32_t linear_depth_tex, uint32_t color_tex, uint32_t velocity_tex, uint32_t velocity_neighbormax_tex, const vec2_t& curr_jitter, const vec2_t& prev_jitter, float feedback_min,
                       float feedback_max, float motion_scale, float time) {
    ASSERT(glIsTexture(linear_depth_tex));
    ASSERT(glIsTexture(color_tex));
    ASSERT(glIsTexture(velocity_tex));
    ASSERT(glIsTexture(velocity_neighbormax_tex));

    static int target = 0;
    target = (target + 1) % 2;

    const int dst_buf = target;
    const int src_buf = (target + 1) % 2;

    const vec2_t res = {(float)gl.tex_width, (float)gl.tex_height};
    const vec2_t inv_res = 1.0f / res;
    const vec4_t texel_size = vec4_t{inv_res.x, inv_res.y, res.x, res.y};
    const vec2_t jitter_uv_curr = curr_jitter / res;
    const vec2_t jitter_uv_prev = prev_jitter / res;
    const vec4_t jitter_uv = vec4_t{jitter_uv_curr.x, jitter_uv_curr.y, jitter_uv_prev.x, jitter_uv_prev.y};

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, linear_depth_tex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, gl.targets.tex_temporal_buffer[src_buf]);

    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, velocity_tex);

    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, velocity_neighbormax_tex);

    int bound_buffer;
    glGetIntegerv(GL_DRAW_BUFFER, &bound_buffer);

    GLenum draw_buffers[2];
    draw_buffers[0] = GL_COLOR_ATTACHMENT2 + dst_buf;  // tex_temporal_buffer[0 or 1]
    draw_buffers[1] = (GLenum)bound_buffer;                    // assume that this is part of the same gbuffer

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.targets.fbo);
    glViewport(0, 0, gl.tex_width, gl.tex_height);
    glDrawBuffers(2, draw_buffers);

    if (motion_scale != 0.f) {
        glUseProgram(gl.temporal.with_motion_blur.program);

        glUniform1i(gl.temporal.with_motion_blur.uniform_loc.tex_linear_depth, 0);
        glUniform1i(gl.temporal.with_motion_blur.uniform_loc.tex_main, 1);
        glUniform1i(gl.temporal.with_motion_blur.uniform_loc.tex_prev, 2);
        glUniform1i(gl.temporal.with_motion_blur.uniform_loc.tex_vel, 3);
        glUniform1i(gl.temporal.with_motion_blur.uniform_loc.tex_vel_neighbormax, 4);

        glUniform4fv(gl.temporal.with_motion_blur.uniform_loc.texel_size, 1, &texel_size.x);
        glUniform4fv(gl.temporal.with_motion_blur.uniform_loc.jitter_uv, 1, &jitter_uv.x);
        glUniform1f(gl.temporal.with_motion_blur.uniform_loc.time, time);
        glUniform1f(gl.temporal.with_motion_blur.uniform_loc.feedback_min, feedback_min);
        glUniform1f(gl.temporal.with_motion_blur.uniform_loc.feedback_max, feedback_max);
        glUniform1f(gl.temporal.with_motion_blur.uniform_loc.motion_scale, motion_scale);
    } else {
        glUseProgram(gl.temporal.no_motion_blur.program);

        glUniform1i(gl.temporal.no_motion_blur.uniform_loc.tex_linear_depth, 0);
        glUniform1i(gl.temporal.no_motion_blur.uniform_loc.tex_main, 1);
        glUniform1i(gl.temporal.no_motion_blur.uniform_loc.tex_prev, 2);
        glUniform1i(gl.temporal.no_motion_blur.uniform_loc.tex_vel, 3);

        glUniform4fv(gl.temporal.no_motion_blur.uniform_loc.texel_size, 1, &texel_size.x);
        glUniform4fv(gl.temporal.no_motion_blur.uniform_loc.jitter_uv, 1, &jitter_uv.x);
        glUniform1f(gl.temporal.no_motion_blur.uniform_loc.time, time);
        glUniform1f(gl.temporal.no_motion_blur.uniform_loc.feedback_min, feedback_min);
        glUniform1f(gl.temporal.no_motion_blur.uniform_loc.feedback_max, feedback_max);
        glUniform1f(gl.temporal.no_motion_blur.uniform_loc.motion_scale, motion_scale);
    }

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blit_texture(uint32_t tex, uint32_t depth) {
    ASSERT(glIsTexture(tex));
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex);

    if (depth) {
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, depth);
        glUseProgram(blit::program_tex_depth);
        glUniform1i(blit::uniform_loc_tex_color, 0);
        glUniform1i(blit::uniform_loc_tex_depth, 1);
    }
    else {
        glUseProgram(blit::program_tex);
        glUniform1i(blit::uniform_loc_texture, 0);
    }

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
    glActiveTexture(GL_TEXTURE0);
}

void blit_color(vec4_t color) {
    glUseProgram(blit::program_col);
    glUniform4fv(blit::uniform_loc_color, 1, &color.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void postprocess(const Settings& settings, const mat4_t& V, const mat4_t& P) {
    ASSERT(p);
    ASSERT(glIsTexture(settings.input_textures.depth));
    ASSERT(glIsTexture(settings.input_textures.color));
    ASSERT(glIsTexture(settings.input_textures.normal));
    if (settings.temporal_reprojection.enabled) {
        ASSERT(glIsTexture(settings.input_textures.velocity));
    }

    // For seeding noise
    static float time = 0.f;
    time = time + 0.016f;
    if (time > 100.f) time -= 100.f;

    static vec2_t prev_jitter = {0,0};
    vec3_t L = mat4_mul_vec3(V, vec3_set(0, 0, 0), 1.0f);
    vec3_t light_dir = vec3_normalize(L);
    vec3_t light_col = vec3_set1(5.0f);
    mat4_t inv_P = mat4_inverse(P);
    vec2_t near_far = extract_near_far(P);
    vec2_t jitter = extract_jitter_uv(P);
    bool ortho = is_ortho_proj_matrix(P);

    int last_fbo;
    int last_viewport[4];
    int last_draw_buffer;
    int last_blend;
    int last_colormask[4];
    int last_depthmask;

    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    glGetIntegerv(GL_DRAW_BUFFER, &last_draw_buffer);
    glGetIntegerv(GL_BLEND, &last_blend);
    glGetIntegerv(GL_COLOR_WRITEMASK, last_colormask);
    glGetIntegerv(GL_DEPTH_WRITEMASK, &last_depthmask);

    int width = last_viewport[2];
    int height = last_viewport[3];

    if (width > (int)gl.tex_width || height > (int)gl.tex_height) {
        initialize(width, height);
    }

    // glViewport(0, 0, gl.tex_width, gl.tex_height);
    glViewport(0, 0, width, height);
    glBindVertexArray(gl.vao);

    PUSH_GPU_SECTION("Linearize Depth") {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.linear_depth.fbo);
        glViewport(0, 0, gl.tex_width, gl.tex_height);
        glClearColor(near_far[1],0,0,0);
        glClear(GL_COLOR_BUFFER_BIT);
        glViewport(0, 0, width, height);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        compute_linear_depth(settings.input_textures.depth, near_far[0], near_far[1], ortho);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
    }
    POP_GPU_SECTION()

    PUSH_GPU_SECTION("Generate Linear Depth Mipmaps") {
        glBindTexture(GL_TEXTURE_2D, gl.linear_depth.texture);
        glGenerateMipmap(GL_TEXTURE_2D);
    }
    POP_GPU_SECTION()

    if (settings.temporal_reprojection.enabled) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.velocity.fbo);
        glViewport(0, 0, gl.velocity.tex_width, gl.velocity.tex_height);

        PUSH_GPU_SECTION("Velocity: Tilemax") {
            glDrawBuffer(GL_COLOR_ATTACHMENT0);
            blit_tilemax(settings.input_textures.velocity, gl.tex_width, gl.tex_height);
        }
        POP_GPU_SECTION()

        PUSH_GPU_SECTION("Velocity: Neighbormax") {
            glDrawBuffer(GL_COLOR_ATTACHMENT1);
            blit_neighbormax(gl.velocity.tex_tilemax, gl.velocity.tex_width, gl.velocity.tex_height);
        }
        POP_GPU_SECTION()
    }

    GLenum dst_buffer = GL_COLOR_ATTACHMENT1;
    uint32_t src_texture = gl.targets.tex_color[0];

    auto swap_target = [&dst_buffer, &src_texture]() {
        dst_buffer = dst_buffer == GL_COLOR_ATTACHMENT0 ? GL_COLOR_ATTACHMENT1 : GL_COLOR_ATTACHMENT0;
        src_texture = src_texture == gl.targets.tex_color[0] ? gl.targets.tex_color[1] : gl.targets.tex_color[0];
    };

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.targets.fbo);
    glViewport(0, 0, width, height);

    if (settings.background.enabled) {
        PUSH_GPU_SECTION("Clear HDR")
        glDrawBuffer(dst_buffer);
        glClearColor(settings.background.r, settings.background.g, settings.background.b, 0.f);
        glClear(GL_COLOR_BUFFER_BIT);
        POP_GPU_SECTION()
    } else {
        GLenum draw_buffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, draw_buffers);
        glClearColor(0.0f, 0.0f, 0.0f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT);
        glDrawBuffer(dst_buffer);
    }

    PUSH_GPU_SECTION("Shade")
    shade_deferred(settings.input_textures.depth, settings.input_textures.color, settings.input_textures.normal, inv_P, light_dir, light_col, time);
    POP_GPU_SECTION()

    PUSH_GPU_SECTION("SSAO")
    if (settings.ambient_occlusion[0].enabled) {
        apply_ssao(gl.linear_depth.texture, settings.input_textures.normal, P, settings.ambient_occlusion[0].intensity, settings.ambient_occlusion[0].radius, settings.ambient_occlusion[0].horizon_bias, settings.ambient_occlusion[0].normal_bias);
    }
    if (settings.ambient_occlusion[1].enabled) {
        apply_ssao(gl.linear_depth.texture, settings.input_textures.normal, P, settings.ambient_occlusion[1].intensity, settings.ambient_occlusion[1].radius, settings.ambient_occlusion[1].horizon_bias, settings.ambient_occlusion[1].normal_bias);
    }
    POP_GPU_SECTION()

    if (settings.temporal_reprojection.enabled) {
#if 0
        swap_target();
        glDrawBuffer(dst_buffer);
        apply_aa_tonemapping(src_texture);
#endif

        swap_target();
        glDrawBuffer(dst_buffer);
        const float feedback_min = settings.temporal_reprojection.feedback_min;
        const float feedback_max = settings.temporal_reprojection.feedback_max;
        const float motion_scale = settings.temporal_reprojection.motion_blur.enabled ? settings.temporal_reprojection.motion_blur.motion_scale : 0.f;
        if (motion_scale != 0.f)
            PUSH_GPU_SECTION("Temporal AA + Motion Blur")
        else
            PUSH_GPU_SECTION("Temporal AA")
            apply_temporal_aa(gl.linear_depth.texture, src_texture, settings.input_textures.velocity, gl.velocity.tex_neighbormax, jitter, prev_jitter, feedback_min, feedback_max,
                motion_scale, time);
        POP_GPU_SECTION()

#if 0
        swap_target();
        glDrawBuffer(dst_buffer);
        apply_inverse_aa_tonemapping(src_texture);
#endif

#if 1
        PUSH_GPU_SECTION("Sharpen")
        swap_target();
        glDrawBuffer(dst_buffer);
        sharpen::sharpen(src_texture);
        POP_GPU_SECTION()
#endif
    }

    if (settings.input_textures.emissive) {
        PUSH_GPU_SECTION("Add Emissive")
        glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_ONE);
        blit_texture(settings.input_textures.emissive, 0);
        glDisable(GL_BLEND);
        POP_GPU_SECTION()
    }

    if (settings.depth_of_field.enabled) {
        swap_target();
        glDrawBuffer(dst_buffer);
        PUSH_GPU_SECTION("DOF")
        apply_dof(gl.linear_depth.texture, src_texture, settings.depth_of_field.focus_depth, settings.depth_of_field.focus_scale, time);
        POP_GPU_SECTION()
    }

    PUSH_GPU_SECTION("Tonemapping") {
        swap_target();
        glDrawBuffer(dst_buffer);
        const auto tonemapper = settings.tonemapping.enabled ? settings.tonemapping.mode : Tonemapping::Passthrough;
        apply_tonemapping(src_texture, tonemapper, settings.tonemapping.exposure, settings.tonemapping.gamma);
    }
    POP_GPU_SECTION()

    if (settings.input_textures.post_tonemap) {
        PUSH_GPU_SECTION("Add Post Tonemap")
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blit_texture(settings.input_textures.post_tonemap, 0);
        glDisable(GL_BLEND);
        POP_GPU_SECTION()
    }

    prev_jitter = jitter;

    if (settings.fxaa.enabled) {
        swap_target();
        glDrawBuffer(dst_buffer);
        PUSH_GPU_SECTION("FXAA");
        fxaa::apply_fxaa(src_texture, width, height);
        POP_GPU_SECTION();
    }

    // Activate backbuffer or whatever was bound before
    {
        PUSH_GPU_SECTION("PostProcess Blit Result")
    
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
        glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
        glDrawBuffer((GLenum)last_draw_buffer);

        swap_target();
        glDepthMask(0);
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blit_texture(src_texture, settings.input_textures.depth);
        glDisable(GL_BLEND);
        
        POP_GPU_SECTION();
    }

    // Reset rest of state
    glBlendFunc(GL_ONE, GL_ZERO);
    if (last_blend)
        glEnable(GL_BLEND);
    else
        glDisable(GL_BLEND);

    glDepthMask(last_depthmask);
    glColorMask(last_colormask[0], last_colormask[1], last_colormask[2], last_colormask[3]);
}

}  // namespace postprocessing
