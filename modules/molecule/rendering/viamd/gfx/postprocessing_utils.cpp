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

#include "postprocessing_utils.h"

//#include <core/types.h>
//#include <core/common.h>
//#include <core/log.h>
//#include <core/math_utils.h>
//#include <core/string_utils.h>

#include <core/md_str.h>
#include <core/md_log.h>
#include "gl_utils.h"
// #include <random_util.h>
#include <stdio.h>
#include <string.h>
#include <float.h>

#define PUSH_GPU_SECTION(lbl)                                                                       \
    {                                                                                               \
        if (&glPushDebugGroup) glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 1, -1, lbl); \
    }
#define POP_GPU_SECTION()                       \
    {                                           \
        if (&glPopDebugGroup) glPopDebugGroup(); \
    }

#define VIAMD_SHADER_DIR "shaders"

namespace postprocessing {

// @TODO: Use half-res render targets for SSAO
// @TODO: Use shared textures for all postprocessing operations
// @TODO: Use some kind of unified pipeline for all post processing operations

static struct {
    GLuint vao = 0;
    GLuint v_shader_fs_quad = 0;
    GLuint tex_width = 0;
    GLuint tex_height = 0;

    struct {
        GLuint fbo = 0;
        GLuint tex_rgba8 = 0;
    } tmp;

    struct {
        GLuint fbo = 0;
        GLuint tex_color[2] = {0, 0};
        GLuint tex_temporal_buffer[2] = {0, 0};  // These are dedicated and cannot be use as intermediate buffers by other shaders
    } targets;

    struct {
        GLuint fbo = 0;
        GLuint tex_tilemax = 0;
        GLuint tex_neighbormax = 0;
        int32_t tex_width = 0;
        int32_t tex_height = 0;
    } velocity;

    struct {
        GLuint fbo = 0;
        GLuint texture = 0;
        GLuint program_persp = 0;
        GLuint program_ortho = 0;
        struct {
            GLint clip_info = -1;
            GLint tex_depth = -1;
        } uniform_loc;
    } linear_depth;

    struct {
        GLuint tex_random = 0;
        GLuint ubo_hbao_data = 0;

        struct {
            GLuint fbo = 0;
            GLuint texture = 0;
            GLuint program_persp = 0;
            GLuint program_ortho = 0;
        } hbao;

        struct {
            GLuint fbo = 0;
            GLuint texture = 0;
            GLuint program = 0;
        } blur;
    } ssao;

    struct {
        GLuint program = 0;
        struct {
            GLint tex_half_res = -1;
            GLint tex_color = -1;
            GLint tex_depth = -1;
            GLint pixel_size = -1;
            GLint focus_point = -1;
            GLint focus_scale = -1;
            GLint time = -1;
        } uniform_loc;

        struct {
            GLuint fbo = 0;
            GLuint program = 0;
            struct {
                GLuint color_coc = 0;
            } tex;
            struct {
                GLint tex_depth = -1;
                GLint tex_color = -1;
                GLint focus_point = -1;
                GLint focus_scale = -1;
            } uniform_loc;
        } half_res;
    } bokeh_dof;

    struct {
        GLuint program = 0;
    } bloom;

    struct {
        GLuint program = 0;
        struct {
            GLint mode = -1;
            GLint tex_color = -1;
        } uniform_loc;
    } tonemapping;

    struct {
        struct {
            GLuint program = 0;
            struct {
                GLint tex_linear_depth = -1;
                GLint tex_main = -1;
                GLint tex_prev = -1;
                GLint tex_vel = -1;
                GLint tex_vel_neighbormax = -1;
                GLint texel_size = -1;
                GLint time = -1;
                GLint feedback_min = -1;
                GLint feedback_max = -1;
                GLint motion_scale = -1;
                GLint jitter_uv = -1;
            } uniform_loc;
        } with_motion_blur;
        struct {
            GLuint program = 0;
            struct {
                GLint tex_linear_depth = -1;
                GLint tex_main = -1;
                GLint tex_prev = -1;
                GLint tex_vel = -1;
                GLint texel_size = -1;
                GLint time = -1;
                GLint feedback_min = -1;
                GLint feedback_max = -1;
                GLint motion_scale = -1;
                GLint jitter_uv = -1;
            } uniform_loc;
        } no_motion_blur;
    } temporal;

    struct {
        GLuint program_edge_detection;
        GLuint program_blending_weight_calculation;
        GLuint program_neighborhood_blending;
    } smaa;

} gl;

static constexpr str_t v_shader_src_fs_quad = MAKE_STR(
R"(
#version 150 core

out vec2 tc;

uniform vec2 u_tc_scl = vec2(1,1);

void main() {
	uint idx = uint(gl_VertexID) % 3U;
	gl_Position = vec4(
		(float( idx     &1U)) * 4.0 - 1.0,
		(float((idx>>1U)&1U)) * 4.0 - 1.0,
		0, 1.0);
	tc = (gl_Position.xy * 0.5 + 0.5) * u_tc_scl;
}
)");

static constexpr str_t f_shader_src_linearize_depth = MAKE_STR(
R"(
#ifndef PERSPECTIVE
#define PERSPECTIVE 1
#endif

// z_n * z_f,  z_n - z_f,  z_f, *not used*
uniform vec4 u_clip_info;
uniform sampler2D u_tex_depth;

float denormalizeFloat(float inpt) {
    if (inpt < 0.0) {
        return inpt + 1.0;
    } else {
        return pow(10, 30) * inpt;
    }
}

float ReconstructCSZ(float d, vec4 clip_info) {
#if PERSPECTIVE
    return (clip_info[0] / (d*clip_info[1] + clip_info[2]));
#else
    return (clip_info[1] + clip_info[2] - d*clip_info[1]);
#endif
}

out vec4 out_frag;

void main() {
  float d = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy), 0).x;
  out_frag = vec4(ReconstructCSZ(d, u_clip_info), 0, 0, 0);
}
)");

/*
static constexpr const char* f_shader_src_mip_map_min_depth = R"(
uniform sampler2D u_tex_depth;

out vec4 out_frag;

void main() {
	float d00 = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy) + ivec2(0,0), 0).x;
	float d01 = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy) + ivec2(0,1), 0).x;
	float d10 = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy) + ivec2(1,0), 0).x;
	float d11 = texelFetch(u_tex_depth, ivec2(gl_FragCoord.xy) + ivec2(1,1), 0).x;

	float dmin0 = min(d00, d01);
	float dmin1 = min(d10, d11);

	out_frag = vec4(min(dmin0, dmin1));
}
)";
*/

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

static GLuint setup_program_from_source(str_t name, str_t f_shader_src, str_t defines = {}) {
    GLuint f_shader = gl::compile_shader_from_source(f_shader_src, GL_FRAGMENT_SHADER, defines);
    GLuint program = 0;

    if (f_shader) {
        char buffer[1024];
        program = glCreateProgram();

        glAttachShader(program, gl.v_shader_fs_quad);
        glAttachShader(program, f_shader);
        glLinkProgram(program);
        if (gl::get_program_link_error(buffer, sizeof(buffer), program)) {
            md_printf(MD_LOG_TYPE_ERROR, "Error while linking %.*s program:\n%s", (int)name.len, name.ptr, buffer);
            glDeleteProgram(program);
            return 0;
        }

        glDetachShader(program, gl.v_shader_fs_quad);
        glDetachShader(program, f_shader);
        glDeleteShader(f_shader);
    }

    return program;
}

static GLuint setup_program_from_file(str_t name, str_t filename, str_t defines = {}) {
    GLuint f_shader = gl::compile_shader_from_file(filename, GL_FRAGMENT_SHADER, defines);
    GLuint program = 0;

    if (f_shader) {
        char buffer[1024];
        program = glCreateProgram();

        glAttachShader(program, gl.v_shader_fs_quad);
        glAttachShader(program, f_shader);
        glLinkProgram(program);
        if (gl::get_program_link_error(buffer, sizeof(buffer), program)) {
            md_printf(MD_LOG_TYPE_ERROR, "Error while linking %s program:\n%s", name, buffer);
            glDeleteProgram(program);
            return 0;
        }

        glDetachShader(program, gl.v_shader_fs_quad);
        glDetachShader(program, f_shader);
        glDeleteShader(f_shader);
    }

    return program;
}

static bool is_orthographic_proj_matrix(const mat4_t& proj_mat) { return proj_mat.elem[2][3] == 0.0f; }

namespace ssao {
#ifndef AO_RANDOM_TEX_SIZE
#define AO_RANDOM_TEX_SIZE 4
#endif

struct HBAOData {
    float radius_to_screen;
    float neg_inv_r2;
    float n_dot_v_bias;
    float time;

    float ao_multiplier;
    float pow_exponent;
    vec2_t inv_full_res;

    vec4_t proj_info;

    vec4_t sample_pattern[32];
};

void setup_ubo_hbao_data(GLuint ubo, int width, int height, const mat4_t& proj_mat, float intensity, float radius, float bias, float time) {
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
    const bool ortho = is_orthographic_proj_matrix(proj_mat);
    if (!ortho) {
        proj_info = {
            2.0f / (proj_data[4 * 0 + 0]),                          // (x) * (R - L)/N
            2.0f / (proj_data[4 * 1 + 1]),                          // (y) * (T - B)/N
            -(1.0f - proj_data[4 * 2 + 0]) / proj_data[4 * 0 + 0],  // L/N
            -(1.0f + proj_data[4 * 2 + 1]) / proj_data[4 * 1 + 1]   // B/N
        };

        // proj_scl = float(height) / (math::tan(fovy * 0.5f) * 2.0f);
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
    data.n_dot_v_bias = CLAMP(bias, 0.f, 1.f - FLT_EPSILON);
    data.time = time;
    data.ao_multiplier = 1.f / (1.f - data.n_dot_v_bias);
    data.pow_exponent = MAX(intensity, 0.f);
    data.inv_full_res = {1.f / float(width), 1.f / float(height)};
    data.proj_info = proj_info;
    memcpy(&data.sample_pattern, SAMPLE_PATTERN, sizeof(SAMPLE_PATTERN));

    glBindBuffer(GL_UNIFORM_BUFFER, ubo);
    glBufferData(GL_UNIFORM_BUFFER, sizeof(HBAOData), &data, GL_DYNAMIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

void initialize_rnd_tex(GLuint rnd_tex) {
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
    gl.ssao.hbao.program_persp = setup_program_from_file(MAKE_STR("ssao persp"), MAKE_STR(VIAMD_SHADER_DIR "/ssao/ssao.frag"), MAKE_STR("#define AO_PERSPECTIVE 1"));
    gl.ssao.hbao.program_ortho = setup_program_from_file(MAKE_STR("ssao ortho"), MAKE_STR(VIAMD_SHADER_DIR "/ssao/ssao.frag"), MAKE_STR("#define AO_PERSPECTIVE 0"));
    gl.ssao.blur.program       = setup_program_from_file(MAKE_STR("ssao blur"),  MAKE_STR(VIAMD_SHADER_DIR "/ssao/blur.frag"));
    
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

namespace highlight {

static struct {
    GLuint program = 0;
    GLuint selection_texture = 0;
    struct {
        GLint texture_atom_idx = -1;
        GLint buffer_selection = -1;
        GLint highlight = -1;
        GLint selection = -1;
        GLint outline = -1;
    } uniform_loc;
} highlight;

void initialize() {
    highlight.program = setup_program_from_file(MAKE_STR("highlight"), MAKE_STR(VIAMD_SHADER_DIR "/highlight.frag"));
    if (!highlight.selection_texture) glGenTextures(1, &highlight.selection_texture);
    highlight.uniform_loc.texture_atom_idx = glGetUniformLocation(highlight.program, "u_texture_atom_idx");
    highlight.uniform_loc.buffer_selection = glGetUniformLocation(highlight.program, "u_buffer_selection");
    highlight.uniform_loc.highlight = glGetUniformLocation(highlight.program, "u_highlight");
    highlight.uniform_loc.selection = glGetUniformLocation(highlight.program, "u_selection");
    highlight.uniform_loc.outline = glGetUniformLocation(highlight.program, "u_outline");
}

void shutdown() {
    if (highlight.program) glDeleteProgram(highlight.program);
}
}  // namespace highlight

namespace hsv {

static struct {
    GLuint program = 0;
    struct {
        GLint texture_color = -1;
        GLint hsv_scale = -1;
    } uniform_loc;
} gl;

void initialize() {
    gl.program = setup_program_from_file(MAKE_STR("scale hsv"), MAKE_STR(VIAMD_SHADER_DIR "/scale_hsv.frag"));
    gl.uniform_loc.texture_color = glGetUniformLocation(gl.program, "u_texture_atom_color");
    gl.uniform_loc.hsv_scale = glGetUniformLocation(gl.program, "u_hsv_scale");
}

void shutdown() {
    if (gl.program) glDeleteProgram(gl.program);
}
}  // namespace hsv

namespace deferred {

static struct {
    GLuint program = 0;
    struct {
        GLint texture_depth = -1;
        GLint texture_color = -1;
        GLint texture_normal = -1;
        GLint inv_proj_mat = -1;
        GLint time = -1;
    } uniform_loc;
} deferred;

void initialize() {
    deferred.program = setup_program_from_file(MAKE_STR("deferred shading"), MAKE_STR(VIAMD_SHADER_DIR "/deferred_shading.frag"));
    deferred.uniform_loc.texture_depth = glGetUniformLocation(deferred.program, "u_texture_depth");
    deferred.uniform_loc.texture_color = glGetUniformLocation(deferred.program, "u_texture_color");
    deferred.uniform_loc.texture_normal = glGetUniformLocation(deferred.program, "u_texture_normal");
    deferred.uniform_loc.inv_proj_mat = glGetUniformLocation(deferred.program, "u_inv_proj_mat");
    deferred.uniform_loc.time = glGetUniformLocation(deferred.program, "u_time");
}

void shutdown() {
    if (deferred.program) glDeleteProgram(deferred.program);
}
}  // namespace deferred

namespace tonemapping {

static struct {
    GLuint program = 0;
    struct {
        GLint texture = -1;
    } uniform_loc;
} passthrough;

static struct {
    GLuint program = 0;
    struct {
        GLint texture = -1;
        GLint exposure = -1;
        GLint gamma = -1;
    } uniform_loc;
} exposure_gamma;

static struct {
    GLuint program = 0;
    struct {
        GLint texture = -1;
        GLint exposure = -1;
        GLint gamma = -1;
    } uniform_loc;
} filmic;

static struct {
    GLuint program_forward = 0;
    GLuint program_inverse = 0;
    struct {
        GLint texture = -1;
    } uniform_loc;
} fast_reversible;

void initialize() {
    {
        // PASSTHROUGH
        passthrough.program = setup_program_from_file(MAKE_STR("Passthrough"), MAKE_STR(VIAMD_SHADER_DIR "/tonemap/passthrough.frag"));
        passthrough.uniform_loc.texture = glGetUniformLocation(passthrough.program, "u_texture");
    }
    {
        // EXPOSURE GAMMA
        exposure_gamma.program = setup_program_from_file(MAKE_STR("Exposure Gamma"), MAKE_STR(VIAMD_SHADER_DIR "/tonemap/exposure_gamma.frag"));
        exposure_gamma.uniform_loc.texture = glGetUniformLocation(exposure_gamma.program, "u_texture");
        exposure_gamma.uniform_loc.exposure = glGetUniformLocation(exposure_gamma.program, "u_exposure");
        exposure_gamma.uniform_loc.gamma = glGetUniformLocation(exposure_gamma.program, "u_gamma");
    }
    {
        // UNCHARTED
        filmic.program = setup_program_from_file(MAKE_STR("Filmic"), MAKE_STR(VIAMD_SHADER_DIR "/tonemap/uncharted.frag"));
        filmic.uniform_loc.texture = glGetUniformLocation(filmic.program, "u_texture");
        filmic.uniform_loc.exposure = glGetUniformLocation(filmic.program, "u_exposure");
        filmic.uniform_loc.gamma = glGetUniformLocation(filmic.program, "u_gamma");
    }
    {
        // Fast Reversible (For AA) (Credits to Brian Karis: http://graphicrants.blogspot.com/2013/12/tone-mapping.html)
        fast_reversible.program_forward = setup_program_from_file(MAKE_STR("Fast Reversible"), MAKE_STR(VIAMD_SHADER_DIR "/tonemap/fast_reversible.frag"), MAKE_STR("#define USE_INVERSE 0"));
        fast_reversible.program_inverse = setup_program_from_file(MAKE_STR("Fast Reversible"), MAKE_STR(VIAMD_SHADER_DIR "/tonemap/fast_reversible.frag"), MAKE_STR("#define USE_INVERSE 1"));
        fast_reversible.uniform_loc.texture = glGetUniformLocation(fast_reversible.program_forward, "u_texture");
    }
}

void shutdown() {
    if (passthrough.program) glDeleteProgram(passthrough.program);
    if (exposure_gamma.program) glDeleteProgram(exposure_gamma.program);
    if (filmic.program) glDeleteProgram(filmic.program);
    if (fast_reversible.program_forward) glDeleteProgram(fast_reversible.program_forward);
    if (fast_reversible.program_inverse) glDeleteProgram(fast_reversible.program_inverse);
}

}  // namespace tonemapping

namespace dof {
void initialize(int32_t width, int32_t height) {
    {
        gl.bokeh_dof.half_res.program = setup_program_from_file(MAKE_STR("DOF prepass"), MAKE_STR(VIAMD_SHADER_DIR "/dof/dof_half_res_prepass.frag"));
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
            md_print(MD_LOG_TYPE_ERROR, "Something went wrong when generating framebuffer for DOF");
        }
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    }

    // DOF
    {
        gl.bokeh_dof.program = setup_program_from_file(MAKE_STR("Bokeh DOF"), MAKE_STR(VIAMD_SHADER_DIR "/dof/dof.frag"));
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
static GLuint program_tex = 0;
static GLuint program_col = 0;
static GLint uniform_loc_texture = -1;
static GLint uniform_loc_color = -1;

constexpr str_t f_shader_src_tex = MAKE_STR(R"(
#version 150 core

uniform sampler2D u_texture;

out vec4 out_frag;

void main() {
    out_frag = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
}
)");

constexpr str_t f_shader_src_col = MAKE_STR(R"(
#version 150 core

uniform vec4 u_color;
out vec4 out_frag;

void main() {
	out_frag = u_color;
}
)");

void initialize() {
    program_tex = setup_program_from_source(MAKE_STR("blit texture"), f_shader_src_tex);
    uniform_loc_texture = glGetUniformLocation(program_tex, "u_texture");

    program_col = setup_program_from_source(MAKE_STR("blit color"), f_shader_src_col);
    uniform_loc_color = glGetUniformLocation(program_col, "u_color");
}

void shutdown() {
    if (program_tex) glDeleteProgram(program_tex);
    if (program_col) glDeleteProgram(program_col);
}
}  // namespace blit

namespace blur {
static GLuint program_gaussian = 0;
static GLuint program_box = 0;
static GLint uniform_loc_texture = -1;
static GLint uniform_loc_inv_res_dir = -1;

constexpr str_t f_shader_src_gaussian = MAKE_STR(R"(
#version 150 core

#define KERNEL_RADIUS 5

uniform sampler2D u_texture;
uniform vec2      u_inv_res_dir;

in vec2 tc;
out vec4 out_frag;

float blur_weight(float r) {
    const float sigma = KERNEL_RADIUS * 0.5;
    const float falloff = 1.0 / (2.0*sigma*sigma);
    float w = exp2(-r*r*falloff);
    return w;
}

void main() {
    vec2 uv = tc;
    vec4  c_tot = texture(u_texture, uv);
    float w_tot = 1.0;

    for (float r = 1; r <= KERNEL_RADIUS; ++r) {
        float w = blur_weight(r);
        vec4  c = texture(u_texture, uv + u_inv_res_dir * r);
        c_tot += c * w;
        w_tot += w;
    }
    for (float r = 1; r <= KERNEL_RADIUS; ++r) {
        float w = blur_weight(r);
        vec4  c = texture(u_texture, uv - u_inv_res_dir * r);
        c_tot += c * w;
        w_tot += w;
    }

    out_frag = c_tot / w_tot;
}
)");

constexpr str_t f_shader_src_box = MAKE_STR(R"(
#version 150 core

uniform sampler2D u_texture;
out vec4 out_frag;

void main() {
    vec4 c = vec4(0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(-1, -1), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2( 0, -1), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(+1, -1), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(-1,  0), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2( 0,  0), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(+1,  0), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(-1, +1), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2( 0, +1), 0);
    c += texelFetch(u_texture, ivec2(gl_FragCoord.xy) + ivec2(+1, +1), 0);

    out_frag = c / 9.0;
}
)");

void initialize() {
    program_gaussian = setup_program_from_source(MAKE_STR("gaussian blur"), f_shader_src_gaussian);
    uniform_loc_texture = glGetUniformLocation(program_gaussian, "u_texture");
    uniform_loc_inv_res_dir = glGetUniformLocation(program_gaussian, "u_inv_res_dir");

    program_box = setup_program_from_source(MAKE_STR("box blur"), f_shader_src_box);
}

void shutdown() {
    if (program_gaussian) glDeleteProgram(program_gaussian);
    if (program_box) glDeleteProgram(program_box);
}
}  // namespace blit

namespace velocity {
#define VEL_TILE_SIZE 8

struct {
    GLuint program = 0;
    struct {
		GLint tex_depth = -1;
        GLint curr_clip_to_prev_clip_mat = -1;
        GLint jitter_uv = -1;
    } uniform_loc;
} blit_velocity;

struct {
    GLuint program = 0;
    struct {
        GLint tex_vel = -1;
        GLint tex_vel_texel_size = -1;
    } uniform_loc;
} blit_tilemax;

struct {
    GLuint program = 0;
    struct {
        GLint tex_vel = -1;
        GLint tex_vel_texel_size = -1;
    } uniform_loc;
} blit_neighbormax;

void initialize(int32_t width, int32_t height) {
    {
        blit_velocity.program = setup_program_from_file(MAKE_STR("screen-space velocity"), MAKE_STR(VIAMD_SHADER_DIR "/velocity/blit_velocity.frag"));
		blit_velocity.uniform_loc.tex_depth = glGetUniformLocation(blit_velocity.program, "u_tex_depth");
        blit_velocity.uniform_loc.curr_clip_to_prev_clip_mat = glGetUniformLocation(blit_velocity.program, "u_curr_clip_to_prev_clip_mat");
        blit_velocity.uniform_loc.jitter_uv = glGetUniformLocation(blit_velocity.program, "u_jitter_uv");

    }
    {
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
        str_t defines = MAKE_STR("#define TILE_SIZE " TOSTRING(VEL_TILE_SIZE));
        blit_tilemax.program = setup_program_from_file(MAKE_STR("tilemax"), MAKE_STR(VIAMD_SHADER_DIR "/velocity/blit_tilemax.frag"), defines);
        blit_tilemax.uniform_loc.tex_vel = glGetUniformLocation(blit_tilemax.program, "u_tex_vel");
        blit_tilemax.uniform_loc.tex_vel_texel_size = glGetUniformLocation(blit_tilemax.program, "u_tex_vel_texel_size");
#undef STRINGIFY
#undef TOSTRING
    }
    {
        blit_neighbormax.program = setup_program_from_file(MAKE_STR("neighbormax"), MAKE_STR(VIAMD_SHADER_DIR "/velocity/blit_neighbormax.frag"));
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
            md_print(MD_LOG_TYPE_ERROR, "Something went wrong in creating framebuffer for velocity");
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
        gl.temporal.with_motion_blur.program = setup_program_from_file(MAKE_STR("temporal aa + motion-blur"), MAKE_STR(VIAMD_SHADER_DIR "/temporal.frag"));
        gl.temporal.no_motion_blur.program   = setup_program_from_file(MAKE_STR("temporal aa"), MAKE_STR(VIAMD_SHADER_DIR "/temporal.frag"), MAKE_STR("#define USE_MOTION_BLUR 0\n"));

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
static GLuint program = 0;
void initialize() {
    constexpr str_t f_shader_src_sharpen = MAKE_STR(
 R"(#version 150 core

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
    program = setup_program_from_source(MAKE_STR("sharpen"), f_shader_src_sharpen);
}

void sharpen(GLuint in_texture) {
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
}

void initialize(int width, int height) {
    if (!gl.vao) glGenVertexArrays(1, &gl.vao);

    gl.v_shader_fs_quad = gl::compile_shader_from_source(v_shader_src_fs_quad, GL_VERTEX_SHADER);

    // LINEARIZE DEPTH

    gl.linear_depth.program_persp = setup_program_from_source(MAKE_STR("linearize depth persp"), f_shader_src_linearize_depth, MAKE_STR("#version 150 core\n#define PERSPECTIVE 1"));
    gl.linear_depth.program_ortho = setup_program_from_source(MAKE_STR("linearize depth ortho"), f_shader_src_linearize_depth, MAKE_STR("#version 150 core\n#define PERSPECTIVE 0"));

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
            md_print(MD_LOG_TYPE_ERROR, "Something went wrong in creating framebuffer for depth linearization");
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
            md_print(MD_LOG_TYPE_ERROR, "Something went wrong in creating framebuffer for targets");
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
    deferred::initialize();
    highlight::initialize();
    hsv::initialize();
    tonemapping::initialize();
    temporal::initialize();
    blit::initialize();
    blur::initialize();
    sharpen::initialize();
}

void shutdown() {
    ssao::shutdown();
    dof::shutdown();
    velocity::shutdown();
    deferred::shutdown();
    highlight::shutdown();
    hsv::shutdown();
    tonemapping::shutdown();
    temporal::shutdown();
    blit::shutdown();
    blur::shutdown();
    sharpen::shutdown();

    if (gl.vao) glDeleteVertexArrays(1, &gl.vao);
    //if (gl.vbo) glDeleteBuffers(1, &gl.vbo);
    if (gl.v_shader_fs_quad) glDeleteShader(gl.v_shader_fs_quad);
    if (gl.tmp.fbo) glDeleteFramebuffers(1, &gl.tmp.fbo);
    if (gl.tmp.tex_rgba8) glDeleteTextures(1, &gl.tmp.tex_rgba8);
}

void compute_linear_depth(GLuint depth_tex, float near_plane, float far_plane, bool orthographic = false) {
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

void apply_ssao(GLuint linear_depth_tex, GLuint normal_tex, const mat4_t& proj_matrix, float intensity, float radius, float bias, float time) {
    ASSERT(glIsTexture(linear_depth_tex));
    ASSERT(glIsTexture(normal_tex));

    const bool ortho = is_orthographic_proj_matrix(proj_matrix);
    const float sharpness = ssao::compute_sharpness(radius);

    GLint last_fbo;
    GLint last_viewport[4];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);

    int width = last_viewport[2];
    int height = last_viewport[3];

    const vec2_t inv_res = vec2_t{1.f / gl.tex_width, 1.f / gl.tex_height};

    glBindVertexArray(gl.vao);

    ssao::setup_ubo_hbao_data(gl.ssao.ubo_hbao_data, width, height, proj_matrix, intensity, radius, bias, time);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.ssao.hbao.fbo);
    glViewport(0, 0, gl.tex_width, gl.tex_height);
    glClearColor(1,1,1,1);
    glClear(GL_COLOR_BUFFER_BIT);

    glViewport(0, 0, width, height);

    // RENDER HBAO
    GLuint program = ortho ? gl.ssao.hbao.program_ortho : gl.ssao.hbao.program_persp;

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
    //glUniform2f(glGetUniformLocation(program, "u_tc_scl"), (float)gl.tex_width/(float)width, (float)gl.tex_height/(float)height);

    glDrawArrays(GL_TRIANGLES, 0, 3);

    POP_GPU_SECTION()

    glUseProgram(gl.ssao.blur.program);

    glUniform1i(glGetUniformLocation(gl.ssao.blur.program, "u_tex_linear_depth"), 0);
    glUniform1i(glGetUniformLocation(gl.ssao.blur.program, "u_tex_ao"), 1);
    glUniform1f(glGetUniformLocation(gl.ssao.blur.program, "u_sharpness"), sharpness);
    glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_inv_res_dir"), inv_res.x, 0);

    glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_tc_scl"), (float)width/(float)gl.tex_width, (float)height/(float)gl.tex_height);

    //glUniform2f(glGetUniformLocation(gl.ssao.blur.program, "u_tc_scl"), (float)gl.tex_width/(float)width, (float)gl.tex_height/(float)height);

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

void shade_deferred(GLuint depth_tex, GLuint color_tex, GLuint normal_tex, const mat4_t& inv_proj_matrix, float time) {
    ASSERT(glIsTexture(depth_tex));
    ASSERT(glIsTexture(color_tex));
    ASSERT(glIsTexture(normal_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth_tex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, color_tex);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, normal_tex);

    glUseProgram(deferred::deferred.program);
    glUniform1i(deferred::deferred.uniform_loc.texture_depth, 0);
    glUniform1i(deferred::deferred.uniform_loc.texture_color, 1);
    glUniform1i(deferred::deferred.uniform_loc.texture_normal, 2);
    glUniformMatrix4fv(deferred::deferred.uniform_loc.inv_proj_mat, 1, GL_FALSE, &inv_proj_matrix.elem[0][0]);
    glUniform1f(deferred::deferred.uniform_loc.time, time);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void highlight_selection(GLuint atom_idx_tex, GLuint selection_buffer, const vec3_t& highlight, const vec3_t& selection, const vec3_t& outline) {
    ASSERT(glIsTexture(atom_idx_tex));
    ASSERT(glIsBuffer(selection_buffer));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, atom_idx_tex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, highlight::highlight.selection_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI, selection_buffer);

    glUseProgram(highlight::highlight.program);
    glUniform1i(highlight::highlight.uniform_loc.texture_atom_idx, 0);
    glUniform1i(highlight::highlight.uniform_loc.buffer_selection, 1);
    glUniform3fv(highlight::highlight.uniform_loc.highlight, 1, &highlight.x);
    glUniform3fv(highlight::highlight.uniform_loc.selection, 1, &selection.x);
    glUniform3fv(highlight::highlight.uniform_loc.outline, 1, &outline.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void half_res_color_coc(GLuint linear_depth_tex, GLuint color_tex, float focus_point, float focus_scale) {
    PUSH_GPU_SECTION("DOF Prepass");
    GLint last_viewport[4];
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

void apply_dof(GLuint linear_depth_tex, GLuint color_tex, float focus_point, float focus_scale, float time) {
    ASSERT(glIsTexture(linear_depth_tex));
    ASSERT(glIsTexture(color_tex));

    const vec2_t pixel_size = vec2_t{1.f / gl.tex_width, 1.f / gl.tex_height};

    GLint last_fbo;
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

void apply_tonemapping(GLuint color_tex, Tonemapping tonemapping, float exposure, float gamma) {
    ASSERT(glIsTexture(color_tex));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    switch (tonemapping) {
        case Tonemapping_ExposureGamma:
            glUseProgram(tonemapping::exposure_gamma.program);
            glUniform1i(tonemapping::exposure_gamma.uniform_loc.texture, 0);
            glUniform1f(tonemapping::exposure_gamma.uniform_loc.exposure, exposure);
            glUniform1f(tonemapping::exposure_gamma.uniform_loc.gamma, gamma);
            break;
        case Tonemapping_Filmic:
            glUseProgram(tonemapping::filmic.program);
            glUniform1i(tonemapping::filmic.uniform_loc.texture, 0);
            glUniform1f(tonemapping::filmic.uniform_loc.exposure, exposure);
            glUniform1f(tonemapping::filmic.uniform_loc.gamma, gamma);
            break;
        case Tonemapping_Passthrough:
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

void apply_aa_tonemapping(GLuint color_tex) {
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

void apply_inverse_aa_tonemapping(GLuint color_tex) {
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

void blit_static_velocity(GLuint depth_tex, const ViewParam& view_param) {
    mat4_t curr_clip_to_prev_clip_mat = view_param.matrix.previous.view_proj_jittered * view_param.matrix.inverse.view_proj_jittered;

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, depth_tex);

    const vec2_t res = view_param.resolution;
    const vec2_t jitter_uv_cur = view_param.jitter.current / res;
    const vec2_t jitter_uv_prev = view_param.jitter.previous / res;
    const vec4_t jitter_uv = {jitter_uv_cur.x, jitter_uv_cur.y, jitter_uv_prev.x, jitter_uv_prev.y};

    glUseProgram(velocity::blit_velocity.program);
	glUniform1i(velocity::blit_velocity.uniform_loc.tex_depth, 0);
    glUniformMatrix4fv(velocity::blit_velocity.uniform_loc.curr_clip_to_prev_clip_mat, 1, GL_FALSE, &curr_clip_to_prev_clip_mat.elem[0][0]);
    glUniform4fv(velocity::blit_velocity.uniform_loc.jitter_uv, 1, &jitter_uv.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blit_tilemax(GLuint velocity_tex, int tex_width, int tex_height) {
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

void blit_neighbormax(GLuint velocity_tex, int tex_width, int tex_height) {
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

void apply_temporal_aa(GLuint linear_depth_tex, GLuint color_tex, GLuint velocity_tex, GLuint velocity_neighbormax_tex, const vec2_t& curr_jitter, const vec2_t& prev_jitter, float feedback_min,
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

    GLint bound_buffer;
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

void scale_hsv(GLuint color_tex, vec3_t hsv_scale) {
    GLint last_fbo;
    GLint last_viewport[4];
    GLint last_draw_buffer;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    glGetIntegerv(GL_DRAW_BUFFER, &last_draw_buffer);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.targets.fbo);
    glDrawBuffer(GL_COLOR_ATTACHMENT0);
    glViewport(0, 0, gl.tex_width, gl.tex_height);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, color_tex);

    glUseProgram(hsv::gl.program);
    glUniform1i(hsv::gl.uniform_loc.texture_color, 0);
    glUniform3fv(hsv::gl.uniform_loc.hsv_scale, 1, &hsv_scale.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    glDrawBuffer((GLenum)last_draw_buffer);

    blit_texture(gl.targets.tex_color[0]);
}

void blit_texture(GLuint tex) {
    ASSERT(glIsTexture(tex));
    glUseProgram(blit::program_tex);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex);
    glUniform1i(blit::uniform_loc_texture, 0);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blit_color(vec4_t color) {
    glUseProgram(blit::program_col);
    glUniform4fv(blit::uniform_loc_color, 1, &color.x);
    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);
    glUseProgram(0);
}

void blur_texture_gaussian(GLuint tex, int num_passes) {
    ASSERT(glIsTexture(tex));
    ASSERT(num_passes > 0);

    GLint last_fbo;
    GLint last_viewport[4];
    GLint last_draw_buffer[8];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    for (int i = 0; i < 8; ++i) glGetIntegerv(GL_DRAW_BUFFER0 + i, &last_draw_buffer[i]);

    GLint w, h;

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex);

    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);

    glBindVertexArray(gl.vao);

    glUseProgram(blur::program_gaussian);
    glUniform1i(blur::uniform_loc_texture, 0);

    glViewport(0, 0, w, h);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.tmp.fbo);

    for (int i = 0; i < num_passes; ++i) {
        glBindTexture(GL_TEXTURE_2D, tex);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.tmp.tex_rgba8, 0);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glUniform2f(blur::uniform_loc_inv_res_dir, 1.0f / w, 0.0f);
        glDrawArrays(GL_TRIANGLES, 0, 3);

        glBindTexture(GL_TEXTURE_2D, gl.tmp.tex_rgba8);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, tex, 0);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glUniform2f(blur::uniform_loc_inv_res_dir, 0.0f, 1.0f / h);
        glDrawArrays(GL_TRIANGLES, 0, 3);
    }

    glUseProgram(0);
    glBindVertexArray(0);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    for (int i = 0; i < 8; ++i) glDrawBuffers(8, (GLenum*)last_draw_buffer);
}

void blur_texture_box(GLuint tex, int num_passes) {
    ASSERT(glIsTexture(tex));
    ASSERT(num_passes > 0);

    GLint last_fbo;
    GLint last_viewport[4];
    GLint last_draw_buffer[8];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    for (int i = 0; i < 8; ++i) glGetIntegerv(GL_DRAW_BUFFER0 + i, &last_draw_buffer[i]);

    GLint w, h;

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex);

    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &h);

    glBindVertexArray(gl.vao);

    glUseProgram(blur::program_box);

    glViewport(0, 0, w, h);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.tmp.fbo);

    for (int i = 0; i < num_passes; ++i) {
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gl.tmp.tex_rgba8, 0);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glDrawArrays(GL_TRIANGLES, 0, 3);

        glBindTexture(GL_TEXTURE_2D, gl.tmp.tex_rgba8);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, tex, 0);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glDrawArrays(GL_TRIANGLES, 0, 3);
    }

    glUseProgram(0);
    glBindVertexArray(0);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    for (int i = 0; i < 8; ++i) glDrawBuffers(8, (GLenum*)last_draw_buffer);
}

void postprocess(const Descriptor& desc, const ViewParam& view_param) {
    ASSERT(glIsTexture(desc.input_textures.depth));
    ASSERT(glIsTexture(desc.input_textures.color));
    ASSERT(glIsTexture(desc.input_textures.normal));
    if (desc.temporal_reprojection.enabled) {
        ASSERT(glIsTexture(desc.input_textures.velocity));
    }

    // For seeding noise
    static float time = 0.f;
    time = time + 0.016f;
    if (time > 100.f) time -= 100.f;

    const auto near_dist = view_param.clip_planes.near;
    const auto far_dist = view_param.clip_planes.far;
    const auto ortho = is_orthographic_proj_matrix(view_param.matrix.current.proj);

    GLint last_fbo;
    GLint last_viewport[4];
    GLint last_draw_buffer;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
    glGetIntegerv(GL_VIEWPORT, last_viewport);
    glGetIntegerv(GL_DRAW_BUFFER, &last_draw_buffer);

    int width = last_viewport[2];
    int height = last_viewport[3];

    if (width > (int)gl.tex_width || height > (int)gl.tex_height) {
        initialize(width, height);
    }

    //glViewport(0, 0, gl.tex_width, gl.tex_height);
    glViewport(0, 0, width, height);
    glBindVertexArray(gl.vao);

    PUSH_GPU_SECTION("Linearize Depth") {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.linear_depth.fbo);
        glViewport(0, 0, gl.tex_width, gl.tex_height);
        glClearColor(far_dist,0,0,0);
        glClear(GL_COLOR_BUFFER_BIT);
        glViewport(0, 0, width, height);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        compute_linear_depth(desc.input_textures.depth, near_dist, far_dist, ortho);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
    }
    POP_GPU_SECTION()

    PUSH_GPU_SECTION("Generate Linear Depth Mipmaps") {
        glBindTexture(GL_TEXTURE_2D, gl.linear_depth.texture);
        glGenerateMipmap(GL_TEXTURE_2D);
    }
    POP_GPU_SECTION()

    if (desc.temporal_reprojection.enabled) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.velocity.fbo);
        glViewport(0, 0, gl.velocity.tex_width, gl.velocity.tex_height);

        PUSH_GPU_SECTION("Velocity: Tilemax") {
            glDrawBuffer(GL_COLOR_ATTACHMENT0);
            blit_tilemax(desc.input_textures.velocity, gl.tex_width, gl.tex_height);
        }
        POP_GPU_SECTION()

        PUSH_GPU_SECTION("Velocity: Neighbormax") {
            glDrawBuffer(GL_COLOR_ATTACHMENT1);
            blit_neighbormax(gl.velocity.tex_tilemax, gl.velocity.tex_width, gl.velocity.tex_height);
        }
        POP_GPU_SECTION()
    }

    GLenum dst_buffer = GL_COLOR_ATTACHMENT1;
    GLuint src_texture = gl.targets.tex_color[0];

    auto swap_target = [&dst_buffer, &src_texture]() {
        dst_buffer = dst_buffer == GL_COLOR_ATTACHMENT0 ? GL_COLOR_ATTACHMENT1 : GL_COLOR_ATTACHMENT0;
        src_texture = src_texture == gl.targets.tex_color[0] ? gl.targets.tex_color[1] : gl.targets.tex_color[0];
    };

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.targets.fbo);
    glDrawBuffer(dst_buffer);
    //glViewport(0, 0, gl.tex_width, gl.tex_height);
    glViewport(0, 0, width, height);

    PUSH_GPU_SECTION("Clear HDR")
    glClearColor(desc.background.intensity.x, desc.background.intensity.y, desc.background.intensity.z, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    POP_GPU_SECTION()

    PUSH_GPU_SECTION("Shade")
    shade_deferred(desc.input_textures.depth, desc.input_textures.color, desc.input_textures.normal, view_param.matrix.inverse.proj_jittered, time);
    POP_GPU_SECTION()

    if (desc.ambient_occlusion.enabled) {
        PUSH_GPU_SECTION("SSAO")
        apply_ssao(gl.linear_depth.texture, desc.input_textures.normal, view_param.matrix.current.proj_jittered, desc.ambient_occlusion.intensity, desc.ambient_occlusion.radius, desc.ambient_occlusion.bias, time);
        POP_GPU_SECTION()
    }

    if (desc.temporal_reprojection.enabled) {
#if 0
        swap_target();
        glDrawBuffer(dst_buffer);
        apply_aa_tonemapping(src_texture);
#endif

        swap_target();
        glDrawBuffer(dst_buffer);
        const float feedback_min = desc.temporal_reprojection.feedback_min;
        const float feedback_max = desc.temporal_reprojection.feedback_max;
        const float motion_scale = desc.temporal_reprojection.motion_blur.enabled ? desc.temporal_reprojection.motion_blur.motion_scale : 0.f;
        if (motion_scale != 0.f)
            PUSH_GPU_SECTION("Temporal AA + Motion Blur")
        else
            PUSH_GPU_SECTION("Temporal AA")

            apply_temporal_aa(gl.linear_depth.texture, src_texture, desc.input_textures.velocity, gl.velocity.tex_neighbormax, view_param.jitter.current, view_param.jitter.previous, feedback_min, feedback_max,
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

    if (desc.input_textures.emissive) {
        PUSH_GPU_SECTION("Add Emissive")
        glEnable(GL_BLEND);
        glBlendFunc(GL_ONE, GL_ONE);
        blit_texture(desc.input_textures.emissive);
        glDisable(GL_BLEND);
        POP_GPU_SECTION()
    }

    if (desc.depth_of_field.enabled) {
        swap_target();
        glDrawBuffer(dst_buffer);
        PUSH_GPU_SECTION("DOF")
        apply_dof(gl.linear_depth.texture, src_texture, desc.depth_of_field.focus_depth, desc.depth_of_field.focus_scale, time);
        POP_GPU_SECTION()
    }

    PUSH_GPU_SECTION("Tonemapping") {
        swap_target();
        glDrawBuffer(dst_buffer);
        const auto tonemapper = desc.tonemapping.enabled ? desc.tonemapping.mode : Tonemapping_Passthrough;
        apply_tonemapping(src_texture, tonemapper, desc.tonemapping.exposure, desc.tonemapping.gamma);
    }
    POP_GPU_SECTION()

    if (desc.input_textures.post_tonemap) {
        PUSH_GPU_SECTION("Add Post Tonemap")
        glEnable(GL_BLEND);
        glColorMask(1, 1, 1, 1);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        blit_texture(desc.input_textures.post_tonemap);
        glDisable(GL_BLEND);
        POP_GPU_SECTION()
    }

    // Activate backbuffer or whatever was bound before
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
    glViewport(last_viewport[0], last_viewport[1], last_viewport[2], last_viewport[3]);
    glDrawBuffer((GLenum)last_draw_buffer);

    swap_target();
    glDepthMask(0);
    blit_texture(src_texture);

    glDepthMask(1);
    glColorMask(1, 1, 1, 1);
}

}  // namespace postprocessing
