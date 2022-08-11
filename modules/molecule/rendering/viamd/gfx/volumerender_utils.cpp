#include "volumerender_utils.h"

#include "image.h"
#include "gfx/gl_utils.h"
#include "gfx/immediate_draw_utils.h"
#include "color_utils.h"

#include <core/md_common.h>
#include <core/md_log.h>
#include <core/md_file.h>
#include <core/md_vec_math.h>

#include <imgui.h>

namespace volume {

static struct {
    GLuint vao = 0;
    GLuint vbo = 0;
    GLuint ubo = 0;
    GLuint fbo = 0;

    struct {
        GLuint dvr_only = 0;
        GLuint iso_only = 0;
        GLuint dvr_and_iso = 0;
    } program;
} gl;

struct UniformData {
    mat4_t view_to_model_mat;
    mat4_t model_to_view_mat;
    mat4_t inv_proj_mat;
    mat4_t model_view_proj_mat;

    vec2_t inv_res;
    float density_scale;
    float alpha_scale;

    vec3_t clip_volume_min;
    float _pad0;
    vec3_t clip_volume_max;
    float time;

    vec3_t gradient_spacing_world_space;
    float _pad1;
    mat4_t gradient_spacing_tex_space;
};

void initialize() {
    GLuint v_shader = gl::compile_shader_from_file(MAKE_STR(VIAMD_SHADER_DIR "/volume/raycaster.vert"), GL_VERTEX_SHADER);
    GLuint f_shader_dvr_only = gl::compile_shader_from_file(MAKE_STR(VIAMD_SHADER_DIR "/volume/raycaster.frag"), GL_FRAGMENT_SHADER, MAKE_STR("#define INCLUDE_DVR"));
    GLuint f_shader_iso_only = gl::compile_shader_from_file(MAKE_STR(VIAMD_SHADER_DIR "/volume/raycaster.frag"), GL_FRAGMENT_SHADER, MAKE_STR("#define INCLUDE_ISO"));
    GLuint f_shader_dvr_and_iso =
        gl::compile_shader_from_file(MAKE_STR(VIAMD_SHADER_DIR "/volume/raycaster.frag"), GL_FRAGMENT_SHADER, MAKE_STR("#define INCLUDE_DVR\n#define INCLUDE_ISO"));
    defer {
        glDeleteShader(v_shader);
        glDeleteShader(f_shader_dvr_only);
        glDeleteShader(f_shader_iso_only);
        glDeleteShader(f_shader_dvr_and_iso);
    };

    if (v_shader == 0u || f_shader_dvr_only == 0u || f_shader_iso_only == 0u || f_shader_dvr_and_iso == 0u) {
        md_print(MD_LOG_TYPE_ERROR, "shader compilation failed, shader program for raycasting will not be updated");
        return;
    }

    if (!gl.program.dvr_only) gl.program.dvr_only = glCreateProgram();
    if (!gl.program.iso_only) gl.program.iso_only = glCreateProgram();
    if (!gl.program.dvr_and_iso) gl.program.dvr_and_iso = glCreateProgram();

    {
        const GLuint shaders[] = {v_shader, f_shader_dvr_only};
        gl::attach_link_detach(gl.program.dvr_only, shaders, ARRAY_SIZE(shaders));
    }
    {
        const GLuint shaders[] = {v_shader, f_shader_iso_only};
        gl::attach_link_detach(gl.program.iso_only, shaders, ARRAY_SIZE(shaders));
    }
    {
        const GLuint shaders[] = {v_shader, f_shader_dvr_and_iso};
        gl::attach_link_detach(gl.program.dvr_and_iso, shaders, ARRAY_SIZE(shaders));
    }

    if (!gl.vbo) {
        // https://stackoverflow.com/questions/28375338/cube-using-single-gl-triangle-strip
        constexpr uint8_t cube_strip[42] = {0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1,
                                            0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0};
        glGenBuffers(1, &gl.vbo);
        glBindBuffer(GL_ARRAY_BUFFER, gl.vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(cube_strip), cube_strip, GL_STATIC_DRAW);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
    }

    if (!gl.vao) {
        glGenVertexArrays(1, &gl.vao);
        glBindVertexArray(gl.vao);
        glBindBuffer(GL_ARRAY_BUFFER, gl.vbo);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_UNSIGNED_BYTE, GL_FALSE, 0, (const GLvoid*)0);
        glBindVertexArray(0);
    }

    if (!gl.ubo) {
        glGenBuffers(1, &gl.ubo);
        glBindBuffer(GL_UNIFORM_BUFFER, gl.ubo);
        glBufferData(GL_UNIFORM_BUFFER, sizeof(UniformData), 0, GL_DYNAMIC_DRAW);
        glBindBuffer(GL_UNIFORM_BUFFER, 0);
    }

    if (!gl.fbo) {
        glGenFramebuffers(1, &gl.fbo);
    }
}

void shutdown() {}

/*
void create_tf_texture(GLuint* texture, int* width, CStringView path) {
    ASSERT(texture);
    // load transfer function
    if (*texture == 0 || !glIsTexture(*texture)) {
        glGenTextures(1, texture);
    }

    Image img;
    defer { free_image(&img); };
    if (read_image(&img, path)) {
        if (width) {
            *width = img.width;
        }
        glBindTexture(GL_TEXTURE_2D, *texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, img.width, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, img.data);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindTexture(GL_TEXTURE_2D, 0);
    } else {
        md_printf(MD_LOG_TYPE_ERROR, "could not read TF ('%.s')", path.length(), path.cstr());
    }
}
*/



/*
void set_volume_texture_data(GLuint texture, ivec3 dim, const float* data, float max_value) {
    if (glIsTexture(texture)) {
        uint8_t* rescaled_data = (uint8_t*)TMP_MALLOC(dim.x * dim.y * dim.z);
        defer { TMP_FREE(rescaled_data); };

        if (max_value > 0) {
            const int size = dim.x * dim.y * dim.z;
            for (int i = 0; i < size; ++i) {
                rescaled_data[i] = (uint8_t)((double)data[i] / (double)max_value * 255);
            }
        }
        glBindTexture(GL_TEXTURE_3D, texture);
        glTexSubImage3D(GL_TEXTURE_3D, 0, 0, 0, 0, dim.x, dim.y, dim.z, GL_RED, GL_UNSIGNED_BYTE, rescaled_data);
        glBindTexture(GL_TEXTURE_3D, 0);
    }
}
*/

mat4_t compute_model_to_world_matrix(vec3_t min_world_aabb, vec3_t max_world_aabb) {
    vec3_t ext = max_world_aabb - min_world_aabb;
    vec3_t off = min_world_aabb;
    return {
        .col = {
            {ext.x, 0, 0, 0},
            {0, ext.y, 0, 0},
            {0, 0, ext.z, 0},
            {off.x, off.y, off.z, 1},
        }
    };
}

mat4_t compute_world_to_model_matrix(vec3_t min_world_aabb, vec3_t max_world_aabb) {
    vec3_t ext = max_world_aabb - min_world_aabb;
    vec3_t off = min_world_aabb;
    return {
        .col = {
            {1.0f / ext.x, 0, 0, 0},
            {0, 1.0f / ext.y, 0, 0},
            {0, 0, 1.0f / ext.z, 0},
            {-off.x, -off.y, -off.z, 1},
        }
    };
}

mat4_t compute_model_to_texture_matrix(int dim_x, int dim_y, int dim_z) {
    (void)dim_x;
    (void)dim_y;
    (void)dim_z;
    return mat4_ident();
}

mat4_t compute_texture_to_model_matrix(int dim_x, int dim_y, int dim_z) {
    (void)dim_x;
    (void)dim_y;
    (void)dim_z;
    return mat4_ident();
}

void render_volume(const RenderDesc& desc) {
    if (!desc.direct_volume_rendering_enabled && !desc.isosurface_enabled && !desc.bounding_box.enabled) return;

    GLint bound_fbo;
    GLint bound_viewport[4];
    GLint bound_draw_buffer[8] = {0};
    GLint bound_draw_buffer_count = 0;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &bound_fbo);
    glGetIntegerv(GL_VIEWPORT, bound_viewport);
    for (int i = 0; i < 8; ++i) {
        glGetIntegerv(GL_DRAW_BUFFER0 + i, &bound_draw_buffer[i]);
        // @NOTE: Assume that its tightly packed and if we stumple upon a zero draw buffer index, we enterpret that as the 'end'
        if (bound_draw_buffer[i] == GL_NONE) {
            bound_draw_buffer_count = i;
            break;
        }
    }

    const mat4_t model_to_view_matrix = mat4_mul(desc.matrix.view, desc.matrix.model);

    static float time = 0.0f;
    time += 1.0f / 60.0f;
    if (time > 100.0) time -= 100.0f;

    if (desc.render_target.texture) {
        ASSERT(glIsTexture(desc.render_target.texture));
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, gl.fbo);
        // We don't need a depth buffer to render the volume
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, desc.render_target.texture, 0);
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glViewport(0, 0, desc.render_target.width, desc.render_target.height);
    }

    UniformData data;
    data.view_to_model_mat = mat4_inverse(model_to_view_matrix);
    data.model_to_view_mat = model_to_view_matrix;
    data.inv_proj_mat = mat4_inverse(desc.matrix.proj);
    data.model_view_proj_mat = desc.matrix.proj * model_to_view_matrix;
    data.inv_res = {1.f / (float)(desc.render_target.width), 1.f / (float)(desc.render_target.height)};
    data.density_scale = desc.global_scaling.density;
    data.alpha_scale = desc.global_scaling.alpha;
    data.clip_volume_min = desc.clip_volume.min;
    data.clip_volume_max = desc.clip_volume.max;
    data.time = time;
    data.gradient_spacing_world_space = desc.voxel_spacing;
    data.gradient_spacing_tex_space = data.view_to_model_mat * mat4_scale(desc.voxel_spacing.x, desc.voxel_spacing.y, desc.voxel_spacing.z);

    glBindBuffer(GL_UNIFORM_BUFFER, gl.ubo);
    glBufferSubData(GL_UNIFORM_BUFFER, 0, sizeof(UniformData), &data);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, desc.texture.depth);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_3D, desc.texture.volume);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, desc.texture.transfer_function);

    glBindBufferBase(GL_UNIFORM_BUFFER, 0, gl.ubo);

    const GLuint program =
        desc.direct_volume_rendering_enabled ? (desc.isosurface_enabled ? gl.program.dvr_and_iso : gl.program.dvr_only) : gl.program.iso_only;

    const GLint uniform_loc_tex_depth = glGetUniformLocation(program, "u_tex_depth");
    const GLint uniform_loc_tex_volume = glGetUniformLocation(program, "u_tex_volume");
    const GLint uniform_loc_tex_tf = glGetUniformLocation(program, "u_tex_tf");
    const GLint uniform_block_index = glGetUniformBlockIndex(program, "UniformData");
    const GLint uniform_loc_iso_values = glGetUniformLocation(program, "u_iso.values");
    const GLint uniform_loc_iso_colors = glGetUniformLocation(program, "u_iso.colors");
    const GLint uniform_loc_iso_count = glGetUniformLocation(program, "u_iso.count");

    glUseProgram(program);

    int    iso_count = CLAMP(desc.iso_surface.count, 0, 8);
    float  iso_values[8];
    vec4_t iso_colors[8];

    memcpy(iso_values, desc.iso_surface.values, iso_count * sizeof(float));
    memcpy(iso_colors, desc.iso_surface.colors, iso_count * sizeof(vec4_t));

    for (int i = 0; i < iso_count - 1; ++i) {
        for (int j = i + 1; j < iso_count; ++j) {
            if (iso_values[j] < iso_values[i]) {
                float  val_tmp = iso_values[i];
                vec4_t col_tmp = iso_colors[i];
                iso_values[i] = iso_values[j];
                iso_colors[i] = iso_colors[j];
                iso_values[j] = val_tmp;
                iso_colors[j] = col_tmp;
            }
        }
    }

    glUniform1i(uniform_loc_tex_depth, 0);
    glUniform1i(uniform_loc_tex_volume, 1);
    glUniform1i(uniform_loc_tex_tf, 2);
    glUniform1fv(uniform_loc_iso_values, iso_count, (const float*)iso_values);
    glUniform4fv(uniform_loc_iso_colors, iso_count, (const float*)iso_colors);
    glUniform1i(uniform_loc_iso_count, iso_count);
    glUniformBlockBinding(program, uniform_block_index, 0);

    glBindVertexArray(gl.vao);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 42);
    glBindVertexArray(0);

    glUseProgram(0);

    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, bound_fbo);
    glViewport(bound_viewport[0], bound_viewport[1], bound_viewport[2], bound_viewport[3]);
    glDrawBuffers(bound_draw_buffer_count, (GLenum*)bound_draw_buffer);

}

}  // namespace volume
