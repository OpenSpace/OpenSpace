#include "conetracing_utils.h"

#include <core/md_allocator.h>
#include <core/md_log.h>
#include <core/md_vec_math.h>
#include <core/md_sync.h>

#include "gl_utils.h"
#include "immediate_draw_utils.h"

#include "../color_utils.h"
// #include "task_system.h"

#include <atomic>


#define VIAMD_SHADER_DIR "shaders"

namespace cone_trace {

static GLuint vao = 0;
static GLuint vbo = 0;

static str_t v_shader_fs_quad_src = MAKE_STR(R"(
#version 150 core

out vec2 uv;

void main() {
	uint idx = uint(gl_VertexID) % 3U;
	gl_Position = vec4(
		(float( idx     &1U)) * 4.0 - 1.0,
		(float((idx>>1U)&1U)) * 4.0 - 1.0,
		0, 1.0);
	uv = gl_Position.xy * 0.5 + 0.5;
}
)");

namespace voxelize {
static struct {
    GLuint program = 0;
    GLuint vao = 0;

    struct {
        GLint volume_dim = -1;
        GLint volume_min = -1;
        GLint voxel_ext = -1;
        GLint tex_volume = -1;
    } uniform_location;
} gl;

static str_t v_shader_src = MAKE_STR(R"(
#version 430 core

uniform ivec3 u_volume_dim;
uniform vec3 u_volume_min;
uniform vec3 u_voxel_ext;
layout(binding=0, rgba8) uniform image3D uex_volume;

in vec4 sphere;
in vec4 color;

ivec3 compute_voxel_coord(vec3 coord) {
    return clamp(ivec3((coord - u_volume_min) / u_voxel_ext), ivec3(0), u_volume_dim - 1);
}

void main() {
	if (color.a == 0.0) discard;

	ivec3 coord = ivec3((sphere.xyz - u_volume_min) / u_voxel_ext);
	vec3 pos = sphere.xyz;
    float r2 = sphere.w * sphere.w;
    ivec3 min_cc = compute_voxel_coord(sphere.xyz - vec3(sphere.w));
    ivec3 max_cc = compute_voxel_coord(sphere.xyz + vec3(sphere.w));
    ivec3 cc;
    for (cc.z = min_cc.z; cc.z <= max_cc.z; cc.z++) {
        for (cc.y = min_cc.y; cc.y <= max_cc.y; cc.y++) {
            for (cc.x = min_cc.x; cc.x <= max_cc.x; cc.x++) {
                vec3 min_voxel = u_volume_min + vec3(cc) * u_voxel_ext;
                vec3 max_voxel = min_voxel + u_voxel_ext;
                vec3 clamped_pos = clamp(pos, min_voxel, max_voxel);
                vec3 d = clamped_pos - pos;

                if (dot(d, d) < r2) {
					imageStore(uex_volume, cc, color);
                }
            }
        }
    }
}
)");

static void initialize(int version_major, int version_minor) {
    if (!gl.program) {
        if (version_major >= 4 && version_minor >= 3) {
            GLuint v_shader = gl::compile_shader_from_source(v_shader_src, GL_VERTEX_SHADER);
            defer { glDeleteShader(v_shader); };
            if (v_shader) {
                gl.program = glCreateProgram();
                if (gl::attach_link_detach(gl.program, &v_shader, 1)) {
                    gl.uniform_location.volume_dim = glGetUniformLocation(gl.program, "u_volume_dim");
                    gl.uniform_location.volume_min = glGetUniformLocation(gl.program, "u_volume_min");
                    gl.uniform_location.voxel_ext = glGetUniformLocation(gl.program, "u_voxel_ext");
                    gl.uniform_location.tex_volume = glGetUniformLocation(gl.program, "u_tex_volume");
                }
            }
        } else {
            md_print(MD_LOG_TYPE_INFO, "Sphere binning shader requires OpenGL 4.3");
        }
    }

    if (!gl.vao) glGenVertexArrays(1, &gl.vao);
}

static void shutdown() {
    if (gl.program) {
        glDeleteProgram(gl.program);
        gl.program = 0;
    }

    if (gl.vao) {
        glDeleteVertexArrays(1, &gl.vao);
        gl.vao = 0;
    }
}

}  // namespace voxelize

namespace cone_trace {

static struct {
    GLuint program = 0;

    struct {
        GLint depth_tex = -1;
        GLint normal_tex = -1;
        GLint color_alpha_tex = -1;
        GLint f0_smoothness_tex = -1;
        GLint voxel_tex = -1;
        GLint voxel_grid_min = -1;
        GLint voxel_grid_size = -1;
        GLint voxel_dimensions = -1;
        GLint voxel_extent = -1;
        GLint indirect_diffuse_scale = -1;
        GLint indirect_specular_scale = -1;
        GLint ambient_occlusion_scale = -1;
        GLint cone_angle = -1;
        GLint inv_view_mat = -1;
        GLint inv_view_proj_mat = -1;
        GLint world_space_camera = -1;
    } uniform_location;

} gl;

// Modified version of
// https://github.com/Cigg/Voxel-Cone-Tracing

static str_t f_shader_src = MAKE_STR(R"(
#version 330 core

in vec2 uv;
out vec4 frag_color;

// Textures
uniform sampler2D u_depthexture;
uniform sampler2D u_normalexture;
uniform sampler2D u_color_alphaexture;
uniform sampler2D u_f0_smoothnessexture;

// Voxel stuff
uniform sampler3D u_voxelexture;
uniform vec3 u_voxel_grid_world_size;
uniform vec3 u_voxel_grid_world_min;
uniform ivec3 u_voxel_dimensions;
uniform float u_voxel_extent;

// Scaling factors
uniform float u_indirect_diffuse_scale = 1.f;
uniform float u_indirect_specular_scale = 1.f;
uniform float u_ambient_occlusion_scale = 1.f;
uniform float u_cone_angle = 0.08;

// View parameters
uniform mat4 u_inv_view_mat;
uniform mat4 u_inv_view_proj_mat;
uniform vec3 u_world_space_camera;

const float MAX_DIST = 1000.0;
const float ALPHAHRESH = 0.95;

// 6 60 degree cone
const int NUM_CONES = 6;
const vec3 cone_directions[6] = vec3[]
(                            vec3(0, 1, 0),
                            vec3(0, 0.5, 0.866025),
                            vec3(0.823639, 0.5, 0.267617),
                            vec3(0.509037, 0.5, -0.700629),
                            vec3(-0.509037, 0.5, -0.700629),
                            vec3(-0.823639, 0.5, 0.267617)
                            );
const float cone_weights[6] = float[](0.25, 0.15, 0.15, 0.15, 0.15, 0.15);

// // 5 90 degree cones
// const int NUM_CONES = 5;
// vec3 coneDirections[5] = vec3[]
// (                            vec3(0, 1, 0),
//                             vec3(0, 0.707, 0.707),
//                             vec3(0, 0.707, -0.707),
//                             vec3(0.707, 0.707, 0),
//                             vec3(-0.707, 0.707, 0)
//                             );
// float coneWeights[5] = float[](0.28, 0.18, 0.18, 0.18, 0.18);

vec4 sample_voxels(vec3 world_position, float lod) {
    vec3 tc = (world_position - u_voxel_grid_world_min) / (u_voxel_grid_world_size);
    //vec3 d = tc - vec3(0.5);
    //if (dot(d,d) > 1) return vec4(0,0,0,1);
	//tc = tc + vec3(0.5) / vec3(u_voxel_dimensions);
    return textureLod(u_voxelexture, tc, lod);
}

// Third argument to say how long between steps?
vec4 conerace(vec3 world_position, vec3 world_normal, vec3 direction, float tan_half_angle, out float occlusion) {
    
    // lod level 0 mipmap is full size, level 1 is half that size and so on
    float lod = 0.0;
    vec4 rgba = vec4(0);
    occlusion = 0.0;

    float dist = u_voxel_extent; // Start one voxel away to avoid self occlusion
    vec3 start_pos = world_position + world_normal * u_voxel_extent; // Plus move away slightly in the normal direction to avoid
                                                                     // self occlusion in flat surfaces

    while(dist < MAX_DIST && rgba.a < ALPHAHRESH) {
        // smallest sample diameter possible is the voxel size
        float diameter = max(u_voxel_extent, 2.0 * tan_half_angle * dist);
        float lod_level = log2(diameter / u_voxel_extent);
        vec4 voxel_color = sample_voxels(start_pos + dist * direction, lod_level);
		//if (voxel_color.a < 0) break;

        // front-to-back compositing
        float a = (1.0 - rgba.a);
        rgba += a * voxel_color;
        occlusion += (a * voxel_color.a) / (1.0 + 0.03 * diameter);

        //dist += diameter * 0.5; // smoother
        dist += diameter; // faster but misses more voxels
    }

    return rgba;
}

vec4 indirect_light(in vec3 world_position, in vec3 world_normal, in mat3 tangento_world, out float occlusion_out) {
    vec4 color = vec4(0);
    occlusion_out = 0.0;

    for(int i = 0; i < NUM_CONES; i++) {
        float occlusion = 0.0;
        // 60 degree cones -> tan(30) = 0.577
        // 90 degree cones -> tan(45) = 1.0
        color += cone_weights[i] * conerace(world_position, world_normal, tangento_world * cone_directions[i], 0.577, occlusion);
        occlusion_out += cone_weights[i] * occlusion;
    }

    occlusion_out = 1.0 - occlusion_out;

    return color;
}

mat3 compute_ON_basis(in vec3 v1) {
    vec3 v0;
    vec3 v2;
    float d0 = dot(vec3(1,0,0), v1);
    float d1 = dot(vec3(0,0,1), v1);
    if (d0 < d1) {
        v0 = normalize(vec3(1,0,0) - v1 * d0);
    } else {
        v0 = normalize(vec3(0,0,1) - v1 * d1);
    }
    v2 = cross(v0, v1);
    return mat3(v0, v1, v2);
}

vec4 deptho_world_coord(vec2 tex_coord, float depth) {
    vec4 clip_coord = vec4(vec3(tex_coord, depth) * 2.0 - 1.0, 1.0);
    vec4 world_coord = u_inv_view_proj_mat * clip_coord;
    return world_coord / world_coord.w;
}

// https://aras-p.info/texts/CompactNormalStorage.html
vec3 decode_normal(vec2 enc) {
    vec2 fenc = enc*4-2;
    float f = dot(fenc,fenc);
    float g = sqrt(1-f/4.0);
    vec3 n;
    n.xy = fenc*g;
    n.z = 1-f/2.0;
    return n;
}

vec3 fresnel(vec3 f0, float H_dot_V) {
    //const float n1 = 1.0;
    //const float n2 = 1.5;
    //const float R0 = pow((n1-n2)/(n1+n2), 2);
    return f0 + (1.0 - f0) * pow(1.0 - H_dot_V, 5.0);
}

vec3 shade(vec3 albedo, float alpha, vec3 f0, float smoothness, vec3 P, vec3 V, vec3 N) {
	const float PI_QUARTER = 3.14159265 * 0.25;
    const vec3 env_radiance = vec3(0.5);
    const vec3 dir_radiance = vec3(0.5);
    const vec3 L = normalize(vec3(1));
    const float spec_exp = 10.0;

    mat3 tangento_world = compute_ON_basis(N);

    float N_dot_V = max(0.0, -dot(N, V));
    vec3 R = -V + 2.0 * dot(N, V) * N;
    vec3 H = normalize(L + V);
    float H_dot_V = max(0.0, dot(H, V));
    float N_dot_H = max(0.0, dot(N, H));
    float N_dot_L = max(0.0, dot(N, L));
    vec3 fresnel_direct = fresnel(f0, H_dot_V);
    vec3 fresnel_indirect = fresnel(f0, N_dot_V);
	float tan_half_angle = tan(mix(PI_QUARTER, 0.0, smoothness));

    float diffuse_occlusion;
    vec3 direct_diffuse = env_radiance + N_dot_L * dir_radiance;
    vec3 indirect_diffuse = indirect_light(P, N, tangento_world, diffuse_occlusion).rgb;

    float transmissive_occlusion;
    vec3 transmissive = vec3(0);
    //alpha = 0.1;
    //if (alpha < 1.0) {
    //    transmissive = conerace(P, N, -V, tan_half_angle, transmissive_occlusion).rgb;
    //    transmissive *= 1.0 - alpha * albedo;
    //    direct_diffuse *= alpha;
    //    indirect_diffuse *= alpha;
    //}

    float specular_occlusion;
    vec3 direct_specular = dir_radiance * pow(N_dot_H, spec_exp);
    vec3 indirect_specular = conerace(P, N, R, tan_half_angle, specular_occlusion).rgb;

	vec3 result = vec3(0);
	result += albedo * (direct_diffuse + u_indirect_diffuse_scale * indirect_diffuse) * pow(diffuse_occlusion, u_ambient_occlusion_scale * 0.5);
	result += direct_specular * fresnel_direct + u_indirect_specular_scale * indirect_specular * fresnel_indirect;
    result += transmissive;

    return result;
}

void main() {
    float depth = texture(u_depthexture, uv).r;
	if (depth == 1.0) discard;

    vec2 encoded_normal = texture(u_normalexture, uv).rg;
	vec4 albedo_alpha = texture(u_color_alphaexture, uv);
	vec4 f0_smoothness = texture(u_f0_smoothnessexture, uv);
	vec3 albedo = albedo_alpha.rgb;
	float alpha = albedo_alpha.a;
	vec3 f0 = f0_smoothness.rgb;
	float smoothness = f0_smoothness.a;

    vec3 world_position = deptho_world_coord(uv, depth).xyz;
    vec3 world_normal = mat3(u_inv_view_mat) * decode_normal(encoded_normal);
    vec3 world_eye = u_world_space_camera;

	vec3 P = world_position;
	vec3 V = normalize(world_eye - world_position);
	vec3 N = world_normal;

    frag_color = vec4(shade(albedo, alpha, f0, smoothness, P, V, N), 1);
}
)");

static void initialize() {
    GLuint v_shader = gl::compile_shader_from_source(v_shader_fs_quad_src, GL_VERTEX_SHADER);
    GLuint f_shader = gl::compile_shader_from_source(f_shader_src, GL_FRAGMENT_SHADER);
    defer{
        glDeleteShader(v_shader);
        glDeleteShader(f_shader);
    };
    
    if (v_shader && f_shader) {
        const GLuint shaders[] = {v_shader, f_shader};
        gl.program = glCreateProgram();
        if (gl::attach_link_detach(gl.program, shaders, 2)) {
            gl.uniform_location.depth_tex = glGetUniformLocation(gl.program, "u_depth_texture");
            gl.uniform_location.normal_tex = glGetUniformLocation(gl.program, "u_normal_texture");
            gl.uniform_location.color_alpha_tex = glGetUniformLocation(gl.program, "u_color_alpha_texture");
            gl.uniform_location.f0_smoothness_tex = glGetUniformLocation(gl.program, "u_f0_smoothness_texture");
            gl.uniform_location.voxel_tex = glGetUniformLocation(gl.program, "u_voxel_texture");
            gl.uniform_location.voxel_grid_min = glGetUniformLocation(gl.program, "u_voxel_grid_world_min");
            gl.uniform_location.voxel_grid_size = glGetUniformLocation(gl.program, "u_voxel_grid_world_size");
            gl.uniform_location.voxel_dimensions = glGetUniformLocation(gl.program, "u_voxel_dimensions");
            gl.uniform_location.voxel_extent = glGetUniformLocation(gl.program, "u_voxel_extent");
            gl.uniform_location.indirect_diffuse_scale = glGetUniformLocation(gl.program, "u_indirect_diffuse_scale");
            gl.uniform_location.indirect_specular_scale = glGetUniformLocation(gl.program, "u_indirect_specular_scale");
            gl.uniform_location.ambient_occlusion_scale = glGetUniformLocation(gl.program, "u_ambient_occlusion_scale");
            gl.uniform_location.cone_angle = glGetUniformLocation(gl.program, "u_cone_angle");
            gl.uniform_location.inv_view_mat = glGetUniformLocation(gl.program, "u_inv_view_mat");
            gl.uniform_location.inv_view_proj_mat = glGetUniformLocation(gl.program, "u_inv_view_proj_mat");
            gl.uniform_location.world_space_camera = glGetUniformLocation(gl.program, "u_world_space_camera");
        }
    }
}

static void shutdown() {
    if (gl.program) {
        glDeleteProgram(gl.program);
        gl.program = 0;
    }
}

}  // namespace cone_trace

namespace directional_occlusion {
static GLuint program = 0;

void initialize() {
    if (!program) {
        program = glCreateProgram();
    }
    GLuint v_shader = gl::compile_shader_from_source(v_shader_fs_quad_src, GL_VERTEX_SHADER);
    GLuint f_shader = gl::compile_shader_from_file(MAKE_STR(VIAMD_SHADER_DIR"/cone_tracing/directional_occlusion.frag"), GL_FRAGMENT_SHADER);
    defer {
        glDeleteShader(v_shader);
        glDeleteShader(f_shader);
    };

    const GLuint shaders[] = {v_shader, f_shader};
    gl::attach_link_detach(program, shaders, ARRAY_SIZE(shaders));
}

void shutdown() {
    if (program) glDeleteProgram(program);
}
}  // namespace directional_occlusion

void initialize(int version_major, int version_minor) {
    if (!vao) glGenVertexArrays(1, &vao);
    if (!vbo) glGenBuffers(1, &vbo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, 12, nullptr, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (const GLvoid*)0);
    glBindVertexArray(0);

    cone_trace::initialize();
    voxelize::initialize(version_major, version_minor);
    directional_occlusion::initialize();
}

void shutdown() {
    cone_trace::shutdown();
    voxelize::shutdown();
    directional_occlusion::shutdown();
}

void init_rgba_volume(GPUVolume* vol, int res_x, int res_y, int res_z, vec3_t min_box, vec3_t max_box) {
    ASSERT(vol);
    if (!vol->texture_id) glGenTextures(1, &vol->texture_id);
    vol->min_box = min_box;
    vol->max_box = max_box;
    vol->voxel_ext = (max_box - min_box) / vec3_t{(float)res_x, (float)res_y, (float)res_z};

    if (res_x != vol->res_x || res_y != vol->res_y || res_z != vol->res_z) {
        vol->res_x = res_x;
        vol->res_y = res_y;
        vol->res_z = res_z;
        glBindTexture(GL_TEXTURE_3D, vol->texture_id);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
        // glTexStorage3D(GL_TEXTURE_3D, 4, GL_RGBA8, res.x, res.y, res.z);
        glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, res_x, res_y, res_z, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
        glBindTexture(GL_TEXTURE_3D, 0);
    }
}

uint32_t floor_power_of_two(uint32_t v) {
    uint32_t r = 0;
    while (v >>= 1) {
        r++;
    }
    return 1 << r;
}

uint32_t ceil_power_of_two(uint32_t x) {
    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    x++;
    return x;
}

void init_occlusion_volume(GPUVolume* vol, vec3_t min_box, vec3_t max_box, float voxel_ext_target) {
    ASSERT(vol);

    if (min_box == max_box) {
        return;
    }

    const float voxel_ext = voxel_ext_target;
    const vec3_t ext = (max_box - min_box);
    const float max_ext = MAX(ext.x, MAX(ext.y, ext.z));
    int32_t dim = (int32_t)CLAMP(ceil_power_of_two((uint32_t)(max_ext / voxel_ext_target)), 1, 256);
    int res[3] = {dim, dim, dim};

    for (int i = 0; i < 3; i++) {
        float half_ext = max_ext * 0.5f;
        while (ext[i] < half_ext && dim > 1) {
            res[i] /= 2;
            half_ext *= 0.5f;
        }
    }

    const vec3_t center = (min_box + max_box) * 0.5f;
    const vec3_t half_ext = vec3_t{(float)res[0], (float)res[1], (float)res[2]} * voxel_ext * 0.5f;
    min_box = center - half_ext;
    max_box = center + half_ext;

    vol->min_box = min_box;
    vol->max_box = max_box;
    vol->voxel_ext = vec3_t{voxel_ext, voxel_ext, voxel_ext};

    if (res[0] != vol->res_x || res[1] != vol->res_y || res[2] != vol->res_z) {
        // @NOTE: Compute log2 (integer version) to find the amount of mipmaps required
        int mips = 1;
        {
            int max_dim = MAX(res[0], MAX(res[1], res[2]));
            while (max_dim >>= 1) ++mips;
        }
        if (vol->texture_id) glDeleteTextures(1, &vol->texture_id);
        glGenTextures(1, &vol->texture_id);
        glBindTexture(GL_TEXTURE_3D, vol->texture_id);
        glTexStorage3D(GL_TEXTURE_3D, mips, GL_R8, res[0], res[1], res[2]);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_BASE_LEVEL, 0);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAX_LEVEL, mips - 1);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
        glBindTexture(GL_TEXTURE_3D, 0);
        vol->res_x = res[0];
        vol->res_y = res[1];
        vol->res_z = res[2];
    }
}

void free_volume(GPUVolume* vol) {
    if (vol->texture_id) {
        glDeleteTextures(1, &vol->texture_id);
        vol->texture_id = 0;
    }
}

struct ivec3_t {
    int x, y, z;
};

inline ivec3_t compute_voxel_coord(const GPUVolume& data, const vec3_t& coord) {
    ivec3_t c;
    c.x = CLAMP((int)((coord.x - data.min_box.x) / data.voxel_ext.x), 0, data.res_x - 1);
    c.y = CLAMP((int)((coord.y - data.min_box.y) / data.voxel_ext.y), 0, data.res_y - 1);
    c.z = CLAMP((int)((coord.z - data.min_box.z) / data.voxel_ext.z), 0, data.res_z - 1);
    return c;
}

inline int compute_voxel_idx(const ivec3_t& res, const ivec3_t& coord) { return coord.z * res.x * res.y + coord.y * res.x + coord.x; }

inline int compute_voxel_idx(const GPUVolume& data, const vec3_t& coord) {
    return compute_voxel_idx({data.res_x, data.res_y, data.res_z}, compute_voxel_coord(data, coord));
}

inline uint32_t accumulate_voxel_color(uint32_t current_color, uint32_t new_color, float counter) {
    vec4_t c = convert_color(current_color);
    vec4_t n = convert_color(new_color);
    c = (counter * c + n) / (counter + 1.f);
    return convert_color(c);
}

void voxelize_spheres_cpu(const GPUVolume& vol, const float* x, const float* y, const float* z, const float* r, const uint32_t* colors, int64_t count) {
    const int32_t voxel_count = vol.res_x * vol.res_y * vol.res_z;
    if (voxel_count == 0) {
        md_print(MD_LOG_TYPE_ERROR, "Volume resolution is zero on one or more axes.");
        return;
    }

    uint32_t* voxel_data = (uint32_t*)md_alloc(default_allocator, voxel_count * sizeof(uint32_t));
    float* voxel_counter = (float*)md_alloc(default_allocator, voxel_count * sizeof(float));
    defer {
        md_free(default_allocator, voxel_counter, voxel_count * sizeof(float));
        md_free(default_allocator, voxel_data, voxel_count * sizeof(uint32_t));
    };

    ivec3_t vol_res = {vol.res_x, vol.res_y, vol.res_z};


    for (int64_t i = 0; i < count; i++) {
        vec3_t pos = {x[i], y[i], z[i]};
        float radius = r[i];
        uint32_t color = colors[i];
        const float r2 = radius * radius;
        ivec3_t min_cc = compute_voxel_coord(vol, pos - radius);
        ivec3_t max_cc = compute_voxel_coord(vol, pos + radius);
        ivec3_t cc;
        for (cc.z = min_cc.z; cc.z <= max_cc.z; cc.z++) {
            for (cc.y = min_cc.y; cc.y <= max_cc.y; cc.y++) {
                for (cc.x = min_cc.x; cc.x <= max_cc.x; cc.x++) {
                    vec3_t min_voxel = vol.min_box + vec3_t{(float)cc.x, (float)cc.y, (float)cc.z} * vol.voxel_ext;
                    vec3_t max_voxel = min_voxel + vol.voxel_ext;
                    vec3_t clamped_pos = vec3_clamp(pos, min_voxel, max_voxel);
                    vec3_t d = clamped_pos - pos;

                    if (vec3_dot(d, d) < r2) {
                        int voxel_idx = compute_voxel_idx(vol_res, cc);
                        voxel_data[voxel_idx] = accumulate_voxel_color(voxel_data[voxel_idx], color, voxel_counter[voxel_idx] + 1);
                        voxel_counter[voxel_idx] += 1;
                    }
                }
            }
        }
    }

    // Apply crude lambert illumination model
    for (int32_t i = 0; i < voxel_count; ++i) {
        vec4_t c = convert_color(voxel_data[i]);
        voxel_data[i] = convert_color(vec4_from_vec3(vec3_from_vec4(c) / 3.1415926535f, c.w));
    }

    glBindTexture(GL_TEXTURE_3D, vol.texture_id);
    glTexSubImage3D(GL_TEXTURE_3D, 0, 0, 0, 0, vol.res_x, vol.res_y, vol.res_z, GL_RGBA, GL_UNSIGNED_BYTE, voxel_data);
    glGenerateMipmap(GL_TEXTURE_3D);
    glBindTexture(GL_TEXTURE_3D, 0);
}

void voxelize_spheres_gpu(const GPUVolume& vol, GLuint position_radius_buffer, GLuint color_buffer, int32_t num_spheres) {
    if (!voxelize::gl.program) {
        md_print(MD_LOG_TYPE_ERROR, "sphere_binning program is not compiled");
        return;
    }

    glClearTexImage(vol.texture_id, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);

    glBindVertexArray(voxelize::gl.vao);
    glBindBuffer(GL_ARRAY_BUFFER, position_radius_buffer);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(vec4_t), nullptr);
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
    glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(uint32_t), nullptr);
    glEnableVertexAttribArray(1);

    glUseProgram(voxelize::gl.program);
    glUniform3i(voxelize::gl.uniform_location.volume_dim, vol.res_x, vol.res_y, vol.res_z);
    glUniform3fv(voxelize::gl.uniform_location.volume_min, 1, &vol.min_box[0]);
    glUniform3fv(voxelize::gl.uniform_location.voxel_ext, 1, &vol.voxel_ext[0]);
    glUniform1i(voxelize::gl.uniform_location.tex_volume, 0);
    glBindTexture(GL_TEXTURE_3D, vol.texture_id);
    glBindImageTexture(0, vol.texture_id, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA8);

    glColorMask(0, 0, 0, 0);
    glDepthMask(0);
    glEnable(GL_RASTERIZER_DISCARD);

    glDrawArrays(GL_POINTS, 0, num_spheres);

    // glMemoryBarrier(GL_ALL_BARRIER_BITS);

    // glGenerateMipmap(GL_TEXTURE_3D);

    glDisable(GL_RASTERIZER_DISCARD);
    glDepthMask(1);
    glColorMask(1, 1, 1, 1);

    glUseProgram(0);
    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

/*
enum IlluminationDirection { POSITIVE_X, NEGATIVE_X, POSITIVE_Y, NEGATIVE_Y, POSITIVE_Z, NEGATIVE_Z };

void illuminate_voxels_directional_constant(Array<vec3> light_voxels, Array<const uint32> rgba_voxels, const ivec3& voxel_dim,
                                            IlluminationDirection direction, const vec3& intensity) {
    const auto dim = cone_trace::volume.dim;
    const bool positive = direction % 2 == 0;

    ivec3_t step = {1, 1, 1};
    ivec3_t beg_idx = {0, 0, 0};
    ivec3_t end_idx = dim;

    switch (direction) {
        case POSITIVE_X:
        case NEGATIVE_X: {
            if (positive) {
                beg_idx.x = 1;
            } else {
                step.x = -1;
                beg_idx.x = dim.x - 2;
                end_idx.x = -1;
            }

            for (int32 z = beg_idx.z; z != end_idx.z; z += step.z) {
                for (int32 y = beg_idx.y; y != end_idx.y; y += step.y) {
                    light_voxels[z * dim.y * dim.x + y * dim.x + (beg_idx.x - step.x)] += intensity;
                }
            }

            for (int32 x = beg_idx.x; x != end_idx.x; x += step.x) {
                for (int32 z = beg_idx.z; z != end_idx.z; z += step.z) {
                    for (int32 y = beg_idx.y; y != end_idx.y; y += step.y) {
                        const int32 src_vol_idx = z * dim.y * dim.x + y * dim.x + (x - step.x);
                        const int32 dst_vol_idx = z * dim.y * dim.x + y * dim.x + x;
                        const vec4_t voxel_rgba = math::convert_color(rgba_voxels[src_vol_idx]);
                        light_voxels[dst_vol_idx] += (1.f - voxel_rgba.a) * light_voxels[src_vol_idx];
                    }
                }
            }
        } break;
        case POSITIVE_Y:
        case NEGATIVE_Y: {
            if (positive) {
                beg_idx.y = 1;
            } else {
                step.y = -1;
                beg_idx.y = dim.y - 2;
                end_idx.y = -1;
            }

            for (int32 z = beg_idx.z; z != end_idx.z; z += step.z) {
                for (int32 x = beg_idx.x; x != end_idx.x; x += step.x) {
                    light_voxels[z * dim.y * dim.x + (beg_idx.y - step.y) * dim.x + x] += intensity;
                }
            }

            for (int32 y = beg_idx.y; y != end_idx.y; y += step.y) {
                for (int32 z = beg_idx.z; z != end_idx.z; z += step.z) {
                    for (int32 x = beg_idx.x; x != end_idx.x; x += step.x) {
                        const int32 src_vol_idx = z * dim.y * dim.x + (y - step.y) * dim.x + x;
                        const int32 dst_vol_idx = z * dim.y * dim.x + y * dim.x + x;
                        const vec4_t voxel_rgba = math::convert_color(rgba_voxels[src_vol_idx]);
                        light_voxels[dst_vol_idx] += (1.f - voxel_rgba.a) * light_voxels[src_vol_idx];
                    }
                }
            }
        } break;
        case POSITIVE_Z:
        case NEGATIVE_Z: {
            if (positive) {
                beg_idx.z = 1;
            } else {
                step.z = -1;
                beg_idx.z = dim.z - 2;
                end_idx.z = -1;
            }

            for (int32 y = beg_idx.y; y != end_idx.y; y += step.y) {
                for (int32 x = beg_idx.x; x != end_idx.x; x += step.x) {
                    light_voxels[(beg_idx.z - step.z) * dim.y * dim.x + y * dim.x + x] += intensity;
                }
            }

            for (int32 z = beg_idx.z; z != end_idx.z; z += step.z) {
                for (int32 y = beg_idx.y; y != end_idx.y; y += step.y) {
                    for (int32 x = beg_idx.x; x != end_idx.x; x += step.x) {
                        const int32 src_vol_idx = (z - step.z) * dim.y * dim.x + y * dim.x + x;
                        const int32 dst_vol_idx = z * dim.y * dim.x + y * dim.x + x;
                        const vec4_t voxel_rgba = math::convert_color(rgba_voxels[src_vol_idx]);
                        light_voxels[dst_vol_idx] += (1.f - voxel_rgba.a) * light_voxels[src_vol_idx];
                    }
                }
            }
        } break;
    }
}

void illuminate_voxels_omnidirectional_constant(const vec3& intensity) {
    auto dim = cone_trace::volume.dim;
    DynamicArray<vec3> light_vol(cone_trace::volume.voxel_data.size(), vec3_t(0));
    illuminate_voxels_directional_constant(light_vol, cone_trace::volume.voxel_data, dim, POSITIVE_X, intensity);
    illuminate_voxels_directional_constant(light_vol, cone_trace::volume.voxel_data, dim, NEGATIVE_X, intensity);

    for (int32 i = 0; i < light_vol.count; i++) {
        vec4_t voxel_rgba = math::convert_color(cone_trace::volume.voxel_data[i]);
        voxel_rgba *= vec4_t(light_vol[i], 1.f);
        cone_trace::volume.voxel_data[i] = math::convert_color(voxel_rgba);
    }
    // illuminate_voxels_directional_constant(NEGATIVE_X, intensity);
    // illuminate_voxels_directional_constant(POSITIVE_Y, intensity);
    // illuminate_voxels_directional_constant(NEGATIVE_Y, intensity);
    // illuminate_voxels_directional_constant(POSITIVE_Z, intensity);

    // illuminate_voxels_directional_constant(NEGATIVE_Z, intensity);

// X-direction sweep plane
    DynamicArray<vec4_t> plane_slice_zy[2] = {
            { cone_trace::volume.dim.y * cone_trace::volume.dim.z, vec4_t(intensity, 0) },
            { cone_trace::volume.dim.y * cone_trace::volume.dim.z, vec4_t(intensity, 0) }
    };

    int curr_plane = 0;
    int prev_plane = 1;

    const auto dim = cone_trace::volume.dim;
    Array<uint32> voxels = cone_trace::volume.voxel_data;

for (int32 x = 1; x < dim.x; x++) {
    for (int32 z = 0; z < dim.z; z++) {
                    for (int32 y = 0; y < dim.y; y++) {
                            const int32 plane_idx = z * dim.y + y;
                            const int32 src_vol_idx = z * dim.y * dim.x + y * dim.x + (x - 1);
                            const int32 dst_vol_idx = z * dim.y * dim.x + y * dim.x + x;
                            const vec4_t voxel_rgba = math::convert_color(voxels[src_vol_idx]);
                            const vec4_t& src_light_voxel = plane_slice_zy[prev_plane][plane_idx];
                            vec4_t& dst_light_voxel = plane_slice_zy[curr_plane][plane_idx];

                            dst_light_voxel = (1.f - voxel_rgba.a) * src_light_voxel;
                            vec4_t dst_rgba = math::convert_color(voxels[dst_vol_idx]);
                            dst_rgba = vec4_t(vec3_t(dst_rgba) * vec3_t(dst_light_voxel), dst_rgba.a);
                            voxels[dst_vol_idx] = math::convert_color(dst_rgba);
                            prev_plane = (prev_plane + 1) % 2;
                            curr_plane = (curr_plane + 1) % 2;
        }
    }
}

}


void draw_voxelized_scene(const GPUVolume& vol, const mat4_t& view_mat, const mat4_t& proj_mat) {
    immediate::set_model_view_matrix(view_mat);
    immediate::set_proj_matrix(proj_mat);
    immediate::set_material(immediate::MATERIAL_ROUGH_BLACK);

    for (int32 z = 0; z < vol.res_z; z++) {
        for (int32 y = 0; y < vol.res_y; y++) {
            for (int32 x = 0; x < vol.res_x; x++) {
                int32 i = compute_voxel_idx(volume.dim, ivec3_t(x, y, z));
                if (cone_trace::volume.voxel_data[i] > 0) {
                    vec3_t min_box = cone_trace::volume.min_box + vec3_t(x, y, z) * cone_trace::volume.voxel_ext;
                    vec3_t max_box = min_box + cone_trace::volume.voxel_ext;
                    immediate::draw_aabb_lines(min_box, max_box);
                }
            }
        }
    }

    immediate::flush();
}
*/

void compute_occupancy_volume(const GPUVolume& vol, const float* x, const float* y, const float* z, const float* rad, int64_t count) {
    const int32_t voxel_count = vol.res_x * vol.res_y * vol.res_z;
    if (voxel_count == 0) {
        md_print(MD_LOG_TYPE_ERROR, "Volume resolution is zero on one or more axes");
        return;
    }

    const float inv_voxel_volume = 1.0f / (vol.voxel_ext.x * vol.voxel_ext.y * vol.voxel_ext.z);
    std::atomic_uint32_t* voxel_data = (std::atomic_uint32_t*)md_alloc(default_allocator, voxel_count * sizeof(std::atomic_uint32_t));
    defer { md_free(default_allocator, voxel_data, voxel_count * sizeof(std::atomic_uint32_t)); };

#if 0
    task_system::ID id = task_system::pool_enqueue(
        "Computing occupancy", (uint32_t)count,
        [voxel_data, x, y, z, r, &vol, inv_voxel_volume](task_system::TaskSetRange range) {
            // We assume atom radius <<< voxel extent and just increment the bin
            constexpr float sphere_vol_scl = (4.0f / 3.0f) * 3.1415926535f;
            for (uint32_t i = range.beg; i < range.end; i++) {
                const int idx = compute_voxel_idx(vol, {x[i], y[i], z[i]});
                const float rad = r[i];
                const float sphere_vol = sphere_vol_scl * rad * rad * rad;
                const uint32_t occ = (uint32_t)(sphere_vol * inv_voxel_volume * 0xFFFFFFFFU);
                atomic_fetch_add(&voxel_data[idx], occ);
            }
        });
    task_system::wait_for_task(id);
#else 
    constexpr float sphere_vol_scl = (4.0f / 3.0f) * 3.1415926535f;
    for (int32_t i = 0; i < count; i++) {
        const vec3_t pos = {x[i], y[i], z[i]};
        const int idx = compute_voxel_idx(vol, pos);
        const float r = rad[i];
        const float sphere_vol = sphere_vol_scl * r * r * r;
        const float fract = sphere_vol * inv_voxel_volume;
        const uint32_t occ = uint32_t(fract * (float)0xFFFFFFFFU);
        voxel_data[idx] += occ;
    }
#endif

    glBindTexture(GL_TEXTURE_3D, vol.texture_id);
    glTexSubImage3D(GL_TEXTURE_3D, 0, 0, 0, 0, vol.res_x, vol.res_y, vol.res_z, GL_RED, GL_UNSIGNED_INT, voxel_data);
    glGenerateMipmap(GL_TEXTURE_3D);
    glBindTexture(GL_TEXTURE_3D, 0);
}

void render_directional_occlusion(GLuint depth_tex, GLuint normal_tex, const GPUVolume& vol, const mat4_t& view_mat, const mat4_t& proj_mat,
                                  float occlusion_scale, float step_scale) {
    const mat4_t inv_view_proj_mat = mat4_inverse(proj_mat * view_mat);
    const mat4_t inv_view_mat = mat4_inverse(view_mat);
    const vec3_t world_space_camera = mat4_mul_vec3(inv_view_mat, {0,0,0}, 1);
    const vec3_t voxel_grid_min = vol.min_box;
    const vec3_t voxel_grid_ext = vol.max_box - vol.min_box;
    float voxel_ext = MAX(MAX(vol.voxel_ext.x, vol.voxel_ext.y), vol.voxel_ext.z);

    GLuint program = directional_occlusion::program;
    glUseProgram(program);

    glUniform1i(glGetUniformLocation(program, "u_depth_texture"), 0);
    glUniform1i(glGetUniformLocation(program, "u_normal_texture"), 1);
    glUniform1i(glGetUniformLocation(program, "u_voxel_texture"), 2);

    glUniform3fv(glGetUniformLocation(program, "u_voxel_grid_world_min"), 1, &voxel_grid_min[0]);
    glUniform3fv(glGetUniformLocation(program, "u_voxel_grid_world_size"), 1, &voxel_grid_ext[0]);
    glUniform3i(glGetUniformLocation(program, "u_voxel_dimensions"), vol.res_x, vol.res_y, vol.res_z);
    glUniform1f(glGetUniformLocation(program, "u_voxel_extent"), voxel_ext);
    glUniform1f(glGetUniformLocation(program, "u_occlusion_scale"), occlusion_scale);
    glUniform1f(glGetUniformLocation(program, "u_step_scale"), step_scale);
    glUniformMatrix4fv(glGetUniformLocation(program, "u_inv_view_mat"), 1, GL_FALSE, &inv_view_mat[0][0]);
    glUniformMatrix4fv(glGetUniformLocation(program, "u_inv_view_proj_mat"), 1, GL_FALSE, &inv_view_proj_mat[0][0]);
    glUniform3fv(glGetUniformLocation(program, "u_world_space_camera"), 1, &world_space_camera[0]);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth_tex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normal_tex);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_3D, vol.texture_id);

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ZERO, GL_SRC_COLOR);
    glColorMask(1, 1, 1, 0);
    glDepthMask(GL_FALSE);

    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    glDisable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);
    glColorMask(1, 1, 1, 1);
    glDepthMask(GL_TRUE);
    glEnable(GL_DEPTH_TEST);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_3D, 0);
}

void cone_trace_scene(GLuint depth_tex, GLuint normal_tex, GLuint color_alpha_tex, GLuint f0_smoothness_tex, const GPUVolume& vol,
                      const mat4_t& view_mat, const mat4_t& proj_mat, float indirect_diffuse_scale, float indirect_specular_scale,
                      float ambient_occlusion_scale) {

    mat4_t inv_view_proj_mat = mat4_inverse(proj_mat * view_mat);
    mat4_t inv_view_mat = mat4_inverse(view_mat);
    vec3_t world_space_camera = mat4_mul_vec3(inv_view_mat, {0, 0, 0}, 1);
    // printf("cam: %.2f %.2f %.2f\n", world_space_camera.x, world_space_camera.y, world_space_camera.z);
    vec3_t voxel_grid_min = vol.min_box;
    vec3_t voxel_grid_ext = vol.max_box - vol.min_box;
    float voxel_ext = MAX(MAX(vol.voxel_ext.x, vol.voxel_ext.y), vol.voxel_ext.z);

    // const float cone_angle = 0.07;  // 0.2 = 22.6 degrees, 0.1 = 11.4 degrees, 0.07 = 8 degrees angle

    glUseProgram(cone_trace::gl.program);

    glUniform1i(cone_trace::gl.uniform_location.depth_tex, 0);
    glUniform1i(cone_trace::gl.uniform_location.normal_tex, 1);
    glUniform1i(cone_trace::gl.uniform_location.color_alpha_tex, 2);
    glUniform1i(cone_trace::gl.uniform_location.f0_smoothness_tex, 3);
    glUniform1i(cone_trace::gl.uniform_location.voxel_tex, 4);
    glUniform3fv(cone_trace::gl.uniform_location.voxel_grid_min, 1, &voxel_grid_min[0]);
    glUniform3fv(cone_trace::gl.uniform_location.voxel_grid_size, 1, &voxel_grid_ext[0]);
    glUniform3i(cone_trace::gl.uniform_location.voxel_dimensions, vol.res_x, vol.res_y, vol.res_z);
    glUniform1f(cone_trace::gl.uniform_location.voxel_extent, voxel_ext);
    glUniform1f(cone_trace::gl.uniform_location.indirect_diffuse_scale, indirect_diffuse_scale);
    glUniform1f(cone_trace::gl.uniform_location.indirect_specular_scale, indirect_specular_scale);
    glUniform1f(cone_trace::gl.uniform_location.ambient_occlusion_scale, ambient_occlusion_scale);
    glUniformMatrix4fv(cone_trace::gl.uniform_location.inv_view_mat, 1, GL_FALSE, &inv_view_mat[0][0]);
    glUniformMatrix4fv(cone_trace::gl.uniform_location.inv_view_proj_mat, 1, GL_FALSE, &inv_view_proj_mat[0][0]);
    glUniform3fv(cone_trace::gl.uniform_location.world_space_camera, 1, &world_space_camera[0]);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth_tex);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normal_tex);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, color_alpha_tex);

    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, f0_smoothness_tex);

    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_3D, vol.texture_id);

    glDisable(GL_DEPTH_TEST);
    // glEnable(GL_BLEND);
    // glBlendFunc(GL_ONE, GL_ONE);
    // glColorMask(1, 1, 1, 0);
    glDepthMask(GL_FALSE);

    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    // glDisable(GL_BLEND);
    // glBlendFunc(GL_ONE, GL_ZERO);
    // glColorMask(1, 1, 1, 1);
    glDepthMask(GL_TRUE);
    glEnable(GL_DEPTH_TEST);
}

}  // namespace cone_trace
