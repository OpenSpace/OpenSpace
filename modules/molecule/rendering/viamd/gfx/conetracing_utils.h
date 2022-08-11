#pragma once

#include "gl.h"

#include <core/md_vec_math.h>

namespace cone_trace {

struct GPUVolume {
    GLuint texture_id = 0;
	int res_x = 0;
	int res_y = 0;
	int res_z = 0;
	vec3_t min_box = { 0,0,0 };
	vec3_t max_box = { 0,0,0 };
	vec3_t voxel_ext = { 0,0,0 };
};


void initialize(int gl_version_major = 3, int gl_version_minor = 3);
void shutdown();

void init_rgba_volume(GPUVolume* vol, int res_x, int res_y, int res_z, vec3_t min_box, vec3_t max_box);
void init_occlusion_volume(GPUVolume* vol, vec3_t min_box, vec3_t max_box, float voxel_ext_target = 4.0f);
void free_volume(GPUVolume* vol);

void compute_occupancy_volume(const GPUVolume& vol, const float* x, const float* y, const float* z, const float* r, int64_t count);

void voxelize_spheres_cpu(const GPUVolume& vol, const float* x, const float* y, const float* z, const float* r, const uint32_t* color, int64_t count);
void voxelize_spheres_gpu(const GPUVolume& vol, GLuint position_radius_buffer, GLuint color_buffer, int64_t count);

void illuminate_voxels_omnidirectional_constant(const GPUVolume& vol, const vec3_t intensity);

void draw_voxels_scene(const GPUVolume& vol, const mat4_t& view_mat, const mat4_t& proj_mat);

void cone_trace_scene(GLuint depth_tex, GLuint normal_tex, GLuint color_alpha_tex, GLuint f0_smoothness_tex, const GPUVolume& vol, const mat4_t& view_mat,
                      const mat4_t& proj_mat, float indirect_diffuse_scale, float indirect_specular_scale, float ambient_occlusion_scale);

void render_directional_occlusion(GLuint depth_tex, GLuint normal_tex, const GPUVolume& vol, const mat4_t& view_mat, const mat4_t& proj_mat,
                                  float occlusion_scale, float step_scale);

}  // namespace render
