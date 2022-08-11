#pragma once

#include <core/md_vec_math.h>
#include <core/md_str.h>

#include <gfx/gl.h>

namespace volume {

void initialize();
void shutdown();

mat4_t compute_model_to_world_matrix(vec3_t min_world_aabb, vec3_t max_world_aabb);
mat4_t compute_world_to_model_matrix(vec3_t min_world_aabb, vec3_t max_world_aabb);
mat4_t compute_texture_to_model_matrix(int dim_x, int dim_y, int dim_z);
mat4_t compute_model_to_texture_matrix(int dim_x, int dim_y, int dim_z);

bool write_volume_to_file(const float* data, int64_t dim_x, int64_t dim_y, int64_t dim_z, str_t path_to_file);

/*
    Renders a volumetric texture using OpenGL.
    - volume_texture: An OpenGL 3D texture containing the data
    - tf_texture:     An OpenGL 1D texture containing the transfer function
    - depth_texture:  An OpenGL 2D texture containing the depth data in the frame (for stopping ray traversal)
    - model_matrix:   Matrix containing model to world transformation of the volume, which is assumed to occupy a unit cube [0,1] in its model-space
    - view_matrix:    Matrix containing world to view transformation of the camera
    - proj_matrix:    Matrix containing view to clip transformation of the camera
    - density_scale:  global scaling of density
    - alpha_scale:    global alpha scaling of the transfer function
    - isosurface:     information on isovalues and associated colors
    - voxel_spacing:  spacing of voxels in world space
    - clip_planes:    define a subvolume (min, max)[0-1] which represents the visible portion of the volume
*/

struct RenderDesc {
    struct {
        GLuint texture;
        int width;
        int height;
    } render_target;

    struct {
        GLuint volume = 0;
        GLuint transfer_function = 0;
        GLuint depth = 0;
    } texture;

    struct {
        mat4_t model = {};
        mat4_t view = {};
        mat4_t proj = {};
    } matrix;

    struct {
        vec3_t min = {0, 0, 0};
        vec3_t max = {1, 1, 1};
    } clip_volume;

    struct {
        vec4_t color = {0, 0, 0, 1};
        bool enabled = true;
    } bounding_box;

    struct {
        float density = 1.0f;
        float alpha = 1.0f;
    } global_scaling;

    struct {
        int count;
        const float* values;
        const vec4_t* colors;
    } iso_surface;
    
    bool isosurface_enabled = false;
    bool direct_volume_rendering_enabled = true;

    vec3_t voxel_spacing = {};
};

void render_volume(const RenderDesc& desc);


}  // namespace volume
