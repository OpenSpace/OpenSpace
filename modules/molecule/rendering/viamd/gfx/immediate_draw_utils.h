#pragma once

#include <core/md_vec_math.h>

namespace immediate {

void initialize();
void shutdown();

void set_model_view_matrix(mat4_t model_view_mat);
void set_proj_matrix(mat4_t proj_mat);

void render();

constexpr uint32_t COLOR_WHITE = 0xffffffff;
constexpr uint32_t COLOR_BLACK = 0xff000000;
constexpr uint32_t COLOR_RED = 0xff0000ff;
constexpr uint32_t COLOR_GREEN = 0xff00ff00;
constexpr uint32_t COLOR_BLUE = 0xffff0000;
constexpr uint32_t COLOR_YELLOW = 0xff00ffff;
constexpr uint32_t COLOR_MAGENTA = 0xffff00ff;
constexpr uint32_t COLOR_CYAN = 0xffffff00;

constexpr uint32_t DEFAULT_COLOR = COLOR_WHITE;

// 3D Primitives
void draw_point(vec3_t pos, uint32_t color = DEFAULT_COLOR);
void draw_line(vec3_t from, vec3_t to, uint32_t color = DEFAULT_COLOR);
void draw_triangle(vec3_t v0, vec3_t v1, vec3_t v2, uint32_t color = DEFAULT_COLOR);

/*
         __________________
        /        ^(v)     /
       /        /        /
      /     (c). -----> /
     /              (u)/
    /_________________/

        Draws a plane given a center point and two support vectors.
*/
void draw_plane(vec3_t center, vec3_t plane_u, vec3_t plane_v, uint32_t color = DEFAULT_COLOR);
void draw_plane_wireframe(vec3_t center, vec3_t plane_u, vec3_t plane_v, uint32_t color = DEFAULT_COLOR, int segments_u = 4, int segments_v = 4);

// Composits
void draw_box_wireframe(vec3_t min_box, vec3_t max_box, uint32_t color = DEFAULT_COLOR);
void draw_box_wireframe(vec3_t min_box, vec3_t max_box, mat4_t model_matrix, uint32_t color = DEFAULT_COLOR);
void draw_basis(mat4_t basis, float scale = 1.f, uint32_t x_color = COLOR_RED, uint32_t y_color = COLOR_GREEN, uint32_t z_color = COLOR_BLUE);

// Advanced
// void draw_sphere_glyph(const float pos[3], const float radius, const uint32 color);
// void draw_capsule(const float v0[3], const float v1[3], const uint32 color);

}  // namespace immediate
