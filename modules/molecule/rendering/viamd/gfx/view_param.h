#pragma once

#include <core/md_vec_math.h>

struct ViewParam {
    struct Block {
        mat4_t view;
        mat4_t proj;
        mat4_t proj_jittered;
        mat4_t view_proj;
        mat4_t view_proj_jittered;
        mat4_t norm;
    };

    struct {
        Block current;
        Block inverse;
        Block previous;
    } matrix;

    struct {
        vec2_t next;
        vec2_t current;
        vec2_t previous;
    } jitter;

    struct {
        float near;
        float far;
    } clip_planes;

    float fov_y;
    vec2_t resolution;
};
