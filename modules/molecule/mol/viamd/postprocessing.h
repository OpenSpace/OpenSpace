#pragma once

struct mat4_t;

namespace postprocessing {

void initialize(int width, int height);
void shutdown();

enum class Tonemapping {
    Passthrough,
    ExposureGamma,
    Filmic,
    ACES
};

struct Settings {
    struct {
        float r = 20.0f;
        float g = 20.0f;
        float b = 20.0f;
    } background_intensity;

    struct {
        bool enabled = true;
        float clip_point = 1.0f;
    } bloom;

    struct {
        bool enabled = true;
        Tonemapping mode = Tonemapping::Filmic;
        float exposure = 1.0f;
        float gamma = 2.2f;
    } tonemapping;

    struct {
        bool enabled = true;
        float radius = 6.0f;
        float intensity = 3.0f;
        float bias = 0.1f;
    } ambient_occlusion;

    struct {
        bool enabled = true;
        float focus_depth = 0.5f;
        float focus_scale = 10.f;
    } depth_of_field;

    struct {
        bool enabled = true;
        float feedback_min = 0.88f;
        float feedback_max = 0.97f;
        struct {
            bool enabled = true;
            float motion_scale = 1.f;
        } motion_blur;
    } temporal_reprojection;

    struct {
        unsigned int depth = 0;
        unsigned int color = 0;
        unsigned int normal = 0;
        unsigned int velocity = 0;
        unsigned int emissive = 0;
        unsigned int post_tonemap = 0;
    } input_textures;
};

void postprocess(const Settings& settings, const mat4_t& persp_matrix);

}  // namespace postprocessing
