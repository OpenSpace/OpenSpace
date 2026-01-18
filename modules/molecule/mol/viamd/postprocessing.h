/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_MODULE_MOLECULE___POSTPROCESSING___H__
#define __OPENSPACE_MODULE_MOLECULE___POSTPROCESSING___H__

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
        bool enabled = true;
        float r = 20.0f;
        float g = 20.0f;
        float b = 20.0f;
    } background;

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
        float horizon_bias = 0.1f;
        float normal_bias = 1.0f;
    } ambient_occlusion[2];

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
        bool enabled = true;
    } fxaa;

    struct {
        unsigned int depth = 0;
        unsigned int color = 0;
        unsigned int normal = 0;
        unsigned int velocity = 0;
        unsigned int emissive = 0;
        unsigned int post_tonemap = 0;
    } input_textures;
};

void postprocess(const Settings& settings, const mat4_t& view_mat, const mat4_t& proj_mat);

}  // namespace postprocessing

#endif // __OPENSPACE_MODULE_MOLECULE___POSTPROCESSING___H__
