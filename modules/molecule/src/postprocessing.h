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

namespace ghoul::opengl { class Texture; }

namespace postprocessing {

void initialize(int width, int height);
void resize(int width, int height);
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
        float r = 20.f;
        float g = 20.f;
        float b = 20.f;
    } background;

    struct {
        bool enabled = true;
        float clipPoint = 1.f;
    } bloom;

    struct {
        bool enabled = true;
        Tonemapping mode = Tonemapping::Filmic;
        float exposure = 1.f;
        float gamma = 2.2f;
    } tonemapping;

    struct {
        bool enabled = true;
        float radius = 6.f;
        float intensity = 3.f;
        float horizonBias = 0.1f;
        float normalBias = 1.f;
    } ambientOcclusion[2];

    struct {
        bool enabled = true;
        float focusDepth = 0.5f;
        float focusScale = 10.f;
    } depthOfField;

    struct {
        bool enabled = true;
        float feedbackMin = 0.88f;
        float feedbackMax = 0.97f;
        struct {
            bool enabled = true;
            float motionScale = 1.f;
        } motionBlur;
    } temporalReprojection;

    struct {
        bool enabled = true;
    } fxaa;

    struct {
        ghoul::opengl::Texture* color = nullptr;
        ghoul::opengl::Texture* depth = nullptr;
        ghoul::opengl::Texture* normal = nullptr;
        ghoul::opengl::Texture* velocity = nullptr;
        ghoul::opengl::Texture* emissive = nullptr;
        ghoul::opengl::Texture* postTonemap = nullptr;
    } inputTextures;
};

void postprocess(const Settings& settings, const glm::mat4& viewMat,
    const glm::mat4& projMat);

} // namespace postprocessing

#endif // __OPENSPACE_MODULE_MOLECULE___POSTPROCESSING___H__
