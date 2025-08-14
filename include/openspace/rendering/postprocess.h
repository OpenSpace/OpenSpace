/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_RENDERING_POSTPROCESS___
#define __OPENSPACE_RENDERING_POSTPROCESS___

#include <ghoul/opengl/ghoul_gl.h>
#include <memory>

namespace ghoul::opengl {
    class ProgramObject;
}

namespace openspace {

class PostprocessingBloom {
public:
    PostprocessingBloom();
    ~PostprocessingBloom();

    void initialize();
    void deinitialize();
    void setResolution(int width, int height);
    void update();
    void render(GLuint inputTexture);

    void setEnabled(bool enabled) { _enabled = enabled; }
    bool isEnabled() const { return _enabled; }
    
    void setThreshold(float threshold) { _threshold = threshold; }
    void setBlurPasses(int passes) { _blurPasses = passes; }
    void setBlurMagnitude(float magnitude) { _blurMagnitude = magnitude; }
    void setIntensity(float intensity) { _intensity = intensity; }

private:
    void createFramebuffers();
    void renderExtract(GLuint inputTexture);
    void renderBlur();
    void renderBlend(GLuint inputTexture);

    bool _enabled = true;
    float _threshold = 0.75f;
    int _blurPasses = 3;
    float _blurMagnitude = 0.001f;
    float _intensity = 1.0f;

    int _width = 0;
    int _height = 0;
    int _bloomWidth = 0;
    int _bloomHeight = 0;

    GLuint _screenQuad = 0;
    GLuint _vertexBuffer = 0;

    GLuint _bloomFramebuffer = 0;
    GLuint _blurFramebuffer = 0;
    GLuint _bloomTexture = 0;
    GLuint _blurTexture[2] = {0, 0};

    std::unique_ptr<ghoul::opengl::ProgramObject> _extractProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _blurProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _blendProgram;

    struct {
        GLint mainColorTexture = -1;
        GLint threshold = -1;
    } _extractUniforms;

    struct {
        GLint mainColorTexture = -1;
        GLint blurDirection = -1;
        GLint blurMagnitude = -1;
    } _blurUniforms;

    struct {
        GLint mainColorTexture = -1;
        GLint bloomTexture = -1;
        GLint bloomIntensity = -1;
    } _blendUniforms;
};

} // namespace openspace

#endif // __OPENSPACE_RENDERING_POSTPROCESS___