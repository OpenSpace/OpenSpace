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

#include <openspace/rendering/postprocess.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "PostprocessingBloom";
    
    constexpr std::string_view BloomPassVertexShaderPath = "${SHADERS}/bloom_pass_vs.glsl";
    constexpr std::string_view BloomExtractFragmentShaderPath = "${SHADERS}/bloom_extract_fs.glsl";
    constexpr std::string_view BloomBlurFragmentShaderPath = "${SHADERS}/bloom_blur_fs.glsl";
    constexpr std::string_view BloomBlendFragmentShaderPath = "${SHADERS}/bloom_blend_fs.glsl";
}

namespace openspace {

PostprocessingBloom::PostprocessingBloom() = default;

PostprocessingBloom::~PostprocessingBloom() {
    deinitialize();
}

void PostprocessingBloom::initialize() {
    const GLfloat vertexData[] = {
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, nullptr);
    glEnableVertexAttribArray(0);

    glBindVertexArray(0);

    _extractProgram = ghoul::opengl::ProgramObject::Build(
        "BloomExtract",
        absPath(BloomPassVertexShaderPath),
        absPath(BloomExtractFragmentShaderPath)
    );
    if (_extractProgram) {
        _extractUniforms.mainColorTexture = _extractProgram->uniformLocation("mainColorTexture");
        _extractUniforms.threshold = _extractProgram->uniformLocation("threshold");
    }

    _blurProgram = ghoul::opengl::ProgramObject::Build(
        "BloomBlur",
        absPath(BloomPassVertexShaderPath),
        absPath(BloomBlurFragmentShaderPath)
    );
    if (_blurProgram) {
        _blurUniforms.mainColorTexture = _blurProgram->uniformLocation("mainColorTexture");
        _blurUniforms.blurDirection = _blurProgram->uniformLocation("blurDirection");
        _blurUniforms.blurMagnitude = _blurProgram->uniformLocation("blurMagnitude");
    }

    _blendProgram = ghoul::opengl::ProgramObject::Build(
        "BloomBlend",
        absPath(BloomPassVertexShaderPath),
        absPath(BloomBlendFragmentShaderPath)
    );
    if (_blendProgram) {
        _blendUniforms.mainColorTexture = _blendProgram->uniformLocation("mainColorTexture");
        _blendUniforms.bloomTexture = _blendProgram->uniformLocation("bloomTexture");
        _blendUniforms.bloomIntensity = _blendProgram->uniformLocation("bloomIntensity");
    }
}

void PostprocessingBloom::deinitialize() {
    if (_screenQuad != 0) {
        glDeleteVertexArrays(1, &_screenQuad);
        _screenQuad = 0;
    }
    
    if (_vertexBuffer != 0) {
        glDeleteBuffers(1, &_vertexBuffer);
        _vertexBuffer = 0;
    }

    if (_bloomFramebuffer != 0) {
        glDeleteFramebuffers(1, &_bloomFramebuffer);
        _bloomFramebuffer = 0;
    }

    if (_blurFramebuffer != 0) {
        glDeleteFramebuffers(1, &_blurFramebuffer);
        _blurFramebuffer = 0;
    }

    if (_bloomTexture != 0) {
        glDeleteTextures(1, &_bloomTexture);
        _bloomTexture = 0;
    }

    if (_blurTexture[0] != 0) {
        glDeleteTextures(2, _blurTexture);
        _blurTexture[0] = 0;
        _blurTexture[1] = 0;
    }
}

void PostprocessingBloom::setResolution(int width, int height) {
    if (_width == width && _height == height) {
        return;
    }

    _width = width;
    _height = height;
    _bloomWidth = width / 2;
    _bloomHeight = height / 2;

    createFramebuffers();
}

void PostprocessingBloom::createFramebuffers() {
    if (_bloomFramebuffer != 0) {
        glDeleteFramebuffers(1, &_bloomFramebuffer);
        glDeleteTextures(1, &_bloomTexture);
    }
    
    if (_blurFramebuffer != 0) {
        glDeleteFramebuffers(1, &_blurFramebuffer);
        glDeleteTextures(2, _blurTexture);
    }

    glGenTextures(1, &_bloomTexture);
    glBindTexture(GL_TEXTURE_2D, _bloomTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, _bloomWidth, _bloomHeight, 0, GL_RGBA, GL_FLOAT, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glGenTextures(2, _blurTexture);
    for (int i = 0; i < 2; i++) {
        glBindTexture(GL_TEXTURE_2D, _blurTexture[i]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, _bloomWidth, _bloomHeight, 0, GL_RGBA, GL_FLOAT, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    }

    glGenFramebuffers(1, &_bloomFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, _bloomFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _bloomTexture, 0);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Bloom framebuffer is not complete");
    }

    glGenFramebuffers(1, &_blurFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, _blurFramebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _blurTexture[0], 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, _blurTexture[1], 0);

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Blur framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void PostprocessingBloom::update() {
    if (_extractProgram && _extractProgram->isDirty()) {
        _extractProgram->rebuildFromFile();
        _extractUniforms.mainColorTexture = _extractProgram->uniformLocation("mainColorTexture");
        _extractUniforms.threshold = _extractProgram->uniformLocation("threshold");
    }

    if (_blurProgram && _blurProgram->isDirty()) {
        _blurProgram->rebuildFromFile();
        _blurUniforms.mainColorTexture = _blurProgram->uniformLocation("mainColorTexture");
        _blurUniforms.blurDirection = _blurProgram->uniformLocation("blurDirection");
        _blurUniforms.blurMagnitude = _blurProgram->uniformLocation("blurMagnitude");
    }

    if (_blendProgram && _blendProgram->isDirty()) {
        _blendProgram->rebuildFromFile();
        _blendUniforms.mainColorTexture = _blendProgram->uniformLocation("mainColorTexture");
        _blendUniforms.bloomTexture = _blendProgram->uniformLocation("bloomTexture");
        _blendUniforms.bloomIntensity = _blendProgram->uniformLocation("bloomIntensity");
    }
}

void PostprocessingBloom::render(GLuint inputTexture) {
    if (!_enabled || !_extractProgram || !_blurProgram || !_blendProgram) {
        return;
    }

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    renderExtract(inputTexture);
    renderBlur();

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    renderBlend(inputTexture);

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
}

void PostprocessingBloom::renderExtract(GLuint inputTexture) {
    glBindFramebuffer(GL_FRAMEBUFFER, _bloomFramebuffer);
    glViewport(0, 0, _bloomWidth, _bloomHeight);
    
    _extractProgram->activate();

    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, inputTexture);
    glUniform1i(_extractUniforms.mainColorTexture, textureUnit);
    glUniform1f(_extractUniforms.threshold, _threshold);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _extractProgram->deactivate();
}

void PostprocessingBloom::renderBlur() {
    _blurProgram->activate();

    int sourceBuffer = 0;
    int targetBuffer = 1;

    glBindTexture(GL_TEXTURE_2D, _bloomTexture);
    
    for (int pass = 0; pass < _blurPasses; pass++) {
        // Horizontal pass
        glBindFramebuffer(GL_FRAMEBUFFER, _blurFramebuffer);
        glDrawBuffer(GL_COLOR_ATTACHMENT0 + targetBuffer);
        glViewport(0, 0, _bloomWidth, _bloomHeight);

        ghoul::opengl::TextureUnit textureUnit;
        textureUnit.activate();
        if (pass == 0) {
            glBindTexture(GL_TEXTURE_2D, _bloomTexture);
        } else {
            glBindTexture(GL_TEXTURE_2D, _blurTexture[sourceBuffer]);
        }
        
        glUniform1i(_blurUniforms.mainColorTexture, textureUnit);
        glUniform2f(_blurUniforms.blurDirection, 1.0f, 0.0f);
        glUniform1f(_blurUniforms.blurMagnitude, _blurMagnitude);

        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        std::swap(sourceBuffer, targetBuffer);

        // Vertical pass
        glDrawBuffer(GL_COLOR_ATTACHMENT0 + targetBuffer);

        textureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _blurTexture[sourceBuffer]);
        glUniform1i(_blurUniforms.mainColorTexture, textureUnit);
        glUniform2f(_blurUniforms.blurDirection, 0.0f, 1.0f);
        glUniform1f(_blurUniforms.blurMagnitude, _blurMagnitude);

        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        std::swap(sourceBuffer, targetBuffer);
    }

    _blurProgram->deactivate();
}

void PostprocessingBloom::renderBlend(GLuint inputTexture) {
    glViewport(0, 0, _width, _height);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);

    _blendProgram->activate();

    ghoul::opengl::TextureUnit mainTextureUnit;
    mainTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, inputTexture);
    glUniform1i(_blendUniforms.mainColorTexture, mainTextureUnit);

    ghoul::opengl::TextureUnit bloomTextureUnit;
    bloomTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _blurTexture[0]);
    glUniform1i(_blendUniforms.bloomTexture, bloomTextureUnit);
    glUniform1f(_blendUniforms.bloomIntensity, _intensity);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _blendProgram->deactivate();

    glDisable(GL_BLEND);
}

} // namespace openspace