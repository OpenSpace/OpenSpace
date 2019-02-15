/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/rendering/framebufferrenderer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/performance/performancemeasurement.h>
#include <openspace/rendering/deferredcaster.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <string>
#include <vector>

namespace {
    constexpr const char* _loggerCat = "FramebufferRenderer";

    constexpr const std::array<const char*, 3> UniformNames = {
        "mainColorTexture", "blackoutFactor", "nAaSamples"
    };

    constexpr const std::array<const char*, 12> HDRUniformNames = {
        "deferredResultsTexture", "blackoutFactor", "backgroundConstant", 
        "backgroundExposure", "gamma", "toneMapOperator", "aveLum", "maxWhite",
        "Hue", "Saturation", "Value", "Lightness"
    };

    constexpr const std::array<const char*, 6> BloomUniformNames  = {
        "renderedImage", "bloomImage", "bloomThresholdMin", "bloomThresholdMax", 
        "bloomOrigFactor", "bloomNewFactor"
    };

    constexpr const std::array<const char*, 4> HistoUniformNames = {
        "renderedImage", "maxWhite", "imageWidth", "imageHeight"
    };

    constexpr const std::array<const char*, 4> TMOUniformNames = {
        "hdrSampler", "key", "Ywhite", "sat"
    };

    constexpr const char* ExitFragmentShaderPath =
        "${SHADERS}/framebuffer/exitframebuffer.frag";
    constexpr const char* RaycastFragmentShaderPath =
        "${SHADERS}/framebuffer/raycastframebuffer.frag";
    constexpr const char* GetEntryInsidePath = "${SHADERS}/framebuffer/inside.glsl";
    constexpr const char* GetEntryOutsidePath = "${SHADERS}/framebuffer/outside.glsl";
    constexpr const char* RenderFragmentShaderPath =
        "${SHADERS}/framebuffer/renderframebuffer.frag";

    void saveTextureToMemory(GLenum attachment, int width, int height,
                             std::vector<double>& memory)
    {
        memory.clear();
        memory.resize(width * height * 3);

        std::vector<float> tempMemory(width * height * 3);

        if (attachment != GL_DEPTH_ATTACHMENT) {
            glReadBuffer(attachment);
            glReadPixels(0, 0, width, height, GL_RGB, GL_FLOAT, tempMemory.data());

        }
        else {
            glReadPixels(
                0,
                0,
                width,
                height,
                GL_DEPTH_COMPONENT,
                GL_FLOAT,
                tempMemory.data()
            );
        }

        for (int i = 0; i < width * height * 3; ++i) {
            memory[i] = static_cast<double>(tempMemory[i]);
        }
    }

} // namespace

namespace openspace {

void FramebufferRenderer::initialize() {
    LDEBUG("Initializing FramebufferRenderer");

    const GLfloat vertexData[] = {
        // x     y
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, nullptr);
    glEnableVertexAttribArray(0);

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    // Main framebuffer
    glGenTextures(1, &_mainColorTexture);
    glGenTextures(1, &_mainDepthTexture);
    glGenTextures(1, &_mainFilterTexture);
    glGenFramebuffers(1, &_mainFramebuffer);

    // Exit framebuffer
    glGenTextures(1, &_exitColorTexture);
    glGenTextures(1, &_exitDepthTexture);
    glGenFramebuffers(1, &_exitFramebuffer);

    // Deferred textures
    glGenTextures(1, &_mainPositionTexture);
    glGenTextures(1, &_mainNormalTexture);

    // HDR / Filtering Framebuffer
    glGenFramebuffers(1, &_hdrFilteringFramebuffer);
    glGenTextures(1, &_hdrFilteringTexture);

    // Compute Average Luminosity
    glGenTextures(1, &_computeAveLumTexture);
    glGenFramebuffers(1, &_computeAveLumFBO);

    // Bloom Filter
    glGenFramebuffers(3, _bloomFilterFBO);
    glGenTextures(3, _bloomTexture);

    // Histogram
    glGenFramebuffers(1, &_histoFramebuffer);
    glGenTextures(1, &_histoTexture);
    glGenVertexArrays(1, &_histoVao);
    glBindVertexArray(_histoVao);
    glGenBuffers(1, &_histoVbo);

    // TMO via mipmapping
    glGenFramebuffers(1, &_tmoFramebuffer);
    glGenTextures(1, &_tmoTexture);
    glGenSamplers(1, &_tmoHdrSampler);
    glSamplerParameteri(_tmoHdrSampler, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);

    updateResolution();
    updateRendererData();
    updateRaycastData();   

    // Builds Main Framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainColorTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainPositionTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT2,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainNormalTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT3,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainFilterTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D_MULTISAMPLE,
        _mainDepthTexture,
        0
    );

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main framebuffer is not complete");
    }

    // Builds Exit Framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _exitColorTexture,
        0
    );
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        _exitDepthTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Exit framebuffer is not complete");
    }

    // Builds HDR/Filtering Framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _hdrFilteringFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _hdrFilteringTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("HDR/Filtering framebuffer is not complete");
    }

    // Buids Average Lum FBO
    glBindFramebuffer(GL_FRAMEBUFFER, _computeAveLumFBO);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _computeAveLumTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Average Luminosity framebuffer is not complete");
    }

    // Builds Bloom Filter FBOs
    //const GLenum buffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    const GLenum buffers[] = { GL_COLOR_ATTACHMENT0 };
    for (int i = 0; i < 3; i++)
    {
        glBindFramebuffer(GL_FRAMEBUFFER, _bloomFilterFBO[i]);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER, 
            GL_COLOR_ATTACHMENT0, 
            GL_TEXTURE_2D, 
            _bloomTexture[i], 
            0
        );
        glDrawBuffers(1, buffers);
        
        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            LERROR(fmt::format("Bloom framebuffer {} is not complete", i));
        }
    }

    // Builds Histogram FBO
    glBindFramebuffer(GL_FRAMEBUFFER, _histoFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _histoTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Histogram framebuffer is not complete");
    }

    // Buids TMO via mipmapping FBO    
    glBindFramebuffer(GL_FRAMEBUFFER, _tmoFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _tmoTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Histogram framebuffer is not complete");
    }

    // JCC: Moved to here to avoid NVidia: "Program/shader state performance warning"
    // Builds HDR and Filtering programs
    updateHDRAndFiltering();
    updateAveLum();
    updateBloomConfig();
    updateHistogramConfig();
    // Builds deferred casters programs
    updateDeferredcastData();
    updateTMOViaMipMappingConfig();

    _dirtyMsaaSamplingPattern = true;

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

    _resolveProgram = ghoul::opengl::ProgramObject::Build(
        "Framebuffer Resolve",
        absPath("${SHADERS}/framebuffer/resolveframebuffer.vert"),
        absPath("${SHADERS}/framebuffer/resolveframebuffer.frag")
    );

    ghoul::opengl::updateUniformLocations(
        *_resolveProgram, 
        _uniformCache, 
        UniformNames
    );
    ghoul::opengl::updateUniformLocations(
        *_hdrFilteringProgram, 
        _hdrUniformCache, 
        HDRUniformNames
    );
    ghoul::opengl::updateUniformLocations(
        *_bloomResolveProgram, 
        _bloomUniformCache,
        BloomUniformNames 
    );

    ghoul::opengl::updateUniformLocations(
        *_histoProgram,
        _histoUniformCache,
        HistoUniformNames
    );
    
   //    /*ghoul::opengl::updateUniformLocations(
   //        *_histoApplyProgram,
   //        _histoApplyUniformCache,
   //        HistoApplyUniformNames
   //    );*/
   
    ghoul::opengl::updateUniformLocations(
        *_tmoProgram,
        _tmoUniformCache,
        TMOUniformNames
    );

    global::raycasterManager.addListener(*this);
    global::deferredcasterManager.addListener(*this);
}

void FramebufferRenderer::deinitialize() {
    LINFO("Deinitializing FramebufferRenderer");

    glDeleteFramebuffers(1, &_mainFramebuffer);
    glDeleteFramebuffers(1, &_exitFramebuffer);
    glDeleteFramebuffers(1, &_hdrFilteringFramebuffer);
    glDeleteFramebuffers(1, &_computeAveLumFBO);
    glDeleteFramebuffers(3, _bloomFilterFBO);
    glDeleteFramebuffers(1, &_histoFramebuffer);
    glDeleteFramebuffers(1, &_tmoFramebuffer);

    glDeleteTextures(1, &_mainColorTexture);
    glDeleteTextures(1, &_mainDepthTexture);

    glDeleteTextures(1, &_hdrFilteringTexture);
    glDeleteTextures(1, &_mainPositionTexture);
    glDeleteTextures(1, &_mainNormalTexture);
    glDeleteTextures(1, &_computeAveLumTexture);
    glDeleteTextures(3, _bloomTexture);
    glDeleteTextures(1, &_histoTexture);
    glDeleteTextures(1, &_tmoTexture);

    glDeleteTextures(1, &_exitColorTexture);
    glDeleteTextures(1, &_exitDepthTexture);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);

    glDeleteSamplers(1, &_tmoHdrSampler);

    global::raycasterManager.removeListener(*this);
    global::deferredcasterManager.removeListener(*this);
}

void FramebufferRenderer::raycastersChanged(VolumeRaycaster&,
                                            RaycasterListener::IsAttached)
{
    _dirtyRaycastData = true;
}

void FramebufferRenderer::deferredcastersChanged(Deferredcaster&,
                                                 DeferredcasterListener::IsAttached)
{
    _dirtyDeferredcastData = true;
}

float FramebufferRenderer::computeBufferAveLuminance() {
    unsigned int texDimension = _resolution.x * _resolution.y;
    
    std::unique_ptr<GLfloat[]> texData(new GLfloat[texDimension * 3]);

    ghoul::opengl::TextureUnit hdrTextureUnit;
    hdrTextureUnit.activate();    
    glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);
    
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGB, GL_FLOAT, texData.get());
    
    float sum = 0.0f;

    for (unsigned int i = 0; i < texDimension; i++) {
        float lum = glm::dot(
            glm::vec3(texData[i * 3 + 0], texData[i * 3 + 1], texData[i * 3 + 2]),
            glm::vec3(0.2126f, 0.7152f, 0.0722f)
        );
        sum += logf(lum + 0.00001f);
    }

    return expf(sum / texDimension);
}

float FramebufferRenderer::computeBufferAveLuminanceGPU() {
    // Capture standard fbo
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    glBindFramebuffer(GL_FRAMEBUFFER, _computeAveLumFBO);
    GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, textureBuffers);
    glClear(GL_COLOR_BUFFER_BIT);
    
    glViewport(0, 0, 1, 1);

    _aveLumProgram->activate();

    //float averageLuminaceInFB = computeBufferAveLuminance();
    //std::cout << "=== Average Lum on CPU = " << averageLuminaceInFB << " ===" << std::endl;

    ghoul::opengl::TextureUnit hdrTextureUnit;
    hdrTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);
    _aveLumProgram->setUniform("hdrTexture", hdrTextureUnit);
    _aveLumProgram->setUniform("bufferWidth", _resolution.x);
    _aveLumProgram->setUniform("bufferHeight", _resolution.y);
    
    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _aveLumProgram->deactivate();

    std::vector<double> gpuAveLum;
    saveTextureToMemory(GL_COLOR_ATTACHMENT0, 1, 1, gpuAveLum);

    //std::cout << "=== Average Lum on GPU = " << gpuAveLum[0] << " ===" << std::endl;

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glViewport(0, 0, _resolution.x, _resolution.y);

    return static_cast<float>(gpuAveLum[0]);
}

void FramebufferRenderer::applyBloomFilter() {
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    GLuint vao;
    glGenVertexArrays(1, &vao);

    glBindFramebuffer(GL_FRAMEBUFFER, _bloomFilterFBO[0]);
    GLenum textureBuffer[] = {
       GL_COLOR_ATTACHMENT0
    };
    glDrawBuffers(1, textureBuffer);
    glClear(GL_COLOR_BUFFER_BIT);
    glViewport(0, 0, _resolution.y, _resolution.x);
    glBindVertexArray(vao);

    _bloomProgram->activate();

    {
        ghoul::opengl::TextureUnit filterTextureUnit;
        filterTextureUnit.activate();
        // The filter texture where the gaussian filter will be applied
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainFilterTexture);
        _bloomProgram->setUniform("filterStep", 1);
        _bloomProgram->setUniform("filterImage", filterTextureUnit);
        
        // Making OpenGL happy...
        ghoul::opengl::TextureUnit dummyTextureUnit;
        dummyTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D, _bloomTexture[2]);
        _bloomProgram->setUniform("filterFirstPass", dummyTextureUnit);

        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        glFlush();
    }

    _bloomProgram->deactivate();
    
    glBindFramebuffer(GL_FRAMEBUFFER, _bloomFilterFBO[1]);
    glDrawBuffers(1, textureBuffer);
    glViewport(0, 0, _resolution.x, _resolution.y);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindVertexArray(vao);

    _bloomProgram->activate();

    {        
        ghoul::opengl::TextureUnit filterTextureUnit;
        filterTextureUnit.activate();
        // The results of the previous pass is passed to this pass
        glBindTexture(GL_TEXTURE_2D, _bloomTexture[0]);
        _bloomProgram->setUniform("filterStep", 2);
        _bloomProgram->setUniform("filterFirstPass", filterTextureUnit);

        // Making OpenGL happy...
        ghoul::opengl::TextureUnit dummyTextureUnit;
        dummyTextureUnit.activate();
        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainFilterTexture);
        _bloomProgram->setUniform("filterImage", dummyTextureUnit);
        
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

        glFlush();
    }

    _bloomProgram->deactivate();

    glBindFramebuffer(GL_FRAMEBUFFER, _bloomFilterFBO[2]);
    glDrawBuffers(1, textureBuffer);
    glViewport(0, 0, _resolution.x, _resolution.y);
    glClear(GL_COLOR_BUFFER_BIT);

    _bloomResolveProgram->activate();

    {
        ghoul::opengl::TextureUnit deferredResultsTextureUnit;
        deferredResultsTextureUnit.activate();
        // Original buffer will be summed to the bloom result
        glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);
        _bloomResolveProgram->setUniform(
            _bloomUniformCache.renderedImage, 
            deferredResultsTextureUnit
        );

        ghoul::opengl::TextureUnit bloomTextureUnit;
        bloomTextureUnit.activate();
        // Results of the second pass are added to the original buffer
        glBindTexture(GL_TEXTURE_2D, _bloomTexture[1]);
        _bloomResolveProgram->setUniform(_bloomUniformCache.bloomImage, bloomTextureUnit);
        
        _bloomResolveProgram->setUniform(
            _bloomUniformCache.bloomOrigFactor, 
            _bloomOrigFactor
        );
        _bloomResolveProgram->setUniform(
            _bloomUniformCache.bloomNewFactor, 
            _bloomNewFactor
        );

        // Write the results to the _bloomDilterFBO[2] texture
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        glFlush();
    }

    _bloomResolveProgram->deactivate();

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
}

void FramebufferRenderer::computeImageHistogram() {
    glBindFramebuffer(GL_FRAMEBUFFER, _histoFramebuffer);
    GLenum textureBuffer[] = {
       GL_COLOR_ATTACHMENT0
    };
    glDrawBuffers(1, textureBuffer);

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glViewport(0, 0, _resolution.x, _resolution.y);

    GLboolean depthTestEnabled = glIsEnabled(GL_DEPTH_TEST);
    
    glDisable(GL_DEPTH_TEST);

    // Saving Blending State
    GLboolean blendEnabled = glIsEnabled(GL_BLEND);
    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;

    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    // Changing blending functions for histogram computation
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_ONE, GL_ONE);
    glEnable(GL_BLEND);

    glBindVertexArray(_histoVao);
    
    _histoProgram->activate();

    ghoul::opengl::TextureUnit renderedImage;
    renderedImage.activate();
    glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);
    _histoProgram->setUniform(_histoUniformCache.renderedImage, renderedImage);
    _histoProgram->setUniform(_histoUniformCache.imageWidth, static_cast<float>(_resolution.x));
    _histoProgram->setUniform(_histoUniformCache.imageHeight, static_cast<float>(_resolution.y));
    _histoProgram->setUniform(_histoUniformCache.maxWhite, _maxWhite);

    glDrawArrays(GL_POINTS, 0, _resolution.x * _resolution.y);
    
    glFlush();

    _histoProgram->deactivate();

    // Testing
    std::vector<double> gpuHistogram;
    saveTextureToMemory(GL_COLOR_ATTACHMENT0, _numberOfBins, 1, gpuHistogram);

    
    // Restores blending state
    if (!blendEnabled) {
        glDisable(GL_BLEND);
    }
    else {
        glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
        glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
    }

    // Restores Depth test state
    if (depthTestEnabled) {
        glEnable(GL_DEPTH_TEST);
    }
}

void FramebufferRenderer::computeMipMappingFromHDRBuffer(GLuint oglImageBuffer) {
    ghoul::opengl::TextureUnit samplerUnit;    
    glBindSampler(samplerUnit, _tmoHdrSampler);
    samplerUnit.activate();
    glBindTexture(GL_TEXTURE_2D, oglImageBuffer);

    glGenerateMipmap(GL_TEXTURE_2D);

    _tmoProgram->setUniform(_tmoUniformCache.hdrSampler, samplerUnit);
    _tmoProgram->setUniform(_tmoUniformCache.key, _tmoKey);
    _tmoProgram->setUniform(_tmoUniformCache.Ywhite, _tmoYwhite);
    _tmoProgram->setUniform(_tmoUniformCache.sat, _tmoSaturation);

    glBindFramebuffer(GL_FRAMEBUFFER, _tmoFramebuffer);
    GLenum textureBuffer[] = {
       GL_COLOR_ATTACHMENT0
    };
    glDrawBuffers(1, textureBuffer);

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glViewport(0, 0, _resolution.x, _resolution.y);

    glBindVertexArray(_screenQuad);

    _tmoProgram->activate();

    glDrawArrays(GL_TRIANGLES, 0, 6);
    
    _tmoProgram->deactivate();

    glBindVertexArray(0);
}

void FramebufferRenderer::update() {
    if (_dirtyMsaaSamplingPattern) {
        updateMSAASamplingPattern();
    }

    if (_dirtyResolution) {
        updateResolution();
        updateMSAASamplingPattern();
    }

    if (_dirtyRaycastData) {
        updateRaycastData();
    }

    if (_dirtyDeferredcastData) {
        updateDeferredcastData();
    }

    if (_resolveProgram->isDirty()) {
        _resolveProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_resolveProgram,
            _uniformCache,
            UniformNames
        );
    }
    
    if (_aveLumProgram->isDirty()) {
        _aveLumProgram->rebuildFromFile();
    }

    if (_bloomProgram->isDirty()) {
        _bloomProgram->rebuildFromFile();
    }

    if (_bloomResolveProgram->isDirty()) {
        _bloomResolveProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_bloomResolveProgram, 
            _bloomUniformCache,
            BloomUniformNames 
        );
    }

    if (_histoProgram->isDirty()) {
        _histoProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_histoProgram,
            _histoUniformCache,
            HistoUniformNames
        );
    }

    //if (_histoApplyProgram->isDirty()) {
    //    _histoApplyProgram->rebuildFromFile();
    //    /*ghoul::opengl::updateUniformLocations(
    //        *_histoApplyProgram,
    //        _histoApplyUniformCache,
    //        HistoApplyUniformNames
    //    );*/
    //}

    if (_hdrFilteringProgram->isDirty()) {
        _hdrFilteringProgram->rebuildFromFile();

        ghoul::opengl::updateUniformLocations(
            *_hdrFilteringProgram,
            _hdrUniformCache,
            HDRUniformNames
        );
    }

    if (_tmoProgram->isDirty()) {
        _tmoProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_tmoProgram,
            _tmoUniformCache,
            TMOUniformNames
        );
    }

    using K = VolumeRaycaster*;
    using V = std::unique_ptr<ghoul::opengl::ProgramObject>;
    for (const std::pair<const K, V>& program : _exitPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<const K, V>& program : _raycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<const K, V>& program : _insideRaycastPrograms) {
        if (program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    for (const std::pair<
            Deferredcaster* const,
            std::unique_ptr<ghoul::opengl::ProgramObject>
        >& program : _deferredcastPrograms)
    {
        if (program.second && program.second->isDirty()) {
            try {
                program.second->rebuildFromFile();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }
}

void FramebufferRenderer::updateResolution() {
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        true
    );

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainFilterTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        true
    );

    // HDR / Filtering
    glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);

    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // Average Luminosity Computation Texture
    glBindTexture(GL_TEXTURE_2D, _computeAveLumTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        1,
        1,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Bloom Filter
    for (int i = 0; i < 3; i++)
    {
        glBindTexture(GL_TEXTURE_2D, _bloomTexture[i]);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGBA16F,
            i ? _resolution.x : _resolution.y,
            i ? _resolution.y : _resolution.x,
            0,
            GL_RGBA,
            GL_FLOAT,
            nullptr
        );

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    }

    // Histogram Texture
    glBindTexture(GL_TEXTURE_2D, _histoTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _numberOfBins,
        1,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    glBindVertexArray(_histoVao);
    glBindBuffer(GL_ARRAY_BUFFER, _histoVbo);

    _histoPoints.clear();
    _histoPoints.reserve(_resolution.x * _resolution.y);
    for (int i = 0; i < _resolution.x * _resolution.y; ++i) {
        _histoPoints.push_back(i);
    }

    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(float) * _histoPoints.size(), 
        _histoPoints.data(), 
        GL_DYNAMIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 1, GL_FLOAT, GL_FALSE, 0, nullptr);    

    glBindVertexArray(0);

    // TMO via mipmapping
    glBindTexture(GL_TEXTURE_2D, _tmoTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_SRGB8,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGB,
        GL_UNSIGNED_BYTE,
        nullptr
    );
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // G-Buffer main position
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture);
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        true
    );

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture);

    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        _resolution.x,
        _resolution.y,
        true
    );

    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_DEPTH_COMPONENT32F,
        _resolution.x,
        _resolution.y,
        true
    );

    glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA16F,
        _resolution.x,
        _resolution.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_SHORT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glBindTexture(GL_TEXTURE_2D, _exitDepthTexture);

    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_DEPTH_COMPONENT32F,
        _resolution.x,
        _resolution.y,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    _dirtyResolution = false;
}

void FramebufferRenderer::updateRaycastData() {
    _raycastData.clear();
    _exitPrograms.clear();
    _raycastPrograms.clear();
    _insideRaycastPrograms.clear();

    const std::vector<VolumeRaycaster*>& raycasters =
        global::raycasterManager.raycasters();

    int nextId = 0;
    for (VolumeRaycaster* raycaster : raycasters) {
        RaycastData data = { nextId++, "Helper" };

        const std::string& vsPath = raycaster->boundsVertexShaderPath();
        std::string fsPath = raycaster->boundsFragmentShaderPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        dict.setValue("fragmentPath", std::move(fsPath));
        dict.setValue("id", data.id);

        std::string helperPath = raycaster->helperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", std::move(helperPath));
        }
        dict.setValue("helperPaths", std::move(helpersDict));
        dict.setValue("raycastPath", raycaster->raycasterPath());

        _raycastData[raycaster] = data;

        try {
            _exitPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " exit",
                absPath(vsPath),
                absPath(ExitFragmentShaderPath),
                dict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary outsideDict = dict;
            outsideDict.setValue("getEntryPath", GetEntryOutsidePath);
            _raycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " raycast",
                absPath(vsPath),
                absPath(RaycastFragmentShaderPath),
                outsideDict
            );
        } catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
        }

        try {
            ghoul::Dictionary insideDict = dict;
            insideDict.setValue("getEntryPath", GetEntryInsidePath);
            _insideRaycastPrograms[raycaster] = ghoul::opengl::ProgramObject::Build(
                "Volume " + std::to_string(data.id) + " inside raycast",
                absPath("${SHADERS}/framebuffer/resolveframebuffer.vert"),
                absPath(RaycastFragmentShaderPath),
                insideDict
            );
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    _dirtyRaycastData = false;
}

void FramebufferRenderer::updateDeferredcastData() {
    _deferredcastData.clear();
    _deferredcastPrograms.clear();

    const std::vector<Deferredcaster*>& deferredcasters =
        global::deferredcasterManager.deferredcasters();
    int nextId = 0;
    for (Deferredcaster* caster : deferredcasters) {
        DeferredcastData data = { nextId++, "HELPER" };

        std::string vsPath = caster->deferredcastVSPath();
        std::string fsPath = caster->deferredcastFSPath();
        std::string deferredShaderPath = caster->deferredcastPath();

        ghoul::Dictionary dict;
        dict.setValue("rendererData", _rendererData);
        //dict.setValue("fragmentPath", fsPath);
        dict.setValue("id", data.id);
        std::string helperPath = caster->helperPath();
        ghoul::Dictionary helpersDict;
        if (!helperPath.empty()) {
            helpersDict.setValue("0", helperPath);
        }
        dict.setValue("helperPaths", helpersDict);

        _deferredcastData[caster] = data;

        try {
            _deferredcastPrograms[caster] = ghoul::opengl::ProgramObject::Build(
                "Deferred " + std::to_string(data.id) + " raycast",
                absPath(vsPath),
                absPath(deferredShaderPath),
                dict
            );

            _deferredcastPrograms[caster]->setIgnoreSubroutineUniformLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );
            _deferredcastPrograms[caster]->setIgnoreUniformLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );

            caster->initializeCachedVariables(*_deferredcastPrograms[caster]);
        }
        catch (ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    _dirtyDeferredcastData = false;
}

void FramebufferRenderer::updateAveLum() {
    _aveLumProgram = ghoul::opengl::ProgramObject::Build(
        "Computes Average Luminace on GPU",
        absPath("${SHADERS}/framebuffer/computeAveLum.vert"),
        absPath("${SHADERS}/framebuffer/computeAveLum.frag")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_aveLumProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_aveLumProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateBloomConfig() {
    _bloomProgram = ghoul::opengl::ProgramObject::Build(
        "Appies the Bloom Filter",
        absPath("${SHADERS}/framebuffer/bloomFilter.vert"),
        absPath("${SHADERS}/framebuffer/bloomFilter.frag")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_bloomProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_bloomProgram->setIgnoreUniformLocationError(IgnoreError::Yes);

    _bloomResolveProgram = ghoul::opengl::ProgramObject::Build(
        "Adds bloom to final image",
        absPath("${SHADERS}/framebuffer/bloomResolveFilter.vert"),
        absPath("${SHADERS}/framebuffer/bloomResolveFilter.frag")
    );
    //_bloomResolveProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_bloomResolveProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateHistogramConfig() {
    _histoProgram = ghoul::opengl::ProgramObject::Build(
        "Computes Histogram from Image",
        absPath("${SHADERS}/framebuffer/computeHistogram_vs.glsl"),
        absPath("${SHADERS}/framebuffer/computeHistogram_fs.glsl")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_histoProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_histoProgram->setIgnoreUniformLocationError(IgnoreError::Yes);

    /*_histoApplyProgram = ghoul::opengl::ProgramObject::Build(
        "Applies histogram on the image",
        absPath("${SHADERS}/framebuffer/applyHistogram.vert"),
        absPath("${SHADERS}/framebuffer/applyHistogram.frag")
    );*/
    //_histoApplyProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_histoApplyProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateTMOViaMipMappingConfig() {
    _tmoProgram = ghoul::opengl::ProgramObject::Build(
        "Computes TMO via MipMapping",
        absPath("${SHADERS}/framebuffer/computeTMO_vs.glsl"),
        absPath("${SHADERS}/framebuffer/computeTMO_fs.glsl")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_tmoProgram ->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_tmoProgram ->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateHDRAndFiltering() {
    _hdrFilteringProgram = ghoul::opengl::ProgramObject::Build(
        "HDR and Filtering Program",
        absPath("${SHADERS}/framebuffer/hdrAndFiltering.vert"),
        absPath("${SHADERS}/framebuffer/hdrAndfiltering.frag")
    );
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_hdrFilteringProgram->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_hdrFilteringProgram->setIgnoreUniformLocationError(IgnoreError::Yes);
}

void FramebufferRenderer::updateMSAASamplingPattern() {
    LDEBUG("Updating MSAA Sampling Pattern");

    constexpr const int GridSize = 32;
    GLfloat step = 2.f / static_cast<GLfloat>(GridSize);
    GLfloat sizeX = -1.f;
    GLfloat sizeY = 1.0;

    constexpr const int NVertex = 4 * 6;
    // openPixelSizeVertexData
    GLfloat vertexData[GridSize * GridSize * NVertex];

    // @CLEANUP(abock): Is this necessary?  I was mucking about with the shader and it
    //                  didn't make any visual difference. If it is necessary, the z and w
    //                  components can be removed for sure since they are always 0, 1 and
    //                  not used in the shader either
    for (int y = 0; y < GridSize; ++y) {
        for (int x = 0; x < GridSize; ++x) {
            vertexData[y * GridSize * NVertex + x * NVertex] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 1] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 2] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 3] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 4] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 5] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 6] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 7] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 8] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 9] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 10] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 11] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 12] = sizeX;
            vertexData[y * GridSize * NVertex + x * NVertex + 13] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 14] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 15] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 16] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 17] = sizeY - step;
            vertexData[y * GridSize * NVertex + x * NVertex + 18] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 19] = 1.f;

            vertexData[y * GridSize * NVertex + x * NVertex + 20] = sizeX + step;
            vertexData[y * GridSize * NVertex + x * NVertex + 21] = sizeY;
            vertexData[y * GridSize * NVertex + x * NVertex + 22] = 0.f;
            vertexData[y * GridSize * NVertex + x * NVertex + 23] = 1.f;

            sizeX += step;
        }
        sizeX = -1.f;
        sizeY -= step;
    }

    GLuint pixelSizeQuadVAO = 0;
    glGenVertexArrays(1, &pixelSizeQuadVAO);
    glBindVertexArray(pixelSizeQuadVAO);

    GLuint pixelSizeQuadVBO = 0;
    glGenBuffers(1, &pixelSizeQuadVBO);
    glBindBuffer(GL_ARRAY_BUFFER, pixelSizeQuadVBO);

    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(GLfloat) * GridSize * GridSize * NVertex,
        vertexData,
        GL_STATIC_DRAW
    );

    // Position
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, nullptr);
    glEnableVertexAttribArray(0);

    // Saves current state
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    // Main framebuffer
    GLuint pixelSizeTexture = 0;
    GLuint pixelSizeFramebuffer = 0;

    glGenTextures(1, &pixelSizeTexture);
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, pixelSizeTexture);

    constexpr const GLsizei OnePixel = 1;
    glTexImage2DMultisample(
        GL_TEXTURE_2D_MULTISAMPLE,
        _nAaSamples,
        GL_RGBA32F,
        OnePixel,
        OnePixel,
        true
    );

    glViewport(0, 0, OnePixel, OnePixel);

    glGenFramebuffers(1, &pixelSizeFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, pixelSizeFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D_MULTISAMPLE,
        pixelSizeTexture,
        0
    );

    GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, textureBuffers);

    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("MSAA Sampling pattern framebuffer is not complete");
        return;
    }

    std::unique_ptr<ghoul::opengl::ProgramObject> pixelSizeProgram =
        ghoul::opengl::ProgramObject::Build(
            "OnePixel MSAA",
            absPath("${SHADERS}/framebuffer/pixelSizeMSAA.vert"),
            absPath("${SHADERS}/framebuffer/pixelSizeMSAA.frag")
        );

    pixelSizeProgram->activate();

    // Draw sub-pixel grid
    glEnable(GL_SAMPLE_SHADING);
    glBindVertexArray(pixelSizeQuadVAO);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(false);
    glDrawArrays(GL_TRIANGLES, 0, GridSize * GridSize * 6);
    glBindVertexArray(0);
    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_SAMPLE_SHADING);

    pixelSizeProgram->deactivate();

    // Now we render the Nx1 quad strip
    GLuint nOneStripFramebuffer = 0;
    GLuint nOneStripVAO = 0;
    GLuint nOneStripVBO = 0;
    GLuint nOneStripTexture = 0;

    sizeX = -1.f;
    step = 2.f / static_cast<GLfloat>(_nAaSamples);

    std::vector<GLfloat>nOneStripVertexData(_nAaSamples * (NVertex + 12));

    for (int x = 0; x < _nAaSamples; ++x) {
        nOneStripVertexData[x * (NVertex + 12)] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 1] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 2] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 3] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 4] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 5] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 6] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 7] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 8] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 9] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 10] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 11] = 1.f;

        nOneStripVertexData[x * (NVertex + 12) + 12] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 13] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 14] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 15] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 16] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 17] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 18] = sizeX;
        nOneStripVertexData[x * (NVertex + 12) + 19] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 20] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 21] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 22] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 23] = 0.f;

        nOneStripVertexData[x * (NVertex + 12) + 24] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 25] = -1.f;
        nOneStripVertexData[x * (NVertex + 12) + 26] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 27] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 28] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 29] = 1.f;

        nOneStripVertexData[x * (NVertex + 12) + 30] = sizeX + step;
        nOneStripVertexData[x * (NVertex + 12) + 31] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 32] = 0.f;
        nOneStripVertexData[x * (NVertex + 12) + 33] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 34] = 1.f;
        nOneStripVertexData[x * (NVertex + 12) + 35] = 1.f;

        sizeX += step;
    }

    glGenVertexArrays(1, &nOneStripVAO);
    glBindVertexArray(nOneStripVAO);
    glGenBuffers(1, &nOneStripVBO);
    glBindBuffer(GL_ARRAY_BUFFER, nOneStripVBO);
    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(GLfloat) * _nAaSamples * (NVertex + 12),
        nOneStripVertexData.data(),
        GL_STATIC_DRAW
    );

    // position
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
    glEnableVertexAttribArray(0);

    // texture coords
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 4)
    );
    glEnableVertexAttribArray(1);

    // fbo texture buffer
    glGenTextures(1, &nOneStripTexture);
    glBindTexture(GL_TEXTURE_2D, nOneStripTexture);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA32F,
        _nAaSamples,
        OnePixel,
        0,
        GL_RGBA,
        GL_FLOAT,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glGenFramebuffers(1, &nOneStripFramebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, nOneStripFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        nOneStripTexture,
        0
    );

    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("nOneStrip framebuffer is not complete");
    }

    glViewport(0, 0, _nAaSamples, OnePixel);

    std::unique_ptr<ghoul::opengl::ProgramObject> nOneStripProgram =
        ghoul::opengl::ProgramObject::Build(
            "OneStrip MSAA",
            absPath("${SHADERS}/framebuffer/nOneStripMSAA.vert"),
            absPath("${SHADERS}/framebuffer/nOneStripMSAA.frag")
        );

    nOneStripProgram->activate();

    ghoul::opengl::TextureUnit pixelSizeTextureUnit;
    pixelSizeTextureUnit.activate();
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, pixelSizeTexture);
    nOneStripProgram->setUniform("pixelSizeTexture", pixelSizeTextureUnit);

    // render strip
    glDrawBuffers(1, textureBuffers);

    glClearColor(0.f, 1.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glBindVertexArray(nOneStripVAO);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(false);

    for (int sample = 0; sample < _nAaSamples; ++sample) {
        nOneStripProgram->setUniform("currentSample", sample);
        glDrawArrays(GL_TRIANGLES, sample * 6, 6);
    }
    glDepthMask(true);
    glEnable(GL_DEPTH_TEST);
    glBindVertexArray(0);

    saveTextureToMemory(GL_COLOR_ATTACHMENT0, _nAaSamples, 1, _mSAAPattern);
    // Convert back to [-1, 1] range and then scale for the current viewport size:
    for (int d = 0; d < _nAaSamples; ++d) {
        _mSAAPattern[d * 3] = (2.0 * _mSAAPattern[d * 3] - 1.0) /
            static_cast<double>(viewport[1]);
        _mSAAPattern[(d * 3) + 1] = (2.0 * _mSAAPattern[(d * 3) + 1] - 1.0) /
            static_cast<double>(viewport[3]);
        _mSAAPattern[(d * 3) + 2] = 0.0;
    }

    nOneStripProgram->deactivate();

    // Restores default state
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    // Deletes unused buffers
    glDeleteFramebuffers(1, &pixelSizeFramebuffer);
    glDeleteTextures(1, &pixelSizeTexture);
    glDeleteBuffers(1, &pixelSizeQuadVBO);
    glDeleteVertexArrays(1, &pixelSizeQuadVAO);

    glDeleteFramebuffers(1, &nOneStripFramebuffer);
    glDeleteTextures(1, &nOneStripTexture);
    glDeleteBuffers(1, &nOneStripVBO);
    glDeleteVertexArrays(1, &nOneStripVAO);

    _dirtyMsaaSamplingPattern = false;
}

void FramebufferRenderer::render(Scene* scene, Camera* camera, float blackoutFactor) {
    const bool doPerformanceMeasurements = global::performanceManager.isEnabled();

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (doPerformanceMeasurements) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "FramebufferRenderer::render"
        );
    }

    if (!scene || !camera) {
        return;
    }    

    // Capture standard fbo
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
    glEnable(GL_DEPTH_TEST);

    // deferred g-buffer plus filter
    GLenum textureBuffers[4] = {
        GL_COLOR_ATTACHMENT0,
        GL_COLOR_ATTACHMENT1,
        GL_COLOR_ATTACHMENT2,
        GL_COLOR_ATTACHMENT3
    };
    glDrawBuffers(4, textureBuffers);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glEnablei(GL_BLEND, 0);
    glDisablei(GL_BLEND, 1);
    glDisablei(GL_BLEND, 2);
    glDisablei(GL_BLEND, 3);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    Time time = global::timeManager.time();

    RenderData data = {
        *camera,
        psc(),
        std::move(time),
        doPerformanceMeasurements,
        0,
        {}
    };
    RendererTasks tasks;

    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Background);
    scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Opaque);
    scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Transparent);
    scene->render(data, tasks);
    data.renderBinMask = static_cast<int>(Renderable::RenderBin::Overlay);
    scene->render(data, tasks);

    {
        std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
        if (doPerformanceMeasurements) {
            perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                "FramebufferRenderer::render::raycasterTasks"
            );
        }
        performRaycasterTasks(tasks.raycasterTasks);
    }

    //glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _hdrFilteringFramebuffer);
    GLenum dBuffer[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, dBuffer);
    glClear(GL_COLOR_BUFFER_BIT);

    {
        std::unique_ptr<performance::PerformanceMeasurement> perfInternal;
        if (doPerformanceMeasurements) {
            perfInternal = std::make_unique<performance::PerformanceMeasurement>(
                "FramebufferRenderer::render::deferredTasks"
            );
        }
        performDeferredTasks(tasks.deferredcasterTasks);
    }

    /*
    if (tasks.deferredcasterTasks.empty()) {
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
        _resolveProgram->activate();

        ghoul::opengl::TextureUnit mainColorTextureUnit;
        mainColorTextureUnit.activate();

        glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);
        _resolveProgram->setUniform(_uniformCache.mainColorTexture, mainColorTextureUnit);
        _resolveProgram->setUniform(_uniformCache.blackoutFactor, blackoutFactor);
        _resolveProgram->setUniform(_uniformCache.nAaSamples, _nAaSamples);
        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        _resolveProgram->deactivate();
    }
    */

    // DEBUG - JCC     
    {
        glDisable(GL_DEPTH_TEST);

        // Results of the DeferredTasks as entry for the bloom filter
        applyBloomFilter();

        float averageLuminaceInFB = 0.0;
        if (_toneMapOperator == 
            static_cast<int>(openspace::RenderEngine::ToneMapOperators::GLOBAL)
            )
        {
            averageLuminaceInFB = computeBufferAveLuminanceGPU();
            if (std::isnan(averageLuminaceInFB)) {
                averageLuminaceInFB = 1000.0;
            }
        }
        
        //float averageLuminaceInFB = 0.5;

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
        glViewport(0, 0, _resolution.x, _resolution.y);
        _hdrFilteringProgram->activate();

        ghoul::opengl::TextureUnit hdrFeedingTextureUnit;
        hdrFeedingTextureUnit.activate();
        if (_bloomEnabled) {
            // Bloom Enabled
            glBindTexture(GL_TEXTURE_2D, _bloomTexture[2]);  
        }
        else {
            // No Bloom
            glBindTexture(GL_TEXTURE_2D, _hdrFilteringTexture);
        }

        _hdrFilteringProgram->setUniform(
            _hdrUniformCache.deferredResultsTexture,
            hdrFeedingTextureUnit
        );
        
        
        _hdrFilteringProgram->setUniform(_hdrUniformCache.blackoutFactor, blackoutFactor);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.backgroundConstant, 
                                         _hdrBackground);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.backgroundExposure, _hdrExposure);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.gamma, _gamma);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.toneMapOperator, _toneMapOperator);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.aveLum, averageLuminaceInFB);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.maxWhite, _maxWhite);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.Hue, _hue);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.Saturation, _saturation);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.Value, _value);
        _hdrFilteringProgram->setUniform(_hdrUniformCache.Lightness, _lightness);

        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);

        _hdrFilteringProgram->deactivate();

        //================================
        // Adjusting color and brightness
        //================================
        
        // Histogram Equalization
        computeImageHistogram();

        computeMipMappingFromHDRBuffer(_hdrFilteringTexture);

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    }
    
    //glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
}

void FramebufferRenderer::performRaycasterTasks(const std::vector<RaycasterTask>& tasks) {
    for (const RaycasterTask& raycasterTask : tasks) {
        VolumeRaycaster* raycaster = raycasterTask.raycaster;

        glBindFramebuffer(GL_FRAMEBUFFER, _exitFramebuffer);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        ghoul::opengl::ProgramObject* exitProgram = _exitPrograms[raycaster].get();
        if (exitProgram) {
            exitProgram->activate();
            raycaster->renderExitPoints(raycasterTask.renderData, *exitProgram);
            exitProgram->deactivate();
        }

        glBindFramebuffer(GL_FRAMEBUFFER, _mainFramebuffer);
        glm::vec3 cameraPosition;
        bool isCameraInside = raycaster->isCameraInside(
            raycasterTask.renderData,
            cameraPosition
        );
        ghoul::opengl::ProgramObject* raycastProgram = nullptr;

        if (isCameraInside) {
            raycastProgram = _insideRaycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            }
            else {
                raycastProgram = _insideRaycastPrograms[raycaster].get();
                raycastProgram->activate();
                raycastProgram->setUniform("cameraPosInRaycaster", cameraPosition);
            }
        }
        else {
            raycastProgram = _raycastPrograms[raycaster].get();
            if (raycastProgram) {
                raycastProgram->activate();
            }
            else {
                raycastProgram = _raycastPrograms[raycaster].get();
                raycastProgram->activate();
            }
        }

        if (raycastProgram) {
            raycaster->preRaycast(_raycastData[raycaster], *raycastProgram);

            ghoul::opengl::TextureUnit exitColorTextureUnit;
            exitColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _exitColorTexture);
            raycastProgram->setUniform("exitColorTexture", exitColorTextureUnit);

            ghoul::opengl::TextureUnit exitDepthTextureUnit;
            exitDepthTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _exitDepthTexture);
            raycastProgram->setUniform("exitDepthTexture", exitDepthTextureUnit);

            ghoul::opengl::TextureUnit mainDepthTextureUnit;
            mainDepthTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainDepthTexture);
            raycastProgram->setUniform("mainDepthTexture", mainDepthTextureUnit);

            raycastProgram->setUniform("nAaSamples", _nAaSamples);
            raycastProgram->setUniform("windowSize", static_cast<glm::vec2>(_resolution));

            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);
            if (isCameraInside) {
                glBindVertexArray(_screenQuad);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);
            }
            else {
                raycaster->renderEntryPoints(raycasterTask.renderData, *raycastProgram);
            }
            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            raycaster->postRaycast(_raycastData[raycaster], *raycastProgram);
            raycastProgram->deactivate();
        }
        else {
            LWARNING("Raycaster is not attached when trying to perform raycaster task");
        }
    }
}

void FramebufferRenderer::performDeferredTasks(
                                             const std::vector<DeferredcasterTask>& tasks)
{
    bool firstPaint = true;

    for (const DeferredcasterTask& deferredcasterTask : tasks) {
        Deferredcaster* deferredcaster = deferredcasterTask.deferredcaster;

        ghoul::opengl::ProgramObject* deferredcastProgram = nullptr;

        if (deferredcastProgram != _deferredcastPrograms[deferredcaster].get()
            || deferredcastProgram == nullptr)
        {
            deferredcastProgram = _deferredcastPrograms[deferredcaster].get();
        }

        if (deferredcastProgram) {
            deferredcastProgram->activate();

            // adding G-Buffer
            ghoul::opengl::TextureUnit mainDColorTextureUnit;
            mainDColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainColorTexture);
            deferredcastProgram->setUniform(
                "mainColorTexture",
                mainDColorTextureUnit
            );

            ghoul::opengl::TextureUnit mainPositionTextureUnit;
            mainPositionTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainPositionTexture);
            deferredcastProgram->setUniform(
                "mainPositionTexture",
                mainPositionTextureUnit
            );

            ghoul::opengl::TextureUnit mainNormalTextureUnit;
            mainNormalTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, _mainNormalTexture);
            deferredcastProgram->setUniform(
                "mainNormalTexture",
                mainNormalTextureUnit
            );

            deferredcastProgram->setUniform("nAaSamples", _nAaSamples);
            // 48 = 16 samples * 3 coords
            deferredcastProgram->setUniform("msaaSamplePatter", &_mSAAPattern[0], 48);

            deferredcastProgram->setUniform("firstPaint", firstPaint);
            deferredcastProgram->setUniform("atmExposure", _hdrExposure);
            deferredcastProgram->setUniform("backgroundConstant", _hdrBackground);

            deferredcaster->preRaycast(
                deferredcasterTask.renderData,
                _deferredcastData[deferredcaster],
                *deferredcastProgram
            );

            glDisable(GL_DEPTH_TEST);
            glDepthMask(false);

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            glDepthMask(true);
            glEnable(GL_DEPTH_TEST);

            deferredcaster->postRaycast(
                deferredcasterTask.renderData,
                _deferredcastData[deferredcaster],
                *deferredcastProgram
            );

            deferredcastProgram->deactivate();

            if (firstPaint) {
                firstPaint = false;
            }
        }
        else {
            LWARNING(
                "Deferredcaster is not attached when trying to perform deferred task"
            );
        }
    }
}

void FramebufferRenderer::setResolution(glm::ivec2 res) {
    _resolution = std::move(res);
    _dirtyResolution = true;
}

void FramebufferRenderer::setNAaSamples(int nAaSamples) {
    ghoul_assert(
        nAaSamples >= 1 && nAaSamples <= 8,
        "Number of AA samples has to be between 1 and 8"
    );
    _nAaSamples = nAaSamples;
    if (_nAaSamples == 0) {
        _nAaSamples = 1;
    }
    if (_nAaSamples > 8) {
        LERROR("Framebuffer renderer does not support more than 8 MSAA samples.");
        _nAaSamples = 8;
    }
    _dirtyMsaaSamplingPattern = true;
}

void FramebufferRenderer::setHDRExposure(float hdrExposure) {
    ghoul_assert(hdrExposure > 0.f, "HDR exposure must be greater than zero");
    _hdrExposure = hdrExposure;
}

void FramebufferRenderer::setHDRBackground(float hdrBackground) {
    ghoul_assert(hdrBackground > 0.f, "HDR Background must be greater than zero");
    _hdrBackground = hdrBackground;
}

void FramebufferRenderer::setGamma(float gamma) {
    ghoul_assert(gamma > 0.f, "Gamma value must be greater than zero");
    _gamma = gamma;
}

void FramebufferRenderer::setMaxWhite(float maxWhite) {
    ghoul_assert(gamma > 0.f, "Max White value must be greater than zero");
    _maxWhite = maxWhite;
}

void FramebufferRenderer::setToneMapOperator(int tmOp) {
    _toneMapOperator = tmOp;
}

void FramebufferRenderer::setBloomThreMin(float minV) {
    _bloomThresholdMin = minV;
}

void FramebufferRenderer::setBloomThreMax(float maxV) {
    _bloomThresholdMax = maxV;
}

void FramebufferRenderer::setBloomOrigFactor(float origFactor) {
    _bloomOrigFactor = origFactor;
}

void FramebufferRenderer::setBloomNewFactor(float newFactor) {
    _bloomNewFactor = newFactor;
}

void FramebufferRenderer::setKey(float key) {
    _tmoKey = key;
}

void FramebufferRenderer::setYwhite(float white) {
    _tmoYwhite = white;
}

void FramebufferRenderer::setTmoSaturation(float sat) {
    _tmoSaturation = sat;
}

void FramebufferRenderer::setHue(float hue) {
    _hue = hue;
}

void FramebufferRenderer::setValue(float value) {
    _value = value;
}

void FramebufferRenderer::setSaturation(float sat) {
    _saturation = sat;
}

void FramebufferRenderer::setLightness(float lightness) {
    _lightness = lightness;
}

void FramebufferRenderer::enableBloom(bool enable) {
    _bloomEnabled = enable;
}

void FramebufferRenderer::enableHistogram(bool enable) {
    _histogramEnabled = enable;
}

float FramebufferRenderer::hdrBackground() const {
    return _hdrBackground;
}

int FramebufferRenderer::nAaSamples() const {
    return _nAaSamples;
}

const std::vector<double>& FramebufferRenderer::mSSAPattern() const {
    return _mSAAPattern;
}

void FramebufferRenderer::updateRendererData() {
    ghoul::Dictionary dict;
    dict.setValue("fragmentRendererPath", std::string(RenderFragmentShaderPath));
    _rendererData = dict;
    global::renderEngine.setRendererData(dict);
}

} // namespace openspace
