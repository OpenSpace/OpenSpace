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

#ifndef __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
#define __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/raycasterlistener.h>
#include <openspace/rendering/deferredcasterlistener.h>

#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <map>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class Camera;
struct DeferredcastData;
struct DeferredcasterTask;
struct RaycastData;
struct RaycasterTask;
class Scene;
struct UpdateStructures;

class FramebufferRenderer : public Renderer, public RaycasterListener,
                            public DeferredcasterListener
{
public:
    typedef std::map<
        VolumeRaycaster*,
        std::unique_ptr<ghoul::opengl::ProgramObject>
    > RaycasterProgObjMap;
    typedef std::map<
        Deferredcaster*,
        std::unique_ptr<ghoul::opengl::ProgramObject>
    > DeferredcasterProgObjMap;
public:
    virtual ~FramebufferRenderer() = default;

    void initialize() override;
    void deinitialize() override;

    void updateResolution();
    void updateRaycastData();
    void updateDeferredcastData();
    void updateHDRAndFiltering();
    void updateAveLum();
    void updateBloomConfig();
    void updateHistogramConfig();
    void updateTMOViaMipMappingConfig();
    void updateMSAASamplingPattern();

    void setResolution(glm::ivec2 res) override;
    void setNAaSamples(int nAaSamples) override;
    void setHDRExposure(float hdrExposure) override;
    void setHDRBackground(float hdrBackground) override;
    void setGamma(float gamma) override;
    void setMaxWhite(float maxWhite) override;
    void setToneMapOperator(int tmOp) override;
    void setBloomThreMin(float minV) override;
    void setBloomThreMax(float maxV) override;
    void setBloomOrigFactor(float origFactor) override;
    void setBloomNewFactor(float newFactor) override;
    void setKey(float key) override;
    void setYwhite(float white) override;
    void setTmoSaturation(float sat) override;
    void setHue(float hue) override;
    void setValue(float value) override;
    void setSaturation(float sat) override;
    void setLightness(float lightness) override;
    void setColorSpace(unsigned int colorspace) override;

    void enableBloom(bool enable) override;
    void enableHistogram(bool enable) override;

    float hdrBackground() const override;
    int nAaSamples() const override;
    const std::vector<double>& mSSAPattern() const override;

    void update() override;
    void performRaycasterTasks(const std::vector<RaycasterTask>& tasks);
    void performDeferredTasks(const std::vector<DeferredcasterTask>& tasks);
    void render(Scene* scene, Camera* camera, float blackoutFactor) override;

    /**
     * Update render data
     * Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData() override;

    virtual void raycastersChanged(VolumeRaycaster& raycaster,
        RaycasterListener::IsAttached attached) override;
    virtual void deferredcastersChanged(Deferredcaster& deferredcaster,
        DeferredcasterListener::IsAttached isAttached) override;

private:
    float computeBufferAveLuminance();
    float computeBufferAveLuminanceGPU();
    void applyBloomFilter();
    void computeImageHistogram();
    void computeMipMappingFromHDRBuffer(GLuint oglImageBuffer);

private:
    std::map<VolumeRaycaster*, RaycastData> _raycastData;
    RaycasterProgObjMap _exitPrograms;
    RaycasterProgObjMap _raycastPrograms;
    RaycasterProgObjMap _insideRaycastPrograms;

    std::map<Deferredcaster*, DeferredcastData> _deferredcastData;
    DeferredcasterProgObjMap _deferredcastPrograms;

    std::unique_ptr<ghoul::opengl::ProgramObject> _hdrFilteringProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _aveLumProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _bloomProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _bloomResolveProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _histoProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _histoApplyProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _tmoProgram;

    std::unique_ptr<ghoul::opengl::ProgramObject> _resolveProgram;
    UniformCache(mainColorTexture, blackoutFactor, nAaSamples) _uniformCache;
    
    UniformCache(deferredResultsTexture, blackoutFactor, backgroundConstant, 
                 hdrExposure, gamma, toneMapOperator, aveLum, maxWhite,
                 Hue, Saturation, Value, Lightness, colorSpace)
        _hdrUniformCache;

    UniformCache(renderedImage, bloomImage, bloomThresholdMin, bloomThresholdMax, 
        bloomOrigFactor, bloomNewFactor) _bloomUniformCache;

    UniformCache(renderedImage, maxWhite, imageWidth, 
                 imageHeight) _histoUniformCache;

    UniformCache(hdrSampler, key, Ywhite, sat) _tmoUniformCache;

    GLuint _screenQuad;
    GLuint _vertexPositionBuffer;
    GLuint _mainColorTexture;
    GLuint _mainFilterTexture;
    GLuint _mainPositionTexture;
    GLuint _mainNormalTexture;
    GLuint _mainDepthTexture;
    GLuint _exitColorTexture;
    GLuint _mainFramebuffer;
    GLuint _exitDepthTexture;
    GLuint _exitFramebuffer;
    GLuint _hdrFilteringFramebuffer;
    GLuint _hdrFilteringTexture;
    GLuint _histoFramebuffer;
    GLuint _histoTexture;
    GLuint _histoVao;
    GLuint _histoVbo;

    GLuint _bloomFilterFBO[3];
    GLuint _bloomTexture[3];

    GLuint _computeAveLumFBO; 
    GLuint _computeAveLumTexture;

    // New TMO via mipmapping
    GLuint _tmoTexture;
    GLuint _tmoFramebuffer;
    GLuint _tmoHdrSampler;

    bool _dirtyDeferredcastData;
    bool _dirtyRaycastData;
    bool _dirtyResolution;
    bool _dirtyMsaaSamplingPattern;

    glm::ivec2 _resolution = glm::ivec2(0);
    int _nAaSamples;
    float _hdrExposure = 0.4f;
    float _hdrBackground = 2.8f;
    float _gamma = 2.2f;
    float _maxWhite = 1.0f;
    bool _bloomEnabled = false;
    float _bloomThresholdMin = 0.0;
    float _bloomThresholdMax = 1.0;
    float _bloomOrigFactor = 1.0;
    float _bloomNewFactor = 1.0;
    int _toneMapOperator = 8; // JCC TODO: temporarilly set to 8 because setProperty seems not to be working for OptionProperty
    bool _histogramEnabled = false;
    int _numberOfBins = 1024; // JCC TODO: Add a parameter control for this.
    float _tmoKey = 0.18f;
    float _tmoYwhite = 1e6f;
    float _tmoSaturation = 1.0f;
    float _hue = 1.f;
    float _saturation = 1.f;
    float _value = 1.f;
    float _lightness = 1.f;
    unsigned int _colorSpace = 1;

    std::vector<double> _mSAAPattern;
    std::vector<float> _histoPoints;

    ghoul::Dictionary _rendererData;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
