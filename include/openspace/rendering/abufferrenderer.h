/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___ABUFFERRENDERER___H__
#define __OPENSPACE_CORE___ABUFFERRENDERER___H__

#ifdef OPENSPACE_WITH_ABUFFER_RENDERER

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/raycasterlistener.h>

#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct RaycasterTask;
class RenderableVolume;
class Camera;
class Scene;
struct RaycastData;

class ABufferRenderer : public Renderer, public RaycasterListener {
public:
    virtual ~ABufferRenderer() = default;

    void initialize() override;
    void deinitialize() override;

    void setResolution(glm::ivec2 res) override;
    void setNAaSamples(int nAaSamples) override;
    void setBlurrinessLevel(int level) override;
    void setHDRExposure(float hdrExposure) override;
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

    int nAaSamples() const override;
    const std::vector<double>& mSSAPattern() const override;

    using Renderer::preRaycast;
    void preRaycast(const RaycasterTask& raycasterTask);
    using Renderer::postRaycast;
    void postRaycast(const RaycasterTask& raycasterTask);

    void update() override;
    void render(Scene* scene, Camera* camera, float blackoutFactor) override;

    /**
     * Update render data
     * Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData() override;
    virtual void raycastersChanged(VolumeRaycaster& raycaster,
        IsAttached attached) override;

private:
    void clear();
    void updateResolution();
    void updateRaycastData();
    void updateResolveDictionary();
    void updateMSAASamplingPattern();
    void saveTextureToMemory(GLenum color_buffer_attachment, int width, int height,
        std::vector<double> & memory) const;

    glm::ivec2 _resolution = glm::ivec2(0);

    bool _dirtyResolution = true;
    bool _dirtyRendererData = true;
    bool _dirtyRaycastData = true;
    bool _dirtyResolveDictionary = true;

    std::unique_ptr<ghoul::opengl::ProgramObject> _resolveProgram = nullptr;

    /**
     * When a volume is attached or detached from the scene graph,
     * the resolve program needs to be recompiled.
     * The _volumes map keeps track of which volumes that can
     * be rendered using the current resolve program, along with their raycast data
     * (id, namespace, etc)
     */
    std::map<VolumeRaycaster*, RaycastData> _raycastData;
    std::map<
        VolumeRaycaster*, std::unique_ptr<ghoul::opengl::ProgramObject>
    > _boundsPrograms;
    std::vector<std::string> _helperPaths;

    ghoul::Dictionary _resolveDictionary;

    GLuint _mainColorTexture;
    GLuint _mainDepthTexture;

    GLuint _mainFramebuffer;
    GLuint _screenQuad;
    GLuint _anchorPointerTexture;
    GLuint _anchorPointerTextureInitializer;
    GLuint _atomicCounterBuffer;
    GLuint _fragmentBuffer;
    GLuint _fragmentTexture;
    GLuint _vertexPositionBuffer;
    int _nAaSamples;
    int _blurrinessLevel = 1;

    float _hdrExposure = 0.4f;
    float _hdrBackground = 2.8f;
    float _gamma = 2.2f;
    float _maxWhite = 1.f;
    float _blackoutFactor;
    bool _bloomEnabled = false;
    float _bloomThresholdMin = 0.0;
    float _bloomThresholdMax = 1.0;
    float _bloomOrigFactor = 1.0;
    float _bloomNewFactor = 1.0;
    int _toneMapOperator = 0;
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

    ghoul::Dictionary _rendererData;
};

} // namespace openspace

#endif // OPENSPACE_WITH_ABUFFER_RENDERER

#endif // __OPENSPACE_CORE___ABUFFERRENDERER___H__
