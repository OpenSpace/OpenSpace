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

#ifndef __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
#define __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/renderengine.h>
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
    virtual ~FramebufferRenderer() = default;

    void initialize() override;
    void deinitialize() override;

    void updateResolution();
    void updateRaycastData();
    void updateDeferredcastData();
    void updateHDRAndFiltering();
    void updateFXAA();
    void updateDownscaledVolume();

    void setResolution(glm::ivec2 res) override;
    void setHDRExposure(float hdrExposure) override;
    void setGamma(float gamma) override;
    void setHue(float hue) override;
    void setValue(float value) override;
    void setSaturation(float sat) override;

    void enableFXAA(bool enable) override;
    void setDisableHDR(bool disable) override;

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
    using RaycasterProgObjMap = std::map<
        VolumeRaycaster*,
        std::unique_ptr<ghoul::opengl::ProgramObject>
    >;
    using DeferredcasterProgObjMap = std::map<
        Deferredcaster*,
        std::unique_ptr<ghoul::opengl::ProgramObject>
    >;

    void resolveMSAA(float blackoutFactor);
    void applyTMO(float blackoutFactor);
    void applyFXAA();
    void updateDownscaleTextures();
    void updateExitVolumeTextures();
    void writeDownscaledVolume();

    std::map<VolumeRaycaster*, RaycastData> _raycastData;
    RaycasterProgObjMap _exitPrograms;
    RaycasterProgObjMap _raycastPrograms;
    RaycasterProgObjMap _insideRaycastPrograms;

    std::map<Deferredcaster*, DeferredcastData> _deferredcastData;
    DeferredcasterProgObjMap _deferredcastPrograms;

    std::unique_ptr<ghoul::opengl::ProgramObject> _hdrFilteringProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _tmoProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _fxaaProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _downscaledVolumeProgram;

    UniformCache(hdrFeedingTexture, blackoutFactor, hdrExposure, gamma,
                 Hue, Saturation, Value) _hdrUniformCache;
    UniformCache(renderedTexture, inverseScreenSize) _fxaaUniformCache;
    UniformCache(downscaledRenderedVolume, downscaledRenderedVolumeDepth)
        _writeDownscaledVolumeUniformCache;

    GLint _defaultFBO;
    GLuint _screenQuad;
    GLuint _vertexPositionBuffer;
    GLuint _exitColorTexture;
    GLuint _exitDepthTexture;
    GLuint _exitFramebuffer;

    struct {
        GLuint colorTexture;
        GLuint positionTexture;
        GLuint normalTexture;
        GLuint depthTexture;
        GLuint framebuffer;
    } _gBuffers;

    struct {
        GLuint framebuffer;
        GLuint colorTexture[2];
    } _pingPongBuffers;

    struct {
        GLuint hdrFilteringFramebuffer;
        GLuint hdrFilteringTexture;
    } _hdrBuffers;

    struct {
        GLuint fxaaFramebuffer;
        GLuint fxaaTexture;
    } _fxaaBuffers;

    struct {
        GLuint framebuffer;
        GLuint colorTexture;
        GLuint depthbuffer;
        float currentDownscaleFactor  = 1.f;
    } _downscaleVolumeRendering;

    unsigned int _pingPongIndex = 0u;

    bool _dirtyDeferredcastData;
    bool _dirtyRaycastData;
    bool _dirtyResolution;

    glm::ivec2 _resolution = glm::ivec2(0);
    int _nAaSamples;
    bool _enableFXAA = true;
    bool _disableHDR = false;

    float _hdrExposure = 3.7f;
    float _gamma = 0.95f;
    float _hue = 1.f;
    float _saturation = 1.f;
    float _value = 1.f;

    ghoul::Dictionary _rendererData;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
