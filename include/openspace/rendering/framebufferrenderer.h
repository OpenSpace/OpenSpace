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

#ifndef __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
#define __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__

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

class FramebufferRenderer final : public RaycasterListener, public DeferredcasterListener
{
public:
    virtual ~FramebufferRenderer() override = default;

    //============================//
    //=====  Reuse textures  =====//
    //============================//
    /**
     * Gives access to the currently NOT used pingPongTexture. This texture is available
     * for all RenderBins. However, it cannot be used at the same time as the Deferred
     * Caster Tasks. The size of the texture is the resolution of the viewport.
     *
     * NOTE (malej 2023-02-21): The currently NOT used pingPongTexture might change
     * depending on where in the render cycle you are. Especially after the Deferred
     * Caster Tasks.
     *
     * \return The identifier of the currently NOT used pingPongTexture
     */
    GLuint additionalColorTexture1() const;

    /**
     * Gives access to the exitColorTexture. This texture is available for all RenderBins.
     * However, it cannot be used at the same time as the Raycaster Tasks. The size of the
     * texture is the resolution of the viewport.
     *
     * \return The identifier of the exitColorTexture
     */
    GLuint additionalColorTexture2() const;

    /**
     * Gives access to the fxaaTexture. This texture is available for all RenderBins.
     * However, it cannot be used at the same time as the FXAA Task. The size of the
     * texture is the resolution of the viewport.
     *
     * \return The identifier of the fxaaTexture
     */
    GLuint additionalColorTexture3() const;

    /**
     * Gives access to the exitDepthTexture. This texture is available for all RenderBins.
     * However, it cannot be used at the same time as the Raycaster Tasks. The size of the
     * texture is the resolution of the viewport.
     *
     * \return The identifier of the exitDepthTexture
     */
    GLuint additionalDepthTexture() const;

    //=============================//
    //=====  Access G-buffer  =====//
    //=============================//
    // Functions to access the G-buffer textures
    /**
     * Gives access to the color texture of the G-buffer. NOTE: This texture is used for
     * the majority of rendering the scene and might be already in use. Use CAUTION when
     * using this function. The size of the texture is the resolution of the viewport.
     *
     * \return The identifier of the color texture of the G-buffer
     */
    GLuint gBufferColorTexture() const;

    /**
     * Gives access to the position texture of the G-buffer. NOTE: This texture is used
     * for the majority of rendering the scene and might be already in use. Use CAUTION
     * when using this function. The size of the texture is the resolution of the
     * viewport.
     *
     * \return The identifier of the position texture of the G-buffer
     */
    GLuint gBufferPositionTexture() const;

    /**
     * Gives access to the normal texture of the G-buffer. NOTE: This texture is used for
     * the majority of rendering the scene and might be already in use. Use CAUTION when
     * using this function. The size of the texture is the resolution of the viewport.
     *
     * \return The identifier of the normal texture of the G-buffer
     */
    GLuint gBufferNormalTexture() const;

    /**
     * Gives access to the depth texture of the G-buffer. NOTE: This texture is used for
     * the majority of rendering the scene and might be already in use. Use CAUTION when
     * using this function. The size of the texture is the resolution of the viewport.
     *
     * \return The identifier of the depth texture of the G-buffer
     */
    GLuint gBufferDepthTexture() const;

    void initialize();
    void deinitialize();

    void updateResolution();
    void updateRaycastData();
    void updateDeferredcastData();
    void updateHDRAndFiltering();
    void updateFXAA();
    void updateDownscaledVolume();

    void setResolution(glm::ivec2 res);
    void setHDRExposure(float hdrExposure);
    void setGamma(float gamma);
    void setHueValueSaturation(float hue, float value, float saturation);

    void enableFXAA(bool enable);
    void setDisableHDR(bool disable);

    void update();
    void performRaycasterTasks(const std::vector<RaycasterTask>& tasks,
        const glm::ivec4& viewport);
    void performDeferredTasks(const std::vector<DeferredcasterTask>& tasks,
        const glm::ivec4& viewport);
    void render(Scene* scene, Camera* camera, float blackoutFactor);

    /**
     * Update render data. Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData();

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
    void applyTMO(float blackoutFactor, const glm::ivec4& viewport);
    void applyFXAA(const glm::ivec4& viewport);
    void updateDownscaleTextures() const;
    void updateExitVolumeTextures();
    void writeDownscaledVolume(const glm::ivec4& viewport);

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
        Hue, Saturation, Value, Viewport, Resolution) _hdrUniformCache;
    UniformCache(renderedTexture, inverseScreenSize, Viewport,
        Resolution) _fxaaUniformCache;
    UniformCache(downscaledRenderedVolume, downscaledRenderedVolumeDepth, viewport,
        resolution) _writeDownscaledVolumeUniformCache;

    GLint _defaultFBO = 0;
    GLuint _screenQuad = 0;
    GLuint _vertexPositionBuffer = 0;
    GLuint _exitColorTexture = 0;
    GLuint _exitDepthTexture = 0;
    GLuint _exitFramebuffer = 0;

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
