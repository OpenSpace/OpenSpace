/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <string>
#include <vector>
#include <map>

#include <openspace/rendering/deferredcasterlistener.h>
#include <openspace/rendering/raycasterlistener.h>
#include <openspace/rendering/renderer.h>
#include <openspace/util/updatestructures.h>

namespace ghoul {
    class Dictionary;
    
    namespace filesystem {
        class File;
    }
    namespace opengl {
        class ProgramObject;
        class Texture;
    }
}

namespace openspace {

class RenderableVolume;
class Camera;
class Scene;
    
class FramebufferRenderer : public Renderer, public RaycasterListener, public DeferredcasterListener {
public:
    FramebufferRenderer();
    virtual ~FramebufferRenderer();

    void initialize() override;
    void deinitialize() override;

    void updateResolution();
    void updateRaycastData();
    void updateDeferredcastData();
    
    void setCamera(Camera* camera) override;
    void setScene(Scene* scene) override;
    void setResolution(glm::ivec2 res) override;
    void setNAaSamples(int nAaSamples) override;

    void update() override;
    void render(float blackoutFactor, bool doPerformanceMeasurements) override;
    
    /**
     * Update render data
     * Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData() override;

    virtual void raycastersChanged(VolumeRaycaster& raycaster, bool attached) override;
    virtual void deferredcastersChanged(Deferredcaster& deferredcaster, bool attached) override;

private:

    std::map<VolumeRaycaster*, RaycastData> _raycastData;
    std::map<VolumeRaycaster*, std::unique_ptr<ghoul::opengl::ProgramObject>> _exitPrograms;
    std::map<VolumeRaycaster*, std::unique_ptr<ghoul::opengl::ProgramObject>> _raycastPrograms;
    std::map<VolumeRaycaster*, std::unique_ptr<ghoul::opengl::ProgramObject>> _insideRaycastPrograms;

    std::map<Deferredcaster*, DeferredcastData> _deferredcastData;
    std::map<Deferredcaster*, std::unique_ptr<ghoul::opengl::ProgramObject>> _deferredcastPrograms;

    std::unique_ptr<ghoul::opengl::ProgramObject> _resolveProgram;

    GLuint _screenQuad;
    GLuint _vertexPositionBuffer;
    GLuint _mainColorTexture;
    GLuint _mainDepthTexture;
    GLuint _exitColorTexture;
    GLuint _mainFramebuffer;
    GLuint _exitDepthTexture;
    GLuint _exitFramebuffer;

    bool _dirtyDeferredcastData;
    bool _dirtyRaycastData;
    bool _dirtyResolution;

    Camera* _camera;
    Scene* _scene;
    glm::vec2 _resolution;
    int _nAaSamples;

    ghoul::Dictionary _rendererData;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___FRAMEBUFFERRENDERER___H__
