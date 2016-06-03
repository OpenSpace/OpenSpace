/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __ABUFFERRENDERER_H__
#define __ABUFFERRENDERER_H__

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>

#include <memory>
#include <string>
#include <vector>
#include <map>


#include <ghoul/opengl/textureunit.h>
#include <openspace/rendering/volume.h>
#include <openspace/rendering/renderer.h>
#include <openspace/rendering/raycasterlistener.h>
#include <openspace/util/updatestructures.h>

namespace ghoul {
    
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
    
class ABufferRenderer : public Renderer, public RaycasterListener {
public:
    ABufferRenderer();
    virtual ~ABufferRenderer();

    void initialize() override;
    void deinitialize() override;

    void setCamera(Camera* camera) override;
    void setScene(Scene* scene) override;
    void setResolution(glm::ivec2 res) override;

    void preRaycast(ghoul::opengl::ProgramObject& programObject);
    void postRaycast(ghoul::opengl::ProgramObject& programObject);

    void update();
    void render(float blackoutFactor, bool doPerformanceMeasurements) override;


    /**
     * Update render data
     * Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData() override;
    virtual void raycastersChanged(VolumeRaycaster& raycaster, bool attached) override;
private:

    void clear();
    void updateResolution();
    void updateRaycastData();
    void updateResolveDictionary();
    
    Camera* _camera;
    Scene* _scene;
    glm::ivec2 _resolution;

    bool _dirtyResolution;
    bool _dirtyRendererData;
    bool _dirtyRaycastData;
    bool _dirtyResolveDictionary;
    
    std::unique_ptr<ghoul::opengl::ProgramObject> _resolveProgram;
    
    /**
     * When a volume is attached or detached from the scene graph,
     * the resolve program needs to be recompiled.
     * The #_volumes map keeps track of which volumes that can
     * be rendered using the current resolve program, along with their raycast data
     * (id, namespace, etc)
     */ 
    std::map<VolumeRaycaster*, RaycastData> _raycastData;
    std::map<VolumeRaycaster*, std::unique_ptr<ghoul::opengl::ProgramObject>> _boundsPrograms;
    std::vector<std::string> _helperPaths;

    ghoul::Dictionary _resolveDictionary;

    GLuint _mainColorTexture;
    GLuint _mainDepthTexture;
    std::unique_ptr<ghoul::opengl::TextureUnit> _mainColorTextureUnit;
    std::unique_ptr<ghoul::opengl::TextureUnit> _mainDepthTextureUnit;

    GLuint _mainFramebuffer;
    GLuint _screenQuad;
    GLuint _anchorPointerTexture;
    GLuint _anchorPointerTextureInitializer;
    GLuint _atomicCounterBuffer;
    GLuint _fragmentBuffer;
    GLuint _fragmentTexture;
    GLuint _vertexPositionBuffer;
    int _nAaSamples;


    std::unique_ptr<RendererTasks> _rendererTasks;
    std::unique_ptr<RenderData> _renderData;
    float _blackoutFactor;

    ghoul::Dictionary _rendererData;
};        // ABufferRenderer
}        // openspace

#endif     // __RENDERER_H__
