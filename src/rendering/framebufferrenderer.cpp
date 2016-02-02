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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <string>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>

namespace {
	const std::string _loggerCat = "FramebufferRenderer";
    const std::string FragmentRendererPath = "${SHADERS}/renderframebuffer.frag";
}

namespace openspace {


    FramebufferRenderer::FramebufferRenderer()
        : _camera(nullptr)
        , _scene(nullptr)
        , _resolution(glm::vec2(0)) {
    }


    FramebufferRenderer::~FramebufferRenderer() {
        
    }
    

    void FramebufferRenderer::initialize() {
        updateRendererData();
    }

    void FramebufferRenderer::deinitialize() {
    }
    

    void FramebufferRenderer::update() {
        // no need to update anything.
    }
    
    void FramebufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {
        if (_scene == nullptr) return;
        if (_camera == nullptr) return;

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        
       _scene->render({ *_camera, psc(), doPerformanceMeasurements });;

    }

    void FramebufferRenderer::setScene(Scene* scene) {
        _scene = scene;
    }

    void FramebufferRenderer::setCamera(Camera* camera) {
        _camera = camera;
    }

    void FramebufferRenderer::setResolution(glm::ivec2 res) {
        _resolution = res;
    }

    void FramebufferRenderer::updateRendererData() {
        ghoul::Dictionary dict;
        dict.setValue("fragmentRendererPath", FragmentRendererPath);
        OsEng.renderEngine()->setRendererData(dict);
    }


}

