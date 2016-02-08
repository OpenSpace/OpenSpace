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

#include <openspace/rendering/abufferrenderer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/dictionary.h>

#include <string>

namespace {
	const std::string _loggerCat = "ABufferRenderer";
    const int MaxLayers = 16;
}

namespace openspace {


    ABufferRenderer::ABufferRenderer()
        : _camera(nullptr)
        , _scene(nullptr)
        , _resolution(glm::ivec2(0))
        , _dirtyResolution(true)
        , _resolveProgram(nullptr) { }

ABufferRenderer::~ABufferRenderer() {}


void ABufferRenderer::initialize() {
    LINFO("Initializing ABufferRenderer");
    const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = {
        //	  x      y     s     t
        -size, -size, 0.0f, 1.0f,
        size,	size, 0.0f, 1.0f, 
        -size,  size, 0.0f, 1.0f, 
        -size, -size, 0.0f, 1.0f, 
        size, -size, 0.0f, 1.0f, 
        size,	size, 0.0f, 1.0f,
    };


    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(0);

	glGenTextures(1, &_anchorPointerTexture);
	glGenBuffers(1, &_anchorPointerTextureInitializer);
	glGenBuffers(1, &_atomicCounterBuffer);
	glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, _atomicCounterBuffer);
	glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);
	glGenBuffers(1, &_fragmentBuffer);
    glGenTextures(1, &_fragmentTexture);
    
    if (_dirtyResolution) {
        updateResolution();
    }
    updateRendererData();
    ghoul::Dictionary dict = createResolveDictionary();

    _resolveProgram = ghoul::opengl::ProgramObject::Build("ABuffer Resolve",
		"${SHADERS}/abuffer/resolveabuffer.vert",
		"${SHADERS}/abuffer/resolveabuffer.frag",
		dict);

}
    
void ABufferRenderer::deinitialize() {
    LINFO("Deinitializing ABufferRenderer");
	glDeleteBuffers(1, &_fragmentBuffer);
    glDeleteTextures(1, &_fragmentTexture);

	glDeleteTextures(1, &_anchorPointerTexture);
	glDeleteBuffers(1, &_anchorPointerTextureInitializer);
	glDeleteBuffers(1, &_atomicCounterBuffer);

    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);
}
    
void ABufferRenderer::update() {
    bool dirtyRendererData = false;
    bool dirtyResolveDictionary = false;
    
    if (_dirtyResolution) {
        updateResolution();
    }
        
    if (dirtyRendererData) {
        dirtyResolveDictionary = true;
        updateRendererData();
    }

    // TODO: Collect volumes from scene graph.
    // Diff against cache, update cache.
    // possibly mark resolve dictionary as dirty
    
    if (dirtyResolveDictionary) {
        ghoul::Dictionary dict = createResolveDictionary();
        _resolveProgram->setDictionary(dict);
    }

    if (_resolveProgram->isDirty()) {
        _resolveProgram->rebuildFromFile();
    }
}

    
void ABufferRenderer::render(float blackoutFactor, bool doPerformanceMeasurements) {
    if (_scene == nullptr) return;
    if (_camera == nullptr) return;
        
    // Reset
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    clear();

    // Step 1: Render geometries to the fragment buffer

	// Bind head-pointer image for read-write
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
    glBindImageTexture(0, _anchorPointerTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32UI);

    _scene->render({ *_camera, psc(), doPerformanceMeasurements });
        
    // Step 2: Render volumes to the fragment buffer
    //TODO: Implement this

    // Step 3: Resolve the buffer
    _resolveProgram->activate();
    _resolveProgram->setUniform("blackoutFactor", blackoutFactor);
    // todo: pre-ray-cast for each volume
    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    // todo: post-ray-cast for each volume
    _resolveProgram->deactivate();
}


void ABufferRenderer::setScene(Scene* scene) {
    _scene = scene;
}

void ABufferRenderer::setCamera(Camera* camera) {
    _camera = camera;
}

void ABufferRenderer::setResolution(glm::ivec2 res) {
    if (res != _resolution) {
        _resolution = res;
        _dirtyResolution = true;
    }
}

void ABufferRenderer::clear() {
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);

	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _resolution.x, _resolution.y, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	static const GLuint zero = 1;
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
	glBufferSubData(GL_ATOMIC_COUNTER_BUFFER, 0, sizeof(zero), &zero);
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, 0);
}

void ABufferRenderer::updateResolution() {
    int totalPixels = _resolution.x * _resolution.y;
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _resolution.x, _resolution.y, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

	GLuint* data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
	memset(data, 0x00, totalPixels * sizeof(GLuint));
	glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glBufferData(GL_TEXTURE_BUFFER, MaxLayers*totalPixels*sizeof(GLuint) * 4, NULL, GL_DYNAMIC_COPY);

	glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
	glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
	glBindTexture(GL_TEXTURE_BUFFER, 0);

	glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

    
    _dirtyResolution = false;
    
}

ghoul::Dictionary ABufferRenderer::createResolveDictionary() {
    ghoul::Dictionary dict;

    dict.setValue("rendererData", _rendererData);
    // TODO: Add volume data to dictionary
    // such as id, bitmask, path to raycaster frag shader, helpers, nVolumes...
    return dict;
}

void ABufferRenderer::updateRendererData() {
    ghoul::Dictionary dict;
    dict.setValue("windowWidth", OsEng.windowWrapper().currentWindowResolution().x);
    dict.setValue("windowHeight", OsEng.windowWrapper().currentWindowResolution().y);
    dict.setValue("maxLayers", MaxLayers);
    dict.setValue("fragmentRendererPath", std::string("${SHADERS}/abuffer/renderabuffer.frag"));
    _rendererData = dict;

    OsEng.renderEngine().setRendererData(dict);
}

}
