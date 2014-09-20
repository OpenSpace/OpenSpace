/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef __RENDERABLEVOLUMEGL_H__
#define __RENDERABLEVOLUMEGL_H__

// open space includes
#include <openspace/rendering/renderablevolume.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/io/rawvolumereader.h>
#include <ghoul/filesystem/file.h>

#define SGCT_WINDOWS_INCLUDE
#include <sgct.h>

#ifdef __APPLE__
    #include <memory>
#else
    #include <mutex>
#endif

namespace sgct_utils {
    class SGCTBox;
}

namespace openspace {

class RenderableVolumeGL: public RenderableVolume {
public:

	// constructors & destructor
	RenderableVolumeGL(const ghoul::Dictionary& dictionary);
	~RenderableVolumeGL();
    
	bool initialize();
    bool deinitialize();

	virtual void render(const Camera *camera, const psc& thisPosition, RuntimeData* runtimeData);
	virtual void update();

private:
    
    
    std::string _filename;
    ghoul::RawVolumeReader::ReadHints _hints;
    float _stepSize;
	ghoul::opengl::FramebufferObject* _fbo;
	ghoul::opengl::Texture* _backTexture;
	ghoul::opengl::Texture* _frontTexture;
	ghoul::opengl::Texture* _volume;
	ghoul::opengl::ProgramObject *_fboProgram, *_twopassProgram;
	sgct_utils::SGCTBox* _boundingBox;
	GLuint _screenQuad;
    
    std::mutex* _shaderMutex;
    
    ghoul::filesystem::File* _vertexSourceFile;
    ghoul::filesystem::File* _fragmentSourceFile;
    bool _programUpdateOnSave;
    
    void safeShaderCompilation();
    
	RuntimeData* _runtimeData;

};

} // namespace openspace

#endif