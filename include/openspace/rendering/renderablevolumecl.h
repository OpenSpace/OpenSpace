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

#ifndef __RENDERABLEVOLUMECL_H__
#define __RENDERABLEVOLUMECL_H__

// open space includes
#include <openspace/rendering/renderablevolume.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opencl/clcontext.h>
#include <ghoul/opencl/clcommandqueue.h>
#include <ghoul/opencl/clprogram.h>
#include <ghoul/opencl/clkernel.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/io/rawvolumereader.h>
#include <ghoul/filesystem/file.h>

#define SGCT_WINDOWS_INCLUDE
#include <sgct.h>

//#include <vector>
//#include <string>
#ifdef __APPLE__
    #include <memory>
#else
    #include <mutex>
#endif

namespace openspace {

class RenderableVolumeCL: public RenderableVolume {
public:

	// constructors & destructor
	RenderableVolumeCL(const ghoul::Dictionary& dictionary);
	~RenderableVolumeCL();
    
    bool initialize();
    bool deinitialize();

	virtual void render(const Camera *camera, const psc& thisPosition);
	virtual void update();

private:

    void safeKernelCompilation();

    std::string _filename;
    ghoul::RawVolumeReader::ReadHints _hints;
    float _stepSize;
	ghoul::opengl::FramebufferObject* _fbo;
	ghoul::opengl::Texture* _backTexture;
	ghoul::opengl::Texture* _frontTexture;
	ghoul::opengl::Texture* _volume;
	ghoul::opengl::Texture* _output;
	ghoul::opengl::ProgramObject *_fboProgram;
	ghoul::opengl::ProgramObject *_quadProgram;
	sgct_utils::SGCTBox* _boundingBox;
	GLuint _screenQuad;
    
    ghoul::opencl::CLContext _context;
    ghoul::opencl::CLCommandQueue _commands;
    ghoul::opencl::CLProgram _program;
    ghoul::opencl::CLKernel _kernel;
    ghoul::opencl::CLWorkSize* _ws;
    cl_mem _clBackTexture, _clFrontTexture, _clVolume, _clOutput;
    
    ghoul::filesystem::File* _kernelSourceFile;
    bool _kernelUpdateOnSave;
    std::mutex* _kernelMutex;
    
};

} // namespace openspace

#endif