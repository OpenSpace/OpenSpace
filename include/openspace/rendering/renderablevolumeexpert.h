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

#ifndef __RENDERABLEVOLUMEEXPERT_H__
#define __RENDERABLEVOLUMEEXPERT_H__

// open space includes
#include <openspace/rendering/renderablevolume.h>
#include <openspace/rendering/volumeraycasterbox.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opencl/clcontext.h>
#include <ghoul/opencl/clcommandqueue.h>
#include <ghoul/opencl/clprogram.h>
#include <ghoul/opencl/clkernel.h>
#include <ghoul/io/rawvolumereader.h>
#include <ghoul/filesystem/file.h>

#ifdef __APPLE__
    #include <memory>
#else
    #include <mutex>
#endif

namespace ghoul {
    namespace opencl {
        class CLWorkSize;
    }
}

namespace openspace {

class RenderableVolumeExpert: public RenderableVolume {
public:

	// constructors & destructor
	RenderableVolumeExpert(const ghoul::Dictionary& dictionary);
	~RenderableVolumeExpert();
    
    bool initialize();
    bool deinitialize();

	virtual void render(const Camera *camera, const psc& thisPosition);
	virtual void update();

private:

    // private methods
    void safeKernelCompilation();
    void safeUpdateTexture(const ghoul::filesystem::File& file);

    // Volumes
    std::vector<std::string> _volumePaths;
    std::vector<ghoul::RawVolumeReader::ReadHints> _volumeHints;
    
    // Textures
	ghoul::opengl::Texture* _output;
	std::vector<ghoul::opengl::Texture*> _volumes;
	std::vector<ghoul::opengl::Texture*> _transferFunctions;
	std::vector<ghoul::filesystem::File*> _transferFunctionsFiles;
    
    // opencl texture memory pointers
    cl_mem _clBackTexture;
    cl_mem _clFrontTexture;
    cl_mem _clOutput;
    std::vector<cl_mem> _clVolumes;
    std::vector<cl_mem> _clTransferFunctions;
    
    // opencl program
    ghoul::opencl::CLContext _context;
    ghoul::opencl::CLCommandQueue _commands;
    ghoul::opencl::CLProgram _program;
    ghoul::opencl::CLKernel _kernel;
    ghoul::opencl::CLWorkSize* _ws;
    ghoul::filesystem::File* _kernelSourceFile;
    std::vector<std::pair<ghoul::opencl::CLProgram::Option, bool> > _kernelOptions;
    bool _programUpdateOnSave;
    
    // mutexes to prevent inconsistencies
    std::mutex* _kernelLock;
    std::mutex* _textureLock;
    
	ghoul::opengl::ProgramObject *_quadProgram;
    sgct_utils::SGCTBox* _boundingBox;
	GLuint _screenQuad;
    
    VolumeRaycasterBox* _colorBoxRenderer;
    
};

} // namespace openspace

#endif