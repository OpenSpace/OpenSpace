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

// open space includes
#include <openspace/rendering/renderablevolumecl.h>

#include <openspace/engine/openspaceengine.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <algorithm>

namespace {
    std::string _loggerCat = "RenderableVolumeCL";
}

namespace openspace {

RenderableVolumeCL::RenderableVolumeCL(const ghoul::Dictionary& dictionary):
    RenderableVolume(dictionary),
    _backTexture(nullptr), _frontTexture(nullptr), _output(nullptr),
    _clBackTexture(0), _clFrontTexture(0), _clOutput(0),
    _kernelSourceFile(nullptr) {
        
    _kernelMutex = new std::mutex;
    
    _filename = "";
    if(dictionary.hasKey("Volume")) {
        if(dictionary.getValue("Volume", _filename)) {
            _filename = findPath(_filename);
        }
    }
    ghoul::Dictionary hintsDictionary;
    if(dictionary.hasKey("Hints"))
        dictionary.getValue("Hints", hintsDictionary);
    _hints = readHints(hintsDictionary);
    
    /*
    if(dictionary.hasKey("TransferFunctions")) {
        ghoul::Dictionary transferFunctions;
        if(dictionary.getValue("TransferFunctions", transferFunctions)) {
            auto keys = transferFunctions.keys();
            for(auto key: keys) {
                std::string transferFunctionPath = "";
                if(transferFunctions.getValue(key, transferFunctionPath)) {
                    transferFunctionPath = findPath(transferFunctionPath);
                    if(transferFunctionPath != "") {
                        ghoul::filesystem::File* tmp = new ghoul::filesystem::File(transferFunctionPath, false);
                        ghoul::opengl::Texture* tmpTexture = ghoul::opengl::loadTexture(tmp->path());
                        
                        _transferFunctions.push_back(tmpTexture);
                        _transferFunctionsFiles.push_back(tmp);
                    }
                }
            }
        }
    }
    */
    
    if(dictionary.hasKey("UpdateOnSave")) {
        dictionary.getValue("UpdateOnSave", _kernelUpdateOnSave);
    }
    
    /*
    if(dictionary.hasKey("KernelOptions")) {
        using namespace ghoul::opencl;
        ghoul::Dictionary kernelOptions;
        if(dictionary.getValue("KernelOptions", kernelOptions)) {
            auto keys = kernelOptions.keys();
            for(auto key: keys) {
                bool value = false;
                if(kernelOptions.getValue(key, value)) {
                    if(key == "DenormsAreZero") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::DenormsAreZero, value));
                    } else if(key == "FastRelaxedMath") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::FastRelaxedMath, value));
                    } else if(key == "FiniteMathOnly") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::FiniteMathOnly, value));
                    } else if(key == "KernelArgInfo") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::KernelArgInfo, value));
                    } else if(key == "MadEnable") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::MadEnable, value));
                    } else if(key == "NoSignedZero") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::NoSignedZero, value));
                    } else if(key == "OptDisable") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::OptDisable, value));
                    } else if(key == "SinglePrecisionConstant") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::SinglePrecisionConstant, value));
                    } else if(key == "StrictAliasing") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::StrictAliasing, value));
                    } else if(key == "UnsafeMathOptimizations") {
                        _kernelOptions.push_back(std::make_pair(CLProgram::Option::UnsafeMathOptimizations, value));
                    }
                }
            }
        }
    }
    */
        
    std::string kernelPath = "";
    if (dictionary.hasKey("Kernel")) {
        if(dictionary.getValue("Kernel", kernelPath)) {
            kernelPath = findPath(kernelPath);
        }
    }
    if (kernelPath != "") {
        _kernelSourceFile = new ghoul::filesystem::File(kernelPath, false);
    }
    
}

RenderableVolumeCL::~RenderableVolumeCL() {
    deinitialize();
    delete _kernelMutex;
}

bool RenderableVolumeCL::initialize() {
    assert(_filename != "");
    //	------ VOLUME READING ----------------
	ghoul::RawVolumeReader rawReader(_hints);
	_volume = rawReader.read(_filename);
    glm::size3_t d = _volume->dimensions();
    
    //	------ SETUP GEOMETRY ----------------
	const GLfloat size = 1.0f;
	const GLfloat vertex_texcoord_data[] = { // square of two triangles (sigh)
        //	  x      y     z     s     t
        -size, -size, 0.0f, 0.0f, 0.0f,
        size,	size, 0.0f, 1.0f, 1.0f,
        -size,  size, 0.0f, 0.0f, 1.0f,
        -size, -size, 0.0f, 0.0f, 0.0f,
        size, -size, 0.0f, 1.0f, 0.0f,
        size,	size, 0.0f, 1.0f, 1.0f
    };
    
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_screenQuad); // generate array
	glBindVertexArray(_screenQuad); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_texcoord_data), vertex_texcoord_data, GL_STATIC_DRAW);
    
	// Vertex positions
	GLuint vertexLocation = 2;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), reinterpret_cast<void*>(0));
    
	// Texture coordinates
	GLuint texcoordLocation = 0;
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 2, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), (void*)(3*sizeof(GLfloat)));
    
	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array
    
	_boundingBox = new sgct_utils::SGCTBox(1.0f, sgct_utils::SGCTBox::Regular);
    
    //	------ SETUP SHADERS -----------------
    // TODO error control or better design pattern
	OsEng.ref().configurationManager().getValue("RaycastProgram", _fboProgram);
	OsEng.ref().configurationManager().getValue("Quad", _quadProgram);
    
    //	------ SETUP FBO ---------------------
	_fbo = new ghoul::opengl::FramebufferObject();
	_fbo->activate();
    
	size_t x = sgct::Engine::instance()->getActiveXResolution();
	size_t y = sgct::Engine::instance()->getActiveYResolution();
	_backTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_frontTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_output = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_backTexture->uploadTexture();
	_frontTexture->uploadTexture();
	_output->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);
    
	_fbo->deactivate();
    
    _context = OsEng.clContext();
    _commands = _context.createCommandQueue();
    
    _clBackTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_backTexture);
    _clFrontTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_frontTexture);
    _clVolume = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_volume);
    _clOutput = _context.createTextureFromGLTexture(CL_MEM_WRITE_ONLY, *_output);
    
    auto privateCallback = [this](const ghoul::filesystem::File& file) {
        safeKernelCompilation();
    };
    
    _kernelSourceFile = new ghoul::filesystem::File(_kernelSourceFile->path(), false);
    if(_kernelUpdateOnSave)
        _kernelSourceFile->setCallback(privateCallback);
   
    safeKernelCompilation();
    
    size_t local_x = 32;
    size_t local_y = 32;
    while (local_x > 1) {
        if(x % local_x == 0)
            break;
        local_x /= 2;
    }
    while (local_y > 1) {
        if(y % local_y == 0)
            break;
        local_y /= 2;
    }
    _ws = new ghoul::opencl::CLWorkSize ({x,y}, {local_x,local_y});
    
    return true;

}

bool RenderableVolumeCL::deinitialize() {

    delete _ws;
    _ws = nullptr;
    
    return true;
}

void RenderableVolumeCL::render(const Camera *camera, const psc &thisPosition) {
    
    if( ! _kernel.isValidKernel())
        return;

	float speed = 50.0f;
	float time = sgct::Engine::getTime();
    glm::mat4 transform = camera->getViewProjectionMatrix();
    
    double factor = pow(10.0,thisPosition[3]);
    transform = glm::translate(transform, glm::vec3(thisPosition[0]*factor, thisPosition[1]*factor, thisPosition[2]*factor));
	transform = glm::rotate(transform, time*speed, glm::vec3(0.0f, 1.0f, 0.0f));
	
        
    if(_kernel.isValidKernel()) {
        _stepSize = 0.01f;
        
        //	------ DRAW TO FBO -------------------
        GLuint sgctFBO = ghoul::opengl::FramebufferObject::getActiveObject(); // Save SGCTs main FBO
        _fbo->activate();
        _fboProgram->activate();
        _fboProgram->setUniform("modelViewProjection", transform);
        
        //	Draw backface
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glClearColor(0.2f, 0.2f, 0.2f, 0);
        glClear(GL_COLOR_BUFFER_BIT);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        _boundingBox->draw();
        glDisable(GL_CULL_FACE);
        
        //	Draw frontface
        glDrawBuffer(GL_COLOR_ATTACHMENT1);
        glClear(GL_COLOR_BUFFER_BIT);
        glClearColor(0.2f, 0.2f, 0.2f, 0);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        _boundingBox->draw();
        glDisable(GL_CULL_FACE);
        
        _fboProgram->deactivate();
        _fbo->deactivate();
        
        //	------ DRAW TO SCREEN ----------------
        glBindFramebuffer(GL_FRAMEBUFFER, sgctFBO); // Re-bind SGCTs main FBO
        
        
        
        glFinish();
        _commands.enqueueKernelBlocking(_kernel, *_ws);
        _commands.finish();
        _quadProgram->activate();
        glActiveTexture(GL_TEXTURE0);
        _output->bind();
        glClearColor(0.0f, 0.0f, 0.0f, 0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);
        
        _quadProgram->deactivate();
        
    }
    
    

}

void RenderableVolumeCL::update() {
    
}

void RenderableVolumeCL::safeKernelCompilation() {
    std::string _loggerCat = "RenderableVolumeCL::safeKernelCompilation";
    if(_context.isValidContext()) {
        
        ghoul::opencl::CLProgram tmpProgram = _context.createProgram(_kernelSourceFile->path());
        tmpProgram.setOption(ghoul::opencl::CLProgram::Option::OptDisable, true);
        tmpProgram.setOption(ghoul::opencl::CLProgram::Option::KernelArgInfo, true);
        if(tmpProgram.build()) {
            ghoul::opencl::CLKernel tmpKernel = tmpProgram.createKernel("volumeraycaster");
            if(tmpKernel.isValidKernel()) {
                tmpKernel.setArgument(0, &_clFrontTexture);
                tmpKernel.setArgument(1, &_clBackTexture);
                tmpKernel.setArgument(2, &_clVolume);
                tmpKernel.setArgument(3, &_clOutput);
                
                // do the actual assignment behind locked doors
                _kernelMutex->lock();
                _program = tmpProgram;
                _kernel = tmpKernel;
                _kernelMutex->unlock();
                
                LDEBUG("Done updating kernel");
            } else {
                LWARNING("Kernel is not valid");
            }
        } else {
            LWARNING("Could not build CLProgram");
        }
    } else {
        LWARNING("No valid CLContext");
    }
}
    /*
void RenderableVolumeCL::safeUpdateTexture(const ghoul::filesystem::File& file) {
    int fileID = 0;
    for (fileID = 0; fileID < _transferFunctionsFiles.size(); ++fileID) {
        if (_transferFunctionsFiles.at(fileID) == &file) {
            //LDEBUG("Transferfunction found at id " << fileID);
            break;
        }
    }
    if(fileID == _transferFunctionsFiles.size())
        return;

    LDEBUG("Updating transferfunction");
    // create the new texture
    ghoul::opengl::Texture* newTexture = ghoul::opengl::loadTexture(file.path());
    
    if(newTexture) {
        
        // upload the new texture and create a cl memory
        newTexture->uploadTexture();
        cl_mem clNewTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *newTexture);
        
        if(clNewTexture == 0) {
            delete newTexture;
            return;
        }
        
        // everything is ok, critical point to replace current texture pointers
        _textureLock->lock();
        
        // deallocate current texture
        delete _transferFunctions.at(fileID);
        clReleaseMemObject(_clTransferFunctions.at(fileID));
        
        // set the new texture
        _transferFunctions.at(fileID) = newTexture;
        _clTransferFunctions.at(fileID) = clNewTexture;
        
        // update kernel
        // __kernel arguments(front, back, output, [_volumes], .. fileID))
        _kernel.setArgument(3 + _volumes.size() + fileID, &clNewTexture);
        
        // end of critical section
        _textureLock->unlock();
    }
}
*/

} // namespace openspace