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
#include <openspace/rendering/renderablevolumeexpert.h>

#include <openspace/engine/openspaceengine.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/rawvolumereader.h>

#include <algorithm>

namespace {
    std::string _loggerCat = "RenderableVolumeExpert";
    
    size_t countKernelArguments(ghoul::opencl::CLKernel& kernel) {
        
        using ghoul::opencl::CLKernel;
        CLKernel::AddressQualifier adq;
        CLKernel::AccessQualifier acq;
        CLKernel::TypeQualifier atq;
        
        size_t arguments = 0;
        do {
            adq = kernel.argumentAddressQualifier(arguments);
            acq = kernel.argumentAccessQualifier(arguments);
            atq = kernel.argumentTypeQualifier(arguments);
            ++arguments;
        } while (adq != CLKernel::AddressQualifier::Error &&
                 acq != CLKernel::AccessQualifier::Error &&
                 atq != CLKernel::TypeQualifier::Error);
        
        return arguments - 1;
    }
}

namespace openspace {

RenderableVolumeExpert::RenderableVolumeExpert(const ghoul::Dictionary& dictionary):
    RenderableVolume(dictionary),
    _output(nullptr),
    _clBackTexture(0), _clFrontTexture(0), _clOutput(0),
    _kernelSourceFile(nullptr), _programUpdateOnSave(false), _colorBoxRenderer(nullptr),
    _boxScaling(1.0,1.0,1.0) {
        
    _kernelLock = new std::mutex;
    _textureLock = new std::mutex;
    
    if(dictionary.hasKey("Volumes")) {
        ghoul::Dictionary volumes;
        if(dictionary.getValue("Volumes", volumes)) {
            auto keys = volumes.keys();
            for(auto key: keys) {
                ghoul::Dictionary volume;
                if(volumes.getValue(key, volume)) {
                    if (volume.hasKey("File")) {
                        std::string file = "";
                        if (volume.getValue("File", file)) {
                            file = findPath(file);
                            if (file != "") {
                                
                                // parse hints
                                ghoul::Dictionary hintsDictionary;
                                if(volume.hasKey("Hints"))
                                    volume.getValue("Hints", hintsDictionary);
                                
                                _volumePaths.push_back(file);
                                _volumeHints.push_back(hintsDictionary);
                            }
                        }
                    }
                }
            }
        }
    }
    
    if(dictionary.hasKey("TransferFunctions")) {
        ghoul::Dictionary transferFunctions;
        if(dictionary.getValue("TransferFunctions", transferFunctions)) {
            auto keys = transferFunctions.keys();
            for(auto key: keys) {
                std::string transferFunctionPath = "";
                if(transferFunctions.getValue(key, transferFunctionPath)) {
                    transferFunctionPath = findPath(transferFunctionPath);
                    ghoul::opengl::Texture* tmpTexture = loadTransferFunction(transferFunctionPath);
                    if(tmpTexture) {
                        ghoul::filesystem::File* tmp = new ghoul::filesystem::File(transferFunctionPath, false);
                        _transferFunctions.push_back(tmpTexture);
                        _transferFunctionsFiles.push_back(tmp);
                    }
                }
            }
        }
    }
    
    std::string kernelPath = "";
    if (dictionary.hasKey("Kernel")) {
        ghoul::Dictionary kernelDictionary;
        if(dictionary.getValue("Kernel", kernelDictionary)) {
            if(kernelDictionary.getValue("Source", kernelPath)) {
                kernelPath = findPath(kernelPath);
                
            }
            if(kernelDictionary.hasKey("UpdateOnSave")) {
                kernelDictionary.getValue("UpdateOnSave", _programUpdateOnSave);
            }
            
            if(kernelDictionary.hasKey("Options")) {
                using namespace ghoul::opencl;
                ghoul::Dictionary kernelOptions;
                if(kernelDictionary.getValue("Options", kernelOptions)) {
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
            
            ghoul::Dictionary includeDictionary;
            if (kernelDictionary.hasKey("Includes") && kernelDictionary.getValue("Includes", includeDictionary)) {
                auto keys = includeDictionary.keys();
                for(auto key: keys) {
                    std::string includePath;
                    if(includeDictionary.getValue(key, includePath)) {
                        if(FileSys.directoryExists(includePath)) {
                            _kernelIncludes.push_back(absPath(includePath));
                        }
                    }
                }
            }
            
            ghoul::Dictionary defineDictionary;
            if (kernelDictionary.hasKey("Definitions") && kernelDictionary.getValue("Definitions", defineDictionary)) {
                auto keys = defineDictionary.keys();
                for(auto key: keys) {
                    std::string defintion;
                    if(defineDictionary.getValue(key, defintion)) {
                        _kernelDefinitions.push_back(std::make_pair(key, defintion));
                    }
                }
            }
        }
    }
    if (kernelPath != "") {
        _kernelSourceFile = new ghoul::filesystem::File(kernelPath, false);
    }
        
    _colorBoxRenderer = new VolumeRaycasterBox();
    double tempValue;
    if(dictionary.hasKey("BoxScaling.1") && dictionary.getValue("BoxScaling.1", tempValue)) {
        if(tempValue > 0.0) {
            _boxScaling[0] = tempValue;
        }
    }
    if(dictionary.hasKey("BoxScaling.2") && dictionary.getValue("BoxScaling.2", tempValue)) {
        if(tempValue > 0.0) {
            _boxScaling[1] = tempValue;
        }
    }
    if(dictionary.hasKey("BoxScaling.3") && dictionary.getValue("BoxScaling.3", tempValue)) {
        if(tempValue > 0.0) {
            _boxScaling[2] = tempValue;
        }
    }
    
    setBoundingSphere(pss::CreatePSS(_boxScaling.length()));
}

RenderableVolumeExpert::~RenderableVolumeExpert() {
    deinitialize();
    delete _textureLock;
    delete _kernelLock;
}

bool RenderableVolumeExpert::initialize() {
    if(_kernelSourceFile == nullptr) {
        LERROR("Could not find the kernel file!");
        return false;
    }
    
    _context = OsEng.clContext();
    
    auto textureCallback = [this](const ghoul::filesystem::File& file) {
        safeUpdateTexture(file);
    };
    auto kernelCallback = [this](const ghoul::filesystem::File& file) {
        safeKernelCompilation();
    };
    
    for(auto texture: _transferFunctions) {
        texture->uploadTexture();
        cl_mem transferMem = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *texture);
        _clTransferFunctions.push_back(transferMem);
    }
    if(_programUpdateOnSave) {
        _kernelSourceFile->setCallback(kernelCallback);
        for(auto texture: _transferFunctionsFiles) {
            texture->setCallback(textureCallback);
        }
    }
    
    for (int i = 0; i < _volumePaths.size(); ++i) {
        ghoul::opengl::Texture* volume = loadVolume(_volumePaths.at(i), _volumeHints.at(i));
        if(volume) {
            volume->uploadTexture();
            
            LDEBUG("Creating CL texture from GL texture with path '" << _volumePaths.at(i) << "'");
            cl_mem volumeTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *volume);
            
            _volumes.push_back(volume);
            _clVolumes.push_back(volumeTexture);
        }
    }
    
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
    
	if( ! OsEng.ref().configurationManager().getValue("Quad", _quadProgram)) {
        LERROR("Could not find 'Quad'");
        return false;
    }
    
    _colorBoxRenderer->initialize();
    glm::size2_t dimensions = _colorBoxRenderer->dimensions();
	ghoul::opengl::Texture* backTexture = _colorBoxRenderer->backFace();
	ghoul::opengl::Texture* frontTexture = _colorBoxRenderer->frontFace();
	_output = new ghoul::opengl::Texture(glm::size3_t(dimensions[0],dimensions[1],1));
	_output->uploadTexture();
    
    _context = OsEng.clContext();
    _commands = _context.createCommandQueue();
    
    _clBackTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *backTexture);
    _clFrontTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *frontTexture);
    _clOutput = _context.createTextureFromGLTexture(CL_MEM_WRITE_ONLY, *_output);
    
    // Compile kernels
    safeKernelCompilation();
    
    // create work group
    size_t local_x = 32;
    size_t local_y = 32;
    while (local_x > 1) {
        if(dimensions[0] % local_x == 0)
            break;
        local_x /= 2;
    }
    while (local_y > 1) {
        if(dimensions[1] % local_y == 0)
            break;
        local_y /= 2;
    }
    _ws = new ghoul::opencl::CLWorkSize({dimensions[0],dimensions[1]}, {local_x,local_y});
    
    return true;
}

bool RenderableVolumeExpert::deinitialize() {

    
    return true;
}

void RenderableVolumeExpert::render(const Camera *camera, const psc &thisPosition) {
    if( ! _kernel.isValidKernel())
        return;
    
    glm::mat4 transform = camera->viewProjectionMatrix();
    glm::mat4 camTransform = camera->viewRotationMatrix();
    psc relative = thisPosition-camera->position();

    transform = transform*camTransform;
    transform = glm::translate(transform, relative.vec3());
    transform = glm::scale(transform, _boxScaling);

    _colorBoxRenderer->render(transform);
    
    _textureLock->lock();
    _kernelLock->lock();
    
    // tell opengl to finish everything before opencl takes ownerhip (uses) the textures
    glFinish();
    
    // Aquire GL objects
    _commands.enqueueAcquireGLObjects(_clBackTexture);
    _commands.enqueueAcquireGLObjects(_clFrontTexture);
    _commands.enqueueAcquireGLObjects(_clOutput);
    _commands.enqueueAcquireGLObjects(_clVolumes);
    _commands.enqueueAcquireGLObjects(_clTransferFunctions);

    _commands.enqueueKernelBlocking(_kernel, *_ws);
    _commands.finish();

    // Release GL objects
    _commands.enqueueReleaseGLObjects(_clBackTexture);
    _commands.enqueueReleaseGLObjects(_clFrontTexture);
    _commands.enqueueReleaseGLObjects(_clOutput);
    _commands.enqueueReleaseGLObjects(_clVolumes);
	_commands.enqueueReleaseGLObjects(_clTransferFunctions);

    _quadProgram->activate();
    glActiveTexture(GL_TEXTURE0);
    _output->bind();
    
    // enable blending
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);

    glBindVertexArray(_screenQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _kernelLock->unlock();
    _textureLock->unlock();
    
    // disable blending
    glDisable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);
    
    _quadProgram->deactivate();
}

void RenderableVolumeExpert::update() {
    
}

void RenderableVolumeExpert::safeKernelCompilation() {
    if(_context.isValidContext()) {
        
        ghoul::opencl::CLProgram tmpProgram = _context.createProgram(_kernelSourceFile->path());
        for(auto option: _kernelOptions) {
            tmpProgram.setOption(option.first, option.second);
        }
        
        for(auto defintion: _kernelDefinitions) {
            tmpProgram.addDefinition(defintion.first, defintion.second);
        }
        
        // add the include directories
        tmpProgram.addIncludeDirectory(_kernelIncludes);
        
        if(tmpProgram.build()) {
            ghoul::opencl::CLKernel tmpKernel = tmpProgram.createKernel("volumeraycaster");
            if(tmpKernel.isValidKernel()) {
            
                auto begin = _kernelOptions.begin();
                auto end = _kernelOptions.end();
                auto f = std::find(begin, end, std::make_pair(ghoul::opencl::CLProgram::Option::KernelArgInfo, true));
                
                int maxarguments = 1024;
                bool argumentError = false;
                if (f != end) {
                    LDEBUG("Checking argument types");
                    
                    using ghoul::opencl::CLKernel;
                    maxarguments = countKernelArguments(tmpKernel);
                    
                    for (int i = 3; i<maxarguments; ++i) {
                        CLKernel::AccessQualifier acq = tmpKernel.argumentAccessQualifier(i);
                        CLKernel::AccessQualifier expected = CLKernel::AccessQualifier::ReadOnly;
                        if (acq != expected) {
                            LWARNING("Argument " << i << " is '" <<
                                     CLKernel::AccessQualifierName(acq) <<"', expected '" <<
                                     CLKernel::AccessQualifierName(expected) << "'");
                            argumentError = true;
                        }
                    }
                }
                
                if(argumentError)
                    return;
                
            
                tmpKernel.setArgument(0, &_clFrontTexture);
                tmpKernel.setArgument(1, &_clBackTexture);
                tmpKernel.setArgument(2, &_clOutput);
                
                size_t argumentNr = 3;
                  for(auto volume: _clVolumes) {
                    tmpKernel.setArgument(argumentNr, &volume);
                    ++argumentNr;
                }
                for(auto transferFunction: _clTransferFunctions) {
                    tmpKernel.setArgument(argumentNr, &transferFunction);
                    ++argumentNr;
                }
                
                if (argumentNr > maxarguments) {
                    LWARNING("More arguments set than kernel accepts.");
                }
                
                // do the actual assignment behind locked doors
                _kernelLock->lock();
                _program = tmpProgram;
                _kernel = tmpKernel;
                _kernelLock->unlock();
                LDEBUG("Done updating kernel");
            }
        }
    }
}
    
void RenderableVolumeExpert::safeUpdateTexture(const ghoul::filesystem::File& file) {
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
    ghoul::opengl::Texture* newTexture = loadTransferFunction(file.path());
    
    if(newTexture) {
        
        // upload the new texture and create a cl memory
        newTexture->uploadTexture();
        cl_mem clNewTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *newTexture);
        
        // check if opencl memory is unsuccessfull
        if(clNewTexture == 0) {
            delete newTexture;
            return;
        }
        
        // everything seems ok, critical point to replace current texture pointers
        _textureLock->lock();
        
        // deallocate current texture
        clReleaseMemObject(_clTransferFunctions.at(fileID));
        delete _transferFunctions.at(fileID);
        
        // set the new texture
        _transferFunctions.at(fileID) = newTexture;
        _clTransferFunctions.at(fileID) = clNewTexture;
        
        // update kernel
        // __kernel arguments(front, back, output, [_volumes], .. fileID))
        _kernel.setArgument(3 + _volumes.size() + fileID, &clNewTexture);
        
        LDEBUG("Transferfunction successfully updated");
        
        // end of critical section
        _textureLock->unlock();
    }
}
	
} // namespace openspace
