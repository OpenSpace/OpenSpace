/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
//#include <ghoul/opengl/ghoul_gl.h>
#include <flare/CLProgram.h>
#include <flare/CLManager.h>
#include <flare/TransferFunction.h>
#include <flare/Utils.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opencl/ghoul_cl.hpp>

namespace {
    std::string _loggerCat = "CLProgram";
}

using namespace osp;

CLProgram::CLProgram(const std::string &_programName, CLManager *_clManager) 
  : programName_(_programName), clManager_(_clManager) {
}

CLProgram * CLProgram::New(const std::string &_programName, 
                           CLManager *_clManager) {
  return new CLProgram(_programName, _clManager);
}

CLProgram::~CLProgram() {
  clReleaseKernel(kernel_);
  clReleaseProgram(program_);
}

bool CLProgram::CreateProgram(std::string _fileName) {
  int numChars;
  char *source = ReadSource(_fileName, numChars);
  program_ = clCreateProgramWithSource(clManager_->_context, 1,
                                       (const char**)&source,
                                       NULL, &error_);
  free(source);
  return (error_ == CL_SUCCESS);
}


bool CLProgram::BuildProgram() {
std::string options = "-cl-opt-disable";
  error_ = clBuildProgram(program_, (cl_uint)0,
                          NULL, options.c_str(), NULL, NULL);
  if (error_ != CL_SUCCESS) {
      LDEBUG("Could not build program " << getErrorString(error_));
    // Print build log
    char * log;
    size_t logSize = 0;
    clGetProgramBuildInfo(program_, clManager_->devices_[0], 
                          CL_PROGRAM_BUILD_LOG, 0, NULL, &logSize);
    if (logSize > 0) {
      INFO("Build log:");
      log = new char[logSize+1];
      clGetProgramBuildInfo(program_, clManager_->devices_[0], 
                            CL_PROGRAM_BUILD_LOG, logSize, log, NULL);
      log[logSize] = '\0';
      INFO(log);
      free(log);                       
    }
    return false;
  }
  return true;
}


bool CLProgram::CreateKernel() {
  kernel_ = clCreateKernel(program_, programName_.c_str(), &error_);
  return (error_ == CL_SUCCESS);
}


bool CLProgram::AddTexture(unsigned int _argNr, ghoul::opengl::Texture *_texture,
                           GLuint _textureType,
                           cl_mem_flags _permissions) {

  // Remove anything already associated with argument index
  if (OGLTextures_.find((cl_uint)_argNr) != OGLTextures_.end()) {
    OGLTextures_.erase((cl_uint)_argNr);
  }
 
  cl_mem texture;
  switch (_textureType) {
    case GL_TEXTURE_1D:
      ERROR("Texture 1D unimplemented");
      return false;
      break;
    case GL_TEXTURE_2D:
#ifdef CL_VERSION_1_2
          texture = clCreateFromGLTexture(clManager_->_context, _permissions,
                                                GL_TEXTURE_2D, 0,
                                                *_texture, &error_);
#else
          texture = clCreateFromGLTexture2D(clManager_->_context, _permissions,
                                                  GL_TEXTURE_2D, 0,
                                                  *_texture, &error_);
#endif
          break;
      case GL_TEXTURE_3D:
#ifdef CL_VERSION_1_2
          texture = clCreateFromGLTexture(clManager_->_context, _permissions,
                                                GL_TEXTURE_3D, 0,
                                                *_texture, &error_);
#else
          texture = clCreateFromGLTexture3D(clManager_->_context, _permissions,
                                                  GL_TEXTURE_3D, 0,
                                                  *_texture, &error_);
#endif
      break;
    default:
      ERROR("Unknown GL texture type");
      return false;
  }

  if (!clManager_->CheckSuccess(error_, "AddTexture")) return false;
 
  OGLTextures_.insert(std::make_pair((cl_uint)_argNr, texture));

  return true;

}


bool CLProgram::AddTexture(unsigned int _argNr, ghoul::opengl::Texture *_texture,
                           GLuint _textureType,
                           cl_mem_flags _permissions,
                           cl_mem& _clTextureMem) {

    // Remove anything already associated with argument index
    if (OGLTextures_.find((cl_uint)_argNr) != OGLTextures_.end()) {
        OGLTextures_.erase((cl_uint)_argNr);
    }

    switch (_textureType) {
    case GL_TEXTURE_1D:
        ERROR("Texture 1D unimplemented");
        return false;
        break;
    case GL_TEXTURE_2D:
#ifdef CL_VERSION_1_2
            _clTextureMem = clCreateFromGLTexture(clManager_->_context, _permissions,
            GL_TEXTURE_2D, 0,
            *_texture, &error_);
#else
        _clTextureMem = clCreateFromGLTexture2D(clManager_->_context, _permissions,
            GL_TEXTURE_2D, 0,
            *_texture, &error_);
#endif
        break;
    case GL_TEXTURE_3D:
#ifdef CL_VERSION_1_2
            _clTextureMem = clCreateFromGLTexture(clManager_->_context, _permissions,
                                                  GL_TEXTURE_3D, 0,
                                                  *_texture, &error_);
#else
            _clTextureMem = clCreateFromGLTexture2D(clManager_->_context, _permissions,
                                                    GL_TEXTURE_3D, 0,
                                                    *_texture, &error_);
#endif
        break;
    default:
        ERROR("Unknown GL texture type");
        return false;
    }

    if (!clManager_->CheckSuccess(error_, "AddTexture")) return false;

    OGLTextures_.insert(std::make_pair((cl_uint)_argNr, _clTextureMem));

    return true;
}

bool osp::CLProgram::AddTexture(unsigned int _argNr,
                                cl_mem _texture,
                                cl_mem_flags _permissions)
{
    // Remove anything already associated with argument index
    if (OGLTextures_.find((cl_uint)_argNr) != OGLTextures_.end()) {
        OGLTextures_.erase((cl_uint)_argNr);
    }
    OGLTextures_.insert(std::make_pair((cl_uint)_argNr, _texture));

    return true;
}


bool CLProgram::AddBuffer(unsigned int _argNr,
                          void *_hostPtr,
                          unsigned int _sizeInBytes,
                          cl_mem_flags _allocMode,
                          cl_mem_flags _permissions) {

  if (!_hostPtr) {
    ERROR("AddBuffer(): Host pointer is NULL");
    return false;
  }

  if (memArgs_.find((cl_uint)_argNr) != memArgs_.end()) {
    memArgs_.erase((cl_uint)_argNr);
  }
  MemArg ma;
  ma.size_ = sizeof(cl_mem);
  ma.mem_ = clCreateBuffer(clManager_->_context,
                           _allocMode | _permissions,
                           (size_t)_sizeInBytes,
                           _hostPtr,
                           &error_);
  if (!clManager_->CheckSuccess(error_, "AddBuffer")) {
    return false;
  }
  memArgs_.insert(std::make_pair((cl_uint)_argNr, ma));
  return true;
}

bool CLProgram::ReadBuffer(unsigned int _argNr,
                           void *_hostPtr,
                           unsigned int _sizeInBytes,
                           cl_bool _blocking) {
  if (memArgs_.find((cl_uint)_argNr) == memArgs_.end()) {
    ERROR("ReadBuffer(): Could not find mem arg " << _argNr);
    return false;
  }
  error_ = clEnqueueReadBuffer(clManager_->commandQueues_[CLManager::EXECUTE],
                               memArgs_[(cl_uint)_argNr].mem_,
                               _blocking,
                               0,
                               _sizeInBytes,
                               _hostPtr,
                               0,
                               NULL,
                               NULL);
  return clManager_->CheckSuccess(error_, "ReadBuffer");
}

bool CLProgram::ReleaseBuffer(unsigned int _argNr) {
  if (memArgs_.find((cl_uint)_argNr) == memArgs_.end()) {
    ERROR("ReleaseBuffer(): Could not find mem arg " << _argNr);
    return false;
  }
  //LDEBUG("Releasing memory");
  error_ = clReleaseMemObject(memArgs_[(cl_uint)_argNr].mem_);
  return clManager_->CheckSuccess(error_, "ReleaseBuffer");
}


bool CLProgram::PrepareProgram() {
//ghoulFinishGL();
/*
#ifdef __APPLE__
    
    //glFlushRenderAPPLE();
    glFinish();
    //glFlush();
#else
*/
  // Let OpenCL take control of the shared GL textures
  for (auto it = OGLTextures_.begin(); it != OGLTextures_.end(); ++it) {
    ghoul::opencl::CLCommandQueue q = clManager_->commandQueues_[CLManager::EXECUTE];
    cl_command_queue clq = q;
    error_ = clEnqueueAcquireGLObjects(clq, 1, &(it->second), 0, NULL, NULL);
      
    if (!clManager_->CheckSuccess(error_, "PrepareProgram")) {
        LDEBUG("error: " << getErrorString(error_));
      ERROR("Failed to enqueue GL object aqcuisition");
      ERROR("Failing object: " << it->first);
      return false;
    }
  }
//#endif

  // Set up kernel arguments of non-shared items
  for (auto it=memArgs_.begin(); it!=memArgs_.end(); ++it) {
    error_ = clSetKernelArg(kernel_,
                            it->first,
                            (it->second).size_,
                            &((it->second).mem_));
    if (!clManager_->CheckSuccess(error_, "PrepareProgram")) {
      ERROR("Failed to set kernel argument " << it->first);
      return false;
    }
  }
  
  // Set up kernel arguments for textures
  for (auto it=OGLTextures_.begin(); it!=OGLTextures_.end(); ++it) {
    error_ = clSetKernelArg(kernel_,
                            it->first,
                            sizeof(cl_mem),
                            &(it->second));
    if (!clManager_->CheckSuccess(error_, "PrepareProgram")) {
      ERROR("Failed to set texture kernel arg " << it->first);
      return false;
    }
  }

  return true;
}

bool CLProgram::SetInt(unsigned int _argNr, int _val) {
  error_ = clSetKernelArg(kernel_, _argNr, sizeof(int), &_val);
  if (!clManager_->CheckSuccess(error_, "SetInt")) {
    ERROR("Failed to set integer value");
    return false;
  }
  return true;
}

bool CLProgram::LaunchProgram(unsigned int _gx, unsigned int _gy,
                              unsigned int _lx, unsigned int _ly) {
  size_t globalSize[] = { _gx, _gy };
  size_t localSize[] = { _lx, _ly };

  error_ = clEnqueueNDRangeKernel(
    clManager_->commandQueues_[CLManager::EXECUTE], kernel_, 2, NULL,
    globalSize, localSize, 0, NULL, NULL);
  return clManager_->CheckSuccess(error_, "LaunchProgram()");
}

bool CLProgram::FinishProgram() {
    
//#ifndef __APPLE__
  // Make sure kernel is done
  error_ = clFinish(clManager_->commandQueues_[CLManager::EXECUTE]);
  if (!clManager_->CheckSuccess(error_, "FinishProgram, clFinish")) {
    ERROR("Failed to finish program");
    return false;
  }
  // Release shared OGL objects
  for (auto it=OGLTextures_.begin(); it!=OGLTextures_.end(); ++it) {
    error_ = clEnqueueReleaseGLObjects(
      clManager_->commandQueues_[CLManager::EXECUTE], 1, 
                                 &(it->second), 0, NULL, NULL);
    if (!clManager_->CheckSuccess(error_, "FinishProgram, release GL objs")) {
      ERROR("Failed to release GL object");
      ERROR("Failed object: " << it->first);
      return false;
    }
  }
  /*
#else
    error_ = clFinish(clManager_->commandQueues_[CLManager::EXECUTE]);
    if (!clManager_->CheckSuccess(error_, "FinishProgram, clFinish")) {
        ERROR("Failed to finish program");
        return false;
    }
#endif
*/
  return true;
}


char * CLProgram::ReadSource(const std::string &_filename, 
                             int &_numChars) const {
  FILE *in;
  char *content = NULL;
  in = fopen(_filename.c_str(), "r");
  int count = 0;
  if (in != NULL) {
    fseek(in, 0, SEEK_END);
    count = ftell(in);
    rewind(in);
    content = (char *)malloc(sizeof(char)*(count+1));
    count = fread(content, sizeof(char), count, in);
    content[count] = '\0';
    fclose(in);
  } else {
    ERROR("Could not read source from file " << _filename);
  }
  _numChars = count;
  return content;
}
