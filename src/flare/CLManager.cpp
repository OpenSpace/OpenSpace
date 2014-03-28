/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */
//#include <ghoul/opencl/ghoul_cl.hpp>
#include <flare/CLManager.h>
#include <flare/CLProgram.h>
#include <flare/TransferFunction.h>
#include <flare/TSP.h>
#include <flare/Texture.h>
#include <flare/Utils.h>
#include <sstream>

#include <ghoul/logging/logmanager.h>
namespace {
    std::string _loggerCat = "CLManager";
}

using namespace osp;

CLManager::CLManager() {
}

CLManager * CLManager::New() {
  return new CLManager();
}

CLManager::~CLManager() {
  for (auto it=clPrograms_.begin(); it!=clPrograms_.end(); ++it) {
    delete it->second;
  }
  for (unsigned int i=0; i<NUM_QUEUE_INDICES; ++i) {
    clReleaseCommandQueue(commandQueues_[i]);
  }
  //clReleaseContext(context_);
}

bool CLManager::InitPlatform() {
  error_ = clGetPlatformIDs(MAX_PLATFORMS, platforms_, &numPlatforms_);
  if (error_ == CL_SUCCESS) {
    INFO("Number of CL platforms: " << numPlatforms_);
  } else {
    CheckSuccess(error_, "InitPlatform()");
    return false;
  }
  // TODO support more platforms?
  if (numPlatforms_ > 1) {
    WARNING("More than one platform found, this is unsupported");
  }

  error_ = clGetPlatformInfo(platforms_[0], CL_PLATFORM_VERSION,
           sizeof(platformVersion_), platformVersion_, NULL);
  if (CheckSuccess(error_, "InitPlatform() printing platform version")) {
    INFO("Platform version: " << platformVersion_);
  } else {
    return false;
  }


  return true;
}


bool CLManager::InitDevices() {
  if (numPlatforms_ < 1) {
    ERROR("Number of platforms is < 1");
    return false;
  }

  // Find devices
  error_ = clGetDeviceIDs(platforms_[0], CL_DEVICE_TYPE_ALL,
                          sizeof(devices_), devices_, &numDevices_);
  if (CheckSuccess(error_, "InitDevices() getting IDs")) {
    INFO("Number of CL devices: " << numDevices_);
  } else {
    return false;
  }

  // Loop over found devices and print info
  for (unsigned int i=0; i<numDevices_; i++) {
    error_ = clGetDeviceInfo(devices_[i], CL_DEVICE_NAME, 
                             sizeof(deviceName_), deviceName_, NULL);
    if (CheckSuccess(error_, "InitDevices() printing info")) {
      INFO("Device " << i << " name: " << deviceName_);
    } else {
      return false;
    }
    error_ = clGetDeviceInfo(devices_[i], CL_DRIVER_VERSION,
                             sizeof(driverVersion_), driverVersion_, NULL);
    if (CheckSuccess(error_, "InitDevices() printing driver versio")) {
      INFO("Driver version " << driverVersion_);
    } else {
      return false;
    }
  }

  // Get maximum allocatable size for each device
  for (unsigned int i=0; i<numDevices_; i++) {
    error_ = clGetDeviceInfo(devices_[i], CL_DEVICE_MAX_MEM_ALLOC_SIZE,
                             sizeof(maxMemAllocSize_[i]), 
                             &(maxMemAllocSize_[i]), NULL);
    if (!CheckSuccess(error_, "InitDevices() finding maxMemAllocSize")) {
      return false;
    }
  }

  return true;
}

bool CLManager::CreateContext() {

  if (numPlatforms_ < 1) {
    ERROR("Number of platforms < 1, can't create context");
    return false;
  }

  if (numDevices_ < 1) {
    ERROR("Number of devices < 1, can't create context");
    return false;
  }
/*
  // Create an OpenCL context with a reference to an OpenGL context
  cl_context_properties contextProperties[] = {
#ifndef _WIN32
    CL_GL_CONTEXT_KHR, (cl_context_properties)glXGetCurrentContext(),
    CL_GLX_DISPLAY_KHR, (cl_context_properties)glXGetCurrentDisplay(),
#else
    CL_GL_CONTEXT_KHR, (cl_context_properties)wglGetCurrentContext(),
    CL_WGL_HDC_KHR, (cl_context_properties)wglGetCurrentDC(),
#endif
    CL_CONTEXT_PLATFORM, (cl_context_properties)platforms_[0],
    0};
  
//  FIXME I am a ugly hack
  if (numDevices_> 1)
	  devices_[0] = devices_[1];

  // TODO Support more than one device?
  context_ = clCreateContext(contextProperties, 1, &devices_[0], NULL,
                             NULL, &error_);
 */
    bool success = _context.createContextFromGLContext();
    if(!success)
        LDEBUG("Could not create GL context");

    devices_[0] = _context.device();
    return success;
  //return CheckSuccess(error_, "CreateContext()");
}


bool CLManager::CreateCommandQueue() {
  for (unsigned int i=0; i<NUM_QUEUE_INDICES; ++i) {
      commandQueues_[i] = std::move(_context.createCommandQueue());
    //commandQueues_[i]=clCreateCommandQueue(context_, devices_[0], 0, &error_);
    if (!CheckSuccess(error_, "CreateCommandQueue()")) {
      return false;
    }
  }
  return true;
}

bool CLManager::CreateProgram(std::string _programName,
                              std::string _fileName) {
  // Make sure program doesn't already exist. If it does, delete it.
  if (clPrograms_.find(_programName) != clPrograms_.end()) {
    delete clPrograms_[_programName];
    clPrograms_.erase(_programName);
  }

  // Create new program and save pointer in map
  CLProgram *program = CLProgram::New(_programName, this);
  clPrograms_[_programName] = program;
 
  // Create program
  return program->CreateProgram(_fileName);
}


bool CLManager::BuildProgram(std::string _programName) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  return clPrograms_[_programName]->BuildProgram();
}


bool CLManager::CreateKernel(std::string _programName) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  return clPrograms_[_programName]->CreateKernel();
}


bool CLManager::AddTexture(std::string _programName, unsigned int _argNr,
                           ghoul::opengl::Texture *_texture, TextureType _textureType,
                           Permissions _permissions) {
    cl_mem_flags flag = ConvertPermissions(_permissions);

    GLuint GLTextureType;
    switch (_textureType) {
    case TEXTURE_1D:
        ERROR("Texture 1D unimplemented");
        return false;
        break;
    case TEXTURE_2D:
        GLTextureType = GL_TEXTURE_2D;
        break;
    case TEXTURE_3D:
        GLTextureType = GL_TEXTURE_3D;
        break;
    default:
        ERROR("Unknown texture type");
        return false;
    }

    if (clPrograms_.find(_programName) == clPrograms_.end()) {
        ERROR("Program " << _programName << " not found");
        return false;
    }
    return clPrograms_[_programName]->AddTexture(_argNr, _texture,
        GLTextureType, flag);
}

bool CLManager::AddTexture(std::string _programName, unsigned int _argNr,
                           ghoul::opengl::Texture *_texture, TextureType _textureType,
                           Permissions _permissions, cl_mem& _clTextureMem) {

	cl_mem_flags flag = ConvertPermissions(_permissions);

    GLuint GLTextureType;
    switch (_textureType) {
    case TEXTURE_1D:
        ERROR("Texture 1D unimplemented");
        return false;
        break;
    case TEXTURE_2D:
        GLTextureType = GL_TEXTURE_2D;
        break;
    case TEXTURE_3D:
        GLTextureType = GL_TEXTURE_3D;
        break;
    default:
        ERROR("Unknown texture type");
        return false;
    }

    if (clPrograms_.find(_programName) == clPrograms_.end()) {
        ERROR("Program " << _programName << " not found");
        return false;
    }
    return clPrograms_[_programName]->AddTexture(_argNr, _texture,
        GLTextureType, flag, _clTextureMem);
}

bool osp::CLManager::AddTexture(std::string _programName,
                                unsigned int _argNr,
                                cl_mem _texture,
                                Permissions _permissions)
{
    cl_mem_flags flag = ConvertPermissions(_permissions);

    if (clPrograms_.find(_programName) == clPrograms_.end()) {
        ERROR("Program " << _programName << " not found");
        return false;
    }
    return clPrograms_[_programName]->AddTexture(_argNr, _texture, flag);
}


bool CLManager::AddBuffer(std::string _programName, unsigned int _argNr,
                          void *_hostPtr, unsigned int _sizeInBytes,
                          AllocMode _allocMode, Permissions _permissions) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  cl_mem_flags allocMode = ConvertAllocMode(_allocMode);
  cl_mem_flags permissions = ConvertPermissions(_permissions);

  return clPrograms_[_programName]->
    AddBuffer(_argNr, _hostPtr, _sizeInBytes, allocMode, permissions);
}

bool CLManager::ReadBuffer(std::string _programName, unsigned int _argNr,
                           void *_hostPtr, unsigned int _sizeInBytes,
                           bool _blocking) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  cl_bool blocking = _blocking ? CL_TRUE : CL_FALSE;
  return clPrograms_[_programName]->
    ReadBuffer(_argNr, _hostPtr, _sizeInBytes, blocking);
}

bool CLManager::ReleaseBuffer(std::string _programName, unsigned int _argNr) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  return clPrograms_[_programName]->ReleaseBuffer(_argNr);
}

bool CLManager::PrepareProgram(std::string _programName) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  if (!clPrograms_[_programName]->PrepareProgram()) {
    ERROR("Error when preparing program " << _programName);
    return false;
  }
  return true;
}

bool CLManager::SetInt(std::string _programName, unsigned int _argNr, 
                       int _val) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  if (!clPrograms_[_programName]->SetInt(_argNr, _val)) {
    ERROR(_programName << " SetInt()");
    return false;
  }
  return true;
}


bool CLManager::LaunchProgram(std::string _programName, 
                              unsigned int _gx, unsigned int _gy,
                              unsigned int _lx, unsigned int _ly) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  if (!clPrograms_[_programName]->LaunchProgram(_gx, _gy, _lx, _ly)) {
    ERROR("Error when launching program " << _programName);
    return false;
  }
  return true;
}


bool CLManager::FinishProgram(std::string _programName) {
  if (clPrograms_.find(_programName) == clPrograms_.end()) {
    ERROR("Program " << _programName << " not found");
    return false;
  }
  if (!clPrograms_[_programName]->FinishProgram()) {
    ERROR("Error when finishing program " << _programName);
    return false;
  }
  return true;
}


bool CLManager::FinishQueue(QueueIndex _queueIndex) {
  clFinish(commandQueues_[_queueIndex]);
  return true;
}


bool CLManager::CheckSuccess(cl_int _error, std::string _location) const {
  if (_error == CL_SUCCESS) {
    return true;
  } else {
    ERROR(_location);
    ERROR(ErrorString(_error));
    return false;
  }
}

cl_mem_flags CLManager::ConvertPermissions(Permissions _p) {
  switch (_p) {
  case READ_WRITE:
    return CL_MEM_READ_WRITE;
  case READ_ONLY:
    return CL_MEM_READ_ONLY;
  case WRITE_ONLY:
    return CL_MEM_WRITE_ONLY;
  default:
    ERROR("Unknown permission type, using READ_WRITE");
    return CL_MEM_READ_WRITE;
  }
}

cl_mem_flags CLManager::ConvertAllocMode(AllocMode _am) {
  switch (_am) {
  case USE_HOST_PTR:
    return CL_MEM_USE_HOST_PTR;
  case ALLOC_HOST_PTR:
    return CL_MEM_ALLOC_HOST_PTR;
  case COPY_HOST_PTR:
    return CL_MEM_COPY_HOST_PTR;
  default:
    ERROR("Unknown alloc mode, using COPY_HOST_PTR");
    return CL_MEM_COPY_HOST_PTR;
  }
}


std::string CLManager::ErrorString(cl_int _error) const {
  switch (_error) {
    case CL_SUCCESS:
      return "CL_SUCCESS";
    case CL_DEVICE_NOT_FOUND:
      return "CL_DEVICE_NOT_FOUND";
    case CL_DEVICE_NOT_AVAILABLE:
      return "CL_DEVICE_NOT_AVAILABLE";
    case CL_COMPILER_NOT_AVAILABLE:
      return "CL_COMPILER_NOT_AVAILABLE";
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:
      return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
    case CL_OUT_OF_RESOURCES:
      return "CL_OUT_OF_RESOURCES";
    case CL_OUT_OF_HOST_MEMORY:
      return "CL_OUT_OF_HOST_MEMORY";
    case CL_PROFILING_INFO_NOT_AVAILABLE:
      return "CL_PROFILING_INFO_NOT_AVAILABLE";
    case CL_MEM_COPY_OVERLAP:
      return "CL_MEM_COPY_OVERLAP";
    case CL_IMAGE_FORMAT_MISMATCH:
      return "CL_IMAGE_FORMAT_MISMATCH";
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:
      return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
    case CL_BUILD_PROGRAM_FAILURE:
      return "CL_BUILD_PROGRAM_FAILURE";
    case CL_MAP_FAILURE:
      return "CL_MAP_FAILURE";
    case CL_INVALID_VALUE:
      return "CL_INVALID_VALUE";
    case CL_INVALID_DEVICE_TYPE:
      return "CL_INVALID_DEVICE_TYPE";
    case CL_INVALID_PLATFORM:
      return "CL_INVALID_PLATFORM";
    case CL_INVALID_DEVICE:
      return "CL_INVALID_DEVICE";
    case CL_INVALID_CONTEXT:
      return "CL_INVALID_CONTEXT";
    case CL_INVALID_QUEUE_PROPERTIES:
      return "CL_INVALID_QUEUE_PROPERTIES";
    case CL_INVALID_COMMAND_QUEUE:
      return "CL_INVALID_COMMAND_QUEUE";
    case CL_INVALID_HOST_PTR:
      return "CL_INVALID_HOST_PTR";
    case CL_INVALID_MEM_OBJECT:
      return "CL_INVALID_MEM_OBJECT";
    case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
      return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
    case CL_INVALID_IMAGE_SIZE:
      return "CL_INVALID_IMAGE_SIZE";
    case CL_INVALID_SAMPLER:
      return "CL_INVALID_SAMPLER";
    case CL_INVALID_BINARY:
      return "CL_INVALID_BINARY";
    case CL_INVALID_BUILD_OPTIONS:
      return "CL_INVALID_BUILD_OPTIONS";
    case CL_INVALID_PROGRAM:
      return "CL_INVALID_PROGRAM";
    case CL_INVALID_PROGRAM_EXECUTABLE:
      return "CL_INVALID_PROGRAM_EXECUTABLE";
    case CL_INVALID_KERNEL_NAME:
      return "CL_INVALID_KERNEL_NAME";
    case CL_INVALID_KERNEL_DEFINITION:
      return "CL_INVALID_KERNEL_DEFINITION";
    case  CL_INVALID_KERNEL:
      return "CL_INVALID_KERNEL";
    case CL_INVALID_ARG_INDEX:
      return "CL_INVALID_ARG_INDEX";
    case CL_INVALID_ARG_VALUE:
      return "CL_INVALID_ARG_VALUE";
    case CL_INVALID_ARG_SIZE:
      return "CL_INVALID_ARG_SIZE";
    case CL_INVALID_KERNEL_ARGS:
      return "CL_INVALID_KERNEL_ARGS";
    case CL_INVALID_WORK_DIMENSION:
      return "CL_INVALID_WORK_DIMENSION";
    case CL_INVALID_WORK_GROUP_SIZE:
      return "CL_INVALID_WORK_GROUP_SIZE";
    case CL_INVALID_WORK_ITEM_SIZE:
      return "CL_INVALID_WORK_ITEM_SIZE";
    case CL_INVALID_GLOBAL_OFFSET:
      return "CL_INVALID_GLOBAL_OFFSET";
    case CL_INVALID_EVENT_WAIT_LIST:
      return "CL_INVALID_EVENT_WAIT_LIST";
    case CL_INVALID_EVENT:
      return "CL_INVALID_EVENT";
    case CL_INVALID_OPERATION:
      return "CL_INVALID_OPERATION";
    case CL_INVALID_GL_OBJECT:
      return "CL_INVALID_GL_OBJECT";
    case  CL_INVALID_BUFFER_SIZE:
      return "CL_INVALID_BUFFER_SIZE";
    case CL_INVALID_MIP_LEVEL:
      return "CL_INVALID_MIP_LEVEL";
    case CL_INVALID_GLOBAL_WORK_SIZE:
      return "CL_INVALID_GLOBAL_WORK_SIZE";
    default:
      std::stringstream ss;
      std::string code;
      ss << "Unknown OpenCL error code - " << _error;
      return ss.str();
  }
}



