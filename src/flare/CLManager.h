/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#ifndef CL_MANAGER_H_
#define CL_MANAGER_H_

#include <ghoul/opencl/ghoul_cl.h>
#include <ghoul/opencl/clcontext.h>
#include <ghoul/opencl/clcommandqueue.h>
#include <ghoul/opencl/clprogram.h>
#include <ghoul/opencl/clkernel.h>
#include <ghoul/opencl/clutil.h>
#include <ghoul/opencl/clworksize.h>
#include <map>
#include <string>
#include <flare/KernelConstants.h>

namespace osp {

class TransferFunction;
class CLProgram;
class TSP;

class CLManager {
public:
  static CLManager * New();
  ~CLManager();
  
  // Different queue indices for working with asynchronous uploads/executions
  enum QueueIndex { EXECUTE, TRANSFER, NUM_QUEUE_INDICES };
  enum TextureType { TEXTURE_1D, TEXTURE_2D, TEXTURE_3D };
  enum Permissions { READ_ONLY, WRITE_ONLY, READ_WRITE };
  enum AllocMode { USE_HOST_PTR, ALLOC_HOST_PTR, COPY_HOST_PTR };

  // These four functions should be run in this order
  bool InitPlatform();
  bool InitDevices();
  bool CreateContext();
  bool CreateCommandQueue();

  // Name a program and create it from source text file
  bool CreateProgram(std::string _programName, std::string _fileName);
  // Build program after creation
  bool BuildProgram(std::string _programName);
  // Create kernel after building program
  bool CreateKernel(std::string _programName);
  
  // Add an OpenGL texture to a program
  bool AddTexture(std::string _programName, unsigned int _argNr,
                  ghoul::opengl::Texture *_texture, TextureType _textureType,
                  Permissions _permissions);

  bool AddTexture(std::string _programName, unsigned int _argNr,
      ghoul::opengl::Texture *_texture, TextureType _textureType,
      Permissions _permissions, cl_mem& _clTextureMem);

  bool AddTexture(std::string _programName, unsigned int _argNr,
                cl_mem _texture, Permissions _permissions);

  bool AddBuffer(std::string _programName, unsigned int _argNr,
                 void *_hostPtr, unsigned int _sizeInBytes,
                 AllocMode _allocMode, Permissions _permissions);

  bool ReadBuffer(std::string _programName, unsigned int _argNr,
                  void *_hostPtr, unsigned int _sizeInBytes,
                  bool _blocking);

  // Free resources
  bool ReleaseBuffer(std::string _programName, unsigned int _argNr);

  // Set the value of an integer argument
  bool SetInt(std::string _programName, unsigned int _argNr, int _val);
  
  // Aquire any shared textures, set up kernel arguments etc
  bool PrepareProgram(std::string _programName);

  // Launch program kernel (returns immediately)
  bool LaunchProgram(std::string _programName, 
                     unsigned int _gx, unsigned int _gy,
                     unsigned int _lx, unsigned int _ly);

  // Wait for kernel to finish, releaste any shared resources
  bool FinishProgram(std::string _programName);

  // Finish all commands in a command queue
  // Can be used e.g. to sync a DMA transfer operation
  bool FinishQueue(QueueIndex _queueIndex);
   
  friend class CLProgram;

private:
  CLManager();
  CLManager(const CLManager&);

  // Read kernel source from file, return char array
  // Store number of chars in _numChar argument
  char * ReadSource(std::string _fileName, int &_numChars) const;
  // Check state in a cl_int, print error if state is not CL_SUCCESS
  bool CheckSuccess(cl_int _error, std::string _location) const;
  // Translate cl_int enum to readable string
  std::string ErrorString(cl_int _error) const;
  // Convert CLManager::Permissions to cl_mem_flags
  cl_mem_flags ConvertPermissions(Permissions _permissions);
  // Conver CLManager::AllocMode to cl_mem_flags
  cl_mem_flags ConvertAllocMode(AllocMode _allocMode);

  static const unsigned int MAX_PLATFORMS = 32;
  static const unsigned int MAX_DEVICES = 32;
  static const unsigned int MAX_NAME_LENGTH = 128;

  // Used for storing and reading error state during function calls
  cl_int error_;
  cl_uint numPlatforms_;
  cl_platform_id platforms_[MAX_PLATFORMS];
  cl_uint numDevices_;
  cl_device_id devices_[MAX_DEVICES];
  // Maximum allocatable size for each device
  cl_ulong maxMemAllocSize_[MAX_DEVICES];
  char deviceName_[MAX_NAME_LENGTH];
  char driverVersion_[MAX_NAME_LENGTH];
  char platformVersion_[MAX_NAME_LENGTH];
  //cl_context context_;
  ghoul::opencl::CLCommandQueue commandQueues_[NUM_QUEUE_INDICES];

  // Programs are mapped using strings
  std::map<std::string, CLProgram*> clPrograms_;
  
  ghoul::opencl::CLContext _context;

};

}

#endif
  


