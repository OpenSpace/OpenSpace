/*
 * Victor Sand (victor.sand@gmail.com)
 *
 */

#ifndef CL_PROGRAM_H_
#define CL_PROGRAM_H_

#ifndef _WIN32
#include <CL/cl.hpp>
#else
#include <CL/cl.h>
#endif
#include <map>
#include <string>
#include <flare/KernelConstants.h>

namespace osp {

class CLManager;
class TransferFunction;
class Texture;

class CLProgram {
public:
  static CLProgram * New(const std::string &_programName, 
                         CLManager *_clManager);
  ~CLProgram();

  struct MemArg {
    size_t size_;
    cl_mem mem_;
  };

  cl_program Program() { return program_; }
  cl_kernel Kernel() { return kernel_; }
  cl_int Error() { return error_; }

  bool CreateProgram(std::string _fileName);
  bool BuildProgram();
  bool CreateKernel();
 
  bool AddTexture(unsigned int _argNr, Texture *_texture,
                  GLuint _textureType,
                  cl_mem_flags _permissions);

  bool AddTexture(unsigned int _argNr, Texture *_texture,
      GLuint _textureType,
      cl_mem_flags _permissions, cl_mem& _clTextureMem);

  bool AddTexture(unsigned int _argNr, cl_mem _texture,
      cl_mem_flags _permissions);

  bool AddBuffer(unsigned int _argNr, 
                 void *_hostPtr, 
                 unsigned int _sizeInBytes, 
                 cl_mem_flags _allocMode,
                 cl_mem_flags _permissions);

  bool ReadBuffer(unsigned int _argNr,
                  void *_hostPtr,
                  unsigned int _sizeInBytes,
                  cl_bool _blocking);

  bool ReleaseBuffer(unsigned int _argNr);

  bool SetInt(unsigned int _argNr, int _val);
   
  bool PrepareProgram();
  bool LaunchProgram(unsigned int _gx, unsigned int _gy,
                     unsigned int _lx, unsigned int _ly);
  bool FinishProgram();

private:
  CLProgram(const std::string &_programName, CLManager *_clManager);
  CLProgram();
  CLProgram(const CLProgram&);

  char * ReadSource(const std::string &_fileName, int &_numChars) const;

  std::string programName_;

  CLManager *clManager_;
  cl_program program_;
  cl_kernel kernel_;
  cl_int error_;
  // Stores device OGL textures together with their kernel arg nummer
  std::map<cl_uint, cl_mem> OGLTextures_;
  // Stores non-texture memory buffer arguments
  std::map<cl_uint, MemArg> memArgs_;

};

}

#endif
