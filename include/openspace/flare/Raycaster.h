#ifndef RAYCASTER_H_
#define RAYCASTER_H_

/*
Author: Victor Sand (victor.sand@gmail.com)
Mammoth workhorse class that ties all parts of the raycaster application
together. Manages state, render calls and a lot of initializations.
TODO: Iteratively break away parts from it into other classes.
*/

#include <openspace/flare/Renderer.h>
#include <map>
#include <string>
#include <vector>
#include <ghoul/glm.h>
#include <ghoul/opencl/ghoul_cl.h>
#include <openspace/flare/KernelConstants.h>
//#include <boost/timer/timer.hpp>
#include <openspace/flare/TSP.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace osp {

class TransferFunction;
class Animator;
class BrickManager;
class CLManager;
class Config;

class Raycaster : public Renderer {
public:
  
  static Raycaster * New(Config *_config);
  virtual ~Raycaster();
  virtual bool Render(float _timestep);

  // Called by the SGCT window manager
  bool Reload();

  // Init pipeline for double buffered setup
  bool InitPipeline();

  // Reload GLSL shaders
  bool ReloadShaders();
  // Reload transfer function file
  bool ReloadTransferFunctions();
  // Initialize the color cube used for raycasting.
  // TODO Make cube class.
  bool InitCube();
  // Init the quad used for output texture rendering
  bool InitQuad();
  // Run all of CLHandlers initialization functions
  bool InitCL();
  // Set up buffers for the different rendering stages
  bool InitFramebuffers();
  // Set up two pixel buffers for texture streaming
  bool InitPixelBuffers();
  // Update matrices with current view parameters
  bool UpdateMatrices();
  // Bind transformation matrices to a ShaderProgram
  bool BindTransformationMatrices(ghoul::opengl::ProgramObject *_program);
  // Read kernel config from file and voxel data,
  // update the constants that get sent to the kernel every frame
  bool UpdateKernelConfig();
  // For keyboard handling, retrieve the last known state of a key
  bool KeyLastState(int _key) const;
  // Add a transfer function to the transfer function list
  // TODO Actually support and make use of multiple TFs
  void AddTransferFunction(TransferFunction *_transferFunction);

  ghoul::opengl::Texture * CubeFrontTexture() const { return cubeFrontTex_; }
  ghoul::opengl::Texture  * CubeBackTexture() const { return cubeBackTex_; }
  ghoul::opengl::Texture * QuadTexture() const { return quadTex_; }

  void SetKernelConfigFilename(const std::string &_filename);
  void SetCubeFrontTexture(ghoul::opengl::Texture  *_cubeFrontTexture);
  void SetCubeBackTexture(ghoul::opengl::Texture  *_cubeBackTexture);
  void SetQuadTexture(ghoul::opengl::Texture *_quadTexture);
  void SetCubeShaderProgram(ghoul::opengl::ProgramObject *_cubeShaderProgram);
  void SetQuadShaderProgram(ghoul::opengl::ProgramObject *_quadShaderProgram);
  void SetAnimator(Animator *_animator);
  void SetCLManager(CLManager *_clManager);
  void SetBrickManager(BrickManager *_brickManager);
  void SetTSP(TSP *_tsp);

  // SGCT 
  void SetModelParams(float _pitch, float _yaw, float _roll);
  void SetViewParams(float _translateX, float _translateY, float _translateZ);

private:
  Raycaster();
  Raycaster(Config *_config);
    
  Config *config_;

  // VAO handles
  unsigned int cubeVAO_;
  unsigned int quadVAO_;

  // Buffer object handles
  unsigned int cubeFrontFBO_;
  unsigned int cubeBackFBO_;
  unsigned int renderbufferObject_;
  unsigned int cubePosbufferObject_;
  unsigned int quadPosbufferObject_;
  unsigned int cubePositionAttrib_;
  unsigned int quadPositionAttrib_;
  // Shaders
  ghoul::opengl::ProgramObject *cubeShaderProgram_;
  ghoul::opengl::ProgramObject *quadShaderProgram_;
  // Textures
  ghoul::opengl::Texture  *cubeFrontTex_;
  ghoul::opengl::Texture  *cubeBackTex_;
  ghoul::opengl::Texture *quadTex_;
  ghoul::opengl::Texture *volumeTex_;
  // Model params
  float pitch_;
  float yaw_;
  float roll_;
  // View
  float translateX_;
  float translateY_;
  float translateZ_;

  // Tranformation matrices
  glm::mat4 model_;
  glm::mat4 view_;
  glm::mat4 proj_;
  // State
  bool cubeInitialized_;
  bool quadInitialized_;
  bool framebuffersInitialized_;
  bool pingPong_; // texture streaming

  // Animator to control update rate and looping
  Animator * animator_;
  // Used to see if it is time to update frames 
  unsigned int lastTimestep_;
  // Used for ping pong memory buffering
  unsigned int pingPongIndex_;
  // Kernel constants
  KernelConstants kernelConstants_;
  TraversalConstants traversalConstants_;
  std::string kernelConfigFilename_;
  // Transfer functions
  std::vector<TransferFunction*> transferFunctions_;
 
  // Brick manager with access to brick data
  BrickManager *brickManager_;

  bool LaunchTSPTraversal(unsigned int _timestep);

  // Brick request list
  std::vector<int> brickRequest_;

  // TSP tree structure (not actual data)
  TSP *tsp_;
  
  // Entry point for all things OpenCL
  CLManager *clManager_;

  // Helper function for updating and binding kernel constants
  bool UpdateKernelConstants();

  // For the corresponding CL kernel
  static const unsigned int cubeFrontArg_ = 0;
  static const unsigned int cubeBackArg_ = 1;
  static const unsigned int quadArg_ = 2;
  static const unsigned int textureAtlasArg_ = 3;
  static const unsigned int constantsArg_ = 4;
  static const unsigned int transferFunctionArg_ = 5; 
  static const unsigned int tspArg_ = 6;
  static const unsigned int brickListArg_ = 7;
  static const unsigned int timestepArg_ = 8;

  static const unsigned int tspCubeFrontArg_ = 0;
  static const unsigned int tspCubeBackArg_ = 1;
  static const unsigned int tspConstantsArg_ = 2;
  static const unsigned int tspTSPArg_ = 3;
  static const unsigned int tspBrickListArg_ = 4;
  static const unsigned int tspTimestepArg_ = 5;

  
  // Timer and timer constants 
  //boost::timer::cpu_timer timer_;
  
  
  cl_mem cubeFrontCLmem;
  cl_mem cubeBackCLmem;
};

}

#endif
