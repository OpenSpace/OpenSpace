/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 * 
 */

#include <ghoul/logging/logmanager.h>
#include <sgct.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <fstream>
#include <flare/Raycaster.h>
#include <flare/Texture2D.h>
#include <flare/Texture3D.h>
#include <flare/TextureAtlas.h>
#include <flare/BrickManager.h>
#include <flare/Utils.h>
#include <flare/ShaderProgram.h>
#include <glm/gtc/matrix_transform.hpp>
#include <flare/TransferFunction.h>
#include <flare/Animator.h>
#include <vector>
#include <flare/CLManager.h>
#include <flare/KernelConstants.h>
#include <flare/Config.h>
#include <stdint.h>
#include <unistd.h> // sync()



using namespace osp;

uint32_t ZOrder(uint16_t xPos, uint16_t yPos, uint16_t zPos) {
  uint32_t x = static_cast<uint32_t>(xPos);
  uint32_t y = static_cast<uint32_t>(yPos);
  uint32_t z = static_cast<uint32_t>(zPos);
  x = (x | (x << 16)) & 0x030000FF;
  x = (x | (x <<  8)) & 0x0300F00F;
  x = (x | (x <<  4)) & 0x030C30C3;
  x = (x | (x <<  2)) & 0x09249249;
  y = (y | (y << 16)) & 0x030000FF;
  y = (y | (y <<  8)) & 0x0300F00F;
  y = (y | (y <<  4)) & 0x030C30C3;
  y = (y | (y <<  2)) & 0x09249249;
  z = (z | (z << 16)) & 0x030000FF;
  z = (z | (z <<  8)) & 0x0300F00F;
  z = (z | (z <<  4)) & 0x030C30C3;
  z = (z | (z <<  2)) & 0x09249249;
  const uint32_t result = x | (y << 1) | (z << 2);
  return result;
}

const double BYTES_PER_GB = 1073741824.0;

Raycaster::Raycaster(Config *_config) 
  : Renderer(),
    config_(_config),
    cubeFrontFBO_(0),
    cubeBackFBO_(0),
    renderbufferObject_(0),
    cubePosbufferObject_(0),
    quadPosbufferObject_(0),
    cubePositionAttrib_(0),
    quadPositionAttrib_(0),
    cubeShaderProgram_(NULL),
    quadShaderProgram_(NULL),
    cubeFrontTex_(NULL),
    cubeBackTex_(NULL),
    quadTex_(NULL),
    pitch_(0.f),
    yaw_(0.f),
    roll_(0.f),
    translateX_(_config->TranslateX()),
    translateY_(_config->TranslateY()),
    translateZ_(_config->TranslateZ()),
    model_(glm::mat4()),
    view_(glm::mat4()),
    proj_(glm::mat4()),
    cubeInitialized_(false),
    quadInitialized_(false),
    framebuffersInitialized_(false),
    pingPongIndex_(true),
    animator_(NULL),
    pingPong_(0),
    lastTimestep_(1),
    brickManager_(NULL),
    clManager_(NULL) {
}

Raycaster::~Raycaster() {
  // TODO relase GL textures
}

Raycaster * Raycaster::New(Config *_config) {
  Raycaster *raycaster = new Raycaster(_config);
  return raycaster;
}

bool Raycaster::Render(float _timestep) {
  proj_ = sgct::Engine::instance()->getActiveProjectionMatrix();

  // Clear cache for benchmarking
  if (config_->ClearCache()) {
    sync();
    std::ofstream ofs("/proc/sys/vm/drop_caches");
    ofs << "3" << std::endl;
    ofs.close();
  }

  //timer_.start();

  if (animator_ == NULL) {
    WARNING("Animator not set");
  }
  
  // Reset any errors
  glGetError();

  // TODO move init checks and only run them once
  if (!cubeInitialized_) {
    ERROR("Rendering failed, cube not initialized");
    return false;
  }

  if (!quadInitialized_) {
    ERROR("Rendering failed, quad not initialized");
    return false;
  }

  if (!framebuffersInitialized_) {
    ERROR("Rendering failed, framebuffers not initialized");
    return false;
  }

  if (!cubeFrontTex_ || !cubeBackTex_ || !quadTex_) {
    ERROR("Rendering failed, one or more texures are not set");
    return false;
  }

  if (!cubeShaderProgram_ || !quadShaderProgram_) {
    ERROR("Rendering failed, one or more shaders are not set");
    return false;
  }

  if (!UpdateMatrices()) return false;
  if (!BindTransformationMatrices(cubeShaderProgram_)) return false;

  // For some reason, setting 0 all across leaves the background white.
  //glClearColor(0.f, 0.f, 0.f, 0.f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  // Render cube
  cubeShaderProgram_->activate();
  //glUseProgram(cubeShaderProgram_->Handle());
  cubePositionAttrib_ = cubeShaderProgram_->attributeLocation("position");
  if (cubePositionAttrib_ == -1) {
    ERROR("Cube position attribute lookup failed");
    return false;
  }
  glFrontFace(GL_CW);
  glEnable(GL_CULL_FACE);

  // Front
  glBindFramebuffer(GL_FRAMEBUFFER, cubeFrontFBO_);
  glCullFace(GL_BACK);
  glBindVertexArray(cubeVAO_);
  glBindBuffer(GL_ARRAY_BUFFER, cubePosbufferObject_);
  glEnableVertexAttribArray(cubePositionAttrib_);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDrawArrays(GL_TRIANGLES, 0, 144);
  glDisableVertexAttribArray(cubePositionAttrib_);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindVertexArray(0);

  if (CheckGLError("Cube front rendering") != GL_NO_ERROR) {
    return false;
  }

  // Back
  glBindFramebuffer(GL_FRAMEBUFFER, cubeBackFBO_);
  glCullFace(GL_FRONT);
  glBindVertexArray(cubeVAO_);
  glBindBuffer(GL_ARRAY_BUFFER, cubePosbufferObject_);
  glEnableVertexAttribArray(cubePositionAttrib_);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDrawArrays(GL_TRIANGLES, 0, 144);
  glDisableVertexAttribArray(cubePositionAttrib_);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindVertexArray(0);

  if (CheckGLError("Cube back rendering") != GL_NO_ERROR) {
    return false;
  }

  glUseProgram(0);

  unsigned int currentTimestep;
  unsigned int nextTimestep;
  if (animator_ != NULL) {
    currentTimestep = animator_->CurrentTimestep();
    nextTimestep = animator_->NextTimestep();
  } else {
    WARNING("Animator not set");
    currentTimestep = 0;
    nextTimestep = 1;
  }

  // Choose buffers
  BrickManager::BUFFER_INDEX currentBuf, nextBuf;
  if (currentTimestep % 2 == 0) {
    currentBuf = BrickManager::EVEN;
    nextBuf = BrickManager::ODD;
  } else {
    currentBuf = BrickManager::ODD;
    nextBuf = BrickManager::EVEN;
  }


  // When starting a rendering iteration, the PBO corresponding to the
  // current timestep is loaded with the data.


  // Launch traversal of the next timestep
  if (!LaunchTSPTraversal(nextTimestep)) return false;
  
  // While traversal of next step is working, upload current data to atlas
  if (!brickManager_->PBOToAtlas(currentBuf)) return false;

  // Make sure the traversal kernel is done
  if (!clManager_->FinishProgram("TSPTraversal")) return false;

  // Read buffer and release the memory
  if (!clManager_->ReadBuffer("TSPTraversal", tspBrickListArg_,
                              reinterpret_cast<void*>(&brickRequest_[0]),
                              brickRequest_.size()*sizeof(int),
                              true)) return false;

  if (!clManager_->ReleaseBuffer("TSPTraversal",tspBrickListArg_))return false;
  
  // When traversal of next timestep is done, launch raycasting kernel
  if (!clManager_->SetInt("RaycasterTSP", timestepArg_, currentTimestep)) 
    return false;

  // Add brick list
  if (!clManager_->
    AddBuffer("RaycasterTSP", brickListArg_,
      reinterpret_cast<void*>(&(brickManager_->BrickList(currentBuf)[0])),
      brickManager_->BrickList(currentBuf).size()*sizeof(int),
      CLManager::COPY_HOST_PTR,
      CLManager::READ_ONLY)) return false;
              
  if (!clManager_->PrepareProgram("RaycasterTSP")) return false;

  if (!clManager_->LaunchProgram("RaycasterTSP",
                                 winWidth_,
                                 winHeight_, 
                                 config_->LocalWorkSizeX(),
                                 config_->LocalWorkSizeY())) 
                                 return false;

  // While the raycaster kernel is working, build next brick list and start 
  // upload to the next PBO
  if (!brickManager_->BuildBrickList(nextBuf, brickRequest_)) return false;

  if (!brickManager_->DiskToPBO(nextBuf)) return false;

  // Finish raycaster and render current frame
  if (!clManager_->ReleaseBuffer("RaycasterTSP", brickListArg_)) return false;
  if (!clManager_->FinishProgram("RaycasterTSP")) return false;


  // Render to framebuffer using quad
  glBindFramebuffer(GL_FRAMEBUFFER, sgct::Engine::instance()->getActiveWindowPtr()->getFBOPtr()->getBufferID());

    glGetError();
    quadShaderProgram_->activate();
    glActiveTexture(GL_TEXTURE0);
    int location = quadShaderProgram_->uniformLocation("quadTex");
    //int location = glGetUniformLocation(_shaderProgram->Handle(),_uniformName.c_str());
    if (location == -1) {
        ERROR("Uniform " << "quadTex" << " could not be found");
        glUseProgram(0);
        return false;
    }
    
    glUniform1i(location, 0);
    glBindTexture(GL_TEXTURE_2D, *quadTex_);
    glUseProgram(0);


  //if (!quadTex_->Bind(quadShaderProgram_, "quadTex", 0)) return false;
  
  glDisable(GL_CULL_FACE);

    quadShaderProgram_->activate();
    //glUseProgram(quadShaderProgram_->Handle());
    //quadPositionAttrib_ = quadShaderProgram_->GetAttribLocation("position");
    quadPositionAttrib_ = quadShaderProgram_->attributeLocation("position");
  if (quadPositionAttrib_ == -1) {
    ERROR("Quad position attribute lookup failed");
    return false;
  }
  glCullFace(GL_BACK);
  glBindVertexArray(quadVAO_);
  glBindBuffer(GL_ARRAY_BUFFER, quadPosbufferObject_);
  glEnableVertexAttribArray(quadPositionAttrib_);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glDisableVertexAttribArray(quadPositionAttrib_);
  glBindVertexArray(0);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  if (CheckGLError("Quad rendering") != GL_NO_ERROR) {
    return false;
  }

  glUseProgram(0);

  //timer_.stop();
  //double time = timer_.elapsed().wall / 1.0e9;
  //INFO("total time: " << time << "s"); 

  
  // Window manager takes care of swapping buffers

  return true;
}

bool Raycaster::LaunchTSPTraversal(unsigned int _timestep) {
  if (!clManager_->SetInt("TSPTraversal", tspTimestepArg_, _timestep)) {
    ERROR("RunTSPTraversal() - Failed to set timestep");
    return false;
  }

  if (!clManager_->AddBuffer("TSPTraversal", tspBrickListArg_,
                             reinterpret_cast<void*>(&brickRequest_[0]),
                             brickRequest_.size()*sizeof(int),
                             CLManager::COPY_HOST_PTR,
                             CLManager::READ_WRITE)) return false;

  if (!clManager_->PrepareProgram("TSPTraversal")) return false;
  if (!clManager_->LaunchProgram("TSPTraversal",
                                 winWidth_,
                                 winHeight_,
                                 config_->LocalWorkSizeX(),
                                 config_->LocalWorkSizeY())) return false;

  return true;
}

bool Raycaster::InitPipeline() {
  INFO("Initializing pipeline");

  if (!tsp_) {
    ERROR("InitPipeline(): No TSP set");
    return false;
  }

  if (!brickManager_) {
    ERROR("InitPipeline(): No BrickManager set");
    return false;
  }

  // Allocate space for the brick request list
  // Use 0 as default value
  brickRequest_.resize(tsp_->NumTotalNodes(), 0);
    
    glFinish();
  // Run TSP traversal for timestep 0
  if (!LaunchTSPTraversal(0)) {
    ERROR("InitPipeline() - failed to launch TSP traversal");
    return false;
  }

  // Finish TSP traversal and read results into brick request
  if (!clManager_->FinishProgram("TSPTraversal")) return false;
    
  if (!clManager_->ReadBuffer("TSPTraversal", tspBrickListArg_,
                              reinterpret_cast<void*>(&brickRequest_[0]),
                              brickRequest_.size()*sizeof(int),
                              true)) return false;

  // Free device memory
  if (!clManager_->ReleaseBuffer("TSPTraversal",tspBrickListArg_))return false;

  // Upload data for timestep 0 to PBO
  if (!brickManager_->BuildBrickList(BrickManager::EVEN, 
                                     brickRequest_)) return false;
  if (!brickManager_->DiskToPBO(BrickManager::EVEN)) return false;
  //if (!brickManager_->PBOToAtlas(BrickManager::EVEN)) return false;

  return true;
}

void Raycaster::SetAnimator(Animator *_animator) {
  animator_ = _animator;
}

void Raycaster::SetBrickManager(BrickManager *_brickManager) {
  brickManager_ = _brickManager;
}

bool Raycaster::InitCube() {
  glGetError();

  float v[] = {
   // front
   1.f, 0.f, 0.f, 1.f,
   0.f, 1.f, 0.f, 1.f,
   0.f, 0.f, 0.f, 1.f,
   1.f, 0.f, 0.f, 1.f,
   1.f, 1.f, 0.f, 1.f,
   0.f, 1.f, 0.f, 1.f,
   // right
   1.f, 0.f, 0.f, 1.f,
   1.f, 0.f, 1.f, 1.f,
   1.f, 1.f, 0.f, 1.f,
   1.f, 0.f, 1.f, 1.f,
   1.f, 1.f, 1.f, 1.f,
   1.f, 1.f, 0.f, 1.f,
   // back
   1.f, 1.f, 1.f, 1.f,
   0.f, 0.f, 1.f, 1.f,
   0.f, 1.f, 1.f, 1.f,
   1.f, 1.f, 1.f, 1.f,
   1.f, 0.f, 1.f, 1.f,
   0.f, 0.f, 1.f, 1.f,
   // left
   0.f, 0.f, 1.f, 1.f,
   0.f, 0.f, 0.f, 1.f,
   0.f, 1.f, 1.f, 1.f,
   0.f, 0.f, 0.f, 1.f,
   0.f, 1.f, 0.f, 1.f,
   0.f, 1.f, 1.f, 1.f,
   // top
   0.f, 1.f, 0.f, 1.f,
   1.f, 1.f, 0.f, 1.f,
   0.f, 1.f, 1.f, 1.f,
   0.f, 1.f, 1.f, 1.f,
   1.f, 1.f, 0.f, 1.f,
   1.f, 1.f, 1.f, 1.f,
   // bottom
   0.f, 0.f, 0.f, 1.f,
   0.f, 0.f, 1.f, 1.f,
   1.f, 0.f, 1.f, 1.f,
   0.f, 0.f, 0.f, 1.f,
   1.f, 0.f, 1.f, 1.f,
   1.f, 0.f, 0.f, 1.f
 };

  glGenVertexArrays(1, &cubeVAO_);
  glBindVertexArray(cubeVAO_);

  glGenBuffers(1, &cubePosbufferObject_);
  glBindBuffer(GL_ARRAY_BUFFER, cubePosbufferObject_);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float)*144, v, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glBindVertexArray(0);

  if (CheckGLError("InitCube()") != GL_NO_ERROR) {
    return false;
  }

  cubeInitialized_ = true;
  return true;
}

bool Raycaster::InitQuad() {
  glGetError();

  float v[] = {
    -1.f, -1.f, 0.f, 1.f,
     1.f, -1.f, 0.f, 1.f,
    -1.f,  1.f, 0.f, 1.f,
     1.f, -1.f, 0.f, 1.f,
     1.f,  1.f, 0.f, 1.f,
    -1.f,  1.0, 0.f, 1.f,
  };

  glGenVertexArrays(1, &quadVAO_);
  glBindVertexArray(quadVAO_);

  glGenBuffers(1, &quadPosbufferObject_);
  glBindBuffer(GL_ARRAY_BUFFER, quadPosbufferObject_);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float)*24, v, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glBindVertexArray(0);

  if (CheckGLError("InitQuad()") != GL_NO_ERROR) {
    return false;
  }

  quadInitialized_ = true;
  return true;
}

bool Raycaster::InitFramebuffers() {
  glGetError();

  if (winWidth_ == 0 || winHeight_ == 0) {
    ERROR("Raycaster window dimension(s) are zero");
    return false;
  }

  if (cubeFrontTex_ == NULL || cubeBackTex_ == NULL) {
    ERROR("Can't init framebuffers, textures are not set");
    return false;
  }

  // Renderbuffer for depth component
  INFO("Initializing renderbuffer for depth");
  glGenRenderbuffers(1, &renderbufferObject_);
  glBindRenderbuffer(GL_RENDERBUFFER, renderbufferObject_);
  glRenderbufferStorage(GL_RENDERBUFFER,
                        GL_DEPTH_COMPONENT,
                        winWidth_,
                        winHeight_);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  CheckGLError("Init renderbuffer");

  // Front cube
  INFO("Initializing front cube framebuffer");
  glGenFramebuffers(1, &cubeFrontFBO_);
  glBindFramebuffer(GL_FRAMEBUFFER, cubeFrontFBO_);
  glFramebufferTexture2D(GL_FRAMEBUFFER,
                         GL_COLOR_ATTACHMENT0,
                         GL_TEXTURE_2D,
                         *cubeFrontTex_,
                         0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                            GL_DEPTH_ATTACHMENT,
                            GL_RENDERBUFFER,
                            renderbufferObject_);
  GLenum status;
  status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (status != GL_FRAMEBUFFER_COMPLETE) {
    ERROR("Front cube framebuffer not complete");
    CheckGLError("Front cube framebuffer");
    return false;
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // Back cube
  INFO("Initializing back cube framebuffer");
  glGenFramebuffers(1, &cubeBackFBO_);
  glBindFramebuffer(GL_FRAMEBUFFER, cubeBackFBO_);
  glFramebufferTexture2D(GL_FRAMEBUFFER,
                         GL_COLOR_ATTACHMENT0,
                         GL_TEXTURE_2D,
                         *cubeBackTex_,
                         0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER,
                            GL_DEPTH_ATTACHMENT,
                            GL_RENDERBUFFER,
                            renderbufferObject_);
  status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (status != GL_FRAMEBUFFER_COMPLETE) {
    ERROR("Back cube framebuffer not complete");
    CheckGLError("Back cube framebuffer");
    return false;
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  if (CheckGLError("InitFramebuffers()") != GL_NO_ERROR) {
    return false;
  }

  INFO("Initializing framebuffers... complete");

  framebuffersInitialized_ = true;
  return true;
}


bool Raycaster::ReloadTransferFunctions() {

  INFO("Reloading transfer functions");
  if (!transferFunctions_[0]->ReadFile()) return false;
  if (!transferFunctions_[0]->ConstructTexture()) return false;

  if (!clManager_->AddTexture("RaycasterTSP", transferFunctionArg_,
                              transferFunctions_[0]->Texture(),
                              CLManager::TEXTURE_2D,
                              CLManager::READ_ONLY)) return false;
  return true;
}

bool Raycaster::UpdateMatrices() {
  model_ = glm::mat4(1.f);
  model_ = glm::translate(model_, glm::vec3(0.5f, 0.5f, 0.5f));
  model_ = glm::rotate(model_, roll_, glm::vec3(1.f, 0.f, 0.f));
  model_ = glm::rotate(model_, -pitch_, glm::vec3(0.f, 1.f, 0.0));
  model_ = glm::rotate(model_, yaw_, glm::vec3(0.f, 0.f, 1.f));
  model_ = glm::translate(model_, glm::vec3(-0.5f, -0.5f, -0.5f));
  /*
  view_ = glm::rotate(glm::mat4(1.f), 180.f, glm::vec3(1.f, 0.f, 0.f));
  view_ = glm::translate(view_, glm::vec3(-0.5f, -0.5f, zoom_));
  */
  glm::mat4 sgctView = sgct::Engine::instance()->getActiveViewMatrix();
  view_ = glm::translate(sgctView, glm::vec3(translateX_, 
                                             translateY_,
                                             translateZ_));
  return true;
}

bool Raycaster::BindTransformationMatrices(ghoul::opengl::ProgramObject * _program)
{
    if (!_program->setUniform("modelMatrix", model_)) return false;
    if (!_program->setUniform("viewMatrix", view_)) return false;
    if (!_program->setUniform("projectionMatrix", proj_)) return false;
    
/*
  if (!_program->BindMatrix4f("modelMatrix", &model_[0][0])) return false;
  if (!_program->BindMatrix4f("viewMatrix", &view_[0][0])) return false;
  if (!_program->BindMatrix4f("projectionMatrix", &proj_[0][0])) return false;
  */
  return true;
}

void Raycaster::SetCubeFrontTexture(ghoul::opengl::Texture  *_cubeFrontTexture) {
  cubeFrontTex_ = _cubeFrontTexture;
}

void Raycaster::SetCubeBackTexture(ghoul::opengl::Texture  *_cubeBackTexture) {
  cubeBackTex_ = _cubeBackTexture;
}

void Raycaster::SetQuadTexture(ghoul::opengl::Texture *_quadTexture) {
  quadTex_ = _quadTexture;
}

void Raycaster::SetCLManager(CLManager *_clManager) {
  clManager_ = _clManager;
}

void Raycaster::SetTSP(TSP *_tsp) {
  tsp_ = _tsp;
}

void Raycaster::SetCubeShaderProgram(ghoul::opengl::ProgramObject *_cubeShaderProgram) {
  cubeShaderProgram_ = _cubeShaderProgram;
}

void Raycaster::SetQuadShaderProgram(ghoul::opengl::ProgramObject *_quadShaderProgram) {
  quadShaderProgram_ = _quadShaderProgram;
}

bool Raycaster::ReloadShaders() {
  glGetError();
    INFO("Reloading shaders");
    if (!cubeShaderProgram_->rebuildFromFile()) return false;
    if (!quadShaderProgram_->rebuildFromFile()) return false;
  CheckGLError("ReloadShaders()");
  return true;
}

bool Raycaster::Reload() {
  if (!config_->Read()) return false; 
   INFO("Config file read");
   if (!UpdateKernelConstants()) return false;
   INFO("Kernel constants updated");
   if (!ReloadShaders()) return false;
   INFO("Shaders reloaded");
   if (!ReloadTransferFunctions()) return false;
   INFO("Transfer functions reloaded");
   if (!animator_->UpdateConfig()) return false;
   INFO("Animator updated");
  return true;
}

bool Raycaster::InitCL() {
  INFO("Initializing OpenCL");

  if (!clManager_) {
    ERROR("InitCL() - No CL manager has been set");
    return false;
  }
  
  // Init common OpenCL resources
  if (!clManager_->InitPlatform()) return false;
  if (!clManager_->InitDevices()) return false;
  if (!clManager_->CreateContext()) return false;
  if (!clManager_->CreateCommandQueue()) return false;

  // TSP traversal part of raycaster
  if (!clManager_->CreateProgram("TSPTraversal",
                                 config_->TSPTraversalKernelFilename())) {
    return false;
  }
  if (!clManager_->BuildProgram("TSPTraversal")) return false;
  if (!clManager_->CreateKernel("TSPTraversal")) return false;
  //cl_mem cubeFrontCLmem;
  if (!clManager_->AddTexture("TSPTraversal", tspCubeFrontArg_, 
                              cubeFrontTex_, CLManager::TEXTURE_2D,
                              CLManager::READ_ONLY, cubeFrontCLmem)) {
    return false;
  }

  //cl_mem cubeBackCLmem;
  if (!clManager_->AddTexture("TSPTraversal", tspCubeBackArg_,
                              cubeBackTex_, CLManager::TEXTURE_2D,
                              CLManager::READ_ONLY, cubeBackCLmem)) {
    return false;
  }
  if (!clManager_->AddBuffer("TSPTraversal", tspTSPArg_,
                             reinterpret_cast<void*>(tsp_->Data()),
                             tsp_->Size()*sizeof(int),
                             CLManager::COPY_HOST_PTR,
                             CLManager::READ_ONLY)) return false;


    LDEBUGC("RAYCASTER", config_->RaycasterKernelFilename());
  // Raycaster part
  if (!clManager_->CreateProgram("RaycasterTSP",
                                config_->RaycasterKernelFilename())) {
    return false;
  }
  if (!clManager_->BuildProgram("RaycasterTSP")) return false;
  if (!clManager_->CreateKernel("RaycasterTSP")) return false;
  if (!clManager_->AddTexture("RaycasterTSP", cubeFrontArg_, cubeFrontCLmem,  
                              CLManager::READ_ONLY)) return false;
  if (!clManager_->AddTexture("RaycasterTSP", cubeBackArg_, cubeBackCLmem, 
                              CLManager::READ_ONLY)) return false;
  if (!clManager_->AddTexture("RaycasterTSP", quadArg_, quadTex_, 
                              CLManager::TEXTURE_2D, 
                              CLManager::WRITE_ONLY)) return false;
  if (!clManager_->AddTexture("RaycasterTSP", textureAtlasArg_, 
                              brickManager_->TextureAtlas(),
                              CLManager::TEXTURE_3D, 
                              CLManager::READ_ONLY)) return false;
  if (!clManager_->AddTexture("RaycasterTSP", transferFunctionArg_,
                              transferFunctions_[0]->Texture(),
                              CLManager::TEXTURE_2D,
                              CLManager::READ_ONLY)) return false;

  // Add transfer function
  //float* tfData = transferFunctions_[0]->FloatData();
  //unsigned int tfSize = sizeof(float)*transferFunctions_[0]->Width()*4;
  //if (!clManager_->AddBuffer("RaycasterTSP", transferFunctionArg_,
  //                           reinterpret_cast<void*>(tfData),
  //                           tfSize,
  //                           CLManager::COPY_HOST_PTR,
  //                           CLManager::READ_ONLY)) return false;

  if (!clManager_->AddBuffer("RaycasterTSP", tspArg_,
                             reinterpret_cast<void*>(tsp_->Data()),
                             tsp_->Size()*sizeof(int),
                             CLManager::COPY_HOST_PTR,
                             CLManager::READ_ONLY)) return false;

  // Update and add kernel constants
  if (!UpdateKernelConstants()) return false;

  return true;
}

bool Raycaster::UpdateKernelConstants() {

  INFO("Updating kernel constants");

  if (!tsp_) {
    ERROR("TSP not set, cannot update kernel constants");
    return false;
  }

  if (!config_) {
    ERROR("Config not set, cannot update kernel constants");
    return false;
  }

  kernelConstants_.gridType_ = static_cast<int>(brickManager_->GridType());
  kernelConstants_.stepsize_ = config_->RaycasterStepsize();
  kernelConstants_.intensity_ = config_->RaycasterIntensity();
  kernelConstants_.numTimesteps_ = static_cast<int>(tsp_->NumTimesteps());
  kernelConstants_.numValuesPerNode_ = 
    static_cast<int>(tsp_->NumValuesPerNode());
  kernelConstants_.numOTNodes_ = static_cast<int>(tsp_->NumOTNodes());
  kernelConstants_.numBoxesPerAxis_ =
    static_cast<int>(tsp_->NumBricksPerAxis());
  kernelConstants_.temporalTolerance_ = config_->TemporalErrorTolerance();
  kernelConstants_.spatialTolerance_ = config_->SpatialErrorTolerance();
  kernelConstants_.rootLevel_ = static_cast<int>(tsp_->NumOTLevels()) - 1;
  kernelConstants_.paddedBrickDim_ = static_cast<int>(tsp_->PaddedBrickDim());

  traversalConstants_.gridType_ = static_cast<int>(brickManager_->GridType());
  traversalConstants_.stepsize_ = config_->TSPTraversalStepsize();
  traversalConstants_.numTimesteps_ = static_cast<int>(tsp_->NumTimesteps());
  traversalConstants_.numValuesPerNode_ = 
    static_cast<int>(tsp_->NumValuesPerNode());
  traversalConstants_.numOTNodes_ = static_cast<int>(tsp_->NumOTNodes());
  traversalConstants_.temporalTolerance_ = config_->TemporalErrorTolerance();
  traversalConstants_.spatialTolerance_ = config_->SpatialErrorTolerance();
  
  
    std::string _loggerCat = "KOLLA KONSTANTER";
    LDEBUG("traversalConstants_.gridType_: " << traversalConstants_.gridType_);
    LDEBUG("traversalConstants_.numOTNodes_: " << traversalConstants_.numOTNodes_);
    LDEBUG("traversalConstants_.numTimesteps_: " << traversalConstants_.numTimesteps_);
    LDEBUG("traversalConstants_.numValuesPerNode_: " << traversalConstants_.numValuesPerNode_);
    LDEBUG("traversalConstants_.spatialTolerance_: " << traversalConstants_.spatialTolerance_);
    LDEBUG("traversalConstants_.stepsize_: " << traversalConstants_.stepsize_);
    LDEBUG("traversalConstants_.temporalTolerance_: " << traversalConstants_.temporalTolerance_);
  

  if (!clManager_->AddBuffer("RaycasterTSP", constantsArg_,
                             reinterpret_cast<void*>(&kernelConstants_),
                             sizeof(KernelConstants),
                             CLManager::COPY_HOST_PTR,
                             CLManager::READ_ONLY)) return false;
  if (!clManager_->AddBuffer("TSPTraversal", tspConstantsArg_,
                             reinterpret_cast<void*>(&traversalConstants_),
                             sizeof(TraversalConstants),
                             CLManager::COPY_HOST_PTR,
                             CLManager::READ_ONLY)) return false;

  return true;
}

void Raycaster::AddTransferFunction(TransferFunction *_transferFunction) {
  transferFunctions_.push_back(_transferFunction);
}

void Raycaster::SetModelParams(float _pitch, float _yaw, float _roll) {
  pitch_ = _pitch;
  yaw_ = _yaw;
  roll_ = _roll;
}

void Raycaster::SetViewParams(float _translateX,
                              float _translateY,
                              float _translateZ) {
  translateX_ = _translateX;
  translateY_ = _translateY;
  translateZ_ = _translateZ;
}
