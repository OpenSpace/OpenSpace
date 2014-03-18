/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#ifndef SGCT_WIN_MANAGER_H_
#define SGCT_WIN_MANAGER_H_

#include <sgct.h>
#include <glm/glm.hpp>
#include <iostream>

namespace osp {

class Config;
class Raycaster;
class Animator;

class SGCTWinManager {
public:

  static SGCTWinManager * Instance();
   ~SGCTWinManager();
  
  void SetRaycaster(Raycaster *_Raycaster);
  void SetConfig(Config *_config);
  void SetAnimator(Animator *_animator);

  // Init engine, set up OpenGL context
  bool InitEngine(int _argc, char **_argv,
                  sgct::Engine::RunMode _runMode);
  bool Render();

  unsigned int FBOHandle() const {
//	return engine->getFBOPtr()->getBufferId();
	return engine_->getActiveWindowPtr()->getFBOPtr()->getBufferID();
//    GLint fbo;
//	glGetIntegerv (GL_FRAMEBUFFER_BINDING, &fbo);
//	return static_cast<unsigned int>(fbo);
  }

  glm::mat4 ProjMatrix() const {
     return engine_->getActiveProjectionMatrix();
  }

  glm::mat4 ViewMatrix() const {
    return engine_->getActiveViewMatrix();
  }

//  const int* GetActiveViewPort() const {
////    return engine_->getActiveViewport();
//    return engine_->getActiveWindowPtr()->getCurrentViewport()->;
//  }

private:
  SGCTWinManager();
  SGCTWinManager(const SGCTWinManager&);

  Config *config_;
  sgct::Engine *engine_;
  Raycaster *raycaster_;

  // Animator and animator state
  Animator *animator_;
  static sgct::SharedBool animationPaused_;
  static sgct::SharedBool fpsMode_;
  static sgct::SharedInt manualTimestep_;

  // Flag for reloading config
  static sgct::SharedBool reloadFlag_;

  // Navigation state
  // Model params
  static sgct::SharedFloat pitch_;
  static sgct::SharedFloat yaw_;
  static sgct::SharedFloat roll_;
  // View
  static sgct::SharedFloat translateX_;
  static sgct::SharedFloat translateY_;
  static sgct::SharedFloat translateZ_;
  // Mouse state
  static bool leftMouseButton_;
  static double currentMouseX_;
  static double currentMouseY_;
  static double lastMouseX_;
  static double lastMouseY_;

  // Helper methods
  void InitNavigation();
  void UpdateNavigation();

  // Callback functions
  static void Draw();
  static void PreSync();
  static void PostDraw();
  static void Encode();
  static void Decode();
  static void Keyboard(int _key, int _action);
  static void Mouse(int _button, int _action);

  // Render timing variables
  static float oldTime_;
  static float currentTime_;
  static sgct::SharedFloat elapsedTime_;

  // Singleton instance
  static SGCTWinManager *instance_;

};

}


#endif
