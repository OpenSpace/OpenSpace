#ifndef ANIMATOR_H
#define ANIMATOR_H

/*
 * Author: Victor Sand (victor.sand@gmail.com) 
 * Encapsulates animation logic 
 *
 */

namespace osp {

class Config;

class Animator {
public:
  static Animator * New(Config *_config);

  // Signals the animator to update its state if needed
  void Update(float _elapsedTime);
  // Pauses if unpaused, unpauses if paused
  void TogglePause();
  // If FPS mode is on, don't wait. Update every timestep.
  void ToggleFPSMode();

  void SetPaused(bool _paused);
  void SetFPSMode(bool _fpsMode);
  void ManualTimestep(int _manualTimestep);

  unsigned int CurrentTimestep() const { return currentTimestep_; }
  unsigned int NextTimestep() const { 
    return currentTimestep_ < numTimesteps_-1 ? currentTimestep_+1 : 0;
  }

  void SetCurrentTimestep(unsigned int _timestep);
  void SetNumTimesteps(unsigned int _numTimesteps);
  void IncTimestep();
  void DecTimestep();

  bool UpdateConfig();

private:
  Animator();
  Animator(Config *_config);
  Animator(const Animator&) { }

  Config *config_;
  
  // Number of timesteps before looping occurs
  unsigned int numTimesteps_;
  // Current timestep, the output of the Animator
  unsigned int currentTimestep_;
  bool fpsMode_;
  bool paused_;
  // Keeps track of elapsed time between timestep updates 
  float elapsedTime_;
  // Time before timestep gets updates
  float refreshInterval_;
};

}

#endif

