#ifndef RENDERER_H
#define RENDERER_H

/* 
Author: Victor Sand (victor.sand@gmail.com)
Renderer abstract class, Render() functions needs to be
implemented by subclasses. 
TODO No real use right now but to handle key and mouse state
TODO Write better and manageable key state functionality
*/

#include <map>

namespace osp {

class Renderer {
public:
  virtual bool Render(float _timestep) = 0;
  virtual ~Renderer();
  void SetKeyPressed(int _key, bool _pressed);
  bool KeyPressed(int _key) const;
  void SetWinWidth(unsigned int _winWidth);
  void SetWinHeight(unsigned int _winHeight);
  void SetMousePosition(float _mouseX, float _mouseY);
  void SetMousePressed(bool _leftPressed, bool _rightPressed);
  float FPS() const;
protected:
  Renderer();
  // Window
  unsigned int winWidth_;
  unsigned int winHeight_;
  // Mouse
  bool leftMouseDown_;
  bool rightMouseDown_;
  float currentMouseX_;
  float currentMouseY_;
  float lastMouseX_;
  float lastMouseY_;
  // Keyboard
  std::map<int, bool> keysPressed_; // Is key pressed or not?
};

}

#endif
