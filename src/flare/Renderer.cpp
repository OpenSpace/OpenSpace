/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#include <openspace/flare/Renderer.h>
#include <utility>

using namespace osp;

Renderer::Renderer() 
  : winWidth_(0), 
    winHeight_(0),
    leftMouseDown_(false),
    rightMouseDown_(false),
    currentMouseX_(0),
    currentMouseY_(0),
    lastMouseX_(0),
    lastMouseY_(0) {
}

Renderer::~Renderer() {
}


void Renderer::SetWinWidth(unsigned int _winWidth) {
  winWidth_ = _winWidth;
}

void Renderer::SetWinHeight(unsigned int _winHeight) {
  winHeight_ = _winHeight;
}

void Renderer::SetMousePosition(float _mouseX, float _mouseY) {
  lastMouseX_ = currentMouseX_;
  lastMouseY_ = currentMouseY_;
  currentMouseX_ = _mouseX;
  currentMouseY_ = _mouseY;
}

void Renderer::SetMousePressed(bool _leftPressed, bool _rightPressed) {
  leftMouseDown_ = _leftPressed;
  rightMouseDown_ = _rightPressed;
}

void Renderer::SetKeyPressed(int _key, bool _pressed) {
  std::map<int, bool>::iterator it;
  it = keysPressed_.find(_key);
  if (it == keysPressed_.end()) {
    keysPressed_.insert(std::make_pair(_key, _pressed));
  } else {
    it->second = _pressed;
  }
}

bool Renderer::KeyPressed(int _key) const {
  std::map<int, bool>::const_iterator it;
  it = keysPressed_.find(_key);
  if (it == keysPressed_.end()) {
    return false;
  } else {
    return it->second;
  }
}
