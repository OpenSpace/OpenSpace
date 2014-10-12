/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __INTERACTIONHANDLER_H__
#define __INTERACTIONHANDLER_H__

#include <openspace/interaction/keyboardcontroller.h>
#include <openspace/interaction/mousecontroller.h>

#include <mutex>

namespace openspace {

class Camera;
class SceneGraphNode;

namespace interaction {

class InteractionHandler {
public:
    InteractionHandler()
		: _camera(nullptr)
		, _focusNode(nullptr)
		, _keyboardController(nullptr)
		, _mouseController(nullptr)
	{
	}

	~InteractionHandler() {
		delete _keyboardController;
		delete _mouseController;
		for (size_t i = 0; i < _controllers.size(); ++i)
			delete _controllers[i];

	}

	void setKeyboardController(KeyboardController* controller) {
		delete _keyboardController;
		_keyboardController = controller;
		_keyboardController->setHandler(this);
	}

	void setMouseController(MouseController* controller) {
		delete _mouseController;
		_mouseController = controller;
		_mouseController->setHandler(this);
	}

	void addController(Controller* controller) {
		_controllers.push_back(controller);
		controller->setHandler(this);
	}

	void lockControls() {
		_mutex.lock();
	}

	void unlockControls() {
		_mutex.unlock();
	}

	void update(double deltaTime) {
		_deltaTime = deltaTime;
	}

	void setFocusNode(SceneGraphNode* node) {
		_focusNode = node;
	}

    void keyboardCallback(int key, int action) {
		if (_keyboardController)
			_keyboardController->keyPressed(KeyAction(action), Keys(key));
	}

	void mouseButtonCallback(int button, int action) {
		if (_mouseController)
			_mouseController->button(MouseAction(action), MouseButton(button));
	}
    
	void mousePositionCallback(int x, int y) {
		if (_mouseController)
			// TODO Remap screen coordinates to [0,1]
			_mouseController->move(float(x), float(y));
	}
    
	void mouseScrollWheelCallback(int pos) {
		if (_mouseController)
			_mouseController->scrollWheel(float(pos));
	}

private:
	friend class Controller;

    InteractionHandler(const InteractionHandler&) = delete;
    InteractionHandler& operator=(const InteractionHandler&) = delete;
	InteractionHandler(InteractionHandler&&) = delete;
	InteractionHandler& operator=(InteractionHandler&&) = delete;

	Camera* _camera;
	SceneGraphNode* _focusNode;

	double _deltaTime;
	std::mutex _mutex;

	KeyboardController* _keyboardController;
	MouseController* _mouseController;
	std::vector<Controller*> _controllers;


 //   glm::vec3 mapToTrackball(glm::vec2 mousePos);
 //   glm::vec3 mapToCamera(glm::vec3 trackballPos);
 //   void trackballRotate(int x, int y);
};

} // namespace interaction
} // namespace openspace

#endif // __INTERACTIONHANDLER_H__
