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

//<<<<<<< HEAD
//#ifndef INTERACTIONHANDLER_H
//#define INTERACTIONHANDLER_H
//
//#include <openspace/scripting/scriptengine.h>
//#include <openspace/util/powerscaledcoordinate.h>
//#include <openspace/util/powerscaledscalar.h>
//
//// std includes
//#include <vector>
//#include <mutex>
//#include <map>
//#include <functional>
//
//namespace openspace {
//
//// Forward declare to minimize dependencies
//class Camera;
//class SceneGraphNode;
//class ExternalControl;
//
//class InteractionHandler {
//public:
//    InteractionHandler(void);
//    InteractionHandler(const InteractionHandler& src);
//    InteractionHandler& operator=(const InteractionHandler& rhs);
//	virtual ~InteractionHandler();
//
//	void enable();
//	void disable();
//	const bool isEnabled() const;
//
//	void connectDevices();
//	void addExternalControl(ExternalControl* controller);
//
//	void setCamera(Camera *camera = nullptr);
//	void setOrigin(SceneGraphNode* node);
//
//	Camera* getCamera() const;
//	const psc getOrigin() const;
//	void lockControls();
//	void unlockControls();
//
//	void setFocusNode(SceneGraphNode *node);
//	
//	void orbit(const glm::quat &rotation);
//	void rotate(const glm::quat &rotation);
//	void distance(const PowerScaledScalar &distance, size_t iterations = 0);
//=======
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
    InteractionHandler();

	~InteractionHandler();

	void setKeyboardController(KeyboardController* controller);
	void setMouseController(MouseController* controller);
	void addController(Controller* controller);

	void lockControls();
	void unlockControls();

	void update(double deltaTime);

	void setFocusNode(SceneGraphNode* node);
	const SceneGraphNode* const focusNode() const;
	void setCamera(Camera* camera);
	const Camera* const camera() const;

    void keyboardCallback(int key, int action);
	void mouseButtonCallback(int button, int action);
	void mousePositionCallback(int x, int y);
	void mouseScrollWheelCallback(int pos);

//<<<<<<< HEAD
	//double dt();
//=======
	double deltaTime() const;

	void orbitDelta(const glm::quat& rotation);

	void rotateDelta(const glm::quat& rotation);

	void distanceDelta(const PowerScaledScalar& distance, size_t iterations = 0);

	void lookAt(const glm::quat& rotation);

	void setRotation(const glm::quat& rotation);

	void resetKeyBindings();
	void bindKey(int key, const std::string& lua);

	/**
	* Returns the Lua library that contains all Lua functions available to affect the
	* interaction. The functions contained are
	* - openspace::luascriptfunctions::setOrigin
	* \return The Lua library that contains all Lua functions available to affect the
	* interaction
	*/
	static scripting::ScriptEngine::LuaLibrary luaLibrary();
	
private:
	friend class Controller;

//<<<<<<< HEAD
//	Camera* _camera;
//	bool _enabled;
//	SceneGraphNode* _node;
//	
//	double _dt;
//=======
    InteractionHandler(const InteractionHandler&) = delete;
    InteractionHandler& operator=(const InteractionHandler&) = delete;
	InteractionHandler(InteractionHandler&&) = delete;
	InteractionHandler& operator=(InteractionHandler&&) = delete;

	Camera* _camera;
	SceneGraphNode* _focusNode;

	double _deltaTime;
	std::mutex _mutex;

//<<<<<<< HEAD
//	// used for calling when updating and deallocation
//	std::vector<ExternalControl*> _controllers;
//
//	// for locking and unlocking
//	std::mutex _cameraGuard;

	bool _validKeyLua;
	std::multimap<int, std::string > _keyLua;
	
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
