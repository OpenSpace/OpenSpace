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

#ifndef INTERACTIONHANDLER_H
#define INTERACTIONHANDLER_H

#include <openspace/scripting/scriptengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/powerscaledscalar.h>

// std includes
#include <vector>
#include <mutex>
#include <map>
#include <functional>

namespace openspace {

// Forward declare to minimize dependencies
class Camera;
class SceneGraphNode;
class ExternalControl;

class InteractionHandler {
public:
    InteractionHandler(void);
    InteractionHandler(const InteractionHandler& src);
    InteractionHandler& operator=(const InteractionHandler& rhs);
	virtual ~InteractionHandler();

	void enable();
	void disable();
	const bool isEnabled() const;

	void connectDevices();
	void addExternalControl(ExternalControl* controller);

	void setCamera(Camera *camera = nullptr);
	void setOrigin(SceneGraphNode* node);

	Camera* getCamera() const;
	const psc getOrigin() const;
	void lockControls();
	void unlockControls();

	void setFocusNode(SceneGraphNode *node);
	
	void orbit(const glm::quat &rotation);
	void rotate(const glm::quat &rotation);
	void distance(const PowerScaledScalar &distance, size_t iterations = 0);

	void lookAt(const glm::quat &rotation);
	void setRotation(const glm::quat &rotation);

	void update(const double dt);

	double getDt();

    void keyboardCallback(int key, int action);
    void mouseButtonCallback(int key, int action);
    void mousePositionCallback(int x, int y);
    void mouseScrollWheelCallback(int pos);

    void addKeyCallback(int key, std::function<void(void)> f);

	/**
	* Returns the Lua library that contains all Lua functions available to affect the
	* interaction. The functions contained are
	* - openspace::luascriptfunctions::setOrigin
	* \return The Lua library that contains all Lua functions available to affect the
	* interaction
	*/
	static scripting::ScriptEngine::LuaLibrary luaLibrary();
	
private:
    glm::vec3 mapToTrackball(glm::vec2 mousePos);
    glm::vec3 mapToCamera(glm::vec3 trackballPos);
    void trackballRotate(int x, int y);

	Camera* camera_;
	bool enabled_;
	SceneGraphNode *node_;
	
	double dt_;

	glm::vec3 _lastTrackballPos;
	bool _leftMouseButtonDown, _isMouseBeingPressedAndHeld;

	// used for calling when updating and deallocation
	std::vector<ExternalControl*> controllers_;

	// for locking and unlocking
	std::mutex cameraGuard_;

	std::multimap<int, std::function<void(void)> > _keyCallbacks;
	
};

} // namespace openspace

#endif
