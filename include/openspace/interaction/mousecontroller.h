/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __MOUSECONTROLLER_H__
#define __MOUSECONTROLLER_H__

#include <openspace/interaction/controller.h>

#include <openspace/interaction/mouse.h>

#include <ghoul/glm.h>

namespace openspace {
namespace interaction {

class MouseController : public Controller {
public:
	MouseController();
    virtual ~MouseController() {}

	virtual void button(MouseAction action, MouseButton button) = 0;
	virtual void move(float x, float y) = 0;
	virtual void scrollWheel(int pos) = 0;
	virtual void update(const double& dt) = 0;

protected:
	glm::vec3 _lastTrackballPos;
	bool _isMouseBeingPressedAndHeld;

	glm::vec3 mapToTrackball(glm::vec2 mousePos);

	glm::vec3 mapToCamera(glm::vec3 trackballPos);

	void trackballRotate(int x, int y);
};

class TrackballMouseController : public MouseController {
public:
	TrackballMouseController();

	void button(MouseAction action, MouseButton button);

	void move(float x, float y);

	void scrollWheel(int pos);

	void update(const double& dt);

protected:
	bool _leftMouseButtonDown;
	glm::vec3 _previousTrackballPos;
};

class OrbitalMouseController : public MouseController {
public:
	OrbitalMouseController();

	void button(MouseAction action, MouseButton button);

	void move(float x, float y);

	void scrollWheel(int pos);

	void update(const double& dt);

protected:
	bool _leftMouseButtonDown;
	bool _rightMouseButtonDown;
	bool _middleMouseButtonDown;
	glm::vec2 _previousCursorPos[3];
	glm::vec2 _currentCursorPos;
	glm::vec2 _currentCursorDiff[3];
	float _rotationSpeed;
	float _navigationSpeed;

private:
	enum MouseButtons{ ButtonLeft = 0, ButtonRight, ButtonMiddle };
};

} // namespace interaction
} // namespace openspace

#endif // __MOUSECONTROLLER_H__