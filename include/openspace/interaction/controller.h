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

#ifndef __CONTROLLER_H__
#define __CONTROLLER_H__

#include <openspace/scenegraph/scenegraphnode.h>

#include <ghoul/glm.h>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {
namespace interaction {

class InteractionHandler;

class Controller {
public:
	Controller() :
		_handler(nullptr)
	{}

	void setHandler(InteractionHandler* handler) {
		_handler = handler;
	}

protected:
	SceneGraphNode* focusNode() const {
		assert(_handler);
		return _handler->_focusNode;
	}

	Camera* camera() const {
		assert(_handler);
		return _handler->_camera;
	}


	double deltaTime() const {
		assert(_handler);
		return _handler->_deltaTime;
	}

	void orbitDelta(const glm::quat& rotation) {
		assert(_handler);
		_handler->lockControls();
	
		// the camera position
		psc relative = _handler->_camera->position();

		// should be changed to something more dynamic =)
		psc origin;
		if (_handler->_focusNode) {
			origin = _handler->_focusNode->worldPosition();
		}

		psc relative_origin_coordinate = relative - origin;
		//glm::mat4 rotation_matrix = glm::mat4_cast(glm::inverse(rotation));
		//relative_origin_coordinate = relative_origin_coordinate.vec4() * glm::inverse(rotation);
		relative_origin_coordinate = glm::inverse(rotation) * relative_origin_coordinate.vec4();
		relative = relative_origin_coordinate + origin;

		_handler->_camera->setPosition(relative);
		//camera_->rotate(rotation);
		//camera_->setRotation(glm::mat4_cast(rotation));

		glm::mat4 la = glm::lookAt(_handler->_camera->position().vec3(), origin.vec3(), glm::rotate(rotation, _handler->_camera->lookUpVector()));
		_handler->_camera->setRotation(la);
		//camera_->setLookUpVector();

		_handler->unlockControls();
	}

	void rotateDelta(const glm::quat& rotation) {
		assert(_handler);
		_handler->lockControls();
		_handler->_camera->rotate(rotation);
		_handler->unlockControls();
	}

	void distanceDelta(const PowerScaledScalar& distance) {
		assert(_handler);
		_handler->lockControls();
	
		psc relative = _handler->_camera->position();
		const psc origin = (_handler->_focusNode) ? _handler->_focusNode->worldPosition() : psc();

		psc relative_origin_coordinate = relative - origin;
		const glm::vec3 dir(relative_origin_coordinate.direction());
		glm:: vec3 newdir = dir * distance[0];
		relative_origin_coordinate = newdir;
		relative_origin_coordinate[3] = distance[1];
		relative = relative + relative_origin_coordinate;

		relative_origin_coordinate = relative - origin;
		newdir = relative_origin_coordinate.direction();

		// update only if on the same side of the origin
		if(glm::angle(newdir, dir) < 90.0f)
			_handler->_camera->setPosition(relative);
	
		_handler->unlockControls();
	}

	void lookAt(const glm::quat& rotation) {
		assert(_handler);
	}

	void setRotation(const glm::quat& rotation) {
		assert(_handler);
	}

private:
	InteractionHandler* _handler;
};

} // namespace interaction
} // namespace openspace

#endif // __CONTROLLER_H__