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

#include <openspace/interaction/keyboardcontroller.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/time.h>

namespace openspace {
namespace interaction {

void KeyboardControllerFixed::keyPressed(KeyAction action, Keys key) {
	// TODO package in script
	const double speed = 2.75;
	const double dt = deltaTime();
	if(action == KeyAction::Press|| action == KeyAction::Repeat) {
		if (key == Keys::S) {
		    glm::vec3 euler(speed * dt, 0.0, 0.0);
		    glm::quat rot = glm::quat(euler);
		    orbitDelta(rot);
		}
		if (key == Keys::W) {
		    glm::vec3 euler(-speed * dt, 0.0, 0.0);
		    glm::quat rot = glm::quat(euler);
		    orbitDelta(rot);
		}
		if (key == Keys::A) {
		    glm::vec3 euler(0.0, -speed * dt, 0.0);
		    glm::quat rot = glm::quat(euler);
		    orbitDelta(rot);
		}
		if (key == Keys::D) {
		    glm::vec3 euler(0.0, speed * dt, 0.0);
		    glm::quat rot = glm::quat(euler);
		    orbitDelta(rot);
		}
		if (key == Keys::Q) {
			Time::ref().advanceTime(dt);
		}
		if (key == Keys::Right) {
		    glm::vec3 euler(0.0, speed * dt, 0.0);
		    glm::quat rot = glm::quat(euler);
		    rotateDelta(rot);
		}
		if (key == Keys::Left) {
		    glm::vec3 euler(0.0, -speed * dt, 0.0);
		    glm::quat rot = glm::quat(euler);
		    rotateDelta(rot);
		}
		if (key == Keys::Down) {
		    glm::vec3 euler(speed * dt, 0.0, 0.0);
		    glm::quat rot = glm::quat(euler);
		    rotateDelta(rot);
		}
		if (key == Keys::Up) {
		    glm::vec3 euler(-speed * dt, 0.0, 0.0);
		    glm::quat rot = glm::quat(euler);
		    rotateDelta(rot);
		}
		if (key == Keys::R) {
		    PowerScaledScalar dist(-speed * dt, 0.0);
		    distanceDelta(dist);
		}
		if (key == Keys::F) {
		    PowerScaledScalar dist(speed * dt, 0.0);
		    distanceDelta(dist);
		}
		if (key == Keys::T) {
			PowerScaledScalar dist(-speed * pow(10, 11) * dt, 0.0);
			distanceDelta(dist);
		}
		//if (key == Keys::G) {
		//	acc += 0.001;
		//	PowerScaledScalar dist(speed * pow(10, 8 * acc) * dt, 0.0);
		//	distanceDelta(dist);
		//}
		if (key == Keys::Y) {
			PowerScaledScalar dist(-speed * 100.0 * dt, 6.0);
			distanceDelta(dist);
		}
		if (key == Keys::H) {
			PowerScaledScalar dist(speed * 100.0 * dt, 6.0);
			distanceDelta(dist);
		}
	
		if (key == Keys::KeypadSubtract) {
			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
			s[1] -= 0.5;
			OsEng.renderEngine().camera()->setScaling(s);
		}
		if (key == Keys::KeypadAdd) {
			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
			s[1] += 0.5;
			OsEng.renderEngine().camera()->setScaling(s);
		}
	}
	/*
	if (key == '1') {
	    SceneGraphNode* node = getSceneGraphNode("sun");
	
	    setFocusNode(node);
	    getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 10.0));
	    getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
	}
	
	if (key == '2') {
	    SceneGraphNode* node = getSceneGraphNode("earth");
	
	    setFocusNode(node);
	    getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 1.0, 8.0));
	    getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
	}
	
	
	if (key == '3') {
	    SceneGraphNode* node = getSceneGraphNode("moon");
	
	    setFocusNode(node);
	    getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 8.0));
	    getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
	}
	*/
}

void KeyboardControllerLua::keyPressed(KeyAction action, Keys key)
{

}

} // namespace interaction
} // namespace openspace