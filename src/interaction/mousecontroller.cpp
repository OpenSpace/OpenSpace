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

#include <openspace/interaction/mousecontroller.h>

#include <openspace/interaction/interactionhandler.h>

namespace openspace {
namespace interaction {

MouseController::MouseController()
	: _lastTrackballPos(0.f)
	, _isMouseBeingPressedAndHeld(false)
{}

glm::vec3 MouseController::mapToTrackball(glm::vec2 mousePos) {
	const float RADIUS = 0.5; // Sphere radius
	glm::vec3 out = glm::vec3(mousePos.x - 0.5, -1.0*(mousePos.y - 0.5), 0);

	// Mapping according to Holroyds trackball
	// Piece-wise sphere + hyperbolic sheet
	if (out.x*out.x + out.y*out.y <= RADIUS*RADIUS / 2.0) {
		//Spherical Region
		out.z = RADIUS*RADIUS - (out.x*out.x + out.y*out.y);
		out.z = out.z > 0.0f ? sqrtf(out.z) : 0.0f;
	}
	else { //Hyperbolic Region - for smooth z values
		out.z = (RADIUS*RADIUS) / (2.0f*sqrt(out.x*out.x + out.y*out.y));
	}

	return glm::normalize(out);
}

glm::vec3 MouseController::mapToCamera(glm::vec3 trackballPos) {
	//	return glm::vec3((sgct::Engine::instance()->getActiveViewMatrix() * glm::vec4(trackballPos,0)));

	//Get x,y,z axis vectors of current camera view
	glm::vec3 currentViewYaxis = glm::normalize(_handler->camera()->lookUpVector());
	psc viewDir = _handler->camera()->position() - _handler->focusNode()->worldPosition();
	glm::vec3 currentViewZaxis = glm::normalize(viewDir.vec3());
	glm::vec3 currentViewXaxis = glm::normalize(glm::cross(currentViewYaxis, currentViewZaxis));

	//mapping to camera co-ordinate
	currentViewXaxis *= trackballPos.x;
	currentViewYaxis *= trackballPos.y;
	currentViewZaxis *= trackballPos.z;
	return (currentViewXaxis + currentViewYaxis + currentViewZaxis);
}

void MouseController::trackballRotate(int x, int y) {
	// Normalize mouse coordinates to [0,1]
	float width = static_cast<float>(sgct::Engine::instance()->getActiveXResolution());
	float height = static_cast<float>(sgct::Engine::instance()->getActiveYResolution());
	glm::vec2 mousePos = glm::vec2((float)x / width, (float)y / height);

	mousePos = glm::clamp(mousePos, -0.5, 1.5); // Ugly fix #1: Camera position becomes NaN on mouse values outside [-0.5, 1.5]
	//mousePos[1] = 0.5; 							// Ugly fix #2: Tempoarily only allow rotation around y

	glm::vec3 curTrackballPos = mapToTrackball(mousePos);
	//	LDEBUG(mousePos.x << ", " << mousePos.y << " = " << curTrackballPos.x << ", " << curTrackballPos.y << ", " << curTrackballPos.z);

	// Disable movement on the first click for extra smoothness
	if (!_isMouseBeingPressedAndHeld) {
		_lastTrackballPos = curTrackballPos;
		_isMouseBeingPressedAndHeld = true;
	}

	if (curTrackballPos != _lastTrackballPos) {
		// calculate rotation angle (in radians)
		float rotationAngle = glm::angle(curTrackballPos, _lastTrackballPos);
		rotationAngle *= static_cast<float>(_handler->deltaTime()) * 100.0f;

		// Map trackballpos to camera
		//		glm::vec3 trackballMappedToCamera = mapToCamera(_lastTrackballPos - curTrackballPos);
		//		psc currentCamPos = camera_->getPosition();
		//		glm::vec3 nextCamPos = currentCamPos.getVec3f() + trackballMappedToCamera;
		//		glm::vec3 rotationAxis = glm::cross(currentCamPos.getVec3f(), nextCamPos);

		glm::vec3 rotationAxis = glm::cross(_lastTrackballPos, curTrackballPos);
		rotationAxis = glm::normalize(rotationAxis);
		glm::quat quaternion = glm::angleAxis(rotationAngle, rotationAxis);

		// Apply quaternion to camera
		_handler->orbitDelta(quaternion);

		_lastTrackballPos = curTrackballPos;
	}
}


TrackballMouseController::TrackballMouseController()
	: MouseController()
	, _leftMouseButtonDown(false)
{}

void TrackballMouseController::button(MouseAction action, MouseButton button) {
	if (button == MouseButton::Left && action == MouseAction::Press)
			_leftMouseButtonDown = true;
	else if (button == MouseButton::Left && action == MouseAction::Release) {
		_leftMouseButtonDown = false;
		_isMouseBeingPressedAndHeld = false;
	}
}

void TrackballMouseController::move(float x, float y) {
	if (_leftMouseButtonDown)
		trackballRotate(static_cast<int>(x), static_cast<int>(y));
}

void TrackballMouseController::scrollWheel(int pos) {
	const float speed = 4.75f;
	const float dt = static_cast<float>(_handler->deltaTime());
	if (pos < 0) {
		PowerScaledScalar dist(speed * dt, 0.0f);
		_handler->distanceDelta(dist);
	}
	else if (pos > 0) {
		PowerScaledScalar dist(-speed * dt, 0.0f);
		_handler->distanceDelta(dist);
	}
}

void TrackballMouseController::update(const double& dt){

}


OrbitalMouseController::OrbitalMouseController()
: MouseController()
, _leftMouseButtonDown(false)
, _rightMouseButtonDown(false)
, _middleMouseButtonDown(false)
, _currentCursorPos(0)
, _rotationSpeed(10.f)
, _navigationSpeed(3.f)
{
	for (int n = 0; n < 3; ++n){
		_previousCursorPos[n] = glm::vec2(0);
	}
}

void OrbitalMouseController::button(MouseAction action, MouseButton button) {
	if (button == MouseButton::Left){
		if (action == MouseAction::Press){
			_leftMouseButtonDown = true;
			double mouseX, mouseY;
			sgct::Engine::instance()->getMousePos(sgct::Engine::instance()->getActiveWindowPtr()->getId(), &mouseX, &mouseY);
			_previousCursorPos[MouseButtons::ButtonLeft] = glm::vec2(static_cast<float>(mouseX), static_cast<float>(mouseY));
		}
		else if (action == MouseAction::Release) {
			_leftMouseButtonDown = false;
			_currentCursorDiff[MouseButtons::ButtonLeft] = glm::vec2(0);
		}
	}
	else if (button == MouseButton::Right){
		if (action == MouseAction::Press){
			_rightMouseButtonDown = true;
			double mouseX, mouseY;
			sgct::Engine::instance()->getMousePos(sgct::Engine::instance()->getActiveWindowPtr()->getId(), &mouseX, &mouseY);
			_previousCursorPos[MouseButtons::ButtonRight] = glm::vec2(static_cast<float>(mouseX), static_cast<float>(mouseY));
		}
		else if (action == MouseAction::Release) {
			_rightMouseButtonDown = false;
			_currentCursorDiff[MouseButtons::ButtonRight] = glm::vec2(0);
		}
	}
	else if (button == MouseButton::Middle){
		if (action == MouseAction::Press){
			_middleMouseButtonDown = true;
			double mouseX, mouseY;
			sgct::Engine::instance()->getMousePos(sgct::Engine::instance()->getActiveWindowPtr()->getId(), &mouseX, &mouseY);
			_previousCursorPos[MouseButtons::ButtonMiddle] = glm::vec2(static_cast<float>(mouseX), static_cast<float>(mouseY));
		}
		else if (action == MouseAction::Release) {
			_middleMouseButtonDown = false;
			_currentCursorDiff[MouseButtons::ButtonMiddle] = glm::vec2(0);
		}
	}

}

void OrbitalMouseController::move(float x, float y) {
	int winID = sgct::Engine::instance()->getActiveWindowPtr()->getId();
	double mouseX, mouseY;
	sgct::Engine::instance()->getMousePos(winID, &mouseX, &mouseY);
	_currentCursorPos = glm::vec2(static_cast<float>(mouseX), static_cast<float>(mouseY));

	if (_leftMouseButtonDown){		
		_currentCursorDiff[MouseButtons::ButtonLeft] = (_currentCursorPos - _previousCursorPos[MouseButtons::ButtonLeft]) / glm::vec2(static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getXResolution()), static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getYResolution()));
	}
	if (_rightMouseButtonDown){
		_currentCursorDiff[MouseButtons::ButtonRight] = (_currentCursorPos - _previousCursorPos[MouseButtons::ButtonRight]) / glm::vec2(static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getXResolution()), static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getYResolution()));
	}
	if (_middleMouseButtonDown){
		_currentCursorDiff[MouseButtons::ButtonMiddle] = (_currentCursorPos - _previousCursorPos[MouseButtons::ButtonMiddle]) / glm::vec2(static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getXResolution()), static_cast<float>(sgct::Engine::instance()->getWindowPtr(winID)->getYResolution()));
	}
}

void OrbitalMouseController::scrollWheel(int pos) {

}

void OrbitalMouseController::update(const double& dt){
	
	//if (_leftMouseButtonDown || _rightMouseButtonDown || _middleMouseButtonDown){
		_handler->orbit(
			static_cast<float>(_leftMouseButtonDown) * static_cast<float>(dt)  *  _currentCursorDiff[MouseButtons::ButtonLeft].x * _rotationSpeed, 
			static_cast<float>(_leftMouseButtonDown) * static_cast<float>(dt)  *  _currentCursorDiff[MouseButtons::ButtonLeft].y * _rotationSpeed,
			static_cast<float>(_middleMouseButtonDown) * static_cast<float>(dt) * _currentCursorDiff[MouseButtons::ButtonMiddle].x * _rotationSpeed,
			static_cast<float>(_rightMouseButtonDown) * static_cast<float>(dt)  * _currentCursorDiff[MouseButtons::ButtonRight].y * _navigationSpeed);
	//}
	
//	if (_leftMouseButtonDown){
//		_handler->orbit(static_cast<float>(dt)* _currentCursorDiff[MouseButtons::ButtonLeft].x * _rotationSpeed, static_cast<float>(dt)* _currentCursorDiff[MouseButtons::ButtonLeft].y * _rotationSpeed, 0.f);
//	}	
//	if (_middleMouseButtonDown){
//		_handler->orbit(0.f, 0.f, static_cast<float>(dt)* _currentCursorDiff[MouseButtons::ButtonMiddle].x * _rotationSpeed);
//	}
//	if (_rightMouseButtonDown){
//		_handler->distance(static_cast<float>(dt)* _currentCursorDiff[MouseButtons::ButtonRight].y * _navigationSpeed);
//	}
}

} // namespace interaction
} // namespace openspace
