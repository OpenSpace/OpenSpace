/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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


#include <modules/touch/include/TouchInteraction.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif

#include <glm/ext.hpp>

namespace {
    const std::string _loggerCat = "TouchInteraction";
}

using namespace TUIO;
using namespace openspace;

TouchInteraction::TouchInteraction()
	: _focusNode{ OsEng.interactionHandler().focusNode() }, _camera{ OsEng.interactionHandler().camera() },
	_sensitivity{ 1.0 }, _baseFriction{ 0.02 },
	_vel{ 0.0, glm::dvec2(0.0), glm::dvec2(0.0), glm::dvec2(0.0), glm::dvec2(0.0) },
	_friction{ _baseFriction, 0.01, _baseFriction, _baseFriction, _baseFriction },
	_centroid{ glm::dvec3(0.0) },
	_previousFocusNodePosition{ glm::dvec3(0.0) }
	{}

TouchInteraction::~TouchInteraction() { }

void TouchInteraction::update(const std::vector<TuioCursor>& list, std::vector<Point>& lastProcessed) {
	TuioCursor cursor = list.at(0);
	
	_interactionMode = interpret(list);
	switch (_interactionMode) {
	case ROT: { // add rotation velocity
		_vel.globalRot += glm::dvec2(cursor.getXSpeed()*0.1, cursor.getYSpeed()*0.1);
		break;
	}
	case PINCH: { // add zooming velocity
		_centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](double x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
		_centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](double y, const TuioCursor& c) { return y + c.getY(); }) / list.size();

		double distance = std::accumulate(list.begin(), list.end(), 0.0, [&](double d, const TuioCursor& c) {
			return d + sqrt(pow(c.getX() - _centroid.x, 2) + pow(c.getY() - _centroid.y, 2));
		});
		double lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&](float d, const Point& p) {
			return d + sqrt(pow(p.second.getX() - _centroid.x, 2) + pow(p.second.getY() - _centroid.y, 2));
		});
		double zoomFactor = distance - lastDistance; // should be dependant on screen size, distance from focusNode
		zoomFactor *= glm::distance(_camera->positionVec3(), _camera->focusPositionVec3());
		// gets really crazy if you set a velocity when we're far away, limit zooming to not go into globe
		_vel.zoom += zoomFactor;
		break;
	}
	case PAN: { // add local rotation velocity
		break;
	}
	default:
		LINFO("Couldn't interpret input" << "\n");
	}

}


int TouchInteraction::interpret(const std::vector<TuioCursor>& list) {
	if (list.size() == 1)
		return ROT;
	else
		return PINCH;
}

void TouchInteraction::step(double dt) {
	using namespace glm;

	// Create variables from current state
	_focusNode = OsEng.interactionHandler().focusNode();
	dvec3 camPos = _camera->positionVec3();
	dvec3 centerPos = _focusNode->worldPosition();
	if (length(_previousFocusNodePosition) == 0) // ugly check to not make startup freak out
		_previousFocusNodePosition = centerPos;

	// Follow the focus node
	dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
	_previousFocusNodePosition = centerPos;
	camPos += focusNodeDiff;

	dvec3 directionToCenter = normalize(centerPos - camPos);
	dvec3 centerToCamera = camPos - centerPos;
	dvec3 lookUp = _camera->lookUpVectorWorldSpace();
	dvec3 camDirection = _camera->viewDirectionWorldSpace();

	// Make a representation of the rotation quaternion with local and global rotations
	dmat4 lookAtMat = lookAt(
		dvec3(0, 0, 0),
		directionToCenter,
		normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
	dquat globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
	dquat localCameraRotation = inverse(globalCameraRotation) * _camera->rotationQuaternion();

	
	{ // Orbit (global rotation)
		dvec2 smoothVelocity = _vel.globalRot*dt;
		dvec3 eulerAngles(smoothVelocity.y, smoothVelocity.x, 0);
		dquat rotationDiffCamSpace = dquat(eulerAngles);

		dquat newRotationCamspace = globalCameraRotation * rotationDiffCamSpace;
		dquat rotationDiffWorldSpace = newRotationCamspace * inverse(globalCameraRotation);
		dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;

		camPos += rotationDiffVec3;
		dvec3 centerToCamera = camPos - centerPos;
		directionToCenter = normalize(-centerToCamera);

		dvec3 lookUpWhenFacingCenter =
			globalCameraRotation * dvec3(_camera->lookUpVectorCameraSpace());
		dmat4 lookAtMat = lookAt(
			dvec3(0, 0, 0),
			directionToCenter,
			lookUpWhenFacingCenter);
		globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
	}
	{ // Zooming
		camPos += directionToCenter*_vel.zoom*dt;
	}

	decelerate();

	// Update the camera state
	_camera->setPositionVec3(camPos);
	_camera->setRotation(globalCameraRotation * localCameraRotation);

	
}

void TouchInteraction::decelerate() {
	_vel.zoom *= (1 - _friction.zoom);
	_vel.globalRot *= (1 - _friction.globalRot);
	_vel.localRot *= (1 - _friction.localRot);
	_vel.globalRoll *= (1 - _friction.globalRoll);
	_vel.localRoll *= (1 - _friction.localRoll);
}


// Getters
Camera* TouchInteraction::getCamera() {
	return _camera;
}
SceneGraphNode* TouchInteraction::getFocusNode() {
	return _focusNode;
}
double TouchInteraction::getFriction() {
	return _baseFriction;
}
double TouchInteraction::getSensitivity() {
	return _sensitivity;
}
// Setters
void TouchInteraction::setCamera(Camera* camera) {
	_camera = camera;
}
void TouchInteraction::setFocusNode(SceneGraphNode* focusNode) {
	_focusNode = focusNode;
}
void TouchInteraction::setFriction(double friction) {
	_baseFriction = std::max(friction, 0.0);
}
void TouchInteraction::setSensitivity(double sensitivity) {
	_sensitivity = sensitivity;
}
