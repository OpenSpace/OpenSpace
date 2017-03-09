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
	: _sensitivity{ 1.0 }, _friction{ 0.98 }, _focusNode{ OsEng.interactionHandler().focusNode() },
	_camera{ OsEng.interactionHandler().camera() },
	_globalCameraRotation{ glm::dquat(0.0, 0.0, 0.0, 0.0) }, _localCameraRotation{ glm::dquat(0.0, 0.0, 0.0, 0.0) },
	_cameraPosition{ OsEng.interactionHandler().camera()->positionVec3() }, // initialise local/global rotations? 
	_velocityPos{ glm::dvec3(0.0) }, _velocityRot{ glm::dvec3(0.0) }, _centroid{ glm::dvec3(0.0) },
	_previousFocusNodePosition{ glm::dvec3(0.0) }
	{}

TouchInteraction::~TouchInteraction() { }

void TouchInteraction::update(const std::vector<TuioCursor>& list, std::vector<Point>& lastProcessed) {
	using namespace glm;

	// Create variables from current state
	_focusNode = OsEng.interactionHandler().focusNode();
	_cameraPosition = _camera->positionVec3();
	dvec3 centerPos = _focusNode->worldPosition();
	if (length(_previousFocusNodePosition) == 0) // ugly check to not make startup freak out
		_previousFocusNodePosition = centerPos;

	TuioCursor cursor = list.at(0);

	// Follow the focus node
	dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
	_previousFocusNodePosition = centerPos;
	_cameraPosition += focusNodeDiff;

	dquat totalRotation = _camera->rotationQuaternion();
	dvec3 directionToCenter = normalize(centerPos - _cameraPosition);
	dvec3 centerToCamera = _cameraPosition - centerPos;
	dvec3 lookUp = _camera->lookUpVectorWorldSpace();
	dvec3 camDirection = _camera->viewDirectionWorldSpace();
	
	// Create the internal representation of the local and global camera rotations
	dmat4 lookAtMat = lookAt(
		dvec3(0, 0, 0),
		directionToCenter,
		normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
	_globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
	_localCameraRotation = inverse(_globalCameraRotation) * totalRotation;
	
	_interactionMode = interpret(list);
	switch (_interactionMode) {
	case ROT: { // add rotation velocity
		// Do global rotation
		dvec2 rotationVelocity = dvec2(cursor.getXSpeed()*0.1, cursor.getYSpeed()*0.1);
		dvec3 eulerAngles(rotationVelocity.y, rotationVelocity.x, 0);
		dquat rotationDiffCamSpace = dquat(eulerAngles);

		dquat newRotationCamspace = _globalCameraRotation * rotationDiffCamSpace;
		dquat rotationDiffWorldSpace = newRotationCamspace * inverse(_globalCameraRotation);
		_rotationDiff = centerToCamera * rotationDiffWorldSpace - centerToCamera; // _velocityPos += rotationDiffVec3


		_cameraPosition += _rotationDiff;
		centerToCamera = _cameraPosition - centerPos;
		directionToCenter = normalize(-centerToCamera);

		dvec3 lookUpWhenFacingCenter = _globalCameraRotation * dvec3(_camera->lookUpVectorCameraSpace());
		dmat4 lookAtMat = lookAt(
			dvec3(0, 0, 0),
			directionToCenter,
			lookUpWhenFacingCenter);
		_globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));

		_camera->setPositionVec3(_cameraPosition);
		_camera->setRotation(_globalCameraRotation * _localCameraRotation);
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
		zoomFactor *= glm::distance(_cameraPosition, _camera->focusPositionVec3());
		// gets really crazy if you set a velocity when we're far away, limit zooming to not go into globe
		_velocityPos += directionToCenter*zoomFactor;
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
	_cameraPosition = _camera->positionVec3();
	_cameraPosition += _velocityPos * dt;
	_velocityPos *= _friction;


	// Update the camera state
	_camera->setPositionVec3(_cameraPosition);
	//_camera->setRotation(_globalCameraRotation * _localCameraRotation); // need to initialize these camerarotations
	

	
}


// Getters
Camera* TouchInteraction::getCamera() {
	return _camera;
}
SceneGraphNode* TouchInteraction::getFocusNode() {
	return _focusNode;
}
double TouchInteraction::getFriction() {
	return _friction;
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
	_friction = std::max(friction, 0.0);
}
void TouchInteraction::setSensitivity(double sensitivity) {
	_sensitivity = sensitivity;
}
