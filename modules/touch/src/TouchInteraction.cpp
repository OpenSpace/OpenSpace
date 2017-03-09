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


namespace {
    const std::string _loggerCat = "TouchInteraction";
}

using namespace TUIO;
using namespace openspace;

TouchInteraction::TouchInteraction()
	: _dt{ 0.0 }, _sensitivity{ 1.0 }, _friction{ 0.8 }, _focusNode{ nullptr },
	_camera{ OsEng.interactionHandler().camera() },
	_cameraPosition{ OsEng.interactionHandler().camera()->positionVec3() }, // initialise local/global rotations? 
	_velocityPos { glm::dvec3(0.0) }, _velocityRot{ glm::dvec3(0.0) }, _centroid{ glm::dvec3(0.0) } 
	{}

TouchInteraction::~TouchInteraction() { }

void TouchInteraction::update(const std::vector<TUIO::TuioCursor>& list, std::vector<Point>& lastProcessed) {
	_cameraPosition = _camera->positionVec3();

	double distance;
	double lastDistance;
	double zoomFactor;
	glm::dvec3 focusDir;

	_interactionMode = interpret(list);
	switch (_interactionMode) {
	case ROT: 
		// add rotation velocity
		break;
	case PINCH: 
		// add zooming velocity
		focusDir = glm::normalize(_camera->focusPositionVec3() - _cameraPosition);

		_centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](double x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
		_centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](double y, const TuioCursor& c) { return y + c.getY(); }) / list.size();

		distance = std::accumulate(list.begin(), list.end(), 0.0, [&](double d, const TuioCursor& c) {
			return d + sqrt(pow(c.getX() - _centroid.x, 2) + pow(c.getY() - _centroid.y, 2));
		});
		lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&](float d, const Point& p) {
			return d + sqrt(pow(p.second.getX() - _centroid.x, 2) + pow(p.second.getY() - _centroid.y, 2));
		});
		zoomFactor = distance - lastDistance; // should be dependant on screen size, distance from focusNode
		zoomFactor *= glm::distance(_cameraPosition, _camera->focusPositionVec3());

		_velocityPos += focusDir*zoomFactor;
		break;
	case PAN:
		// add local rotation velocity
		break;
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

void TouchInteraction::performStep(double dt) {
	if (dt != _dt) {
		_cameraPosition += _velocityPos * (dt - _dt);
		_velocityPos *= _friction;
		if (glm::length(_velocityPos) < 100) // max of zero to have a shut off range
			_velocityPos = glm::dvec3(0.0);

		_dt = dt;

		// Update the camera state
		_camera->setPositionVec3(_cameraPosition);
		//_camera->setRotation(_globalCameraRotation * _localCameraRotation);
	}
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
	_friction = glm::max(friction, 0.0);
}
void TouchInteraction::setSensitivity(double sensitivity) {
	_sensitivity = sensitivity;
}
