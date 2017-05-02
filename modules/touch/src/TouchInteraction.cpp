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

#include <openspace/interaction/interactionmode.h>
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

#include <cmath>
#include <functional>
#include <glm/ext.hpp>

namespace {
    const std::string _loggerCat = "TouchInteraction";
}

using namespace TUIO;
using namespace openspace;

TouchInteraction::TouchInteraction()
	: properties::PropertyOwner("TouchInteraction"),
	_origin("origin", "Origin", ""),
	_touchScreenSize("TouchScreenSize", "Normalizes _sensitivity with screen size given in inches.", 55.0f, 5.5f, 150.0f),
	_vel{ 0.0, glm::dvec2(0.0), glm::dvec2(0.0), 0.0, 0.0 },
	_friction{ 0.02, 0.01, 0.02, 1, 0.02 },
	_sensitivity{ 2.0 * 55.0, 0.1, 0.1, 1, 0.4 * 55.0 },
	_projectionScaleFactor{ 1.000004 }, // calculated with two vectors with known diff in length, then projDiffLength/diffLength.
	_currentRadius{ 1.0 }, _time{ 1.0 },
	_directTouchMode{ false }, _tap{ false }, _levSuccess{ true }, _guiON{ false }
{
	addProperty(_touchScreenSize);
	levmarq_init(&_lmstat);
	_origin.onChange([this]() {
		SceneGraphNode* node = sceneGraphNode(_origin.value());
		if (!node) {
			LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
			return;
		}
		setFocusNode(node);
	});
}

TouchInteraction::~TouchInteraction() { }

// Called each frame if there is any input
void TouchInteraction::update(const std::vector<TuioCursor>& list, std::vector<Point>& lastProcessed) {
	// check if tapped in corner to activate gui mode
	glm::dvec2 pos = glm::dvec2(list.at(0).getX(), list.at(0).getY());
	if (_tap && list.size() == 1 && pos.x < 0.0001 && pos.y < 0.0001 /*|| check if showGUI*/) {
		// if !showGUI press F1, 
		_guiON = !_guiON;
	}
	else if (!_guiON) {
		if (_directTouchMode && _selected.size() > 0 && list.size() == _selected.size()) {
			manipulate(list);
		}
		if (_levSuccess)
			trace(list);
		if (!_directTouchMode) {
			accelerate(list, lastProcessed);
		}

		// evaluates if current frame is in directTouchMode (will if so be used next frame)
		if (_currentRadius > 0.3 && _selected.size() == list.size()) { // good value to make any planet sufficiently large for direct-touch, needs better definition
			_directTouchMode = true;
		}
		else {
			_directTouchMode = false;
		}
	}
}

// Sets _vel to update _camera according to direct-manipulation (L2 error)
void TouchInteraction::manipulate(const std::vector<TuioCursor>& list) {
	// Returns the screen point s(xi,par) dependant the transform M(par) and object point xi
	auto distToMinimize = [](double* par, int x, void* fdata) {
		FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);

		// Apply transform to camera and find the new screen point of the updated camera state
		double q[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }; // { vec2 globalRot, zoom, roll, vec2 localRot }
		for (int i = 0; i < ptr->nDOF; ++i) {
			q[i] = par[i];
		}

		using namespace glm;
		// Create variables from current state
		dvec3 camPos = ptr->camera->positionVec3();
		dvec3 centerPos = ptr->node->worldPosition();

		dvec3 directionToCenter = normalize(centerPos - camPos);
		dvec3 centerToCamera = camPos - centerPos;
		dvec3 lookUp = ptr->camera->lookUpVectorWorldSpace();
		dvec3 camDirection = ptr->camera->viewDirectionWorldSpace();

		// Make a representation of the rotation quaternion with local and global rotations
		dmat4 lookAtMat = lookAt(
			dvec3(0, 0, 0),
			directionToCenter,
			normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
		dquat globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
		dquat localCamRot = inverse(globalCamRot) * ptr->camera->rotationQuaternion();

		{ // Roll
			dquat camRollRot = angleAxis(q[3], dvec3(0.0, 0.0, 1.0));
			localCamRot = localCamRot * camRollRot;
		}
		{ // Panning (local rotation)
			dvec3 eulerAngles(q[5], q[4], 0);
			dquat rotationDiff = dquat(eulerAngles);
			localCamRot = localCamRot * rotationDiff;
		}
		{ // Orbit (global rotation)
			dvec3 eulerAngles(q[1], q[0], 0);
			dquat rotationDiffCamSpace = dquat(eulerAngles);

			dquat rotationDiffWorldSpace = globalCamRot * rotationDiffCamSpace * inverse(globalCamRot);
			dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;
			camPos += rotationDiffVec3;

			dvec3 centerToCamera = camPos - centerPos;
			directionToCenter = normalize(-centerToCamera);
			dvec3 lookUpWhenFacingCenter = globalCamRot * dvec3(ptr->camera->lookUpVectorCameraSpace());
			dmat4 lookAtMat = lookAt(
				dvec3(0, 0, 0),
				directionToCenter,
				lookUpWhenFacingCenter);
			globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
		}
		{ // Zooming
			camPos += directionToCenter * q[2];
		}

		// Update the camera state
		Camera cam = *(ptr->camera);
		cam.setPositionVec3(camPos);
		cam.setRotation(globalCamRot * localCamRot);

		// we now have a new position and orientation of camera, project surfacePoint to the new screen to get distance to minimize
		glm::dvec2 newScreenPoint = ptr->castToNDC(ptr->selectedPoints.at(x), cam, ptr->node, ptr->aspectRatio);

		return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
	};
	// Gradient of distToMinimize w.r.t par (using forward difference)
	auto gradient = [](double* g, double* par, int x, void* fdata) {
		FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
		double f0 = ptr->distToMinimize(par, x, fdata);
		double f1, der, minStep = 1e-11;
		glm::dvec3 camPos = ptr->camera->positionVec3();
		glm::dvec3 selectedPoint = (ptr->node->rotationMatrix() * ptr->selectedPoints.at(x)) + ptr->node->worldPosition();
		double h = minStep * glm::distance(camPos, selectedPoint);
		double* dPar = new double[ptr->nDOF];
		for (int i = 0; i < ptr->nDOF; ++i) {
			dPar[i] = par[i];
		}

		for (int i = 0; i < ptr->nDOF; ++i) {
			h = (i == 2) ? 1e-4 : h; // the 'zoom'-DOF is so big a smaller step creates NAN
			dPar[i] += h;
			f1 = ptr->distToMinimize(dPar, x, fdata);
			dPar[i] -= h;
			der = (f1 - f0) / h;
			
			g[i] = (i > 1 && i < 4) ? der : der / abs(der);
		}
		delete[] dPar;
	};

	SceneGraphNode* node = _selected.at(0).node;
	auto castToNDC = [](glm::dvec3 vec, Camera& camera, SceneGraphNode* node, double aspectRatio) {
		glm::dvec3 backToScreenSpace = glm::inverse(camera.rotationQuaternion())
			* glm::normalize(((node->rotationMatrix() * vec) + node->worldPosition() - camera.positionVec3()));
		backToScreenSpace *= (-3.2596558 / backToScreenSpace.z);
		backToScreenSpace.x /= aspectRatio;

		return glm::dvec2(backToScreenSpace);
	};

	const int nFingers = list.size();
	int nDOF = std::min(nFingers * 2, 6);
	double* par = new double[nDOF];
	for (int i = 0; i < nDOF; ++i) { // initial values of q or 0.0? (ie current model or no rotation/translation)
		par[i] = 0.0;
	}
	std::vector<glm::dvec3> selectedPoints;
	std::vector<glm::dvec2> screenPoints;
	for (const SelectedBody& sb : _selected) {
		selectedPoints.push_back(sb.coordinates);

		std::vector<TuioCursor>::const_iterator c = find_if(list.begin(), list.end(), [&sb](const TuioCursor& c) { return c.getSessionID() == sb.id; });
		double xCo = 2 * (c->getX() - 0.5);
		double yCo = -2 * (c->getY() - 0.5); // normalized -1 to 1 coordinates on screen
		screenPoints.push_back(glm::dvec2(xCo, yCo));
	}
	glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
	FunctionData fData = { selectedPoints, screenPoints, nDOF, castToNDC, distToMinimize, _camera, node, res.x / res.y };
	void* dataPtr = reinterpret_cast<void*>(&fData);

	bool success = levmarq(nDOF, par, nFingers, NULL, distToMinimize, gradient, dataPtr, &_lmstat); // finds best transform values and stores them in par

	if (success) { // if good values were found set new camera state
		_vel.globalRot = glm::dvec2(par[0], par[1]);
		if (nDOF > 2) {
			_vel.zoom = par[2];
			_vel.localRoll = par[3];
			if (nDOF > 4) {
				_vel.localRot = glm::dvec2(par[4], par[5]);
			}
		}
		step(1);
	}

	// debugging
	std::ostringstream os;
	for (int i = 0; i < nDOF; ++i) {
	os << par[i] << ", ";
	}
	std::cout << "Levmarq success after " << _lmstat.final_it << " iterations. Values: " << os.str() << "\n";

	// cleanup
	delete[] par;
}

// Traces the touch input into the scene and finds the surface coordinates of touched planets (if occuring)
void TouchInteraction::trace(const std::vector<TuioCursor>& list) {
	
	//trim list to only contain visible nodes that make sense
	std::string selectables[30] = { "Sun", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto",
		"Moon", "Titan", "Rhea", "Mimas", "Iapetus", "Enceladus", "Dione", "Io", "Ganymede", "Europa",
		"Callisto", "NewHorizons", "Styx", "Nix", "Kerberos", "Hydra", "Charon", "Tethys", "OsirisRex", "Bennu" };
	std::vector<SceneGraphNode*> selectableNodes;
	for (SceneGraphNode* node : OsEng.renderEngine().scene()->allSceneGraphNodes())
		for (std::string name : selectables)
			if (node->name() == name)
				selectableNodes.push_back(node);

	glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
	double aspectRatio = res.x/res.y;
	glm::dquat camToWorldSpace = _camera->rotationQuaternion();
	glm::dvec3 camPos = _camera->positionVec3();
	std::vector<SelectedBody> newSelected;
	for (const TuioCursor& c : list) {
		double xCo = 2 * (c.getX() - 0.5) * aspectRatio;
		double yCo = -2 * (c.getY() - 0.5); // normalized -1 to 1 coordinates on screen
		glm::dvec3 cursorInWorldSpace = camToWorldSpace * glm::dvec3(xCo, yCo, -3.2596558);
		glm::dvec3 raytrace = glm::normalize(cursorInWorldSpace);
		int id = c.getSessionID();
		for (SceneGraphNode* node : selectableNodes) {
			double boundingSphere = node->boundingSphere();
			glm::dvec3 camToSelectable = node->worldPosition() - camPos;
			double dist = length(glm::cross(cursorInWorldSpace, camToSelectable)) / glm::length(cursorInWorldSpace) - boundingSphere;
			if (dist <= 0.0) {
				// finds intersection closest point between boundingsphere and line in world coordinates, assumes line direction is normalized
				double d = glm::dot(raytrace, camToSelectable);
				double root = boundingSphere * boundingSphere - glm::dot(camToSelectable, camToSelectable) + d * d;
				if (root > 0) // two intersection points (take the closest one)
					d -= sqrt(root);
				glm::dvec3 intersectionPoint = camPos + d * raytrace;
				glm::dvec3 pointInModelView = glm::inverse(node->rotationMatrix()) * (intersectionPoint - node->worldPosition());
				// spherical coordinates for point on surface, maybe not required
				double theta = atan(pointInModelView.y / pointInModelView.x);
				double phi = atan(glm::length(glm::dvec2(pointInModelView.x, pointInModelView.y)) / pointInModelView.z);

				// Add id, node and surface coordinates to the selected list
				std::vector<SelectedBody>::iterator oldNode = find_if(newSelected.begin(), newSelected.end(), [id](SelectedBody s) { return s.id == id; });
				if (oldNode != newSelected.end()) {
					double oldNodeDist = glm::length(oldNode->node->worldPosition() - camPos);
					if (glm::length(camToSelectable) < oldNodeDist) { // new node is closer, remove added node and add the new one instead
						newSelected.pop_back();
						newSelected.push_back({ id, node, pointInModelView });
					}
				}
				else {
					newSelected.push_back({ id, node, pointInModelView });
				}
			}
		}
		
	}
	//debugging
	for (auto it : newSelected) {
		//std::cout << it.node->name() << " hit with cursor " << it.id << ". Surface Coordinates: " << glm::to_string(it.coordinates) << "\n";
	}

	_selected = newSelected;
}

// Interprets the input gesture to a specific interaction
int TouchInteraction::interpret(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
	double dist = 0;
	double lastDist = 0;
	TuioCursor cursor = list.at(0);
	for (const TuioCursor& c : list) {
		dist += glm::length(glm::dvec2(c.getX(), c.getY()) - glm::dvec2(cursor.getX(), cursor.getY()));
		cursor = c;
	}
	TuioPoint point = lastProcessed.at(0).second;
	for (const Point& p : lastProcessed) {
		lastDist += glm::length(glm::dvec2(p.second.getX(), p.second.getY()) - glm::dvec2(point.getX(), point.getY()));
		point = p.second;
	}

	double minDiff = 1000;
	int id = 0;
	for (const TuioCursor& c : list) {
		TuioPoint point = find_if(lastProcessed.begin(), lastProcessed.end(), [&c](const Point& p) { return p.first == c.getSessionID(); })->second;
		double diff = c.getX() - point.getX() + c.getY() - point.getY();
		if (!c.isMoving()) {
			diff = minDiff = 0.0;
			id = c.getSessionID();
		}
		else if (std::abs(diff) < std::abs(minDiff)) {
			minDiff = diff;
			id = c.getSessionID();
		}
	}

	if (_tap) {
		_tap = false;
		return PICK;
	}
	else  if (list.size() == 1) {
		return ROT;
	}
	else {
		if (std::abs(dist - lastDist) / list.at(0).getMotionSpeed() < 0.01 && list.size() == 3) { // if distance between fingers is constant we have panning
			return PAN;
		}
		else if (list.size() > 1 && std::abs(minDiff) < 0.0008) { // if one finger is 'still' (0.0008 works as epsilon) and another moving, we have roll
			return ROLL;
		}
		else {
			return PINCH;
		}
	}
}

// Calculate how much interpreted interaction should change the camera state (based on _vel)
void TouchInteraction::accelerate(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
	TuioCursor cursor = list.at(0);
	glm::dvec3 centroid;
	int action = interpret(list, lastProcessed);

	if (action != ROT || action != PICK) {
		centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](double x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
		centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](double y, const TuioCursor& c) { return y + c.getY(); }) / list.size();
	}

	switch (action) {
		case ROT: { // add rotation velocity
			_vel.globalRot += glm::dvec2(cursor.getXSpeed(), cursor.getYSpeed()) * _sensitivity.globalRot;
			break;
		}
		case PINCH: { // add zooming velocity
			double distance = std::accumulate(list.begin(), list.end(), 0.0, [&](double d, const TuioCursor& c) {
				return d + c.getDistance(centroid.x, centroid.y);
			});
			double lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&](float d, const Point& p) {
				return d + p.second.getDistance(centroid.x, centroid.y);
			});

			double zoomFactor = (distance - lastDistance) * glm::distance(_camera->positionVec3(), _camera->focusPositionVec3());
			_vel.zoom += zoomFactor * _sensitivity.zoom / _touchScreenSize.value();
			break;
		}
		case ROLL: { // add global roll rotation velocity
			double rollFactor = std::accumulate(list.begin(), list.end(), 0.0, [&](double diff, const TuioCursor& c) {
				TuioPoint point = find_if(lastProcessed.begin(), lastProcessed.end(), [&c](const Point& p) { return p.first == c.getSessionID(); })->second;
				double res = diff;
				double lastAngle = point.getAngle(centroid.x, centroid.y);
				double currentAngle = c.getAngle(centroid.x, centroid.y);
				if (lastAngle > currentAngle + 1.5 * M_PI)
					res += currentAngle + (2 * M_PI - lastAngle);
				else if (currentAngle > lastAngle + 1.5 * M_PI)
					res += (2 * M_PI - currentAngle) + lastAngle;
				else
					res += currentAngle - lastAngle;
				return res;
			});
			_vel.localRoll += -rollFactor * _sensitivity.localRoll / _touchScreenSize.value();
			break;
		}
		case PAN: { // add local rotation velocity
			_vel.localRot += glm::dvec2(cursor.getXSpeed(), cursor.getYSpeed()) * _sensitivity.localRot;
			break;
		}
		case PICK: { // pick something in the scene as focus node
			if (_selected.size() == 1 && _selected.at(0).node != _focusNode) {
				setFocusNode(_selected.at(0).node);
				OsEng.interactionHandler().setFocusNode(_focusNode); // cant do setFocusNode since TouchInteraction is not subclass of InteractionMode
				glm::dvec3 camToFocus = _focusNode->worldPosition() - _camera->positionVec3();
				glm::dvec3 camForward = glm::normalize(_camera->viewDirectionWorldSpace());
				double angle = glm::angle(camForward, camToFocus);
				glm::dvec3 axis = glm::normalize(glm::cross(camForward, camToFocus));
				_toSlerp.x = axis.x * sin(angle / 2.0); // rotate camera to look at new focus
				_toSlerp.y = axis.y * sin(angle / 2.0);
				_toSlerp.z = axis.z * sin(angle / 2.0);
				_toSlerp.w = cos(angle / 2.0);
				_time = 0.0;
			}
			else { // should zoom in to current but not too much
				double dist = glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()) - _focusNode->boundingSphere();
				double startDecline = _focusNode->boundingSphere() / (0.15 * _projectionScaleFactor);
				double factor = 2.0;
				if (dist < startDecline) { // double-check this
					factor = 1.0 + std::pow(dist / startDecline, 2);
				}
				double response = _focusNode->boundingSphere() / (factor * _currentRadius * _projectionScaleFactor);
				_vel.zoom = (_sensitivity.zoom / 55) * response;
			}
			break;
		}
	}
}

// Main update call, calculates the new orientation and position for the camera depending on _vel and dt. Called every frame
void TouchInteraction::step(double dt) {
	using namespace glm;

	setFocusNode(OsEng.interactionHandler().focusNode()); // since functions cant be called directly (TouchInteraction not a subclass of InteractionMode)
	if (_focusNode && _camera) {
		// Create variables from current state
		dvec3 camPos = _camera->positionVec3();
		dvec3 centerPos = _focusNode->worldPosition();

		dvec3 directionToCenter = normalize(centerPos - camPos);
		dvec3 centerToCamera = camPos - centerPos;
		dvec3 lookUp = _camera->lookUpVectorWorldSpace();
		dvec3 camDirection = _camera->viewDirectionWorldSpace();

		// Make a representation of the rotation quaternion with local and global rotations
		dmat4 lookAtMat = lookAt(
			dvec3(0, 0, 0),
			directionToCenter,
			normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
		dquat globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
		dquat localCamRot = inverse(globalCamRot) * _camera->rotationQuaternion();

		double boundingSphere = _focusNode->boundingSphere();
		dvec3 centerToBoundingSphere;
		double distance = std::max(length(centerToCamera) - boundingSphere, 0.0);
		_currentRadius = boundingSphere / std::max(distance * _projectionScaleFactor, 1.0);
		
		{ // Roll
			dquat camRollRot = angleAxis(_vel.localRoll*dt, dvec3(0.0, 0.0, 1.0));
			localCamRot = localCamRot * camRollRot;
		}
		{ // Panning (local rotation)
			dvec3 eulerAngles(_vel.localRot.y*dt, _vel.localRot.x*dt, 0);
			dquat rotationDiff = dquat(eulerAngles);
			localCamRot = localCamRot * rotationDiff;

			// if we have chosen a new focus node
			if (_time < 1) {
				_time += 0.25 * dt;
				localCamRot = slerp(localCamRot, _toSlerp, _time);
			}
		}
		{ // Orbit (global rotation)
			dvec3 eulerAngles(_vel.globalRot.y*dt, _vel.globalRot.x*dt, 0);
			dquat rotationDiffCamSpace = dquat(eulerAngles);

			dquat rotationDiffWorldSpace = globalCamRot * rotationDiffCamSpace * inverse(globalCamRot);
			dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;
			camPos += rotationDiffVec3;

			dvec3 centerToCamera = camPos - centerPos;
			directionToCenter = normalize(-centerToCamera);
			dvec3 lookUpWhenFacingCenter = globalCamRot * dvec3(_camera->lookUpVectorCameraSpace());
			dmat4 lookAtMat = lookAt(
				dvec3(0, 0, 0),
				directionToCenter,
				lookUpWhenFacingCenter);
			globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
		}
		{ // Zooming
			centerToBoundingSphere = -directionToCenter * boundingSphere;
			dvec3 centerToCamera = camPos - centerPos;
			double distToSurface = length(centerToCamera - centerToBoundingSphere);

			if (length(_vel.zoom*dt) < distToSurface && length(centerToCamera + directionToCenter*_vel.zoom*dt) > length(centerToBoundingSphere)) {
				camPos += directionToCenter * _vel.zoom * dt;
			}
			else {
				_vel.zoom = 0.0;
			}
		}
		{ // Roll around sphere normal - is this something we ever want to do?
			dquat camRollRot = angleAxis(_vel.globalRoll*dt, -directionToCenter);
			globalCamRot = camRollRot * globalCamRot;
		}

		decelerate();
		// Update the camera state
		_camera->setPositionVec3(camPos);
		_camera->setRotation(globalCamRot * localCamRot);
	}
}

// Decelerate velocities (set 0 for directTouch)
void TouchInteraction::decelerate() {
	if (_directTouchMode) {
		_vel.globalRot = glm::dvec2(0.0, 0.0);
		_vel.zoom = 0.0;
		_vel.localRoll = 0.0;
		_vel.localRot = glm::dvec2(0.0, 0.0);
	}
	else {
		if (!_directTouchMode && _currentRadius > 0.3 && _vel.zoom > _focusNode->boundingSphere()) { // check for velocity speed too
			_vel.zoom *= (1 - 2*_friction.zoom);
		}
		_vel.zoom *= (1 - _friction.zoom);
		_vel.globalRot *= (1 - _friction.globalRot);
		_vel.localRot *= (1 - _friction.localRot);
		_vel.globalRoll *= (1 - _friction.globalRoll);
		_vel.localRoll *= (1 - _friction.localRoll);
	}
}

// Called if all fingers are off the screen
void TouchInteraction::clear() {
	//_directTouchMode = false;
	_levSuccess = true;
	_selected.clear(); // should clear if no longer have a direct-touch input
}

void TouchInteraction::tap() {
	_tap = true;
}

bool TouchInteraction::guiON() {
	return _guiON;
}
// Get & Setters
Camera* TouchInteraction::getCamera() {
	return _camera;
}
SceneGraphNode* TouchInteraction::getFocusNode() {
	return _focusNode;
}
void TouchInteraction::setCamera(Camera* camera) {
	_camera = camera;
}
void TouchInteraction::setFocusNode(SceneGraphNode* focusNode) {
	_focusNode = focusNode;
}

