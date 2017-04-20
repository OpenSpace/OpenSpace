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
#include <glm/ext.hpp>

namespace {
    const std::string _loggerCat = "TouchInteraction";
}

using namespace TUIO;
using namespace openspace;

TouchInteraction::TouchInteraction()
	: properties::PropertyOwner("TouchInteraction"),
	_origin("origin", "Origin", ""),
	_baseSensitivity{ 0.1 }, _baseFriction{ 0.02 },
	_vel{ 0.0, glm::dvec2(0.0), glm::dvec2(0.0), 0.0, 0.0 },
	_friction{ _baseFriction, _baseFriction/2.0, _baseFriction, _baseFriction, _baseFriction },
	_touchScreenSize("TouchScreenSize", "Touch Screen Normalizer", glm::vec2(122, 68), glm::vec2(0), glm::vec2(1000)), // glm::vec2(width, height) in cm. (13.81, 6.7) for iphone 6s plus
	_centroid{ glm::dvec3(0.0) },
	_sensitivity{ 2.0, 0.1, 0.1, 0.1, 0.4 }, 
	_projectionScaleFactor{ 1.000004 }, // calculated with two vectors with known diff in length, then projDiffLength/diffLength.
	_currentRadius{ 1.0 },
	_directTouchMode{ false }, _tap{ false }
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

void TouchInteraction::update(const std::vector<TuioCursor>& list, std::vector<Point>& lastProcessed) {
	if (_directTouchMode && _selected.size() > 0 && list.size() == _selected.size()) { // should just be a function call
		// Returns the screen point s(xi,par) dependant the transform M(par) and object point xi
		auto distToMinimize = [](double* par, int x, void* fdata) {
			FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
			glm::dvec3 selectedPoint = ptr->selectedPoints.at(x); 

			// Create transformation matrix M(q) and apply transform for newPointInModelView
			glm::dvec3 T = glm::dvec3(par[0], par[1], 0.0);
			glm::dquat Q;
			Q.x = Q.y = Q.z = Q.w = 0.0;
			if (ptr->nDOF > 2) { // need to check how many DOF we can handle (+2 DOF per finger down up to 6)
				T.z = par[2];
				Q.x = par[3];
				if (ptr->nDOF > 4) {
					Q.y = par[4];
					Q.z = par[5];
				}
			}
			Q.w = sqrt(1.0 - std::min(glm::length(Q), 0.99999)); // if check is not done Q.w becomes NAN. Do we need a better definition of Q.w?
			
			/*glm::dmat4 rotate = glm::toMat4(Q);
			glm::dmat4 translate = glm::translate(glm::dmat4(1.0f), T);
			glm::dmat4 transform = rotate * translate;*/

			glm::dvec3 pointInCamSpace = glm::inverse(ptr->camera->rotationQuaternion()) * (ptr->node->rotationMatrix() * selectedPoint);
			glm::dvec3 newWorldSP = Q * (pointInCamSpace + T); // cam space around origin
			//glm::dvec3 newWorldSP = transform * glm::dvec4(pointInCamSpace, 1.0); // cam space around origin


			glm::dvec2 newScreenPoint = ptr->castToScreen(newWorldSP, ptr->camera, ptr->node, ptr->aspectRatio);
			return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
		};
		// Gradient (finite derivative) of distToMinimize w.r.t par
		auto gradient = [](double* g, double* par, int x, void* fdata) { // should g[i] = 1.0 or the derivative -> project to screen -> .x or .y?
			FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
			double f0 = ptr->distToMinimize(par, x, fdata);
			double f1, h = 1e-4;
			double* dPar = new double[ptr->nDOF];

			for (int i = 0; i < ptr->nDOF; ++i) {
				for (int j = 0; j < ptr->nDOF; ++j) {
					dPar[j] = par[j];
				}

				dPar[i] += h;
				f1 = ptr->distToMinimize(dPar, x, fdata);
				g[i] = (f1 - f0) / h;
			}
			delete[] dPar;
		};

		SceneGraphNode* node = _selected.at(0).node;
		const int nFingers = list.size();
		int nDOF = std::min(nFingers * 2, 6);
		double* par = new double[nDOF];
		double tPar[6] = { node->worldPosition().x, node->worldPosition().y, node->worldPosition().z, 0.0, 0.0, 0.0 };
		for (int i = 0; i < nDOF; ++i) // initial values of q or 0.0? (ie current model or no rotation/translation)
			par[i] = 0.0;
		std::vector<glm::dvec3> selectedPoints;
		std::vector<glm::dvec2> screenPoints;
		for (const SelectedBody& sb : _selected) {
			selectedPoints.push_back(sb.coordinates);

			std::vector<TuioCursor>::const_iterator c = find_if(list.begin(), list.end(), [&sb](const TuioCursor& c) { return c.getSessionID() == sb.id; });
			screenPoints.push_back(glm::dvec2(c->getX(), c->getY()));
		}
		double* squaredError = new double[nFingers]; // probably not needed
		for (int i = 0; i < nFingers; ++i) {
			double err = glm::length(screenPoints.at(i) - modelToScreenSpace(selectedPoints.at(i), node));
			squaredError[i] = err;
		}
		
		auto castToScreen = [](glm::dvec3 camVec, Camera* camera, SceneGraphNode* node, double aspectRatio) {
			glm::dvec3 vecInWorldSpace = camera->rotationQuaternion() * camVec;
			glm::dvec3 rayFromCamToPoint = glm::normalize(vecInWorldSpace + node->worldPosition() - camera->positionVec3());

			glm::dvec3 pointInNDC = glm::inverse(camera->rotationQuaternion()) * rayFromCamToPoint;
			pointInNDC *= (-3.2596558 / pointInNDC.z);

			return glm::dvec2(pointInNDC.x / (2 * aspectRatio) + 0.5, -pointInNDC.y / 2 + 0.5); // returns [0,0] - [1,1] point
		};

		glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
		FunctionData fData = { selectedPoints, screenPoints, nDOF, castToScreen, distToMinimize, _camera, node, res.x / res.y };
		void* dataPtr = reinterpret_cast<void*>(&fData);

		levmarq_init(&_lmstat);
		int nIterations = levmarq(nDOF, par, nFingers, squaredError, NULL, distToMinimize, gradient, dataPtr, &_lmstat); // finds best transform values and stores them in par

		double temp[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
		for (int i = 0; i < nDOF; ++i)
			temp[i] = par[i];

		glm::dvec3 T = glm::dvec3(temp[0], temp[1], temp[2]);
		glm::dquat Q;
		Q.x = temp[3]; Q.y = temp[4]; Q.z = temp[5];
		double len = Q.x*Q.x + Q.y*Q.y + Q.z*Q.z;
		Q.w = sqrt(1.0 - len);

		/*glm::dmat4 rotate = glm::toMat4(Q);
		glm::dmat4 translate = glm::translate(glm::dmat4(1.0f), T);
		glm::dmat4 transform = rotate * translate;*/

		glm::dquat worldQ = Q;
		glm::dvec3 worldT = _camera->rotationQuaternion() * T; // glm::dvec3(transform[3][0], transform[3][1], transform[3][2]);

		_camera->rotate(worldQ);
		_camera->setPositionVec3(_camera->positionVec3() - worldT);

		// debugging
		std::cout << "Levmarq success after " << nIterations << " iterations. Camera T: " << glm::to_string(worldT) << ", Q: " << glm::to_string(Q) << "\n";

		// cleanup
		delete[] squaredError;
		delete[] par;
	}
	trace(list);
	//if (!_directTouchMode) {
		interpret(list, lastProcessed);
		//accelerate(list, lastProcessed);
	//}
	
	if (_currentRadius > 0.3 && _selected.size() == list.size()) { // good value to make any planet sufficiently large for direct-touch, needs better definition
		_directTouchMode = true;
	}
	else {
		_directTouchMode = false;
	}
}

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

void TouchInteraction::interpret(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
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
		_action.rot = false;
		_action.pinch = false;
		_action.pan = false;
		_action.roll = false;
		_action.pick = true;
	}
	else  if (list.size() == 1) {
		_action.rot = true;
		_action.pinch = false;
		_action.pan = false;
		_action.roll = false;
		_action.pick = false;
	}
	else {
		if (std::abs(dist - lastDist)/list.at(0).getMotionSpeed() < 0.01 && list.size() == 2) {
			_action.rot = false;
			_action.pinch = false;
			_action.pan = true;
			_action.roll = false;
			_action.pick = false;
		}
		else if (list.size() > 2 && std::abs(minDiff) < 0.0008) {
			_action.rot = false;
			_action.pinch = false;
			_action.pan = false;
			_action.roll = true;
			_action.pick = false;
		}
		else {
			_action.rot = false;
			_action.pinch = true;
			_action.pan = false;
			_action.roll = false;
			_action.pick = false;
		}
	}	
}

void TouchInteraction::accelerate(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
	TuioCursor cursor = list.at(0);
	if (!_action.rot || !_action.pick) {
		_centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](double x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
		_centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](double y, const TuioCursor& c) { return y + c.getY(); }) / list.size();
	}

	if (_action.rot) { // add rotation velocity
		_vel.globalRot += glm::dvec2(cursor.getXSpeed(), cursor.getYSpeed()) * _sensitivity.globalRot;
	}
	if (_action.pinch) { // add zooming velocity
		double distance = std::accumulate(list.begin(), list.end(), 0.0, [&](double d, const TuioCursor& c) {
			return d + c.getDistance(_centroid.x, _centroid.y);
		});
		double lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&](float d, const Point& p) {
			return d + p.second.getDistance(_centroid.x, _centroid.y);
		});

		double zoomFactor = (distance - lastDistance) * glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()); // make into log space instead

		_vel.zoom += zoomFactor * _sensitivity.zoom;
	}
	if (_action.pan) { // add local rotation velocity
		_vel.localRot += glm::dvec2(cursor.getXSpeed(), cursor.getYSpeed()) * _sensitivity.localRot;
	}
	if (_action.roll) { // add global roll rotation velocity
		double rollFactor = std::accumulate(list.begin(), list.end(), 0.0, [&](double diff, const TuioCursor& c) {
			TuioPoint point = find_if(lastProcessed.begin(), lastProcessed.end(), [&c](const Point& p) { return p.first == c.getSessionID(); })->second;
			double res = diff;
			double lastAngle = point.getAngle(_centroid.x, _centroid.y);
			double currentAngle = c.getAngle(_centroid.x, _centroid.y);
			if (lastAngle > currentAngle + 1.5*M_PI)
				res += currentAngle + (2 * M_PI - lastAngle);
			else if (currentAngle > lastAngle + 1.5*M_PI)
				res += (2 * M_PI - currentAngle) + lastAngle;
			else
				res += currentAngle - lastAngle;
			return res;
		});
		_vel.localRoll += -rollFactor * _sensitivity.localRoll;
	}
	if (_action.pick) { // pick something in the scene as focus node
		
		if (_selected.size() == 1 && _selected.at(0).node != _focusNode) {
			_focusNode = _selected.at(0).node; // rotate camera to look at new focus
			OsEng.interactionHandler().setFocusNode(_focusNode); // cant do setFocusNode since TouchInteraction is not subclass of InteractionMode
			glm::dvec3 camToFocus = glm::normalize(_focusNode->worldPosition() - _camera->positionVec3());
			double angleX = glm::orientedAngle(_camera->viewDirectionWorldSpace(), camToFocus, glm::normalize(_camera->rotationQuaternion() * _camera->lookUpVectorWorldSpace()));
			double angleY = glm::orientedAngle(_camera->viewDirectionWorldSpace(), camToFocus, glm::normalize(_camera->rotationQuaternion() * glm::dvec3(1,0,0)));
			//std::cout << "x: " << angleX << ", y: " << angleY << "\n";
			_vel.localRot = _sensitivity.localRot * glm::dvec2(-angleX, -angleY);
		}
		else { // should zoom in to current _selected.coordinates position
			double dist = glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()) - _focusNode->boundingSphere();
			double startDecline = _focusNode->boundingSphere() / (0.15 * _projectionScaleFactor);
			double factor = 2.0;
			if (dist < startDecline) {
				factor = 1.0 + std::pow(dist / startDecline, 2);
			}
			double response = _focusNode->boundingSphere() / (factor * _currentRadius * _projectionScaleFactor);
			_vel.zoom = _sensitivity.zoom * response;
		}
			
	}
}

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
		double minHeightAboveBoundingSphere = 1;
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
			if (length(_vel.zoom*dt) < length(centerToCamera - centerToBoundingSphere) && length(centerToCamera + directionToCenter*_vel.zoom*dt) > length(centerToBoundingSphere)) // should get boundingsphere from focusnode
				camPos += directionToCenter*_vel.zoom*dt;
			else
				_vel.zoom = 0.0;
		}
		{ // Roll around sphere normal
			dquat camRollRot = angleAxis(_vel.globalRoll*dt, -directionToCenter);
			globalCamRot = camRollRot * globalCamRot;
		}
		{ // Push up to surface
			dvec3 sphereSurfaceToCamera = camPos - (centerPos + centerToBoundingSphere);
			double distFromSphereSurfaceToCamera = length(sphereSurfaceToCamera);
			camPos += -directionToCenter * max(minHeightAboveBoundingSphere - distFromSphereSurfaceToCamera, 0.0);
		}
		
		configSensitivities(length(camPos - (centerPos + centerToBoundingSphere)));
		decelerate();

		// Update the camera state
		_camera->setPositionVec3(camPos);
		_camera->setRotation(globalCamRot * localCamRot);
	}
}


glm::dvec2 TouchInteraction::modelToScreenSpace(glm::dvec3 vec, SceneGraphNode* node) { // probably not needed, if squaredError isnt
	glm::dvec3 backToScreenSpace =  glm::inverse(_camera->rotationQuaternion())
		* glm::normalize(((node->rotationMatrix() * vec) + node->worldPosition() - _camera->positionVec3()));
	backToScreenSpace *= (-3.2596558 / backToScreenSpace.z);

	glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
	double aspectRatio = res.x / res.y;
	return glm::dvec2(backToScreenSpace.x / (2 * aspectRatio) + 0.5, -backToScreenSpace.y / 2 + 0.5);
}

void TouchInteraction::configSensitivities(double dist) {
	// Configurates sensitivities to appropriate values when the camera is close to the focus node.
	std::shared_ptr<interaction::GlobeBrowsingInteractionMode> gbim =
		std::dynamic_pointer_cast<interaction::GlobeBrowsingInteractionMode> (OsEng.interactionHandler().interactionmode());
	double close = 4.6 * 1000000;
	if (gbim && dist < close) {
		_sensitivity.zoom = 2.0 * std::max(dist, 100.0)/close;
		_sensitivity.globalRot = 0.1 * std::max(dist, 100.0) /close;
		//_sensitivity.localRot = 0.1;
		//_sensitivity.globalRoll = 0.1;
		//_sensitivity.localRoll = 0.1;
	}
	else if (dist < close) {
		_sensitivity.zoom = 2.0 * std::max(dist, 100.0) / close;
		_sensitivity.globalRot = 0.1 * std::max(dist, 100.0) / close;
	}
	else {
		_sensitivity.zoom = 2.0;
		_sensitivity.globalRot = 0.1;
		_sensitivity.localRot = 0.1;
		_sensitivity.globalRoll = 0.1;
		_sensitivity.localRoll = 0.2;
	}


}

void TouchInteraction::decelerate() {
	_vel.zoom *= (1 - _friction.zoom);
	_vel.globalRot *= (1 - _friction.globalRot);
	_vel.localRot *= (1 - _friction.localRot);
	_vel.globalRoll *= (1 - _friction.globalRoll);
	_vel.localRoll *= (1 - _friction.localRoll);
}

void TouchInteraction::clear() {
	_action.rot = false;
	_action.pinch = false;
	_action.pan = false;
	_action.roll = false;
	_action.pick = false;

	//_directTouchMode = false;
	_selected.clear(); // should clear if no longer have a direct-touch input
}

void TouchInteraction::tap() {
	_tap = true;
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
	return _baseSensitivity;
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
	_baseSensitivity = sensitivity;
}
