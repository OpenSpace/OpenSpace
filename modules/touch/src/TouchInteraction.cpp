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
#include <modules/onscreengui/onscreenguimodule.h>

#include <openspace/interaction/interactionmode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif

#include <cmath>
#include <functional>
#include <fstream>
#include <glm/ext.hpp>

namespace {
    const std::string _loggerCat = "TouchInteraction";
}

using namespace TUIO;
namespace openspace {

TouchInteraction::TouchInteraction()
	: properties::PropertyOwner("TouchInteraction"),
	_origin("origin", "Origin", ""),
	_unitTest("Click to take a unit test", "Take a unit test saving the LM data into file", false),
	_onlyPan("Toggle Panning Mode", "Toggle pan interaction on direct-manipulation three finger case (FOR FEEDBACK)", true), // temp
	_touchActive("TouchEvents", "True if we have a touch event", false, properties::Property::Visibility::Hidden),
	_reset("Default Values", "Reset all properties to default", false),
	_maxTapTime("Max Tap Time", "Max tap delay (in ms) for double tap", 300, 10, 1000),
	_deceleratesPerSecond("Decelerates per second", "Deceleration rate of velocity, times per second", 240, 60, 300),
	_touchScreenSize("TouchScreenSize", "Touch Screen size in inches", 55.0f, 5.5f, 150.0f),
	_tapZoomFactor("Tap zoom factor","Scaling distance travelled on tap", 0.1, 0.0, 0.5),
	_nodeRadiusThreshold("Activate direct-manipulation", "Radius a planet has to have to activate direct-manipulation", 0.2f, 0.0f, 1.0f),
	_rollAngleThreshold("Interpret roll", "Threshold for min angle for roll interpret", 0.019f, 0.0f, 0.05f),
	_orbitSpeedThreshold("Activate orbit spinning", "Threshold to activate orbit spinning in direct-manipulation", 0.038f, 0.0f, 0.1f),
	_panSpeedThreshold("Activate pan spinning", "Threshold to activate pan spinning in direct-manipulation", 0.004f, 0.0, 0.01),
	_spinSensitivity("Sensitivity of Spinning", "Sensitivity of spinning in direct-manipulation", 0.25f, 0, 1),
	_inputStillThreshold("Input still", "Threshold for interpreting input as still", 0.0005f, 0, 0.001),
	_interpretPan("Pan delta distance", "Delta distance between fingers allowed for interpreting pan interaction", 0.01f, 0, 0.1),
	_slerpTime("Time to slerp", "Time to slerp in seconds to new orientation with new node picking", 1, 0, 5),
	_guiButton("GUI Button", "GUI button size in pixels.", glm::ivec2(32, 64), glm::ivec2(8, 16), glm::ivec2(128, 256)),
	_friction("Friction", "Friction for different interactions (orbit, zoom, roll, pan)", glm::vec4(0.01, 0.02, 0.02, 0.02), glm::vec4(0.0), glm::vec4(0.2)), 
	
	_vel{ glm::dvec2(0.0), 0.0, 0.0, glm::dvec2(0.0) },
	_sensitivity{glm::dvec2(0.0808181818181818, 0.0454545454545455), 4.0, 2.75, glm::dvec2(0.0808181818181818, 0.0454545454545455) },
	_centroid{ glm::dvec3(0.0) },
	_projectionScaleFactor{ 1.000004 }, // calculated with two vectors with known diff in length, then projDiffLength/diffLength.
	_currentRadius{ 1.0 }, _slerpdT{ 1000 }, _numOfTests{ 0 }, _timeSlack { 0.0 },
	_directTouchMode{ false }, _tap{ false }, _doubleTap{ false }, _lmSuccess{ true }, _guiON{ false }
{
	addProperty(_touchActive); // how do i hide this?

	addProperty(_unitTest);
	addProperty(_onlyPan); // temp
	addProperty(_reset);
	addProperty(_maxTapTime);
	addProperty(_deceleratesPerSecond);
	addProperty(_touchScreenSize);
	addProperty(_tapZoomFactor);
	addProperty(_nodeRadiusThreshold);
	addProperty(_rollAngleThreshold);
	addProperty(_orbitSpeedThreshold);
	addProperty(_panSpeedThreshold);
	addProperty(_spinSensitivity);
	addProperty(_inputStillThreshold);
	addProperty(_interpretPan);
	addProperty(_slerpTime);
	addProperty(_guiButton);
	addProperty(_friction);
	
	_origin.onChange([this]() {
		SceneGraphNode* node = sceneGraphNode(_origin.value());
		if (!node) {
			LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
			return;
		}
		setFocusNode(node);
	});

	levmarq_init(&_lmstat);
	OnScreenGUIModule::touchInput = { false, glm::vec2(0), 0 };
	_time.initSession();
}

// Called each frame if there is any input
void TouchInteraction::updateStateFromInput(const std::vector<TuioCursor>& list, std::vector<Point>& lastProcessed) {
	//ghoul_precondition(!list.empty(), "List must not be empty");
	if (_tap) { // check for doubletap
		if (_time.getSessionTime().getTotalMilliseconds() < _maxTapTime) {
			_doubleTap = true;
			_tap = false;
		}
		_time.initSession();
	}
	
	if (!guiMode(list)) {
		if (_directTouchMode && _selected.size() > 0 && list.size() == _selected.size()) {
			directControl(list);
		}
		if (_lmSuccess) {
			findSelectedNode(list);
		}
		if (!_directTouchMode) {
			computeVelocities(list, lastProcessed);
		}

		// evaluates if current frame is in directTouchMode (will if so be used next frame)
		if (_currentRadius > _nodeRadiusThreshold && _selected.size() == list.size()) { // needs better definition?
			//_vel.pan = glm::dvec2(0.0, 0.0);
			_directTouchMode = true;
		}
		else {
			_directTouchMode = false;
		}
	}
}

bool TouchInteraction::guiMode(const std::vector<TuioCursor>& list) {
	WindowWrapper& wrapper = OsEng.windowWrapper();
	glm::ivec2 res = wrapper.currentWindowSize();
	glm::dvec2 pos = glm::vec2(list.at(0).getScreenX(res.x), list.at(0).getScreenY(res.y)); // mouse pixel position
	_guiON = OnScreenGUIModule::gui.isEnabled();
	if (_tap && list.size() == 1 && std::abs(pos.x) < _guiButton.value().x && std::abs(pos.y) < _guiButton.value().y) { // pressed invisible button
		_guiON = !_guiON;
		OnScreenGUIModule::gui.setEnabled(_guiON);

		std::string mode = (_guiON) ? "" : "de";
		LINFO("GUI mode is " << mode << "activated. Inside box by: (" <<
			static_cast<int>(100 * (pos.x / _guiButton.value().x)) << "%, " << static_cast<int>(100 * (pos.y / _guiButton.value().y)) << "%)\n");
	}
	else if (_guiON) {
		OnScreenGUIModule::touchInput = { _guiON, pos, 1 };
	}
	return _guiON; // return if consumed
}

// Sets _vel to update _camera according to direct-manipulation (L2 error)
void TouchInteraction::directControl(const std::vector<TuioCursor>& list) {
	// Reset old velocities upon new interaction
	_vel.orbit = glm::dvec2(0.0, 0.0);
	_vel.zoom = 0.0;
	_vel.roll = 0.0;
	_vel.pan = glm::dvec2(0.0, 0.0);

	// Returns the screen point s(xi,par) dependant the transform M(par) and object point xi
	auto distToMinimize = [](double* par, int x, void* fdata, LMstat* lmstat) {
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
		lmstat->pos.push_back(newScreenPoint);
		return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
	};
	// Gradient of distToMinimize w.r.t par (using forward difference)
	auto gradient = [](double* g, double* par, int x, void* fdata, LMstat* lmstat) {
		FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
		glm::dvec3 camPos = ptr->camera->positionVec3();
		glm::dvec3 selectedPoint = (ptr->node->rotationMatrix() * ptr->selectedPoints.at(x)) + ptr->node->worldPosition();
		double h = 1e-11 * glm::distance(camPos, selectedPoint), hZoom = 1e-4;
		double der, f1, f0 = ptr->distToMinimize(par, x, fdata, lmstat);
		double planetScale = ptr->node->boundingSphere() / 1e7;
		if (planetScale > 1.0) {
			hZoom *= planetScale;
			h *= (planetScale / std::pow(10, fmod(planetScale,10.0) / 2.0));
		}

		for (int i = 0; i < ptr->nDOF; ++i) {
			h = (i == 2) ? hZoom : h; // the 'zoom'-DOF is so big a smaller step creates NAN
			par[i] += h;
			f1 = ptr->distToMinimize(par, x, fdata, lmstat);
			par[i] -= h;
			der = (f1 - f0) / h;
			
			g[i] = der;
		}
		if (ptr->nDOF == 2) {
			for (int i = 0; i < 2; ++i) {
				g[i] = g[i]/std::abs(g[i]);
			}
		}
		else if (ptr->nDOF == 6) {
			for (int i = 0; i < ptr->nDOF; ++i) { // 3 finger case
				if (ptr->onlyPan) { // temp for feedback
					g[i] = g[i] / std::abs(g[i]); // no zoom, weird roll sometimes, otherwise only pan
				}
				else {
					// do nothing - fits fingers well, but is difficult to control
					//g[i] = (i < 4) ? g[i] : g[i] / std::abs(g[i]); // like it used to be, mute roll or not?
				}	
			}
		}
	};

	SceneGraphNode* node = _selected.at(0).node;
	auto castToNDC = [](glm::dvec3 vec, Camera& camera, SceneGraphNode* node, double aspectRatio) {
		glm::dvec3 backToScreenSpace = glm::inverse(camera.rotationQuaternion())
			* glm::normalize(((node->rotationMatrix() * vec) + (node->worldPosition() - camera.positionVec3()) ));
		backToScreenSpace *= (-3.2596558 / backToScreenSpace.z);
		backToScreenSpace.x /= aspectRatio;

		return glm::dvec2(backToScreenSpace);
	};

	const int nFingers = list.size();
	int nDOF = std::min(nFingers * 2, 6);
    std::vector<double> par(nDOF, 0.0);
	std::vector<glm::dvec3> selectedPoints;
	std::vector<glm::dvec2> screenPoints;
	for (const SelectedBody& sb : _selected) {
		selectedPoints.push_back(sb.coordinates);

		std::vector<TuioCursor>::const_iterator c = find_if(list.begin(), list.end(), [&sb](const TuioCursor& c) { return c.getSessionID() == sb.id; });
		screenPoints.push_back(glm::dvec2(2 * (c->getX() - 0.5), -2 * (c->getY() - 0.5))); // normalized -1 to 1 coordinates on screen
	}
	//glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
	FunctionData fData = { selectedPoints, screenPoints, nDOF, castToNDC, distToMinimize, _camera, node, 1.88, _lmstat, _onlyPan };
	void* dataPtr = reinterpret_cast<void*>(&fData);

	_lmSuccess = levmarq(nDOF, par.data(), nFingers, NULL, distToMinimize, gradient, dataPtr, &_lmstat); // finds best transform values and stores them in par

	if (_lmSuccess && !_unitTest) { // if good values were found set new camera state
		_vel.orbit = glm::dvec2(par.at(0), par.at(1));
		if (nDOF > 2) {
			_vel.zoom = par.at(2);
			_vel.roll = par.at(3);
			if (nDOF > 4) {
				if (_onlyPan) {
					_vel.roll = 0.0;
				}
				_vel.pan = glm::dvec2(par.at(4), par.at(5));
			}
		}
		step(1.0);
		_lastVel = _vel;
		_vel.orbit = glm::dvec2(0.0, 0.0);
		_vel.zoom = 0.0;
		_vel.roll = 0.0;
		_vel.pan = glm::dvec2(0.0, 0.0);
	}

	// debugging
	if (_lmstat.verbose) {
		/*std::cout << _lmstat.data;
		std::ostringstream os;
		for (int i = 0; i < nDOF; ++i) {
			os << par[i] << ", ";
		}
		std::cout << "Levmarq success after " << _lmstat.final_it << " iterations. Values: " << os.str() << "\n";*/
	}
}

// Traces the touch input into the scene and finds the surface coordinates of touched planets (if occuring)
void TouchInteraction::findSelectedNode(const std::vector<TuioCursor>& list) {
	
	//trim list to only contain visible nodes that make sense
	std::string selectables[30] = { "Sun", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto",
		"Moon", "Titan", "Rhea", "Mimas", "Iapetus", "Enceladus", "Dione", "Io", "Ganymede", "Europa",
		"Callisto", "NewHorizons", "Styx", "Nix", "Kerberos", "Hydra", "Charon", "Tethys", "OsirisRex", "Bennu" };
	std::vector<SceneGraphNode*> selectableNodes;
	for (SceneGraphNode* node : OsEng.renderEngine().scene()->allSceneGraphNodes())
		for (std::string name : selectables)
			if (node->name() == name)
				selectableNodes.push_back(node);
				
	//glm::dvec2 res = OsEng.windowWrapper().currentWindowResolution();
    // @COMMENT  ^_^
	double aspectRatio = 1.88; //res.x/res.y;
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
	_selected = newSelected;

	//debugging
	for (auto it : newSelected) {
		//std::cout << it.node->name() << " hit with cursor " << it.id << ". Surface Coordinates: " << glm::to_string(it.coordinates) << 
			//", Node at:" << glm::to_string(it.node->worldPosition()) << ", Camera at: " << glm::to_string(_camera->positionVec3()) << "\n";
	}
}

// Interprets the input gesture to a specific interaction
int TouchInteraction::interpretInteraction(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
	//ghoul_precondition(!list.empty(), "List must not be empty");
	_centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](double x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
	_centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](double y, const TuioCursor& c) { return y + c.getY(); }) / list.size();

	// see if the distance between fingers changed
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
	// find the slowest moving finger
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
	// find if all fingers angles are high
	double rollOn = std::accumulate(list.begin(), list.end(), 0.0, [&](double diff, const TuioCursor& c) {
		TuioPoint point = find_if(lastProcessed.begin(), lastProcessed.end(), [&c](const Point& p) { return p.first == c.getSessionID(); })->second;
		double res = 0.0;
		double lastAngle = point.getAngle(_centroid.x, _centroid.y);
		double currentAngle = c.getAngle(_centroid.x, _centroid.y);
		if (lastAngle > currentAngle + 1.5 * M_PI)
			res = currentAngle + (2 * M_PI - lastAngle);
		else if (currentAngle > lastAngle + 1.5 * M_PI)
			res = (2 * M_PI - currentAngle) + lastAngle;
		else
			res = currentAngle - lastAngle;
		if (std::abs(res) < _rollAngleThreshold)
			return 1000.0;
		else
			return (diff + res);
	});

	if (_doubleTap) {
		return PICK;
	}
	else  if (list.size() == 1) {
		return ROT;
	}
	else {
		if (std::abs(dist - lastDist) / list.at(0).getMotionSpeed() < _interpretPan && list.size() == 3) { // if distance between fingers is constant we have panning
			return PAN;
		}
		else if (std::abs(minDiff) < _inputStillThreshold || std::abs(rollOn) < 100.0) { // if one finger is 'still' (epsilon) and another moving, we have roll
			return ROLL; // also interpret if angles are high
		}
		else {
			return PINCH;
		}
	}
}

// Calculate how much interpreted interaction should change the camera state (based on _vel)
void TouchInteraction::computeVelocities(const std::vector<TuioCursor>& list, const std::vector<Point>& lastProcessed) {
	TuioCursor cursor = list.at(0);
	int action = interpretInteraction(list, lastProcessed);


	switch (action) {
		case ROT: { // add rotation velocity
			_vel.orbit += glm::dvec2(cursor.getXSpeed() * _sensitivity.orbit.x, cursor.getYSpeed() * _sensitivity.orbit.y);
			break;
		}
		case PINCH: { // add zooming velocity
			double distance = std::accumulate(list.begin(), list.end(), 0.0, [&](double d, const TuioCursor& c) {
				return d + c.getDistance(_centroid.x, _centroid.y);
			}) / list.size();
			double lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&](float d, const Point& p) {
				return d + p.second.getDistance(_centroid.x, _centroid.y);
			}) / lastProcessed.size();

			double zoomFactor = (distance - lastDistance) * (glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()) - _focusNode->boundingSphere());
			_vel.zoom += zoomFactor * _sensitivity.zoom * std::max(_touchScreenSize.value() * 0.1, 1.0);
			break;
		}
		case ROLL: { // add global roll rotation velocity
			double rollFactor = std::accumulate(list.begin(), list.end(), 0.0, [&](double diff, const TuioCursor& c) {
				TuioPoint point = find_if(lastProcessed.begin(), lastProcessed.end(), [&c](const Point& p) { return p.first == c.getSessionID(); })->second;
				double res = diff;
				double lastAngle = point.getAngle(_centroid.x, _centroid.y);
				double currentAngle = c.getAngle(_centroid.x, _centroid.y);
				if (lastAngle > currentAngle + 1.5 * M_PI)
					res += currentAngle + (2 * M_PI - lastAngle);
				else if (currentAngle > lastAngle + 1.5 * M_PI)
					res += (2 * M_PI - currentAngle) + lastAngle;
				else
					res += currentAngle - lastAngle;
				return res;
			}) / list.size();

			_vel.roll += -rollFactor * _sensitivity.roll;
			break;
		}
		case PAN: { // add local rotation velocity
			_vel.pan += glm::dvec2(cursor.getXSpeed() * _sensitivity.pan.x, cursor.getYSpeed() * _sensitivity.pan.y);
			break;
		}
		case PICK: { // pick something in the scene as focus node
			if (_selected.size() == 1 && _selected.at(0).node) {
				setFocusNode(_selected.at(0).node);
				OsEng.interactionHandler().setFocusNode(_focusNode); // cant do setFocusNode since TouchInteraction is not subclass of InteractionMode

				// rotate camera to look at new focus
				glm::dvec3 camToFocus = _focusNode->worldPosition() - _camera->positionVec3();
				glm::dvec3 camForward = glm::normalize(_camera->viewDirectionWorldSpace());
				double angle = glm::angle(camForward, camToFocus);
				glm::dvec3 axis = glm::normalize(glm::cross(camForward, camToFocus));
				_toSlerp.x = axis.x * sin(angle / 2.0); 
				_toSlerp.y = axis.y * sin(angle / 2.0);
				_toSlerp.z = axis.z * sin(angle / 2.0);
				_toSlerp.w = cos(angle / 2.0);
				_slerpdT = 0.0;
			}
			else { // should zoom in to current but not too much
				double dist = glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()) - _focusNode->boundingSphere();
				_vel.zoom = (_sensitivity.zoom * std::max(_touchScreenSize.value() * 0.1, 1.0)) * _tapZoomFactor * dist; // this should not be based on dt
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
			dquat camRollRot = angleAxis(_vel.roll*dt, dvec3(0.0, 0.0, 1.0));
			localCamRot = localCamRot * camRollRot;
		}
		{ // Panning (local rotation)
			dvec3 eulerAngles(_vel.pan.y*dt, _vel.pan.x*dt, 0);
			dquat rotationDiff = dquat(eulerAngles);
			localCamRot = localCamRot * rotationDiff;

			// if we have chosen a new focus node
			if (_slerpdT < _slerpTime) {
				_slerpdT += dt;
				localCamRot = slerp(localCamRot, _toSlerp, _slerpdT / _slerpTime);
			}
		}
		{ // Orbit (global rotation)
			dvec3 eulerAngles(_vel.orbit.y*dt, _vel.orbit.x*dt, 0);
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

		decelerate(dt);
		// Update the camera state
		_camera->setPositionVec3(camPos);
		_camera->setRotation(globalCamRot * localCamRot);

		_tap = false;
		_doubleTap = false;
		if (_reset) {
			resetToDefault();
		}
	}
}

void TouchInteraction::unitTest() {
	if (_unitTest) {
		_lmstat.verbose = true;
		// time set and paused in .scene file
		//openspace.time.setTime("2016 SEP 8 23:00:00.500")
		//openspace.time.togglePause()

		// set camera pos and rot
		//_camera->setPositionVec3(glm::dvec3(26974419543.178154, 76302892465.068359, -127116625827.843369));
		//_camera->setRotation(glm::dquat(0.791502, -0.576456, -0.001228, -0.203029));

		// set _selected pos and new pos (on screen)
		std::vector<TuioCursor> lastFrame = {
			{ TuioCursor(0, 10, 0.45, 0.4) }, // session id, cursor id, x, y
			{ TuioCursor(1, 11, 0.55, 0.6) }
		};
		std::vector<TuioCursor> currFrame = {
			{ TuioCursor(0, 10, 0.2, 0.6) }, // (-0.6,-0.2)
			{ TuioCursor(1, 11, 0.8, 0.4) } // (0.6, 0.2)
		};

		// call update
		findSelectedNode(lastFrame);
		directControl(currFrame);

		// save lmstats.data into a file and clear it
		char buffer[32];
		snprintf(buffer, sizeof(char) * 32, "lmdata%i.csv", _numOfTests);
		_numOfTests++;
		std::ofstream file(buffer);
		file << _lmstat.data;

		// clear everything
		_selected.clear();
		_vel.orbit = glm::dvec2(0.0, 0.0);
		_vel.zoom = 0.0;
		_vel.roll = 0.0;
		_vel.pan = glm::dvec2(0.0, 0.0);
		_lastVel = _vel;
		_unitTest = false;

		// could be the camera copy in func
	}
}

// Decelerate velocities (set 0 for directTouch)
void TouchInteraction::decelerate(double dt) {
	double frequency = 1.0 / _deceleratesPerSecond;
	int times = (dt + _timeSlack) / frequency;
	_timeSlack = fmod((dt + _timeSlack), frequency);

	if (!_directTouchMode && _currentRadius > _nodeRadiusThreshold && _vel.zoom > _focusNode->boundingSphere()) {
		_vel.zoom *= std::pow(1 - 2 * _friction.value().y, times);
	}
	_vel.orbit *= std::pow(1 - _friction.value().x, times);
	_vel.zoom *= std::pow(1 - _friction.value().y, times);
	_vel.roll *= std::pow(1 - _friction.value().z, times);
	_vel.pan *= std::pow(1 - _friction.value().w, times);
}

// Called if all fingers are off the screen
void TouchInteraction::resetAfterInput() {
	//ghoul_postcondition(_selected.empty(), "Selected list must be empty after reset");
	_lmSuccess = true;
	_guiON = OnScreenGUIModule::gui.isEnabled();
	//_directTouchMode = false;
	if (_directTouchMode && _selected.size() > 0) {
		double spinDelta = _spinSensitivity / OsEng.windowWrapper().averageDeltaTime();
		if (glm::length(_lastVel.pan) > _panSpeedThreshold) { // might not be desired
			_vel.pan = _lastVel.pan * spinDelta;
		}
		else if (glm::length(_lastVel.orbit) > _orbitSpeedThreshold) { // good value to activate "spinning"
			_vel.orbit = _lastVel.orbit * spinDelta;
		}
	}
	if (_guiON) {
		bool activeLastFrame = false;
		if (OnScreenGUIModule::touchInput.action) {
			activeLastFrame = true;
		}
		OnScreenGUIModule::touchInput.active = false;
		if (activeLastFrame) {
			OnScreenGUIModule::touchInput.active = true;
			OnScreenGUIModule::touchInput.action = 0;
		}
	}
	_selected.clear(); // should clear if no longer have a direct-touch input
}

void TouchInteraction::resetToDefault() {
	_unitTest.set(false);
	_onlyPan.set(true); // temp
	_reset.set(false);
	_maxTapTime.set(300);
	_deceleratesPerSecond.set(240);
	_touchScreenSize.set(55.0f);
	_tapZoomFactor.set(0.1f);
	_nodeRadiusThreshold.set(0.2f);
	_rollAngleThreshold.set(0.019f);
	_orbitSpeedThreshold.set(0.038f);
	_panSpeedThreshold.set(0.004f);
	_spinSensitivity.set(0.25f);
	_inputStillThreshold.set(0.0005f);
	_interpretPan.set(0.01f);
	_slerpTime.set(1.0f);
	_guiButton.set(glm::ivec2(32, 64));
	_friction.set(glm::vec4(0.01, 0.02, 0.02, 0.02));
}

void TouchInteraction::tap() {
	_tap = true;
}
void TouchInteraction::touchActive(bool active) {
	_touchActive = active;
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

} // openspace namespace