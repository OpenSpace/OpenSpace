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

#ifndef __OPENSPACE_TOUCH___INTERACTION___H__
#define __OPENSPACE_TOUCH___INTERACTION___H__

#include <modules/touch/include/TuioEar.h>
#include <modules/touch/touchmodule.h>

#include <openspace/util/camera.h>
#include <openspace/interaction/interactionmode.h>
#include <openspace/interaction/interactionhandler.h>

#include <openspace/network/parallelconnection.h>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#endif

#include <list>

#define ROT 0
#define PINCH 1
#define PAN 2
#define PICK 3

using Point = std::pair<int, TUIO::TuioPoint>;

class TouchInteraction
{
	public:
		TouchInteraction();
		~TouchInteraction();
		
		void update(const std::vector<TUIO::TuioCursor>& list, std::vector<Point>& lastProcessed);
		int interpret(const std::vector<TUIO::TuioCursor>& list);
		void performStep(double dt);


		// Get & Setters
		openspace::Camera* TouchInteraction::getCamera();
		openspace::SceneGraphNode* TouchInteraction::getFocusNode();
		double TouchInteraction::getFriction();
		double TouchInteraction::getSensitivity();

		void setFocusNode(openspace::SceneGraphNode* focusNode);
		void setCamera(openspace::Camera* cam);
		void setFriction(double friction);
		void setSensitivity(double sensitivity);


		#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
			// later work
		#endif

	private:
		double _dt;
		int _interactionMode;

		double _sensitivity;
		double _friction;
		
		glm::dvec3 _cameraPosition;
		glm::dquat _localCameraRotation;
		glm::dquat _globalCameraRotation;
		glm::dvec3 _centroid;

		glm::dvec3 _velocityPos;
		glm::dvec3 _velocityRot;
		
		openspace::Camera* _camera;
		openspace::SceneGraphNode* _focusNode;
		//bool globebrowsing;

		#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
			// later work
		#endif
		
};

	#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
		//globebrowsing::RenderableGlobe* _globe;
	#endif

#endif // __OPENSPACE_TOUCH___INTERACTION___H__