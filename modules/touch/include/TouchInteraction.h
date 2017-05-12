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
#include <modules/touch/ext/levmarq.h>

#include <openspace/util/camera.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionmode.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/network/parallelconnection.h>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#endif

#include <list>

// @COMMENT  It's better to use strongly-typed enums here:
// enum class Type { Rot = 0, Pinch, Pan, Roll, Pick };
// #define's leak into other parts of the program, especially if they are defined in header files
#define ROT 0
#define PINCH 1
#define PAN 2
#define ROLL 3
#define PICK 4

namespace openspace {

// @COMMENT  These structs are defined in the openspace namespace;  it would be better to place that in either
// a subnamespace or in the Touchinteraction class
struct VelocityStates {
	glm::dvec2 orbit;
	double zoom;
	double roll;
	glm::dvec2 pan;
};
struct SelectedBody { 
	int id;
	SceneGraphNode* node;
	glm::dvec3 coordinates;
};
struct FunctionData {
	std::vector<glm::dvec3> selectedPoints;
	std::vector<glm::dvec2> screenPoints;
	int nDOF;
	glm::dvec2(*castToNDC)(glm::dvec3, Camera&, SceneGraphNode*, double);
	double(*distToMinimize)(double* par, int x, void* fdata, LMstat* lmstat);
	Camera* camera;
	SceneGraphNode* node;
	double aspectRatio;
	LMstat stats;
};

// @COMMENT  Double definition
#define ROT 0
#define PINCH 1
#define ROLL 2
#define PAN 3
#define PICK 4

// @COMMENT  This is also polluting the openspace namespace
using Point = std::pair<int, TUIO::TuioPoint>;

class TouchInteraction : public properties::PropertyOwner
{
	public:
		TouchInteraction();
        // @COMMENT  The destructor doesn't do anything, so it could be deleted
		~TouchInteraction();
		
        // @COMMENT  How many of these functions have to be public, and could be made private instead?

        void update(const std::vector<TUIO::TuioCursor>& list, std::vector<Point>& lastProcessed);
        // @COMMENT  all of the function names here are not very descriptive. Especially
        // when it comes to the return values
        bool gui(const std::vector<TUIO::TuioCursor>& list);
		void manipulate(const std::vector<TUIO::TuioCursor>& list);
		void trace(const std::vector<TUIO::TuioCursor>& list);
		int interpret(const std::vector<TUIO::TuioCursor>& list, const std::vector<Point>& lastProcessed);
		void accelerate(const std::vector<TUIO::TuioCursor>& list, const std::vector<Point>& lastProcessed);

		void step(double dt);
		void unitTest();

		void decelerate();
		void clear();
		void tap();

		// Get & Setters
		Camera* getCamera();
		SceneGraphNode* getFocusNode();
		void setFocusNode(SceneGraphNode* focusNode);
		void setCamera(Camera* cam);

	private:
		Camera* _camera;
		SceneGraphNode* _focusNode = nullptr;

		// Property variables
		properties::StringProperty _origin;
		properties::BoolProperty _lmVerbose;
		properties::BoolProperty _unitTest;
		properties::IntProperty _maxTapTime;
		properties::FloatProperty _touchScreenSize;
		properties::FloatProperty _nodeRadiusThreshold;
		properties::FloatProperty _orbitSpeedThreshold;
		properties::FloatProperty _panSpeedThreshold;
		properties::FloatProperty _spinSensitivity;
		properties::FloatProperty _inputStillThreshold;
		properties::FloatProperty _interpretPan;
		properties::FloatProperty _slerpTime;
		properties::IVec2Property _guiButton;
		properties::Vec4Property _friction;

		// Class variables
		VelocityStates _vel;
		VelocityStates _lastVel;
		VelocityStates _sensitivity;

		double _projectionScaleFactor;
		double _currentRadius;
		double _slerpdT;
		int _numOfTests;
		TUIO::TuioTime _time;
		bool _directTouchMode;
		bool _tap;
		bool _doubleTap;
		bool _lmSuccess;
		bool _guiON;
		std::vector<SelectedBody> _selected;
		LMstat _lmstat;
		glm::dquat _toSlerp;
};

} // openspace namespace

#endif // __OPENSPACE_TOUCH___INTERACTION___H__

