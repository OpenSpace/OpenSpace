/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/rendering/clipmapglobe.h>

#include <modules/globebrowsing/util/converter.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "ClipMapGlobe";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {
	ClipMapGlobe::ClipMapGlobe(const ghoul::Dictionary& dictionary)
		: _patch(40, 40, 0, 0, M_PI / 4, M_PI / 4, 6.3e6)
		, _patch1(40, 40, 0, 0, M_PI / 8, M_PI / 8, 6.3e6)
		, _rotation("rotation", "Rotation", 0, 0, 360)
	{
		std::string name;
		bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
		ghoul_assert(success,
			"ClipMapGlobe need the '" << SceneGraphNode::KeyName << "' be specified");
		setName(name);

		dictionary.getValue(keyFrame, _frame);
		dictionary.getValue(keyBody, _target);
		if (_target != "")
			setBody(_target);

		// Mainly for debugging purposes @AA
		addProperty(_rotation);
	}

	ClipMapGlobe::~ClipMapGlobe() {
	}

	bool ClipMapGlobe::initialize() {
		_patch.initialize();
		_patch1.initialize();
		return isReady();
	}

	bool ClipMapGlobe::deinitialize() {
		_patch.deinitialize();
		_patch1.deinitialize();
		return true;
	}

	bool ClipMapGlobe::isReady() const {
		bool ready = true;
		ready &= _patch.isReady();
		ready &= _patch1.isReady();
		return ready;
	}

	void ClipMapGlobe::render(const RenderData& data)
	{
		// Set patch to follow camera
		int segmentsPerPatch = 10;
		glm::vec2 cameraPositionLatLon =
			converter::cartesianToLatLon(data.camera.position().dvec3());
		_patch.setPositionLatLon(
			glm::vec2((M_PI / 2.0 / segmentsPerPatch) * int(cameraPositionLatLon.x / (M_PI * 2) * segmentsPerPatch * 4),
				(M_PI / 2.0 / segmentsPerPatch) * int(cameraPositionLatLon.y / (M_PI)* segmentsPerPatch * 2)));
		_patch1.setPositionLatLon(
			glm::vec2((M_PI / 4.0 / segmentsPerPatch) * int(cameraPositionLatLon.x / (M_PI * 2) * segmentsPerPatch * 8),
				(M_PI / 4.0 / segmentsPerPatch) * int(cameraPositionLatLon.y / (M_PI)* segmentsPerPatch * 4)));		// render
		_patch.render(data);
		_patch1.render(data);
	}

	void ClipMapGlobe::update(const UpdateData& data) {
		_patch.update(data);
		_patch1.update(data);
		// set spice-orientation in accordance to timestamp
		_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
		_time = data.time;
	}

}  // namespace openspace
