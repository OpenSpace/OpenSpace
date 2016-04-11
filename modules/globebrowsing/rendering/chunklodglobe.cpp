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

#include <modules/globebrowsing/rendering/chunklodglobe.h>

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
	const std::string _loggerCat = "ChunkLodGlobe";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {

	const LatLonPatch ChunkLodGlobe::LEFT_HEMISPHERE = LatLonPatch(0, -M_PI/2, M_PI/2, M_PI/2);
	const LatLonPatch ChunkLodGlobe::RIGHT_HEMISPHERE = LatLonPatch(0, M_PI/2, M_PI/2, M_PI/2);


	ChunkLodGlobe::ChunkLodGlobe(const ghoul::Dictionary& dictionary)
		: _leftRoot(new ChunkNode(*this, LEFT_HEMISPHERE))
		, _rightRoot(new ChunkNode(*this, RIGHT_HEMISPHERE))
		, globeRadius(6.3e6)
		, minSplitDepth(1)
		, maxSplitDepth(7)
		, _rotation("rotation", "Rotation", 0, 0, 360)
	{
		std::string name;
		bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
		//ghoul_assert(success, "ChunkLodGlobe need the '" << SceneGraphNode::KeyName << "' be specified");
		setName(name);

		dictionary.getValue(keyFrame, _frame);
		dictionary.getValue(keyBody, _target);
		if (_target != "")
			setBody(_target);

		// Mainly for debugging purposes @AA
		addProperty(_rotation);

		// ---------
		// init Renderer
		auto geometry = std::shared_ptr<Geometry>(new GridGeometry(10, 10,
			Geometry::Positions::No,
			Geometry::TextureCoordinates::Yes,
			Geometry::Normals::No));

		auto patchRenderer = new LatLonPatchRenderer(geometry);
		_patchRenderer.reset(patchRenderer);

	}

	ChunkLodGlobe::~ChunkLodGlobe() {

	}

	bool ChunkLodGlobe::initialize() {
		_leftRoot->initialize();
		_rightRoot->initialize();
		return isReady();
	}

	bool ChunkLodGlobe::deinitialize() {
		_leftRoot->deinitialize();
		_rightRoot->deinitialize();
		return true;
	}

	bool ChunkLodGlobe::isReady() const {
		bool ready = true;
		ready &= _leftRoot->isReady();
		ready &= _rightRoot->isReady();
		return ready;
	}

	PatchRenderer& ChunkLodGlobe::getPatchRenderer() {
		return *_patchRenderer;
	}

	void ChunkLodGlobe::render(const RenderData& data){
		minDistToCamera = INFINITY;
		_leftRoot->render(data);
		_rightRoot->render(data);

		//LDEBUG("min distnace to camera: " << minDistToCamera);

		Vec3 cameraPos = data.camera.position().dvec3();
		//LDEBUG("cam pos  x: " << cameraPos.x << "  y: " << cameraPos.y << "  z: " << cameraPos.z);

		//LDEBUG("ChunkNode count: " << ChunkNode::instanceCount);
	}

	void ChunkLodGlobe::update(const UpdateData& data) {
		// set spice-orientation in accordance to timestamp
		_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
		_time = data.time;
	}



}  // namespace openspace
