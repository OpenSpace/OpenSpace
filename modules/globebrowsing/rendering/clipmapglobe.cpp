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


#define _USE_MATH_DEFINES
#include <math.h>

#include <modules/globebrowsing/rendering/clipmapglobe.h>

#include <modules/globebrowsing/rendering/clipmapgrid.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>


namespace {
	const std::string _loggerCat = "ClipMapGlobe";

	const std::string keyFrame = "Frame";
	const std::string keyGeometry = "Geometry";
	const std::string keyShading = "PerformShading";

	const std::string keyBody = "Body";
}

namespace openspace {
	ClipMapGlobe::ClipMapGlobe(const ghoul::Dictionary& dictionary)
		: _rotation("rotation", "Rotation", 0, 0, 360)
		, _clipMapPyramid(LatLon(M_PI / 2, M_PI / 2))
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

		// ---------
		// init Renderer
		auto outerPatchRenderer = new ClipMapPatchRenderer(shared_ptr<OuterClipMapGrid>(new OuterClipMapGrid(32)));
		_outerPatchRenderer.reset(outerPatchRenderer);
		auto innerPatchRenderer = new ClipMapPatchRenderer(shared_ptr<InnerClipMapGrid>(new InnerClipMapGrid(32)));
		_innerPatchRenderer.reset(innerPatchRenderer);
	}

	ClipMapGlobe::~ClipMapGlobe() {
	}

	bool ClipMapGlobe::initialize() {
		return isReady();
	}

	bool ClipMapGlobe::deinitialize() {
		return true;
	}

	bool ClipMapGlobe::isReady() const {
		bool ready = true; 
		return ready;
	}

	void ClipMapGlobe::render(const RenderData& data)
	{
		// TODO : Choose the max depth and the min depth depending on the camera
		int maxDepth = 10;
		int minDepth = 0;
		// render patches
		for (size_t i = minDepth; i < maxDepth; i++)
		{
			LatLon patchSize = _clipMapPyramid.getPatchSizeAtLevel(i);
			_outerPatchRenderer->renderPatch(patchSize, data, 6.3e6);
		}
		LatLon patchSize = _clipMapPyramid.getPatchSizeAtLevel(maxDepth);
		_innerPatchRenderer->renderPatch(patchSize, data, 6.3e6);
	}

	void ClipMapGlobe::update(const UpdateData& data) {
		// set spice-orientation in accordance to timestamp
		_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
		_time = data.time;
	}

}  // namespace openspace
