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


#include <modules/globebrowsing/globes/clipmapglobe.h>

#include <modules/globebrowsing/meshes/clipmapgrid.h>

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
}

namespace openspace {
	ClipMapGlobe::ClipMapGlobe(const Ellipsoid& ellipsoid)
		: _clipMapPyramid(Geodetic2(M_PI / 2, M_PI / 2))
		, _ellipsoid(ellipsoid)
	{
		// init Renderer
		auto outerPatchRenderer = new ClipMapPatchRenderer(shared_ptr<OuterClipMapGrid>(new OuterClipMapGrid(256)));
		_outerPatchRenderer.reset(outerPatchRenderer);
		auto innerPatchRenderer = new ClipMapPatchRenderer(shared_ptr<InnerClipMapGrid>(new InnerClipMapGrid(256)));
		_innerPatchRenderer.reset(innerPatchRenderer);
	}

	ClipMapGlobe::~ClipMapGlobe() {
	}
	
	const Ellipsoid& ClipMapGlobe::ellipsoid() const
	{
		return _ellipsoid;
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
			Geodetic2 patchSize = _clipMapPyramid.getPatchSizeAtLevel(i);
			_outerPatchRenderer->renderPatch(patchSize, data, _ellipsoid);
		}
		Geodetic2 patchSize = _clipMapPyramid.getPatchSizeAtLevel(maxDepth);
		_innerPatchRenderer->renderPatch(patchSize, data, _ellipsoid);
	}

	void ClipMapGlobe::update(const UpdateData& data) {
		
	}

}  // namespace openspace
