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

#include <ghoul/misc/assert.h>

#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>

#include <modules/globebrowsing/datastructures/chunknode.h>
#include <modules/globebrowsing/rendering/chunklodglobe.h>



namespace {
	const std::string _loggerCat = "ChunkNode";
}

namespace openspace {

int ChunkNode::instanceCount = 0;

ChunkNode::ChunkNode(ChunkLodGlobe& owner, const LatLonPatch& patch, ChunkNode* parent)
: _owner(owner)
, _patch(patch)
, _parent(parent)
{
	_children[0] = nullptr;
	_children[1] = nullptr;
	_children[2] = nullptr;
	_children[3] = nullptr;
	instanceCount++;
}

ChunkNode::~ChunkNode() {
	instanceCount--;
}

bool ChunkNode::isRoot() const {
	return _parent == nullptr;
}

bool ChunkNode::isLeaf() const {
	return _children[0] == nullptr;
}


void ChunkNode::render(const RenderData& data, ChunkIndex traverseData) {
	ghoul_assert(isRoot(), "this method should only be invoked on root");
	//LDEBUG("-------------");
	internalUpdateChunkTree(data, traverseData);
	internalRender(data, traverseData);
}


// Returns true or false wether this node can be merge or not
bool ChunkNode::internalUpdateChunkTree(const RenderData& data, ChunkIndex& traverseData) {
	using namespace glm;
	LatLon center = _patch.center;


	//LDEBUG("x: " << patch.x << " y: " << patch.y << " level: " << patch.level << "  lat: " << center.lat << " lon: " << center.lon);

	if (isLeaf()) {
		int desiredLevel = calculateDesiredLevel(data, traverseData);
		if (desiredLevel > traverseData.level) {
			split();
		}
		else if(desiredLevel < traverseData.level){
			return true; // request a merge from parent
		}
		return false;
	}
	else {
		
		int requestedMergeMask = 0;
		std::vector<ChunkIndex> childIndices = traverseData.childIndices();
		for (int i = 0; i < 4; ++i) {
			if (_children[i]->internalUpdateChunkTree(data, childIndices[i])) {
				requestedMergeMask |= (1 << i);
			}
		}
		

		// check if all children requested merge
		if (requestedMergeMask == 0xf) {
			merge();

			// re-run this method on this, now that this is a leaf node
			return internalUpdateChunkTree(data, traverseData);
		}
		return false;
	}	
}


void ChunkNode::internalRender(const RenderData& data, ChunkIndex& traverseData) {
	if (isLeaf()) {
		PatchRenderer& patchRenderer = _owner.getPatchRenderer();
		patchRenderer.renderPatch(_patch, data, _owner.globeRadius);
	}
	else {
		std::vector<ChunkIndex> childIndices = traverseData.childIndices();
		for (int i = 0; i < 4; ++i) {
			_children[i]->internalRender(data, childIndices[i]);
		}
	}
}

int ChunkNode::calculateDesiredLevel(const RenderData& data, const ChunkIndex& traverseData) {


	Vec3 patchNormal = _patch.center.asUnitCartesian();
	Vec3 patchPosition = data.position.dvec3() + _owner.globeRadius * patchNormal;

	Vec3 cameraPosition = data.camera.position().dvec3();
	Vec3 cameraDirection = Vec3(data.camera.viewDirection());
	Vec3 cameraToChunk = patchPosition - cameraPosition;


	// if camera points at same direction as latlon patch normal,
	// we see the back side and dont have to split it
	Scalar cosNormalCameraDirection = glm::dot(patchNormal, cameraDirection);
	if (cosNormalCameraDirection > 0.3) {
		return traverseData.level - 1;
	}


	Scalar distance = glm::length(cameraToChunk);
	_owner.minDistToCamera = fmin(_owner.minDistToCamera, distance);

	Scalar scaleFactor = 100 * _owner.globeRadius;
	Scalar projectedScaleFactor = scaleFactor / distance;
	int desiredDepth = floor( log2(projectedScaleFactor) );
	return glm::clamp(desiredDepth, _owner.minSplitDepth, _owner.maxSplitDepth);
}



void ChunkNode::split(int depth) {
	if (depth > 0 && isLeaf()) {

		// Defining short handles for center, halfSize and quarterSize
		const LatLon& c = _patch.center;
		const LatLon& hs = _patch.halfSize;
		LatLon qs = LatLon(0.5 * hs.lat, 0.5 * hs.lon);

		// Subdivide bounds
		LatLonPatch nwBounds = LatLonPatch(LatLon(c.lat + qs.lat, c.lon - qs.lon), qs);
		LatLonPatch neBounds = LatLonPatch(LatLon(c.lat - qs.lat, c.lon - qs.lon), qs);
		LatLonPatch swBounds = LatLonPatch(LatLon(c.lat + qs.lat, c.lon + qs.lon), qs);
		LatLonPatch seBounds = LatLonPatch(LatLon(c.lat - qs.lat, c.lon + qs.lon), qs);

		// Create new chunk nodes
		_children[Quad::NORTH_WEST] = std::unique_ptr<ChunkNode>(new ChunkNode(_owner, nwBounds, this));
		_children[Quad::NORTH_EAST] = std::unique_ptr<ChunkNode>(new ChunkNode(_owner, neBounds, this));
		_children[Quad::SOUTH_WEST] = std::unique_ptr<ChunkNode>(new ChunkNode(_owner, swBounds, this));
		_children[Quad::SOUTH_EAST] = std::unique_ptr<ChunkNode>(new ChunkNode(_owner, seBounds, this));
	}

	if (depth - 1 > 0) {
		for (int i = 0; i < 4; ++i) {
			_children[i]->split(depth - 1);
		}
	}
}

void ChunkNode::merge() {
	for (int i = 0; i < 4; ++i) {
		if (_children[i] != nullptr) {
			_children[i]->merge();
		}
		_children[i] = nullptr;
	}
}

const ChunkNode& ChunkNode::getChild(Quad quad) const {
	return *_children[quad];
}



} // namespace openspace
