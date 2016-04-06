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

#include <modules/globebrowsing/datastructures/chunknode.h>
#include <modules/globebrowsing/rendering/chunklodglobe.h>

namespace openspace {

BoundingRect::BoundingRect(Scalar cx, Scalar cy, Scalar hsx, Scalar hsy)
	: center(Vec2(cx, cy)), halfSize(Vec2(hsx, hsy)) { }

BoundingRect::BoundingRect(const Vec2& center, const Vec2& halfSize)
	:center(center), halfSize(halfSize) { }






ChunkNode::ChunkNode(ChunkLodGlobe& owner, const BoundingRect& bounds, ChunkNode* parent)
: _owner(owner)
, bounds(bounds)
, _parent(parent)
{
	_children[0] = nullptr;
	_children[1] = nullptr;
	_children[2] = nullptr;
	_children[3] = nullptr;
}

ChunkNode::~ChunkNode() {
	
}

bool ChunkNode::isRoot() const {
	return _parent == nullptr;
}

bool ChunkNode::isLeaf() const {
	return _children[0] == nullptr;
}



bool ChunkNode::initialize()  {
	if (!isLeaf()) {
		for (int i = 0; i < 4; ++i) {
			_children[i]->initialize();
		}
	}
	
	return isReady();
}

bool ChunkNode::deinitialize() {
	if (!isLeaf()) {
		for (int i = 0; i < 4; ++i) {
			_children[i]->deinitialize();
		}
	}
	return true;
}

bool ChunkNode::isReady() const{
	bool ready = true;
	return ready;
}

void ChunkNode::render(const RenderData& data) {
	internalRender(data, 0);
}

void ChunkNode::internalRender(const RenderData& data, int currLevel) {
	if (isLeaf()) {
		LatLonPatch& templatePatch = _owner.getTemplatePatch();
		templatePatch.setPositionLatLon(bounds.center);
		templatePatch.setSizeLatLon(bounds.halfSize);
		templatePatch.render(data);
	}
	else {
		for (int i = 0; i < 4; ++i) {
			_children[i]->internalRender(data, currLevel+1);
		}
	}
}

void ChunkNode::update(const UpdateData& data) {
	internalUpdate(data, 0);
}

void ChunkNode::internalUpdate(const UpdateData& data, int currLevel) {
	if (!isLeaf()) {
		for (int i = 0; i < 4; ++i) {
			_children[i]->internalUpdate(data, currLevel + 1);
		}
	}
}


void ChunkNode::split(int depth) {
	if (depth > 0 && isLeaf()) {

		// Defining short handles for center, halfSize and quarterSize
		const Vec2& c = bounds.center;
		const Vec2& hs = bounds.halfSize;
		Vec2 qs = 0.5 * bounds.halfSize;

		// Subdivide bounds
		BoundingRect nwBounds = BoundingRect(c + Vec2(-qs.x, -qs.y), qs);
		BoundingRect neBounds = BoundingRect(c + Vec2(+qs.x, -qs.y), qs);
		BoundingRect swBounds = BoundingRect(c + Vec2(-qs.x, +qs.y), qs);
		BoundingRect seBounds = BoundingRect(c + Vec2(+qs.x, +qs.y), qs);

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


const ChunkNode&  ChunkNode::getChild(Quad quad) const {
	return *_children[quad];
}





} // namespace openspace
