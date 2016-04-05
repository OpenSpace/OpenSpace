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


namespace openspace {

BoundingRect::BoundingRect(Scalar cx, Scalar cy, Scalar hsx, Scalar hsy)
	: center(Vec2(cx, cy)), halfSize(Vec2(hsx, hsy)) { }

BoundingRect::BoundingRect(const Vec2& center, const Vec2& halfSize)
	:center(center), halfSize(halfSize) { }




ChunkNode::ChunkNode(const BoundingRect& bounds, ChunkNode* parent)
: bounds(bounds), _parent(parent)
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


void ChunkNode::split() {
	// Defining short handles for center, halfSize and quarterSize
	const Vec2& c = bounds.center;
	const Vec2& hs =bounds.halfSize;
	Vec2 qs = 0.5 * bounds.halfSize;

	// Subdivide bounds
	BoundingRect nwBounds = BoundingRect(c + Vec2(-qs.x, -qs.y), qs);
	BoundingRect neBounds = BoundingRect(c + Vec2(+qs.x, -qs.y), qs);
	BoundingRect swBounds = BoundingRect(c + Vec2(-qs.x, +qs.y), qs);
	BoundingRect seBounds = BoundingRect(c + Vec2(+qs.x, +qs.y), qs);

	// Create new chunk nodes
	_children[Quad::NORTH_WEST] = std::unique_ptr<ChunkNode>(new ChunkNode(nwBounds, this));
	_children[Quad::NORTH_EAST] = std::unique_ptr<ChunkNode>(new ChunkNode(neBounds, this));
	_children[Quad::SOUTH_WEST] = std::unique_ptr<ChunkNode>(new ChunkNode(swBounds, this));
	_children[Quad::SOUTH_EAST] = std::unique_ptr<ChunkNode>(new ChunkNode(seBounds, this));
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
