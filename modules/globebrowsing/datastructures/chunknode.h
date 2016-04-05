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

#ifndef __QUADTREE_H__
#define __QUADTREE_H__

#include <glm/glm.hpp>
#include <vector>
#include <memory>
#include <ostream>

// Using double precision
typedef double Scalar;
typedef glm::dvec2 Vec2;

namespace openspace {

enum Quad {
	NORTH_WEST,
	NORTH_EAST,
	SOUTH_WEST,
	SOUTH_EAST
};



struct BoundingRect {
	BoundingRect(Scalar, Scalar, Scalar, Scalar);
	BoundingRect(const Vec2& center, const Vec2& halfSize);
	Vec2 center;
	Vec2 halfSize;
};





class ChunkNode {
public:
	ChunkNode(const BoundingRect&, ChunkNode* parent = nullptr);
	~ChunkNode();


	void split();
	void merge();
	
	bool isRoot() const;
	bool isLeaf() const;
	
	
	const ChunkNode& getChild(Quad quad) const;
	const BoundingRect bounds;

private:
	

	ChunkNode* _parent;
	std::unique_ptr<ChunkNode> _children[4];

};



} // namespace openspace


#include <modules/globebrowsing/datastructures/chunknode.inl>


#endif // __QUADTREE_H__
