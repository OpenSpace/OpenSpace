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

#include "gtest/gtest.h"

#include <openspace/scene/scenegraphnode.h>
#include <openspace/../modules/globebrowsing/datastructures/chunknode.h>

#include <fstream>
#include <glm/glm.hpp>

using namespace openspace;

class ChunkNodeTest : public testing::Test {};

TEST_F(ChunkNodeTest, Split) {
	BoundingRect bounds(Vec2(2, 2), Vec2(2, 2));
	auto cn = std::shared_ptr<ChunkNode>(new ChunkNode(bounds));
	ASSERT_TRUE(cn->isRoot()) << "Chunk node is root";
	ASSERT_TRUE(cn->isLeaf()) << "Chunk node is leaf";

	cn->split();
	ASSERT_TRUE(cn->isRoot()) << "Chunk node is root";
	ASSERT_FALSE(cn->isLeaf()) << "Chunk node is not leaf";

	ASSERT_EQ(cn->bounds.center.x, cn->getChild(Quad::NORTH_WEST).bounds.center.x * 2);
	ASSERT_EQ(cn->bounds.center.x, cn->getChild(Quad::NORTH_EAST).bounds.center.x * 2/3);

	ASSERT_EQ(cn->bounds.halfSize.x, cn->getChild(Quad::NORTH_WEST).bounds.halfSize.x * 2);
	ASSERT_EQ(cn->bounds.halfSize.y, cn->getChild(Quad::NORTH_WEST).bounds.halfSize.y * 2);
}

TEST_F(ChunkNodeTest, Merge) {
	BoundingRect bounds(Vec2(2, 2), Vec2(2, 2));
	ChunkNode cn(bounds);
	ASSERT_TRUE(cn.isRoot()) << "Chunk node is root";
	ASSERT_TRUE(cn.isLeaf()) << "Chunk node is leaf";

	cn.split();
	ASSERT_TRUE(cn.isRoot()) << "Chunk node is root";
	ASSERT_FALSE(cn.isLeaf()) << "Chunk node is not leaf";

	cn.merge();
	ASSERT_TRUE(cn.isRoot()) << "Chunk node is root";
	ASSERT_TRUE(cn.isLeaf()) << "Chunk node is leaf";

}