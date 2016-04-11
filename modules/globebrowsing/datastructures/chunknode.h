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

#include <modules/globebrowsing/datastructures/latlon.h>
#include <modules/globebrowsing/rendering/patchrenderer.h>



// forward declaration
namespace openspace {
	class ChunkLodGlobe;
}


namespace openspace {

enum Quad {
	NORTH_WEST,
	NORTH_EAST,
	SOUTH_WEST,
	SOUTH_EAST
};




class ChunkNode : public Renderable{
public:
	ChunkNode(ChunkLodGlobe&, const LatLonPatch&, ChunkNode* parent = nullptr);
	~ChunkNode();


	void split(int depth = 1);
	void merge();

	bool isRoot() const;
	bool isLeaf() const;
	
	const ChunkNode& getChild(Quad quad) const;


	bool initialize() override;
	bool deinitialize() override;
	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

	static int instanceCount;

private:

	void internalRender(const RenderData& data, int currLevel);
	void internalUpdate(const UpdateData& data, int currLevel);
	bool internalUpdateChunkTree(const RenderData& data, int currLevel);
	int desiredSplitDepth(const RenderData& data);
	
	
	ChunkNode* _parent;
	std::unique_ptr<ChunkNode> _children[4];

	ChunkLodGlobe& _owner;

	LatLonPatch _patch;
	
};

} // namespace openspace



#endif // __QUADTREE_H__
