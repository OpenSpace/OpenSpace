/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK___H__

#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace { struct RenderData; }

namespace openspace::globebrowsing {

class RenderableGlobe;
struct TileIndex;

class Chunk {
public:
    constexpr static float DefaultHeight = 0.f;

    struct BoundingHeights {
        float min;
        float max;
        bool available;
    };

    enum class Status {
        DoNothing,
        WantMerge,
        WantSplit
    };

    Chunk(const RenderableGlobe& owner, const TileIndex& tileIndex,
          bool initVisible = true);

    /**
     * Updates the Chunk internally and returns the Status of the Chunk.
     *
     * Tests if the Chunk is cullable and gets the desired level of the Chunk. If the
     * Chunk is cullable it will be set to invisible and return Status::WANT_MERGE.
     * If the desired level is smaller than the current level of the chunk it will
     * return Status::WANT_MERGE, if it is larger it will return Status::WANT_SPLIT,
     * otherwise Status::DO_NOTHING.
     *
     * \return The Status of the chunk.
     */
    Status update(const RenderData& data);

    /**
     * Returns a convex polyhedron of eight vertices tightly bounding the volume of
     * the Chunk.
    */
    std::vector<glm::dvec4> boundingPolyhedronCorners() const;

    const GeodeticPatch& surfacePatch() const;
    const RenderableGlobe& owner() const;
    const TileIndex tileIndex() const;
    bool isVisible() const;

    /**
     * Returns BoundingHeights that fits the Chunk as tightly as possible.
     *
     * If the Chunk uses more than one HightLayer, the BoundingHeights will be set
     * to cover all HeightLayers. If the Chunk has a higher level than its highest
     * resolution HightLayer Tile, it will base its BoundingHeights on that Tile.
     * This means that high level Chunks can have BoundingHeights that are not
     * tightly fitting.
     */
    BoundingHeights boundingHeights() const;

private:
    const RenderableGlobe& _owner;
    const TileIndex _tileIndex;
    bool _isVisible;
    const GeodeticPatch _surfacePatch;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK___H__
