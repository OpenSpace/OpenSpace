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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___ATLASMANAGER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___ATLASMANAGER___H__

#include <ghoul/glm.h>
#include <glm/gtx/std_based_type.hpp>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ghoul::opengl { class Texture; }

namespace openspace {

class TSP;

class AtlasManager {
public:
    enum BufferIndex {
        EVEN = 0,
        ODD = 1
    };

    AtlasManager(TSP* tsp);
    ~AtlasManager() = default;

    void updateAtlas(BufferIndex bufferIndex, std::vector<int>& brickIndices);
    void addToAtlas(int firstBrickIndex, int lastBrickIndex, float* mappedBuffer);
    void removeFromAtlas(int brickIndex);
    bool initialize();
    const std::vector<unsigned int>& atlasMap() const;
    unsigned int atlasMapBuffer() const;

    void pboToAtlas(BufferIndex bufferIndex);
    ghoul::opengl::Texture& textureAtlas();

    unsigned int numDiskReads() const;
    unsigned int numUsedBricks() const;
    unsigned int numStreamedBricks() const;

    glm::size3_t textureSize() const;

private:
    const unsigned int NotUsedIndex = std::numeric_limits<unsigned int>::max();

    TSP* _tsp;
    unsigned int _pboHandle[2];
    unsigned int _atlasMapBuffer;

    std::vector<unsigned int> _atlasMap;
    std::map<unsigned int, unsigned int> _brickMap;
    std::vector<unsigned int> _freeAtlasCoords;
    std::set<unsigned int> _requiredBricks;
    std::set<unsigned int> _prevRequiredBricks;

    ghoul::opengl::Texture* _textureAtlas;

    // Stats
    unsigned int _nUsedBricks;
    unsigned int _nStreamedBricks;
    unsigned int _nDiskReads;

    unsigned int _nBricksPerDim;
    unsigned int _nOtLeaves;
    unsigned int _nOtNodes;
    unsigned int _nOtLevels;
    unsigned int _brickSize;
    unsigned int _nBrickVals;
    unsigned int _volumeSize;
    unsigned int _paddedBrickDim;
    unsigned int _nBricksInAtlas;
    unsigned int _nBricksInMap;
    unsigned int _atlasDim;

    void fillVolume(float* in, float* out, unsigned int linearAtlasCoords);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___ATLASMANAGER___H__
