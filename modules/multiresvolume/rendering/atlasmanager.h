/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/multiresvolume/rendering/tsp.h>
#include <ghoul/glm.h>
#include <glm/gtx/std_based_type.hpp>

#include <string>
#include <vector>
#include <climits>
#include <map>
#include <set>

namespace ghoul {
    namespace opengl {
        class Texture;
    }
}

namespace openspace {

class AtlasManager {
public:
    enum BUFFER_INDEX { EVEN = 0, ODD = 1 };

    AtlasManager(TSP* tsp);
    ~AtlasManager();

    void updateAtlas(BUFFER_INDEX bufferIndex, std::vector<int>& brickIndices);
    void addToAtlas(int firstBrickIndex, int lastBrickIndex, float* mappedBuffer);
    void removeFromAtlas(int brickIndex);
    bool initialize();
    std::vector<unsigned int> atlasMap();
    unsigned int atlasMapBuffer();

    void pboToAtlas(BUFFER_INDEX bufferIndex);
    ghoul::opengl::Texture& textureAtlas();
    glm::size3_t textureSize();

    unsigned int getNumDiskReads();
    unsigned int getNumUsedBricks();
    unsigned int getNumStreamedBricks();
private:
    const unsigned int NOT_USED = UINT_MAX;
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

    unsigned int _nBricksPerDim,
                 _nOtLeaves,
                 _nOtNodes,
                 _nOtLevels,
                 _brickSize,
                 _nBrickVals,
                 _volumeSize,
                 _paddedBrickDim,
                 _nBricksInAtlas,
                 _nBricksInMap,
                 _atlasDim;

    void fillVolume(float* in, float* out, unsigned int linearAtlasCoords);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___ATLASMANAGER___H__
