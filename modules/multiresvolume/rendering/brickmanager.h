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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__

#include <modules/multiresvolume/rendering/tsp.h>

#include <vector>

namespace ghoul::opengl { class Texture; }

namespace openspace {

class BrickManager {
public:
    enum BUFFER_INDEX { EVEN = 0, ODD = 1 };

    BrickManager(TSP* tsp);
    ~BrickManager();

    bool readHeader();

    bool initialize();

    bool buildBrickList(BUFFER_INDEX bufferIndex, std::vector<int>& brickRequest);

    bool fillVolume(float* in, float* out, unsigned int x, unsigned int y,
        unsigned int z);
    bool diskToPBO(BUFFER_INDEX pboIndex);
    bool pboToAtlas(BUFFER_INDEX pboIndex);

    ghoul::opengl::Texture* textureAtlas();
    unsigned int pbo(BUFFER_INDEX pboIndex) const;
    const std::vector<int>& brickList(BUFFER_INDEX index) const;

private:
    void incrementCoordinates();
    unsigned int linearCoordinates(int x, int y, int z);
    void coordinatesFromLinear(int idx, int& x, int& y, int& z);

    TSP* _tsp = nullptr;
    TSP::Header _header;

    unsigned int _numBricks = 0;
    unsigned int _brickDim = 0;
    unsigned int _paddedBrickDim = 0;
    unsigned int _atlasDim = 0;

    const unsigned int _paddingWidth = 1;

    unsigned int _numBrickVals = 0;
    unsigned int _numBricksFrame = 0;
    unsigned int _numBricksTree = 0;
    unsigned int _brickSize = 0;
    unsigned int _volumeSize = 0;
    unsigned int _numValsTot = 0;

    // Texture coordinates to be assigned
    int _xCoord = 0;
    int _yCoord = 0;
    int _zCoord = 0;

    // Texture where the actual atlas is kept
    ghoul::opengl::Texture* _textureAtlas = nullptr;

    std::vector<std::vector<int>> _brickLists;

    bool _hasReadHeader = false;
    bool _atlasInitialized = false;

    // PBOs
    unsigned int _pboHandle[2];

    // Caching, one for each PBO
    std::vector<std::vector<int>> _bricksInPBO;
    std::vector<std::vector<bool>> _usedCoords;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__
