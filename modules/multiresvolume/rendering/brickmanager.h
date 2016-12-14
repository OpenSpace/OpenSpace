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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__

#include <modules/multiresvolume/rendering/tsp.h>

#include <string>
#include <vector>

namespace ghoul {
    namespace opengl {
        class Texture;
    }
}

namespace openspace {

class BrickManager {
public:
    enum BUFFER_INDEX { EVEN = 0, ODD = 1 };

    BrickManager(TSP* tsp);
    ~BrickManager();

    bool readHeader();

    bool initialize(); 

    bool BuildBrickList(BUFFER_INDEX _bufIdx, std::vector<int> &_brickRequest);

    bool FillVolume(float *_in, float *_out,
        unsigned int _x,
        unsigned int _y,
        unsigned int _z);
    bool DiskToPBO(BUFFER_INDEX _pboIndex);
    bool PBOToAtlas(BUFFER_INDEX _pboIndex);

    ghoul::opengl::Texture* textureAtlas();
    unsigned int pbo(BUFFER_INDEX _pboIndex);
    const std::vector<int>& brickList(BUFFER_INDEX index) const;

private:

    void IncCoord();
    unsigned int LinearCoord(int _x, int _y, int _z);
    void CoordsFromLin(int _idx, int &_x, int &_y, int &_z);

    TSP* _tsp;
    TSP::Header _header;

    unsigned int numBricks_;
    unsigned int brickDim_;
    unsigned int paddedBrickDim_;
    unsigned int atlasDim_;

    const unsigned int paddingWidth_ = 1;

    unsigned int numBrickVals_;
    unsigned int numBricksFrame_;
    unsigned int numBricksTree_;
    unsigned int brickSize_;
    unsigned int volumeSize_;
    unsigned int numValsTot_;

    // Texture coordinates to be assigned
    int xCoord_;
    int yCoord_;
    int zCoord_;

    // Texture where the actual atlas is kept
    ghoul::opengl::Texture* textureAtlas_;

    std::vector<std::vector<int> > brickLists_;

    bool hasReadHeader_;
    bool atlasInitialized_;

    // PBOs
    unsigned int pboHandle_[2];

    // Caching, one for each PBO
    std::vector<std::vector<int> > bricksInPBO_;
    std::vector<std::vector<bool> > usedCoords_;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKMANAGER___H__
