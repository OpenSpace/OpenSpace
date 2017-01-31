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

#include <modules/multiresvolume/rendering/brickmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <iostream>
#include <fstream>

namespace {
    const std::string _loggerCat = "BrickManager";
}

namespace openspace {

BrickManager::BrickManager(TSP* tsp) 
    : _tsp(tsp)
    , numBricks_(0)
    , brickDim_(0)
    , paddedBrickDim_(0)
    , atlasDim_(0)
    , numBrickVals_(0)
    , numBricksFrame_(0)
    , numBricksTree_(0)
    , brickSize_(0)
    , volumeSize_(0)
    , numValsTot_(0)
    , xCoord_(0)
    , yCoord_(0)
    , zCoord_(0)
    , textureAtlas_(nullptr)
    , hasReadHeader_(false)
    , atlasInitialized_(false)
{}

BrickManager::~BrickManager() {

}

bool BrickManager::readHeader() {

    if (!_tsp->file().is_open())
        return false;

    _header = _tsp->header();

    LDEBUG("Grid type: " << _header.gridType_);
    LDEBUG("Original num timesteps: " << _header.numOrigTimesteps_);
    LDEBUG("Num timesteps: " << _header.numTimesteps_);
    LDEBUG("Brick dims: " << _header.xBrickDim_ << " " << _header.yBrickDim_ << " " << _header.zBrickDim_);
    LDEBUG("Num bricks: " << _header.xNumBricks_ << " " << _header.yNumBricks_ << " " << _header.zNumBricks_);
    LDEBUG("");

    brickDim_ = _header.xBrickDim_;
    numBricks_ = _header.xNumBricks_;
    paddedBrickDim_ = brickDim_ + paddingWidth_ * 2;
    atlasDim_ = paddedBrickDim_*numBricks_;

    LDEBUG("Padded brick dim: " << paddedBrickDim_);
    LDEBUG("Atlas dim: " << atlasDim_);

    numBrickVals_ = paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;
    // Number of bricks per frame
    numBricksFrame_ = numBricks_*numBricks_*numBricks_;

    // Calculate number of bricks in tree
    unsigned int numOTLevels = static_cast<unsigned int>(log((int)numBricks_) / log(2) + 1);
    unsigned int numOTNodes = static_cast<unsigned int>((pow(8, numOTLevels) - 1) / 7);
    unsigned int numBSTNodes = static_cast<unsigned int>(_header.numTimesteps_ * 2 - 1);
    numBricksTree_ = numOTNodes * numBSTNodes;
    LDEBUG("Num OT levels: " << numOTLevels);
    LDEBUG("Num OT nodes: " << numOTNodes);
    LDEBUG("Num BST nodes: " << numBSTNodes);
    LDEBUG("Num bricks in tree: " << numBricksTree_);
    LDEBUG("Num values per brick: " << numBrickVals_);

    brickSize_ = sizeof(float)*numBrickVals_;
    volumeSize_ = brickSize_*numBricksFrame_;
    numValsTot_ = numBrickVals_*numBricksFrame_;

    _tsp->file().seekg(0, _tsp->file().end);
    long long fileSize = _tsp->file().tellg();
    long long calcFileSize = static_cast<long long>(numBricksTree_)*
        static_cast<long long>(brickSize_) + TSP::dataPosition();


    if (fileSize != calcFileSize) {
        LERROR("Sizes don't match");
        LERROR("calculated file size: " << calcFileSize);
        LERROR("file size: " << fileSize);
        return false;
    }

    hasReadHeader_ = true;

    // Hold two brick lists
    brickLists_.resize(2);
    // Make sure the brick list can hold the maximum number of bricks
    // Each entry holds tree coordinates
    brickLists_[EVEN].resize(numBricksTree_ * 3, -1);
    brickLists_[ODD].resize(numBricksTree_ * 3, -1);

    // Allocate space for keeping tracks of bricks in PBO
    bricksInPBO_.resize(2);
    bricksInPBO_[EVEN].resize(numBricksTree_, -1);
    bricksInPBO_[ODD].resize(numBricksTree_, -1);

    // Allocate space for keeping track of the used coordinates in atlas
    usedCoords_.resize(2);
    usedCoords_[EVEN].resize(numBricksFrame_, false);
    usedCoords_[ODD].resize(numBricksFrame_, false);

    return true;
}

bool BrickManager::initialize() {
    if (atlasInitialized_) {
        LWARNING("InitAtlas() - already initialized");
    }

    if (!hasReadHeader_) {
        LWARNING("InitAtlas() - Has not read header, trying to read");
        return readHeader();
    }

    // Prepare the 3D texture
    std::vector<unsigned int> dims;
    dims.push_back(atlasDim_);
    dims.push_back(atlasDim_);
    dims.push_back(atlasDim_);
    textureAtlas_ = new ghoul::opengl::Texture(
        glm::size3_t(atlasDim_, atlasDim_, atlasDim_), 
        ghoul::opengl::Texture::Format::RGBA, 
        GL_RGBA, 
        GL_FLOAT);
    textureAtlas_->uploadTexture();
    //textureAtlas_ = Texture3D::New(dims);

    //if (!textureAtlas_->Init()) return false;

    atlasInitialized_ = true;


    glGenBuffers(2, pboHandle_);

    return true;
}

bool BrickManager::BuildBrickList(BUFFER_INDEX _bufIdx,
    std::vector<int> &_brickRequest) {

    // Keep track of number bricks used and number of bricks cached
    // (for benchmarking)
    int numBricks = 0;
    int numCached = 0;

    // For every non-zero entry in the request list, assign a texture atlas
    // coordinate. For zero entries, signal "no brick" using -1.

    for (unsigned int i = 0; i<_brickRequest.size(); ++i) {

        if (_brickRequest[i] > 0) {

            numBricks++;

            //INFO("Checking brick " << i);

            // If the brick is already in the atlas, keep the coordinate
            if (bricksInPBO_[_bufIdx][i] != -1) {

                numCached++;

                // Get the corresponding coordinates from index
                int x, y, z;
                CoordsFromLin(bricksInPBO_[_bufIdx][i], x, y, z);
                brickLists_[_bufIdx][3 * i + 0] = x;
                brickLists_[_bufIdx][3 * i + 1] = y;
                brickLists_[_bufIdx][3 * i + 2] = z;

                // Mark coordinate as used
                usedCoords_[_bufIdx][bricksInPBO_[_bufIdx][i]] = true;

            }
            else {

                // If coord is already usedi by another brick, 
                // skip it and try the next one
                while (usedCoords_[_bufIdx][LinearCoord(xCoord_, yCoord_, zCoord_)]) {
                    IncCoord();
                }

                brickLists_[_bufIdx][3 * i + 0] = xCoord_;
                brickLists_[_bufIdx][3 * i + 1] = yCoord_;
                brickLists_[_bufIdx][3 * i + 2] = zCoord_;
                usedCoords_[_bufIdx][LinearCoord(xCoord_, yCoord_, zCoord_)] = true;

                IncCoord();
            }


        }
        else {

            // -1 is for "not used"
            brickLists_[_bufIdx][3 * i + 0] = -1;
            brickLists_[_bufIdx][3 * i + 1] = -1;
            brickLists_[_bufIdx][3 * i + 2] = -1;

        }

        // Reset brick list during iteration
        _brickRequest[i] = 0;

    }


    // Brick list is build, reset coordinate list
    for (auto it = usedCoords_[_bufIdx].begin();
        it != usedCoords_[_bufIdx].end(); ++it) {
        *it = false;
    }

    //INFO("bricks NOT used: " << (float)(numBricksFrame_-numBricks) / (float)(numBricksFrame_));
    //INFO("bricks cached: " << (float)numCached / (float)(numBricksFrame_));

    return true;
}

bool BrickManager::FillVolume(float *_in, float *_out,
    unsigned int _x,
    unsigned int _y,
    unsigned int _z) {

    //timer_.start();
    unsigned int xMin = _x*paddedBrickDim_;
    unsigned int yMin = _y*paddedBrickDim_;
    unsigned int zMin = _z*paddedBrickDim_;
    unsigned int xMax = xMin + paddedBrickDim_;
    unsigned int yMax = yMin + paddedBrickDim_;
    unsigned int zMax = zMin + paddedBrickDim_;

    // Loop over the brick using three loops
    unsigned int from = 0;
    for (unsigned int zValCoord = zMin; zValCoord<zMax; ++zValCoord) {
        for (unsigned int yValCoord = yMin; yValCoord<yMax; ++yValCoord) {
            for (unsigned int xValCoord = xMin; xValCoord<xMax; ++xValCoord) {

                unsigned int idx =
                    xValCoord +
                    yValCoord*atlasDim_ +
                    zValCoord*atlasDim_*atlasDim_;

                _out[idx] = _in[from];
                from++;
            }
        }
    }
    return true;
}

void BrickManager::IncCoord() {
    // Update atlas coordinate
    xCoord_++;
    if (xCoord_ == _header.xNumBricks_) {
        xCoord_ = 0;
        yCoord_++;
        if (yCoord_ == _header.yNumBricks_) {
            yCoord_ = 0;
            zCoord_++;
            if (zCoord_ == _header.zNumBricks_) {
                zCoord_ = 0;
            }
        }
    }
}

unsigned int BrickManager::LinearCoord(int _x, int _y, int _z) {
    return _x + _y*_header.xNumBricks_ + _z*_header.xNumBricks_*_header.yNumBricks_;
}

void BrickManager::CoordsFromLin(int _idx, int &_x, int &_y, int &_z) {
    _x = _idx % _header.xNumBricks_;
    _idx /= _header.xNumBricks_;
    _y = _idx % _header.yNumBricks_;
    _idx /= _header.yNumBricks_;
    _z = _idx;
}


bool BrickManager::DiskToPBO(BUFFER_INDEX _pboIndex) {

    // Map PBO
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandle_[_pboIndex]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, volumeSize_, 0, GL_STREAM_DRAW);
    float *mappedBuffer = reinterpret_cast<float*>(
        glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY));

    if (!mappedBuffer) {
        LERROR("Failed to map PBO");
        return false;
    }

    // Loop over brick request list
    unsigned int brickIndex = 0;
    while (brickIndex < brickLists_[_pboIndex].size() / 3) {

        // Find first active brick index in list
        while (brickIndex<brickLists_[_pboIndex].size() / 3 &&
            brickLists_[_pboIndex][3 * brickIndex] == -1) {
            // If not used, remove from PBO cache list
            bricksInPBO_[_pboIndex][brickIndex] = -1;
            brickIndex++;
        }

        // If we are at the end of the list, exit
        if (brickIndex == brickLists_[_pboIndex].size() / 3) {
            break;
        }

        // Find a sequence of consecutive bricks in list
        unsigned int sequence = 0;
        // Count number of bricks already in PBO
        unsigned int inPBO = 0;
        unsigned int brickIndexProbe = brickIndex;
        while (brickIndexProbe < brickLists_[_pboIndex].size() / 3 &&
            brickLists_[_pboIndex][3 * brickIndexProbe] != -1) {
            sequence++;
            if (bricksInPBO_[_pboIndex][brickIndexProbe] != -1) {
                inPBO++;
            }
            brickIndexProbe++;
        }
        //INFO("Reading " << sequence << " bricks");

        // Read the sequence into a buffer
        float *seqBuffer = new float[sequence*numBrickVals_];
        size_t bufSize = sequence*numBrickVals_*sizeof(float);
        /*
        std::ios::pos_type offset = dataPos_ +
        static_cast<std::ios::pos_type>(brickIndex) *
        static_cast<std::ios::pos_type>(brickSize_);
        */

        long offset = TSP::dataPosition() +
            static_cast<long>(brickIndex)*
            static_cast<long>(brickSize_);

        // Skip reading if all bricks in sequence is already in PBO
        if (inPBO != sequence) {

            //timer_.start();

            /*
            std::streamoff off = static_cast<std::streamoff>(offset);
            in_.seekg(off);
            if (in_.tellg() == -1) {
            ERROR("Failed to get input stream position");
            INFO("offset: " << offset);
            INFO("streamoff max: " << std::numeric_limits<std::streamoff>::max());
            INFO("size_t max: " << std::numeric_limits<size_t>::max());
            return false;
            }
            INFO("in.tellg(): " << in_.tellg());
            in_.read(reinterpret_cast<char*>(seqBuffer), brickSize_*sequence);
            */

            _tsp->file().seekg(offset);
            _tsp->file().read(reinterpret_cast<char*>(seqBuffer), bufSize);


            //timer_.stop();
            //double time = timer_.elapsed().wall / 1.0e9;
            //double mb = (brickSize_*sequence) / 1048576.0;
            //INFO("Disk read "<<mb<<" MB in "<<time<<" s, "<< mb/time<<" MB/s");

            // For each brick in the buffer, put it the correct buffer spot
            for (unsigned int i = 0; i<sequence; ++i) {


                // Only upload if needed
                // Pointless if implementation only skips reading when ALL bricks in
                // sequence are in PBO, but could be useful if other solutions that
                // considers part of the buffer are implemented
                if (bricksInPBO_[_pboIndex][brickIndex + i] == -1) {

                    unsigned int x = static_cast<unsigned int>(
                        brickLists_[_pboIndex][3 * (brickIndex + i) + 0]);
                    unsigned int y = static_cast<unsigned int>(
                        brickLists_[_pboIndex][3 * (brickIndex + i) + 1]);
                    unsigned int z = static_cast<unsigned int>(
                        brickLists_[_pboIndex][3 * (brickIndex + i) + 2]);

                    // Put each brick in the correct buffer place.
                    // This needs to be done because the values are in brick order, and
                    // the volume needs to be filled with one big float array.
                    FillVolume(&seqBuffer[numBrickVals_*i], mappedBuffer, x, y, z);
                    // Update the atlas list since the brick will be uploaded
                    //INFO(brickIndex+i);
                    bricksInPBO_[_pboIndex][brickIndex + i] = LinearCoord(x, y, z);

                }
            }

            delete[] seqBuffer;

        } // if in pbo

        // Update the brick index
        brickIndex += sequence;

    }

    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    return true;
}

bool BrickManager::PBOToAtlas(BUFFER_INDEX _pboIndex) {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandle_[_pboIndex]);
    glm::size3_t dim = textureAtlas_->dimensions();
    glBindTexture(GL_TEXTURE_3D, *textureAtlas_);
    glTexSubImage3D(GL_TEXTURE_3D,    // target
        0,                            // level
        0,                            // xoffset
        0,                            // yoffset
        0,                            // zoffset
        dim[0],                        // width
        dim[1],                        // height
        dim[2],                        // depth
        GL_RED,                        // format
        GL_FLOAT,                    // type
        NULL);                        // *pixels
    glBindTexture(GL_TEXTURE_3D, 0);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    return true;
}

ghoul::opengl::Texture* BrickManager::textureAtlas() {
    return textureAtlas_;
}

unsigned int BrickManager::pbo(BUFFER_INDEX _pboIndex) {
    return pboHandle_[_pboIndex];
}

const std::vector<int>& BrickManager::brickList(BUFFER_INDEX index) const {
    return brickLists_.at(index);
}

}
