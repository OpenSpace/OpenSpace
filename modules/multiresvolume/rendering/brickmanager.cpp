/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <glm/gtx/std_based_type.hpp>

namespace {
    constexpr std::string_view _loggerCat = "BrickManager";
} // namespace

namespace openspace {

BrickManager::BrickManager(TSP* tsp)
    : _tsp(tsp)
{}

BrickManager::~BrickManager() {}

bool BrickManager::readHeader() {
    if (!_tsp->file().is_open()) {
        return false;
    }

    _header = _tsp->header();

    LDEBUG(std::format("Grid type: {}", _header.gridType));
    LDEBUG(std::format("Original num timesteps: {}", _header.numOrigTimesteps));
    LDEBUG(std::format("Num timesteps: {}", _header.numTimesteps));
    LDEBUG(std::format(
        "Brick dims: {} {} {}", _header.xBrickDim, _header.yBrickDim, _header.zBrickDim
    ));
    LDEBUG(std::format(
        "Num bricks: {} {} {}",
        _header.xNumBricks,
        _header.yNumBricks,
        _header.zNumBricks
    ));

    _brickDim = _header.xBrickDim;
    _numBricks = _header.xNumBricks;
    _paddedBrickDim = _brickDim + _paddingWidth * 2;
    _atlasDim = _paddedBrickDim*_numBricks;

    LDEBUG(std::format("Padded brick dim: {}", _paddedBrickDim));
    LDEBUG(std::format("Atlas dim: {}", _atlasDim));

    _numBrickVals = _paddedBrickDim*_paddedBrickDim*_paddedBrickDim;
    // Number of bricks per frame
    _numBricksFrame = _numBricks*_numBricks*_numBricks;

    // Calculate number of bricks in tree
    unsigned int numOTLevels = static_cast<unsigned int>(
        log(static_cast<int>(_numBricks)) / log(2) + 1
    );
    unsigned int numOTNodes = static_cast<unsigned int>((pow(8, numOTLevels) - 1) / 7);
    unsigned int numBSTNodes = _header.numTimesteps * 2 - 1;
    _numBricksTree = numOTNodes * numBSTNodes;
    LDEBUG(std::format("Num OT levels: {}", numOTLevels));
    LDEBUG(std::format("Num OT nodes: {}", numOTNodes));
    LDEBUG(std::format("Num BST nodes: {}", numBSTNodes));
    LDEBUG(std::format("Num bricks in tree: {}", _numBricksTree));
    LDEBUG(std::format("Num values per brick: {}", _numBrickVals));

    _brickSize = sizeof(float) * _numBrickVals;
    _volumeSize = _brickSize * _numBricksFrame;
    _numValsTot = _numBrickVals * _numBricksFrame;

    _tsp->file().seekg(0, _tsp->file().end);
    long long fileSize = _tsp->file().tellg();
    long long calcFileSize = static_cast<long long>(_numBricksTree) *
                             static_cast<long long>(_brickSize) + TSP::dataPosition();


    if (fileSize != calcFileSize) {
        LERROR("Sizes do not match");
        LERROR(std::format("Calculated file size: {}", calcFileSize));
        LERROR(std::format("File size: {}", fileSize));
        return false;
    }

    _hasReadHeader = true;

    // Hold two brick lists
    _brickLists.resize(2);
    // Make sure the brick list can hold the maximum number of bricks
    // Each entry holds tree coordinates
    _brickLists[EVEN].resize(_numBricksTree * 3, -1);
    _brickLists[ODD].resize(_numBricksTree * 3, -1);

    // Allocate space for keeping tracks of bricks in PBO
    _bricksInPBO.resize(2);
    _bricksInPBO[EVEN].resize(_numBricksTree, -1);
    _bricksInPBO[ODD].resize(_numBricksTree, -1);

    // Allocate space for keeping track of the used coordinates in atlas
    _usedCoords.resize(2);
    _usedCoords[EVEN].resize(_numBricksFrame, false);
    _usedCoords[ODD].resize(_numBricksFrame, false);

    return true;
}

bool BrickManager::initialize() {
    if (_atlasInitialized) {
        LWARNING("InitAtlas() - already initialized");
    }

    if (!_hasReadHeader) {
        LWARNING("InitAtlas() - Has not read header, trying to read");
        return readHeader();
    }

    // Prepare the 3D texture
    std::vector<unsigned int> dims;
    dims.push_back(_atlasDim);
    dims.push_back(_atlasDim);
    dims.push_back(_atlasDim);
    _textureAtlas = new ghoul::opengl::Texture(
        glm::size3_t(_atlasDim, _atlasDim, _atlasDim),
        GL_TEXTURE_3D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_FLOAT
    );
    _textureAtlas->uploadTexture();

    _atlasInitialized = true;

    glGenBuffers(2, _pboHandle);

    return true;
}

bool BrickManager::buildBrickList(BUFFER_INDEX bufferIndex,
                                  std::vector<int> &brickRequest)
{
    // Keep track of number bricks used and number of bricks cached
    // (for benchmarking)
    int numBricks = 0;
    int numCached = 0;

    // For every non-zero entry in the request list, assign a texture atlas
    // coordinate. For zero entries, signal "no brick" using -1.

    for (unsigned int i = 0; i < brickRequest.size(); i++) {
        if (brickRequest[i] > 0) {
            numBricks++;

            //INFO("Checking brick " << i);

            // If the brick is already in the atlas, keep the coordinate
            if (_bricksInPBO[bufferIndex][i] != -1) {
                numCached++;

                // Get the corresponding coordinates from index
                int x, y, z;
                coordinatesFromLinear(_bricksInPBO[bufferIndex][i], x, y, z);
                _brickLists[bufferIndex][3 * i + 0] = x;
                _brickLists[bufferIndex][3 * i + 1] = y;
                _brickLists[bufferIndex][3 * i + 2] = z;

                // Mark coordinate as used
                _usedCoords[bufferIndex][_bricksInPBO[bufferIndex][i]] = true;
            }
            else {
                // If coord is already usedi by another brick,
                // skip it and try the next one
                while (_usedCoords[bufferIndex][
                           linearCoordinates(_xCoord, _yCoord, _zCoord)
                       ])
                {
                    incrementCoordinates();
                }

                _brickLists[bufferIndex][3 * i + 0] = _xCoord;
                _brickLists[bufferIndex][3 * i + 1] = _yCoord;
                _brickLists[bufferIndex][3 * i + 2] = _zCoord;
                _usedCoords[bufferIndex][
                    linearCoordinates(_xCoord, _yCoord, _zCoord)
                ] = true;

                incrementCoordinates();
            }
        }
        else {
            // -1 is for "not used"
            _brickLists[bufferIndex][3 * i + 0] = -1;
            _brickLists[bufferIndex][3 * i + 1] = -1;
            _brickLists[bufferIndex][3 * i + 2] = -1;
        }

        // Reset brick list during iteration
        brickRequest[i] = 0;
    }

    // Brick list is build, reset coordinate list
    std::fill(_usedCoords[bufferIndex].begin(), _usedCoords[bufferIndex].end(), false);

    return true;
}

bool BrickManager::fillVolume(float* in, float* out, unsigned int x, unsigned int y,
                              unsigned int z)
{

    //timer_.start();
    unsigned int xMin = x * _paddedBrickDim;
    unsigned int yMin = y * _paddedBrickDim;
    unsigned int zMin = z * _paddedBrickDim;
    unsigned int xMax = xMin + _paddedBrickDim;
    unsigned int yMax = yMin + _paddedBrickDim;
    unsigned int zMax = zMin + _paddedBrickDim;

    // Loop over the brick using three loops
    unsigned int from = 0;
    for (unsigned int zValCoord = zMin; zValCoord < zMax; ++zValCoord) {
        for (unsigned int yValCoord = yMin; yValCoord < yMax; ++yValCoord) {
            for (unsigned int xValCoord = xMin; xValCoord < xMax; ++xValCoord) {
                unsigned int idx = xValCoord + yValCoord * _atlasDim +
                                   zValCoord * _atlasDim * _atlasDim;

                out[idx] = in[from];
                from++;
            }
        }
    }
    return true;
}

void BrickManager::incrementCoordinates() {
    // Update atlas coordinate
    _xCoord++;
    if (_xCoord == static_cast<int>(_header.xNumBricks)) {
        _xCoord = 0;
        _yCoord++;
        if (_yCoord == static_cast<int>(_header.yNumBricks)) {
            _yCoord = 0;
            _zCoord++;
            if (_zCoord == static_cast<int>(_header.zNumBricks)) {
                _zCoord = 0;
            }
        }
    }
}

unsigned int BrickManager::linearCoordinates(int x, int y, int z) {
    return x + y * _header.xNumBricks + z * _header.xNumBricks * _header.yNumBricks;
}

void BrickManager::coordinatesFromLinear(int idx, int& x, int& y, int& z) {
    x = idx % _header.xNumBricks;
    idx /= _header.xNumBricks;
    y = idx % _header.yNumBricks;
    idx /= _header.yNumBricks;
    z = idx;
}

bool BrickManager::diskToPBO(BUFFER_INDEX pboIndex) {
    // Map PBO
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _pboHandle[pboIndex]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _volumeSize, nullptr, GL_STREAM_DRAW);
    float* mappedBuffer = reinterpret_cast<float*>(
        glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY)
    );

    if (!mappedBuffer) {
        LERROR("Failed to map PBO");
        return false;
    }

    // Loop over brick request list
    unsigned int brickIndex = 0;
    while (brickIndex < _brickLists[pboIndex].size() / 3) {
        // Find first active brick index in list
        while (brickIndex < _brickLists[pboIndex].size() / 3 &&
               _brickLists[pboIndex][3 * brickIndex] == -1)
        {
            // If not used, remove from PBO cache list
            _bricksInPBO[pboIndex][brickIndex] = -1;
            brickIndex++;
        }

        // If we are at the end of the list, exit
        if (brickIndex == _brickLists[pboIndex].size() / 3) {
            break;
        }

        // Find a sequence of consecutive bricks in list
        unsigned int sequence = 0;
        // Count number of bricks already in PBO
        unsigned int inPBO = 0;
        unsigned int brickIndexProbe = brickIndex;
        while (brickIndexProbe < _brickLists[pboIndex].size() / 3 &&
            _brickLists[pboIndex][3 * brickIndexProbe] != -1)
        {
            sequence++;
            if (_bricksInPBO[pboIndex][brickIndexProbe] != -1) {
                inPBO++;
            }
            brickIndexProbe++;
        }
        //INFO("Reading " << sequence << " bricks");

        // Read the sequence into a buffer
        float* seqBuffer = new float[sequence * _numBrickVals];
        size_t bufSize = sequence * _numBrickVals * sizeof(float);
        /*
        std::ios::pos_type offset = dataPos_ +
        static_cast<std::ios::pos_type>(brickIndex) *
        static_cast<std::ios::pos_type>(_brickSize);
        */

        long long offset = TSP::dataPosition() +
                           static_cast<long>(brickIndex) * static_cast<long>(_brickSize);

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
            in_.read(reinterpret_cast<char*>(seqBuffer), _brickSize*sequence);
            */

            _tsp->file().seekg(offset);
            _tsp->file().read(reinterpret_cast<char*>(seqBuffer), bufSize);


            //timer_.stop();
            //double time = timer_.elapsed().wall / 1.0e9;
            //double mb = (_brickSize*sequence) / 1048576.0;
            //INFO("Disk read "<<mb<<" MB in "<<time<<" s, "<< mb/time<<" MB/s");

            // For each brick in the buffer, put it the correct buffer spot
            for (unsigned int i = 0; i < sequence; i++) {
                // Only upload if needed
                // Pointless if implementation only skips reading when ALL bricks in
                // sequence are in PBO, but could be useful if other solutions that
                // considers part of the buffer are implemented
                if (_bricksInPBO[pboIndex][brickIndex + i] == -1) {
                    unsigned int x = static_cast<unsigned int>(
                        _brickLists[pboIndex][3 * (brickIndex + i) + 0]
                    );
                    unsigned int y = static_cast<unsigned int>(
                        _brickLists[pboIndex][3 * (brickIndex + i) + 1]
                    );
                    unsigned int z = static_cast<unsigned int>(
                        _brickLists[pboIndex][3 * (brickIndex + i) + 2]
                    );

                    // Put each brick in the correct buffer place.
                    // This needs to be done because the values are in brick order, and
                    // the volume needs to be filled with one big float array.
                    fillVolume(&seqBuffer[_numBrickVals*i], mappedBuffer, x, y, z);
                    // Update the atlas list since the brick will be uploaded
                    //INFO(brickIndex+i);
                    _bricksInPBO[pboIndex][brickIndex + i] = linearCoordinates(x, y, z);
                }
            }
        } // if in pbo

        // Update the brick index
        brickIndex += sequence;

        delete[] seqBuffer;
    }

    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    return true;
}

bool BrickManager::pboToAtlas(BUFFER_INDEX _pboIndex) {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _pboHandle[_pboIndex]);
    glm::size3_t dim = _textureAtlas->dimensions();
    glBindTexture(GL_TEXTURE_3D, *_textureAtlas);
    glTexSubImage3D(
        GL_TEXTURE_3D,
        0,
        0,
        0,
        0,
        static_cast<GLsizei>(dim[0]),
        static_cast<GLsizei>(dim[1]),
        static_cast<GLsizei>(dim[2]),
        GL_RED,
        GL_FLOAT,
        nullptr
    );
    glBindTexture(GL_TEXTURE_3D, 0);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    return true;
}

ghoul::opengl::Texture* BrickManager::textureAtlas() {
    return _textureAtlas;
}

unsigned int BrickManager::pbo(BUFFER_INDEX pboIndex) const {
    return _pboHandle[pboIndex];
}

const std::vector<int>& BrickManager::brickList(BUFFER_INDEX index) const {
    return _brickLists.at(index);
}

} // namespace
