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

#include <modules/multiresvolume/rendering/atlasmanager.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cstring>

namespace openspace {

AtlasManager::AtlasManager(TSP* tsp) : _tsp(tsp) {}

bool AtlasManager::initialize() {
    TSP::Header header = _tsp->header();

    _nBricksPerDim = header.xNumBricks;
    _nOtLeaves = _nBricksPerDim * _nBricksPerDim * _nBricksPerDim;
    _nOtNodes = _tsp->numOTNodes();
    _nOtLevels = static_cast<unsigned int>(log(_nOtLeaves) / log(8) + 1);
    _paddedBrickDim = _tsp->paddedBrickDim();
    _nBricksInMap = _nBricksPerDim * _nBricksPerDim * _nBricksPerDim;
    _atlasDim = _nBricksPerDim * _paddedBrickDim;
    _nBrickVals = _paddedBrickDim*_paddedBrickDim*_paddedBrickDim;
    _brickSize = _nBrickVals * sizeof(float);
    _volumeSize = _brickSize * _nOtLeaves;
    _atlasMap = std::vector<unsigned int>(_nOtLeaves, NotUsedIndex);
    _nBricksInAtlas = _nBricksInMap;

    _freeAtlasCoords = std::vector<unsigned int>(_nBricksInAtlas, 0);

    for (unsigned int i = 0; i < _nBricksInAtlas; i++) {
        _freeAtlasCoords[i] = i;
    }

    _textureAtlas = new ghoul::opengl::Texture(
        glm::size3_t(_atlasDim, _atlasDim, _atlasDim),
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_FLOAT
    );
    _textureAtlas->uploadTexture();

    glGenBuffers(2, _pboHandle);

    glGenBuffers(1, &_atlasMapBuffer);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _atlasMapBuffer);
    glBufferData(
        GL_SHADER_STORAGE_BUFFER,
        sizeof(GLint) * _nBricksInMap,
        nullptr,
        GL_DYNAMIC_READ
    );
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);

    return true;
}

const std::vector<unsigned int>& AtlasManager::atlasMap() const {
    return _atlasMap;
}

unsigned int AtlasManager::atlasMapBuffer() const {
    return _atlasMapBuffer;
}

void AtlasManager::updateAtlas(BufferIndex bufferIndex, std::vector<int>& brickIndices) {
    size_t nBrickIndices = brickIndices.size();

    _requiredBricks.clear();
    for (size_t i = 0; i < nBrickIndices; i++) {
        _requiredBricks.insert(brickIndices[i]);
    }

    for (unsigned int it : _prevRequiredBricks) {
        if (!_requiredBricks.count(it)) {
            removeFromAtlas(it);
        }
    }

    // Stats
    _nUsedBricks = static_cast<unsigned int>(_requiredBricks.size());
    _nStreamedBricks = 0;
    _nDiskReads = 0;

    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _pboHandle[bufferIndex]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _volumeSize, nullptr, GL_STREAM_DRAW);
    float* mappedBuffer = reinterpret_cast<float*>(
        glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY)
    );

    if (!mappedBuffer) {
        LERRORC("AtlasManager", "Failed to map PBO");
        return;
    }

    for (auto itStart = _requiredBricks.begin(); itStart != _requiredBricks.end();) {
        int firstBrick = *itStart;
        int lastBrick = firstBrick;

        auto itEnd = itStart;
        for (itEnd++;
            itEnd != _requiredBricks.end() &&
            *itEnd == static_cast<unsigned int>(lastBrick) + 1;
            itEnd++)
        {
            lastBrick = *itEnd;
        }

        addToAtlas(firstBrick, lastBrick, mappedBuffer);

        itStart = itEnd;
    }

    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    for (size_t i = 0; i < nBrickIndices; i++) {
        _atlasMap[i] = _brickMap[brickIndices[i]];
    }

    std::swap(_prevRequiredBricks, _requiredBricks);

    pboToAtlas(bufferIndex);

    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _atlasMapBuffer);
    GLint* to = reinterpret_cast<GLint*>(
        glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_WRITE_ONLY)
    );
    memcpy(to, _atlasMap.data(), sizeof(GLint)*_atlasMap.size());
    glUnmapBuffer(GL_SHADER_STORAGE_BUFFER);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
}

void AtlasManager::addToAtlas(int firstBrickIndex, int lastBrickIndex,
                              float* mappedBuffer)
{
    while (_brickMap.count(firstBrickIndex) && firstBrickIndex <= lastBrickIndex) {
        firstBrickIndex++;
    }
    while (_brickMap.count(lastBrickIndex) && lastBrickIndex >= firstBrickIndex) {
        lastBrickIndex--;
    }
    if (lastBrickIndex < firstBrickIndex) {
        return;
    }

    int sequenceLength = lastBrickIndex - firstBrickIndex + 1;
    float* sequenceBuffer = new float[sequenceLength*_nBrickVals];
    size_t bufferSize = sequenceLength * _brickSize;

    long long offset = TSP::dataPosition() + static_cast<long long>(firstBrickIndex) *
                       static_cast<long long>(_brickSize);
    _tsp->file().seekg(offset);
    _tsp->file().read(reinterpret_cast<char*>(sequenceBuffer), bufferSize);
    _nDiskReads++;

    for (int brickIndex = firstBrickIndex; brickIndex <= lastBrickIndex; brickIndex++) {
        if (!_brickMap.count(brickIndex)) {
            unsigned int atlasCoords = _freeAtlasCoords.back();
            _freeAtlasCoords.pop_back();
            int level = _nOtLevels - static_cast<int>(
                floor(log((7.0 * (float(brickIndex % _nOtNodes)) + 1.0))/log(8)) - 1
            );
            ghoul_assert(atlasCoords <= 0x0FFFFFFF, "@MISSING");
            unsigned int atlasData = (level << 28) + atlasCoords;
            _brickMap.emplace(brickIndex, atlasData);
            _nStreamedBricks++;
            fillVolume(
                &sequenceBuffer[_nBrickVals*(brickIndex - firstBrickIndex)],
                mappedBuffer,
                atlasCoords
            );
        }
    }

    delete[] sequenceBuffer;
}

void AtlasManager::removeFromAtlas(int brickIndex) {
    unsigned int atlasData = _brickMap[brickIndex];
    unsigned int atlasCoords = atlasData & 0x0FFFFFFF;
    _brickMap.erase(brickIndex);
    _freeAtlasCoords.push_back(atlasCoords);
}

void AtlasManager::fillVolume(float* in, float* out, unsigned int linearAtlasCoords) {
    int x = linearAtlasCoords % _nBricksPerDim;
    int y = (linearAtlasCoords / _nBricksPerDim) % _nBricksPerDim;
    int z = linearAtlasCoords / _nBricksPerDim / _nBricksPerDim;

    unsigned int xMin = x*_paddedBrickDim;
    unsigned int yMin = y*_paddedBrickDim;
    unsigned int zMin = z*_paddedBrickDim;
    unsigned int xMax = xMin + _paddedBrickDim;
    unsigned int yMax = yMin + _paddedBrickDim;
    unsigned int zMax = zMin + _paddedBrickDim;

    unsigned int from = 0;
    for (unsigned int zValCoord = zMin; zValCoord<zMax; ++zValCoord) {
        for (unsigned int yValCoord = yMin; yValCoord<yMax; ++yValCoord) {
            for (unsigned int xValCoord = xMin; xValCoord<xMax; ++xValCoord) {
                unsigned int idx = xValCoord + yValCoord * _atlasDim +
                                   zValCoord * _atlasDim * _atlasDim;

                out[idx] = in[from];
                from++;
            }
        }
    }
}

void AtlasManager::pboToAtlas(BufferIndex bufferIndex) {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _pboHandle[bufferIndex]);
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
}

ghoul::opengl::Texture& AtlasManager::textureAtlas() {
    ghoul_assert(_textureAtlas != nullptr, "Texture atlas is nullptr");
    return *_textureAtlas;
}

unsigned int AtlasManager::numDiskReads() const {
    return _nDiskReads;
}

unsigned int AtlasManager::numUsedBricks() const {
    return _nUsedBricks;
}

unsigned int AtlasManager::numStreamedBricks() const {
    return _nStreamedBricks;
}

glm::size3_t AtlasManager::textureSize() const {
    return _textureAtlas->dimensions();
}

} // namespace openspace
