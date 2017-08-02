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

#include <modules/multiresvolume/rendering/allatlasmanager.h>
#include <modules/multiresvolume/rendering/atlasmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <iostream>
#include <fstream>
#include <cassert>
#include <cstring>

namespace {
    const char* _loggerCat = "AllAtlasManager";
} // namespace

namespace openspace {

AllAtlasManager::AllAtlasManager(TSP* tsp) : AtlasManager(tsp) {}

AllAtlasManager::~AllAtlasManager() {}

void AtlasManager::updateAtlas(BUFFER_INDEX bufferIndex, std::vector<int>& brickIndices) {
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
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _volumeSize, 0, GL_STREAM_DRAW);
    float* mappedBuffer = reinterpret_cast<float*>(glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY));

    if (!mappedBuffer) {
        LERROR("Failed to map PBO");
        std::cout << glGetError() << std::endl;
        return;
    }

    for (auto itStart = _requiredBricks.begin(); itStart != _requiredBricks.end();) {
        int firstBrick = *itStart;
        int lastBrick = firstBrick;

        auto itEnd = itStart;
        for (itEnd++; itEnd != _requiredBricks.end() && *itEnd == lastBrick + 1; itEnd++) {
            lastBrick = *itEnd;
        }

        addToAtlas(firstBrick, lastBrick, mappedBuffer);

        itStart = itEnd;
    }

    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    for (int i = 0; i < nBrickIndices; i++) {
        _atlasMap[i] = _brickMap[brickIndices[i]];
    }

    std::swap(_prevRequiredBricks, _requiredBricks);

    pboToAtlas(bufferIndex);

    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _atlasMapBuffer);
    GLint *to = (GLint*)glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_WRITE_ONLY);
    memcpy(to, _atlasMap.data(), sizeof(GLint)*_atlasMap.size());
    glUnmapBuffer(GL_SHADER_STORAGE_BUFFER);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);

}