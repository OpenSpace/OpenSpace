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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___LOCALERRORHISTOGRAMMANAGER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___LOCALERRORHISTOGRAMMANAGER___H__

#include <openspace/util/histogram.h>
#include <ghoul/glm.h>
#include <iosfwd>
#include <map>

namespace openspace {

class TSP;

class LocalErrorHistogramManager {
public:
    LocalErrorHistogramManager(TSP* tsp);

    bool buildHistograms(int numBins);
    const Histogram* spatialHistogram(unsigned int brickIndex) const;
    const Histogram* temporalHistogram(unsigned int brickIndex) const;

    bool loadFromFile(const std::string& filename);
    bool saveToFile(const std::string& filename);

private:
    TSP* _tsp = nullptr;
    std::ifstream* _file;

    std::vector<Histogram> _spatialHistograms;
    std::vector<Histogram> _temporalHistograms;
    unsigned int _numInnerNodes = 0;
    float _minBin = 0.f;
    float _maxBin = 0.f;
    int _numBins = 0;

    std::map<unsigned int, std::vector<float>> _voxelCache;

    bool buildFromOctreeChild(unsigned int bstOffset, unsigned int octreeOffset);
    bool buildFromBstChild(unsigned int bstOffset, unsigned int octreeOffset);

    std::vector<float> readValues(unsigned int brickIndex) const;

    int parentOffset(int offset, int base) const;

    unsigned int brickToInnerNodeIndex(unsigned int brickIndex) const;
    unsigned int innerNodeToBrickIndex(unsigned int innerNodeIndex) const;
    unsigned int linearCoords(glm::vec3 coords) const;
    unsigned int linearCoords(int x, int y, int z) const;
    unsigned int linearCoords(glm::ivec3 coords) const;

    float interpolate(glm::vec3 samplePoint, const std::vector<float>& voxels) const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___LOCALERRORHISTOGRAMMANAGER___H__
