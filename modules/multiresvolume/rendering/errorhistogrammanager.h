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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___ERRORHISTOGRAMMANAGER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___ERRORHISTOGRAMMANAGER___H__

#include <openspace/util/histogram.h>
#include <ghoul/glm.h>
#include <iosfwd>
#include <map>

namespace openspace {

class TSP;

class ErrorHistogramManager {
public:
    ErrorHistogramManager(TSP* tsp);
    ~ErrorHistogramManager() = default;

    bool buildHistograms(int numBins);
    const Histogram* histogram(unsigned int brickIndex) const;

    bool loadFromFile(const std::string& filename);
    bool saveToFile(const std::string& filename);

private:
    TSP* _tsp;
    std::ifstream* _file;

    std::vector<Histogram> _histograms;
    unsigned int _numInnerNodes;
    float _minBin;
    float _maxBin;
    int _numBins;

    std::map<unsigned int, std::vector<float>> _voxelCache;

    bool buildFromLeaf(unsigned int bstOffset, unsigned int octreeOffset);
    std::vector<float> readValues(unsigned int brickIndex) const;

    int parentOffset(int offset, int base) const;

    unsigned int brickToInnerNodeIndex(unsigned int brickIndex) const;
    unsigned int innerNodeToBrickIndex(unsigned int innerNodeIndex) const;
    unsigned int linearCoords(const glm::vec3& coords) const;
    unsigned int linearCoords(int x, int y, int z) const;
    unsigned int linearCoords(const glm::ivec3& coords) const;

    float interpolate(const glm::vec3& samplePoint,
        const std::vector<float>& voxels) const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___ERRORHISTOGRAMMANAGER___H__
