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

#include <float.h>
#include <map>
#include <string.h>
#include <cmath>

#include <modules/multiresvolume/rendering/localerrorhistogrammanager.h>
#include <openspace/util/histogram.h>

#include <openspace/util/progressbar.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "LocalErrorHistogramManager";
}

namespace openspace {

LocalErrorHistogramManager::LocalErrorHistogramManager(TSP* tsp) : ErrorHistogramManager(tsp) { }

LocalErrorHistogramManager::~LocalErrorHistogramManager() {}

bool LocalErrorHistogramManager::buildHistograms(int numBins) {
    if (!initHistogramVars(numBins)) return false;

    unsigned int numOtLevels = _tsp->numOTLevels();
    unsigned int numOtLeaves = pow(8, numOtLevels - 1);
    unsigned int numBstLeaves = pow(2, _tsp->numBSTLevels() - 1);

    _numInnerNodes = _tsp->numTotalNodes() - numOtLeaves * numBstLeaves;

    _spatialHistograms = std::vector<Histogram>(_numInnerNodes);
    _temporalHistograms = std::vector<Histogram>(_numInnerNodes);
    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        _spatialHistograms[i] = Histogram(_minBin, _maxBin, numBins);
        _temporalHistograms[i] = Histogram(_minBin, _maxBin, numBins);
    }

    // All TSP Leaves
    int numOtNodes = _tsp->numOTNodes();
    int otOffset = (pow(8, numOtLevels - 1) - 1) / 7;

    int numBstNodes = _tsp->numBSTNodes();
    int bstOffset = numBstNodes / 2;

    int numberOfLeaves = numOtLeaves * numBstLeaves;

    LINFO("Building spatial histograms");
    ProgressBar pb1(numberOfLeaves);
    int processedLeaves = 0;
    pb1.print(processedLeaves);
    bool success = true;
    for (int bst = bstOffset; bst < numBstNodes; bst++) {
        for (int ot = otOffset; ot < numOtNodes; ot++) {
            success &= buildFromOctreeChild(bst, ot);
            if (!success) { LERROR("Failed in buildFromOctreeChild"); return false; }
            pb1.print(processedLeaves++);
        }
    }

    LINFO("Building temporal histograms");
    ProgressBar pb2(numberOfLeaves);
    processedLeaves = 0;
    pb2.print(processedLeaves);
    for (int ot = otOffset; ot < numOtNodes; ot++) {
        for (int bst = bstOffset; bst < numBstNodes; bst++) {
            success &= buildFromBstChild(bst, ot);
            if (!success) { LERROR("Failed in buildFromBstChild"); return false; }
            pb2.print(processedLeaves++);
        }
    }
    return success;
}

bool LocalErrorHistogramManager::buildFromOctreeChild(unsigned int bstOffset, unsigned int octreeOffset) {
    // Add errors to octree parent histogram
    int numOtNodes = _tsp->numOTNodes();
    unsigned int childIndex = bstOffset * numOtNodes + octreeOffset;
    bool isOctreeLeaf = _tsp->isOctreeLeaf(childIndex);

    if (octreeOffset > 0) {
        // Not octree root
        std::vector<float> childValues;
        std::vector<float> parentValues;

        int octreeParent = parentOffset(octreeOffset, 8);
        unsigned int parentIndex = bstOffset * numOtNodes + octreeParent;
        unsigned int parentInnerNodeIndex = brickToInnerNodeIndex(parentIndex);

        if (isOctreeLeaf) {
            childValues = readValues(childIndex);
        } else {
            unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
            auto it = _voxelCache.find(childInnerNodeIndex);
            if (it != _voxelCache.end()) {
                childValues = it->second;
            } else {
                LERROR("Child " << childIndex << " visited without cache, " << bstOffset << ", " << octreeOffset);
                return false;
            }
        }

        int octreeChildIndex = (octreeOffset - 1) % 8;
        if (octreeChildIndex == 0) {
            parentValues = readValues(parentIndex);
            _voxelCache[parentInnerNodeIndex] = parentValues;
        } else {
            auto it = _voxelCache.find(parentInnerNodeIndex);
            if (it != _voxelCache.end()) {
                parentValues = it->second;
            } else {
                LERROR("Parent " << parentIndex << " visited without cache");
                return false;
            }
        }

        // Compare values and add errors to parent histogram
        unsigned int paddedBrickDim = _tsp->paddedBrickDim();
        unsigned int brickDim = _tsp->brickDim();
        unsigned int padding = (paddedBrickDim - brickDim) / 2;

        glm::vec3 parentOffset = glm::vec3(octreeChildIndex % 2, (octreeChildIndex / 2) % 2, octreeChildIndex / 4) * float(brickDim) / 2.f;

        for (int z = 0; z < brickDim; z++) {
            for (int y = 0; y < brickDim; y++) {
                for (int x = 0; x < brickDim; x++) {
                    glm::vec3 childSamplePoint = glm::vec3(x, y, z) + glm::vec3(padding);
                    glm::vec3 parentSamplePoint = parentOffset + (glm::vec3(x, y, z) + glm::vec3(0.5)) * 0.5f;
                    float childValue = childValues[linearCoords(childSamplePoint)];
                    float parentValue = interpolate(parentSamplePoint, parentValues);

                    // Divide by number of child voxels that will be taken into account
                    float rectangleHeight = std::abs(childValue - parentValue) / 8.0;
                    _spatialHistograms[parentInnerNodeIndex].addRectangle(childValue, parentValue, rectangleHeight);
                }
            }
        }

        bool isLastOctreeChild = octreeOffset > 0 && octreeChildIndex == 7;
        if (isLastOctreeChild) {
            buildFromOctreeChild(bstOffset, octreeParent);
        }
    }

    if (!isOctreeLeaf) {
        unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
        _voxelCache.erase(childInnerNodeIndex);
    }

    int bstChildIndex = bstOffset % 2;
    bool isLastBstChild = bstOffset > 0 && bstChildIndex == 0;
    if (isOctreeLeaf && isLastBstChild) {
        int bstParent = parentOffset(bstOffset, 2);
        buildFromOctreeChild(bstParent, octreeOffset);
    }

    return true;
}

bool LocalErrorHistogramManager::buildFromBstChild(unsigned int bstOffset, unsigned int octreeOffset) {
    // Add errors to bst parent histogram
    int numOtNodes = _tsp->numOTNodes();
    unsigned int childIndex = bstOffset * numOtNodes + octreeOffset;
    bool isBstLeaf = _tsp->isBstLeaf(childIndex);

    if (bstOffset > 0) {
        // Not BST root
        std::vector<float> childValues;
        std::vector<float> parentValues;

        int bstParent = parentOffset(bstOffset, 2);
        unsigned int parentIndex = bstParent * numOtNodes + octreeOffset;
        unsigned int parentInnerNodeIndex = brickToInnerNodeIndex(parentIndex);

        if (isBstLeaf) {
            childValues = readValues(childIndex);
        } else {
            unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
            auto it = _voxelCache.find(childInnerNodeIndex);
            if (it != _voxelCache.end()) {
                childValues = it->second;
            } else {
                LERROR("Child " << childIndex << " visited without cache");
                return false;
            }
        }

        int bstChildIndex = bstOffset % 2;
        if (bstChildIndex == 1) {
            parentValues = readValues(parentIndex);
            _voxelCache[parentInnerNodeIndex] = parentValues;
        } else {
            auto it = _voxelCache.find(parentInnerNodeIndex);
            if (it != _voxelCache.end()) {
                parentValues = it->second;
            } else {
                LERROR("Parent " << parentIndex << " visited without cache");
                return false;
            }
        }

        // Compare values and add errors to parent histogram
        unsigned int paddedBrickDim = _tsp->paddedBrickDim();
        unsigned int brickDim = _tsp->brickDim();
        unsigned int padding = (paddedBrickDim - brickDim) / 2;

        for (int z = 0; z < brickDim; z++) {
            for (int y = 0; y < brickDim; y++) {
                for (int x = 0; x < brickDim; x++) {
                    glm::vec3 samplePoint = glm::vec3(x, y, z) + glm::vec3(padding);
                    unsigned int linearSamplePoint = linearCoords(samplePoint);
                    float childValue = childValues[linearSamplePoint];
                    float parentValue = parentValues[linearSamplePoint];

                    // Divide by number of child voxels that will be taken into account
                    float rectangleHeight = std::abs(childValue - parentValue) / 2.0;
                    _temporalHistograms[parentInnerNodeIndex].addRectangle(childValue, parentValue, rectangleHeight);
                }
            }
        }

        bool isLastBstChild = bstOffset > 0 && bstChildIndex == 0;
        if (isLastBstChild) {
            buildFromBstChild(bstParent, octreeOffset);
        }
    }

    if (!isBstLeaf) {
        unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
        _voxelCache.erase(childInnerNodeIndex);
    }

    int octreeChildIndex = (octreeOffset - 1) % 8;
    bool isLastOctreeChild = octreeOffset > 0 && octreeChildIndex == 7;
    if (isBstLeaf && isLastOctreeChild) {
        int octreeParent = parentOffset(octreeOffset, 8);
        buildFromBstChild(bstOffset, octreeParent);
    }

    return true;
}

bool LocalErrorHistogramManager::loadFromFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
        return false;
    }

    file.read(reinterpret_cast<char*>(&_numInnerNodes), sizeof(int));
    file.read(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.read(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.read(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    int nFloats = _numInnerNodes * _numBins;
    float* histogramData = new float[nFloats];
    file.read(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);

    _spatialHistograms = std::vector<Histogram>(_numInnerNodes);
    for (int i = 0; i < _numInnerNodes; ++i) {
        int offset = i*_numBins;
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _spatialHistograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    file.read(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);
    _temporalHistograms = std::vector<Histogram>(_numInnerNodes);
    for (int i = 0; i < _numInnerNodes; ++i) {
        int offset = i*_numBins;
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _temporalHistograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    delete[] histogramData;
    // No need to deallocate histogram data, since histograms take ownership.
    file.close();
    return true;
}


bool LocalErrorHistogramManager::saveToFile(const std::string& filename) {
    std::ofstream file(filename, std::ios::out | std::ios::binary);
    if (!file.is_open()) {
        return false;
    }

    file.write(reinterpret_cast<char*>(&_numInnerNodes), sizeof(int));
    file.write(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.write(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.write(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    int nFloats = _numInnerNodes * _numBins;
    float* histogramData = new float[nFloats];

    for (int i = 0; i < _numInnerNodes; ++i) {
        int offset = i*_numBins;
        memcpy(&histogramData[offset], _spatialHistograms[i].data(), sizeof(float) * _numBins);
    }
    file.write(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);

    for (int i = 0; i < _numInnerNodes; ++i) {
        int offset = i*_numBins;
        memcpy(&histogramData[offset], _temporalHistograms[i].data(), sizeof(float) * _numBins);
    }
    file.write(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);

    delete[] histogramData;

    file.close();
    return true;
}

const Histogram* LocalErrorHistogramManager::getSpatialHistogram(unsigned int brickIndex) const {
    unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_spatialHistograms[innerNodeIndex]);
    } else {
        return nullptr;
    }
}

const Histogram* LocalErrorHistogramManager::getTemporalHistogram(unsigned int brickIndex) const {
    unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_temporalHistograms[innerNodeIndex]);
    } else {
        return nullptr;
    }
}

const char * LocalErrorHistogramManager::getName() const { return _name; }
} // namespace openspace

