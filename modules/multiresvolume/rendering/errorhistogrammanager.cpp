/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2015                                                                    *
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

#include <modules/multiresvolume/rendering/errorhistogrammanager.h>
#include <openspace/util/histogram.h>

#include <openspace/util/progressbar.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "ErrorHistogramManager";
}

namespace openspace {

ErrorHistogramManager::ErrorHistogramManager(TSP* tsp) : _tsp(tsp) {}

ErrorHistogramManager::~ErrorHistogramManager() {}

bool ErrorHistogramManager::buildHistograms(int numBins) {
    _numBins = numBins;

    _file = &(_tsp->file());
    if (!_file->is_open()) {
        return false;
    }
    _minBin = 0.0; // Should be calculated from tsp file
    _maxBin = 1.0; // Should be calculated from tsp file as (maxValue - minValue)

    unsigned int numOtLevels = _tsp->numOTLevels();
    unsigned int numOtLeaves = pow(8, numOtLevels - 1);
    unsigned int numBstLeaves = pow(2, _tsp->numBSTLevels() - 1);

    _numInnerNodes = _tsp->numTotalNodes() - numOtLeaves * numBstLeaves;
    _histograms = std::vector<Histogram>(_numInnerNodes);
    LINFO("Build " << _numInnerNodes << " histograms with " << numBins << " bins each");

    // All TSP Leaves
    int numOtNodes = _tsp->numOTNodes();
    int otOffset = (pow(8, numOtLevels - 1) - 1) / 7;

    int numBstNodes = _tsp->numBSTNodes();
    int bstOffset = numBstNodes / 2;

    int numberOfLeaves = (numBstNodes - bstOffset) * (numOtNodes - otOffset);
    ProgressBar pb(numberOfLeaves);
    int processedLeaves = 0;
    bool success = true;
    for (int bst = bstOffset; bst < numBstNodes; bst++) {
        for (int ot = otOffset; ot < numOtNodes; ot++) {
            success &= buildFromLeaf(bst, ot);
            if (!success) return false;
            pb.print(++processedLeaves);
        }
    }
    
    return success;
}

bool ErrorHistogramManager::buildFromLeaf(unsigned int bstOffset, unsigned int octreeOffset) {
    // Traverse all ancestors of leaf and add errors to their histograms

    unsigned int brickDim = _tsp->brickDim();
    unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    unsigned int padding = (paddedBrickDim - brickDim) / 2;

    int numOtNodes = _tsp->numOTNodes();
    unsigned int leafIndex = bstOffset * numOtNodes + octreeOffset;
    std::vector<float> leafValues = readValues(leafIndex);
    int numVoxels = leafValues.size();

    int bstNode = bstOffset;
    bool bstRightOnly = true;
    unsigned int bstLevel = 0;

    do {

        glm::vec3 leafOffset(0.0); // Leaf offset in leaf sized voxels
        unsigned int octreeLevel = 0;
        unsigned int octreeNode = octreeOffset;
        bool octreeLastOnly = true;
        do {
            // Visit ancestor
            if (bstNode != bstOffset || octreeNode != octreeOffset) {
                // Is actually an ancestor

                std::vector<float> ancestorVoxels;
                unsigned int ancestorBrickIndex = bstNode * numOtNodes + octreeNode;
                unsigned int innerNodeIndex = brickToInnerNodeIndex(ancestorBrickIndex);
                auto it = _voxelCache.find(innerNodeIndex);
                if (it == _voxelCache.end()) {
                    // First visit
                    _histograms[innerNodeIndex] = Histogram(_minBin, _maxBin, _numBins);
                    ancestorVoxels = readValues(ancestorBrickIndex);
                    _voxelCache[innerNodeIndex] = ancestorVoxels;
                } else {
                    ancestorVoxels = it->second;
                }

                float voxelScale = pow(2, octreeLevel);
                float invVoxelScale = 1.0 / voxelScale;

                // Calculate leaf offset in ancestor sized voxels
                glm::vec3 ancestorOffset = (leafOffset * invVoxelScale) + glm::vec3(padding - 0.5);

                for (int z = 0; z < brickDim; z++) {
                    for (int y = 0; y < brickDim; y++) {
                        for (int x = 0; x < brickDim; x++) {
                            glm::vec3 leafSamplePoint = glm::vec3(x, y, z) + glm::vec3(padding);
                            glm::vec3 ancestorSamplePoint = ancestorOffset + (glm::vec3(x, y, z) + glm::vec3(0.5)) * invVoxelScale;
                            float leafValue = leafValues[linearCoords(leafSamplePoint)];
                            float ancestorValue = interpolate(ancestorSamplePoint, ancestorVoxels);

                            _histograms[innerNodeIndex].addRectangle(leafValue, ancestorValue, std::abs(leafValue - ancestorValue));
                        }
                    }
                }

                if (bstRightOnly && octreeLastOnly) {
                    _voxelCache.erase(innerNodeIndex);
                }
            }

            // Traverse to next octree ancestor
            int octreeChild = (octreeNode - 1) % 8;
            octreeLastOnly &= octreeChild == 7;
            octreeNode = parentOffset(octreeNode, 8);

            int childSize = pow(2, octreeLevel) * brickDim;
            leafOffset.x += (octreeChild % 2) * childSize;
            leafOffset.y += ((octreeChild / 2) % 2) * childSize;
            leafOffset.z += (octreeChild / 4) * childSize;

            octreeLevel++;
        } while (octreeNode != -1);

        bstRightOnly &= (bstNode % 2 == 0);
        bstNode = parentOffset(bstNode, 2);

        bstLevel++;
    } while (bstNode != -1);

    return true;
}

bool ErrorHistogramManager::loadFromFile(const std::string& filename) {
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

    _histograms = std::vector<Histogram>(_numInnerNodes);

    for (int i = 0; i < _numInnerNodes; ++i) {
        int offset = i*_numBins;
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _histograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    delete[] histogramData;
    // No need to deallocate histogram data, since histograms take ownership.
    file.close();
    return true;
}


bool ErrorHistogramManager::saveToFile(const std::string& filename) {
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
        memcpy(&histogramData[offset], _histograms[i].data(), sizeof(float) * _numBins);
    }

    file.write(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);
    delete[] histogramData;

    file.close();
    return true;
}

unsigned int ErrorHistogramManager::linearCoords(glm::vec3 coords) const {
    return linearCoords(glm::ivec3(coords));
}

unsigned int ErrorHistogramManager::linearCoords(int x, int y, int z) const {
    return linearCoords(glm::ivec3(x, y, z));
}

unsigned int ErrorHistogramManager::linearCoords(glm::ivec3 coords) const {
    unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    return coords.z * paddedBrickDim * paddedBrickDim + coords.y * paddedBrickDim + coords.x;
}

float ErrorHistogramManager::interpolate(glm::vec3 samplePoint, const std::vector<float>& voxels) const {
    int lowX = samplePoint.x;
    int lowY = samplePoint.y;
    int lowZ = samplePoint.z;

    int highX = ceil(samplePoint.x);
    int highY = ceil(samplePoint.y);
    int highZ = ceil(samplePoint.z);

    float interpolatorX = 1.0 - (samplePoint.x - lowX);
    float interpolatorY = 1.0 - (samplePoint.y - lowY);
    float interpolatorZ = 1.0 - (samplePoint.z - lowZ);

    float v000 = voxels[linearCoords(lowX, lowY, lowZ)];
    float v001 = voxels[linearCoords(lowX, lowY, highZ)];
    float v010 = voxels[linearCoords(lowX, highY, lowZ)];
    float v011 = voxels[linearCoords(lowX, highY, highZ)];
    float v100 = voxels[linearCoords(highX, lowY, lowZ)];
    float v101 = voxels[linearCoords(highX, lowY, highZ)];
    float v110 = voxels[linearCoords(highX, highY, lowZ)];
    float v111 = voxels[linearCoords(highX, highY, highZ)];

    float v00 = interpolatorZ * v000 + (1.0 - interpolatorZ) * v001;
    float v01 = interpolatorZ * v010 + (1.0 - interpolatorZ) * v011;
    float v10 = interpolatorZ * v100 + (1.0 - interpolatorZ) * v101;
    float v11 = interpolatorZ * v110 + (1.0 - interpolatorZ) * v111;

    float v0 = interpolatorY * v00 + (1.0 - interpolatorY) * v01;
    float v1 = interpolatorY * v10 + (1.0 - interpolatorY) * v11;

    return interpolatorX * v0 + (1.0 - interpolatorX) * v1;

}

const Histogram* ErrorHistogramManager::getHistogram(unsigned int brickIndex) const {
    unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_histograms[innerNodeIndex]);
    } else {
        return nullptr;
    }
}

int ErrorHistogramManager::parentOffset(int offset, int base) const {
    if (offset == 0) {
        return -1;
    }
    int depth = floor(log(((base - 1) * offset + 1.0)) / log(base));
    int firstInLevel = (pow(base, depth) - 1) / (base - 1);
    int inLevelOffset = offset - firstInLevel;

    int parentDepth = depth - 1;
    int firstInParentLevel = (pow(base, parentDepth) - 1) / (base - 1);
    int parentInLevelOffset = inLevelOffset / base;

    int parentOffset = firstInParentLevel + parentInLevelOffset;
    return parentOffset;
}

std::vector<float> ErrorHistogramManager::readValues(unsigned int brickIndex) const {
    unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    unsigned int numBrickVals = paddedBrickDim * paddedBrickDim * paddedBrickDim;
    std::vector<float> voxelValues(numBrickVals);

    std::streampos offset = _tsp->dataPosition() + static_cast<long long>(brickIndex*numBrickVals*sizeof(float));
    _file->seekg(offset);

    _file->read(reinterpret_cast<char*>(&voxelValues[0]),
        static_cast<size_t>(numBrickVals)*sizeof(float));

    return voxelValues;
}

unsigned int ErrorHistogramManager::brickToInnerNodeIndex(unsigned int brickIndex) const {
    unsigned int numOtNodes = _tsp->numOTNodes();
    unsigned int numBstLevels = _tsp->numBSTLevels();

    unsigned int numInnerBstNodes = (pow(2, numBstLevels - 1) - 1) * numOtNodes;
    if (brickIndex < numInnerBstNodes) return brickIndex;

    unsigned int numOtLeaves = pow(8, _tsp->numOTLevels() - 1);
    unsigned int numOtInnerNodes = (numOtNodes - numOtLeaves);

    unsigned int innerBstOffset = brickIndex - numInnerBstNodes;
    unsigned int rowIndex = innerBstOffset / numOtNodes;
    unsigned int indexInRow = innerBstOffset % numOtNodes;

    if (indexInRow >= numOtInnerNodes) return -1;

    unsigned int offset = rowIndex * numOtInnerNodes;
    unsigned int leavesOffset = offset + indexInRow;

    return numInnerBstNodes + leavesOffset;
}

unsigned int ErrorHistogramManager::innerNodeToBrickIndex(unsigned int innerNodeIndex) const {
    if (innerNodeIndex < 0 || innerNodeIndex >= _numInnerNodes) return -1; // Not an inner node

    unsigned int numOtNodes = _tsp->numOTNodes();
    unsigned int numBstLevels = _tsp->numBSTLevels();

    unsigned int numInnerBstNodes = (pow(2, numBstLevels - 1) - 1) * numOtNodes;
    if (innerNodeIndex < numInnerBstNodes) return innerNodeIndex;

    unsigned int numOtLeaves = pow(8, _tsp->numOTLevels() - 1);
    unsigned int numOtInnerNodes = (numOtNodes - numOtLeaves);

    unsigned int innerBstOffset = innerNodeIndex - numInnerBstNodes;
    unsigned int rowIndex = innerBstOffset / numOtInnerNodes;
    unsigned int indexInRow = innerBstOffset % numOtInnerNodes;

    unsigned int offset = rowIndex * numOtNodes;
    unsigned int leavesOffset = offset + indexInRow;

    return numInnerBstNodes + leavesOffset;
}

} // namespace openspace

