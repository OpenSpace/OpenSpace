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

#include <modules/multiresvolume/rendering/errorhistogrammanager.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <openspace/util/progressbar.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

namespace openspace {

ErrorHistogramManager::ErrorHistogramManager(TSP* tsp) : _tsp(tsp) {}

bool ErrorHistogramManager::buildHistograms(int numBins) {
    _numBins = numBins;

    _file = &(_tsp->file());
    if (!_file->is_open()) {
        return false;
    }
    _minBin = 0.f; // Should be calculated from tsp file
    _maxBin = 1.f; // Should be calculated from tsp file as (maxValue - minValue)

    unsigned int numOtLevels = _tsp->numOTLevels();
    unsigned int numOtLeaves = static_cast<unsigned int>(pow(8, numOtLevels - 1));
    unsigned int numBstLeaves = static_cast<unsigned int>(
        pow(2, _tsp->numBSTLevels() - 1)
    );

    _numInnerNodes = _tsp->numTotalNodes() - numOtLeaves * numBstLeaves;
    _histograms = std::vector<Histogram>(_numInnerNodes);
    LINFOC(
        "ErrorHistogramManager",
        std::format("Build {} histograms with {} bins each", _numInnerNodes, numBins)
    );

    // All TSP Leaves
    int numOtNodes = _tsp->numOTNodes();
    int otOffset = static_cast<int>((pow(8, numOtLevels - 1) - 1) / 7);

    int numBstNodes = _tsp->numBSTNodes();
    int bstOffset = numBstNodes / 2;

    int numberOfLeaves = (numBstNodes - bstOffset) * (numOtNodes - otOffset);
    ProgressBar pb(numberOfLeaves);
    int processedLeaves = 0;
    bool success = true;
    for (int bst = bstOffset; bst < numBstNodes; bst++) {
        for (int ot = otOffset; ot < numOtNodes; ot++) {
            success &= buildFromLeaf(bst, ot);
            if (!success) {
                return false;
            }
            pb.print(++processedLeaves);
        }
    }

    return success;
}

bool ErrorHistogramManager::buildFromLeaf(unsigned int bstOffset,
                                          unsigned int octreeOffset)
{
    // Traverse all ancestors of leaf and add errors to their histograms

    unsigned int brickDim = _tsp->brickDim();
    unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    unsigned int padding = (paddedBrickDim - brickDim) / 2;

    int numOtNodes = _tsp->numOTNodes();
    unsigned int leafIndex = bstOffset * numOtNodes + octreeOffset;
    std::vector<float> leafValues = readValues(leafIndex);
//    int numVoxels = leafValues.size();

    int bstNode = bstOffset;
    bool bstRightOnly = true;
    unsigned int bstLevel = 0;

    do {
        glm::vec3 leafOffset(0.f); // Leaf offset in leaf sized voxels
        unsigned int octreeLevel = 0;
        unsigned int octreeNode = octreeOffset;
        bool octreeLastOnly = true;
        do {
            // Visit ancestor
            if (bstNode != static_cast<int>(bstOffset) || octreeNode != octreeOffset) {
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
                }
                else {
                    ancestorVoxels = it->second;
                }

                float voxelScale = static_cast<float>(pow(2.f, octreeLevel));
                float invVoxelScale = 1.f / voxelScale;

                // Calculate leaf offset in ancestor sized voxels
                glm::vec3 ancestorOffset = (leafOffset * invVoxelScale) +
                                           glm::vec3(padding - 0.5f);

                for (int z = 0; z < static_cast<int>(brickDim); z++) {
                    for (int y = 0; y < static_cast<int>(brickDim); y++) {
                        for (int x = 0; x < static_cast<int>(brickDim); x++) {
                            glm::vec3 leafSamplePoint = glm::vec3(x, y, z) +
                                                   glm::vec3(static_cast<float>(padding));
                            glm::vec3 ancestorSamplePoint = ancestorOffset +
                                (glm::vec3(x, y, z) + glm::vec3(0.5)) * invVoxelScale;
                            float leafValue = leafValues[linearCoords(leafSamplePoint)];
                            float ancestorValue = interpolate(
                                ancestorSamplePoint,
                                ancestorVoxels
                            );

                            _histograms[innerNodeIndex].addRectangle(
                                leafValue,
                                ancestorValue,
                                std::abs(leafValue - ancestorValue)
                            );
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

            int childSize = static_cast<int>(pow(2, octreeLevel) * brickDim);
            leafOffset.x += (octreeChild % 2) * childSize;
            leafOffset.y += ((octreeChild / 2) % 2) * childSize;
            leafOffset.z += (octreeChild / 4) * childSize;

            octreeLevel++;
        // @TODO(emiax):  This does not make sense? unsigned int check against -1
        } while (octreeNode != -1);

        bstRightOnly &= (bstNode % 2 == 0);
        bstNode = parentOffset(bstNode, 2);

        bstLevel++;
    } while (bstNode != -1);

    return true;
}

bool ErrorHistogramManager::loadFromFile(const std::filesystem::path& filename) {
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

    for (int i = 0; i < static_cast<int>(_numInnerNodes); i++) {
        int offset = i * _numBins;
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _histograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    delete[] histogramData;
    // No need to deallocate histogram data, since histograms take ownership.
    file.close();
    return true;
}


bool ErrorHistogramManager::saveToFile(const std::filesystem::path& filename) {
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

    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        int offset = i * _numBins;
        memcpy(&histogramData[offset], _histograms[i].data(), sizeof(float) * _numBins);
    }

    file.write(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);
    delete[] histogramData;

    file.close();
    return true;
}

unsigned int ErrorHistogramManager::linearCoords(const glm::vec3& coords) const {
    return linearCoords(glm::ivec3(coords));
}

unsigned int ErrorHistogramManager::linearCoords(int x, int y, int z) const {
    return linearCoords(glm::ivec3(x, y, z));
}

unsigned int ErrorHistogramManager::linearCoords(const glm::ivec3& coords) const {
    const unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    return coords.z * paddedBrickDim * paddedBrickDim +
           coords.y * paddedBrickDim + coords.x;
}

float ErrorHistogramManager::interpolate(const glm::vec3& samplePoint,
                                         const std::vector<float>& voxels) const
{
    const int lowX = static_cast<int>(samplePoint.x);
    const int lowY = static_cast<int>(samplePoint.y);
    const int lowZ = static_cast<int>(samplePoint.z);

    const int highX = static_cast<int>(ceil(samplePoint.x));
    const int highY = static_cast<int>(ceil(samplePoint.y));
    const int highZ = static_cast<int>(ceil(samplePoint.z));

    const float interpolatorX = 1.f - (samplePoint.x - lowX);
    const float interpolatorY = 1.f - (samplePoint.y - lowY);
    const float interpolatorZ = 1.f - (samplePoint.z - lowZ);

    const float v000 = voxels[linearCoords(lowX, lowY, lowZ)];
    const float v001 = voxels[linearCoords(lowX, lowY, highZ)];
    const float v010 = voxels[linearCoords(lowX, highY, lowZ)];
    const float v011 = voxels[linearCoords(lowX, highY, highZ)];
    const float v100 = voxels[linearCoords(highX, lowY, lowZ)];
    const float v101 = voxels[linearCoords(highX, lowY, highZ)];
    const float v110 = voxels[linearCoords(highX, highY, lowZ)];
    const float v111 = voxels[linearCoords(highX, highY, highZ)];

    const float v00 = interpolatorZ * v000 + (1.f - interpolatorZ) * v001;
    const float v01 = interpolatorZ * v010 + (1.f - interpolatorZ) * v011;
    const float v10 = interpolatorZ * v100 + (1.f - interpolatorZ) * v101;
    const float v11 = interpolatorZ * v110 + (1.f - interpolatorZ) * v111;

    const float v0 = interpolatorY * v00 + (1.f - interpolatorY) * v01;
    const float v1 = interpolatorY * v10 + (1.f - interpolatorY) * v11;

    return interpolatorX * v0 + (1.f - interpolatorX) * v1;

}

const Histogram* ErrorHistogramManager::histogram(unsigned int brickIndex) const {
    const unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_histograms[innerNodeIndex]);
    }
    else {
        return nullptr;
    }
}

int ErrorHistogramManager::parentOffset(int offset, int base) const {
    if (offset == 0) {
        return -1;
    }
    const int depth = static_cast<int>(
        floor(log1p(((base - 1) * offset)) / log(base))
    );
    const int firstInLevel = static_cast<int>((pow(base, depth) - 1) / (base - 1));
    const int inLevelOffset = offset - firstInLevel;

    const int parentDepth = depth - 1;
    const int firstInParentLevel = static_cast<int>(
        (pow(base, parentDepth) - 1) / (base - 1)
    );
    const int parentInLevelOffset = inLevelOffset / base;

    const int parentOffset = firstInParentLevel + parentInLevelOffset;
    return parentOffset;
}

std::vector<float> ErrorHistogramManager::readValues(unsigned int brickIndex) const {
    const unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    const unsigned int numBrickVals = paddedBrickDim * paddedBrickDim * paddedBrickDim;
    std::vector<float> voxelValues(numBrickVals);

    std::streampos offset = _tsp->dataPosition() +
        static_cast<long long>(brickIndex*numBrickVals*sizeof(float));
    _file->seekg(offset);

    _file->read(
        reinterpret_cast<char*>(voxelValues.data()),
        static_cast<size_t>(numBrickVals)*sizeof(float)
    );

    return voxelValues;
}

unsigned int ErrorHistogramManager::brickToInnerNodeIndex(unsigned int brickIndex) const {
    const unsigned int numOtNodes = _tsp->numOTNodes();
    const unsigned int numBstLevels = _tsp->numBSTLevels();

    const unsigned int numInnerBstNodes = static_cast<int>(
        (pow(2, numBstLevels - 1) - 1) * numOtNodes
    );
    if (brickIndex < numInnerBstNodes) {
        return brickIndex;
    }

    const unsigned int numOtLeaves = static_cast<unsigned int>(
        pow(8, _tsp->numOTLevels() - 1)
    );
    const unsigned int numOtInnerNodes = (numOtNodes - numOtLeaves);

    const unsigned int innerBstOffset = brickIndex - numInnerBstNodes;
    const unsigned int rowIndex = innerBstOffset / numOtNodes;
    const unsigned int indexInRow = innerBstOffset % numOtNodes;

    if (indexInRow >= numOtInnerNodes) {
        return std::numeric_limits<unsigned int>::max();
    }

    const unsigned int offset = rowIndex * numOtInnerNodes;
    const unsigned int leavesOffset = offset + indexInRow;

    return numInnerBstNodes + leavesOffset;
}

unsigned int ErrorHistogramManager::innerNodeToBrickIndex(
                                                        unsigned int innerNodeIndex) const
{
    if (innerNodeIndex >= _numInnerNodes) {
        return std::numeric_limits<unsigned int>::max(); // Not an inner node
    }

    const unsigned int numOtNodes = _tsp->numOTNodes();
    const unsigned int numBstLevels = _tsp->numBSTLevels();

    const unsigned int numInnerBstNodes = static_cast<unsigned int>(
        (pow(2, numBstLevels - 1) - 1) * numOtNodes
    );
    if (innerNodeIndex < numInnerBstNodes) {
        return innerNodeIndex;
    }

    const unsigned int numOtLeaves = static_cast<unsigned int>(
        pow(8, _tsp->numOTLevels() - 1)
    );
    const unsigned int numOtInnerNodes = (numOtNodes - numOtLeaves);

    const unsigned int innerBstOffset = innerNodeIndex - numInnerBstNodes;
    const unsigned int rowIndex = innerBstOffset / numOtInnerNodes;
    const unsigned int indexInRow = innerBstOffset % numOtInnerNodes;

    const unsigned int offset = rowIndex * numOtNodes;
    const unsigned int leavesOffset = offset + indexInRow;

    return numInnerBstNodes + leavesOffset;
}

} // namespace openspace

