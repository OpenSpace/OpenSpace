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

#include <modules/multiresvolume/rendering/localerrorhistogrammanager.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <openspace/util/progressbar.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

namespace {
    constexpr std::string_view _loggerCat = "LocalErrorHistogramManager";
} // namespace

namespace openspace {

LocalErrorHistogramManager::LocalErrorHistogramManager(TSP* tsp) : _tsp(tsp) {}

bool LocalErrorHistogramManager::buildHistograms(int numBins) {
    LINFO(std::format("Build histograms with {} bins each", numBins));
    _numBins = numBins;

    _file = &(_tsp->file());
    if (!_file->is_open()) {
        return false;
    }
    _minBin = 0.f; // Should be calculated from tsp file
    _maxBin = 1.f; // Should be calculated from tsp file as (maxValue - minValue)

    const unsigned int numOtLevels = _tsp->numOTLevels();
    const unsigned int numOtLeaves = static_cast<unsigned int>(pow(8, numOtLevels - 1));
    const unsigned int numBstLeaves = static_cast<unsigned int>(
        pow(2, _tsp->numBSTLevels() - 1)
    );

    _numInnerNodes = _tsp->numTotalNodes() - numOtLeaves * numBstLeaves;

    _spatialHistograms = std::vector<Histogram>(_numInnerNodes);
    _temporalHistograms = std::vector<Histogram>(_numInnerNodes);
    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        _spatialHistograms[i] = Histogram(_minBin, _maxBin, numBins);
        _temporalHistograms[i] = Histogram(_minBin, _maxBin, numBins);
    }

    // All TSP Leaves
    const int numOtNodes = _tsp->numOTNodes();
    const int otOffset = static_cast<int>((pow(8, numOtLevels - 1) - 1) / 7);

    const int numBstNodes = _tsp->numBSTNodes();
    const int bstOffset = numBstNodes / 2;

    const int numberOfLeaves = numOtLeaves * numBstLeaves;

    LINFO("Building spatial histograms");
    ProgressBar pb1(numberOfLeaves);
    int processedLeaves = 0;
    pb1.print(processedLeaves);
    bool success = true;
    for (int bst = bstOffset; bst < numBstNodes; bst++) {
        for (int ot = otOffset; ot < numOtNodes; ot++) {
            success &= buildFromOctreeChild(bst, ot);
            if (!success) {
                LERROR("Failed in buildFromOctreeChild");
                return false;
            }
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
            if (!success) {
                LERROR("Failed in buildFromBstChild");
                return false;
            }
            pb2.print(processedLeaves++);
        }
    }

    return success;
}

bool LocalErrorHistogramManager::buildFromOctreeChild(unsigned int bstOffset,
                                                      unsigned int octreeOffset)
{
    // Add errors to octree parent histogram
    const int numOtNodes = _tsp->numOTNodes();
    const unsigned int childIndex = bstOffset * numOtNodes + octreeOffset;
    const bool isOctreeLeaf = _tsp->isOctreeLeaf(childIndex);

    if (octreeOffset > 0) {
        // Not octree root
        std::vector<float> childValues;
        std::vector<float> parentValues;

        const int octreeParent = parentOffset(octreeOffset, 8);
        const unsigned int parentIndex = bstOffset * numOtNodes + octreeParent;
        const unsigned int parentInnerNodeIndex = brickToInnerNodeIndex(parentIndex);

        if (isOctreeLeaf) {
            childValues = readValues(childIndex);
        }
        else {
            const unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
            auto it = _voxelCache.find(childInnerNodeIndex);
            if (it != _voxelCache.end()) {
                childValues = it->second;
            }
            else {
                LERROR(std::format(
                    "Child {} visited without cache, {}, {}",
                    childIndex, bstOffset, octreeOffset
                ));
                return false;
            }
        }

        const int octreeChildIndex = (octreeOffset - 1) % 8;
        if (octreeChildIndex == 0) {
            parentValues = readValues(parentIndex);
            _voxelCache[parentInnerNodeIndex] = parentValues;
        }
        else {
            auto it = _voxelCache.find(parentInnerNodeIndex);
            if (it != _voxelCache.end()) {
                parentValues = it->second;
            }
            else {
                LERROR(std::format("Parent {} visited without cache", parentIndex));
                return false;
            }
        }

        // Compare values and add errors to parent histogram
        const unsigned int paddedBrickDim = _tsp->paddedBrickDim();
        const int brickDim = static_cast<int>(_tsp->brickDim());
        const unsigned int padding = (paddedBrickDim - brickDim) / 2;

        glm::vec3 parentOffset = glm::vec3(
            octreeChildIndex % 2,
            (octreeChildIndex / 2) % 2,
            octreeChildIndex / 4
        ) * (brickDim / 2.f);

        for (int z = 0; z < brickDim; z++) {
            for (int y = 0; y < brickDim; y++) {
                for (int x = 0; x < brickDim; x++) {
                    glm::ivec3 childSamplePoint = glm::ivec3(x, y, z) +
                                                  glm::ivec3(padding);
                    glm::vec3 parentSamplePoint = parentOffset +
                                           glm::vec3(x + 0.5f, y + 0.5f, z + 0.5f) * 0.5f;
                    float childValue = childValues[linearCoords(childSamplePoint)];
                    float parentValue = interpolate(parentSamplePoint, parentValues);

                    // Divide by number of child voxels that will be taken into account
                    float rectangleHeight = std::abs(childValue - parentValue) / 8.f;
                    _spatialHistograms[parentInnerNodeIndex].addRectangle(
                        childValue,
                        parentValue,
                        rectangleHeight
                    );
                }
            }
        }

        const bool isLastOctreeChild = octreeOffset > 0 && octreeChildIndex == 7;
        if (isLastOctreeChild) {
            buildFromOctreeChild(bstOffset, octreeParent);
        }
    }

    if (!isOctreeLeaf) {
        const unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
        _voxelCache.erase(childInnerNodeIndex);
    }

    const int bstChildIndex = bstOffset % 2;
    const bool isLastBstChild = bstOffset > 0 && bstChildIndex == 0;
    if (isOctreeLeaf && isLastBstChild) {
        const int bstParent = parentOffset(bstOffset, 2);
        buildFromOctreeChild(bstParent, octreeOffset);
    }

    return true;
}

bool LocalErrorHistogramManager::buildFromBstChild(unsigned int bstOffset,
                                                   unsigned int octreeOffset)
{
    // Add errors to bst parent histogram
    const int numOtNodes = _tsp->numOTNodes();
    const unsigned int childIndex = bstOffset * numOtNodes + octreeOffset;
    const bool isBstLeaf = _tsp->isBstLeaf(childIndex);

    if (bstOffset > 0) {
        // Not BST root
        std::vector<float> childValues;
        std::vector<float> parentValues;

        const int bstParent = parentOffset(bstOffset, 2);
        const unsigned int parentIndex = bstParent * numOtNodes + octreeOffset;
        const unsigned int parentInnerNodeIndex = brickToInnerNodeIndex(parentIndex);

        if (isBstLeaf) {
            childValues = readValues(childIndex);
        }
        else {
            unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
            auto it = _voxelCache.find(childInnerNodeIndex);
            if (it != _voxelCache.end()) {
                childValues = it->second;
            }
            else {
                LERROR(std::format("Child {} visited without cache", childIndex));
                return false;
            }
        }

        const int bstChildIndex = bstOffset % 2;
        if (bstChildIndex == 1) {
            parentValues = readValues(parentIndex);
            _voxelCache[parentInnerNodeIndex] = parentValues;
        }
        else {
            auto it = _voxelCache.find(parentInnerNodeIndex);
            if (it != _voxelCache.end()) {
                parentValues = it->second;
            }
            else {
                LERROR(std::format("Parent {} visited without cache", parentIndex));
                return false;
            }
        }

        // Compare values and add errors to parent histogram
        const unsigned int paddedBrickDim = _tsp->paddedBrickDim();
        const int brickDim = static_cast<int>(_tsp->brickDim());
        const unsigned int padding = (paddedBrickDim - brickDim) / 2;

        for (int z = 0; z < brickDim; z++) {
            for (int y = 0; y < brickDim; y++) {
                for (int x = 0; x < brickDim; x++) {
                    glm::ivec3 samplePoint = glm::ivec3(x, y, z) + glm::ivec3(padding);
                    unsigned int linearSamplePoint = linearCoords(samplePoint);
                    float childValue = childValues[linearSamplePoint];
                    float parentValue = parentValues[linearSamplePoint];

                    // Divide by number of child voxels that will be taken into account
                    float rectangleHeight = std::abs(childValue - parentValue) / 2.f;
                    _temporalHistograms[parentInnerNodeIndex].addRectangle(
                        childValue,
                        parentValue,
                        rectangleHeight
                    );
                }
            }
        }

        const bool isLastBstChild = bstOffset > 0 && bstChildIndex == 0;
        if (isLastBstChild) {
            buildFromBstChild(bstParent, octreeOffset);
        }
    }

    if (!isBstLeaf) {
        const unsigned int childInnerNodeIndex = brickToInnerNodeIndex(childIndex);
        _voxelCache.erase(childInnerNodeIndex);
    }

    const int octreeChildIndex = (octreeOffset - 1) % 8;
    const bool isLastOctreeChild = octreeOffset > 0 && octreeChildIndex == 7;
    if (isBstLeaf && isLastOctreeChild) {
        const int octreeParent = parentOffset(octreeOffset, 8);
        buildFromBstChild(bstOffset, octreeParent);
    }

    return true;
}

bool LocalErrorHistogramManager::loadFromFile(const std::filesystem::path& filename) {
    std::ifstream file(filename, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
        return false;
    }

    file.read(reinterpret_cast<char*>(&_numInnerNodes), sizeof(int));
    file.read(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.read(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.read(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    const int nFloats = _numInnerNodes * _numBins;
    std::vector<float> histogramData(nFloats);

    file.read(reinterpret_cast<char*>(histogramData.data()), sizeof(float) * nFloats);
    _spatialHistograms = std::vector<Histogram>(_numInnerNodes);
    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        const int offset = i * _numBins;
        // No need to deallocate histogram data, since histograms take ownership.
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _spatialHistograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    file.read(reinterpret_cast<char*>(histogramData.data()), sizeof(float) * nFloats);
    _temporalHistograms = std::vector<Histogram>(_numInnerNodes);
    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        const int offset = i * _numBins;
        // No need to deallocate histogram data, since histograms take ownership.
        float* data = new float[_numBins];
        memcpy(data, &histogramData[offset], sizeof(float) * _numBins);
        _temporalHistograms[i] = Histogram(_minBin, _maxBin, _numBins, data);
    }

    file.close();
    return true;
}


bool LocalErrorHistogramManager::saveToFile(const std::filesystem::path& filename) {
    std::ofstream file(filename, std::ios::out | std::ios::binary);
    if (!file.is_open()) {
        return false;
    }

    file.write(reinterpret_cast<char*>(&_numInnerNodes), sizeof(int));
    file.write(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.write(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.write(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    const int nFloats = _numInnerNodes * _numBins;
    std::vector<float> histogramData(nFloats);

    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        int offset = i * _numBins;
        memcpy(
            &histogramData[offset],
            _spatialHistograms[i].data(),
            sizeof(float) * _numBins
        );
    }
    file.write(reinterpret_cast<char*>(histogramData.data()), sizeof(float) * nFloats);

    for (unsigned int i = 0; i < _numInnerNodes; i++) {
        int offset = i * _numBins;
        memcpy(
            &histogramData[offset],
            _temporalHistograms[i].data(),
            sizeof(float) * _numBins
        );
    }
    file.write(reinterpret_cast<char*>(histogramData.data()), sizeof(float) * nFloats);

    file.close();
    return true;
}

unsigned int LocalErrorHistogramManager::linearCoords(glm::vec3 coords) const {
    return linearCoords(glm::ivec3(coords));
}

unsigned int LocalErrorHistogramManager::linearCoords(int x, int y, int z) const {
    return linearCoords(glm::ivec3(x, y, z));
}

unsigned int LocalErrorHistogramManager::linearCoords(glm::ivec3 coords) const {
    unsigned int paddedBrickDim = _tsp->paddedBrickDim();
    return coords.z * paddedBrickDim * paddedBrickDim + coords.y * paddedBrickDim +
           coords.x;
}

float LocalErrorHistogramManager::interpolate(glm::vec3 samplePoint,
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

const Histogram* LocalErrorHistogramManager::spatialHistogram(
                                                            unsigned int brickIndex) const
{
    const unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_spatialHistograms[innerNodeIndex]);
    }
    else {
        return nullptr;
    }
}

const Histogram* LocalErrorHistogramManager::temporalHistogram(
                                                            unsigned int brickIndex) const
{
    const unsigned int innerNodeIndex = brickToInnerNodeIndex(brickIndex);
    if (innerNodeIndex < _numInnerNodes) {
        return &(_temporalHistograms[innerNodeIndex]);
    }
    else {
        return nullptr;
    }
}

int LocalErrorHistogramManager::parentOffset(int offset, int base) const {
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

std::vector<float> LocalErrorHistogramManager::readValues(unsigned int brickIndex) const {
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

unsigned int LocalErrorHistogramManager::brickToInnerNodeIndex(
                                                            unsigned int brickIndex) const
{
    const unsigned int numOtNodes = _tsp->numOTNodes();
    const unsigned int numBstLevels = _tsp->numBSTLevels();
    const unsigned int numInnerBstNodes = static_cast<unsigned int>(
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
        return _numInnerNodes;
    }

    const unsigned int offset = rowIndex * numOtInnerNodes;
    const unsigned int leavesOffset = offset + indexInRow;

    return numInnerBstNodes + leavesOffset;
}

unsigned int LocalErrorHistogramManager::innerNodeToBrickIndex(
                                                        unsigned int innerNodeIndex) const
{
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

