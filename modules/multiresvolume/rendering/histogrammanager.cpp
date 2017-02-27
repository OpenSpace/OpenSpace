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

#include <float.h>
#include <math.h>
#include <string.h>
#include <cassert>

#include <modules/multiresvolume/rendering/histogrammanager.h>
#include <openspace/util/histogram.h>

namespace {
    const std::string _loggerCat = "HistogramManager";
}

namespace openspace {

HistogramManager::HistogramManager() {}

HistogramManager::~HistogramManager() {}

bool HistogramManager::buildHistograms(TSP* tsp, int numBins) {
    std::cout << "Build histograms with " << numBins << " bins each" << std::endl;
    _numBins = numBins;

    std::ifstream& file = tsp->file();
    if (!file.is_open()) {
        return false;
    }
    _minBin = 0.0; // Should be calculated from tsp file
    _maxBin = 1.0; // Should be calculated from tsp file

    int numTotalNodes = tsp->numTotalNodes();
    _histograms = std::vector<Histogram>(numTotalNodes);

    bool success = buildHistogram(tsp, 0);

    return success;
}

Histogram* HistogramManager::getHistogram(unsigned int brickIndex) {
    return &_histograms[brickIndex];
}

bool HistogramManager::buildHistogram(TSP* tsp, unsigned int brickIndex) {
    Histogram histogram(_minBin, _maxBin, _numBins);
    bool isBstLeaf = tsp->isBstLeaf(brickIndex);
    bool isOctreeLeaf = tsp->isOctreeLeaf(brickIndex);

    if (isBstLeaf && isOctreeLeaf) {
        // TSP leaf, read from file and build histogram
        std::vector<float> voxelValues = readValues(tsp, brickIndex);
        unsigned int numVoxels = voxelValues.size();

        for (unsigned int v = 0; v < numVoxels; ++v) {
            histogram.add(voxelValues[v], 1.0);
        }
    } else {
        // Has children
        auto children = std::vector<unsigned int>();

        if (!isBstLeaf) {
            // Push BST children
            children.push_back(tsp->getBstLeft(brickIndex));
            children.push_back(tsp->getBstRight(brickIndex));
        }
        if (!isOctreeLeaf) {
            // Push Octree children
            unsigned int firstChild = tsp->getFirstOctreeChild(brickIndex);
            for (int c = 0; c < 8; c++) {
                children.push_back(firstChild + c);
            }
        }
        int numChildren = children.size();
        for (int c = 0; c < numChildren; c++) {
            // Visit child
            unsigned int childIndex = children[c];
            if (_histograms[childIndex].isValid() || buildHistogram(tsp, childIndex)) {
                if (numChildren <= 8 || c < 2) {
                    // If node has both BST and Octree children, only add BST ones
                    histogram.add(_histograms[childIndex]);
                }
            } else {
                return false;
            }
        }
    }

    //histogram.normalize();
    _histograms[brickIndex] = std::move(histogram);


    return true;
}

std::vector<float> HistogramManager::readValues(TSP* tsp, unsigned int brickIndex) {
    unsigned int paddedBrickDim = tsp->paddedBrickDim();
    unsigned int numBrickVals = paddedBrickDim * paddedBrickDim * paddedBrickDim;
    std::vector<float> voxelValues(numBrickVals);

    std::streampos offset = tsp->dataPosition() + static_cast<long long>(brickIndex*numBrickVals*sizeof(float));
    std::ifstream& file = tsp->file();
    file.seekg(offset);

    file.read(reinterpret_cast<char*>(&voxelValues[0]),
        static_cast<size_t>(numBrickVals)*sizeof(float));

    return voxelValues;
}

bool HistogramManager::loadFromFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
    return false;
    }

    int numHistograms;
    file.read(reinterpret_cast<char*>(&numHistograms), sizeof(int));
    file.read(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.read(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.read(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    int nFloats = numHistograms * _numBins;
    float* histogramData = new float[nFloats];
    file.read(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);

    _histograms = std::vector<Histogram>(numHistograms);

    for (int i = 0; i < numHistograms; ++i) {
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


bool HistogramManager::saveToFile(const std::string& filename) {
    std::ofstream file(filename, std::ios::out | std::ios::binary);
    if (!file.is_open()) {
    return false;
    }

    int numHistograms = _histograms.size();
    file.write(reinterpret_cast<char*>(&numHistograms), sizeof(int));
    file.write(reinterpret_cast<char*>(&_numBins), sizeof(int));
    file.write(reinterpret_cast<char*>(&_minBin), sizeof(float));
    file.write(reinterpret_cast<char*>(&_maxBin), sizeof(float));

    int nFloats = numHistograms * _numBins;
    float* histogramData = new float[nFloats];

    for (int i = 0; i < numHistograms; ++i) {
    int offset = i*_numBins;
    memcpy(&histogramData[offset], _histograms[i].data(), sizeof(float) * _numBins);
    }

    file.write(reinterpret_cast<char*>(histogramData), sizeof(float) * nFloats);
    delete[] histogramData;

    file.close();
    return true;
}

} // namespace openspace

