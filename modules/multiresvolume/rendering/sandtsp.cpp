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

#include <modules/multiresvolume/rendering/sandtsp.h>

// ghoul
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

// boost
#include <boost/iostreams/device/mapped_file.hpp>

// std
#include <algorithm>
#include <math.h>
#include <queue>

# include <chrono>

namespace {
    const std::string _loggerCat = "SandTSP";
}

namespace openspace {

SandTSP::SandTSP(const std::string& filename)
    : TSP(filename) { }

SandTSP::~SandTSP() {
    if (_file.is_open())
        _file.close();
}

bool SandTSP::construct() {
    LDEBUG("Constructing TSP...");
    return TSP::construct();
}

std::vector<float> SandTSP::generateLeafCoverages() {

    unsigned int leaves = _header.numOrigTimesteps_;
    unsigned int levels = numBSTLevels_;
    unsigned int nodes = numBSTNodes_;
    unsigned int voxels = std::pow(paddedBrickDim_, 3);

    // Get the number of levels -- log(number of time steps) + 1 for root
    // int levels = ceil(log2(_header.numOrigTimesteps_) + 1);
    // Sum the number of nodes as geometric series (1-2^levels)/1-2
    // int bstNodeCount = -(1 - std::exp2(levels));
    LINFO("Orig: " << _header.numOrigTimesteps_ << " Levels : " << levels << " leaves : " << leaves << " Count : " << nodes);

    std::vector<float> averages(nodes);

    // First propogate all the leaves values
    for (size_t n = 0; n < leaves; n++) {
        std::vector<float> voxelAverages(voxels);
        std::vector<float> voxelStdDevs(voxels);

        // Read the whole brick to fill the averages
        std::streampos offset = dataPosition() + static_cast<long long>(n * voxels * sizeof(float));
        LINFO("Brick " << n << ", offset " << offset);
        _file.seekg(offset);

        _file.read(reinterpret_cast<char*>(&voxelAverages[0]), static_cast<size_t>(voxels) * sizeof(float));

        averages[nodes - leaves + n] = 1;
    }

    // For each node, sum the level below it
    for (int n = levels - 2; 0 <= n; n--) {
        LINFO("Levels " << n );
        averages[n] = n;
    }
    return averages;
}

bool SandTSP::calculateSpatialError() {
    unsigned int numBrickVals = paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;

    if (!_file.is_open())
        return false;

    std::vector<float> buffer(numBrickVals);
    std::vector<float> averages(numTotalNodes_);
    std::vector<float> stdDevs(numTotalNodes_);

    // First pass: Calculate average color for each brick
    LDEBUG("Calculating spatial error, first pass");
    for (unsigned int brick = 0; brick<numTotalNodes_; ++brick) {

        // Offset in file
        std::streampos offset = dataPosition() + static_cast<long long>(brick*numBrickVals*sizeof(float));
        _file.seekg(offset);

        _file.read(reinterpret_cast<char*>(&buffer[0]),
            static_cast<size_t>(numBrickVals)*sizeof(float));

        double average = 0.0;
        for (auto it = buffer.begin(); it != buffer.end(); ++it) {
            average += *it;
        }

        averages[brick] = average / static_cast<double>(numBrickVals);
    }

    // Spatial SNR stats
    float minError = 1e20f;
    float maxError = 0.f;
    std::vector<float> medianArray(numTotalNodes_);

    // Second pass: For each brick, compare the covered leaf voxels with
    // the brick average
    LDEBUG("Calculating spatial error, second pass");
    for (unsigned int brick = 0; brick<numTotalNodes_; ++brick) {

        // Fetch mean intensity 
        float brickAvg = averages[brick];

        // Sum  for std dev computation
        float stdDev = 0.f;

        // Get a list of leaf bricks that the current brick covers
        std::list<unsigned int> coveredLeafBricks =
            CoveredLeafBricks(brick);

        // If the brick is already a leaf, assign a negative error.
        // Ad hoc "hack" to distinguish leafs from other nodes that happens
        // to get a zero error due to rounding errors or other reasons.
        if (coveredLeafBricks.size() == 1) {
            stdDev = -0.1f;
        }
        else {

            // Calculate "standard deviation" corresponding to leaves
            for (auto lb = coveredLeafBricks.begin();
                lb != coveredLeafBricks.end(); ++lb) {

                // Read brick
                std::streampos offset = dataPosition() + static_cast<long long>((*lb)*numBrickVals*sizeof(float));
                _file.seekg(offset);

                _file.read(reinterpret_cast<char*>(&buffer[0]),
                    static_cast<size_t>(numBrickVals)*sizeof(float));

                // Add to sum
                for (auto v = buffer.begin(); v != buffer.end(); ++v) {
                    stdDev += pow(*v - brickAvg, 2.f);
                }


            }

            // Finish calculation
            if (sizeof(float) != sizeof(int)) {
                LERROR("Float and int sizes don't match, can't reintepret");
                return false;
            }

            stdDev /= static_cast<float>(coveredLeafBricks.size()*numBrickVals);
            stdDev = sqrt(stdDev);

        } // if not leaf

        if (stdDev < minError) {
            minError = stdDev;
        }
        else if (stdDev > maxError) {
            maxError = stdDev;
        }

        stdDevs[brick] = stdDev;
        medianArray[brick] = stdDev;

    }

    std::sort(medianArray.begin(), medianArray.end());

    // "Normalize" errors
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<numTotalNodes_; ++i) {

        if (stdDevs[i] > 0.f) {
            stdDevs[i] = pow(stdDevs[i], 0.5f);
        }

        data_[i*NUM_DATA + SPATIAL_ERR] = glm::floatBitsToInt(stdDevs[i]);
        if (stdDevs[i] < minNorm) {
            minNorm = stdDevs[i];
        }
        else if (stdDevs[i] > maxNorm) {
            maxNorm = stdDevs[i];
        }
    }

    std::sort(stdDevs.begin(), stdDevs.end());
    float medNorm = stdDevs[stdDevs.size() / 2];

    minSpatialError_ = minNorm;
    maxSpatialError_ = maxNorm;
    medianSpatialError_ = medNorm;

    LDEBUG("Min normalized spatial std dev: " << minNorm);
    LDEBUG("Max normalized spatial std dev: " << maxNorm);
    LDEBUG("Median normalized spatial std dev: " << medNorm);

    return true;
}

/* https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm */
float SandTSP::onlineVariance( std::vector<float> data ) {
    size_t n = 0;
    float mean = 0, M2 = 0.0;

    for (float x : data) {
        n += 1;
        const float delta = x - mean;
        mean += delta / n;
        const float delta2 = x - mean;
        M2 += delta*delta2;
    }

    if (n < 2)
        return -FLT_MAX;
    return M2 / n;
}

bool SandTSP::calculateTemporalError() {

    if (!_file.is_open())
        return false;

    const long long dataOffset = dataPosition();

    boost::iostreams::mapped_file_source mfile;
    mfile.open(_filename);

    if (!mfile.is_open()) {
        return false;
    }

    auto dataStart = mfile.begin() + dataOffset;
    LDEBUG("Size: " << std::distance(mfile.begin(), mfile.end()));
    LDEBUG("Size: " << std::distance(dataStart, mfile.end()));

    float * f = (float *)mfile.data();
    auto off = dataOffset / sizeof(float);
    LDEBUG("Data is: " << f[off] );
    LDEBUG("Calculating temporal error");

    generateLeafCoverages();
    // Statistics
    std::vector<float> meanArray(numTotalNodes_);

    // Save errors
    std::vector<float> errors(numTotalNodes_);

    const unsigned int numBrickVals =
        paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;

    // Calculate temporal error for one brick at a time
    for (unsigned int brick = 0; brick<numTotalNodes_; ++brick) {
        if (!(brick % 10)) LDEBUG("Working on bricks " << brick << " of " << numTotalNodes_);

        // Save the individual voxel's average over timesteps. Because the
        // BSTs are built by averaging leaf nodes, we only need to sample
        // the brick at the correct coordinate.
        std::vector<float> voxelAverages(numBrickVals);
        std::vector<float> voxelStdDevs(numBrickVals);
        std::vector<float> memMap(numBrickVals);

        // Read the whole brick to fill the averages
        const std::streampos offset = dataOffset + static_cast<long long>(brick*numBrickVals*sizeof(float));
        _file.seekg(offset);

        _file.read(reinterpret_cast<char*>(&voxelAverages[0]),
            static_cast<size_t>(numBrickVals)*sizeof(float));

        // float * memMMap  = (float *)mfile.data();
        // memMMap += off + static_cast<long long>(brick*numBrickVals * sizeof(float));

        LDEBUG("File: " << voxelAverages[0]);
        LDEBUG("Memmap: " << f[off+ static_cast<long long>(brick*numBrickVals)]);

        // Build a list of the BST leaf bricks (within the same octree level) that
        // this brick covers

        std::list<unsigned int> coveredBricks = CoveredBSTLeafBricks(brick);

        // If the brick is at the lowest BST level, automatically set the error 
        // to -0.1 (enables using -1 as a marker for "no error accepted");
        // Somewhat ad hoc to get around the fact that the error could be
        // 0.0 higher up in the tree
        if (coveredBricks.size() == 1) {
            errors[brick] = -0.1f;
            continue;
        } // done: move to next iteration

        LINFO("I'm the slow part...");
        auto t1 = std::chrono::steady_clock::now();
        auto end = std::chrono::steady_clock::now();
        // Calculate standard deviation per voxel, average over brick
        float avgStdDev = 0.f;  
        for (unsigned int voxel = 0; voxel<numBrickVals; ++voxel) {
            auto t2 = std::chrono::steady_clock::now();
            float stdDev = 0.f;
            for (auto leaf = coveredBricks.begin(); leaf != coveredBricks.end(); ++leaf) {
                // Sample the leaves at the corresponding voxel position
                auto t3 = std::chrono::steady_clock::now();
                const std::streampos offset = dataOffset + static_cast<long long>((*leaf*numBrickVals + voxel)*sizeof(float));
                _file.seekg(offset);

                float sample;
                _file.read(reinterpret_cast<char*>(&sample), sizeof(float));

                stdDev += pow(sample - voxelAverages[voxel], 2.f);

                end = std::chrono::steady_clock::now();
            }
            stdDev /= static_cast<float>(coveredBricks.size());
            stdDev = sqrt(stdDev);

            avgStdDev += stdDev;
            end = std::chrono::steady_clock::now();
            //LINFO("Elapsed per voxel " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - t2).count());
        } // for voxel
        end = std::chrono::steady_clock::now();
        LINFO("Elapsed per brick " << std::chrono::duration_cast<std::chrono::seconds>(end-t1).count());
        avgStdDev /= static_cast<float>(numBrickVals);
        meanArray[brick] = avgStdDev;
        errors[brick] = avgStdDev;

    } // for all bricks
    mfile.close();

    std::sort(meanArray.begin(), meanArray.end());

    // Adjust errors using user-provided exponents
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<numTotalNodes_; ++i) {
        if (errors[i] > 0.f) {
            errors[i] = pow(errors[i], 0.25f);
        }

        data_[i*NUM_DATA + TEMPORAL_ERR] = glm::floatBitsToInt(errors[i]);
        if (errors[i] < minNorm) {
            minNorm = errors[i];
        }
        else if (errors[i] > maxNorm) {
            maxNorm = errors[i];
        }
    }

    std::sort(errors.begin(), errors.end());
    float medNorm = errors[errors.size() / 2];

    minTemporalError_ = minNorm;
    maxTemporalError_ = maxNorm;
    medianTemporalError_ = medNorm;

    LDEBUG("Min normalized temporal std dev: " << minNorm);
    LDEBUG("Max normalized temporal std dev: " << maxNorm);
    LDEBUG("Median normalized temporal std dev: " << medNorm);

    return true;
}

}
