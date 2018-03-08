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

#include <modules/multiresvolume/rendering/shentsp.h>

// ghoul
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>
#include <ghoul/fmt.h>

// boost
#include <boost/iostreams/device/mapped_file.hpp>

// std
#include <algorithm>
#include <math.h>
#include <queue>

namespace {
    const std::string _loggerCat = "ShenTSP";
}

namespace openspace {

ShenTSP::ShenTSP(const std::string& filename)
    : TSP(filename) { }

ShenTSP::~ShenTSP() {
    if (_file.is_open())
        _file.close();
}

bool ShenTSP::construct() {
    LDEBUG("Constructing TSP...");
    return TSP::construct();
}

std::vector<float> ShenTSP::calculateBrickStdDevs(std::vector<float> brickAverages) {
    // Sanity check
    if (sizeof(float) != sizeof(int)) {
        LERROR("Float and int sizes don't match, can't reintepret");
        return {};
    }

    boost::iostreams::mapped_file_source mfile;
    mfile.open(_filename);

    if (!mfile.is_open()) {
        return {};
    }

    const float * voxelData = (float *)mfile.data();
    const long long headerOffset = dataPosition() / sizeof(float);
    const unsigned int numBrickVals = paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;
    std::vector<float> stdDevs(numTotalNodes_);

    // Second pass: For each brick, compare the covered leaf voxels with
    // the brick average
    LDEBUG("Calculating spatial error, second pass");
    for (size_t brick = 0; brick<numTotalNodes_; ++brick) {

        // If the brick is already a leaf, assign a negative error.
        // Ad hoc "hack" to distinguish leafs from other nodes that happens
        // to get a zero error due to rounding errors or other reasons.
        if (isOctreeLeaf(brick)) {
            stdDevs[brick] = -0.1f;
            continue;
        }

        // Else for non-leaves: Calculate "standard deviation" corresponding
        // to leaves by averaging the stdDevs
        float stdDev = 0.f;
        // Fetch mean intensity 
        const float brickAvg = brickAverages[brick];

        // Offset in file
        const auto brickStart = headerOffset + static_cast<long long>(brick*numBrickVals);
        for (size_t i = 0; i < numBrickVals; i++) {
            stdDev += pow(voxelData[brickStart + i] - brickAvg, 2.f);
        }

// Finish calculation
stdDev /= static_cast<float>(numBrickVals);
stdDev = sqrt(stdDev);

stdDevs[brick] = stdDev;
    }

    mfile.close();

    return stdDevs;
}

bool ShenTSP::calculateSpatialError() {
    LDEBUG("Calculating spatial error");
    std::vector<float> averages = calculateBrickAverages();
    if (!averages.size()) {
        LERROR("Could not calculate brick averages");
        return false;
    }

    std::vector<float> stdDevs = calculateBrickStdDevs(averages);
    if (!stdDevs.size()) {
        LERROR("Could not calculate brick standard deviations");
        return false;
    }

    // "Normalize" errors
    float minNorm = 1e20f;
    float maxNorm = 0.f;

    for (size_t i = 0; i < stdDevs.size(); ++i) {

        // Store the coefficient of variation (stdDev/mean)
        if (stdDevs[i] > 0.f) {
            stdDevs[i] /= averages[i];
        }
        data_[i*NUM_DATA + SPATIAL_ERR] = glm::floatBitsToInt(stdDevs[i]);

        // Store statistics
        if (stdDevs[i] < minNorm && 0 <= stdDevs[i]) {
            minNorm = stdDevs[i];
        }
        if (stdDevs[i] > maxNorm && 0 <= stdDevs[i]) {
            maxNorm = stdDevs[i];
        }
    }

    std::sort(stdDevs.begin(), stdDevs.end());
    float medNorm = stdDevs[stdDevs.size() / 2];

    minSpatialError_ = minNorm;
    maxSpatialError_ = maxNorm;
    medianSpatialError_ = medNorm;

    LDEBUG(fmt::format("Min spatial coefficient of variation: {}", minNorm));
    LDEBUG(fmt::format("Max spatial coefficient of variation: {}", maxNorm));
    LDEBUG(fmt::format("Median spatial coefficient of variation: {}", medNorm));

    return true;
}

bool ShenTSP::calculateTemporalError() {

    boost::iostreams::mapped_file_source mfile;
    mfile.open(_filename);

    if (!mfile.is_open()) {
        return false;
    }

    const float * voxelData = (float *)mfile.data();
    const long long headerOffset = dataPosition() / sizeof(float);

    LDEBUG("Calculating temporal error");

    // Save errors
    std::vector<float> errors(numTotalNodes_);

    const unsigned int numBrickVals =
        paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;

    // Calculate temporal error for one brick at a time
    for (unsigned int brick = 0; brick < numTotalNodes_; ++brick) {

        // Save the individual voxel's average over timesteps. Because the
        // BSTs are built by averaging leaf nodes, we only need to sample
        // the brick at the correct coordinate.
        const auto brickStart = headerOffset + static_cast<long long>(brick*numBrickVals);

        std::list<unsigned int> coveredBricks = CoveredBSTLeafBricks(brick);

        // If the brick is at the lowest BST level, automatically set the error 
        // to -0.1 (enables using -1 as a marker for "no error accepted");
        // Somewhat ad hoc to get around the fact that the error could be
        // 0.0 higher up in the tree
        if (coveredBricks.size() == 1) {
            errors[brick] = -0.1f;
            continue;
        } // done: move to next iteration

        // Calculate standard deviation per voxel, average over brick
        float cov = 0.f;
        for (size_t voxel = 0; voxel< numBrickVals; ++voxel) {
            float stdDev = 0.f;
            for (auto leaf = coveredBricks.begin(); leaf != coveredBricks.end(); ++leaf) {
                // Sample the leaves at the corresponding voxel position
                const auto leafOffset = headerOffset + static_cast<long long>(*leaf*numBrickVals + voxel);

                const float sample = voxelData[leafOffset];
                stdDev += pow(sample - voxelData[brickStart + voxel], 2.f);
            }
            stdDev /= static_cast<float>(coveredBricks.size());
            stdDev = sqrt(stdDev);

            cov += stdDev / voxelData[brickStart + voxel];

        } // for voxel

        cov /= static_cast<float>(numBrickVals);
        errors[brick] = cov;

    } // for all bricks

    mfile.close();

    // Adjust errors using user-provided exponents
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<numTotalNodes_; ++i) {

        data_[i*NUM_DATA + TEMPORAL_ERR] = glm::floatBitsToInt(errors[i]);
        if (errors[i] < minNorm && 0 <= errors[i]) {
            minNorm = errors[i];
        }
        if (errors[i] > maxNorm && 0 <= errors[i]) {
            maxNorm = errors[i];
        }
    }

    std::sort(errors.begin(), errors.end());
    float medNorm = errors[errors.size() / 2];

    minTemporalError_ = minNorm;
    maxTemporalError_ = maxNorm;
    medianTemporalError_ = medNorm;

    LDEBUG(fmt::format("Min temporal coefficient of variation: {}", minNorm));
    LDEBUG(fmt::format("Max temporal coefficient of variation: {}", maxNorm));
    LDEBUG(fmt::format("Median temporal coefficient of variation: {}", medNorm));

    return true;
}

}
