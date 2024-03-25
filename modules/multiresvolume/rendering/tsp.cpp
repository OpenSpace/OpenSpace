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

#include <modules/multiresvolume/rendering/tsp.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <numeric>
#include <queue>

namespace {
    constexpr std::string_view _loggerCat = "TSP";
} // namespace

namespace openspace {

TSP::TSP(const std::string& filename)
    : _filename(filename)
{
    _file.open(_filename, std::ios::in | std::ios::binary);
}

TSP::~TSP() {
    if (_file.is_open()) {
        _file.close();
    }
}

bool TSP::load() {
    if (!readHeader()) {
        LERROR("Could not read header");
        return false;
    }

    if (readCache()) {
        LINFO("Using cache");
    }
    else {
        if (!construct()) {
            LERROR("Could not construct");
            return false;
        }

        if (false) {
            if (!calculateSpatialError()) {
                LERROR("Could not calculate spatial error");
                return false;
            }
            if (!calculateTemporalError()) {
                LERROR("Could not calculate temporal error");
                return false;
            }
            if (!writeCache()) {
                LERROR("Could not write cache");
                return false;
            }
        }
    }
    initalizeSSO();

    return true;
}

bool TSP::readHeader() {

    if (!_file.good())
        return false;

    _file.seekg(_file.beg);

    _file.read(reinterpret_cast<char*>(&_header), sizeof(Header));

    LDEBUG(std::format("Grid type: {}", _header.gridType));
    LDEBUG(std::format(
        "Brick dimensions: {} {} {}",
        _header.xBrickDim, _header.yBrickDim, _header.zBrickDim
    ));
    LDEBUG(std::format(
        "Num bricks: {} {} {}",
        _header.xNumBricks, _header.yNumBricks, _header.zNumBricks
    ));

    _paddedBrickDim = _header.xBrickDim + 2 * _paddingWidth;
    // TODO support dimensions of different size
    _numOTLevels = static_cast<unsigned int>(
        log(static_cast<int>(_header.xNumBricks)) / log(2) + 1
    );
    _numOTNodes = static_cast<unsigned int>((pow(8, _numOTLevels) - 1) / 7);
    _numBSTLevels = static_cast<unsigned int>(
        log(static_cast<int>(_header.numTimesteps)) / log(2) + 1
    );
    _numBSTNodes = _header.numTimesteps * 2 - 1;
    _numTotalNodes = _numOTNodes * _numBSTNodes;

    LDEBUG(std::format("Num OT levels: {}", _numOTLevels));
    LDEBUG(std::format("Num OT nodes: {}", _numOTNodes));
    LDEBUG(std::format("Num BST levels: {}", _numBSTLevels));
    LDEBUG(std::format("Num BST nodes: {}", _numBSTNodes));
    LDEBUG(std::format("Num total nodes: {}", _numTotalNodes));

    // Allocate space for TSP structure
    _data.resize(_numTotalNodes*NUM_DATA);
    LDEBUG(std::format("Data size: {}",  _data.size()));

    return true;
}

bool TSP::construct() {
    LDEBUG("Constructing TSP tree");

    // Loop over the OTs (one per BST node)
    for (unsigned int OT = 0; OT < _numBSTNodes; ++OT) {
        // Start at the root of each OT
        unsigned int OTNode = OT * _numOTNodes;

        // Calculate BST level (first level is level 0)
        unsigned int BSTLevel = static_cast<unsigned int>(log1p(OT) / log(2));

        // Traverse OT
        unsigned int OTChild = 1;
        unsigned int OTLevel = 0;
        while (OTLevel < _numOTLevels) {
            unsigned int OTNodesInLevel = static_cast<unsigned int>(pow(8, OTLevel));
            for (unsigned int i = 0; i<OTNodesInLevel; i++) {
                // Brick index
                _data[OTNode*NUM_DATA + BRICK_INDEX] = static_cast<int>(OTNode);

                // Error metrics
                _data[OTNode*NUM_DATA + TEMPORAL_ERR] = static_cast<int>(
                    _numBSTLevels - 1 - BSTLevel
                );
                _data[OTNode*NUM_DATA + SPATIAL_ERR] = static_cast<int>(
                    _numOTLevels - 1 - OTLevel
                );

                if (BSTLevel == 0) {
                    // Calculate OT child index (-1 if node is leaf)
                    int OTChildIndex = (OTChild < _numOTNodes) ?
                        static_cast<int>(OT*_numOTNodes + OTChild) :
                        -1;
                    _data[OTNode*NUM_DATA + CHILD_INDEX] = OTChildIndex;
                }
                else {
                    // Calculate BST child index (-1 if node is BST leaf)

                    // First BST node of current level
                    int firstNode = static_cast<unsigned int>(
                        (2 * pow(2, BSTLevel - 1) - 1) * _numOTNodes
                    );
                    // First BST node of next level
                    int firstChild = static_cast<unsigned int>(
                        (2 * pow(2, BSTLevel) - 1) * _numOTNodes
                    );
                    // Difference between first nodes between levels
                    int levelGap = firstChild - firstNode;
                    // How many nodes away from the first node are we?
                    int offset = (OTNode - firstNode) / _numOTNodes;

                    // Use level gap and offset to calculate child index
                    int BSTChildIndex = (BSTLevel < _numBSTLevels - 1) ?
                        static_cast<int>(OTNode + levelGap + (offset*_numOTNodes)) :
                        -1;

                    _data[OTNode*NUM_DATA + CHILD_INDEX] = BSTChildIndex;
                }

                OTNode++;
                OTChild += 8;
            }

            OTLevel++;
        }
    }
    return true;
}

bool TSP::initalizeSSO() {
    if (!_dataSSBO) {
        glGenBuffers(1, &_dataSSBO);
    }

    const size_t size = sizeof(GLint)*_data.size();
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _dataSSBO);
    //glBufferData(GL_SHADER_STORAGE_BUFFER, size, _data.data(), GL_DYNAMIC_READ);
    glBufferData(GL_SHADER_STORAGE_BUFFER, size, _data.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    glFinish();
    return true;
}

const TSP::Header& TSP::header() const {
    return _header;
}

long long TSP::dataPosition() {
    return sizeof(Header);
}

std::ifstream& TSP::file() {
    return _file;
}

unsigned int TSP::numTotalNodes() const {
    return _numTotalNodes;
}

unsigned int TSP::numValuesPerNode() const {
    return NUM_DATA;
}

unsigned int TSP::numBSTNodes() const {
    return _numBSTNodes;
}

unsigned int TSP::numBSTLevels() const {
    return _numBSTLevels;
}

unsigned int TSP::numOTNodes() const {
    return _numOTNodes;
}

unsigned int TSP::numOTLevels() const {
    return _numOTLevels;
}

unsigned int TSP::brickDim() const {
    return _header.xBrickDim;
}

unsigned int TSP::paddedBrickDim() const {
    return _paddedBrickDim;
}

unsigned int TSP::numBricksPerAxis() const {
    return _header.xNumBricks;
}

GLuint TSP::ssbo() const {
    return _dataSSBO;
}

bool TSP::calculateSpatialError() {
    unsigned int numBrickVals = _paddedBrickDim*_paddedBrickDim*_paddedBrickDim;

    if (!_file.is_open()) {
        return false;
    }

    std::vector<float> buffer(numBrickVals);
    std::vector<float> averages(_numTotalNodes);
    std::vector<float> stdDevs(_numTotalNodes);

    // First pass: Calculate average color for each brick
    LDEBUG("Calculating spatial error, first pass");
    for (unsigned int brick = 0; brick<_numTotalNodes; ++brick) {
        // Offset in file
        std::streampos offset = dataPosition() +
                                static_cast<long long>(brick*numBrickVals*sizeof(float));
        _file.seekg(offset);

        _file.read(
            reinterpret_cast<char*>(&buffer[0]),
            static_cast<size_t>(numBrickVals) * sizeof(float)
        );

        double average = std::accumulate(
            buffer.begin(),
            buffer.end(),
            0.0,
            [](double a, float b) { return a + static_cast<double>(b); }
        );
        averages[brick] = static_cast<float>(average / static_cast<double>(numBrickVals));
    }

    // Spatial SNR stats
    float minError = 1e20f;
    float maxError = 0.f;
    std::vector<float> medianArray(_numTotalNodes);

    // Second pass: For each brick, compare the covered leaf voxels with
    // the brick average
    LDEBUG("Calculating spatial error, second pass");
    for (unsigned int brick = 0; brick < _numTotalNodes; ++brick) {
        // Fetch mean intensity
        float brickAvg = averages[brick];

        // Sum  for std dev computation
        float stdDev = 0.f;

        // Get a list of leaf bricks that the current brick covers
        std::list<unsigned int> leafBricksCovered = coveredLeafBricks(brick);

        // If the brick is already a leaf, assign a negative error.
        // Ad hoc "hack" to distinguish leafs from other nodes that happens
        // to get a zero error due to rounding errors or other reasons.
        if (leafBricksCovered.size() == 1) {
            stdDev = -0.1f;
        }
        else {

            // Calculate "standard deviation" corresponding to leaves
            for (auto lb = leafBricksCovered.begin(); lb != leafBricksCovered.end(); ++lb)
            {
                // Read brick
                std::streampos offset = dataPosition() +
                                 static_cast<long long>((*lb)*numBrickVals*sizeof(float));
                _file.seekg(offset);

                _file.read(reinterpret_cast<char*>(&buffer[0]),
                    static_cast<size_t>(numBrickVals)*sizeof(float));

                // Add to sum
                for (auto v = buffer.begin(); v != buffer.end(); ++v) {
                    stdDev += pow(*v - brickAvg, 2.f);
                }
            }

            stdDev /= static_cast<float>(leafBricksCovered.size()*numBrickVals);
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
    //float medError = medianArray[medianArray.size()/2];

    // "Normalize" errors
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<_numTotalNodes; i++) {
        //float normalized = (stdDevs[i]-minError)/(maxError-minError);
        if (stdDevs[i] > 0.f) {
            stdDevs[i] = pow(stdDevs[i], 0.5f);
        }
        //_data[i*NUM_DATA + SPATIAL_ERR] = *reinterpret_cast<int*>(&stdDevs[i]);
        _data[i*NUM_DATA + SPATIAL_ERR] = glm::floatBitsToInt(stdDevs[i]);
        if (stdDevs[i] < minNorm) {
            minNorm = stdDevs[i];
        }
        else if (stdDevs[i] > maxNorm) {
            maxNorm = stdDevs[i];
        }
    }

    std::sort(stdDevs.begin(), stdDevs.end());
    float medNorm = stdDevs[stdDevs.size() / 2];

    _minSpatialError = minNorm;
    _maxSpatialError = maxNorm;
    _medianSpatialError = medNorm;

    LDEBUG(std::format("Min normalized spatial std dev: {}", minNorm));
    LDEBUG(std::format("Max normalized spatial std dev: {}", maxNorm));
    LDEBUG(std::format("Median normalized spatial std dev: {}", medNorm));

    return true;
}

bool TSP::calculateTemporalError() {
    if (!_file.is_open()) {
        return false;
    }

    LDEBUG("Calculating temporal error");

    // Statistics
    //float minErr = 1e20f;
    //float maxErr = 0.f;
    std::vector<float> meanArray(_numTotalNodes);

    // Save errors
    std::vector<float> errors(_numTotalNodes);

    // Calculate temporal error for one brick at a time
    for (unsigned int brick = 0; brick<_numTotalNodes; ++brick) {
        unsigned int numBrickVals = _paddedBrickDim * _paddedBrickDim * _paddedBrickDim;

        // Save the individual voxel's average over timesteps. Because the
        // BSTs are built by averaging leaf nodes, we only need to sample
        // the brick at the correct coordinate.
        std::vector<float> voxelAverages(numBrickVals);
        std::vector<float> voxelStdDevs(numBrickVals);

        // Read the whole brick to fill the averages
        std::streampos offset = dataPosition() +
                                static_cast<long long>(brick*numBrickVals*sizeof(float));
        _file.seekg(offset);

        _file.read(
            reinterpret_cast<char*>(voxelAverages.data()),
            static_cast<size_t>(numBrickVals)*sizeof(float)
        );

        // Build a list of the BST leaf bricks (within the same octree level) that
        // this brick covers
        std::list<unsigned int> coveredBricks = coveredBSTLeafBricks(brick);

        // If the brick is at the lowest BST level, automatically set the error
        // to -0.1 (enables using -1 as a marker for "no error accepted");
        // Somewhat ad hoc to get around the fact that the error could be
        // 0.0 higher up in the tree
        if (coveredBricks.size() == 1) {
            errors[brick] = -0.1f;
        }
        else {
            // Calculate standard deviation per voxel, average over brick
            float avgStdDev = 0.f;
            for (unsigned int voxel = 0; voxel<numBrickVals; ++voxel) {
                float stdDev = 0.f;
                for (auto leaf = coveredBricks.begin();
                    leaf != coveredBricks.end(); ++leaf)
                {
                    // Sample the leaves at the corresponding voxel position
                    _file.seekg(dataPosition() +
                        static_cast<long long>(
                            (*leaf * numBrickVals + voxel) * sizeof(float)
                        )
                    );

                    float sample;
                    _file.read(reinterpret_cast<char*>(&sample), sizeof(float));

                    stdDev += pow(sample - voxelAverages[voxel], 2.f);
                }
                stdDev /= static_cast<float>(coveredBricks.size());
                stdDev = sqrt(stdDev);

                avgStdDev += stdDev;
            } // for voxel

            avgStdDev /= static_cast<float>(numBrickVals);
            meanArray[brick] = avgStdDev;
            errors[brick] = avgStdDev;
        }
    } // for all bricks

    std::sort(meanArray.begin(), meanArray.end());
    //float medErr = meanArray[meanArray.size()/2];

    // Adjust errors using user-provided exponents
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i < _numTotalNodes; i++) {
        if (errors[i] > 0.f) {
            errors[i] = pow(errors[i], 0.25f);
        }
        _data[i * NUM_DATA + TEMPORAL_ERR] = glm::floatBitsToInt(errors[i]);
        if (errors[i] < minNorm) {
            minNorm = errors[i];
        }
        else if (errors[i] > maxNorm) {
            maxNorm = errors[i];
        }
    }

    std::sort(errors.begin(), errors.end());
    float medNorm = errors[errors.size() / 2];

    _minTemporalError = minNorm;
    _maxTemporalError = maxNorm;
    _medianTemporalError = medNorm;

    LDEBUG(std::format("Min normalized temporal std dev: {}", minNorm));
    LDEBUG(std::format("Max normalized temporal std dev: {}", maxNorm));
    LDEBUG(std::format("Median normalized temporal std dev: {}", medNorm));

    return true;
}

bool TSP::readCache() {
    if (!FileSys.cacheManager())
        return false;

    std::filesystem::path cacheFilename = FileSys.cacheManager()->cachedFilename(
        std::filesystem::path(_filename).stem(),
        ""
    );

    std::ifstream file(cacheFilename, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
        LWARNING(std::format("Failed to open {}", cacheFilename));
        return false;
    }


    file.read(reinterpret_cast<char*>(&_minSpatialError), sizeof(float));
    file.read(reinterpret_cast<char*>(&_maxSpatialError), sizeof(float));
    file.read(reinterpret_cast<char*>(&_medianSpatialError), sizeof(float));
    file.read(reinterpret_cast<char*>(&_minTemporalError), sizeof(float));
    file.read(reinterpret_cast<char*>(&_maxTemporalError), sizeof(float));
    file.read(reinterpret_cast<char*>(&_medianTemporalError), sizeof(float));
    size_t dataSize = static_cast<size_t>(_numTotalNodes * NUM_DATA) * sizeof(int);
    file.read(reinterpret_cast<char*>(_data.data()), dataSize);
    file.close();

    LDEBUG("Cached errors:");
    LDEBUG(std::format("Min spatial error: {}", _minSpatialError));
    LDEBUG(std::format("Max spatial error: {}", _maxSpatialError));
    LDEBUG(std::format("Median spatial error: {}", _medianSpatialError));
    LDEBUG(std::format("Min temporal error: {}", _minTemporalError));
    LDEBUG(std::format("Max temporal error: {}", _maxTemporalError));
    LDEBUG(std::format("Median temporal error: {}", _medianTemporalError));

    return true;
}

bool TSP::writeCache() {
    if (!FileSys.cacheManager()) {
        return false;
    }

    std::filesystem::path cacheFilename = FileSys.cacheManager()->cachedFilename(
        std::filesystem::path(_filename).stem(),
        ""
    );

    std::ofstream file(cacheFilename, std::ios::out | std::ios::binary);
    if (!file.is_open()) {
        LWARNING(std::format("Failed to open {}", cacheFilename));
        return false;
    }
    LINFO(std::format("Writing cache to {}", cacheFilename));

    file.write(reinterpret_cast<char*>(&_minSpatialError), sizeof(float));
    file.write(reinterpret_cast<char*>(&_maxSpatialError), sizeof(float));
    file.write(reinterpret_cast<char*>(&_medianSpatialError), sizeof(float));
    file.write(reinterpret_cast<char*>(&_minTemporalError), sizeof(float));
    file.write(reinterpret_cast<char*>(&_maxTemporalError), sizeof(float));
    file.write(reinterpret_cast<char*>(&_medianTemporalError), sizeof(float));
    file.write(reinterpret_cast<char*>(_data.data()), _data.size() * sizeof(float));

    file.close();

    return true;
}

float TSP::spatialError(unsigned int brickIndex) const {
    return *reinterpret_cast<const float*>(_data[brickIndex*NUM_DATA + SPATIAL_ERR]);
}

float TSP::temporalError(unsigned int brickIndex) const {
    return *reinterpret_cast<const float*>(_data[brickIndex*NUM_DATA + TEMPORAL_ERR]);
}

unsigned int TSP::firstOctreeChild(unsigned int brickIndex) const {
    const unsigned int otNode = brickIndex % _numOTNodes;
    const unsigned int bstOffset = brickIndex - otNode;

    const unsigned int depth = static_cast<unsigned int>(log1p(7 * otNode) / log(8));
    const unsigned int firstInLevel = static_cast<unsigned int>((pow(8, depth) - 1) / 7);
    const unsigned int levelOffset = otNode - firstInLevel;
    const unsigned int firstInChildLevel = static_cast<unsigned int>(
        (pow(8, depth + 1) - 1) / 7
    );
    const unsigned int childIndex = firstInChildLevel + 8 * levelOffset;

    return bstOffset + childIndex;
}

unsigned int TSP::bstLeft(unsigned int brickIndex) const {
    const unsigned int bstNode = brickIndex / _numOTNodes;
    const unsigned int otOffset = brickIndex % _numOTNodes;
    const unsigned int depth = static_cast<unsigned int>(log1p(bstNode) / log(2));
    const unsigned int firstInLevel = static_cast<unsigned int>(pow(2, depth) - 1);
    const unsigned int levelOffset = bstNode - firstInLevel;
    const unsigned int firstInChildLevel = static_cast<unsigned int>(
        pow(2, depth + 1) - 1
    );
    const unsigned int childIndex = firstInChildLevel + 2 * levelOffset;
    return otOffset + childIndex * _numOTNodes;
}

unsigned int TSP::bstRight(unsigned int brickIndex) const {
    return bstLeft(brickIndex) + _numOTNodes;
}

bool TSP::isBstLeaf(unsigned int brickIndex) const {
    const unsigned int bstNode = brickIndex / _numOTNodes;
    return bstNode >= _numBSTNodes / 2;
}

bool TSP::isOctreeLeaf(unsigned int brickIndex) const {
    const unsigned int otNode = brickIndex % _numOTNodes;
    const unsigned int depth = static_cast<unsigned int>(log1p(7 * otNode) / log(8));
    return depth == _numOTLevels - 1;
}

std::list<unsigned int> TSP::coveredLeafBricks(unsigned int brickIndex) const {
    std::list<unsigned int> out;

    // Find what octree skeleton node the index belongs to
    const unsigned int OTNode = brickIndex % _numOTNodes;

    // Find what BST node the index corresponds to using int division
    const unsigned int BSTNode = brickIndex / _numOTNodes;
    // Calculate BST offset (to translate to root octree)
    const unsigned int BSTOffset = BSTNode * _numOTNodes;

    // Traverse root octree structure to leaves
    // When visiting the leaves, translate back to correct BST level and save
    std::queue<unsigned int> queue;
    queue.push(OTNode);
    do {
        // Get front of queue and pop it
        const unsigned int toVisit = queue.front();
        queue.pop();

        // See if the node has children
        int child = _data[toVisit*NUM_DATA + CHILD_INDEX];
        if (child == -1) {
            // Translate back and save
            out.push_back(toVisit + BSTOffset);
        }
        else {
            // Queue the eight children
            for (int i = 0; i<8; i++) {
                queue.push(child + i);
            }
        }
    } while (!queue.empty());

    return out;
}

std::list<unsigned int> TSP::coveredBSTLeafBricks(unsigned int brickIndex) const {
    std::list<unsigned int> out;

    // Traverse the BST children until we are at root
    std::queue<unsigned int> queue;
    queue.push(brickIndex);
    do {
        const unsigned int toVisit = queue.front();
        queue.pop();

        bool BSTRoot = toVisit < _numOTNodes;
        if (BSTRoot) {
            if (_numBSTLevels == 1) {
                out.push_back(toVisit);
            }
            else {
                queue.push(toVisit + _numOTNodes);
                queue.push(toVisit + _numOTNodes * 2);
            }
        }
        else {
            int child = _data[toVisit*NUM_DATA + CHILD_INDEX];
            if (child == -1) {
                // Save leaf brick to list
                out.push_back(toVisit);
            }
            else {
                // Queue children
                queue.push(child);
                queue.push(child + _numOTNodes);
            }
        }

    } while (!queue.empty());

    return out;
}

} // namespace openspace
