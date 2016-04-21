/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

//#include <sgct.h>
#include <modules/multiresvolume/rendering/tsp.h>

// ghoul
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

// std
#include <algorithm>
#include <math.h>
#include <queue>

namespace {
    const std::string _loggerCat = "TSP";
}

namespace openspace {

TSP::TSP(const std::string& filename)
    : _filename(filename)
    , _dataSSBO(0)
    , paddedBrickDim_(0)
    , numTotalNodes_(0)
    , numBSTLevels_(0)
    , numBSTNodes_(0)
    , numOTLevels_(0)
    , numOTNodes_(0)
    , minSpatialError_(0.0f)
    , maxSpatialError_(0.0f)
    , medianSpatialError_(0.0f)
    , minTemporalError_(0.0f)
    , maxTemporalError_(0.0f)
    , medianTemporalError_(0.0f)
{
    _file.open(_filename, std::ios::in | std::ios::binary);
}

TSP::~TSP() {
    if (_file.is_open())
        _file.close();
}

bool TSP::load() {
    if (!readHeader()) {
        LERROR("Could not read header");
        return false;
    }

    if (readCache()) {
    //if (false) {
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
    /*
    file.read(reinterpret_cast<char*>(&gridType_),            sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&numOrigTimesteps_),    sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&numTimesteps_),        sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&xBrickDim_),            sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&yBrickDim_),            sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&zBrickDim_),            sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&xNumBricks_),        sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&yNumBricks_),        sizeof(unsigned int));
    file.read(reinterpret_cast<char*>(&zNumBricks_),        sizeof(unsigned int));
    */

    LDEBUG("Grid type: " << _header.gridType_);
    LDEBUG("Brick dimensions: " << _header.xBrickDim_ << " " << _header.yBrickDim_ << " " << _header.zBrickDim_);
    LDEBUG("Num bricks: " << _header.xNumBricks_ << " " << _header.yNumBricks_ << " " << _header.zNumBricks_);

    paddedBrickDim_ = _header.xBrickDim_ + 2 * paddingWidth_;
    // TODO support dimensions of different size
    numOTLevels_ = static_cast<unsigned int>(log((int)_header.xNumBricks_) / log(2) + 1);
    numOTNodes_ = static_cast<unsigned int>((pow(8, numOTLevels_) - 1) / 7);
    numBSTLevels_ = static_cast<unsigned int>(log((int)_header.numTimesteps_) / log(2) + 1);
    numBSTNodes_ = static_cast<unsigned int>(_header.numTimesteps_ * 2 - 1);
    numTotalNodes_ = numOTNodes_ * numBSTNodes_;

    LDEBUG("Num OT levels: " << numOTLevels_);
    LDEBUG("Num OT nodes: " << numOTNodes_);
    LDEBUG("Num BST levels: " << numBSTLevels_);
    LDEBUG("Num BST nodes: " << numBSTNodes_);
    LDEBUG("NUm total nodes: " << numTotalNodes_);

    // Allocate space for TSP structure
    data_.resize(numTotalNodes_*NUM_DATA);
    LDEBUG("data size: " << data_.size());

    return true;
}

bool TSP::construct() {

    LDEBUG("Constructing TSP tree");

    // Loop over the OTs (one per BST node)
    for (unsigned int OT = 0; OT<numBSTNodes_; ++OT) {

        // Start at the root of each OT  
        unsigned int OTNode = OT*numOTNodes_;

        // Calculate BST level (first level is level 0)
        unsigned int BSTLevel = static_cast<unsigned int>(log(OT + 1) / log(2));

        // Traverse OT
        unsigned int OTChild = 1;
        unsigned int OTLevel = 0;
        while (OTLevel < numOTLevels_) {

            unsigned int OTNodesInLevel = static_cast<unsigned int>(pow(8, OTLevel));
            for (unsigned int i = 0; i<OTNodesInLevel; ++i) {

                // Brick index
                data_[OTNode*NUM_DATA + BRICK_INDEX] = (int)OTNode;

                // Error metrics
                //int localOTNode = (OTNode - OT*numOTNodes_);
                data_[OTNode*NUM_DATA + TEMPORAL_ERR] = static_cast<int>(numBSTLevels_ - 1 - BSTLevel);
                data_[OTNode*NUM_DATA + SPATIAL_ERR] = static_cast<int>(numOTLevels_ - 1 - OTLevel);

                if (BSTLevel == 0) {
                    // Calculate OT child index (-1 if node is leaf)
                    int OTChildIndex =
                        (OTChild < numOTNodes_) ? static_cast<int>(OT*numOTNodes_ + OTChild) : -1;
                    data_[OTNode*NUM_DATA + CHILD_INDEX] = OTChildIndex;
                }
                else {
                    // Calculate BST child index (-1 if node is BST leaf)

                    // First BST node of current level
                    int firstNode =
                        static_cast<unsigned int>((2 * pow(2, BSTLevel - 1) - 1)*numOTNodes_);
                    // First BST node of next level
                    int firstChild =
                        static_cast<unsigned int>((2 * pow(2, BSTLevel) - 1)*numOTNodes_);
                    // Difference between first nodes between levels
                    int levelGap = firstChild - firstNode;
                    // How many nodes away from the first node are we?
                    int offset = (OTNode - firstNode) / numOTNodes_;

                    // Use level gap and offset to calculate child index
                    int BSTChildIndex =
                        (BSTLevel < numBSTLevels_ - 1) ?
                        static_cast<int>(OTNode + levelGap + (offset*numOTNodes_)) : -1;

                    data_[OTNode*NUM_DATA + CHILD_INDEX] = BSTChildIndex;
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

    if (!_dataSSBO)
        glGenBuffers(1, &_dataSSBO);

    const size_t size = sizeof(GLint)*data_.size();
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _dataSSBO);
    //glBufferData(GL_SHADER_STORAGE_BUFFER, size, data_.data(), GL_DYNAMIC_READ);
    glBufferData(GL_SHADER_STORAGE_BUFFER, size, data_.data(), GL_STATIC_DRAW);
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
    return numTotalNodes_; 
}

unsigned int TSP::numValuesPerNode() const { 
    return NUM_DATA; 
}

unsigned int TSP::numBSTNodes() const { 
    return numBSTNodes_; 
}

unsigned int TSP::numBSTLevels() const {
    return numBSTLevels_;
}

unsigned int TSP::numOTNodes() const { 
    return numOTNodes_; 
}

unsigned int TSP::numOTLevels() const { 
    return numOTLevels_; 
}

unsigned int TSP::brickDim() const {
    return _header.xBrickDim_;
}

unsigned int TSP::paddedBrickDim() const {
    return paddedBrickDim_;
}

unsigned int TSP::numBricksPerAxis() const {
    return _header.xNumBricks_;
}

GLuint TSP::ssbo() const {
    return _dataSSBO;
}

bool TSP::calculateSpatialError() {
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
    //float medError = medianArray[medianArray.size()/2];

    /*
    LDEBUG("\nMin spatial std dev: " << minError);
    LDEBUG("Max spatial std dev: " << maxError);
    LDEBUG("Median spatial std dev: " << medError);
    LDEBUG("");
    */

    // "Normalize" errors
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<numTotalNodes_; ++i) {
        //float normalized = (stdDevs[i]-minError)/(maxError-minError);
        if (stdDevs[i] > 0.f) {
            stdDevs[i] = pow(stdDevs[i], 0.5f);
        }
        //data_[i*NUM_DATA + SPATIAL_ERR] = *reinterpret_cast<int*>(&stdDevs[i]);
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

bool TSP::calculateTemporalError() {

    if (!_file.is_open())
        return false;

    LDEBUG("Calculating temporal error");

    // Statistics
    //float minErr = 1e20f;
    //float maxErr = 0.f;
    std::vector<float> meanArray(numTotalNodes_);

    // Save errors
    std::vector<float> errors(numTotalNodes_);

    // Calculate temporal error for one brick at a time
    for (unsigned int brick = 0; brick<numTotalNodes_; ++brick) {

        unsigned int numBrickVals =
            paddedBrickDim_*paddedBrickDim_*paddedBrickDim_;

        // Save the individual voxel's average over timesteps. Because the
        // BSTs are built by averaging leaf nodes, we only need to sample
        // the brick at the correct coordinate.
        std::vector<float> voxelAverages(numBrickVals);
        std::vector<float> voxelStdDevs(numBrickVals);

        // Read the whole brick to fill the averages
        std::streampos offset = dataPosition() + static_cast<long long>(brick*numBrickVals*sizeof(float));
        _file.seekg(offset);

        _file.read(reinterpret_cast<char*>(&voxelAverages[0]),
            static_cast<size_t>(numBrickVals)*sizeof(float));

        // Build a list of the BST leaf bricks (within the same octree level) that
        // this brick covers
        std::list<unsigned int> coveredBricks = CoveredBSTLeafBricks(brick);

        // If the brick is at the lowest BST level, automatically set the error 
        // to -0.1 (enables using -1 as a marker for "no error accepted");
        // Somewhat ad hoc to get around the fact that the error could be
        // 0.0 higher up in the tree
        if (coveredBricks.size() == 1) {
            errors[brick] = -0.1f;
        } else {
            // Calculate standard deviation per voxel, average over brick
            float avgStdDev = 0.f;
            for (unsigned int voxel = 0; voxel<numBrickVals; ++voxel) {

                float stdDev = 0.f;
                for (auto leaf = coveredBricks.begin();
                    leaf != coveredBricks.end(); ++leaf) {

                    // Sample the leaves at the corresponding voxel position

                    std::streampos offset = dataPosition() + static_cast<long long>((*leaf*numBrickVals + voxel)*sizeof(float));
                    _file.seekg(offset);

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

        /*
        if (avgStdDev < minErr) {
        minErr = avgStdDev;
        } else if (avgStdDev > maxErr) {
        maxErr = avgStdDev;
        }
        */

    } // for all bricks

    std::sort(meanArray.begin(), meanArray.end());
    //float medErr = meanArray[meanArray.size()/2];

    /*
    LDEBUG("\nMin temporal error: " << minErr);
    LDEBUG("Max temporal error: " << maxErr);
    LDEBUG("Median temporal error: " << medErr);
    */

    // Adjust errors using user-provided exponents
    float minNorm = 1e20f;
    float maxNorm = 0.f;
    for (unsigned int i = 0; i<numTotalNodes_; ++i) {
        if (errors[i] > 0.f) {
            errors[i] = pow(errors[i], 0.25f);
        }
        //data_[i*NUM_DATA + TEMPORAL_ERR] = *reinterpret_cast<int*>(&errors[i]);
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


bool TSP::readCache() {

    if (!FileSys.cacheManager())
        return false;

    ghoul::filesystem::File f = _filename;
    std::string cacheFilename = FileSys.cacheManager()->cachedFilename(
        f.baseName(), "", ghoul::filesystem::CacheManager::Persistent::Yes);

    std::ifstream file(cacheFilename, std::ios::in | std::ios::binary);
    if (!file.is_open()) {
        LWARNING("Failed to open " << cacheFilename);
        return false;
    }


    file.read(reinterpret_cast<char*>(&minSpatialError_), sizeof(float));
    file.read(reinterpret_cast<char*>(&maxSpatialError_), sizeof(float));
    file.read(reinterpret_cast<char*>(&medianSpatialError_), sizeof(float));
    file.read(reinterpret_cast<char*>(&minTemporalError_), sizeof(float));
    file.read(reinterpret_cast<char*>(&maxTemporalError_), sizeof(float));
    file.read(reinterpret_cast<char*>(&medianTemporalError_), sizeof(float));
    size_t dataSize = static_cast<size_t>(numTotalNodes_*NUM_DATA)*sizeof(int);
    file.read(reinterpret_cast<char*>(&data_[0]), dataSize);
    file.close();

    LDEBUG("Cached errors:");
    LDEBUG("Min spatial error: " << minSpatialError_);
    LDEBUG("Max spatial error: " << maxSpatialError_);
    LDEBUG("Median spatial error: " << medianSpatialError_);
    LDEBUG("Min temporal error: " << minTemporalError_);
    LDEBUG("Max temporal error: " << maxTemporalError_);
    LDEBUG("Median temporal error: " << medianTemporalError_);

    return true;
}

bool TSP::writeCache() {

    if (!FileSys.cacheManager())
        return false;

    ghoul::filesystem::File f = _filename;
    std::string cacheFilename = FileSys.cacheManager()->cachedFilename(
        f.baseName(), "", ghoul::filesystem::CacheManager::Persistent::Yes);

    std::ofstream file(cacheFilename, std::ios::out | std::ios::binary);
    if (!file.is_open()) {
        LWARNING("Failed to open " << cacheFilename);
        return false;
    }
    LINFO("Writing cache to " << cacheFilename);


    file.write(reinterpret_cast<char*>(&minSpatialError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&maxSpatialError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&medianSpatialError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&minTemporalError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&maxTemporalError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&medianTemporalError_), sizeof(float));
    file.write(reinterpret_cast<char*>(&data_[0]), data_.size()*sizeof(float));

    file.close();

    /*
    LDEBUG("\nData:");
    for (unsigned i=0; i<data_.size()/NUM_DATA; ++i) {
    LDEBUG("Brick nr " << i);
    LDEBUG("Brick index " << data_[i*NUM_DATA + BRICK_INDEX]);
    LDEBUG("Child index " << data_[i*NUM_DATA + CHILD_INDEX]);
    LDEBUG("Spatial err " << *reinterpret_cast<float*>(&data_[i*NUM_DATA + SPATIAL_ERR]));
    LDEBUG("Temporal err " << *reinterpret_cast<float*>(&data_[i*NUM_DATA + TEMPORAL_ERR]));
    }
    */

    return true;
}

float TSP::getSpatialError(unsigned int _brickIndex) {
    return reinterpret_cast<float &>(data_[_brickIndex*NUM_DATA + SPATIAL_ERR]);
}

float TSP::getTemporalError(unsigned int _brickIndex) {
    return reinterpret_cast<float &>(data_[_brickIndex*NUM_DATA + TEMPORAL_ERR]);
}

unsigned int TSP::getFirstOctreeChild(unsigned int _brickIndex) {
    unsigned int otNode = _brickIndex % numOTNodes_;
    unsigned int bstOffset = _brickIndex - otNode;

    unsigned int depth = log(7 * otNode + 1) / log(8);
    unsigned int firstInLevel = (pow(8, depth) - 1) / 7;
    unsigned int levelOffset = otNode - firstInLevel;
    unsigned int firstInChildLevel = (pow(8, depth + 1) - 1) / 7;
    unsigned int childIndex = firstInChildLevel + 8*levelOffset;

    return bstOffset + childIndex;
}

unsigned int TSP::getBstLeft(unsigned int _brickIndex) {
    unsigned int bstNode = _brickIndex / numOTNodes_;
    unsigned int otOffset = _brickIndex % numOTNodes_;
    unsigned int depth = log(bstNode + 1) / log(2);
    unsigned int firstInLevel = pow(2, depth) - 1;
    unsigned int levelOffset = bstNode - firstInLevel;
    unsigned int firstInChildLevel = pow(2, depth + 1) - 1;
    unsigned int childIndex = firstInChildLevel + 2*levelOffset;
    return otOffset + childIndex * numOTNodes_;
}

unsigned int TSP::getBstRight(unsigned int _brickIndex) {
    return getBstLeft(_brickIndex) + numOTNodes_;
}

bool TSP::isBstLeaf(unsigned int _brickIndex) {
    unsigned int bstNode = _brickIndex / numOTNodes_;
    return bstNode >= numBSTNodes_ / 2;
}

bool TSP::isOctreeLeaf(unsigned int _brickIndex) {
    unsigned int otNode = _brickIndex % numOTNodes_;
    unsigned int depth = log(7 * otNode + 1) / log(8);
    return depth == numOTLevels_ - 1;
}


std::list<unsigned int> TSP::CoveredLeafBricks(unsigned int _brickIndex) {
    std::list<unsigned int> out;

    // Find what octree skeleton node the index belongs to
    unsigned int OTNode = _brickIndex % numOTNodes_;

    // Find what BST node the index corresponds to using int division
    unsigned int BSTNode = _brickIndex / numOTNodes_;
    // Calculate BST offset (to translate to root octree)
    unsigned int BSTOffset = BSTNode * numOTNodes_;

    // Traverse root octree structure to leaves
    // When visiting the leaves, translate back to correct BST level and save
    std::queue<unsigned int> queue;
    queue.push(OTNode);
    do {

        // Get front of queue and pop it
        unsigned int toVisit = queue.front();
        queue.pop();

        // See if the node has children
        int child = data_[toVisit*NUM_DATA + CHILD_INDEX];
        if (child == -1) {
            // Translate back and save
            out.push_back(toVisit + BSTOffset);
        }
        else {
            // Queue the eight children
            for (int i = 0; i<8; ++i) {
                queue.push(child + i);
            }
        }

    } while (!queue.empty());

    return out;
}

std::list<unsigned int> TSP::CoveredBSTLeafBricks(unsigned int _brickIndex) {
    std::list<unsigned int> out;

    // Traverse the BST children until we are at root
    std::queue<unsigned int> queue;
    queue.push(_brickIndex);
    do {

        unsigned int toVisit = queue.front();
        queue.pop();

        bool BSTRoot = toVisit < numOTNodes_;
        if (BSTRoot) {
            if (numBSTLevels_ == 1) {
                out.push_back(toVisit);
            }
            else {
                queue.push(toVisit + numOTNodes_);
                queue.push(toVisit + numOTNodes_ * 2);
            }
        }
        else {
            int child = data_[toVisit*NUM_DATA + CHILD_INDEX];
            if (child == -1) {
                // Save leaf brick to list
                out.push_back(toVisit);
            }
            else {
                // Queue children
                queue.push(child);
                queue.push(child + numOTNodes_);
            }
        }

    } while (!queue.empty());

    return out;
}

}
