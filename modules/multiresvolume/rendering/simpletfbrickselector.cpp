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

#include <modules/multiresvolume/rendering/tsp.h>
#include <modules/multiresvolume/rendering/simpletfbrickselector.h>
#include <modules/multiresvolume/rendering/histogrammanager.h>
#include <openspace/util/histogram.h>
#include <openspace/rendering/transferfunction.h>
#include <algorithm>
#include <cassert>
#include <ghoul/logging/logmanager.h>

namespace {
    const char* _loggerCat = "SimpleTfBrickSelector";
} // namespace

namespace openspace {

SimpleTfBrickSelector::SimpleTfBrickSelector(TSP* tsp, HistogramManager* hm,
                                             TransferFunction* tf, int memoryBudget,
                                             int streamingBudget)
    : _tsp(tsp)
    , _histogramManager(hm)
    , _transferFunction(tf)
    , _memoryBudget(memoryBudget)
    , _streamingBudget(streamingBudget) {}

SimpleTfBrickSelector::~SimpleTfBrickSelector() {}

bool SimpleTfBrickSelector::initialize() {
    return true;
}

void SimpleTfBrickSelector::setMemoryBudget(int memoryBudget) {
    _memoryBudget = memoryBudget;
}

void SimpleTfBrickSelector::setStreamingBudget(int streamingBudget) {
    _streamingBudget = streamingBudget;
}

void SimpleTfBrickSelector::selectBricks(int timestep, std::vector<int>& bricks) {
    int numTimeSteps = _tsp->header().numTimesteps_;
    int numBricksPerDim = _tsp->header().xNumBricks_;

    unsigned int rootNode = 0;
    BrickSelection::SplitType splitType;
    float rootSplitPoints = splitPoints(rootNode, splitType);

    BrickSelection brickSelection = BrickSelection(numBricksPerDim, numTimeSteps, splitType, rootSplitPoints);

    std::vector<BrickSelection> priorityQueue;
    std::vector<BrickSelection> leafSelections;
    std::vector<BrickSelection> temporalSplitQueue;
    std::vector<BrickSelection> deadEnds;

    if (splitType != BrickSelection::SplitType::None) {
        priorityQueue.push_back(brickSelection);
    } else {
        leafSelections.push_back(brickSelection);
    }

    int memoryBudget = _memoryBudget;
    int totalStreamingBudget = _streamingBudget * numTimeSteps;
    int nBricksInMemory = 1;
    int nStreamedBricks = 1;

    while (nBricksInMemory <= memoryBudget - 7 && priorityQueue.size() > 0) {
        std::pop_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);
        BrickSelection bs = priorityQueue.back();

        // TODO: handle edge case when we can only afford temporal splits or no split (only 1 spot left)

        unsigned int brickIndex = bs.brickIndex;
        priorityQueue.pop_back();
        if (bs.splitType == BrickSelection::SplitType::Temporal) {
            unsigned int childBrickIndex;
            bool pickRightTimeChild = bs.timestepInRightChild(timestep);

            // On average on the whole time period, splitting this spatial brick in two time steps
            // would generate twice as much streaming. Current number of streams of this spatial brick
            // is 2^nTemporalSplits over the whole time period.
            int newStreams = static_cast<int>(std::pow(2, bs.nTemporalSplits));

            // Refining this one more step would require the double amount of streams
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // Reached dead end (streaming budget would be exceeded)
                deadEnds.push_back(bs);
                break;
            }
            nStreamedBricks += newStreams;

            if (pickRightTimeChild) {
                childBrickIndex = _tsp->getBstRight(brickIndex);
            } else {
                childBrickIndex = _tsp->getBstLeft(brickIndex);
            }

            BrickSelection::SplitType childSplitType;
            float childSplitPoints = splitPoints(childBrickIndex, childSplitType);
            BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, childSplitType, childSplitPoints);

            if (childSplitType != BrickSelection::SplitType::None) {
                priorityQueue.push_back(childSelection);
                std::push_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);
            } else {
                leafSelections.push_back(childSelection);
            }
        } else if (bs.splitType == BrickSelection::SplitType::Spatial) {
            nBricksInMemory += 7; // Remove one and add eight.
            unsigned int firstChild = _tsp->getFirstOctreeChild(brickIndex);

            // On average on the whole time period, splitting this spatial brick into eight spatial bricks
            // would generate eight times as much streaming. Current number of streams of this spatial brick
            // is 2^nTemporalStreams over the whole time period.
            int newStreams = 7 * static_cast<int>(std::pow(2, bs.nTemporalSplits));
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // Reached dead end (streaming budget would be exceeded)
                // However, temporal split might be possible
                if (bs.splitType != BrickSelection::SplitType::Temporal) {
                    bs.splitType = BrickSelection::SplitType::Temporal;
                    bs.splitPoints = temporalSplitPoints(bs.brickIndex);
                }
                if (bs.splitPoints > -1) {
                    temporalSplitQueue.push_back(bs);
                } else {
                    deadEnds.push_back(bs);
                }
                break;
            }
            nStreamedBricks += newStreams;

            for (unsigned int i = 0; i < 8; i++) {
                unsigned int childBrickIndex = firstChild + i;

                BrickSelection::SplitType childSplitType;
                float childSplitPoints = splitPoints(childBrickIndex, childSplitType);
                BrickSelection childSelection = bs.splitSpatially(i % 2, (i/2) % 2, i/4, childBrickIndex, childSplitType, childSplitPoints);

                if (childSplitType != BrickSelection::SplitType::None) {
                    priorityQueue.push_back(childSelection);
                    std::push_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);
                } else {
                    leafSelections.push_back(childSelection);
                }
            }
        }
    }

    if (nStreamedBricks < totalStreamingBudget) {
        while (priorityQueue.size() > 0) {
            BrickSelection bs = priorityQueue.back();
            if (bs.splitType != BrickSelection::SplitType::Temporal) {
                bs.splitType = BrickSelection::SplitType::Temporal;
                bs.splitPoints = temporalSplitPoints(bs.brickIndex);
            }
            priorityQueue.pop_back();
            if (bs.splitPoints > -1) {
                temporalSplitQueue.push_back(bs);
                std::push_heap(temporalSplitQueue.begin(), temporalSplitQueue.end(), BrickSelection::compareSplitPoints);
            } else {
                deadEnds.push_back(bs);
            }
        }

        // Keep splitting until it's not possible anymore
        while (nStreamedBricks < totalStreamingBudget - 1 && temporalSplitQueue.size() > 0) {
            std::pop_heap(temporalSplitQueue.begin(), temporalSplitQueue.end(), BrickSelection::compareSplitPoints);
            BrickSelection bs = temporalSplitQueue.back();
            temporalSplitQueue.pop_back();

            unsigned int brickIndex = bs.brickIndex;
            int newStreams = static_cast<int>(std::pow(2, bs.nTemporalSplits));
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // The current best choice would make us exceed the streaming budget, try next instead.
                deadEnds.push_back(bs);
                continue;
            }

            nStreamedBricks += newStreams;
            unsigned int childBrickIndex;
            bool pickRightTimeChild = bs.timestepInRightChild(timestep);

            if (pickRightTimeChild) {
                childBrickIndex = _tsp->getBstRight(brickIndex);
            } else {
                childBrickIndex = _tsp->getBstLeft(brickIndex);
            }

            float childSplitPoints = temporalSplitPoints(childBrickIndex);

            if (childSplitPoints > -1) {
                BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, BrickSelection::SplitType::Temporal, childSplitPoints);
                temporalSplitQueue.push_back(childSelection);
                std::push_heap(temporalSplitQueue.begin(), temporalSplitQueue.end(), BrickSelection::compareSplitPoints);
            } else {
                BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, BrickSelection::SplitType::None, -1);
                deadEnds.push_back(childSelection);
            }
        }
    } else {
        // Write selected inner nodes to brickSelection vector
        for (const BrickSelection& bs : priorityQueue) {
            writeSelection(bs, bricks);
        } 
    }

    // Write selected inner nodes to brickSelection vector
    for (const BrickSelection& bs : temporalSplitQueue) {
        writeSelection(bs, bricks);
    }
    for (const BrickSelection& bs : deadEnds) {
        writeSelection(bs, bricks);
    }
    // Write selected leaf nodes to brickSelection vector
    for (const BrickSelection& bs : leafSelections) {
        writeSelection(bs, bricks);
    }

    //std::cout << "Bricks in memory: " << nBricksInMemory << "/" << _memoryBudget << "___\t\t"
    //          << "Streamed bricks:  " << nStreamedBricks << "/" << totalStreamingBudget << std::flush << "___\r";
}

float SimpleTfBrickSelector::temporalSplitPoints(unsigned int brickIndex) {
    if (_tsp->isBstLeaf(brickIndex)) {
        return -1;
    }
    return _brickImportances[brickIndex] * 0.5f;
}

float SimpleTfBrickSelector::spatialSplitPoints(unsigned int brickIndex) {
    if (_tsp->isOctreeLeaf(brickIndex)) {
        return -1;
    }
    return _brickImportances[brickIndex] * 0.125f;
}

float SimpleTfBrickSelector::splitPoints(unsigned int brickIndex, BrickSelection::SplitType& splitType) {
    float temporalPoints = temporalSplitPoints(brickIndex);
    float spatialPoints = spatialSplitPoints(brickIndex);
    float splitPoints;

    if (spatialPoints > 0 && spatialPoints > temporalPoints) {
        splitPoints = spatialPoints;
        splitType = BrickSelection::SplitType::Spatial;
    } else if (temporalPoints > 0) {
        splitPoints = temporalPoints;
        splitType = BrickSelection::SplitType::Temporal;
    } else {
        splitPoints = -1;
        splitType = BrickSelection::SplitType::None;
    }

    return splitPoints;
}


bool SimpleTfBrickSelector::calculateBrickImportances() {
    TransferFunction *tf = _transferFunction;
    if (!tf) return false;

    size_t tfWidth = tf->width();
    
    // By changing tfWidth to the correct type size_t, this check is no longer valid since
    // size_t is unsigned ---abock
    //if (tfWidth <= 0) return false;

    /*    std::vector<float> gradients(tfWidth - 1);
    for (size_t offset = 0; offset < tfWidth - 1; offset++) {
        glm::vec4 prevRgba = tf->sample(offset);
        glm::vec4 nextRgba = tf->sample(offset + 1);

        float colorDifference = glm::distance(prevRgba, nextRgba);
        float alpha = (prevRgba.w + nextRgba.w) * 0.5;

        gradients[offset] = colorDifference*alpha;
        }*/

    unsigned int nHistograms = _tsp->numTotalNodes();
    _brickImportances = std::vector<float>(nHistograms);

    for (unsigned int brickIndex = 0; brickIndex < nHistograms; brickIndex++) {
        const Histogram* histogram = _histogramManager->getHistogram(brickIndex);
        if (!histogram->isValid()) {
            return false;
        }

        float dotProduct = 0;
        for (int i = 0; i < tf->width(); i++) {
            float x = static_cast<float>(i) / static_cast<float>(tfWidth);
            float sample = histogram->interpolate(x);

            assert(sample >= 0);
            dotProduct += sample * tf->sample(i).w;
        }
        _brickImportances[brickIndex] = dotProduct;
    }

    LINFO("Updated brick importances");

    return true;
}

int SimpleTfBrickSelector::linearCoords(int x, int y, int z) {
    const TSP::Header &header = _tsp->header();
    return x + (header.xNumBricks_ * y) + (header.xNumBricks_ * header.yNumBricks_ * z);
}

void SimpleTfBrickSelector::writeSelection(BrickSelection brickSelection, std::vector<int>& bricks) {
    BrickCover coveredBricks = brickSelection.cover;
    for (int z = coveredBricks.lowZ; z < coveredBricks.highZ; z++) {
        for (int y = coveredBricks.lowY; y < coveredBricks.highY; y++) {
            for (int x = coveredBricks.lowX; x < coveredBricks.highX; x++) {
                bricks[linearCoords(x, y, z)] = brickSelection.brickIndex;
            }
        }
    }
}


} // namespace openspace
