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
#include <modules/multiresvolume/rendering/timebrickselector.h>
#include <modules/multiresvolume/rendering/errorhistogrammanager.h>
#include <openspace/util/histogram.h>
#include <openspace/rendering/transferfunction.h>
#include <algorithm>
#include <cassert>

namespace {
    const std::string _loggerCat = "TimeBrickSelector";
}

namespace openspace {
TimeBrickSelector::TimeBrickSelector(std::shared_ptr<TSP> tsp, ErrorHistogramManager* hm, TransferFunction* tf, int memoryBudget, int streamingBudget)
    : TfBrickSelector(tsp, hm, tf, memoryBudget, streamingBudget) { }

TimeBrickSelector::~TimeBrickSelector() {}

void TimeBrickSelector::selectBricks(int timestep, std::vector<int>& bricks) {
    const unsigned int numTimeSteps = _tsp->header().numTimesteps_;
    const unsigned int numBricksPerDim = _tsp->header().xNumBricks_;
    const unsigned int memoryBudget = _memoryBudget;
    const unsigned int totalStreamingBudget = _streamingBudget * numTimeSteps;

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
    }
    else {
        leafSelections.push_back(brickSelection);
    }

    int nBricksInMemory = 1;
    int nStreamedBricks = 1;

    // First loop: While neither the memory nor the streaming budget is reached,
    // try to optimize for visual quality vs memory.

    while (nBricksInMemory <= memoryBudget - 7 && priorityQueue.size() > 0) {

        std::pop_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);

        BrickSelection bs = priorityQueue.back();
        unsigned int brickIndex = bs.brickIndex;
        priorityQueue.pop_back();

        // TODO: handle edge case when we can only afford temporal splits or no split (only 1 spot left)
        if (bs.splitType == BrickSelection::SplitType::Temporal) {
            unsigned int childBrickIndex;
            bool pickRightTimeChild = bs.timestepInRightChild(timestep);

            // On average on the whole time period, splitting this spatial brick in two time steps
            // would generate twice as much streaming. Current number of streams of this spatial brick
            // is 2^nTemporalSplits over the whole time period.

            int newStreams = std::pow(2, bs.nTemporalSplits);

            // Refining this one more step would require the double amount of streams
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // Reached dead end (streaming budget would be exceeded)
                deadEnds.push_back(bs);
                break;
            }
            nStreamedBricks += newStreams;

            if (pickRightTimeChild) {
                childBrickIndex = _tsp->getBstRight(brickIndex);
            }
            else {
                childBrickIndex = _tsp->getBstLeft(brickIndex);
            }

            BrickSelection::SplitType childSplitType;
            float childSplitPoints = splitPoints(childBrickIndex, childSplitType);
            BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, childSplitType, childSplitPoints);
            if (childSplitType != BrickSelection::SplitType::None) {
                priorityQueue.push_back(childSelection);
                std::push_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);
            }
            else {
                leafSelections.push_back(childSelection);
            }
        }
        else if (bs.splitType == BrickSelection::SplitType::Spatial) {
            nBricksInMemory += 7; // Remove one and add eight.
            const unsigned int firstChild = _tsp->getFirstOctreeChild(brickIndex);

            // On average on the whole time period, splitting this spatial brick into eight spatial bricks
            // would generate eight times as much streaming. Current number of streams of this spatial brick
            // is 2^nTemporalStreams over the whole time period.
            int newStreams = 7 * std::pow(2, bs.nTemporalSplits);

            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // Reached dead end (streaming budget would be exceeded)
                // However, temporal split might be possible
                    if (bs.splitType != BrickSelection::SplitType::Temporal) {
                    bs.splitType = BrickSelection::SplitType::Temporal;
                    bs.splitPoints = temporalSplitPoints(bs.brickIndex);
                }
                if (bs.splitPoints > -1) {
                    temporalSplitQueue.push_back(bs);
                }
                else {
                    deadEnds.push_back(bs);
                }
                break;
            }
            nStreamedBricks += newStreams;

            for (unsigned int i = 0; i < 8; i++) {
                unsigned int childBrickIndex = firstChild + i;

                BrickSelection::SplitType childSplitType;
                float childSplitPoints = splitPoints(childBrickIndex, childSplitType);
                BrickSelection childSelection = bs.splitSpatially(i % 2, (i / 2) % 2, i / 4, childBrickIndex, childSplitType, childSplitPoints);

                if (childSplitType != BrickSelection::SplitType::None) {
                    priorityQueue.push_back(childSelection);
                    std::push_heap(priorityQueue.begin(), priorityQueue.end(), BrickSelection::compareSplitPoints);
                }
                else {
                    leafSelections.push_back(childSelection);
                }
            }
        }
    }

    if (nBricksInMemory <= memoryBudget - 7) {
        std::cout << "memory budget not reached. " << nBricksInMemory << " out of " << memoryBudget << std::endl;
    }

    // Is it possible that we may stream more bricks?
    if (nStreamedBricks < totalStreamingBudget - 1) {

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
            }
            else {
                deadEnds.push_back(bs);
            }
        }

        // Keep splitting until it's not possible anymore
        while (nStreamedBricks < totalStreamingBudget - 1 && temporalSplitQueue.size() > 0) {
            std::pop_heap(temporalSplitQueue.begin(), temporalSplitQueue.end(), BrickSelection::compareSplitPoints);
            BrickSelection bs = temporalSplitQueue.back();
            temporalSplitQueue.pop_back();

            unsigned int brickIndex = bs.brickIndex;
            int newStreams = std::pow(2, bs.nTemporalSplits);
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
            }
            else {
                childBrickIndex = _tsp->getBstLeft(brickIndex);
            }

            float childSplitPoints = temporalSplitPoints(childBrickIndex);

            if (childSplitPoints > -1) {
                BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, BrickSelection::SplitType::Temporal, childSplitPoints);
                temporalSplitQueue.push_back(childSelection);
                std::push_heap(temporalSplitQueue.begin(), temporalSplitQueue.end(), BrickSelection::compareSplitPoints);
            }
            else {
                BrickSelection childSelection = bs.splitTemporally(pickRightTimeChild, childBrickIndex, BrickSelection::SplitType::None, -1);
                deadEnds.push_back(childSelection);
            }
        }
    }
    else {
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
}

float TimeBrickSelector::splitPoints(unsigned int brickIndex, BrickSelection::SplitType& splitType) {
    float temporalPoints = temporalSplitPoints(brickIndex);
    float spatialPoints = spatialSplitPoints(brickIndex);
    float splitPoints;

    splitType = BrickSelection::SplitType::None;
    splitPoints = -1;

    if (spatialPoints > 0 && spatialPoints > temporalPoints) {
        splitType = BrickSelection::SplitType::Spatial;
        return spatialPoints;
    } else if (temporalPoints > 0) {
        splitType = BrickSelection::SplitType::Temporal;
        return temporalPoints;
    }

    return splitPoints;
}

bool TimeBrickSelector::calculateBrickErrors() {
    TransferFunction *tf = _transferFunction;
    if (!tf) return false;

    size_t tfWidth = tf->width();
    if (tfWidth <= 0) return false;

    std::vector<float> gradients(tfWidth - 1);
    for (size_t offset = 0; offset < tfWidth - 1; offset++) {
        glm::vec4 prevRgba = tf->sample(offset);
        glm::vec4 nextRgba = tf->sample(offset + 1);

        float colorDifference = glm::distance(prevRgba, nextRgba);
        float alpha = (prevRgba.w + nextRgba.w) * 0.5;

        gradients[offset] = colorDifference*alpha;
    }



    unsigned int nHistograms = _tsp->numTotalNodes();
    _brickErrors = std::vector<float>(nHistograms);

    for (unsigned int brickIndex = 0; brickIndex < nHistograms; brickIndex++) {
        if (_tsp->isBstLeaf(brickIndex) && _tsp->isOctreeLeaf(brickIndex)) {
            _brickErrors[brickIndex] = 0;
        }
        else {
            const Histogram* histogram = _histogramManager->getHistogram(brickIndex);
            float error = 0;
            for (int i = 0; i < gradients.size(); i++) {
                float x = (i + 0.5) / tfWidth;
                float sample = histogram->interpolate(x);
                assert(sample >= 0);
                assert(gradients[i] >= 0);
                error += sample * gradients[i];
            }
            _brickErrors[brickIndex] = error;
        }
    }

    return true;
}

} // namespace openspace
