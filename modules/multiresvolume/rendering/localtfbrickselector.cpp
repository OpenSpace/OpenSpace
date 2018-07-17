/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/multiresvolume/rendering/localtfbrickselector.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <modules/multiresvolume/rendering/localerrorhistogrammanager.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/misc/assert.h>

namespace {
    bool compareSplitPoints(const openspace::BrickSelection& a,
                            const openspace::BrickSelection& b)
    {
        return a.splitPoints < b.splitPoints;
    }
} // namespace

namespace openspace {

LocalTfBrickSelector::LocalTfBrickSelector(TSP* tsp, LocalErrorHistogramManager* hm,
                                           TransferFunction* tf, int memoryBudget,
                                           int streamingBudget)
    : _tsp(tsp)
    , _histogramManager(hm)
    , _transferFunction(tf)
    , _memoryBudget(memoryBudget)
    , _streamingBudget(streamingBudget)
{}

bool LocalTfBrickSelector::initialize() {
    return calculateBrickErrors();
}

void LocalTfBrickSelector::setMemoryBudget(int memoryBudget) {
    _memoryBudget = memoryBudget;
}

void LocalTfBrickSelector::setStreamingBudget(int streamingBudget) {
    _streamingBudget = streamingBudget;
}

void LocalTfBrickSelector::selectBricks(int timestep, std::vector<int>& bricks) {
    const int numTimeSteps = _tsp->header().numTimesteps;
    const int numBricksPerDim = _tsp->header().xNumBricks;

    unsigned int rootNode = 0;
    BrickSelection::SplitType splitType;
    const float rootSplitPoints = splitPoints(rootNode, splitType);
    BrickSelection brickSelection = BrickSelection(
        numBricksPerDim,
        numTimeSteps,
        splitType,
        rootSplitPoints
    );

    std::vector<BrickSelection> priorityQueue;
    std::vector<BrickSelection> leafSelections;
    std::vector<BrickSelection> temporalSplitQueue;
    std::vector<BrickSelection> deadEnds;

    if (splitType != BrickSelection::SplitType::None) {
        priorityQueue.push_back(brickSelection);
    } else {
        leafSelections.push_back(brickSelection);
    }

    int totalStreamingBudget = _streamingBudget * numTimeSteps;
    int nBricksInMemory = 1;
    int nStreamedBricks = 1;

    while (nBricksInMemory <= _memoryBudget - 7 && priorityQueue.size() > 0) {
        std::pop_heap(priorityQueue.begin(), priorityQueue.end(), compareSplitPoints);
        BrickSelection bs = priorityQueue.back();

        // TODO: handle edge case when we can only afford temporal splits or
        //       no split (only 1 spot left)

        unsigned int brickIndex = bs.brickIndex;
        priorityQueue.pop_back();
        if (bs.splitType == BrickSelection::SplitType::Temporal) {
//            int timeSpanCenter = bs.centerT();
            bool pickRightTimeChild = bs.timestepInRightChild(timestep);

            // On average on the whole time period, splitting this spatial brick in two
            // time steps would generate twice as much streaming. Current number of
            // streams of this spatial brick is 2^nTemporalSplits over the whole time
            // period.
            int newStreams = static_cast<int>(std::pow(2, bs.nTemporalSplits));
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // Reached dead end (streaming budget would be exceeded)
                deadEnds.push_back(bs);
                break;
            }
            nStreamedBricks += newStreams;

            const unsigned int childBrickIndex = pickRightTimeChild ?
                _tsp->bstRight(brickIndex) :
                _tsp->bstLeft(brickIndex);

            BrickSelection::SplitType childSplitType;
            float childSplitPoints = splitPoints(childBrickIndex, childSplitType);
            BrickSelection childSelection = bs.splitTemporally(
                pickRightTimeChild,
                childBrickIndex,
                childSplitType,
                childSplitPoints
            );

            if (childSplitType != BrickSelection::SplitType::None) {
                priorityQueue.push_back(childSelection);
                std::push_heap(
                    priorityQueue.begin(),
                    priorityQueue.end(),
                    compareSplitPoints
                );
            } else {
                leafSelections.push_back(childSelection);
            }
        } else if (bs.splitType == BrickSelection::SplitType::Spatial) {
            nBricksInMemory += 7; // Remove one and add eight.
            const unsigned int firstChild = _tsp->firstOctreeChild(brickIndex);

            // On average on the whole time period, splitting this spatial brick into
            // eight spatial bricks would generate eight times as much streaming. Current
            // number of streams of this spatial brick is 2^nTemporalStreams over the
            // whole time period.
            const int newStreams = 7 * static_cast<int>(std::pow(2, bs.nTemporalSplits));
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
                const unsigned int childBrickIndex = firstChild + i;

                BrickSelection::SplitType childSplitType;
                const float childSplitPoints = splitPoints(
                    childBrickIndex,
                    childSplitType
                );
                BrickSelection childSelection = bs.splitSpatially(
                    i % 2,
                    (i/2) % 2,
                    i/4,
                    childBrickIndex,
                    childSplitType,
                    childSplitPoints
                );

                if (childSplitType != BrickSelection::SplitType::None) {
                    priorityQueue.push_back(childSelection);
                    std::push_heap(
                        priorityQueue.begin(),
                        priorityQueue.end(),
                        compareSplitPoints
                    );
                } else {
                    leafSelections.push_back(childSelection);
                }
            }
        }
    }

    // Is it possible that we may stream more bricks?
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
                std::push_heap(
                    temporalSplitQueue.begin(),
                    temporalSplitQueue.end(),
                    compareSplitPoints
                );
            } else {
                deadEnds.push_back(bs);
            }
        }

        while (nStreamedBricks < totalStreamingBudget - 1 &&
               temporalSplitQueue.size() > 0)
        {
            std::pop_heap(
                temporalSplitQueue.begin(),
                temporalSplitQueue.end(),
                compareSplitPoints
            );
            BrickSelection bs = temporalSplitQueue.back();
            temporalSplitQueue.pop_back();

            const unsigned int brickIndex = bs.brickIndex;
            const int newStreams = static_cast<int>(std::pow(2, bs.nTemporalSplits));
            if (nStreamedBricks + newStreams > totalStreamingBudget) {
                // The current best choice would make us exceed the streaming budget, try
                // next instead.
                deadEnds.push_back(bs);
                continue;
            }

            nStreamedBricks += newStreams;
            unsigned int childBrickIndex;
            bool pickRightTimeChild = bs.timestepInRightChild(timestep);

            if (pickRightTimeChild) {
                childBrickIndex = _tsp->bstRight(brickIndex);
            } else {
                childBrickIndex = _tsp->bstLeft(brickIndex);
            }

            float childSplitPoints = temporalSplitPoints(childBrickIndex);

            if (childSplitPoints > -1) {
                const BrickSelection childSelection = bs.splitTemporally(
                    pickRightTimeChild,
                    childBrickIndex,
                    BrickSelection::SplitType::Temporal,
                    childSplitPoints
                );
                temporalSplitQueue.push_back(childSelection);
                std::push_heap(
                    temporalSplitQueue.begin(),
                    temporalSplitQueue.end(),
                    compareSplitPoints
                );
            } else {
                BrickSelection childSelection = bs.splitTemporally(
                    pickRightTimeChild,
                    childBrickIndex,
                    BrickSelection::SplitType::None,
                    -1
                );
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
    // Write dead end nodes to brickSelection vector
    for (const BrickSelection& bs : deadEnds) {
        writeSelection(bs, bricks);
    }
    // Write selected leaf nodes to brickSelection vector
    for (const BrickSelection& bs : leafSelections) {
        writeSelection(bs, bricks);
    }
}

float LocalTfBrickSelector::temporalSplitPoints(unsigned int brickIndex) const {
    if (_tsp->isBstLeaf(brickIndex)) {
        return -1;
    }
    return _brickErrors[brickIndex].temporal * 0.5f;
}

float LocalTfBrickSelector::spatialSplitPoints(unsigned int brickIndex) const {
    if (_tsp->isOctreeLeaf(brickIndex)) {
        return -1;
    }
    return _brickErrors[brickIndex].spatial * 0.125f;
}

float LocalTfBrickSelector::splitPoints(unsigned int brickIndex,
                                        BrickSelection::SplitType& splitType)
{
    const float temporalPoints = temporalSplitPoints(brickIndex);
    const float spatialPoints = spatialSplitPoints(brickIndex);

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

bool LocalTfBrickSelector::calculateBrickErrors() {
    TransferFunction* tf = _transferFunction;
    if (!tf) {
        return false;
    }

    size_t tfWidth = tf->width();
    if (tfWidth <= 0) {
        return false;
    }

    std::vector<float> gradients(tfWidth - 1);
    for (size_t offset = 0; offset < tfWidth - 1; offset++) {
        const glm::vec4 prevRgba = tf->sample(offset);
        const         glm::vec4 nextRgba = tf->sample(offset + 1);

        const float colorDifference = glm::distance(prevRgba, nextRgba);
        const float alpha = (prevRgba.w + nextRgba.w) * 0.5f;

        gradients[offset] = colorDifference*alpha;
    }

    const unsigned int nHistograms = _tsp->numTotalNodes();
    _brickErrors = std::vector<Error>(nHistograms);

    for (unsigned int brickIndex = 0; brickIndex < nHistograms; brickIndex++) {
        if (_tsp->isOctreeLeaf(brickIndex)) {
            _brickErrors[brickIndex].spatial = 0.0;
        } else {
            const Histogram* histogram = _histogramManager->spatialHistogram(
                brickIndex
            );
            float error = 0;
            for (size_t i = 0; i < gradients.size(); i++) {
                float x = (i + 0.5f) / tfWidth;
                float sample = histogram->interpolate(x);
                ghoul_assert(sample >= 0, "@MISSING");
                ghoul_assert(gradients[i] >= 0, "@MISSING");
                error += sample * gradients[i];
            }
            _brickErrors[brickIndex].spatial = error;
        }

        if (_tsp->isBstLeaf(brickIndex)) {
            _brickErrors[brickIndex].temporal = 0.0;
        } else {
            const Histogram* histogram = _histogramManager->temporalHistogram(
                brickIndex
            );
            float error = 0;
            for (size_t i = 0; i < gradients.size(); i++) {
                float x = (i + 0.5f) / tfWidth;
                float sample = histogram->interpolate(x);
                ghoul_assert(sample >= 0, "@MISSING");
                ghoul_assert(gradients[i] >= 0, "@MISSING");
                error += sample * gradients[i];
            }
            _brickErrors[brickIndex].temporal = error;
        }
    }

    return true;
}

int LocalTfBrickSelector::linearCoordinates(int x, int y, int z) const {
    const TSP::Header &header = _tsp->header();
    return x + (header.xNumBricks * y) + (header.xNumBricks * header.yNumBricks * z);
}

void LocalTfBrickSelector::writeSelection(BrickSelection brickSelection,
                                          std::vector<int>& bricks)
{
    BrickCover coveredBricks = brickSelection.cover;
    for (int z = coveredBricks.lowZ; z < coveredBricks.highZ; z++) {
        for (int y = coveredBricks.lowY; y < coveredBricks.highY; y++) {
            for (int x = coveredBricks.lowX; x < coveredBricks.highX; x++) {
                bricks[linearCoordinates(x, y, z)] = brickSelection.brickIndex;
            }
        }
    }
}

} // namespace openspace
