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

#include <modules/multiresvolume/rendering/brickselector.h>
#include <modules/multiresvolume/rendering/brickselection.h>
#include <modules/multiresvolume/rendering/errorhistogrammanager.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/histogram.h>
#include <algorithm>
#include <cassert>

namespace {
    const std::string _loggerCat = "BrickSelector";
}

namespace openspace {
BrickSelector::BrickSelector() {}

BrickSelector::BrickSelector(int memoryBudget, int streamingBudget)
    : _memoryBudget(memoryBudget), _streamingBudget(streamingBudget) {};

BrickSelector::~BrickSelector() {}

bool BrickSelector::initialize() { return true; }

void BrickSelector::setMemoryBudget(int memoryBudget) { _memoryBudget = memoryBudget; };

void BrickSelector::setStreamingBudget(int streamingBudget) { _streamingBudget = streamingBudget; };

int BrickSelector::getTimeStepCount() {
    return 0;
}

int BrickSelector::getBrickCount() {
    return 0;
}

float BrickSelector::getRank() {
    return -1.;
}

void BrickSelector::selectBricks(int timestep, std::vector<int>& bricks) {
    int numTimeSteps = getTimeStepCount();
    int numBricksPerDim = getBrickCount();

    unsigned int rootNode = 0;
    BrickSelection::SplitType splitType = BrickSelection::SplitType::None;
    float rootSplitPoints = getRank();


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

    // First loop: While neither the memory nor the streaming budget is reached,
    // try to optimize for visual quality vs memory.

    while (nBricksInMemory <= memoryBudget - 7 && priorityQueue.size() > 0) {

    }
}

} // namespace openspace
