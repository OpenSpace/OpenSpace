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

#include <modules/multiresvolume/rendering/tspbrickselector.h>
#include <openspace/rendering/transferfunction.h>
#include <algorithm>
#include <cassert>
#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "TSPBrickSelector";
}

namespace openspace {
    TSPBrickSelector::TSPBrickSelector()
    : BrickSelector(0, 0) {}

    TSPBrickSelector::TSPBrickSelector(TSP* tsp)
    : BrickSelector(0, 0)
    , _tsp(tsp) {}

    TSPBrickSelector::TSPBrickSelector(TSP* tsp, int memoryBudget, int streamingBudget)
    : BrickSelector(memoryBudget, streamingBudget)
    , _tsp(tsp) {}

    TSPBrickSelector::TSPBrickSelector(TSP* tsp, TransferFunction* tf, int memoryBudget, int streamingBudget)
    : BrickSelector(memoryBudget, streamingBudget)
        , _tsp(tsp)
        ,_transferFunction(tf) {}

TSPBrickSelector::~TSPBrickSelector() {}

int TSPBrickSelector::linearCoords(int x, int y, int z) {
    const TSP::Header &header = _tsp->header();
    return x + (header.xNumBricks_ * y) + (header.xNumBricks_ * header.yNumBricks_ * z);
}

void TSPBrickSelector::writeSelection(BrickSelection brickSelection, std::vector<int>& bricks) {
    BrickCover coveredBricks = brickSelection.cover;
    for (int z = coveredBricks.lowZ; z < coveredBricks.highZ; z++) {
        for (int y = coveredBricks.lowY; y < coveredBricks.highY; y++) {
            for (int x = coveredBricks.lowX; x < coveredBricks.highX; x++) {
                bricks[linearCoords(x, y, z)] = brickSelection.brickIndex;
            }
        }
    }
}

std::vector<float> TSPBrickSelector::getTfGradients() {
    TransferFunction* tf = _transferFunction;
    std::vector<float> gradients;
    if (!tf) return gradients;

    size_t tfWidth = tf->width();
    if (tfWidth <= 0) return gradients;

    gradients.resize(tfWidth - 1);

    for (size_t offset = 0; offset < tfWidth - 1; offset++) {
        glm::vec4 prevRgba = tf->sample(offset);
        glm::vec4 nextRgba = tf->sample(offset + 1);

        float colorDifference = glm::distance(prevRgba, nextRgba);
        float alpha = (prevRgba.w + nextRgba.w) * 0.5;

        gradients[offset] = colorDifference*alpha;
    }
    return gradients;
}

} // namespace openspace
