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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___SIMPLETFBRICKSELECTOR___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___SIMPLETFBRICKSELECTOR___H__

#include <modules/multiresvolume/rendering/brickselector.h>

#include <modules/multiresvolume/rendering/brickselection.h>
#include <vector>

namespace openspace {

class TSP;
class HistogramManager;
class TransferFunction;

class SimpleTfBrickSelector : public BrickSelector {
public:
    SimpleTfBrickSelector(TSP* tsp, HistogramManager* hm, TransferFunction* tf,
        int memoryBudget, int streamingBudget);
    ~SimpleTfBrickSelector() = default;

    void selectBricks(int timestep, std::vector<int>& bricks) override;
    void setMemoryBudget(int memoryBudget);
    void setStreamingBudget(int streamingBudget);
    bool calculateBrickImportances();

private:
    float spatialSplitPoints(unsigned int brickIndex) const;
    float temporalSplitPoints(unsigned int brickIndex) const;
    float splitPoints(unsigned int brickIndex, BrickSelection::SplitType& splitType);

    int linearCoords(int x, int y, int z) const;
    void writeSelection(BrickSelection coveredBricks, std::vector<int>& bricks);

    TSP* _tsp;
    HistogramManager* _histogramManager;
    TransferFunction* _transferFunction;
    std::vector<float> _brickImportances;

    int _memoryBudget;
    int _streamingBudget;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___SIMPLETFBRICKSELECTOR___H__
