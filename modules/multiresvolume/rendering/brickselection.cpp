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

#include <modules/multiresvolume/rendering/brickselection.h>

namespace openspace {

BrickSelection::BrickSelection() {}

BrickSelection::BrickSelection(int numBricks, int numTimeSteps, SplitType splitType_,
                               float splitPoints_)
{
    cover = BrickCover(numBricks);
    lowT = 0;
    highT = numTimeSteps;
    brickIndex = 0;
    splitType = splitType_;
    splitPoints = splitPoints_;
    nSpatialSplits = 0;
    nTemporalSplits = 0;
}

BrickSelection BrickSelection::splitSpatially(bool x, bool y, bool z,
                                              unsigned int childBrickIndex,
                                              SplitType childSplitType,
                                              float childSplitPoints)
{
    BrickSelection child;
    child.cover = cover.split(x, y, z);
    child.brickIndex = childBrickIndex;
    child.splitPoints = childSplitPoints;
    child.splitType = childSplitType;
    child.nSpatialSplits = nSpatialSplits + 1;
    child.nTemporalSplits = nTemporalSplits;
    child.lowT = lowT;
    child.highT = highT;
    return child;
}

BrickSelection BrickSelection::splitTemporally(bool t, unsigned int childBrickIndex,
                                               SplitType childSplitType,
                                               float childSplitPoints)
{
    BrickSelection child;
    child.cover = cover;
    child.brickIndex = childBrickIndex;
    child.splitPoints = childSplitPoints;
    child.splitType = childSplitType;
    if (t) {
        child.lowT = centerT();
        child.highT = highT;
    } else {
        child.lowT = lowT;
        child.highT = centerT();
    }
    child.nSpatialSplits = nSpatialSplits;
    child.nTemporalSplits = nTemporalSplits + 1;

    return child;
}

int BrickSelection::centerT() const {
    return lowT + (highT - lowT) / 2;
}

bool BrickSelection::timestepInRightChild(int timestep) const {
    return timestep >= centerT();
}

} // namespace openspace
