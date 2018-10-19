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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKSELECTION___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKSELECTION___H__

#include <modules/multiresvolume/rendering/brickcover.h>

namespace openspace {

struct BrickSelection {
    enum SplitType {
        None = 0,
        Temporal = 1,
        Spatial = 2
    };

    BrickSelection();
    BrickSelection(int numBricks, int numTimeSteps, SplitType splitType,
        float splitPoints);

    BrickSelection splitSpatially(bool x, bool y, bool z, unsigned int childBrickIndex,
        SplitType childSplitType, float childSplitPoints);

    BrickSelection splitTemporally(bool t, unsigned int childBrickIndex,
        SplitType childSplitType, float childSplitPoints);

    int centerT() const;
    bool timestepInRightChild(int timestep) const;

    unsigned int brickIndex;
    float splitPoints;
    BrickSelection::SplitType splitType;
    BrickCover cover;
    int lowT;
    int highT;
    int nSpatialSplits;
    int nTemporalSplits;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKSELECTION___H__
