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

#include <modules/multiresvolume/rendering/shenbrickselector.h>

#include <modules/multiresvolume/rendering/tsp.h>

namespace openspace {

ShenBrickSelector::ShenBrickSelector(TSP* tsp, float spatialTolerance,
                                     float temporalTolerance)
    : _tsp(tsp)
    , _spatialTolerance(spatialTolerance)
    , _temporalTolerance(temporalTolerance)
{}

void ShenBrickSelector::setSpatialTolerance(float spatialTolerance) {
    _spatialTolerance = spatialTolerance;
}

void ShenBrickSelector::setTemporalTolerance(float temporalTolerance) {
    _temporalTolerance = temporalTolerance;
}

void ShenBrickSelector::selectBricks(int timestep, std::vector<int>& bricks) {
    int numTimeSteps = _tsp->header().numTimesteps;
    BrickCover coveredBricks(_tsp->header().xNumBricks);
    selectBricks(timestep, 0, 0, 0, numTimeSteps, coveredBricks, bricks);
}

/**
 * Traverse the Octree in the BST root
 */
void ShenBrickSelector::traverseOT(int timestep, unsigned int brickIndex,
                                   BrickCover coveredBricks, std::vector<int>& bricks)
{
    unsigned int firstChild = _tsp->firstOctreeChild(brickIndex);
    int numTimeSteps = _tsp->header().numTimesteps;
    for (unsigned int i = 0; i < 8; i++) {
        unsigned int child = firstChild + i;
        BrickCover cover = coveredBricks.split(i % 2, (i/2) % 2, (i/4));
        selectBricks(timestep, child, child, 0, numTimeSteps, cover, bricks);
    }
}

void ShenBrickSelector::traverseBST(int timestep, unsigned int brickIndex,
                                    unsigned int bstRootBrickIndex, int timeSpanStart,
                                    int timeSpanEnd, BrickCover coveredBricks,
                                    std::vector<int>& bricks)
{
    int timeSpanCenter = timeSpanStart + (timeSpanEnd - timeSpanStart) / 2;
    unsigned int bstChild;
    if (timestep <= timeSpanCenter) {
        bstChild = _tsp->bstLeft(brickIndex);
        timeSpanEnd = timeSpanCenter;
    } else {
        bstChild = _tsp->bstRight(brickIndex);
        timeSpanStart = timeSpanCenter;
    }
    selectBricks(
        timestep,
        bstChild,
        bstRootBrickIndex,
        timeSpanStart,
        timeSpanEnd,
        coveredBricks,
        bricks
    );
}

void ShenBrickSelector::selectBricks(int timestep, unsigned int brickIndex,
                                     unsigned int bstRootBrickIndex, int timeSpanStart,
                                     int timeSpanEnd, BrickCover coveredBricks,
                                     std::vector<int>& bricks)
{
    if (_tsp->temporalError(brickIndex) <= _temporalTolerance) {
        if (_tsp->isOctreeLeaf(bstRootBrickIndex)) {
            selectCover(coveredBricks, brickIndex, bricks);
        } else if (_tsp->spatialError(brickIndex) <= _spatialTolerance) {
            selectCover(coveredBricks, brickIndex, bricks);
        } else if (_tsp->isBstLeaf(brickIndex)) {
            traverseOT(timestep, bstRootBrickIndex, coveredBricks, bricks);
        } else {
            traverseBST(
                timestep,
                brickIndex,
                bstRootBrickIndex,
                timeSpanStart,
                timeSpanEnd,
                coveredBricks,
                bricks
            );
        }
    } else if (_tsp->isBstLeaf(brickIndex)) {
        if (_tsp->isOctreeLeaf(bstRootBrickIndex)) {
            selectCover(coveredBricks, brickIndex, bricks);
        } else {
            traverseOT(timestep, bstRootBrickIndex, coveredBricks, bricks);
        }
    } else {
        traverseBST(
            timestep,
            brickIndex,
            bstRootBrickIndex,
            timeSpanStart,
            timeSpanEnd,
            coveredBricks,
            bricks
        );
    }
}

int ShenBrickSelector::linearCoords(int x, int y, int z) const {
    const TSP::Header& header = _tsp->header();
    return x + (header.xNumBricks * y) + (header.xNumBricks * header.yNumBricks * z);
}

void ShenBrickSelector::selectCover(BrickCover coveredBricks, unsigned int brickIndex,
                                    std::vector<int>& bricks) const
{
    for (int z = coveredBricks.lowZ; z < coveredBricks.highZ; z++) {
        for (int y = coveredBricks.lowY; y < coveredBricks.highY; y++) {
            for (int x = coveredBricks.lowX; x < coveredBricks.highX; x++) {
                bricks[linearCoords(x, y, z)] = brickIndex;
            }
        }
    }
}

} // namespace openspace
