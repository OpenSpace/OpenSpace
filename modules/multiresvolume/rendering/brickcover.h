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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKCOVER___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKCOVER___H__

namespace openspace {

struct BrickCover {
    int lowX, highX, lowY, highY, lowZ, highZ;

    BrickCover() {}

    BrickCover(int numBricks) {
        lowX = lowY = lowZ = 0;
        highX = highY = highZ = numBricks;
    }

    BrickCover split(bool x, bool y, bool z) {
        BrickCover child;
        if (x) {
            child.lowX = lowX + (highX - lowX) / 2;
            child.highX = highX;
        } else {
            child.lowX = lowX;
            child.highX = lowX + (highX - lowX) / 2;
        }
        if (y) {
            child.lowY = lowY + (highY - lowY) / 2;
            child.highY = highY;
        } else {
            child.lowY = lowY;
            child.highY = lowY + (highY - lowY) / 2;
        }
        if (z) {
            child.lowZ = lowZ + (highZ - lowZ) / 2;
            child.highZ = highZ;
        } else {
            child.lowZ = lowZ;
            child.highZ = lowZ + (highZ - lowZ) / 2;
        }
        return child;
    }
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___BRICKCOVER___H__
