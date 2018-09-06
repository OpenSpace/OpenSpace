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

#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>

#include <modules/globebrowsing/tile/pixelregion.h>
#include <ghoul/misc/assert.h>

namespace openspace::globebrowsing {

IODescription IODescription::cut(PixelRegion::Side side, int pos) {
    const PixelRegion readPreCut = read.region;
    const PixelRegion writePreCut = write.region;

    glm::dvec2 ratio = {
        write.region.numPixels.x / static_cast<double>(read.region.numPixels.x),
        write.region.numPixels.y / static_cast<double>(read.region.numPixels.y)
    };

    IODescription whatCameOff = *this;
    whatCameOff.read.region = read.region.globalCut(side, pos);

    PixelRegion::PixelRange cutSize = whatCameOff.read.region.numPixels;
    PixelRegion::PixelRange localWriteCutSize = ratio * glm::dvec2(cutSize);

    if (cutSize.x == 0 || cutSize.y == 0) {
        ghoul_assert(
            read.region.equals(readPreCut),
            "Read region should not have been modified"
        );
        ghoul_assert(
            write.region.equals(writePreCut),
            "Write region should not have been modified"
        );
    }

    int localWriteCutPos =
        (side == PixelRegion::Side::LEFT || side == PixelRegion::Side::RIGHT) ?
        localWriteCutSize.x :
        localWriteCutSize.y;

    whatCameOff.write.region = write.region.localCut(side, localWriteCutPos);

    return whatCameOff;
}

} // namespace openspace::globebrowsing
