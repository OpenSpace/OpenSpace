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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PIXELREGION___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PIXELREGION___H__

#include <ghoul/glm.h>

namespace openspace::globebrowsing {

struct PixelRegion {
    using PixelCoordinate = glm::ivec2;
    using PixelRange = glm::ivec2;

    enum class Side {
        LEFT = 0,
        TOP,
        RIGHT,
        BOTTOM
    };

    PixelRegion(const PixelCoordinate& pixelStart = PixelCoordinate(0, 0),
        const PixelRange& numberOfPixels = PixelRange(0, 0));
    PixelRegion(const PixelRegion& o) = default;

    /**
     * Sets one of the sides of the pixel region the specified position. This changes
     * the number of pixels in the region.
     *
     * Example: side = LEFT and pos 16:
     *          set start.x = 16 and keep the end position the same.
     */
    void setSide(Side side, int pos);
    void setLeft(int x);
    void setTop(int p);
    void setRight(int x);
    void setBottom(int y);

    /**
     * Aligns one the sides of the pixel regino to the specified position. This does
     * not change the number of pixels within the region.
     *
     * Example: Side = left and pos = 16:
     *                 start.x = 16 and keep the size the same
     */
    void align(Side side, int pos);
    void alignLeft(int x);
    void alignTop(int y);
    void alignRight(int x);
    void alignBottom(int y);

    void scale(const glm::dvec2& s);
    void scale(double s);
    void downscalePow2(int exponent, PixelCoordinate wrt = { 0, 0 });
    void upscalePow2(int exponent, PixelCoordinate wrt = { 0, 0 });

    void move(Side side, int amount);
    void pad(const PixelRegion& padding);
    void clampTo(const PixelRegion& boundingRegion);

    void forceNumPixelToDifferByNearestMultipleOf(unsigned int multiple);
    void roundUpNumPixelToNearestMultipleOf(unsigned int multiple);
    void roundDownToQuadratic();

    PixelRegion globalCut(Side side, int globalPos);
    PixelRegion localCut(Side side, int localPos);

    int area() const;
    int edge(Side side) const;
    int edgeDirectionSign(Side side) const;
    PixelCoordinate end() const;

    bool lineIntersect(Side side, int p);
    bool isInside(const PixelRegion& r) const;
    bool equals(const PixelRegion& r) const;

    PixelCoordinate start;
    PixelRange numPixels;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PIXELREGION___H__
