/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __PIXEL_REGION_H__
#define __PIXEL_REGION_H__

#include <glm/glm.hpp>

namespace openspace {

    typedef glm::ivec2 PixelCoordinate;
    typedef glm::ivec2 PixelRange;

    struct PixelRegion {

        enum Side {
            LEFT = 0, TOP, RIGHT, BOTTOM,
        };

        PixelRegion();
        PixelRegion(const PixelRegion& o);
        PixelRegion(const PixelCoordinate& pixelStart, const PixelRange& numberOfPixels);

        
        //////////////////////////////////////////////////////////////////////////////////
        //                                  Mutators                                    //
        //////////////////////////////////////////////////////////////////////////////////

        void setSide(Side side, int pos);
        void setLeft(int x);
        void setTop(int p);
        void setRight(int x);
        void setBottom(int y);
        
        void align(Side side, int pos);
        void alignLeft(int x);
        void alignTop(int y);
        void alignRight(int x);
        void alignBottom(int y);

        void scale(const glm::dvec2& s);
        void scale(double s);
        void downscalePow2(int exponent);
        void upscalePow2(int exponent);

        void move(Side side, int amount);
        void pad(const PixelRegion& padding);
        void clampTo(const PixelRegion& boundingRegion);


        PixelRegion globalCut(Side side, int globalPos);
        PixelRegion localCut(Side side, int localPos);


        //////////////////////////////////////////////////////////////////////////////////
        //                                 Accessors                                    //
        //////////////////////////////////////////////////////////////////////////////////

        int area() const;
        int edge(Side side) const;
        int edgeDirectionSign(Side side) const;
        PixelCoordinate end() const;


        //////////////////////////////////////////////////////////////////////////////////
        //                                Comparators                                   //
        //////////////////////////////////////////////////////////////////////////////////

        bool lineIntersect(Side side, int p);
        bool isInside(const PixelRegion& r) const;
        bool equals(const PixelRegion& r) const;


        //////////////////////////////////////////////////////////////////////////////////
        //                                   Members                                    //
        //////////////////////////////////////////////////////////////////////////////////

        PixelCoordinate start;
        PixelRange numPixels;
    };

    



} // namespace openspace



#endif  // __PIXEL_REGION_H__