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

#ifndef __LAYERED_TEXTURES_H__
#define __LAYERED_TEXTURES_H__

#include <memory>
#include <vector>
#include <string>

namespace openspace {

    class LayeredTextures {

    public:

        static const size_t NUM_TEXTURE_CATEGORIES = 6;
        static const size_t MAX_NUM_TEXTURES_PER_CATEGORY = 5;

        enum TextureCategory {
            ColorTextures,
            GrayScaleOverlays,
            NightTextures,
            WaterMasks,
            Overlays,
            HeightMaps,
        };

        static const std::string TEXTURE_CATEGORY_NAMES[NUM_TEXTURE_CATEGORIES];
    };

} // namespace openspace
#endif  // __LAYERED_TEXTURES_H__