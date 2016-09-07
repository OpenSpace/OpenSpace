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

#ifndef __TILE_H__
#define __TILE_H__

#include <ghoul/opengl/texture.h> // Texture

#include <modules/globebrowsing/tile/asynctilereader.h> // TilePreprocessData


namespace openspace {
    
    using namespace ghoul::opengl;    

    struct Tile {
        std::shared_ptr<Texture> texture;
        std::shared_ptr<TilePreprocessData> preprocessData;

        enum class Status { Unavailable, OutOfRange, IOError, OK } status;
    
        
        /**
         * Instantiaes a new tile unicolored tile. The texture gets the provided size and
         * color in rgba. Color values ranges between 0-255.
         */
        static Tile createPlainTile(const glm::uvec2& size, const glm::uvec4& color);

        static const Tile TileUnavailable;

    };

}  // namespace openspace




#endif  // __TILE_H__