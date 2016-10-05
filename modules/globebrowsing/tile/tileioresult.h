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

#ifndef __TILE_IO_RESULT_H__
#define __TILE_IO_RESULT_H__

#include <ghoul/opengl/texture.h>
#include <modules/globebrowsing/geometry/geodetic2.h>

#include <ghoul/filesystem/file.h>


#include <modules/globebrowsing/tile/tiledepthtransform.h>

#include "gdal_priv.h"


#include <memory>
#include <iostream>



namespace openspace {
    using namespace ghoul::opengl;
    using namespace ghoul::filesystem;

    struct TilePreprocessData {
        std::vector<float> maxValues;
        std::vector<float> minValues;
        std::vector<bool> hasMissingData;

        void serialize(std::ostream& s);
        static TilePreprocessData deserialize(std::istream& s);
    };

    struct TextureFormat {
        Texture::Format ghoulFormat;
        GLuint glFormat;
    };

    struct TileIOResult {
        TileIOResult();

        char* imageData;
        glm::uvec3 dimensions;
        std::shared_ptr<TilePreprocessData> preprocessData;
        TileIndex tileIndex;
        CPLErr error;
        size_t nBytesImageData;

        void serializeMetaData(std::ostream& s);
        static TileIOResult deserializeMetaData(std::istream& s);
   
        static TileIOResult createDefaultRes();

    };




} // namespace openspace





#endif  // __TILE_IO_RESULT_H__