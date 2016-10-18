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

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/tile/tileioresult.h>

#include <modules/globebrowsing/geometry/angle.h>

#include <float.h>

#include <sstream>
#include <algorithm>

namespace {
    const std::string _loggerCat = "TileIOResult";
}

namespace openspace {
namespace globebrowsing {
    void TilePreprocessData::serialize(std::ostream& os) {
        os << maxValues.size() << std::endl;
        for (float f : maxValues) {
            os << f << " ";
        }
        os << std::endl;
        for (float f : minValues) {
            os << f << " ";
        }
        os << std::endl;
    }

    TilePreprocessData TilePreprocessData::deserialize(std::istream& is) {
        TilePreprocessData res;
        int n; is >> n;
        res.maxValues.resize(n);
        for (int i = 0; i < n; i++) {
            is >> res.maxValues[i];
        }
        res.minValues.resize(n);
        for (int i = 0; i < n; i++) {
            is >> res.minValues[i];
        }

        return std::move(res);
    }

    TileIOResult::TileIOResult()
        : imageData(nullptr)
        , dimensions(0, 0, 0)
        , preprocessData(nullptr)
        , tileIndex(0, 0, 0)
        , error(CE_None)
        , nBytesImageData(0)
    {}
        
    TileIOResult TileIOResult::createDefaultRes() {
        TileIOResult defaultRes;
        int w = 8;
        int h = 8;
        defaultRes.dimensions = glm::uvec3(w, h, 1);
        defaultRes.nBytesImageData = w * h * 1 * 3 * 4; // assume max 3 channels, max 4 bytes per pixel
        defaultRes.imageData = new char[defaultRes.nBytesImageData];
        std::fill_n((char*)defaultRes.imageData, defaultRes.nBytesImageData, 0);
        return std::move(defaultRes);
    }

    void TileIOResult::serializeMetaData(std::ostream& os) {
        os << dimensions.x << " " << dimensions.y << " " << dimensions.z << std::endl;
        os << tileIndex.x << " " << tileIndex.y << " " << tileIndex.level << std::endl;
        os << error << std::endl;

        // preprocess data
        os << (preprocessData != nullptr) << std::endl;
        if (preprocessData != nullptr) {    
            preprocessData->serialize(os);
        }
        
        os << nBytesImageData << std::endl;
    }

    TileIOResult TileIOResult::deserializeMetaData(std::istream& is) {
        TileIOResult res;
        is >> res.dimensions.x >> res.dimensions.y >> res.dimensions.z;
        is >> res.tileIndex.x >> res.tileIndex.y >> res.tileIndex.level;
        int err; is >> err; res.error = (CPLErr) err;
        
        res.preprocessData = nullptr;
        bool hasPreprocessData; 
        is >> hasPreprocessData;
        if (hasPreprocessData) {
            TilePreprocessData preprocessData = TilePreprocessData::deserialize(is);
            res.preprocessData = std::make_shared<TilePreprocessData>(preprocessData);
        }
        
        is >> res.nBytesImageData;

        char binaryDataSeparator;
        is >> binaryDataSeparator; // not used
        
        char* buffer = new char[res.nBytesImageData]();
        return std::move(res);
    }

} // namespace globebrowsing
} // namespace openspace
