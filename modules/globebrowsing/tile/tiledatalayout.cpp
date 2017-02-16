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

#include <modules/globebrowsing/tile/tiledatalayout.h>

#include <limits>

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>


#include <modules/globebrowsing/geometry/angle.h>

#include <float.h>
#include <sstream>
#include <algorithm>

#include <gdal_priv.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>

#include <memory>
#include <set>
#include <queue>
#include <iostream>
#include <unordered_map>

#include <ghoul/filesystem/file.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/misc/threadpool.h>

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledatatype.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tilemetadata.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>

namespace {
    const std::string _loggerCat = "TileDataset";
}

namespace openspace {
namespace globebrowsing {

TileDataLayout::TileDataLayout() {}

TileDataLayout::TileDataLayout(GDALDataset* dataSet, GLuint preferredGlType) {
    // Assume all raster bands have the same data type
    gdalType =preferredGlType != 0 ?
        tiledatatype::getGdalDataType(preferredGlType) :
        dataSet->GetRasterBand(1)->GetRasterDataType();

    glType = tiledatatype::getOpenGLDataType(gdalType);
    numRasters = dataSet->GetRasterCount();
    bytesPerDatum = tiledatatype::numberOfBytes(gdalType);
    bytesPerPixel = bytesPerDatum * numRasters;
    textureFormat = tiledatatype::getTextureFormat(numRasters, gdalType);
}
  
} // namespace globebrowsing
} // namespace openspace
