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

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/tileprovider.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    bool TileProvider::hasInitializedGDAL = false;

    TileProvider::TileProvider(const std::string& filePath, int tileCacheSize)
    : _filePath(filePath)
    , _tileCache(tileCacheSize) // setting cache size
    {
        int downloadApplicationVersion = 1;
        if (!DownloadManager::isInitialized()) {
            DownloadManager::initialize(
                "../tmp_openspace_downloads/",
                downloadApplicationVersion);
        }
        
        if (!hasInitializedGDAL) {
            GDALAllRegister();
            hasInitializedGDAL = true;
        }

        std::string absFilePath = absPath(filePath);
        _gdalDataSet = (GDALDataset *)GDALOpen(absFilePath.c_str(), GA_ReadOnly);
        //auto desc = _gdalDataSet->GetDescription();
        
        ghoul_assert(_gdalDataSet != nullptr, "Failed to load dataset: " << filePath);
    }

    TileProvider::~TileProvider(){
        delete _gdalDataSet;
    }

    void TileProvider::prerender() {
        while (_tileLoadManager.numFinishedJobs() > 0) {
            auto finishedJob = _tileLoadManager.popFinishedJob();
            std::shared_ptr<UninitializedTextureTile> uninitedTex =
                finishedJob->product();
            HashKey key = uninitedTex->tileIndex.hashKey();
            std::shared_ptr<Texture> texture = initializeTexture(uninitedTex);
            _tileCache.put(key, texture);
        }
    }

    std::shared_ptr<Texture> TileProvider::getTile(const GeodeticTileIndex& tileIndex) {
        HashKey hashkey = tileIndex.hashKey();
        
        if (_tileCache.exist(hashkey)) {
            return _tileCache.get(hashkey);
        }
        else {
            // enque load job
            std::shared_ptr<TextureTileLoadJob> job = std::shared_ptr<TextureTileLoadJob>(
                new TextureTileLoadJob(this, tileIndex));

            _tileLoadManager.enqueueJob(job);

            // map key to nullptr while tile is loaded
            _tileCache.put(hashkey, nullptr);
            return nullptr;
        }
    }

    std::shared_ptr<UninitializedTextureTile> TileProvider::getUninitializedTextureTile(
        const GeodeticTileIndex& tileIndex) {
        
        // We assume here that all rasterbands have the same data type
        GDALDataType gdalType = _gdalDataSet->GetRasterBand(1)->GetRasterDataType();

        switch (gdalType)
        {
        case GDT_Byte:
            return _uByteConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_UInt16:
            return _uShortConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_Int16:
            return _shortConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_UInt32:
            return _uIntConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_Int32:
            return _intConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_Float32:
            return _floatConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        case GDT_Float64:
            return _doubleConverter.getUninitializedTextureTile(_gdalDataSet, tileIndex);
            break;
        default:
            LERROR("GDAL data type unknown to OpenGL: " << gdalType);
        }
        return nullptr;
    }

    std::shared_ptr<Texture> TileProvider::initializeTexture(
        std::shared_ptr<UninitializedTextureTile> uninitedTexture) {
        Texture* tex = new Texture(
            uninitedTexture->imageData,
            uninitedTexture->dimensions,
            uninitedTexture->texFormat.ghoulFormat,
            uninitedTexture->texFormat.glFormat,
            uninitedTexture->glType,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToBorder);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(tex);

        texture->uploadTexture();
        /*
        texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        */
        return texture;
    }



}  // namespace openspace
