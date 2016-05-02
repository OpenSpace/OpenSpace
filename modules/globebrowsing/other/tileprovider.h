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

#ifndef __TILE_PROVIDER_H__
#define __TILE_PROVIDER_H__

#include "gdal_priv.h"

#include <openspace/engine/downloadmanager.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // absPath
#include <ghoul/opengl/texture.h>
#include <ghoul/io/texture/texturereader.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/lrucache.h>
#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/other/gdaldataconverter.h>







/*
namespace openspace {
    
    using namespace ghoul::opengl;

    struct TextureLoadJob : public Job<Texture> {
        TextureLoadJob(const std::string& filePath, HashKey hashkey)
        : _filePath(filePath)
        , _hashkey(hashkey){
            
        }

        virtual ~TextureLoadJob() { }

        virtual void execute() {
        
            auto textureReader = ghoul::io::TextureReader::ref();
            _texture = std::move(textureReader.loadTexture(absPath(_filePath)));
        
        }
        
        virtual std::shared_ptr<Texture> product() {
            return _texture;
        }
        
        HashKey hashKey() { return _hashkey; }


    private:
        std::string _filePath;
        HashKey _hashkey;
        std::shared_ptr<Texture> _texture;
    };
}
*/

//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////


namespace openspace {
    using namespace ghoul::opengl;

    class TileProvider {
    public:
        TileProvider(const std::string& fileName, int tileCacheSize);
        ~TileProvider();

        std::shared_ptr<Texture> getTile(const GeodeticTileIndex& tileIndex);

        void prerender();


    private:

        std::shared_ptr<Texture> getTileInternal(const GeodeticTileIndex& tileIndex, int GLType);


        LRUCache<HashKey, std::shared_ptr<Texture>> _tileCache;


        const std::string _filePath;

        static bool hasInitializedGDAL;
        GDALDataset* _gdalDataSet;
        GdalDataConverter _converter;

        

    };

}  // namespace openspace

#endif  // __TILE_PROVIDER_H__