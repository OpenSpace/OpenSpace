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

#ifndef __TEMPORAL_TILE_PROVIDER_H__
#define __TEMPORAL_TILE_PROVIDER_H__


#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/tileprovider.h>
#include <openspace/util/time.h>

#include <unordered_map>

#include "gdal_priv.h"
#include "vrtdataset.h"




//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
   
    struct TileProviderInitData {
        int minimumPixelSize;
        int threads;
        int cacheSize;
        int framesUntilRequestQueueFlush;
    };

    
    class TemporalTileProvider {
    public:
        TemporalTileProvider(const std::string& datasetFile, const TileProviderInitData& tileProviderInitData);

        std::shared_ptr<TileProvider> getTileProvider(Time t = Time::ref());

    private:

        typedef std::string TimeKey;

        std::string getGdalDatasetXML(Time t);
        std::string getGdalDatasetXML(TimeKey key);

        static const std::string TIME_PLACEHOLDER;

        TimeKey getTimeKey(const Time& t);

        std::shared_ptr<TileProvider> initTileProvider(TimeKey timekey);


        //////////////////////////////////////////////////////////////////////////////////
        //                                Members variables                             //
        //////////////////////////////////////////////////////////////////////////////////

        const std::string _datasetFile;
        
        std::string _dataSourceXmlTemplate;

        std::unordered_map<TimeKey, std::shared_ptr<TileProvider> > _tileProviderMap;

        TileProviderInitData _tileProviderInitData;

    };



}  // namespace openspace




#endif  // __TEMPORAL_TILE_PROVIDER_H__