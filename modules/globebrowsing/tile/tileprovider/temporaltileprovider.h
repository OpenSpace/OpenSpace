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

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <openspace/util/time.h>

#include <unordered_map>

#include <gdal_priv.h>




//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {

    //////////////////////////////////////////////////////////////////////////////////////
    //                                 Time Id Providers                                //
    //////////////////////////////////////////////////////////////////////////////////////

    struct TimeFormat {
        virtual std::string stringify(const Time& t) const = 0;
    };

    struct YYYY_MM_DD : public TimeFormat {
        virtual std::string stringify(const Time& t) const;
    };

    struct YYYY_MM_DDThh_mm_ssZ : public TimeFormat {
        virtual std::string stringify(const Time& t) const;
    };



    struct TimeIdProviderFactory {
        static TimeFormat* getProvider(const std::string& format);
        static void init();

        static std::unordered_map<std::string, std::unique_ptr<TimeFormat>> _timeIdProviderMap;
        static bool initialized;
    };



    //////////////////////////////////////////////////////////////////////////////////////
    //                                  Time Quantizer                                  //
    //////////////////////////////////////////////////////////////////////////////////////
    struct TimeQuantizer {
        TimeQuantizer() {}
        TimeQuantizer(const Time& start, const Time& end, double resolution);
        TimeQuantizer(const Time& start, const Time& end, const std::string& resolutionStr);

        static double parseTimeResolutionStr(const std::string& resoltutionStr);


        bool quantize(Time& t) const;

    private:
        double _start;
        double _end;
        double _resolution;

        bool _clampTime = true;
    };


    //////////////////////////////////////////////////////////////////////////////////////
    //                              Temporal tile Provider                              //
    //////////////////////////////////////////////////////////////////////////////////////

    class TemporalTileProvider : public TileProvider {
    public:
        TemporalTileProvider(const std::string& datasetFile, const TileProviderInitData& tileProviderInitData);



        // These methods implements TileProvider
        virtual Tile getTile(const ChunkIndex& chunkIndex);
        virtual Tile getDefaultTile();
        virtual Tile::Status getTileStatus(const ChunkIndex& chunkIndex);
        virtual TileDepthTransform depthTransform();
        virtual void update();
        virtual void reset();
        virtual int maxLevel();




        typedef std::string TimeKey;

        std::shared_ptr<TileProvider> getTileProvider(Time t = Time::ref());
        std::shared_ptr<TileProvider> getTileProvider(TimeKey timekey);

    private:

        static const std::string TIME_PLACEHOLDER;


        std::string getGdalDatasetXML(Time t);
        std::string getGdalDatasetXML(TimeKey key);

        
        std::shared_ptr<TileProvider> initTileProvider(TimeKey timekey);

        std::string consumeTemporalMetaData(const std::string &xml);
        std::string getXMLValue(CPLXMLNode*, const std::string& key, const std::string& defaultVal);

        void ensureUpdated();

        //////////////////////////////////////////////////////////////////////////////////
        //                                Members variables                             //
        //////////////////////////////////////////////////////////////////////////////////

        const std::string _datasetFile;
        std::string _gdalXmlTemplate;

        std::unordered_map<TimeKey, std::shared_ptr<TileProvider> > _tileProviderMap;
        TileProviderInitData _tileProviderInitData;

        Tile _defaultTile;

        std::shared_ptr<TileProvider> _currentTileProvider;

        TimeFormat * _timeFormat;
        TimeQuantizer _timeQuantizer;
    };



}  // namespace openspace




#endif  // __TEMPORAL_TILE_PROVIDER_H__