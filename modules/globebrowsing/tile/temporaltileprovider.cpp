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

#include <modules/globebrowsing/geometry/geodetic2.h>

#include <modules/globebrowsing/tile/temporaltileprovider.h>

#include <modules/globebrowsing/chunk/chunkindex.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/logging/logmanager.h>

#include <openspace/util/time.h>


#include <string>
#include <fstream>
#include <streambuf>

#include "cpl_minixml.h"



namespace {
    const std::string _loggerCat = "TemporalTileProvider";
}


namespace openspace {

    const std::string TemporalTileProvider::TIME_PLACEHOLDER("${OpenSpaceTimeId}");

    TemporalTileProvider::TemporalTileProvider(const std::string& datasetFile, 
        const TileProviderInitData& tileProviderInitData)
        : _datasetFile(datasetFile)
        , _tileProviderInitData(tileProviderInitData)
    {
        std::ifstream in(datasetFile.c_str());
        ghoul_assert(errno == 0, strerror(errno) << std::endl << datasetFile);

        // read file
        std::string xml( (std::istreambuf_iterator<char>(in)), (std::istreambuf_iterator<char>()));
        _gdalXmlTemplate = consumeTemporalMetaData(xml);
    }

    std::string TemporalTileProvider::consumeTemporalMetaData(const std::string& xml) {
        CPLXMLNode* node = CPLParseXMLString(xml.c_str());

        std::string timeStart = getXMLValue(node, "OpenSpaceTimeStart", "2000 Jan 1");
        std::string timeResolution = getXMLValue(node, "OpenSpaceTimeResolution", "2d");
        std::string timeEnd = getXMLValue(node, "OpenSpaceTimeEnd", "Now");
        std::string timeIdFormat = getXMLValue(node, "OpenSpaceTimeIdFormat", "YYYY-MM-DDThh:mm:ssZ");

        Time start; start.setTime(timeStart);
        Time end(Time::now());
        if (timeEnd != "Now") {
            end.setTime(timeEnd);
        }

        _timeQuantizer = TimeQuantizer(start, end, timeResolution);
        _timeFormat = TimeIdProviderFactory::getProvider(timeIdFormat);

        CPLXMLNode* gdalNode = CPLSearchXMLNode(node, "GDAL_WMS");
        return CPLSerializeXMLTree(gdalNode);
    }


    std::string TemporalTileProvider::getXMLValue(CPLXMLNode* root, const std::string& key, const std::string& defaultVal) {
        CPLXMLNode * n = CPLSearchXMLNode(root, key.c_str());
        bool hasValue = (n != nullptr && n->psChild != nullptr && n->psChild->pszValue != nullptr);
        return hasValue ? std::string(n->psChild->pszValue) : defaultVal;
    }

    TileAndTransform TemporalTileProvider::getHighestResolutionTile(ChunkIndex chunkIndex, int parents) {
        if (_currentTileProvider == nullptr) {
            LDEBUG("Warning: had to call prerender from getHighestResolutionTile()");
            prerender();
        }

        return _currentTileProvider->getHighestResolutionTile(chunkIndex, parents);
    }

    TileDepthTransform TemporalTileProvider::depthTransform() {
        if (_currentTileProvider == nullptr) {
            LDEBUG("Warning: had to call prerender from depthTransform()");
            prerender();
        }

        return _currentTileProvider->depthTransform();
    }

    Tile::Status TemporalTileProvider::getTileStatus(const ChunkIndex& chunkIndex) {
        if (_currentTileProvider == nullptr) {
            LDEBUG("Warning: had to call prerender from getTileStatus()");
            prerender();
        }

        return _currentTileProvider->getTileStatus(chunkIndex);
    }


    void TemporalTileProvider::prerender() {
        _currentTileProvider = getTileProvider();
        _currentTileProvider->prerender();
    }


    std::shared_ptr<CachingTileProvider> TemporalTileProvider::getTileProvider(Time t) {
        Time tCopy(t);
        if (_timeQuantizer.quantize(tCopy)) {
            TimeKey timekey = _timeFormat->stringify(tCopy);
            return getTileProvider(timekey);
        }
        return nullptr;
    }


    std::shared_ptr<CachingTileProvider> TemporalTileProvider::getTileProvider(TimeKey timekey) {
        auto it = _tileProviderMap.find(timekey);
        if (it != _tileProviderMap.end()) {
            return it->second;
        }
        else {
            auto tileProvider = initTileProvider(timekey);
            _tileProviderMap[timekey] = tileProvider;
            return tileProvider;
        }
    }


    std::shared_ptr<CachingTileProvider> TemporalTileProvider::initTileProvider(TimeKey timekey) {
        std::string gdalDatasetXml = getGdalDatasetXML(timekey);
        
        std::shared_ptr<TileDataset> tileDataset = std::shared_ptr<TileDataset>(
            new TileDataset(gdalDatasetXml,
                _tileProviderInitData.minimumPixelSize,
                _tileProviderInitData.preprocessTiles));

        std::shared_ptr<ThreadPool> threadPool = std::shared_ptr<ThreadPool>(
            new ThreadPool(_tileProviderInitData.threads));

        std::shared_ptr<AsyncTileDataProvider> tileReader = std::shared_ptr<AsyncTileDataProvider>(
            new AsyncTileDataProvider(tileDataset, threadPool));

        std::shared_ptr<TileCache> tileCache = std::shared_ptr<TileCache>(new TileCache(_tileProviderInitData.cacheSize));

        std::shared_ptr<CachingTileProvider> tileProvider= std::shared_ptr<CachingTileProvider>(
            new CachingTileProvider(tileReader, tileCache,
                _tileProviderInitData.framesUntilRequestQueueFlush));

        return tileProvider;
    }
    
    std::string TemporalTileProvider::getGdalDatasetXML(Time t) {
        TimeKey timekey = _timeFormat->stringify(t);
        return getGdalDatasetXML(timekey);
    }

    std::string TemporalTileProvider::getGdalDatasetXML(TimeKey timeKey) {
        std::string xmlTemplate(_gdalXmlTemplate);
        size_t pos = xmlTemplate.find(TIME_PLACEHOLDER);
        size_t numChars = TIME_PLACEHOLDER.length();
        ghoul_assert(pos != std::string::npos, "Invalid dataset file");
        std::string timeSpecifiedXml = xmlTemplate.replace(pos, numChars, timeKey);
        return timeSpecifiedXml;
    }





    //////////////////////////////////////////////////////////////////////////////////////
    //                                 Time Id Providers                                //
    //////////////////////////////////////////////////////////////////////////////////////
    std::string YYYY_MM_DD::stringify(const Time& t) const {
        return t.ISO8601().substr(0, 10);
    }

    std::string YYYY_MM_DDThh_mm_ssZ::stringify(const Time& t) const {
        return t.ISO8601().substr(0, 19) + "Z";
    }

    

    //////////////////////////////////////////////////////////////////////////////////////
    //                            Time Id Providers Facotry                             //
    //////////////////////////////////////////////////////////////////////////////////////

    bool TimeIdProviderFactory::initialized = false;

    std::unordered_map<std::string, TimeFormat*> TimeIdProviderFactory::_timeIdProviderMap = std::unordered_map<std::string, TimeFormat*>();

    void TimeIdProviderFactory::init() {
        _timeIdProviderMap.insert({ "YYYY-MM-DD", new YYYY_MM_DD });
        _timeIdProviderMap.insert({ "YYYY-MM-DDThh:mm:ssZ", new YYYY_MM_DDThh_mm_ssZ });
        initialized = true;
    }

    TimeFormat* TimeIdProviderFactory::getProvider(const std::string& format) {
        if (!initialized) {
            init();
        }
        return _timeIdProviderMap[format];
    }




    //////////////////////////////////////////////////////////////////////////////////////
    //                                  Time Quantizer                                  //
    //////////////////////////////////////////////////////////////////////////////////////
    TimeQuantizer::TimeQuantizer(const Time& start, const Time& end, double resolution)
        : _start(start.unsyncedJ2000Seconds())
        , _end(end.unsyncedJ2000Seconds())
        , _resolution(resolution)
    {

    }

    TimeQuantizer::TimeQuantizer(const Time& start, const Time& end, const std::string& resolution)
        : TimeQuantizer(start, end, parseTimeResolutionStr(resolution))
    {

    }

    double TimeQuantizer::parseTimeResolutionStr(const std::string& resoltutionStr) {
        const char unit = resoltutionStr.back();
        double value = std::stod(resoltutionStr);

        // convert value to seconds, based on unit.
        // The switch statment has intentional fall throughs
        switch (unit) {
        case 'y': value *= 365;
        case 'd': value *= 24.0;
        case 'h': value *= 60.0;
        case 'm': value *= 60.0;
        case 's': value *= 1.0;
            break;
        default:
            ghoul_assert(false, "Invalid unit format. Using default value 1 d");
            value = 60 * 60 * 24;
        }

        return value;
    }

    bool TimeQuantizer::quantize(Time& t) const {
        double unquantized = t.unsyncedJ2000Seconds();
        if (_start <= unquantized && unquantized <= _end) {
            double quantized = std::floor((unquantized - _start) / _resolution) * _resolution + _start;
            t.setTime(quantized);
            return true;
        }
        else if (_clampTime) {
            double clampedTime = unquantized;
            clampedTime = std::max(clampedTime, _start);
            clampedTime = std::min(clampedTime, _end);
            t.setTime(clampedTime);
            return true;
        }
        else {
            return false;
        }
    }
}  // namespace openspace
