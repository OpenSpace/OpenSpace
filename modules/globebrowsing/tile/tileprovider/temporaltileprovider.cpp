/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/tileprovider/temporaltileprovider.h>

#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tileprovider/defaulttileprovider.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringconversion.h>
#include <fstream>
#include "cpl_minixml.h"

namespace {
    constexpr const char* _loggerCat = "TemporalTileProvider";

    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* KeyBasePath = "BasePath";
    constexpr const char* KeyPreCacheStartTime = "PreCacheStartTime";
    constexpr const char* KeyPreCacheEndTime = "PreCacheEndTime";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "This is the path to the XML configuration file that describes the temporal tile "
        "information."
    };
} // namespace

namespace ghoul {

template <>
openspace::globebrowsing::tileprovider::TemporalTileProvider::TimeFormatType
from_string(const std::string& string)
{
    using namespace openspace::globebrowsing::tileprovider;
    if (string == "YYYY-MM-DD") {
        return TemporalTileProvider::TimeFormatType::YYYY_MM_DD;
    }
    else if (string == "YYYY-MM-DDThh:mm:ssZ") {
        return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ;
    }
    else if (string == "YYYY-MM-DDThh_mm_ssZ") {
        return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ;
    }
    else if (string == "YYYYMMDD_hhmmss") {
        return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss;
    }
    else if (string == "YYYYMMDD_hhmm") {
        return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm;
    }
    else {
        throw ghoul::RuntimeError("Unknown timeformat " + string);
    }
}

} // namespace ghoul

namespace openspace::globebrowsing::tileprovider {

namespace {
    std::string timeStringify(TemporalTileProvider::TimeFormatType type, const Time& t) {
        switch (type) {
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DD:
                return t.ISO8601().substr(0, 10);
            case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss: {
                std::string ts = t.ISO8601().substr(0, 19);

                // YYYY_MM_DDThh_mm_ss -> YYYYMMDD_hhmmss
                ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
                ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
                replace(ts.begin(), ts.end(), 'T', '_');
                return ts;
            }
            case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm: {
                std::string ts = t.ISO8601().substr(0, 16);

                // YYYY_MM_DDThh_mm -> YYYYMMDD_hhmm
                ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
                ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
                replace(ts.begin(), ts.end(), 'T', '_');
                return ts;
            }
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ:
                return t.ISO8601().substr(0, 19) + "Z";
            case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ: {
                std::string timeString = t.ISO8601().substr(0, 19) + "Z";
                replace(timeString.begin(), timeString.end(), ':', '_');
                return timeString;
            }
            default:
                throw ghoul::MissingCaseException();
        }
    }
} // namespace

TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary)
    : _initDict(dictionary)
    , _filePath(FilePathInfo)
    , _successfulInitialization(false)
{
    _filePath = dictionary.value<std::string>(KeyFilePath);
    addProperty(_filePath);

    if (readFilePath()) {
        const bool hasStart = dictionary.hasKeyAndValue<std::string>(
            KeyPreCacheStartTime
            );
        const bool hasEnd = dictionary.hasKeyAndValue<std::string>(KeyPreCacheEndTime);
        if (hasStart && hasEnd) {
            const std::string start = dictionary.value<std::string>(KeyPreCacheStartTime);
            const std::string end = dictionary.value<std::string>(KeyPreCacheEndTime);
            _preCacheTimes = _timeQuantizer.quantized(
                Time(Time::convertTime(start)),
                Time(Time::convertTime(end))
            );
        }
        _successfulInitialization = true;
    }
    else {
        LERROR("Unable to read file " + _filePath.value());
        _successfulInitialization = false;
    }
}


bool TemporalTileProvider::initialize() {
    const bool success = TileProvider::initialize();

    if (!_preCacheTimes.empty()) {
        LINFO(fmt::format("Preloading: {}", _filePath.value()));
        for (const Time& t : _preCacheTimes) {
            getTileProvider(t);
        }
        _preCacheTimes.clear();
    }

    return success;
}

bool TemporalTileProvider::readFilePath() {
    std::ifstream in(_filePath.value().c_str());
    std::string xml;
    if (in.is_open()) {
        // read file
        xml = std::string(
            std::istreambuf_iterator<char>(in),
            (std::istreambuf_iterator<char>())
        );
    }
    else {
        // Assume that it is already an xml
        xml = _filePath.value();
    }

    // File path was not a path to a file but a GDAL config or empty
    if (FileSys.fileExists(_filePath.value())) {
        _initDict.setValue<std::string>(
            KeyBasePath,
            ghoul::filesystem::File(_filePath.value()).directoryName()
        );
    }

    _gdalXmlTemplate = consumeTemporalMetaData(xml);
    return true;
}

std::string TemporalTileProvider::consumeTemporalMetaData(const std::string& xml) {
    CPLXMLNode* node = CPLParseXMLString(xml.c_str());

    std::string timeStart = getXMLValue(node, TemporalXMLTags::TimeStart, "2000 Jan 1");
    std::string timeResolution = getXMLValue(node, TemporalXMLTags::TimeResolution, "2d");
    std::string timeEnd = getXMLValue(node, TemporalXMLTags::TimeEnd, "Today");
    std::string timeIdFormat = getXMLValue(
        node,
        TemporalXMLTags::TimeFormat,
        "YYYY-MM-DDThh:mm:ssZ"
    );

    Time start;
    start.setTime(std::move(timeStart));
    Time end(Time::now());
    if (timeEnd == "Yesterday") {
        end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
    }
    else if (timeEnd != "Today") {
        end.setTime(std::move(timeEnd));
    }

    try {
        _timeQuantizer = TimeQuantizer(start, end, timeResolution);
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::RuntimeError(fmt::format(
            "Could not create time quantizer for Temporal GDAL dataset '{}'. {}",
            _filePath.value(), e.message
        ));
    }
    _timeFormat = ghoul::from_string<TimeFormatType>(timeIdFormat);

    std::string gdalDescription;
    CPLXMLNode* gdalNode = CPLSearchXMLNode(node, "GDAL_WMS");
    if (gdalNode) {
        gdalDescription = CPLSerializeXMLTree(gdalNode);
    }
    if (!gdalNode) {
        gdalNode = CPLSearchXMLNode(node, "FilePath");
        gdalDescription = std::string(gdalNode->psChild->pszValue);
    }

    return gdalDescription;
}

std::string TemporalTileProvider::getXMLValue(CPLXMLNode* node, const std::string& key,
                                              const std::string& defaultVal)
{
    CPLXMLNode* n = CPLSearchXMLNode(node, key.c_str());
    if (!n) {
        throw ghoul::RuntimeError(
            fmt::format("Unable to parse file {}. {} missing", _filePath.value(), key)
        );
    }

    const bool hasValue = n && n->psChild && n->psChild->pszValue;
    return hasValue ? n->psChild->pszValue : defaultVal;
}

TileDepthTransform TemporalTileProvider::depthTransform() {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->depthTransform();
    }
    else {
        return { 1.f, 0.f };
    }
}

Tile::Status TemporalTileProvider::tileStatus(const TileIndex& tileIndex) {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->tileStatus(tileIndex);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

Tile TemporalTileProvider::tile(const TileIndex& tileIndex) {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->tile(tileIndex);
    }
    else {
        return Tile::TileUnavailable;
    }
}

int TemporalTileProvider::maxLevel() {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->maxLevel();
    }
    else {
        return 0;
    }
}

void TemporalTileProvider::ensureUpdated() {
    if (!_currentTileProvider) {
        LDEBUG("Warning: update was done lazily");
        update();
    }
}

void TemporalTileProvider::update() {
    if (_successfulInitialization) {
        std::shared_ptr<TileProvider> newCurrent = getTileProvider(
            global::timeManager.time()
        );
        if (newCurrent) {
            _currentTileProvider = newCurrent;
        }
        _currentTileProvider->update();
    }
}

void TemporalTileProvider::reset() {
    if (_successfulInitialization) {
        using K = TimeKey;
        using V = std::shared_ptr<TileProvider>;
        for (std::pair<const K, V>& it : _tileProviderMap) {
            it.second->reset();
        }
    }
}
std::shared_ptr<TileProvider> TemporalTileProvider::getTileProvider(const Time& t) {
    Time tCopy(t);
    if (_timeQuantizer.quantize(tCopy, true)) {
        TimeKey timeKey = timeStringify(_timeFormat, tCopy);
        try {
            return getTileProvider(timeKey);
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(e.message);
            return nullptr;
        }
    }
    return nullptr;
}

std::shared_ptr<TileProvider> TemporalTileProvider::getTileProvider(
                                                                   const TimeKey& timekey)
{
    const auto it = _tileProviderMap.find(timekey);
    if (it != _tileProviderMap.end()) {
        return it->second;
    }
    else {
        std::shared_ptr<TileProvider> tileProvider = initTileProvider(timekey);
        tileProvider->initialize();

        _tileProviderMap[timekey] = tileProvider;
        return tileProvider;
    }
}

std::shared_ptr<TileProvider> TemporalTileProvider::initTileProvider(TimeKey timekey) {
    static const std::vector<std::string> AllowedTokens = {
        // From: http://www.gdal.org/frmt_wms.html
        // @FRAGILE:  What happens if a user specifies one of these as path tokens?
        // ---abock
        "${x}",
        "${y}",
        "${z}",
        "${version}",
        "${format}",
        "${layer}"
    };

    std::string gdalDatasetXml = getGdalDatasetXML(std::move(timekey));
    FileSys.expandPathTokens(gdalDatasetXml, AllowedTokens);

    _initDict.setValue<std::string>(KeyFilePath, gdalDatasetXml);
    std::shared_ptr<DefaultTileProvider> tileProvider =
        std::make_shared<DefaultTileProvider>(_initDict);
    return tileProvider;
}

std::string TemporalTileProvider::getGdalDatasetXML(const Time& t) {
    TimeKey timeKey = timeStringify(_timeFormat, t);
    return getGdalDatasetXML(timeKey);
}

std::string TemporalTileProvider::getGdalDatasetXML(const TimeKey& timeKey) {
    std::string xmlTemplate(_gdalXmlTemplate);
    const size_t pos = xmlTemplate.find(UrlTimePlaceholder);
    //size_t numChars = std::string(UrlTimePlaceholder).length();
    const size_t numChars = strlen(UrlTimePlaceholder);
    // @FRAGILE:  This will only find the first instance. Dangerous if that instance is
    // commented out ---abock
    const std::string timeSpecifiedXml = xmlTemplate.replace(pos, numChars, timeKey);
    return timeSpecifiedXml;
}

} // namespace openspace::globebrowsing::tileprovider

#endif // GLOBEBROWSING_USE_GDAL
