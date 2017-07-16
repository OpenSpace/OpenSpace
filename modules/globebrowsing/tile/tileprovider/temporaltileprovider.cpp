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

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/tileprovider/temporaltileprovider.h>

#include <modules/globebrowsing/tile/tileprovider/defaulttileprovider.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logmanager.h>

#include "cpl_minixml.h"
#include <fmt/format.h>
#include <fstream>

namespace {
    const char* _loggerCat = "TemporalTileProvider";

    const char* KeyFilePath = "FilePath";
    const char* KeyBasePath = "BasePath";
    const char* KeyPreCacheStartTime = "PreCacheStartTime";
    const char* KeyPreCacheEndTime = "PreCacheEndTime";
} // namespace

namespace openspace::globebrowsing::tileprovider {
    
const char* TemporalTileProvider::URL_TIME_PLACEHOLDER("${OpenSpaceTimeId}");

const char* TemporalTileProvider::TemporalXMLTags::TIME_START = "OpenSpaceTimeStart";
const char* TemporalTileProvider::TemporalXMLTags::TIME_END = "OpenSpaceTimeEnd";
const char* TemporalTileProvider::TemporalXMLTags::TIME_RESOLUTION =
                                                                "OpenSpaceTimeResolution";
const char* TemporalTileProvider::TemporalXMLTags::TIME_FORMAT = "OpenSpaceTimeIdFormat";

TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary) 
    : _initDict(dictionary)
    , _filePath("filePath", "File Path", "")
    , _successfulInitialization(false)
{
    std::string filePath;
    dictionary.getValue<std::string>(KeyFilePath, filePath);
    try {
        filePath = absPath(filePath);
    }
    catch (const std::runtime_error& e) {
        // File path was not a path to a file but a GDAL config or empty
    }
  
    _filePath.setValue(filePath);
    addProperty(_filePath);
  
    if (readFilePath()) {
        const bool hasStart = dictionary.hasKeyAndValue<std::string>(KeyPreCacheStartTime);
        const bool hasEnd = dictionary.hasKeyAndValue<std::string>(KeyPreCacheEndTime);
        if (hasStart && hasEnd) {
            const std::string start = dictionary.value<std::string>(KeyPreCacheStartTime);
            const std::string end = dictionary.value<std::string>(KeyPreCacheEndTime);
            std::vector<Time> preCacheTimes = _timeQuantizer.quantized(
                Time(Time::convertTime(start)),
                Time(Time::convertTime(end))
            );

            LINFO("Preloading: " << _filePath.value());
            for (const Time& t : preCacheTimes) {
                getTileProvider(t);
            }
        }
        _successfulInitialization = true;
    }
    else {
        LERROR("Unable to read file " + _filePath.value());
        _successfulInitialization = false;
    }  
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

    try {
        _initDict.setValue<std::string>(
            KeyBasePath,
            ghoul::filesystem::File(_filePath.value()).directoryName()
        );
    }
    catch (const std::runtime_error& e) {
        // File path was not a path to a file but a GDAL config or empty
    }

    _gdalXmlTemplate = consumeTemporalMetaData(xml);
    return true;
}

std::string TemporalTileProvider::consumeTemporalMetaData(const std::string& xml) {
    CPLXMLNode* node = CPLParseXMLString(xml.c_str());

    std::string timeStart = getXMLValue(
        node,
        TemporalXMLTags::TIME_START,
        "2000 Jan 1"
    );
    std::string timeResolution = getXMLValue(
        node,
        TemporalXMLTags::TIME_RESOLUTION,
        "2d"
    );
    std::string timeEnd = getXMLValue(
        node,
        TemporalXMLTags::TIME_END,
        "Now"
    );
    std::string timeIdFormat = getXMLValue(
        node,
        TemporalXMLTags::TIME_FORMAT,
        "YYYY-MM-DDThh:mm:ssZ"
    );

    Time start; start.setTime(timeStart);
    Time end(Time::now());
    if (timeEnd == "Yesterday") {
        end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
    }
    else if (timeEnd != "Now") {
        end.setTime(timeEnd);
    }

    try {
        _timeQuantizer = TimeQuantizer(start, end, timeResolution);
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::RuntimeError(
            "Could not create time quantizer for Temporal GDAL dataset '" +
            _filePath.value() + "'. " + e.message);
    }
    _timeFormat = TimeIdProviderFactory::getProvider(timeIdFormat);
    if (!_timeFormat) {
        throw ghoul::RuntimeError(
            "Invalid Time Format " + timeIdFormat + " in " + _filePath.value()
        );
    }

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

std::string TemporalTileProvider::getXMLValue(CPLXMLNode* root, const std::string& key,
                                              const std::string& defaultVal)
{
    CPLXMLNode * n = CPLSearchXMLNode(root, key.c_str());
    if (!n) {
        throw ghoul::RuntimeError(
            "Unable to parse file " + _filePath.value() + ". " + key + " missing."
        );
    }

    bool hasValue =
        (n != nullptr && n->psChild != nullptr && n->psChild->pszValue != nullptr);
    return hasValue ? std::string(n->psChild->pszValue) : defaultVal;
}

TileDepthTransform TemporalTileProvider::depthTransform() {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->depthTransform();    
    }
    else {
        return { 1.0f, 0.0f};
    }
}

Tile::Status TemporalTileProvider::getTileStatus(const TileIndex& tileIndex) {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->getTileStatus(tileIndex);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

Tile TemporalTileProvider::getTile(const TileIndex& tileIndex) {
    if (_successfulInitialization) {
        ensureUpdated();
        return _currentTileProvider->getTile(tileIndex);
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
    if (_currentTileProvider == nullptr) {
        LDEBUG("Warning: update was done lazily");
        update();
    }
}

void TemporalTileProvider::update() {
    if (_successfulInitialization) {
        std::shared_ptr<TileProvider> newCurrent = getTileProvider();
        if (newCurrent) {
            _currentTileProvider = newCurrent;
        }
        _currentTileProvider->update();
    }
}

void TemporalTileProvider::reset() {
    if (_successfulInitialization) {
        for (std::pair<const TimeKey, std::shared_ptr<TileProvider>>& it : _tileProviderMap) {
            it.second->reset();
        }
    }
}
std::shared_ptr<TileProvider> TemporalTileProvider::getTileProvider(const Time& t) {
    Time tCopy(t);
    if (_timeQuantizer.quantize(tCopy, true)) {
        TimeKey timekey = _timeFormat->stringify(tCopy);
        try {
            return getTileProvider(timekey);
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
    std::unordered_map<TimeKey, std::shared_ptr<TileProvider>>::iterator it = _tileProviderMap.find(timekey);
    if (it != _tileProviderMap.end()) {
        return it->second;
    }
    else {
        std::shared_ptr<TileProvider> tileProvider = initTileProvider(timekey);

        _tileProviderMap[timekey] = tileProvider;
        return tileProvider;
    }
}

std::shared_ptr<TileProvider> TemporalTileProvider::initTileProvider(TimeKey timekey) {
    static const std::vector<std::string> AllowedToken = {
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

    std::string gdalDatasetXml = getGdalDatasetXML(timekey);
    FileSys.expandPathTokens(gdalDatasetXml, AllowedToken);

    _initDict.setValue<std::string>(KeyFilePath, gdalDatasetXml);
    auto tileProvider = std::make_shared<DefaultTileProvider>(_initDict);
    return tileProvider;
}
    
std::string TemporalTileProvider::getGdalDatasetXML(Time t) {
    TimeKey timekey = _timeFormat->stringify(t);
    return getGdalDatasetXML(timekey);
}

std::string TemporalTileProvider::getGdalDatasetXML(TimeKey timeKey) {
    std::string xmlTemplate(_gdalXmlTemplate);
    size_t pos = xmlTemplate.find(URL_TIME_PLACEHOLDER);
    //size_t numChars = std::string(URL_TIME_PLACEHOLDER).length();
    size_t numChars = strlen(URL_TIME_PLACEHOLDER);
    // @FRAGILE:  This will only find the first instance. Dangerous if that instance is
    // commented out ---abock
    std::string timeSpecifiedXml = xmlTemplate.replace(pos, numChars, timeKey);
    return timeSpecifiedXml;
}

std::string YYYY_MM_DD::stringify(const Time& t) const {
    return t.ISO8601().substr(0, 10);
}

std::string YYYYMMDD_hhmmss::stringify(const Time& t) const {
    std::string ts = t.ISO8601().substr(0, 19);

    // YYYY_MM_DDThh_mm_ss -> YYYYMMDD_hhmmss
    ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
    ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
    replace(ts.begin(), ts.end(), 'T', '_');
    return ts;
}

std::string YYYYMMDD_hhmm::stringify(const Time& t) const {
    std::string ts = t.ISO8601().substr(0, 16);
    
    // YYYY_MM_DDThh_mm -> YYYYMMDD_hhmm
    ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
    ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
    replace(ts.begin(), ts.end(), 'T', '_');
    return ts;
}

std::string YYYY_MM_DDThhColonmmColonssZ::stringify(const Time& t) const {
    return t.ISO8601().substr(0, 19) + "Z";
}
    
std::string YYYY_MM_DDThh_mm_ssZ::stringify(const Time& t) const {
    std::string timeString = t.ISO8601().substr(0, 19) + "Z";
    replace( timeString.begin(), timeString.end(), ':', '_' );
    return timeString;
}

bool TimeIdProviderFactory::initialized = false;

std::unordered_map<std::string, std::unique_ptr<TimeFormat>>
    TimeIdProviderFactory::_timeIdProviderMap =
    std::unordered_map<std::string, std::unique_ptr<TimeFormat>>();

void TimeIdProviderFactory::init() {
    _timeIdProviderMap.insert(std::pair<std::string, std::unique_ptr<TimeFormat>>(
        { "YYYY-MM-DD" , std::make_unique<YYYY_MM_DD>() }
    ));
    _timeIdProviderMap.insert(std::pair<std::string, std::unique_ptr<TimeFormat>>(
        { "YYYY-MM-DDThh:mm:ssZ", std::make_unique<YYYY_MM_DDThhColonmmColonssZ>() }
    ));
    _timeIdProviderMap.insert(std::pair<std::string, std::unique_ptr<TimeFormat>>(
        { "YYYY-MM-DDThh_mm_ssZ", std::make_unique<YYYY_MM_DDThh_mm_ssZ>() }
    ));
    _timeIdProviderMap.insert(std::pair<std::string, std::unique_ptr<TimeFormat>>(
        { "YYYYMMDD_hhmmss", std::make_unique<YYYYMMDD_hhmmss>() }
    ));
    _timeIdProviderMap.insert(std::pair<std::string, std::unique_ptr<TimeFormat>>(
        { "YYYYMMDD_hhmm" , std::make_unique<YYYYMMDD_hhmm>() }
    ));
    initialized = true;
}

TimeFormat* TimeIdProviderFactory::getProvider(const std::string& format) {
    if (!initialized) {
        init();
    }
    ghoul_assert(
        _timeIdProviderMap.find(format) != _timeIdProviderMap.end(), 
        "Unsupported Time format: " + format
    );
    return _timeIdProviderMap[format].get();
}

TimeQuantizer::TimeQuantizer(const Time& start, const Time& end, double resolution)
    : _timerange(start.j2000Seconds(), end.j2000Seconds())
    , _resolution(resolution)
{}

TimeQuantizer::TimeQuantizer(const Time& start, const Time& end,
                             const std::string& resolution)
    : TimeQuantizer(start, end, parseTimeResolutionStr(resolution))
{}

double TimeQuantizer::parseTimeResolutionStr(const std::string& resoltutionStr) {
    const char unit = resoltutionStr.back();
    std::string numberString = resoltutionStr.substr(0, resoltutionStr.length() - 1);
        
    char* p;
    double value = strtol(numberString.c_str(), &p, 10);
    if (*p) { // not a number
        throw ghoul::RuntimeError("Cannot convert " + numberString + " to number");
    }
    else {
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
            throw ghoul::RuntimeError("Invalid unit format '" + std::string(1, unit) +
                "'. Expected 'y', 'd', 'h', 'm' or 's'.");
        }
        return value;
    }
}

bool TimeQuantizer::quantize(Time& t, bool clamp) const {
    double unquantized = t.j2000Seconds();
    if (_timerange.includes(unquantized)) {
        double quantized = std::floor((unquantized - _timerange.start) / _resolution) * _resolution + _timerange.start;
        t.setTime(quantized);
        return true;
    }
    else if (clamp) {
        double clampedTime = unquantized;
        clampedTime = std::max(clampedTime, _timerange.start);
        clampedTime = std::min(clampedTime, _timerange.end);
        t.setTime(clampedTime);
        return true;
    }
    else {
        return false;
    }
}

std::vector<Time> TimeQuantizer::quantized(const Time& start, const Time& end) const {
    Time s = start;
    quantize(s, true);

    Time e = end;
    quantize(e, true);

    const double startSeconds = s.j2000Seconds();
    const double endSeconds = e.j2000Seconds();
    const double delta = endSeconds - startSeconds;

    ghoul_assert(int(delta) % int(_resolution) == 0, "Quantization error");
    const int nSteps = delta / _resolution;

    std::vector<Time> result(nSteps + 1);
    for (int i = 0; i <= nSteps; ++i) {
        result[i].setTime(startSeconds + i * _resolution, false);
    }

    return result;
}

} // namespace openspace::globebrowsing::tileprovider

#endif // GLOBEBROWSING_USE_GDAL
