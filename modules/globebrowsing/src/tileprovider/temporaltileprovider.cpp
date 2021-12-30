/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/tileprovider/temporaltileprovider.h>

#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include "cpl_minixml.h"
#include <fstream>

namespace {
    constexpr const char* KeyBasePath = "BasePath";

    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* UrlTimePlaceholder = "${OpenSpaceTimeId}";
    constexpr const char* TimeStart = "OpenSpaceTimeStart";
    constexpr const char* TimeEnd = "OpenSpaceTimeEnd";
    constexpr const char* TimeResolution = "OpenSpaceTimeResolution";
    constexpr const char* TimeFormat = "OpenSpaceTimeIdFormat";
    constexpr const char* TimeInterpolation = "OpenSpaceTimeInterpolation";
    constexpr const char* TransferFunction = "OpenSpaceTransferFunction";
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "This is the path to the XML configuration file that describes the temporal tile "
        "information."
    };

    constexpr openspace::properties::Property::PropertyInfo UseFixedTimeInfo = {
        "UseFixedTime",
        "Use Fixed Time",
        "If this value is enabled, the time-varying timevarying dataset will always use "
        "the time that is specified in the 'FixedTime' property, rather than using the "
        "actual time from OpenSpace"
    };

    constexpr openspace::properties::Property::PropertyInfo FixedTimeInfo = {
        "FixedTime",
        "Fixed Time",
        "If the 'UseFixedTime' is enabled, this time will be used instead of the actual "
        "time taken from OpenSpace for the displayed tiles."
    };


    std::string xmlValue(openspace::globebrowsing::TemporalTileProvider& t,
                         CPLXMLNode* node, const std::string& key,
                         const std::string& defaultVal, bool isOptional = false)
    {
        CPLXMLNode* n = CPLSearchXMLNode(node, key.c_str());
        if (!n && !isOptional) {
            throw ghoul::RuntimeError(
                fmt::format("Unable to parse file {}. {} missing", t.filePath.value(), key)
            );
        }

        const bool hasValue = n && n->psChild && n->psChild->pszValue;
        return hasValue ? n->psChild->pszValue : defaultVal;
    }
} // namespace

namespace ghoul {
    template <>
    constexpr openspace::globebrowsing::TemporalTileProvider::TimeFormatType
        from_string(std::string_view string)
    {
        using namespace openspace::globebrowsing;
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
            throw ghoul::RuntimeError("Unknown timeformat '" + std::string(string) + "'");
        }
    }
} // namespace ghoul

namespace openspace::globebrowsing {

TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary)
    : initDict(dictionary)
    , filePath(FilePathInfo)
    , useFixedTime(UseFixedTimeInfo, false)
    , fixedTime(FixedTimeInfo)
{
    ZoneScoped

    filePath = dictionary.value<std::string>(KeyFilePath);
    addProperty(filePath);

    if (dictionary.hasValue<bool>(UseFixedTimeInfo.identifier)) {
        useFixedTime = dictionary.value<bool>(UseFixedTimeInfo.identifier);
    }
    addProperty(useFixedTime);

    if (dictionary.hasValue<std::string>(FixedTimeInfo.identifier)) {
        fixedTime = dictionary.value<std::string>(FixedTimeInfo.identifier);
    }
    addProperty(fixedTime);

    readFilePath();

    if (interpolation) {
        interpolateTileProvider = std::make_unique<InterpolateTileProvider>(dictionary);
        interpolateTileProvider->colormap = colormap;
        interpolateTileProvider->initialize();
        ghoul::Dictionary dict;
        dict.setValue("FilePath", colormap);
        interpolateTileProvider->singleImageProvider =
            std::make_unique<SingleImageProvider>(dict);
    }
}

void TemporalTileProvider::readFilePath() {
    ZoneScoped

    std::ifstream in(filePath.value());
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
        xml = filePath;
    }

    // File path was not a path to a file but a GDAL config or empty
    std::filesystem::path f(filePath.value());
    if (std::filesystem::is_regular_file(f)) {
        initDict.setValue(KeyBasePath, f.parent_path().string());
    }

    gdalXmlTemplate = consumeTemporalMetaData(xml);
}

std::string TemporalTileProvider::consumeTemporalMetaData(const std::string& xml) {
    ZoneScoped

    CPLXMLNode* node = CPLParseXMLString(xml.c_str());

    std::string timeStart = xmlValue(*this, node, TimeStart, "2000 Jan 1");
    std::string timeResolution = xmlValue(*this, node, TimeResolution, "2d");
    std::string timeEnd = xmlValue(*this, node, TimeEnd, "Today");
    std::string timeIdFormat = xmlValue(
        *this,
        node,
        TimeFormat,
        "YYYY-MM-DDThh:mm:ssZ"
    );
    std::string timeInterpolation = xmlValue(
        *this,
        node,
        TimeInterpolation,
        "none",
        true
    );
    colormap = xmlValue(*this, node, TransferFunction, "none", true);

    Time start = Time(timeStart);
    Time end = Time::now();
    Time endOfInterval = Time(timeEnd);
    startTimeJ2000 = start.j2000Seconds();
    endTimeJ2000 = endOfInterval.j2000Seconds();
    if (timeEnd == "Yesterday") {
        end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
    }
    else if (timeEnd != "Today") {
        end.setTime(std::move(timeEnd));
    }

    try {
        timeQuantizer.setStartEndRange(
            std::string(start.ISO8601()),
            std::string(end.ISO8601())
        );
        timeQuantizer.setResolution(timeResolution);
        myResolution = timeResolution;
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::RuntimeError(fmt::format(
            "Could not create time quantizer for Temporal GDAL dataset '{}'. {}",
            filePath.value(), e.message
        ));
    }
    timeFormat = ghoul::from_string<TemporalTileProvider::TimeFormatType>(timeIdFormat);
    interpolation = (timeInterpolation == "linear");

    CPLXMLNode* gdalNode = CPLSearchXMLNode(node, "GDAL_WMS");
    if (gdalNode) {
        std::string gdalDescription = CPLSerializeXMLTree(gdalNode);
        return gdalDescription;
    }
    else {
        gdalNode = CPLSearchXMLNode(node, "FilePath");
        if (gdalNode) {
            std::string gdalDescription = std::string(gdalNode->psChild->pszValue);
            return gdalDescription;
        }
        else {
            return "";
        }
    }
}

void TemporalTileProvider::ensureUpdated() {
    ZoneScoped

    if (!currentTileProvider) {
        update();
    }
}

Tile TemporalTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    ensureUpdated();
    return currentTileProvider->tile(tileIndex);
}

Tile::Status TemporalTileProvider::tileStatus(const TileIndex& index) {
    ensureUpdated();
    return currentTileProvider->tileStatus(index);
}

TileDepthTransform TemporalTileProvider::depthTransform() {
    ensureUpdated();
    return currentTileProvider->depthTransform();
}

void TemporalTileProvider::update() {
    TileProvider* newCurr = getTileProvider(global::timeManager->time());
    if (newCurr) {
        currentTileProvider = newCurr;
    }
    if (currentTileProvider) {
        currentTileProvider->update();
    }
}

void TemporalTileProvider::reset() {
    using K = TemporalTileProvider::TimeKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : tileProviderMap) {
        it.second->reset();
    }
}

int TemporalTileProvider::maxLevel() {
    ensureUpdated();
    return currentTileProvider->maxLevel();
}

float TemporalTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

// Buffer needs at least 22 characters space
std::string_view TemporalTileProvider::timeStringify(TemporalTileProvider::TimeFormatType type, const Time& t) {
    ZoneScoped

    char* buffer = reinterpret_cast<char*>(
        global::memoryManager->TemporaryMemory.allocate(22)
    );

    std::memset(buffer, 0, 22);
    const double time = t.j2000Seconds();

    switch (type) {
    case TemporalTileProvider::TimeFormatType::YYYY_MM_DD:
    {
        constexpr const char Format[] = "YYYY-MM-DD";
        constexpr const int Size = sizeof(Format);
        SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
        return std::string_view(buffer, Size - 1);
    }
    case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss: {
        constexpr const char Format[] = "YYYYMMDD_HRMNSC";
        constexpr const int Size = sizeof(Format);
        SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
        return std::string_view(buffer, Size - 1);
    }
    case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm: {
        constexpr const char Format[] = "YYYYMMDD_HRMN";
        constexpr const int Size = sizeof(Format);
        SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
        return std::string_view(buffer, Size - 1);
    }
    case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ:
    {
        constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SCZ";
        constexpr const int Size = sizeof(Format);
        SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
        return std::string_view(buffer, Size - 1);
    }
    case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ: {
        constexpr const char Format[] = "YYYY-MM-DDTHR_MN_SCZ";
        constexpr const int Size = sizeof(Format);
        SpiceManager::ref().dateFromEphemerisTime(time, buffer, Size, Format);
        return std::string_view(buffer, Size - 1);
    }
    default:
        throw ghoul::MissingCaseException();
    }
}

std::unique_ptr<TileProvider> TemporalTileProvider::initTileProvider(
                                                                 std::string_view timekey)
{
    ZoneScoped

        static const std::vector<std::string> IgnoredTokens = {
        // From: http://www.gdal.org/frmt_wms.html
        "${x}",
        "${y}",
        "${z}",
        "${version}",
        "${format}",
        "${layer}"
    };


    std::string xmlTemplate(gdalXmlTemplate);
    const size_t pos = xmlTemplate.find(UrlTimePlaceholder);
    const size_t numChars = strlen(UrlTimePlaceholder);
    // @FRAGILE:  This will only find the first instance. Dangerous if that instance is
    // commented out ---abock
    std::string xml = xmlTemplate.replace(pos, numChars, timekey);

    xml = FileSys.expandPathTokens(std::move(xml), IgnoredTokens).string();

    initDict.setValue(KeyFilePath, xml);
    return std::make_unique<DefaultTileProvider>(initDict);
}

TileProvider* TemporalTileProvider::getTileProvider(std::string_view timekey) {
    ZoneScoped

    // @TODO (abock, 2020-08-20) This std::string creation can be removed once we switch
    // to C++20 thanks to P0919R2
    const auto it = tileProviderMap.find(std::string(timekey));
    if (it != tileProviderMap.end()) {
        return it->second.get();
    }
    else {
        std::unique_ptr<TileProvider> tileProvider = initTileProvider(timekey);
        tileProvider->initialize();

        TileProvider* res = tileProvider.get();
        tileProviderMap[std::string(timekey)] = std::move(tileProvider);
        return res;
    }
}

TileProvider* TemporalTileProvider::getTileProvider(const Time& time) {
    ZoneScoped

    if (!interpolation) {
        if (useFixedTime && !fixedTime.value().empty()) {
            try {
                return getTileProvider(fixedTime.value());
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC("TemporalTileProvider", e.message);
                return nullptr;
            }
        }
        else {
            Time tCopy(time);
            if (timeQuantizer.quantize(tCopy, true)) {
                std::string_view timeStr = timeStringify(timeFormat, tCopy);
                try {
                    return getTileProvider(timeStr);
                }
                catch (const ghoul::RuntimeError& e) {
                    LERRORC("TemporalTileProvider", e.message);
                    return nullptr;
                }
            }
        }
    }

    Time tCopy(time);
    if (!timeQuantizer.quantize(tCopy, true)) {
        return nullptr;
    }

    Time simulationTime(time);
    Time nextTile;
    Time nextNextTile;
    Time prevTile;
    Time secondToLast;
    Time secondToFirst;

    std::string_view tCopyStr = timeStringify(timeFormat, tCopy);
    try {
        interpolateTileProvider->t1 = getTileProvider(tCopyStr);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("TemporalTileProvider", e.message);
        return nullptr;
    }
    // if the images are for each hour
    if (myResolution == "1h") {
        // the second tile to interpolate between
        nextTile.setTime(tCopy.j2000Seconds() + 60 * 60);
        // the tile after the second tile
        nextNextTile.setTime(tCopy.j2000Seconds() + 120 * 60);
        // the tile before the first tile
        prevTile.setTime(tCopy.j2000Seconds() - 60 * 60 + 1);
        // to make sure that an image outside the dataset is not searched for both ends of
        // the dataset are calculated
        secondToLast.setTime(endTimeJ2000 - 60 * 60);
        secondToFirst.setTime(startTimeJ2000 + 60 * 60);
    }
    // if the images are for each month
    if (myResolution == "1M") {
        // the second tile to interpolate between
        nextTile.setTime(tCopy.j2000Seconds() + 32 * 60 * 60 * 24);
        // the tile after the second tile
        nextNextTile.setTime(tCopy.j2000Seconds() + 64 * 60 * 60 * 24);
        // the tile before the first tile
        prevTile.setTime(tCopy.j2000Seconds() - 2 * 60 * 60 * 24);
        // to make sure that an image outside the dataset is not searched for both ends of
        // the dataset are calculated
        secondToLast.setTime(endTimeJ2000 - 2 * 60 * 60 * 24);
        secondToFirst.setTime(startTimeJ2000 + 32 * 60 * 60 * 24);

        // since months vary in length the time strings are set to the first of each month
        auto setToFirstOfMonth = [](Time& time) {
            std::string timeString = std::string(time.ISO8601());
            timeString[8] = '0';
            timeString[9] = '1';
            time.setTime(timeString);
        };

        setToFirstOfMonth(nextTile);
        setToFirstOfMonth(nextNextTile);
        setToFirstOfMonth(prevTile);
        setToFirstOfMonth(secondToLast);
        setToFirstOfMonth(secondToFirst);
    }

    std::string_view nextTileStr = timeStringify(timeFormat, nextTile);
    std::string_view nextNextTileStr = timeStringify(timeFormat, nextNextTile);
    std::string_view prevTileStr = timeStringify(timeFormat, prevTile);
    try {
        // the necessary tile providers are loaded if they exist within the
        // dataset's timespan
        if (secondToLast.j2000Seconds() > simulationTime.j2000Seconds() &&
            secondToFirst.j2000Seconds() < simulationTime.j2000Seconds())
        {
            interpolateTileProvider->t2 = getTileProvider(nextTileStr);
            interpolateTileProvider->future = getTileProvider(nextNextTileStr);
            interpolateTileProvider->before = getTileProvider(prevTileStr);
        }
        else if (secondToLast.j2000Seconds() < simulationTime.j2000Seconds() &&
                 endTimeJ2000 > simulationTime.j2000Seconds())
        {
            interpolateTileProvider->t2 = getTileProvider(nextTileStr);
            interpolateTileProvider->future = getTileProvider(tCopyStr);
            interpolateTileProvider->before = getTileProvider(prevTileStr);
        }
        else if (secondToFirst.j2000Seconds() > simulationTime.j2000Seconds() &&
                 startTimeJ2000 < simulationTime.j2000Seconds())
        {
            interpolateTileProvider->t2 = getTileProvider(nextTileStr);
            interpolateTileProvider->future = getTileProvider(nextNextTileStr);
            interpolateTileProvider->before = getTileProvider(tCopyStr);
        }
        else {
            interpolateTileProvider->t2 = getTileProvider(tCopyStr);
            interpolateTileProvider->future = getTileProvider(tCopyStr);
            interpolateTileProvider->before = getTileProvider(tCopyStr);
        }
        interpolateTileProvider->factor =
            (simulationTime.j2000Seconds() - tCopy.j2000Seconds()) /
            (nextTile.j2000Seconds() - tCopy.j2000Seconds());

        if (interpolateTileProvider->factor > 1) {
            interpolateTileProvider->factor = 1;
        }
        return interpolateTileProvider.get();
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("TemporalTileProvider", e.message);
        return nullptr;
    }
}

} // namespace openspace::globebrowsing
