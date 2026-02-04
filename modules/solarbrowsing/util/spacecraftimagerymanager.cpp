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

#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/json.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/threadpool.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <format>
#include <fstream>
#include <future>
#include <string>
#include <iostream>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "SpacecraftImageryManager";
    constexpr const double SUN_RADIUS = 1391600000.0 * 0.5;
} // namespace

namespace openspace {

// Conversion needed before passing dates into the spice manager
std::string SpacecraftImageryManager::ISO8601(std::string& datetime) {
    std::string month = datetime.substr(5, 3);

    std::string MM = "";
    if (month == "JAN") MM = "01";
    else if (month == "FEB") MM = "02";
    else if (month == "MAR") MM = "03";
    else if (month == "APR") MM = "04";
    else if (month == "MAY") MM = "05";
    else if (month == "JUN") MM = "06";
    else if (month == "JUL") MM = "07";
    else if (month == "AUG") MM = "08";
    else if (month == "SEP") MM = "09";
    else if (month == "OCT") MM = "10";
    else if (month == "NOV") MM = "11";
    else if (month == "DEC") MM = "12";
    else ghoul_assert(false, "Bad month");

    datetime.replace(4, 5, "-" + MM + "-");
    return datetime;
}

void SpacecraftImageryManager::loadTransferFunctions(const std::filesystem::path& dir,
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap)
{

    if (!std::filesystem::is_directory(dir)) {
        LERROR(std::format("Could not load directory '{}'", dir.string()));
    }

    std::vector<std::filesystem::path> sequencePaths = ghoul::filesystem::walkDirectory(
        dir,
        ghoul::filesystem::Recursive::Yes,
        ghoul::filesystem::Sorted::Yes
    );

    for (const std::filesystem::path& seqPath : sequencePaths) {
        if (seqPath.extension() == ".txt") {
            std::string key = seqPath.stem().string();
            _tfMap[key] = std::make_shared<TransferFunction>(seqPath);
        }
    }
}

bool SpacecraftImageryManager::loadMetadataFromDisk(const std::filesystem::path& rootDir,
           std::unordered_map<std::string, Timeline<ImageMetadata>>& imageMetadataMap)
{
    if (!std::filesystem::is_directory(rootDir)) {
        throw ghoul::RuntimeError(std::format(
            "Could not load directory '{}'", rootDir
        ));
    }

    bool metadataLoaded = false;

    std::vector<std::filesystem::path> sequencePaths = ghoul::filesystem::walkDirectory(
        rootDir,
        ghoul::filesystem::Recursive::No,
        ghoul::filesystem::Sorted::No
    );

    for (const std::filesystem::path& seqPath : sequencePaths) {
        const std::string extension = seqPath.extension().string();
        const std::string base = seqPath.filename().string();
        const size_t foundCachedImageData = base.find("_cached");
        if (extension != ".txt" || foundCachedImageData == std::string::npos) {
            continue;
        }

        const size_t separator = base.rfind("_");
        const std::string instrument = base.substr(0, separator);
        LDEBUG(std::format("Loading instrument: {}", instrument));

        metadataLoaded = true;
        std::ifstream myfile(seqPath);
        if (!myfile.is_open()) {
            LERROR(std::format("Failed to open metadata file '{}'", seqPath));
            return false;
        }

        int numStates;
        myfile >> numStates;

        for (int i = 0; i < numStates; i++) {
            ImageMetadata im;

            myfile >> std::ws; // Skip the rest of the line
            std::string date;
            std::getline(myfile, date);

            if (date.empty()) {
                LERROR(std::format(
                    "Failed to read metadata state: date, file: '{}'", seqPath
                ));
                return false;
            }

            double timeObserved =
                SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

            std::string relPath;
            myfile >> relPath;

            if (myfile.bad()) {
                LERROR(std::format(
                    "Failed to read metadata state: relPath, file: '{}'", seqPath
                ));
                return false;
            }

            im.filePath = rootDir / relPath;

            myfile >> im.fullResolution;

            if (myfile.bad()) {
                LERROR(std::format(
                    "Failed to read metadata state: fullResolution, file: '{}'", seqPath
                ));
                return false;
            }

            myfile >> im.scale;

            if (myfile.bad()) {
                LERROR(std::format(
                    "Failed to read metadata state: scale, file: '{}'", seqPath
                ));
                return false;
            }

            float x, y;
            myfile >> x >> y;
            im.centerPixel = glm::vec2(x,y);
            myfile >> im.isCoronaGraph;

            if (myfile.bad()) {
                LERROR(std::format(
                    "Failed to read metadata state : isCoronaGraph, file : '{}'", seqPath
                ));
                return false;
            }

            imageMetadataMap[instrument].addKeyframe(timeObserved, std::move(im));
        }
        myfile.close();
    }
    return metadataLoaded;
}

void SpacecraftImageryManager::saveMetadataToDisk(const std::filesystem::path& rootPath,
           std::unordered_map<std::string, Timeline<ImageMetadata>>& _imageMetadataMap)
{
    using K = std::string;
    using V = Timeline<ImageMetadata>;
    for (const std::pair<K, V>& instrument : _imageMetadataMap) {
        const std::filesystem::path cacheFile =
            rootPath / std::format("{}_cached.txt", instrument.first);

        std::ofstream ofs(cacheFile);
        if (!ofs.is_open()) {
            LERROR(std::format("Failed to open file '{}'", cacheFile));
            continue;
        }

        const Timeline<ImageMetadata>& sequence = instrument.second;
        ofs << sequence.nKeyframes() << '\n';

        for (const Keyframe<ImageMetadata>& metadata : sequence.keyframes()) {
            const std::string date = SpiceManager::ref().dateFromEphemerisTime(
                metadata.timestamp
            );

            const ImageMetadata& im = metadata.data;
            const std::filesystem::path relativePath = std::filesystem::relative(
                im.filePath,
                rootPath
            );

            ofs << std::format("{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
                date,
                relativePath.generic_string(),
                im.fullResolution,
                im.scale,
                im.centerPixel.x,
                im.centerPixel.y,
                static_cast<int>(im.isCoronaGraph) // Output bool as 0/1
            );
        }
        ofs.close();
    }
}

// @TODO emiax: If openjpeg ever starts supporting reading XML metadata,
// this implementation should be improved in order not to search the entire buffer for
// XML data. There is an issue here:
// (https://github.com/uclouvain/openjpeg/issues/929)
ImageMetadata SpacecraftImageryManager::parseJ2kMetadata(
                                                    const std::filesystem::path& filePath)
{
    ImageMetadata im;
    im.filePath = filePath;

    std::ifstream stream(filePath, std::ios::binary | std::ios::ate);
    std::streamsize size = stream.tellg();
    stream.seekg(0, std::ios::beg);
    std::vector<char> buffer(size);
    if (!stream.read(buffer.data(), size)) {
        LERROR(std::format("Failed to read data from '{}' ", filePath));
        return im;
    }
    std::string_view bufferView(buffer.data(), size);

    auto extractInnerXml = [](std::string_view view, const std::string& elementName) ->
        std::optional<std::string_view>
        {
            const std::string startTag =
                std::string("<") + elementName + std::string(">");
            const std::string endTag = std::string("</") + elementName + std::string(">");

            const auto begin =
                std::search(view.begin(), view.end(), startTag.begin(), startTag.end());
            if (begin == view.end()) {
                return std::nullopt;
;           }
            const auto afterBeginTag = begin + startTag.size();

            const auto end =
                std::search(afterBeginTag, view.end(), endTag.begin(), endTag.end());
            if (end == view.end()) {
                return std::nullopt;
            }
            return std::string_view(&*afterBeginTag, end - afterBeginTag);
        };

    std::optional<std::string_view> metaData = extractInnerXml(bufferView, "meta");
    if (!metaData.has_value()) {
        LERROR(std::format("Could not find metadata in {}", filePath));
        return im;
    }

    std::optional<std::string_view> telescop =
        extractInnerXml(metaData.value(), "TELESCOP");
    if (!telescop.has_value()) {
        LERROR(std::format("Could not find TELESCOP tag {}", filePath));
        return im;
    }

    if (std::optional<std::string_view> naxis =
        extractInnerXml(metaData.value(), "NAXIS1"))
    {
        im.fullResolution = std::stoi(std::string(naxis.value()));
    }
    else {
        LERROR(std::format("Could not find NAXIS1 tag {}", filePath));
        return im;
    }

    const float halfRes = im.fullResolution / 2.f;

    glm::vec2 centerPixel;
    if (std::optional<std::string_view> centerPixelX =
        extractInnerXml(bufferView, "CRPIX1"))
    {
        centerPixel.x = std::stof(std::string(centerPixelX.value()));
    }
    else {
        LERROR(std::format("Could not find CRPIX1 tag {}", filePath));
        return im;
    }

    if (std::optional<std::string_view> centerPixelY =
        extractInnerXml(bufferView, "CRPIX2"))
    {
        centerPixel.y = std::stof(std::string(centerPixelY.value()));
    }
    else {
        LERROR(std::format("Could not find CRPIX2 tag {}", filePath));
        return im;
    }

    const glm::vec2 offset = ((halfRes - centerPixel) / halfRes) * glm::vec2(SUN_RADIUS);
    im.centerPixel = offset;

    if (telescop.value() == "SOHO") {
        if (std::optional<std::string_view> plateScl =
            extractInnerXml(metaData.value(), "PLATESCL"))
        {
            const float plateScale = std::stof(std::string(plateScl.value()));
            im.scale = 1.f / (plateScale / 2.f);
            im.isCoronaGraph = true;
        }
        else {
            LERROR(std::format("Could not find NAXIS1 tag {}", filePath));
            return im;
        }
    }
    else if (telescop.value() == "SDO") {
        std::optional<std::string_view> rsunObs = extractInnerXml(bufferView, "RSUN_OBS");
        std::optional<std::string_view> cDelt1 = extractInnerXml(bufferView, "CDELT1");

        if (!rsunObs.has_value()) {
            LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
            return im;
        }
        if (!cDelt1.has_value()) {
            LERROR(std::format("Could not find CDELT1 tag {}", filePath));
            return im;
        }

        im.scale = (std::stof(std::string(rsunObs.value())) /
            std::stof(std::string(cDelt1.value()))) /
            (im.fullResolution / 2.f);
        im.isCoronaGraph = false;
    }
    else { // Telescope is assumed to be STEREO
        std::optional<std::string_view> rsun = extractInnerXml(bufferView, "RSUN");
        std::optional<std::string_view> cDelt1 = extractInnerXml(bufferView, "CDELT1");
        if (!rsun.has_value()) {
            LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
            return im;
        }
        if (!cDelt1.has_value()) {
            LERROR(std::format("Could not find CDELT1 tag {}", filePath));
            return im;
        }

        im.scale = (std::stof(std::string(rsun.value())) /
            std::stof(std::string(cDelt1.value()))) /
            (im.fullResolution / 2.f);
        im.isCoronaGraph = false;

        if (std::optional<std::string_view> detector =
            extractInnerXml(bufferView, "DETECTOR"))
        {
            im.isCoronaGraph =
                detector.value() == "COR1" || detector.value() == "COR2";
        }
        else {
            LWARNING(std::format(
                "Could not find DETECTOR tag {}", filePath
            ));
        }
    }
    return im;
}

// This is currently not used. Instead, the parseJ2kMetadata is used,
// extracting the data directoy from the JPEG2000 file by naively searching the entire
// buffer for metadata, avoiding pre-processing steps.
// If you want to use this, you need to extract metadata to json first,
// for example using: https://github.com/novalain/j2kcodec
ImageMetadata SpacecraftImageryManager::parseJsonMetadata(
                                                      const ghoul::filesystem::File& file)
{
    ImageMetadata im;
    //im.filename = file.path().string();
    //const std::string filename = std::string(file.path().string() + ".json");

    //if (!std::filesystem::exists(filename)) {
    //    LERROR(std::format("'{}' has no specified json metadata", file.path()));
    //    // TODO: Hardcoded values, when there are no json files.
    //    return im;
    //}

    //// Parse JSON metadata
    //using json = nlohmann::json;

    //std::ifstream i(filename);
    //if (i.fail()) {
    //    LERROR(std::format("Error opening file '{}'", filename));
    //}

    //json j;
    //i >> j;
    //const json data = j["meta"]["fits"];

    //if (data.is_null()) {
    //    LERROR(std::format("Error in metadata '{}'", filename));
    //}

    //json value = data["TELESCOP"];
    //if (value.is_null() && !value.is_string()) {
    //    LERROR("Metadata did contain information about type of spacecraft");
    //    return im;
    //}
    //// TODO: value might not exist
    //std::string spacecraftType = value;

    //value = data["NAXIS1"];
    //if (value.is_null()) {
    //    LERROR("Metadata did not contain information about resolution");
    //}

    //const std::string sFullResolution = value;
    //im.fullResolution = std::stoi(sFullResolution);
    //// Special case of sdo - RSUN is given in pixels
    //// For SOHO the radius of the sun is not specified - we instead use platescl
    //if (spacecraftType == "SOHO") {
    //    const std::string sScale = data["PLATESCL"];
    //    const float plateScale = stof(sScale);
    //    im.scale = 1.f / (plateScale / 2.f);
    //    im.isCoronaGraph = true;
    //} else {
    //    float sunRadiusPixels = 0.f;
    //    // SDO has RSUN specified in pixels
    //    if (spacecraftType == "SDO") {
    //        value = data["RSUN_OBS"];
    //        if (value.is_null()) {
    //            LERROR("SDO Metadata: RSUN_OBS missing!");
    //        }
    //        std::string sSunRadiusArcsec = value;
    //        value = data["CDELT1"];
    //        if (value.is_null()) {
    //            LERROR("SDO Metadata: CDELT1 missing!");
    //        }
    //        std::string sCdelt1 = value;
    //        const float cdelt1 = stof(sCdelt1);
    //        const float sunRadiusArcsec = stof(sSunRadiusArcsec);
    //        sunRadiusPixels = sunRadiusArcsec / cdelt1;
    //        im.isCoronaGraph = false;
    //    } else {
    //        // STEREO has RSUN specified in arcsecs - need to divide by factor
    //        std::string sCdelt1 = data["CDELT1"];
    //        const float cdelt1 = stof(sCdelt1);
    //        std::string sSunRadiusArcsec = data["RSUN"];
    //        const float sunRadiusArcsec = stof(sSunRadiusArcsec);
    //        sunRadiusPixels = sunRadiusArcsec / cdelt1;

    //        value = data["DETECTOR"];
    //        if (value.is_null()) {
    //            LERROR("No observer specified in Stereo");
    //        }

    //        std::string sObserver = value;
    //        if (sObserver == "COR1" || sObserver == "COR2") {
    //            im.isCoronaGraph = true;
    //        } else {
    //            im.isCoronaGraph = false;
    //        }
    //    }
    //    float scale = sunRadiusPixels / (im.fullResolution / 2.f);
    //    im.scale = scale;
    //}

    //const std::string sCenterPixelX = data["CRPIX1"];
    //const std::string sCenterPixelY = data["CRPIX2"];

    //const float centerPixelX = stof(sCenterPixelX);
    //const float centerPixelY = stof(sCenterPixelY);
    //const float halfRes = im.fullResolution / 2.f;

    //const float offsetX = ((halfRes - centerPixelX) / halfRes) * SUN_RADIUS;
    //const float offsetY = ((halfRes - centerPixelY) / halfRes) * SUN_RADIUS;

    //im.centerPixel = glm::vec2(offsetX, offsetY);

    return im;
}

void SpacecraftImageryManager::loadImageMetadata(const std::filesystem::path& rootDir,
      std::unordered_map<std::string, Timeline<ImageMetadata>>& _imageMetadataMap)
{
    if (!std::filesystem::is_directory(rootDir)) {
        throw ghoul::RuntimeError(std::format(
            "Could not load directory '{}'", rootDir
        ));
    }

    LDEBUG("Begin loading spacecraft imagery metadata");
    std::unordered_map<std::string, Timeline<ImageMetadata>> result;
    // Pre-processed data
    if (loadMetadataFromDisk(rootDir, result)) {
        _imageMetadataMap.insert(result.begin(), result.end());
        return;
    }

    LDEBUG("Loading sequence directory");
    std::vector<std::filesystem::path> sequencePaths = ghoul::filesystem::walkDirectory(
        rootDir,
        ghoul::filesystem::Recursive::Yes,
        ghoul::filesystem::Sorted::Yes
    );

    LDEBUG("Filtering data values");
    sequencePaths.erase(
        std::remove_if(
            sequencePaths.begin(),
            sequencePaths.end(),
            [](const std::filesystem::path& path) {
                const std::string& ext = path.extension().string();
                return (ext != ".jp2") && (ext != ".j2k");
            }
        ),
        sequencePaths.end()
    );

    // TODO anden 2026-02-04
    // Steps to validate cache:
    // 1. Check that all files in the cache still exists
    // 2. Remove any entry from cache that doesn't have a valid path
    // 3. Read new files:
    // 3.5 Filter away any file already in the cache
    // 4. Add new files to cache


    // TODO anden 2026-02-04
    // Steps for streaming new image data
    // 1. Check if image exists in cache
    // 2. If not -> spawn a thread to download it
    // 3. once downloaded put it through the normal pipeline of storing the file in
    // correct folder.
    // 4. Add the image data to the cache (file and in memory)


    LDEBUG("Reading metadata");
    size_t count = 0;

    std::mutex spiceAndPushMutex;

    std::vector<std::future<void>> futures;
    futures.reserve(sequencePaths.size());

    std::cout << '\n';
    auto exec = [&](const std::filesystem::path& seqPath) {
        // An example image has the following naming scheme:
        // 2024_05_08__00_58_23_814__SDO_AIA-211.jp2
        std::string fileName = seqPath.stem().string();
        size_t posSatelliteInfoStart = fileName.rfind("__") + 2;
        std::string satelliteInfo = fileName.substr(posSatelliteInfoStart); // e.g., SDO_AIA-211

        // Name
        size_t posSatelliteNameEnd = satelliteInfo.find_first_of("_");
        //std::string satelliteName = satelliteInfo.substr(0, posSatelliteNameEnd); // e.g., SDO

        // Instrument
        size_t posInstrumentNameStart = posSatelliteNameEnd + 1;
        std::string instrumentName = satelliteInfo.substr(posInstrumentNameStart); // e.g., AIA-211

        int year, month, day, hour, minute, second, millisecond;

        int scanned = std::sscanf(fileName.c_str(), "%d_%d_%d__%d_%d_%d_%d",
            &year, &month, &day, &hour, &minute, &second, &millisecond
        );

        if (scanned == 7) {
            std::string dateTime = std::format(
                "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}",
                year, month, day,
                hour, minute, second, millisecond
            );

            ImageMetadata im = parseJ2kMetadata(seqPath);
            spiceAndPushMutex.lock();
            result[instrumentName].addKeyframe(
                global::timeManager->time().convertTime(dateTime),
                std::move(im)
            );
            spiceAndPushMutex.unlock();
        }
        else {
            LERROR(std::format("Failed to parse date '{}' from file '{}'",
                fileName, seqPath
            ));
        }
        ++count;

        if (count % 250 == 0) {
            LINFO(std::format(
                "Processing image {} out of {} ", count, sequencePaths.size()
            ));
        }
    };

    for (const std::filesystem::path& seqPath : sequencePaths) {
        exec(seqPath);
    }

    LDEBUG("Finish loading imagery metadata");
    LDEBUG("Saving imagery metadata");
    saveMetadataToDisk(rootDir, result);

    _imageMetadataMap.insert(result.begin(), result.end());
    LDEBUG(std::format("{} images loaded", count));
    LDEBUG(std::format("{} values in metamap", _imageMetadataMap.size()));
}

} //namespace openspace
