/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/solarbrowsing/util/solarbrowsinghelper.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/progressbar.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <scn/scan.h>
#include <execution>
#include <format>
#include <fstream>
#include <string>
#include <string_view>
#include <iostream>


namespace {
    constexpr std::string_view _loggerCat = "SolarBrowsingHelper";
    constexpr double SUN_RADIUS = 1391600000.0 * 0.5;

    // @TODO emiax: If openjpeg ever starts supporting reading XML metadata,
    // this implementation should be improved in order not to search the entire buffer for
    // XML data. There is an issue here:
    // (https://github.com/uclouvain/openjpeg/issues/929)
    std::optional<openspace::ImageMetadata> parseJ2kMetadata(
        const std::filesystem::path& filePath)
    {
        openspace::ImageMetadata im;
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

        auto extractInnerXml =
            [](std::string_view view, const std::string& elementName) ->
                                                           std::optional<std::string_view>
        {
            const std::string startTag = std::format("<{}>", elementName);
            const std::string endTag = std::format("</{}>", elementName);

            const auto begin = std::search(
                view.begin(),
                view.end(),
                startTag.begin(),
                startTag.end()
            );

            if (begin == view.end()) {
                return std::nullopt;
                ;
            }

            const auto afterBeginTag = begin + startTag.size();

            const auto end = std::search(
                afterBeginTag,
                view.end(),
                endTag.begin(),
                endTag.end()
            );

            if (end == view.end()) {
                return std::nullopt;
            }

            return std::string_view(&*afterBeginTag, end - afterBeginTag);
        };

        std::optional<std::string_view> metaData = extractInnerXml(bufferView, "meta");

        if (!metaData.has_value()) {
            LERROR(std::format("Could not find metadata in {}", filePath));
            return std::nullopt;
        }

        std::optional<std::string_view> telescop = extractInnerXml(
            metaData.value(),
            "TELESCOP"
        );

        if (!telescop.has_value()) {
            LERROR(std::format("Could not find TELESCOP tag {}", filePath));
            return std::nullopt;
        }

        std::optional<std::string_view> naxis = extractInnerXml(
            metaData.value(),
            "NAXIS1"
        );

        if (!naxis.has_value()) {
            LERROR(std::format("Could not find NAXIS1 tag {}", filePath));
            return std::nullopt;
        }

        std::optional<std::string_view> centerPixelX = extractInnerXml(
            bufferView,
            "CRPIX1"
        );

        if (!centerPixelX.has_value()) {
            LERROR(std::format("Could not find CRPIX1 tag {}", filePath));
            return std::nullopt;
        }

        std::optional<std::string_view> centerPixelY = extractInnerXml(
            bufferView,
            "CRPIX2"
        );

        if (!centerPixelY.has_value()) {
            LERROR(std::format("Could not find CRPIX2 tag {}", filePath));
            return std::nullopt;
        }

        im.fullResolution = std::stoi(std::string(naxis.value()));
        const float halfRes = im.fullResolution / 2.f;

        glm::vec2 centerPixel;
        centerPixel.x = std::stof(std::string(centerPixelX.value()));
        centerPixel.y = std::stof(std::string(centerPixelY.value()));
        const glm::vec2 offset = ((halfRes - centerPixel) / halfRes) * glm::vec2(SUN_RADIUS);
        im.centerPixel = offset;

        if (telescop.value() == "SOHO") {
            std::optional<std::string_view> plateScl = extractInnerXml(
                metaData.value(),
                "PLATESCL"
            );

            if (!plateScl.has_value()) {
                LERROR(std::format("Could not find NAXIS1 tag {}", filePath));
                return std::nullopt;
            }

            const float plateScale = std::stof(std::string(plateScl.value()));
            im.scale = 1.f / (plateScale / 2.f);
            im.isCoronaGraph = true;
        }
        else if (telescop.value() == "SDO") {
            std::optional<std::string_view> rsunObs = extractInnerXml(bufferView, "RSUN_OBS");
            std::optional<std::string_view> cDelt1 = extractInnerXml(bufferView, "CDELT1");

            if (!rsunObs.has_value()) {
                LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
                return std::nullopt;
            }
            if (!cDelt1.has_value()) {
                LERROR(std::format("Could not find CDELT1 tag {}", filePath));
                return std::nullopt;
            }

            const float rSunObsValue = std::stof(std::string(rsunObs.value()));
            const float cDelt1Value = std::stof(std::string(cDelt1.value()));
            im.scale = (rSunObsValue / cDelt1Value) / (im.fullResolution / 2.f);
            im.isCoronaGraph = false;
        }
        else { // Telescope is assumed to be STEREO
            std::optional<std::string_view> rsun = extractInnerXml(bufferView, "RSUN");
            std::optional<std::string_view> cDelt1 = extractInnerXml(bufferView, "CDELT1");

            if (!rsun.has_value()) {
                LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
                return std::nullopt;
            }
            if (!cDelt1.has_value()) {
                LERROR(std::format("Could not find CDELT1 tag {}", filePath));
                return std::nullopt;
            }

            const float rSunvalue = std::stof(std::string(rsun.value()));
            const float cDelt1Value = std::stof(std::string(cDelt1.value()));
            im.scale = (rSunvalue / cDelt1Value) / (im.fullResolution / 2.f);
            im.isCoronaGraph = false;

            std::optional<std::string_view> detector = extractInnerXml(bufferView, "DETECTOR");

            if (detector.has_value()) {
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

    // Conversion needed before passing dates into the spice manager
    std::string ISO8601(std::string& datetime) {
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

    bool loadMetadataFromDisk(const std::filesystem::path& rootDir,
        openspace::ImageMetadataMap& imageMetadataMap)
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

        size_t readCachedFiles = 0;
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
                openspace::ImageMetadata im;

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
                    openspace::SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

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
                im.centerPixel = glm::vec2(x, y);
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
            readCachedFiles++;
        }

        // Currently assumes each cache file is correct @TODO (anden88 2026-02-13) make sure
        // it actually is. This catches any new folder added - However, it does not catch if
        // one removes a folder and add a new one.
        bool readAllCacheFiles = sequencePaths.size() == readCachedFiles * 2;
        return readAllCacheFiles;
    }

    void saveMetadataToDisk(const std::filesystem::path& rootPath,
        const openspace::ImageMetadataMap& imageMetadataMap)
    {
        for (const auto& [instrument, sequence] : imageMetadataMap) {
            const std::filesystem::path cacheFile =
                rootPath / std::format("{}_cached.txt", instrument);

            std::ofstream ofs(cacheFile);
            if (!ofs.is_open()) {
                LERROR(std::format("Failed to open file '{}'", cacheFile));
                continue;
            }

            ofs << sequence.nKeyframes() << '\n';

            for (const openspace::Keyframe<openspace::ImageMetadata>& metadata : sequence.keyframes()) {
                const std::string date = openspace::SpiceManager::ref().dateFromEphemerisTime(
                    metadata.timestamp
                );

                const openspace::ImageMetadata& im = metadata.data;
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
} // namespace


namespace openspace::solarbrowsing {
void loadTransferFunctions(const std::filesystem::path& dir,
                std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& tfMap)
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
            tfMap[key] = std::make_shared<TransferFunction>(seqPath);
        }
    }
}

void loadImageMetadata(const std::filesystem::path& rootDir,
                       ImageMetadataMap& imageMetadataMap)
{
    if (!std::filesystem::is_directory(rootDir)) {
        throw ghoul::RuntimeError(std::format(
            "Could not load directory '{}'", rootDir
        ));
    }

    LDEBUG("Begin loading spacecraft imagery metadata");
    ImageMetadataMap result;
    // Pre-processed data
    if (loadMetadataFromDisk(rootDir, result)) {
        imageMetadataMap.insert(result.begin(), result.end());
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

    // @TODO (anden88 2026-02-04):
    // Steps to validate cache:
    // 1. Check that all files in the cache still exists
    // 2. Remove any entry from cache that doesn't have a valid path
    // 3. Read new files:
    // 3.5 Filter away any file already in the cache
    // 4. Add new files to cache


    // @TODO (anden88 2026-02-04):
    // Steps for streaming new image data
    // 1. Check if image exists in cache
    // 2. If not -> spawn a thread to download it
    // 3. once downloaded put it through the normal pipeline of storing the file in
    // correct folder.
    // 4. Add the image data to the cache (file and in memory)


    LDEBUG("Reading metadata");
    std::atomic<size_t> count = 0;
    std::mutex spiceAndPushMutex;
    std::mutex onProgressMutex;
    const size_t totalImages = sequencePaths.size();

    ProgressBar progressBar = ProgressBar(100);
    auto onProgress = [&progressBar](float progress) {
        progressBar.print(static_cast<int>(progress * 100.f));
    };

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

        auto r = scn::scan<int, int, int, int, int, int, int>(
            fileName,
            "{}_{}_{}__{}_{}_{}_{}"
        );
        ghoul_assert(r, "Invalid date");
        auto& [year, month, day, hour, minute, second, millisecond] = r->values();

        if (r) {
            std::string dateTime = std::format(
                "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}",
                year, month, day,
                hour, minute, second, millisecond
            );

            std::optional<ImageMetadata> im = parseJ2kMetadata(seqPath);

            if (im.has_value()) {
                std::lock_guard lock(spiceAndPushMutex);
                result[instrumentName].addKeyframe(
                    global::timeManager->time().convertTime(dateTime),
                    std::move(im.value())
                );
            }
            else {
                LERROR(std::format(
                    "Failed to parse J2K metadata from file '{}'",
                    seqPath
                ));
            }
        }
        else {
            LERROR(std::format("Failed to parse date '{}' from file '{}'",
                fileName, seqPath
            ));
        }

        size_t done = ++count;
        {
            std::lock_guard lock(onProgressMutex);
            onProgress(done / static_cast<float>(totalImages));
        }
    };

    std::for_each(
        std::execution::par,
        sequencePaths.begin(),
        sequencePaths.end(),
        exec
    );

    progressBar.finish();
    LDEBUG("Finish loading imagery metadata");
    LDEBUG("Saving imagery metadata");
    saveMetadataToDisk(rootDir, result);
    imageMetadataMap.insert(result.begin(), result.end());
    LDEBUG(std::format("{} images loaded", static_cast<size_t>(count)));
    LDEBUG(std::format("{} values in metamap", imageMetadataMap.size()));
}

} // namespace openspace::solarbrowsing
