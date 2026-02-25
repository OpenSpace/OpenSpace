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
#include <ghoul/filesystem/cachemanager.h>
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
    using namespace openspace;

    constexpr std::string_view _loggerCat = "SolarBrowsingHelper";
    constexpr double SunRadius = 1391600000.0 * 0.5;
    using IsValidCacheFile = bool;

    bool isValidJ2000ImageFile(const std::filesystem::path& path) {
        if (!std::filesystem::is_regular_file(path)) {
            return false;
        }
        const std::string& ext = path.extension().string();
        return (ext == ".jp2") || (ext == ".j2k");
    }

    // @TODO emiax: If openjpeg ever starts supporting reading XML metadata,
    // this implementation should be improved in order not to search the entire buffer for
    // XML data. There is an issue here:
    // (https://github.com/uclouvain/openjpeg/issues/929)
    std::optional<ImageMetadata> parseJ2kMetadata(
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

        std::optional<std::string_view> telescop = extractInnerXml(*metaData, "TELESCOP");

        if (!telescop.has_value()) {
            LERROR(std::format("Could not find TELESCOP tag {}", filePath));
            return std::nullopt;
        }

        std::optional<std::string_view> naxis = extractInnerXml(*metaData, "NAXIS1");

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

        im.fullResolution = std::stoi(std::string(*naxis));
        const float halfRes = im.fullResolution / 2.f;

        glm::vec2 centerPixel = glm::vec2(
            std::stof(std::string(*centerPixelX)),
            std::stof(std::string(*centerPixelY))
        );
        const glm::vec2 offset =
            ((halfRes - centerPixel) / halfRes) * glm::vec2(SunRadius);
        im.centerPixel = offset;

        if (*telescop == "SOHO") {
            std::optional<std::string_view> plateScl = extractInnerXml(
                *metaData,
                "PLATESCL"
            );

            if (!plateScl.has_value()) {
                LERROR(std::format("Could not find NAXIS1 tag {}", filePath));
                return std::nullopt;
            }

            const float plateScale = std::stof(std::string(*plateScl));
            im.scale = 1.f / (plateScale / 2.f);
            im.isCoronaGraph = true;
        }
        else if (*telescop == "SDO") {
            std::optional<std::string_view> rsunObs = extractInnerXml(
                bufferView,
                "RSUN_OBS"
            );
            std::optional<std::string_view> cDelt1 = extractInnerXml(
                bufferView,
                "CDELT1"
            );

            if (!rsunObs.has_value()) {
                LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
                return std::nullopt;
            }
            if (!cDelt1.has_value()) {
                LERROR(std::format("Could not find CDELT1 tag {}", filePath));
                return std::nullopt;
            }

            const float rSunObsValue = std::stof(std::string(*rsunObs));
            const float cDelt1Value = std::stof(std::string(*cDelt1));
            im.scale = (rSunObsValue / cDelt1Value) / (im.fullResolution / 2.f);
            im.isCoronaGraph = false;
        }
        else if (*telescop == "STEREO") {
            std::optional<std::string_view> rsun = extractInnerXml(bufferView, "RSUN");
            std::optional<std::string_view> cDelt1 = extractInnerXml(
                bufferView,
                "CDELT1"
            );

            if (!rsun.has_value()) {
                LERROR(std::format("Could not find RSUN_OBS tag {}", filePath));
                return std::nullopt;
            }
            if (!cDelt1.has_value()) {
                LERROR(std::format("Could not find CDELT1 tag {}", filePath));
                return std::nullopt;
            }

            const float rSunvalue = std::stof(std::string(*rsun));
            const float cDelt1Value = std::stof(std::string(*cDelt1));
            im.scale = (rSunvalue / cDelt1Value) / (im.fullResolution / 2.f);
            im.isCoronaGraph = false;

            std::optional<std::string_view> detector = extractInnerXml(
                bufferView,
                "DETECTOR"
            );

            if (detector.has_value()) {
                im.isCoronaGraph = *detector == "COR1" || *detector == "COR2";
            }
            else {
                LWARNING(std::format("Could not find DETECTOR tag {}", filePath));
            }
        }
        else {
            LERROR(std::format(
                "Recieved unknown spacecraft image '{}'. Supported spacecrafts are {}, "
                "{}, {}", *telescop, "SOHO", "SDO", "STEREO"
            ));
            return std::nullopt;
        }
        return im;
    }

    // Conversion needed before passing dates into the spice manager
    std::string ISO8601(std::string& datetime) {
        std::string month = datetime.substr(5, 3);

        std::string MM = "";
        if (month == "JAN") { MM = "01"; }
        else if (month == "FEB") { MM = "02"; }
        else if (month == "MAR") { MM = "03"; }
        else if (month == "APR") { MM = "04"; }
        else if (month == "MAY") { MM = "05"; }
        else if (month == "JUN") { MM = "06"; }
        else if (month == "JUL") { MM = "07"; }
        else if (month == "AUG") { MM = "08"; }
        else if (month == "SEP") { MM = "09"; }
        else if (month == "OCT") { MM = "10"; }
        else if (month == "NOV") { MM = "11"; }
        else if (month == "DEC") { MM = "12"; }
        else { ghoul_assert(false, "Bad month") };

        datetime.replace(4, 5, "-" + MM + "-");
        return datetime;
    }

    /**
     * Loads image metadata from cache files located in the immediate subdirectories of
     * \p rootDir.
     *
     * Any valid cache file found in the directory is parsed and its metadata is inserted
     * into \p imageMetadataMap. Cache files are validated to ensure that they are
     * consistent with the current contents of their corresponding image directory. If
     * validation fails, the directory is marked for reprocessing.
     *
     * The returned map indicates, for each discovered subdirectory, whether a
     * corresponding cache file was considered valid and successfully loaded.
     *
     * \param rootDir The root directory containing image sequence subdirectories and
     *        optional cache files
     * \param imageMetadataMap The metadata map that will be populated with metadata from
     *        valid cache files
     *
     * \return A map from subdirectory path to a boolean indicating whether the cache for
     *         that directory was valid
     */
    std::unordered_map<std::filesystem::path, IsValidCacheFile> loadMetadataFromDisk(
                                                     const std::filesystem::path& rootDir,
                                                       ImageMetadataMap& imageMetadataMap)
    {
        if (!std::filesystem::is_directory(rootDir)) {
            throw ghoul::RuntimeError(std::format(
                "Could not load directory '{}'", rootDir
            ));
        }

        std::vector<std::filesystem::path> subdirectories =
            ghoul::filesystem::walkDirectory(
                rootDir,
                ghoul::filesystem::Recursive::No,
                ghoul::filesystem::Sorted::No,
                [](const std::filesystem::path& path) {
                    return std::filesystem::is_directory(path);
                }
        );

        std::unordered_map<std::filesystem::path, IsValidCacheFile> subDirectoriesMap;
        for (const std::filesystem::path& path : subdirectories) {
            // Assume all cache files are invalid from the beginning
            subDirectoriesMap[path] = false;
        }

        std::vector<std::filesystem::path> cacheFiles = ghoul::filesystem::walkDirectory(
            rootDir,
            ghoul::filesystem::Recursive::No,
            ghoul::filesystem::Sorted::No,
            [](const std::filesystem::path& path) {
                const std::string extension = path.extension().string();
                const std::string base = path.filename().string();
                const size_t pos = base.find("_cached");
                const bool isCacheFile = extension == ".txt" && pos != std::string::npos;
                return isCacheFile;
            }
        );

        for (const std::filesystem::path& cacheFile : cacheFiles) {
            const std::string base = cacheFile.filename().string();
            const size_t separator = base.rfind("_");
            const std::string instrument = base.substr(0, separator);
            LDEBUG(std::format("Loading instrument: {}", instrument));

            std::ifstream myfile(cacheFile);
            if (!myfile.is_open()) {
                LERROR(std::format("Failed to open metadata file '{}'", cacheFile));
                continue;
            }

            int numStates;
            myfile >> numStates;

            std::string subDir;
            myfile >> subDir;
            std::filesystem::path subDirectory = rootDir / subDir;

            if (!std::filesystem::is_directory(subDirectory)) {
                LWARNING(std::format(
                    "Could not find subdirectory '{}' for cache file '{}'",
                    subDirectory, cacheFile));
                continue;
            }

            // Early check if the number of files in the subdirectoy match what was stored
            // in cache, however, this does not guarantee that the files are the same
            const bool cacheHasCorrectNFiles = ghoul::filesystem::walkDirectory(
                subDirectory,
                ghoul::filesystem::Recursive::No,
                ghoul::filesystem::Sorted::No,
                isValidJ2000ImageFile
            ).size() == numStates;

            if (!cacheHasCorrectNFiles) {
                subDirectoriesMap[subDirectory] = false;
                continue;
            }

            for (int i = 0; i < numStates; i++) {
                ImageMetadata im;

                myfile >> std::ws; // Skip the rest of the line
                std::string date;
                std::getline(myfile, date);

                if (date.empty()) {
                    LERROR(std::format(
                        "Failed to read metadata state: date, file: '{}'", cacheFile
                    ));
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                double timeObserved =
                    SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

                std::string relPath;
                myfile >> relPath;

                if (myfile.bad()) {
                    LERROR(std::format(
                        "Failed to read metadata state: relPath, file: '{}'", cacheFile
                    ));
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                im.filePath = rootDir / relPath;

                // Check that the filePath still exists
                if (!std::filesystem::is_regular_file(im.filePath)) {
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                myfile >> im.fullResolution;

                if (myfile.bad()) {
                    LERROR(std::format(
                        "Failed to read metadata state: fullResolution, file: '{}'",
                        cacheFile
                    ));
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                myfile >> im.scale;

                if (myfile.bad()) {
                    LERROR(std::format(
                        "Failed to read metadata state: scale, file: '{}'", cacheFile
                    ));
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                float x, y;
                myfile >> x >> y;
                im.centerPixel = glm::vec2(x, y);
                myfile >> im.isCoronaGraph;

                if (myfile.bad()) {
                    LERROR(std::format(
                        "Failed to read metadata state : isCoronaGraph, file : '{}'",
                        cacheFile
                    ));
                    subDirectoriesMap[subDirectory] = false;
                    break;
                }

                imageMetadataMap[instrument].addKeyframe(timeObserved, std::move(im));
            }
            myfile.close();
            // All files in cache exists and there were no additional files in the
            // subdirectory, cache is assumed to be up-to-date
            subDirectoriesMap[subDirectory] = true;
        }

        return subDirectoriesMap;
    }

    void saveMetadataToDisk(const std::filesystem::path& rootPath,
                            const ImageMetadataMap& imageMetadataMap)
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

            bool isFirstWrite = true;

            for (const Keyframe<ImageMetadata>& metadata : sequence.keyframes()) {
                const std::string date = SpiceManager::ref().dateFromEphemerisTime(
                        metadata.timestamp
                );
                const ImageMetadata& im = metadata.data;
                const std::filesystem::path relativePath = std::filesystem::relative(
                    im.filePath,
                    rootPath
                );

                if (isFirstWrite) {
                    std::filesystem::path relativeDirectory = relativePath.parent_path();
                    ofs << relativeDirectory.generic_string() << '\n';
                    isFirstWrite = false;
                }

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
        }
    }
} // namespace

namespace openspace {

std::unordered_map<std::string, std::shared_ptr<TransferFunction>> loadTransferFunctions(
                                                     const std::filesystem::path& rootDir,
                                                 const ImageMetadataMap& imageMetadataMap)
{
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>> tfMap;

    if (!std::filesystem::is_directory(rootDir)) {
        throw ghoul::RuntimeError(std::format("Could not load directory '{}'", rootDir));
    }

    std::vector<std::filesystem::path> subdirectories = ghoul::filesystem::walkDirectory(
        rootDir,
        ghoul::filesystem::Recursive::No,
        ghoul::filesystem::Sorted::No,
        [](const std::filesystem::path& path) {
            return std::filesystem::is_directory(path);
        }
    );

    using T = Timeline<ImageMetadata>;
    for (const std::pair<InstrumentName, T>& instrument : imageMetadataMap) {
        // The subdirectories might have a different name than the instrument name so we
        // have to search the directories for the correct texture map
        bool found = false;
        for (const std::filesystem::path& subdirectory : subdirectories) {
            const std::filesystem::path texturePath =
                subdirectory / std::format("{}.txt", instrument.first);

            if (std::filesystem::is_regular_file(texturePath)) {
                tfMap[instrument.first] = std::make_shared<TransferFunction>(texturePath);
                found = true;
                break;
            }
        }

        if (!found) {
            LERROR(std::format("Unable to load a color map for instrument '{}'",
                instrument.first
            ));
        }
    }

    return tfMap;
}

ImageMetadataMap loadImageMetadata(const std::filesystem::path& rootDir) {
    if (!std::filesystem::is_directory(rootDir)) {
        throw ghoul::RuntimeError(std::format("Could not load directory '{}'", rootDir));
    }

    LDEBUG("Begin loading spacecraft imagery metadata");
    ImageMetadataMap result;

    // Load pre-processed data from any cache file that might exist
    std::unordered_map<std::filesystem::path, IsValidCacheFile> cacheFilesValidityMap =
        loadMetadataFromDisk(rootDir, result);

    // There might be cache files that are outdated due to new images or if a new
    // directory of images have been added. Remove the cache files that were validated
    std::erase_if(
        cacheFilesValidityMap,
        [](const std::pair<std::filesystem::path, bool>& entry) {
            return entry.second;
        }
    );

    if (cacheFilesValidityMap.empty()) {
        // All cache files are ok, no more files to load.
        return result;
    }

    std::vector<std::filesystem::path> sequencePaths;

    // Get all files that were not correctly processed from cache file
    for (const auto& [directory, isValidCacheFile] : cacheFilesValidityMap) {
        LDEBUG(std::format("Loading sequence directory '{}'", directory));
        std::vector<std::filesystem::path> files = ghoul::filesystem::walkDirectory(
            directory,
            ghoul::filesystem::Recursive::No,
            ghoul::filesystem::Sorted::Yes
        );

        sequencePaths.reserve(sequencePaths.size() + files.size());
        for (std::filesystem::path& file : files) {
            sequencePaths.push_back(std::move(file));
        }
    }

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

    LDEBUG("Processing metadata");
    std::atomic<size_t> count = 0;
    std::mutex spiceAndPushMutex;
    std::mutex onProgressMutex;
    const size_t totalImages = sequencePaths.size();

    ProgressBar progressBar = ProgressBar(100);
    auto onProgress = [&progressBar](float progress) {
        progressBar.print(static_cast<int>(progress * 100.f));
    };

    auto processImageMetadata = [&](const std::filesystem::path& seqPath) {
        // An example image has the following naming scheme:
        // 2024_05_08__00_58_23_814__SDO_AIA-211.jp2
        std::string fileName = seqPath.stem().string();

        // Satellite
        size_t posSatelliteInfoStart = fileName.rfind("__") + 2;
        // e.g., SDO_AIA-211
        std::string satelliteInfo = fileName.substr(posSatelliteInfoStart);

        // Name
        size_t posSatelliteNameEnd = satelliteInfo.find_first_of("_");

        // Instrument
        size_t posInstrumentNameStart = posSatelliteNameEnd + 1;
        // e.g., AIA-211
        std::string instrumentName = satelliteInfo.substr(posInstrumentNameStart);

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
                    std::move(*im)
                );
            }
            else {
                LERROR(std::format(
                    "Failed to parse J2K metadata from file '{}'", seqPath
                ));
            }
        }
        else {
            LERROR(std::format(
                "Failed to parse date '{}' from file '{}'", fileName, seqPath
            ));
        }

        size_t done = ++count;
        if (done % 20 == 0) { // Reduce spam on the callback
            std::lock_guard lock(onProgressMutex);
            onProgress(done / static_cast<float>(totalImages));
        }
    };

    std::for_each(
        std::execution::par,
        sequencePaths.begin(),
        sequencePaths.end(),
        processImageMetadata
    );

    progressBar.finish();
    LDEBUG("Finish loading imagery metadata");
    LDEBUG("Saving imagery metadata");
    saveMetadataToDisk(rootDir, result);
    LDEBUG(std::format("{} images loaded", static_cast<size_t>(count)));
    return result;
}

DecodedImageData loadDecodedDataFromCache(const std::filesystem::path& path,
                                          const ImageMetadata& metadata,
                                          unsigned int imageSize)
{
    std::ifstream file = std::ifstream(path, std::ifstream::binary);
    if (!file.good()) {
        FileSys.cacheManager()->removeCacheFile(
            metadata.filePath,
            std::format("{}x{}", imageSize, imageSize)
        );
        throw ghoul::RuntimeError(std::format(
            "Error, could not open cache file '{}'", path
        ));
    }

    size_t nEntries = 0;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(nEntries));
    DecodedImageData data;
    data.imageSize = imageSize;
    data.metadata = metadata;
    data.buffer.resize(nEntries);
    file.read(reinterpret_cast<char*>(data.buffer.data()), nEntries * sizeof(uint8_t));

    if (!file) {
        file.close();
        FileSys.cacheManager()->removeCacheFile(
            metadata.filePath,
            std::format("{}x{}", imageSize, imageSize)
        );
        throw ghoul::RuntimeError(std::format(
            "Failed to read image data from cache '{}'", path
        ));
    }

    return data;
}

void saveDecodedDataToCache(const std::filesystem::path& path,
                            const DecodedImageData& data, bool verboseMode)
{
    if (verboseMode) {
        LINFO(std::format("Saving cache '{}'", path));
    }
    std::ofstream file = std::ofstream(path, std::ofstream::binary);
    size_t nEntries = data.buffer.size();
    file.write(reinterpret_cast<const char*>(&nEntries), sizeof(nEntries));
    file.write(
        reinterpret_cast<const char*>(data.buffer.data()),
        nEntries * sizeof(uint8_t)
    );
    file.close();
}

} // namespace openspace
