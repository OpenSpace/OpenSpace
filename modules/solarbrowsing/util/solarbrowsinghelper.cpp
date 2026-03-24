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

#include <modules/solarbrowsing/solarbrowsingmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/distanceconstants.h>
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
    using IsValidCacheFile = bool;

    bool isValidJ2000ImageFile(const std::filesystem::path& path) {
        if (!std::filesystem::is_regular_file(path)) {
            return false;
        }
        const std::string& ext = path.extension().string();
        return (ext == ".jp2") || (ext == ".j2k");
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
        else { ghoul_assert(false, "Bad month"); };

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
                    subDirectory, cacheFile
                ));
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
                        "Failed to read metadata state: isCoronaGraph, file: '{}'",
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
    for (const std::pair<const InstrumentName, T>& instrument : imageMetadataMap) {
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
            LERROR(std::format(
                "Unable to find and load a color map for instrument '{}'",
                instrument.first
            ));
        }
    }

    return tfMap;
}


// @TODO emiax: If openjpeg ever starts supporting reading XML metadata,
    // this implementation should be improved in order not to search the entire buffer for
    // XML data. There is an issue here:
    // (https://github.com/uclouvain/openjpeg/issues/929)
std::optional<ImageMetadata> parseJ2kMetadata(const std::filesystem::path& filePath)
{
    ImageMetadata im;
    im.filePath = filePath;

    std::ifstream stream(filePath, std::ios::binary | std::ios::ate);
    if (!stream) {
        LERROR(std::format("Failed to open '{}' for metadata parsing", filePath));
        return std::nullopt;
    }

    std::streamsize size = stream.tellg();
    if (size <= 0) {
        LERROR(std::format(
            "Failed to determine metadata file size for '{}'",
            filePath
        ));
        return std::nullopt;
    }

    stream.seekg(0, std::ios::beg);
    std::vector<char> buffer(size);
    if (!stream.read(buffer.data(), size)) {
        LERROR(std::format("Failed to read data from '{}' ", filePath));
        return std::nullopt;
    }
    std::string_view bufferView(buffer.data(), size);

    auto extractInnerXml =
        [](std::string_view view, const std::string& elementName)
        -> std::optional<std::string_view>
        {
            // Find "<TAG" (not "<TAG>")
            const std::string startTagPrefix = std::format("<{}", elementName);
            const std::string endTag = std::format("</{}>", elementName);

            auto begin = std::search(
                view.begin(), view.end(),
                startTagPrefix.begin(), startTagPrefix.end()
            );
            if (begin == view.end()) {
                return std::nullopt;
            }

            // Ensure we matched a tag name boundary:
            // next char must be '>' or whitespace or '/' (just in case)
            auto afterName = begin + startTagPrefix.size();
            if (afterName == view.end()) return std::nullopt;

            char c = *afterName;
            if (!(c == '>' || std::isspace(static_cast<unsigned char>(c)) || c == '/')) {
                return std::nullopt;
            }

            // Find the end of the opening tag '>'
            auto openEnd = std::find(afterName, view.end(), '>');
            if (openEnd == view.end()) {
                return std::nullopt;
            }

            auto contentBegin = openEnd + 1;

            // Self-closing tag "<TAG .../>"
            if (openEnd != view.begin() && *(openEnd - 1) == '/') {
                return std::string_view{};
            }

            // Find "</TAG>"
            auto end = std::search(
                contentBegin, view.end(),
                endTag.begin(), endTag.end()
            );
            if (end == view.end()) {
                return std::nullopt;
            }

            return std::string_view(&*contentBegin, end - contentBegin);
        };

    auto iequals = [](std::string_view a, std::string_view b) {
        if (a.size() != b.size()) return false;
        for (size_t i = 0; i < a.size(); ++i) {
            const char ca = static_cast<char>(std::tolower(static_cast<unsigned char>(a[i])));
            const char cb = static_cast<char>(std::tolower(static_cast<unsigned char>(b[i])));
            if (ca != cb) return false;
        }
        return true;
        };

    auto istarts_with = [&](std::string_view s, std::string_view prefix) {
        return s.size() >= prefix.size() && iequals(s.substr(0, prefix.size()), prefix);
        };

    auto getTag = [&](std::string_view primary, std::string_view secondary,
        const std::string& name) -> std::optional<std::string_view>
        {
            if (auto v = extractInnerXml(primary, name); v.has_value()) return v;
            return extractInnerXml(secondary, name);
        };

    auto getFloatTag = [&](std::string_view primary, std::string_view secondary,
        const std::string& name) -> std::optional<float>
        {
            auto v = getTag(primary, secondary, name);
            if (!v.has_value()) return std::nullopt;
            try {
                return std::stof(std::string(*v));
            }
            catch (...) {
                return std::nullopt;
            }
        };

    // Interpret CDELT1 with CUNIT1 (arcsec/deg/rad) and return arcsec-per-pixel.
    // If CUNIT1 is missing, assume arcsec (common in solar FITS WCS).
    auto arcsecPerPixel = [&](std::string_view primary, std::string_view secondary)
        -> std::optional<float>
        {
            const auto cdelt1 = getFloatTag(primary, secondary, "CDELT1");
            if (!cdelt1.has_value()) return std::nullopt;

            const auto cunit1sv = getTag(primary, secondary, "CUNIT1");
            if (!cunit1sv.has_value()) {
                return *cdelt1; // assume arcsec / pixel
            }

            const std::string unit = std::string(*cunit1sv);
            if (unit == "arcsec" || unit == "ARCSEC") return *cdelt1;
            if (unit == "deg" || unit == "DEG") return (*cdelt1) * 3600.0f;
            if (unit == "rad" || unit == "RAD") return (*cdelt1) * (180.0f / 3.14159265358979323846f) * 3600.0f;

            // Unknown unit, fall back (better than failing hard)
            return *cdelt1;
        };

    // Derive apparent solar radius (arcsec) from DSUN_OBS (meters) if RSUN_OBS is absent.
    // Uses solar radius 695700 km.
    auto rsunObsArcsec = [&](std::string_view primary, std::string_view secondary)
        -> std::optional<float>
        {
            if (auto rs = getFloatTag(primary, secondary, "RSUN_OBS"); rs.has_value()) {
                return *rs; // already angular radius in arcsec in many solar FITS headers
            }

            const auto dsun = getFloatTag(primary, secondary, "DSUN_OBS");
            if (!dsun.has_value()) return std::nullopt;

            // DSUN_OBS is "distance to the center of sun from the observation platform" (SUVI). :contentReference[oaicite:3]{index=3}
            // Assume meters. If your files are in km, adjust here.
            constexpr double R_SUN_M = 695700000.0; // 695700 km
            const double D_M = static_cast<double>(*dsun);

            // Angular radius (radians) ≈ asin(R/D)
            const double angRad = std::asin(std::clamp(R_SUN_M / D_M, 0.0, 1.0));
            const double angArcsec = angRad * (180.0 / 3.14159265358979323846) * 3600.0;
            return static_cast<float>(angArcsec);
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


    // Read both dimensions
    auto naxis1sv = extractInnerXml(*metaData, "NAXIS1");
    auto naxis2sv = extractInnerXml(*metaData, "NAXIS2");
    if (!naxis1sv || !naxis2sv) {
        LERROR(std::format("Could not find NAXIS1/NAXIS2 tag {}", filePath));
        return std::nullopt;
    }

    const int srcW = std::stoi(std::string(*naxis1sv));
    const int srcH = std::stoi(std::string(*naxis2sv));
    const int dstSize = std::max(srcW, srcH);

    // IMPORTANT: store padded size
    im.fullResolution = dstSize;

    const float dstHalf = dstSize / 2.f;
    const float padLeft = (dstSize - srcW) / 2.f;
    const float padTop = (dstSize - srcH) / 2.f;

    // Prefer derived solar center
    auto cxSv = extractInnerXml(*metaData, "EUXCEN");
    auto cySv = extractInnerXml(*metaData, "EUYCEN");

    // Fallback to WCS reference pixel
    if (!cxSv || !cySv) {
        cxSv = extractInnerXml(*metaData, "CRPIX1");
        cySv = extractInnerXml(*metaData, "CRPIX2");
    }

    if (!cxSv || !cySv) {
        LERROR(std::format("Could not find EUXCEN/EUYCEN nor CRPIX1/CRPIX2 in {}", filePath));
        return std::nullopt;
    }

    const float cxSrc = std::stof(std::string(*cxSv));
    const float cySrc = std::stof(std::string(*cySv));

    // Map into padded buffer coordinates
    const float cxDst = cxSrc + padLeft;
    const float cyDst = cySrc + padTop;

    // Compute offset in SunRadius units
    glm::vec2 centerPixel(cxDst, cyDst);
    glm::vec2 offset = ((dstHalf - centerPixel) / dstHalf) *
        glm::vec2(static_cast<float>(distanceconstants::SolarRadius));
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
    else if (istarts_with(*telescop, "SOLO/") || iequals(*telescop, "SOLAR ORBITER")) {
        // For EUI etc we can use RSUN_OBS + CDELT1 just like SDO
        auto rsunObs = extractInnerXml(bufferView, "RSUN_OBS");
        auto cdelt1 = extractInnerXml(bufferView, "CDELT1");
        if (!rsunObs.has_value() || !cdelt1.has_value()) {
            LERROR(std::format("Solar Orbiter: missing RSUN_OBS or CDELT1 in {}", filePath));
            return std::nullopt;
        }

        const float rSunObsValue = std::stof(std::string(*rsunObs)); // arcsec
        const float cDelt1Value = std::stof(std::string(*cdelt1));  // arcsec/pixel
        im.scale = (rSunObsValue / cDelt1Value) / (im.fullResolution / 2.f);
        im.isCoronaGraph = false;
    }
    else if (istarts_with(*telescop, "GOES") || iequals(*telescop, " CCOR-1")) {
        // GOES-R SUVI uses FITS WCS keywords like CRPIXn/CDELTn/CUNITn and also DSUN_OBS. :contentReference[oaicite:4]{index=4}

        const auto rsArcsec = rsunObsArcsec(bufferView, *metaData);
        const auto cdeltArcsec = arcsecPerPixel(bufferView, *metaData);

        if (!rsArcsec.has_value()) {
            LERROR(std::format("Could not find RSUN_OBS nor derive from DSUN_OBS in {}", filePath));
            return std::nullopt;
        }
        if (!cdeltArcsec.has_value()) {
            LERROR(std::format("Could not find CDELT1 (and/or parse CUNIT1) in {}", filePath));
            return std::nullopt;
        }

        // Same normalization you use for SDO/STEREO:
        im.scale = (*rsArcsec / *cdeltArcsec) / (im.fullResolution / 2.f);
        im.isCoronaGraph = false; // SUVI is an EUV imager (not a coronagraph)
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
        SolarBrowsingModule* module = global::moduleEngine->module<SolarBrowsingModule>();
        module->cacheManager()->removeCacheFile(
            path,
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
        SolarBrowsingModule* module = global::moduleEngine->module<SolarBrowsingModule>();
        file.close();
        module->cacheManager()->removeCacheFile(
            path,
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
