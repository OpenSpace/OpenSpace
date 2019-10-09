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
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/threadpool.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ext/json/json.hpp>
#include <chrono>
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

void SpacecraftImageryManager::loadTransferFunctions(const std::string& path,
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap)
{
    ghoul::filesystem::Directory sequenceDir(
        path,
        ghoul::filesystem::Directory::RawPath::Yes
    );

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load directory '{}'", sequenceDir.path()));
    }

    std::vector<std::string> sequencePaths = sequenceDir.read(
        ghoul::filesystem::Directory::Recursive::Yes,
        ghoul::filesystem::Directory::Sort::Yes
    );

    for (const std::string& seqPath : sequencePaths) {
        ghoul::filesystem::File currentFile(seqPath);
        if (currentFile.fileExtension() == "txt") {
            std::string key = currentFile.baseName();
            _tfMap[key] = std::make_shared<TransferFunction>(seqPath);
        }
    }
}

bool SpacecraftImageryManager::loadMetadataFromDisk(const std::string& rootPath,
           std::unordered_map<std::string, ImageMetadataStateSequence>& _imageMetadataMap)
{

    ghoul::filesystem::Directory sequenceDir(
        rootPath,
        ghoul::filesystem::Directory::RawPath::Yes
    );
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load directory '{}'", sequenceDir.path()));
    }

    bool metadataLoaded = false;

    std::vector<std::string> sequencePaths = sequenceDir.read(
        ghoul::filesystem::Directory::Recursive::No,
        ghoul::filesystem::Directory::Sort::No
    );

    for (const std::string& seqPath : sequencePaths) {
        ghoul::filesystem::File currentFile(seqPath);
        const std::string extension = currentFile.fileExtension();
        const std::string base = currentFile.baseName();
        const std::size_t foundCachedImageData = base.find("_cached");
        if (extension != "txt" || foundCachedImageData == std::string::npos) {
            continue;
        }
                    
        const std::string::size_type separator = base.rfind("_");
        const std::string instrument = base.substr(0, separator);
        LDEBUG(fmt::format("Loading instrument: {}", instrument));

        metadataLoaded = true;
        std::ifstream myfile(currentFile.path());
        if (!myfile.is_open()) {
            LERROR("Failed to open metadata file");
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
                LERROR("Failed to open state metadata: date");
                return false;
            }

            double timeObserved =
                SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

            std::string relPath;
            myfile >> relPath;

            if (myfile.bad()) {
                LERROR("Failed to open state metadata: relPath");
                return false;
            }

            im.filename = rootPath + "/" + relPath;

            myfile >> im.fullResolution;

            if (myfile.bad()) {
                LERROR("Failed to open state metadata: fullResolution");
                return false;
            }

            myfile >> im.scale;

            if (myfile.bad()) {
                LERROR("Failed to open state metadata: scale");
                return false;
            }

            float x, y;
            myfile >> x >> y;
            im.centerPixel = glm::vec2(x,y);
            myfile >> im.isCoronaGraph;

            if (myfile.bad()) {
                LERROR("Failed to open state metadata: isCoronaGraph");
                return false;
            }

            std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
            TimedependentState<ImageMetadata> timeState(
                std::move(data),
                timeObserved,
                im.filename
            );
            _imageMetadataMap[instrument].addState(std::move(timeState));
        }
        myfile.close();
    }
    return metadataLoaded;
}

void SpacecraftImageryManager::saveMetadataToDisk(const std::string& rootPath,
           std::unordered_map<std::string, ImageMetadataStateSequence>& _imageMetadataMap)
{
    using K = std::string;
    using V = ImageMetadataStateSequence;
    for (const std::pair<K, V>& instrument : _imageMetadataMap) {
        std::ofstream ofs(rootPath + "/" + instrument.first + "_cached" + ".txt");
        if (!ofs.is_open()) {
            LERROR("Failed to open file");
        }
        const ImageMetadataStateSequence& sequence = instrument.second;
        ofs << sequence.numStates() << "\n";
        for (const TimedependentState<ImageMetadata>& metadata : sequence.states()) {
                ofs << SpiceManager::ref().dateFromEphemerisTime(
                    metadata.timeObserved()
                ) << "\n";
                const std::shared_ptr<ImageMetadata>& im = metadata.contents();

                size_t filenamePos = im->filename.find("imagedata");
                if (filenamePos == std::string::npos) {
                    LERROR(fmt::format("Invalid image filename {}", im->filename));
                    return;
                }
                std::string fname = im->filename.substr(filenamePos);
                ofs << fname << "\n";
                ofs << im->fullResolution << "\n";
                ofs << im->scale << "\n";
                ofs << im->centerPixel.x << "\n";
                ofs << im->centerPixel.y << "\n";
                ofs << im->isCoronaGraph << "\n";
        }
        ofs.close();
    }
}

// @TODO emiax: If openjpeg ever starts supporting reading XML metadata,
// this implementation should be improved in order not to search the entire buffer for
// XML data. There is an issue here:
// (https://github.com/uclouvain/openjpeg/issues/929)
ImageMetadata SpacecraftImageryManager::parseJ2kMetadata(
                                                      const ghoul::filesystem::File& file)
{
    ImageMetadata im;
    im.filename = file.path();

    std::ifstream stream(file.path(), std::ios::binary | std::ios::ate);
    std::streamsize size = stream.tellg();
    stream.seekg(0, std::ios::beg);
    std::vector<char> buffer(size);
    if (!stream.read(buffer.data(), size)) {
        LERROR(fmt::format("Failed to read data from '{}' ", file.fullBaseName()));
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
        LERROR(fmt::format("Could not find metadata in {}", file.fullBaseName()));
        return im;
    }

    std::optional<std::string_view> telescop =
        extractInnerXml(metaData.value(), "TELESCOP");
    if (!telescop.has_value()) {
        LERROR(fmt::format("Could not find TELESCOP tag {}", file.fullBaseName()));
        return im;
    }

    if (std::optional<std::string_view> naxis =
        extractInnerXml(metaData.value(), "NAXIS1"))
    {
        im.fullResolution = std::stoi(std::string(naxis.value()));
    }
    else {
        LERROR(fmt::format("Could not find NAXIS1 tag {}", file.fullBaseName()));
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
        LERROR(fmt::format("Could not find CRPIX1 tag {}", file.fullBaseName()));
        return im;
    }

    if (std::optional<std::string_view> centerPixelY =
        extractInnerXml(bufferView, "CRPIX2"))
    {
        centerPixel.y = std::stof(std::string(centerPixelY.value()));
    }
    else {
        LERROR(fmt::format("Could not find CRPIX2 tag {}", file.fullBaseName()));
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
            LERROR(fmt::format("Could not find NAXIS1 tag {}", file.fullBaseName()));
            return im;
        }       
    }
    else if (telescop.value() == "SDO") {
        std::optional<std::string_view> rsunObs = extractInnerXml(bufferView, "RSUN_OBS");
        std::optional<std::string_view> cDelt1 = extractInnerXml(bufferView, "CDELT1");
        
        if (!rsunObs.has_value()) {
            LERROR(fmt::format("Could not find RSUN_OBS tag {}", file.fullBaseName()));
            return im;
        }
        if (!cDelt1.has_value()) {
            LERROR(fmt::format("Could not find CDELT1 tag {}", file.fullBaseName()));
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
            LERROR(fmt::format("Could not find RSUN_OBS tag {}", file.fullBaseName()));
            return im;
        }
        if (!cDelt1.has_value()) {
            LERROR(fmt::format("Could not find CDELT1 tag {}", file.fullBaseName()));
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
            LWARNING(fmt::format(
                "Could not find DETECTOR tag {}", file.fullBaseName()
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
    im.filename = file.path();
    const std::string filename = std::string(file.fullBaseName() + ".json");

    if (!FileSys.fileExists(filename)) {
        LERROR(fmt::format("'{}' has no specified json metadata", file.fullBaseName()));
        // TODO: Hardcoded values, when there are no json files.
        return im;
    }

    // Parse JSON metadata
    using json = nlohmann::json;

    std::ifstream i(filename);
    if (i.fail()) {
        LERROR(fmt::format("Error opening file '{}'", filename));
    }

    json j;
    i >> j;
    const json data = j["meta"]["fits"];

    if (data.is_null()) {
        LERROR(fmt::format("Error in metadata '{}'", filename));
    }

    json value = data["TELESCOP"];
    if (value.is_null() && !value.is_string()) {
        LERROR("Metadata did contain information about type of spacecraft");
        return im;
    }
    // TODO: value might not exist
    std::string spacecraftType = value;

    value = data["NAXIS1"];
    if (value.is_null()) {
        LERROR("Metadata did not contain information about resolution");
    }

    const std::string sFullResolution = value;
    im.fullResolution = std::stoi(sFullResolution);
    // Special case of sdo - RSUN is given in pixels
    // For SOHO the radius of the sun is not specified - we instead use platescl
    if (spacecraftType == "SOHO") {
        const std::string sScale = data["PLATESCL"];
        const float plateScale = stof(sScale);
        im.scale = 1.f / (plateScale / 2.f);
        im.isCoronaGraph = true;
    } else {
        float sunRadiusPixels = 0.f;
        // SDO has RSUN specified in pixels
        if (spacecraftType == "SDO") {
            value = data["RSUN_OBS"];
            if (value.is_null()) {
                LERROR("SDO Metadata: RSUN_OBS missing!");
            }
            std::string sSunRadiusArcsec = value;
            value = data["CDELT1"];
            if (value.is_null()) {
                LERROR("SDO Metadata: CDELT1 missing!");
            }
            std::string sCdelt1 = value;
            const float cdelt1 = stof(sCdelt1);
            const float sunRadiusArcsec = stof(sSunRadiusArcsec);
            sunRadiusPixels = sunRadiusArcsec / cdelt1;
            im.isCoronaGraph = false;
        } else {
            // STEREO has RSUN specified in arcsecs - need to divide by factor
            std::string sCdelt1 = data["CDELT1"];
            const float cdelt1 = stof(sCdelt1);
            std::string sSunRadiusArcsec = data["RSUN"];
            const float sunRadiusArcsec = stof(sSunRadiusArcsec);
            sunRadiusPixels = sunRadiusArcsec / cdelt1;

            value = data["DETECTOR"];
            if (value.is_null()) {
                LERROR("No observer specified in Stereo");
            }

            std::string sObserver = value;
            if (sObserver == "COR1" || sObserver == "COR2") {
                im.isCoronaGraph = true;
            } else {
                im.isCoronaGraph = false;
            }
        }
        float scale = sunRadiusPixels / (im.fullResolution / 2.f);
        im.scale = scale;
    }

    const std::string sCenterPixelX = data["CRPIX1"];
    const std::string sCenterPixelY = data["CRPIX2"];

    const float centerPixelX = stof(sCenterPixelX);
    const float centerPixelY = stof(sCenterPixelY);
    const float halfRes = im.fullResolution / 2.f;

    const float offsetX = ((halfRes - centerPixelX) / halfRes) * SUN_RADIUS;
    const float offsetY = ((halfRes - centerPixelY) / halfRes) * SUN_RADIUS;

    im.centerPixel = glm::vec2(offsetX, offsetY);

    return im;
}

void SpacecraftImageryManager::loadImageMetadata(const std::string& path,
      std::unordered_map<std::string, ImageMetadataStateSequence>& _imageMetadataMap)
{
    std::unordered_map<std::string, ImageMetadataStateSequence> result;

    LDEBUG("Begin loading spacecraft imagery metadata");

    // Pre-processed data
    if (loadMetadataFromDisk(path, result)) {
        _imageMetadataMap.insert(result.begin(), result.end());
        return;
    }

    ghoul::filesystem::Directory sequenceDir(
        path,
        ghoul::filesystem::Directory::RawPath::Yes
    );

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load directory '{}'", sequenceDir.path()));
    }


    LDEBUG("Loading sequence directory");
    std::vector<std::string> sequencePaths = sequenceDir.read(
        ghoul::filesystem::Directory::Recursive::Yes,
        ghoul::filesystem::Directory::Sort::Yes
    );

    LDEBUG("Filtering data values");
    sequencePaths.erase(
        std::remove_if(
            sequencePaths.begin(),
            sequencePaths.end(),
            [](const std::string& path) {
                const std::string& ext = ghoul::filesystem::File(path).fileExtension();
                return (ext != "jp2") && (ext != "j2k");
            }
        ),
        sequencePaths.end()
    );
    

    LDEBUG("Reading meta data");
    size_t count = 0;

    std::mutex spiceAndPushMutex;

    std::vector<std::future<void>> futures;
    futures.reserve(sequencePaths.size());

    std::cout << '\n';
    auto exec = [&](const std::string& seqPath) {
        ghoul::filesystem::File currentFile(seqPath);
        std::string fileName = currentFile.filename();
        size_t posSatelliteInfoStart = fileName.rfind("__") + 2;
        std::string satelliteInfo = fileName.substr(posSatelliteInfoStart);

        // Name
        size_t posSatelliteNameEnd = satelliteInfo.find_first_of("_");
        std::string satelliteName = satelliteInfo.substr(0, posSatelliteNameEnd);

        // Instrument
        size_t posInstrumentNameStart = posSatelliteNameEnd + 1;
        std::string instrumentName = satelliteInfo.substr(posInstrumentNameStart);
        size_t dot = instrumentName.rfind(".");
        instrumentName = instrumentName.substr(0, dot);

        // Time
        std::vector<std::string> tokens;
        std::stringstream ss;
        ss.str(currentFile.filename());
        std::string item;
        while (std::getline(ss, item, '_')) {
            tokens.push_back(item);
        }
        if (tokens.size() >= 8) {
            std::string time = tokens[0] + "-" + tokens[1] + "-" +
                tokens[2] + "T" + tokens[4] + ":" +
                tokens[5] + ":" + tokens[6] + "." + tokens[7];

            const ImageMetadata im = parseJ2kMetadata(currentFile);
            std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
            spiceAndPushMutex.lock();
            TimedependentState<ImageMetadata> timeState(
                std::move(data),
                global::timeManager.time().convertTime(time), seqPath
            );
            result[instrumentName].addState(std::move(timeState));
            spiceAndPushMutex.unlock();
        }
        ++count;

        if (count % 10000 == 0) {
            LINFO(fmt::format(
                "Processing image {} out of {} ", count, sequencePaths.size()
            ));
        }
    };

    for (const std::string& seqPath : sequencePaths) {
        exec(seqPath);
    }

    saveMetadataToDisk(path, result);
    _imageMetadataMap.insert(result.begin(), result.end());

    LDEBUG("Finish loading imagery metadata");
    LDEBUG("Saving imagery metadata");

    LDEBUG(fmt::format("{} images loaded", count));
    LDEBUG(fmt::format("{} values in metamap", _imageMetadataMap.size()));
}

} //namespace openspace
