﻿/*****************************************************************************************
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

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/threadpool.h>
#include <ghoul/opengl/texture.h>
#include <ext/json/json.hpp>
#include <chrono>
#include <fstream>
#include <future>
#include <string>

namespace {
    constexpr const char* _loggerCat = "SpacecraftImageryManager";
    constexpr const double SUN_RADIUS = 1391600000.0 * 0.5;
} // namespace

namespace openspace {

// MUST do this conversion before passing in the spice manager again - WTF.
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
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                const std::string extension = currentFile.fileExtension();
                const std::string base = currentFile.baseName();
                const std::size_t foundCachedImageData = base.find("_cached");
                if (foundCachedImageData != std::string::npos && extension == "txt") {
                    
                    const std::string::size_type separator = base.rfind("_");
                    const std::string instrument = base.substr(0, separator);
                    LDEBUG(fmt::format("Loading instrument: {}", instrument));

                    metadataLoaded = true;
                    std::ifstream myfile(currentFile.path());
                    if (!myfile.is_open()) {
                        LERROR("Failed to open metadata file");
                    }

                    int numStates;
                    myfile >> numStates;

                    for (int i = 0; i < numStates; i++) {
                        ImageMetadata im;

                        myfile >> std::ws; // Skip the rest of the line
                        std::string date;
                        std::getline(myfile, date);
                        double timeObserved = SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

                        std::string relPath;
                        myfile >> relPath;
                        im.filename = rootPath + "/" + relPath;

                        myfile >> im.fullResolution;
                        myfile >> im.scale;

                        float x, y;
                        myfile >> x >> y;
                        im.centerPixel = glm::vec2(x,y);
                        myfile >> im.isCoronaGraph;
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
            }
        }
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

// TODO(mnoven): Should NOT require JSON files to collect real metadata, when OpenJPEG supports reading XML metadata
// (https://github.com/uclouvain/openjpeg/issues/929), this should be implemented here.

ImageMetadata SpacecraftImageryManager::parseMetadata(const ghoul::filesystem::File& file,
                                                      const std::string& instrumentName)
{
    ImageMetadata im;
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
    im.filename = file.path();
    // TODO: value might not exist
    std::string spacecraftType = value;

    //int res = data["NAXIS1"];
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
    //unsigned int count = 0;
    std::atomic_uint count = 0;


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

            const ImageMetadata im = parseMetadata(currentFile, instrumentName);
            std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
            spiceAndPushMutex.lock();
            TimedependentState<ImageMetadata> timeState(
                std::move(data),
                OsEng.timeManager().time().convertTime(time), seqPath
            );
            result[instrumentName].addState(std::move(timeState));
            spiceAndPushMutex.unlock();
        }
        ++count;

        if (count % 10000 == 0) {
            std::cout << count << " / " << sequencePaths.size() << '\n';
        }
    };
    std::cout << '\n';

    //ghoul::ThreadPool pool(1);
    for (const std::string& seqPath : sequencePaths) {
        exec(seqPath);
    //    pool.queue(exec, seqPath);
    }
    //pool.stop();

    //std::for_each(
    //    std::execution::par_unseq,
    //    sequencePaths.begin(),
    //    sequencePaths.end(),
    //    [&](const std::string& seqPath) {
    //        ghoul::filesystem::File currentFile(seqPath);
    //        std::string fileName = currentFile.filename();
    //        size_t posSatelliteInfoStart = fileName.rfind("__") + 2;
    //        std::string satelliteInfo = fileName.substr(posSatelliteInfoStart);

    //        // Name
    //        size_t posSatelliteNameEnd = satelliteInfo.find_first_of("_");
    //        std::string satelliteName = satelliteInfo.substr(0, posSatelliteNameEnd);

    //        // Instrument
    //        size_t posInstrumentNameStart = posSatelliteNameEnd + 1;
    //        std::string instrumentName = satelliteInfo.substr(posInstrumentNameStart);
    //        size_t dot = instrumentName.rfind(".");
    //        instrumentName = instrumentName.substr(0, dot);

    //        // Time
    //        std::vector<std::string> tokens;
    //        std::stringstream ss;
    //        ss.str(currentFile.filename());
    //        std::string item;
    //        while (std::getline(ss, item, '_')) {
    //            tokens.push_back(item);
    //        }
    //        if (tokens.size() >= 8) {
    //            std::string time = tokens[0] + "-" + tokens[1] + "-" +
    //                tokens[2] + "T" + tokens[4] + ":" +
    //                tokens[5] + ":" + tokens[6] + "." + tokens[7];

    //            const ImageMetadata im = parseMetadata(currentFile, instrumentName);
    //            std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
    //            spiceAndPushMutex.lock();
    //            TimedependentState<ImageMetadata> timeState(
    //                std::move(data),
    //                OsEng.timeManager().time().convertTime(time), seqPath
    //            );
    //            _imageMetadataMap[instrumentName].addState(std::move(timeState));
    //            spiceAndPushMutex.unlock();
    //        }
    //        ++count;
    //    }
    //);


    //for (const std::string& seqPath : sequencePaths) {
    //    ghoul::filesystem::File currentFile(seqPath);
    //    std::string fileName = currentFile.filename();
    //    size_t posSatelliteInfoStart = fileName.rfind("__") + 2;
    //    std::string satelliteInfo = fileName.substr(posSatelliteInfoStart);

    //    // Name
    //    size_t posSatelliteNameEnd = satelliteInfo.find_first_of("_");
    //    std::string satelliteName = satelliteInfo.substr(0, posSatelliteNameEnd);

    //    // Instrument
    //    size_t posInstrumentNameStart = posSatelliteNameEnd + 1;
    //    std::string instrumentName = satelliteInfo.substr(posInstrumentNameStart);
    //    size_t dot = instrumentName.rfind(".");
    //    instrumentName = instrumentName.substr(0, dot);

    //    // Time
    //    std::vector<std::string> tokens;
    //    std::stringstream ss;
    //    ss.str(currentFile.filename());
    //    std::string item;
    //    while (std::getline(ss, item, '_')) {
    //        tokens.push_back(item);
    //    }
    //    if (tokens.size() >= 8) {
    //        std::string time = tokens[0] + "-" + tokens[1] + "-" +
    //            tokens[2] + "T" + tokens[4] + ":" +
    //            tokens[5] + ":" + tokens[6] + "." + tokens[7];

    //        const ImageMetadata im = parseMetadata(currentFile, instrumentName);
    //        std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
    //        TimedependentState<ImageMetadata> timeState(
    //            std::move(data),
    //            OsEng.timeManager().time().convertTime(time), seqPath
    //        );
    //        _imageMetadataMap[instrumentName].addState(std::move(timeState));
    //    }
    //    ++count;
    //}

    saveMetadataToDisk(path, result);
    _imageMetadataMap.insert(result.begin(), result.end());

    LDEBUG("Finish loading imagery metadata");
    LDEBUG("Saving imagery metadata");

    LDEBUG(fmt::format("{} images loaded", count));
    LDEBUG(fmt::format("{} values in metamap", _imageMetadataMap.size()));
}

} //namespace openspace
