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

#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <ghoul/opengl/texture.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem>
#include <modules/solarbrowsing/util/simplej2kcodec.h>
#include <openspace/rendering/transferfunction.h>
#include <string>
#include <chrono>

typedef std::chrono::high_resolution_clock Clock;

#include <fstream>
#include <ext/json/json.hpp>

using namespace ghoul::opengl;

namespace {
    const std::string _loggerCat = "SpacecraftImageryManager";
    // Might be needed for fits compability later
    //std::vector<std::string> _headerKeywords = {"EXPTIME", "BITPIX", "DATAVALS"};
    const double SUN_RADIUS = (1391600000.0 * 0.50);
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() {}

void SpacecraftImageryManager::ConvertTileJ2kImages(const std::string& path,
                                                    const unsigned int tileWidth,
                                                    const unsigned int tileHeight)
{
    using RawPath = ghoul::filesystem::Directory::RawPath;
    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::No, Sort::Yes);

    int limit=0;
     for (auto seqPath : sequencePaths) {
        if(limit++ == 10) break;
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                std::string extension = currentFile.fileExtension();
                if (extension == "j2k" || extension == "jp2") {
                    const std::string relativePath = FileSys.relativePath(seqPath);
                    const std::string outPath = FileSys.relativePath(path + "/converted/" + currentFile.filename()).c_str();

                    SimpleJ2kCodec j2c;
                    auto decodedImg = j2c.Decode(relativePath, 0);
                    j2c.EncodeAsTiles(outPath.c_str(),
                                      decodedImg->data,
                                      decodedImg->w,
                                      decodedImg->h,
                                      /*tileWidth=*/tileWidth,
                                      /*tileHeight=*/tileHeight,
                                      /*numComps=*/1,
                                      /*compPrec=*/8);
                }
            }
        }
        LDEBUG("Finished converting " << seqPath);
    }
}

void SpacecraftImageryManager::loadTransferFunctions(
    const std::string& path,
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap,
    const std::unordered_set<std::string>& _filter)
{
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load directory '" << sequenceDir.path() << "'");
    }

    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::Yes);

    for (auto seqPath : sequencePaths) {
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                std::string extension = currentFile.fileExtension();
                if (extension == "txt") {
                    std::string key = currentFile.baseName();
                    std::string filterKey = key;
                    std::transform(filterKey.begin(), filterKey.end(), filterKey.begin(),
                                   ::tolower);

                    // If filter is empty or value exist
                    if (_filter.size() == 0
                        || _filter.find(filterKey) != _filter.end()) {
                        _tfMap[key] = std::make_shared<TransferFunction>(seqPath);
                    }
                }
            }
        }
    }
}

ImageMetadata SpacecraftImageryManager::parseMetadata(const ghoul::filesystem::File& file) {
    auto t1 = Clock::now();

    const std::string filename = std::string(file.fullBaseName() + ".json");
    using json = nlohmann::json;
    ImageMetadata im;

    std::ifstream i(filename);
    json j;
    i >> j;
    const json data = j["meta"]["fits"];

    if (data.is_null()) {
        LERROR("Error in metadata " << filename);
    }

    const json value = data["TELESCOP"];
    if (value.is_null() && !value.is_string()) {
        LERROR("Metadata did not hold information about type of spacecraft");
        return im;
    }
    im.filename = file.path();
    im.spacecraftType = value;
    // TODO: value might not exist

    //int res = data["NAXIS1"];
    std::string sFullResolution = data["NAXIS1"];
    im.fullResolution = std::stoi(sFullResolution);

    // Special case of sdo - RSUN is given in pixels
    // For SOHO the radius of the sun is not specified - we instead use platescl
    if (im.spacecraftType == "SOHO") {
        std::string sScale = data["PLATESCL"];
        float plateScale = stof(sScale);
        im.scale = 1.0 / plateScale;
    } else {
        float sunRadiusPixels = 0.f;
        // SDO has RSUN specified in pixels
        if (im.spacecraftType == "SDO") {
            std::string sSunRadiusPixels = data["RSUN"];
            sunRadiusPixels = stof(sSunRadiusPixels);
        }
        // STEREO has RSUN specified in arcsecs - need to divide by factor
        else {
            std::string sCdelt1 = data["CDELT1"];
            const float cdelt1 = stof(sCdelt1);
            std::string sSunRadiusArcsec = data["RSUN"];
            const float sunRadiusArcsec = stof(sSunRadiusArcsec);
            sunRadiusPixels = sunRadiusArcsec / cdelt1;
        }

        float scale = sunRadiusPixels / (im.fullResolution / 2.f); //* SUN_RADIUS;
        im.scale = scale;
    }

    LDEBUG("scale " << im.scale);

    //float centerpixelX = data["CRPIX1"];
    //float centerpixelY = data["CRPIX2"];
    std::string sCenterPixelX = data["CRPIX1"];
    std::string sCenterPixelY = data["CRPIX2"];

    float centerPixelX = stof(sCenterPixelX);
    float centerPixelY = stof(sCenterPixelY);
    float halfRes = im.fullResolution / 2.f;

    float offsetX = ((halfRes - centerPixelX) / halfRes) * SUN_RADIUS;
    float offsetY = ((halfRes - centerPixelY) / halfRes) * SUN_RADIUS;

    im.centerPixel = glm::vec2(offsetX, offsetY);

    // Measure time
    auto t2 = Clock::now();
  // LDEBUG("Metadata parse time "
    //       << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
      //     << " ms" << std::endl);
    return im;

}

void SpacecraftImageryManager::loadImageMetadata(
      const std::string& path,
      std::unordered_map<std::string, TimedependentStateSequence<ImageMetadata>>& _imageMetadataMap,
      const std::unordered_set<std::string>& _filter)
{

    LDEBUG("Begin loading imagery metadata");

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load directory '" << sequenceDir.path() << "'");
    }

    unsigned int count = 0;
    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::Yes);

    for (auto seqPath : sequencePaths) {
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                std::string extension = currentFile.fileExtension();
                if (extension == "jp2" || extension == "j2k") {
                    // // TODO(mnoven): Prettify or read metadata instead
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
                    std::string filterKey = instrumentName;
                    std::transform(filterKey.begin(), filterKey.end(), filterKey.begin(),
                                   ::tolower);

                    // If filter is empty or value exist
                    if (_filter.size() == 0
                        || _filter.find(filterKey) != _filter.end()) {
                        count++;
                        // Time
                        std::vector<std::string> tokens;
                        std::stringstream ss;
                        ss.str(currentFile.filename());
                        std:: string item;
                        while (std::getline(ss, item, '_')) {
                            tokens.push_back(item);
                        }
                        std::string time = tokens[0] + "-" + tokens[1] + "-" +
                                           tokens[2] + "T" + tokens[4] + ":" +
                                           tokens[5] + ":" + tokens[6] + "." + tokens[7];

                        //auto t = OsEng.timeManager();
                        const ImageMetadata im = parseMetadata(currentFile);
                        std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
                        TimedependentState<ImageMetadata> timeState(
                              std::move(data), /*OsEng.timeManager().time().convertTime(time)*/2123132, seqPath);
                        _imageMetadataMap[instrumentName].addState(std::move(timeState));
                    }
                }
            }
        }
    }

    LDEBUG("Finish loading imagery metadata");
    LDEBUG(count << " Images loaded");
}

} //namespace openspace
