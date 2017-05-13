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
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem>
#include <modules/solarbrowsing/util/simplej2kcodec.h>
#include <openspace/rendering/transferfunction.h>

using namespace ghoul::opengl;

namespace {
    const std::string _loggerCat = "SpacecraftImageryManager";
    // Might be needed for fits compability later
    //std::vector<std::string> _headerKeywords = {"EXPTIME", "BITPIX", "DATAVALS"};
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() { }

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
    std::unordered_map<std::string, std::unique_ptr<TransferFunction>>& _tfMap,
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
                        _tfMap[key] = std::make_unique<TransferFunction>(seqPath);
                    }
                }
            }
        }
    }
}

void SpacecraftImageryManager::loadImageMetadata(
      const std::string& path,
      std::unordered_map<std::string, std::vector<ImageMetadata>>& _imageMetadataMap,
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
                    //LDEBUG("Satellite NAME: " << satelliteName);

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
                        ImageMetadata metadata;
                        metadata.filename = seqPath;
                        metadata.timeObserved = Time::ref().convertTime(time);
                        _imageMetadataMap[instrumentName].push_back(metadata);
                    }
                }
            }
        }
        //LDEBUG("Finished loading path " << seqPath);
    }

    LDEBUG("Finish loading imagery metadata");
    LDEBUG(count << " Images loaded");
}

std::vector<ImageMetadata> SpacecraftImageryManager::loadImageMetadata(const std::string& path) {
    std::vector<ImageMetadata> imageSequenceMetadata;

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load directory '" << sequenceDir.path() << "'");
    }

    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::No, Sort::Yes);
    imageSequenceMetadata.reserve(sequencePaths.size());

    // TODO(mnoven): Remove this
    int limit = 0;
    for (auto seqPath : sequencePaths) {
       // if (limit++ == 1000) break;
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                std::string extension = currentFile.fileExtension();
                if (extension == "jp2" || extension == "j2k") {
                    const std::string relativePath = FileSys.relativePath(seqPath);

                    // TODO(mnoven): Prettify or read metadata instead
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
                    ImageMetadata metadata;
                    metadata.filename = seqPath;
                    metadata.timeObserved = Time::ref().convertTime(time);
                    imageSequenceMetadata.push_back(metadata);
                }
            }
        }
        LDEBUG("Finished loading path " << seqPath);
    }
    return std::move(imageSequenceMetadata);
}

// std::vector<ImageDataObject> SpacecraftImageryManager::loadImageData(const std::string& path, int& imageSize) {
//     std::vector<ImageDataObject> imageData;

//     using RawPath = ghoul::filesystem::Directory::RawPath;
//     ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

//     if (!FileSys.directoryExists(sequenceDir)) {
//         LERROR("Could not load FITS Directory '" << sequenceDir.path() << "'");
//     }

//     using Recursive = ghoul::filesystem::Directory::RawPath;
//     using Sort = ghoul::filesystem::Directory::Sort;
//     std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::Yes);
//     imageData.reserve(sequencePaths.size());

//     // TODO(mnoven): Remove this
//     int limit = 0;

//     for (auto seqPath : sequencePaths) {
//         if (limit++ == 2) break;
//         if (size_t position = seqPath.find_last_of(".") + 1) {
//             if (position != std::string::npos) {
//                 ghoul::filesystem::File currentFile(seqPath);
//                 std::string extension = currentFile.fileExtension();
//                 if (extension == "fits" || extension == "fit" || extension == "fts") {
//                     const std::string relativePath = FileSys.relativePath(seqPath);
//                     // We'll need to scan the header of the fits
//                     // and insert in some smart data structure that handles time / mn

//                     FitsFileReader::open(relativePath);
//                     ImageDataObject im;
//                     im.contents = FitsFileReader::readImage<IMG_PRECISION>();

//                     ImageMetadata metaData;
//                     metaData.filename = relativePath;
//                     metaData.expTime = FitsFileReader::readHeaderValue<float>(std::string("EXPTIME"));
//                     metaData.waveLength = FitsFileReader::readHeaderValue<int>(std::string("WAVELNTH"));

//                     metaData.min = FitsFileReader::readHeaderValue<int>(std::string("DATAMIN"));
//                     metaData.max = FitsFileReader::readHeaderValue<int>(std::string("DATAMAX"));

//                     std::string time = FitsFileReader::readHeaderValue<std::string>(std::string("DATE-OBS"));
//                     LDEBUG("Sending in TIME" << time);
//                     LDEBUG("Converted to " << Time::ref().convertTime(time));
//                     metaData.timeObserved = Time::ref().convertTime(time);

//                     im.metaData = metaData;

//                     //TODO(mnoven): Ugly, fix!
//                     imageSize = FitsFileReader::getImageSize().first;

//                     imageData.push_back(im);
//                     FitsFileReader::close();
//                 }
//             }
//         }
//         LDEBUG("Finished loading path " << seqPath);
//         LDEBUG("Count" << limit);
//     }
//     return std::move(imageData);
// }

} //namespace openspace
