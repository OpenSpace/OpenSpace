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

using namespace ghoul::opengl;

namespace {
    const std::string _loggerCat = "SpacecraftImageryManager";
   // static const std::unordered_set<std::string> _spacecraftTypes = {"SDO"};
    //static std::map<std::string, std::unique_ptr<Texture>> imageFiles; // Spacecraft -> Imagebuffer
    std::vector<std::string> _headerKeywords = {"EXPTIME", "BITPIX", "DATAVALS"};

    // const std::unordered_map<int, std::pair<int, int>> SDOclips = {
    //     {1600, {0.f,   4000.f},
    //     {1700, {0.f,   10000.f},
    //     {4500, {0.f,   26000.f},
    //     {94,   {1.5f,     50.f},
    //     {131,  {7.f,    1200.f},
    //     {171,  {10.f,  12000.f},
    //     {193,  {120.f, 12000.f},
    //     {211,  {30.f,  13000.f},
    //     {304,  {15.f,   3000.f},
    //     {335,  {3.5f,   1000.f}
    // };
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() { }

// void SpacecraftImageryManager::scaleImageData(std::vector<ImageDataObject>& imageData, const std::string& type, const int& channel) {
//     if (type == "SDO") {
//         // 1600, 1700, 4500, 94, 131, 171, 193, 211, 304, 335 // (1600, 1700, 4500)
//         //const std::vector<float> normtimes = {2.99911f, 1.00026f, 1.00026f, 4.99803f, 6.99685f, 4.99803f, 2.99950f, 4.99801f, 4.99941f, 6.99734f};
//         //const std::vector<float> clipmins =           {0.f, 0.f, 0.f, 1.5f, 7.f, 10.f, 120.f, 30.f, 15.f, 3.5f};
//         //const std::vector<float> clipmax = {4000.f, 10000.f, 26000.f, 50.f, 1200.f, 12000.f, 12000.f, 13000.f, 3000.f, 1000.f};
//         //const std::vector<float> clipmins = {20.f, 220.f, 4000.f, 0.1f, 0.7f, 10.f, 20.f, 7.f, 0.2f, 0.4f};
//         //const std::vector<float> clipmax = {400.f, 5000.f, 20000.f, 30.f, 500.f, 2000.f, 2500.f, 1500.f, 150.f, 80.f};
//         //const float normtime = normtimes[channel];

//         for (auto& dataObject : imageData) {
//             std::valarray<IMG_PRECISION>& data = dataObject.contents;

//             // const float& exptime = dataObject.metaData.expTime;
//             // assert(exptime > 0.f && exptime < 10.f);

//             // //data = data * (normtimes[channel] / exptime);
//             // data = data / (1.f * exptime);

//             // // Just copied from IDL, does this make any sense at all?
//             // data[0] = 0.f;
//             // data[1] = 500000.f;

//             // LDEBUG("Loading channel: " << channel);
//             // LDEBUG("Max data: " << data.max());
//             // LDEBUG("MIN data: " << data.min());
//             // LDEBUG("Exptime " << exptime);

//             // // TODO(mnoven) : Show wavelengths 1700, 4500, 1600? Where to get?
//             // if (channel == 0 || channel == 1 || channel == 2) {
//             //     data = 0.f;
//             //     return;
//             // }
//             // if (channel == 5) { // 171
//             //     // TODO(mnoven): Make this prettier and optimize
//             //     data -= 5;
//             //     const float datamax = data.max();
//             //     // TODO(mnoven): Use std::clamp in C++17
//             //     std::for_each(begin(data), end(data), [&datamax](float& val) {
//             //         val = std::min(datamax, std::max(val, 0.01f));
//             //     });

//             //     data = std::pow(data, 0.35f);
//             //     std::for_each(begin(data), end(data), [](float& val) {
//             //         val = std::min(13.f, std::max(val, 0.01f));
//             //     });
//             // } else {
//             //     const float cmin = clipmins[channel];
//             //     const float cmax = clipmax[channel];

//             //     // TODO(mnoven): Use std::clamp in C++17
//             //     std::for_each(begin(data), end(data), [&cmin, &cmax](float& val) {
//             //         val = std::min(cmax, std::max(val, cmin));
//             //     });

//             //     data = log10(data);
//             // }
//             // Scale values to [0, 1]
//            // float min = data.min();
//             //float max = data.max();
//             //data = (data - min) / (max - min);

//             LDEBUG("Min fits" << dataObject.metaData.min);
//             LDEBUG("Max fits" << dataObject.metaData.max);

//             // TODO(mnoven): Remove
//             // For some reason the small test images have wrong min and max values
//             //assert(data.min() == dataObject.metaData.min);
//             //assert(data.max() == dataObject.metaData.max);

//         }
//     } else {
//         LERROR("Couldn't find any spacecraft with type " << type);
//     }
// }

// std::unique_ptr<ghoul::opengl::Texture> SpacecraftImageryManager::createLUT() {
//     // Let texture class handle deallocation
//     float* LUT1D = new float[256 * 4];

//     for (int i = 0; i < 256; i++) {
//         float c0 = (float)i;
//         float c1 = sqrt(c0) * sqrt(255.f);
//         float c2 = std::pow(c0, 2.0) / 255.f;
//         float c3 = ((int)c1 + ((int)c2) / 2.f) * 255.f / (255.f + 127.f);

//         LUT1D[4*i + 0] = std::floor(c0) / 255.f; // R
//         LUT1D[4*i + 1] = std::floor(c1) / 255.f; // G
//         LUT1D[4*i + 2] = std::floor(c2) / 255.f; // B
//         LUT1D[4*i + 3] = std::floor(c3) / 255.f; // A

//         assert(!(LUT1D[4*i + 0] < 0.f ) && !(LUT1D[4*i + 0] > 1.f));
//         assert(!(LUT1D[4*i + 1] < 0.f ) && !(LUT1D[4*i + 1] > 1.f));
//         assert(!(LUT1D[4*i + 2] < 0.f ) && !(LUT1D[4*i + 2] > 1.f));
//         assert(!(LUT1D[4*i + 3] < 0.f ) && !(LUT1D[4*i + 3] > 1.f));

//         //std::cout << "mappingkey " << ((float)i/255.f) << " " << (int)c2 << " " << (int)c0 << " " << (int)c1 << " " << "255" << std::endl;
//     }

//     const glm::size3_t imageSize(256, 1, 1); // TODO(mnoven) : Metadata
//     std::unique_ptr<Texture> t = std::make_unique<Texture>(
//                                     LUT1D,
//                                     imageSize,
//                                     Texture::RGBA, // Format of the pixeldata
//                                     GL_RGBA32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
//                                     GL_FLOAT, // Type of data
//                                     Texture::FilterMode::Linear,
//                                     Texture::WrappingMode::ClampToEdge
//                                 );
//     return std::move(t);
// }

// std::vector<std::unique_ptr<Texture>> SpacecraftImageryManager::loadTextures(std::vector<ImageDataObject>& imageData) {
//     std::vector<std::unique_ptr<Texture>> textures;
//     textures.reserve(imageData.size());

//     std::transform(imageData.begin(), imageData.end(), std::back_inserter(textures), [](ImageDataObject& dataObject) {
//         std::valarray<float>& data = dataObject.contents;

//         const int& sizeX = dataObject.metaData.size.first;
//         const int& sizeY = dataObject.metaData.size.second;

//         const glm::size3_t imageSize(sizeX, sizeY, 1);
//         const Texture::Format format = ghoul::opengl::Texture::Red;
//         const Texture::FilterMode filterMode = Texture::FilterMode::Linear;

//         // TODO(mnoven): Remove this
//         for ( int i = 0; i < data.size(); i++) {
//             assert(!(data[i] < 0.f) && !(data[i] > 1.f));
//         }

//         std::unique_ptr<Texture> t = std::make_unique<Texture>(
//                                         &data[0],
//                                         imageSize,
//                                         format, // Format of the pixeldata
//                                         GL_R32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
//                                         GL_FLOAT, // Type of data
//                                         Texture::FilterMode::Linear,
//                                         Texture::WrappingMode::ClampToEdge
//                                     );
//         // Memory is owned by renderable
//         t->setDataOwnership(ghoul::Boolean::No);
//         return t;
//     });

//     return std::move(textures);
// }

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
                    j2c.CreateInfileStream(relativePath);
                    j2c.SetupDecoder(0);
                    auto decodedImg = j2c.Decode();
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
