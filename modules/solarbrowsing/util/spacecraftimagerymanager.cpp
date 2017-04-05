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

using namespace ghoul::opengl;

namespace {
    static const std::string _loggerCat = "SpacecraftImageryManager";
   // static const std::unordered_set<std::string> _spacecraftTypes = {"SDO"};
    //static std::map<std::string, std::unique_ptr<Texture>> imageFiles; // Spacecraft -> Imagebuffer
    static std::vector<std::string> _headerKeywords = {"EXPTIME", "BITPIX", "DATAVALS"};
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() { }

void SpacecraftImageryManager::scaleImageData(std::vector<ImageDataObject>& imageData, const std::string& type, const int& channel) {
    if (type == "SDO") {
        // 1600, 1700, 4500, 94, 131, 171, 193, 211, 304, 335
        const std::vector<float> normtimes = {2.99911f, 1.00026f, 1.00026f, 4.99803f, 6.99685f, 4.99803f, 2.99950f, 4.99801f, 4.99941f, 6.99734f};
        const std::vector<float> clipmins = {0.f, 0.f, 0.f, 1.5f, 7.f, 10.f, 120.f, 30.f, 15.f, 3.5f};
        const std::vector<float> clipmax = {4000.f, 10000.f, 26000.f, 50.f, 1200.f, 12000.f, 12000.f, 13000.f, 3000.f, 1000.f};

        const float normtime = normtimes[channel];
        const float cmin = clipmins[channel];
        const float cmax = clipmax[channel];

        for (auto& dataObject : imageData) {
            std::unordered_map<std::string, float>& metaData = dataObject.metaData;
            std::valarray<float>& data = dataObject.contents;

            const float exptime = metaData["EXPTIME"];
            assert(exptime > 0.f && exptime < 10.f);

            data = data * (normtimes[channel] / exptime);

            std::for_each(begin(data), end(data), [&cmin, &cmax](float& val) {
                val = std::min(cmax, std::max(val, cmin));
            });

            // Note: No need to set range right now,
            // glTexImage2D() will clamp all values to [0,1]
            switch (channel) {
                case 0 ... 2: {
                    data = 0.f; // TODO(mnoven): Remove this
                    //data = (data - cmin) / (cmax - cmin);
                    break;
                }
                case 3: {
                    float sqcmin = sqrt(cmin), sqcmax = sqrt(cmax);
                    data = sqrt(data);
                    data = (data - sqcmin) / (sqcmax - sqcmin);
                    break;
                }
                case 4 ... 9: {
                    // TODO(mnoven) : Remove this
                    //if (channel == 9) { data = 0.0f; continue;}

                    float alogcmin = log10(cmin), alogcmax = log10(cmax);
                    data = log10(data);
                    data = (data - alogcmin) / (alogcmax - alogcmin);
                    break;
                }
                default: {
                    LERROR("Wrong channel for SDO");
                    break;
                }

            }

        }
    } else {
        LERROR("Couldn't find any spacecraft with type " << type);
    }
}

std::unique_ptr<ghoul::opengl::Texture> SpacecraftImageryManager::createLUT() {
    // Let texture class handle deallocation
    float* LUT1D = new float[256 * 4];

    for (int i = 0; i < 256; i++) {
        float c0 = (float)i;
        float c1 = sqrt(c0) * sqrt(255.f);
        float c2 = std::pow(c0, 2.0) / 255.f;
        float c3 = ((int)c1 + ((int)c2) / 2.f) * 255.f / (255.f + 127.f);

        LUT1D[4*i + 0] = std::floor(c0) / 255.f; // R
        LUT1D[4*i + 1] = std::floor(c1) / 255.f; // G
        LUT1D[4*i + 2] = std::floor(c2) / 255.f; // B
        LUT1D[4*i + 3] = std::floor(c3) / 255.f; // A

        assert(!(LUT1D[4*i + 0] < 0.f ) && !(LUT1D[4*i + 0] > 1.f));
        assert(!(LUT1D[4*i + 1] < 0.f ) && !(LUT1D[4*i + 1] > 1.f));
        assert(!(LUT1D[4*i + 2] < 0.f ) && !(LUT1D[4*i + 2] > 1.f));
        assert(!(LUT1D[4*i + 3] < 0.f ) && !(LUT1D[4*i + 3] > 1.f));

        //std::cout << "mappingkey " << ((float)i/255.f) << " " << (int)c2 << " " << (int)c0 << " " << (int)c1 << " " << "255" << std::endl;
    }

    const glm::size3_t imageSize(256, 1, 1); // TODO(mnoven) : Metadata
    std::unique_ptr<Texture> t = std::make_unique<Texture>(
                                    LUT1D,
                                    imageSize,
                                    Texture::RGBA, // Format of the pixeldata
                                    GL_RGBA32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                                    GL_FLOAT, // Type of data
                                    Texture::FilterMode::Linear,
                                    Texture::WrappingMode::ClampToEdge
                                );
    return std::move(t);
}

std::vector<std::unique_ptr<Texture>> SpacecraftImageryManager::loadTextures(std::vector<ImageDataObject>& imageData) {
    std::vector<std::unique_ptr<Texture>> textures;
    textures.reserve(imageData.size());

    std::transform(imageData.begin(), imageData.end(), std::back_inserter(textures), [](ImageDataObject& dataObject) {
        std::valarray<float>& data = dataObject.contents;
        const glm::size3_t imageSize(4096, 4096, 1); // TODO(mnoven) : Metadata
        const Texture::Format format = ghoul::opengl::Texture::Red;
        const Texture::FilterMode filterMode = Texture::FilterMode::Linear;

        // TODO(mnoven): Remove this
        for ( int i = 0; i < data.size(); i++) {
            assert(!(data[i] < 0.f) && !(data[i] > 1.f));
        }

        std::unique_ptr<Texture> t = std::make_unique<Texture>(
                                        &data[0],
                                        imageSize,
                                        format, // Format of the pixeldata
                                        GL_R32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                                        GL_FLOAT, // Type of data
                                        Texture::FilterMode::Linear,
                                        Texture::WrappingMode::Repeat
                                    );
        // Memory is owned by renderable
        t->setDataOwnership(ghoul::Boolean::No);
        return t;
    });

    return std::move(textures);
}

std::vector<ImageDataObject> SpacecraftImageryManager::loadImageData(const std::string& path) {
    std::vector<ImageDataObject> imageData;

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
    }

    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::Yes);
    imageData.reserve(sequencePaths.size());

    // TODO(mnoven): Remove this
    int limit = 0;

    for (auto path : sequencePaths) {
        if (limit++ == 1) break;
        if (size_t position = path.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(path);
                std::string extension = currentFile.fileExtension();
                if (extension == "fits" || extension == "fit" || extension == "fts") {
                    std::string relativePath = FileSys.relativePath(path);
                    // We'll need to scan the header of the fits
                    // and insert in some smart data structure that handles time / mn
                    ImageDataObject im;
                    im.metaData = FitsFileReader::readHeaderFromImageTable(relativePath, _headerKeywords);
                    im.contents = FitsFileReader::readImageTable(relativePath);
                    im.type = relativePath;
                    imageData.push_back(im);
                }
            }
        }
        LDEBUG("Finished loading path " << path);
    }
    return std::move(imageData);
}

} //namespace openspace
