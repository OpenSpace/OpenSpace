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
#include <unordered_set>

using namespace ghoul::opengl;

namespace {
    static const std::string _loggerCat = "SpacecraftImageryManager";
    static const std::unordered_set<std::string> _spacecraftTypes = {"SDO"};
    //static std::map<std::string, std::unique_ptr<Texture>> imageFiles; // Spacecraft -> Imagebuffer
}

namespace openspace {

SpacecraftImageryManager::SpacecraftImageryManager() { }

void SpacecraftImageryManager::scaleImageData(std::vector<std::valarray<float>>& imageData, const std::string& type) {
    if (_spacecraftTypes.find(type) != _spacecraftTypes.end()) {
        if (type == "SDO") {
            for (auto& data : imageData) {
                const float exptime = 2.902028f;
                const float minvalue = -1.0f;
                const float maxvalue = 41.0f;
                data *= (4.99803f / exptime);
                data = data.apply([](float val)->float {
                    if (val < 1.5f / 1.06f) return 1.5f / 1.06f;
                    else if (val > 50.0f / 1.06f) return 50.0f / 1.06f;
                });
                data = sqrt(data);
                float max = data.max();
                float min = data.min();

                data = (255.0f + 0.9999f) * (data - min) / (max - min);
                data /= 255.f;
            }
        }
    } else {
        LERROR("Couldn't find any spacecraft with type " << type);
    }
}

std::vector<std::unique_ptr<Texture>> SpacecraftImageryManager::loadTextures(std::vector<std::valarray<float>>& imageData) {
    std::vector<std::unique_ptr<Texture>> textures;
    textures.reserve(imageData.size());

    std::transform(imageData.begin(), imageData.end(), std::back_inserter(textures), [](std::valarray<float>& data) {
        const glm::size3_t imageSize(1024, 1024, 1); // TODO(mnoven) : Metadata
        const Texture::Format format = ghoul::opengl::Texture::Red;
        const Texture::FilterMode filterMode = Texture::FilterMode::Linear;
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

std::vector<std::valarray<float>> SpacecraftImageryManager::loadImageData(const std::string& path) {
    std::vector<std::valarray<float>> imageData;

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(path, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
    }

    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::Yes);
    imageData.reserve(sequencePaths.size());

    for (auto path : sequencePaths) {
        if (size_t position = path.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(path);
                std::string extension = currentFile.fileExtension();
                if (extension == "fits" || extension == "fit" || extension == "fts") {
                    std::string relativePath = FileSys.relativePath(path);
                    // We'll need to scan the header of the fits
                    // and insert in some smart data structure that handles time / mn
                    imageData.push_back(FitsFileReader::readImage(relativePath));
                }
            }
        }
    }
    return std::move(imageData);
}

} //namespace openspace
