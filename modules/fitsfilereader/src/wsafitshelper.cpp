/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/fitsfilereader/include/wsafitshelper.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/logging/logmanager.h>
#include <CCfits>

constexpr std::string_view _loggerCat = "RenderableTimeVaryingSphere";

using namespace CCfits;

namespace openspace {


std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
                                                         const std::filesystem::path path,
                                                                        size_t layerIndex,
                                                           std::pair<float, float> minMax)
{
    try {
        readFitsHeader(path);
        std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);
        if (!file.get()) {
            LERROR(std::format(
                "Failed to open, therefor removing file {}", path.string()
            ));
            std::filesystem::remove(path);
            return nullptr;
        }
        // Convert fits path with fits-file-reader functions
        const std::shared_ptr<ImageData<float>> fitsValues =
            readImageInternal<float>(file->pHDU());
        int layerSize = fitsValues->width * fitsValues->height;

        int nLayers = fitsValues->contents.size() / layerSize;
        if (layerIndex > nLayers -1) {
            LERROR(
                "Chosen layer in fits file is not supported. Index to high. ",
                "First layer chosen instead"
            );
            layerIndex = 0;
        }

        std::valarray<float> layerValues =
            fitsValues->contents[std::slice(layerIndex*layerSize, layerSize, 1)];

        float* imageData = new float[layerValues.size()];
        std::vector<glm::vec3> rgbLayers;
        for (size_t i = 0; i < layerValues.size(); i++) {
            // normalization
            float normalizedValue = (layerValues[i] - minMax.first) / (minMax.second - minMax.first);
            // clamping causes overexposure above and below max and min values
            // intentionally as desired by Nick Arge from WSA
            normalizedValue = std::clamp(normalizedValue, 0.f, 1.f);

            imageData[i] = normalizedValue;
        }

        // Create texture from imagedata
        auto texture = std::make_unique<ghoul::opengl::Texture>(
            imageData,
            glm::size3_t(fitsValues->width, fitsValues->height, 1),
            GL_TEXTURE_2D,
            ghoul::opengl::Texture::Format::Red,
            GL_RED,
            GL_FLOAT
        );
        // Tell it to use the single color channel as grayscale
        convertTextureFormat(*texture, ghoul::opengl::Texture::Format::RGB);
        texture->uploadTexture();
        return texture;
    }
    catch (const CCfits::FitsException& e) {
        LERROR(std::format(
            "Failed to open fits file '{}'. '{}'", path.string(), e.message()
        ));
        std::filesystem::remove(path);
        return nullptr;
    }
    catch (const std::exception& e) {
        LERROR(std::format(
            "Failed to open fits file '{}'. '{}'", path, e.what()
        ));
        std::filesystem::remove(path);
        return nullptr;
    }
    catch (...) {
        LERROR(std::format(
            "Unknown exception caught for file '{}'", path
        ));
        std::filesystem::remove(path);
        return nullptr;
    }
}

void readFitsHeader(const std::filesystem::path& path) {
    std::unique_ptr<CCfits::FITS> file =
        std::make_unique<CCfits::FITS>(path.string(), CCfits::Read, true);
    CCfits::PHDU& pHDU = file->pHDU();
    pHDU.readAllKeys();
    const std::map<CCfits::String, CCfits::Keyword*>& keyNames = pHDU.keyWord();

    std::string val;
    pHDU.readKey("CARRLONG", val);
    std::cout << "CARRLONG: " << val << std::endl;

    //for (const auto& [name, keyWord] : keyNames) {
    //    try {
    //        std::string val;
    //        keyWord->value(val);
    //        std::cout << name << " = " << val << std::endl;
    //    }
    //    catch (const CCfits::FitsException& e) {
    //        std::cerr << "Could not read value for key: " << name << " (" << e.message() << ")" << std::endl;
    //    }
    //}
}

int nLayers(const std::filesystem::path& path) {
    try {
        std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);
        if (!file.get()) {
            LERROR(std::format("Failed to open fits file '{}'", path));
            return -1;
        }
        // Convirt fits path with fits-file-reader functions
        const std::shared_ptr<ImageData<float>> fitsValues =
            readImageInternal<float>(file->pHDU());
        int layerSize = fitsValues->width * fitsValues->height;

        return fitsValues->contents.size() / layerSize;
    }
    catch (CCfits::FitsException& e) {
        LERROR(std::format(
            "Failed to open fits file '{}'. '{}'", path, e.message()
        ));
    }
    catch (std::exception& e) {
        LERROR(std::format(
            "Failed to open fits file '{}'. '{}'", path, e.what()
        ));
    }
    catch (...) {
        LERROR(std::format(
            "Unknown exception caught for file '{}'", path
        ));
    }
}

template<typename T, typename U>
std::shared_ptr<ImageData<T>> readImageInternal(U& image) {
    try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {
            .contents = std::move(contents),
            .width = static_cast<int>(image.axis(0)),
            .height = static_cast<int>(image.axis(1))
        };
        return std::make_shared<ImageData<T>>(im);
    }
    catch (const FitsException& e) {
        LERROR("Could not read FITS layer");
    }
    return nullptr;
}
} // namespace openspace
