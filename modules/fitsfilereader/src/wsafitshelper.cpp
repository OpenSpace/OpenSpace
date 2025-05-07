#include <modules/fitsfilereader/include/wsafitshelper.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/logging/logmanager.h>
#include <CCfits>

constexpr std::string_view _loggerCat = "RenderableTimeVaryingSphere";


using namespace CCfits;
namespace openspace {


std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
                                                         const std::filesystem::path path,
                                                         int layerIndex, float minMax)
{
    try {
        std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);
        if (!file.get()) {
            LERROR(
                std::format("Failed to open, therefor removing file {}", path.string())
            );
            std::filesystem::remove(path);
            return nullptr;
        }
        // Convert fits path with fits-file-reader functions
        const std::shared_ptr<ImageData<float>> fitsValues =
            readImageInternal<float>(file->pHDU());
        int layerSize = fitsValues->width * fitsValues->height;

        int nLayers = fitsValues->contents.size() / layerSize;
        if (layerIndex > nLayers -1) {
            LERROR(std::format(
                "Chosen layer in fits file is not supported. Index to high. ",
                "First layer chosen instead"
            ));
            layerIndex = 0;
        }
        // The numbers 64800, 16200 means: grab the fifth layer in the fits file, where the
        // magnetogram map is, in the wsa file
        std::valarray<float> layerValues =
            fitsValues->contents[std::slice(layerIndex*layerSize, layerSize, 1)];

        const float maxValue = minMax; // layerValues.max();
        const float minValue = -minMax; // layerValues.min();
        float* imageData = new float[layerValues.size()];
        std::vector<glm::vec3> rgbLayers;
        for (size_t i = 0; i < layerValues.size(); i++) {
            // normalization
            float normalizedValue = (layerValues[i] - minValue) / (maxValue - minValue);
            // clamping causes overexposure above and below max and min values intentionally
            // as desired by Nick Arge from WSA
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
    catch (CCfits::FitsException& e) {
        LERROR(
            std::format("Failed to open fits file '{}'. '{}'", path.string(), e.message())
        );
        std::filesystem::remove(path);
        return nullptr;
    }
    catch (std::exception& e) {
        LERROR(
            std::format("Failed to open fits file '{}'. '{}'", path.string(), e.what())
        );
        std::filesystem::remove(path);
        return nullptr;
    }
    catch (...) {
        LERROR(
            std::format("Unknown exception caught for file '{}'", path.string())
        );
        std::filesystem::remove(path);
        return nullptr;
    }
}

int nLayers(const std::filesystem::path path) {
    try {
        std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);
        if (!file.get()) {
            LERROR(
                std::format("Failed to open fits file '{}'", path.string())
            );
            return -1;
        }
        // Convirt fits path with fits-file-reader functions
        const std::shared_ptr<ImageData<float>> fitsValues =
            readImageInternal<float>(file->pHDU());
        int layerSize = fitsValues->width * fitsValues->height;

        return fitsValues->contents.size() / layerSize;
    }
    catch (CCfits::FitsException& e) {
        LERROR(
            std::format("Failed to open fits file '{}'. '{}'", path.string(), e.message())
        );
    }
    catch (std::exception& e) {
        LERROR(
            std::format("Failed to open fits file '{}'. '{}'", path.string(), e.what())
        );
    }
    catch (...) {
        LERROR(
            std::format("Unknown exception caught for file '{}'", path.string())
        );
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
