#include <modules/fitsfilereader/include/wsafitshelper.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/logging/logmanager.h>
#include <CCfits>

constexpr std::string_view _loggerCat = "RenderableTimeVaryingSphere";


using namespace CCfits;
namespace openspace {


std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
                                                         const std::filesystem::path path,
                                                         int layerIndex)
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
        const std::shared_ptr<ImageData<float>> fitsValues = callCorrectImageReader(file);
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

        // Calculate median:
        //std::valarray<float> sorted = layerValues;
        //std::sort(std::begin(sorted), std::end(sorted));
        //float median;
        //if (sorted.size() % 2 == 0)
        //    median = (sorted[sorted.size() / 2 - 1] + sorted[sorted.size() / 2]) / 2;
        //else
        //    median = sorted[sorted.size() / 2];

        const float maxValue = 50.f;// layerValues.max();
        const float minValue = -50.f;// layerValues.min();
        float* imageData = new float[layerValues.size()];
        //test.assign(std::begin(layerValues), std::end(layerValues));
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
        const std::shared_ptr<ImageData<float>> fitsValues = callCorrectImageReader(file);
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

// It was easier to make this into a little function to return right away than store it
// passed the if-statements
std::shared_ptr<ImageData<float>> callCorrectImageReader(const std::unique_ptr<FITS>& file) {
    try {
//        bool isPrimaryHDU = file->extension().empty();
//        isPrimaryHDU = true;
//        if (isPrimaryHDU) {
            return readImageInternal<float>(file->pHDU());
//        }
//        else {
//            return readImageInternal<float>(file->currentExtension());
//        }
    }
    catch (const FitsException& e) {
        LERROR("Could not read FITS image from table. " + e.message());
    }
}

// This is pretty annoying, the read method is not derived from the HDU class
// in CCfits - need to explicitly cast to the sub classes to access read
//template<typename T>
//std::shared_ptr<ImageData<T>> readImageInternal(ExtHDU& image) {
//    try {
//        std::valarray<T> contents;
//        image.read(contents);
//        ImageData<T> im = {
//            .contents = std::move(contents),
//            .width = static_cast<int>(image.axis(0)),
//            .height = static_cast<int>(image.axis(1))
//        };
//        return std::make_shared<ImageData<T>>(im);
//    }
//    catch (const FitsException& e) {
//        LERROR("Could not read FITS image EXTHDU. " + e.message());
//    }
//    return nullptr;
//}

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

float readHeaderValueFloat(const std::string key, const std::unique_ptr<FITS>& file) {
    float value;
    HDU& image = static_cast<HDU&>(file->pHDU());
    image.readKey(key, value);
    return value;
}
} // namespace openspace
