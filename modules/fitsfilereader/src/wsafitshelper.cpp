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
    std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);

    // Convirt fits path with fits-file-reader functions
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
    std::valarray<float> magnetogram =
        fitsValues->contents[std::slice(layerIndex*layerSize, layerSize, 1)];

    // Calculate median:
    //std::valarray<float> sorted = magnetogram;
    //std::sort(std::begin(sorted), std::end(sorted));
    //float median;
    //if (sorted.size() % 2 == 0)
    //    median = (sorted[sorted.size() / 2 - 1] + sorted[sorted.size() / 2]) / 2;
    //else
    //    median = sorted[sorted.size() / 2];

    const float maxValue = 50.f;// magnetogram.max();
    const float minValue = -50.f;// magnetogram.min();
    std::vector<float> imageData;
    //test.assign(std::begin(magnetogram), std::end(magnetogram));
    std::vector<glm::vec3> rgbLayers;
    for (float mapValue : magnetogram) {

        float normalizedValue = (mapValue - minValue) / (maxValue - minValue); // actual normalization
        normalizedValue = std::clamp(normalizedValue, 0.f, 1.f);

        imageData.emplace_back(normalizedValue);
    }

    // Potentially shifting the image goes here
    // Longitude leading edge of map header value
    // //todo do I need this?
    //const int long0 = static_cast<int>(readHeaderValueFloat("CARRLONG", file));
    //const int resolutionFactor = 360 / fitsValues->width;
    //const int shift = (360 - long0) / resolutionFactor;
    //for (int i = 0; i < fitsValues->height; ++i) {
    //    std::rotate(
    //        rgbLayers.begin() + (i * 180),
    //        rgbLayers.begin() + (i * 180) + shift,
    //        rgbLayers.begin() + (i * 180) + 179);
    //}



    // Create texture from imagedata
    auto texture = std::make_unique<ghoul::opengl::Texture>(
        imageData.data(),
        glm::size3_t(fitsValues->width, fitsValues->height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red,
        GL_RED,
        GL_FLOAT
    );
    texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    texture->uploadTexture();
    // Tell it to use the single color channel as grayscale
    convertTextureFormat(*texture, ghoul::opengl::Texture::Format::RGB);
    return std::move(texture);
}

int nLayers(const std::filesystem::path path) {
    std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);

    // Convirt fits path with fits-file-reader functions
    const std::shared_ptr<ImageData<float>> fitsValues = callCorrectImageReader(file);
    int layerSize = fitsValues->width * fitsValues->height;

    return fitsValues->contents.size() / layerSize;
}

// It was easier to make this into a little function to return right away than store it
// passed the if-statements
std::shared_ptr<ImageData<float>> callCorrectImageReader(const std::unique_ptr<FITS>& file) {
    try {
        bool isPrimaryHDU = file->extension().empty();
        isPrimaryHDU = true;
        if (isPrimaryHDU) {
            return readImageInternal<float>(file->pHDU());
        }
        else {
            return readImageInternal<float>(file->currentExtension());
        }
    }
    catch (const FitsException& e) {
        LERROR("Could not read FITS image from table. " + e.message());
    }
}

// This is pretty annoying, the read method is not derived from the HDU class
// in CCfits - need to explicitly cast to the sub classes to access read
template<typename T>
std::shared_ptr<ImageData<T>> readImageInternal(ExtHDU& image) {
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
        LERROR("Could not read FITS image EXTHDU. " + e.message());
    }
    return nullptr;
}

template<typename T>
std::shared_ptr<ImageData<T>> readImageInternal(PHDU& image) {
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
        LERROR("Could not read FITS image PHDU. " + e.message());
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
