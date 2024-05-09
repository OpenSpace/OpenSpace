#include <modules/fitsfilereader/include/wsafitshelper.h>
#include <ghoul/logging/logmanager.h>
#include <CCfits>

constexpr std::string_view _loggerCat = "RenderableTimeVaryingSphere";


using namespace CCfits;
namespace openspace {


std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
                                                        const std::filesystem::path path)
{
    // Convirt fits path with fits-file-reader functions
    const std::shared_ptr<ImageData<float>> fitsValues = callCorrectImageReader(path);

    // Magnetogram slice
    // The numbers 64800, 16200 means, grab the fifth layer in the fits file, where the
    // photospheric map is, in the wsa file
    std::valarray<float> magnetogram = fitsValues->contents[std::slice(64800, 16200, 1)];
    const float maxValue = abs(magnetogram.max());

    std::vector<glm::vec3> rgbLayers;
    for (float mapValue : magnetogram) {
        float colorIntensity = abs(mapValue) / maxValue; // normalized value
        float r, g, b = 0.5f;
        if (mapValue < 0) {
            r = colorIntensity * 0.5f + 0.5f;
            g = colorIntensity * 0.5f + 0.5f;
            b = colorIntensity * 0.5f + 0.5f;
        }
        else if (mapValue > 0) {
            r = 1.0f - (colorIntensity * 0.5f + 0.5f);
            g = 1.0f - (colorIntensity * 0.5f + 0.5f);
            b = 1.0f - (colorIntensity * 0.5f + 0.5f);
        }
        glm::vec3 rgb = glm::vec3(r, g, b);
        rgbLayers.emplace_back(rgb);
    }

    // Potentially shifting the image goes here

    // Longitude leading edge of map header value
    // //todo do I need this?
    // const int long0 = *fitsFileReader.readHeaderValueFloat("CARRLONG");

    // Make into imagedata
    std::vector<float> imageData;
    for (glm::vec3 val : rgbLayers) {
        imageData.emplace_back(val.x);
        imageData.emplace_back(val.y);
        imageData.emplace_back(val.z);
    }

    // Create texture from imagedata
    auto texture = std::make_unique<ghoul::opengl::Texture>(
        imageData.data(),
        glm::vec3(180, 90, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_RGB,
        GL_FLOAT
    );
    texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    texture->uploadTexture();

    return std::move(texture);
}

// It was easier to make this into a little function to return right away than store it
// passed the if-statements
std::shared_ptr<ImageData<float>> callCorrectImageReader(const std::filesystem::path& path) {
    try {
        std::unique_ptr<FITS> file = std::make_unique<FITS>(path.string(), Read, true);
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
            .width = image.axis(0),
            .height = image.axis(1)
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
            .width = image.axis(0),
            .height = image.axis(1)
        };
        return std::make_shared<ImageData<T>>(im);
    }
    catch (const FitsException& e) {
        LERROR("Could not read FITS image PHDU. " + e.message());
    }
    return nullptr;
}













} // namespace openspace
