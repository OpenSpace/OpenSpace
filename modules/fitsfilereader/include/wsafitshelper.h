#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <valarray>

namespace CCfits {
    class FITS;
    class PHDU;
    class ExtHDU;
} // namespace CCfits


//namespace ghoul::opengl { class Texture; }
namespace openspace {

template<typename T>
struct ImageData {
    std::valarray<T> contents;
    long int width;
    long int height;
};

std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(std::filesystem::path path);
std::shared_ptr<ImageData<float>> callCorrectImageReader(const std::unique_ptr<CCfits::FITS>& file);
template<typename T>
std::shared_ptr<ImageData<T>> readImageInternal(CCfits::ExtHDU& image);
template<typename T>
std::shared_ptr<ImageData<T>> readImageInternal(CCfits::PHDU& image);
float readHeaderValueFloat(const std::string key, const std::unique_ptr<CCfits::FITS>& file);


} // namespace openspace

