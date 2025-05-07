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
    int width;
    int height;
};
// path of file to load, layerIndex is which layer in the fits file to make into a texture
// and minMax is the value, positive and negative, in which to cap the data between
// to cause overexposure of higher and lower values.;
std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
                                                         const std::filesystem::path path,
                                                         int layerIndex, float minMax);

int nLayers(const std::filesystem::path path);
template<typename T, typename U>
std::shared_ptr<ImageData<T>> readImageInternal(U& image);

} // namespace openspace

