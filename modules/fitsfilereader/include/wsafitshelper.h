#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
//namespace ghoul::opengl { class Texture; }

std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(std::filesystem::path path);
