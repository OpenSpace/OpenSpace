#include <modules/fitsfilereader/include/wsafitshelper.h>
#include <CCfits>

std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(std::filesystem::path path) {

    //convirt fits path with fits file reader functions

    //make into imagedata
    std::vector<float> imagedata;

    //create texture from imagedata or already an image?
    auto texture = std::make_unique<ghoul::opengl::Texture>(
        std::move(imagedata.data()),
        glm::vec3(180, 90, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_RGB32F,
        GL_FLOAT
    );
    texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    texture->uploadTexture();

    return std::move(texture);
}






























































