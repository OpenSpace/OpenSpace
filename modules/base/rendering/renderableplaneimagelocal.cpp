/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/base/rendering/renderableplaneimagelocal.h>

// Included for testing purposes
#include <modules/fitsfilereader/include/fitsfilereader.h>
//#include <filesystem>
#include <ghoul/opengl/framebufferobject.h>
#include <cctype>
// <- End of experimental includes

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/opengl/texture.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "RenderableImagePlaneLocal";
} // namespace

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneImageLocal::Documentation() {
    using namespace documentation;
    return {
        "Renderable Plane Image Local",
        "base_renderable_plane_image_local",
        {
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                TextureInfo.description,
            }
        }
    };
}

RenderablePlaneImageLocal::RenderablePlaneImageLocal(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlaneImageLocal"
    );

    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);

    addProperty(_texturePath);
    _texturePath.onChange([this]() {loadTexture(); });
    _textureFile->setCallback(
        [this](const ghoul::filesystem::File&) { _textureIsDirty = true; }
    );
}

bool RenderablePlaneImageLocal::isReady() const {
    return RenderablePlane::isReady() && (_texture != nullptr);
}

void RenderablePlaneImageLocal::initializeGL() {
    RenderablePlane::initializeGL();

    loadTexture();
}

void RenderablePlaneImageLocal::deinitializeGL() {
    _textureFile = nullptr;

    BaseModule::TextureManager.release(_texture);
    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageLocal::bindTexture() {
    _texture->bind();
}

void RenderablePlaneImageLocal::update(const UpdateData& data) {
    RenderablePlane::update(data);

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneImageLocal::loadTexture() {
    if (!_texturePath.value().empty()) {
        ghoul::opengl::Texture* t = _texture;

        unsigned int hash = ghoul::hashCRC32File(_texturePath);

        _texture = BaseModule::TextureManager.request(
            std::to_string(hash),
            [path = _texturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
             /*   std::unique_ptr<ghoul::opengl::Texture> texture =
                    ghoul::io::TextureReader::ref().loadTexture(absPath(path));

                LDEBUGC(
                    "RenderablePlaneImageLocal",
                    fmt::format("Loaded texture from '{}'", absPath(path))
                );
                texture->uploadTexture();

                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

             //   return texture; */

            // Testing out the FitsFileReader:

            FitsFileReader fitsFileReader(false);
            std::unique_ptr<ghoul::opengl::FramebufferObject> testFBO =  std::make_unique<ghoul::opengl::FramebufferObject>();


            // Given that the node-part is located just outside the openspace-directory
            const std::string fitsDir = "../../../../../../node/FITSdata/mrzqs190501/";
            std::string testpath = absPath(fitsDir + "mrzqs190501t0114c2216_006.fits");
            GLenum oneTexture;
                
            
            // All the files in the given directory
            for (const auto & entry : ghoul::filesystem::Directory(fitsDir).readFiles()) {
                LERROR("Starting to upload texture");

                // To get a handle to the regular framebuffer, that we can switch back to once we have uploaded all textures
                GLint defaultFBO = testFBO->getActiveObject();
                
                testFBO->activate();
                LERROR("Initial framebufferobject status: " + testFBO->errorChecking((glCheckFramebufferStatus(GL_FRAMEBUFFER))));

                // Mapkey is to get a handle of the timestep of the texture
                std::string mapKey ="";
                const auto tempBild = fitsFileReader.readImageFloat(entry);
                int magicalCounter = -6;
                for (char c : entry) {
                    if (std::isdigit(c)) {
                        if (magicalCounter >= 0 && magicalCounter < 10) {
                            mapKey += c;
                        }
                        magicalCounter++;
                    }
                }
                const float minvalue = *fitsFileReader.readHeaderValueFloat("IMGMIN01");
                const float maxvalue = *fitsFileReader.readHeaderValueFloat("IMGMAX01");

                const int imageHeight = tempBild->height;
                const int imageWidth = tempBild->width;
  
                GLuint fitsImage[360][180];

                /*auto texture = std::make_unique<ghoul::opengl::Texture>(glm::uvec3(
                    imageHeight,
                    imageWidth,
                    1
                )); */

                // Commented this to see if it accepts an empty texture
                for (int i = 0; i < 360; i++) {
                    for (int j = 0; j < 180; j++) {
                        float color = tempBild->contents[(i * 180) + j];
                        color = (color - minvalue) / (maxvalue - minvalue);
                        fitsImage[i][j] = (GLuint)(color * 255);
                    }
                } 
                
                auto textureFits = std::make_unique<ghoul::opengl::Texture>(fitsImage, glm::uvec3(360, 180, 1));
                LERROR(std::to_string(fitsImage[100][100]));
                textureFits->uploadTexture();
                

                oneTexture = static_cast<GLenum>(std::stol(mapKey));
                //texture->uploadTexture();
                
                testFBO->attachTexture(textureFits.get(), oneTexture);
                LERROR("After texture upload: " + testFBO->errorChecking((glCheckFramebufferStatus(GL_FRAMEBUFFER))));
                LERROR("Uploaded one texture! " + mapKey);
                //LERROR(std::to_string(testFBO->getActiveObject()));
                testFBO->deactivate();
                glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
            }

            //auto testbild = fitsFileReader.readImageFloat(testpath);

            //const float minvalue = *fitsFileReader.readHeaderValueFloat("IMGMIN01");
            //const float maxvalue = *fitsFileReader.readHeaderValueFloat("IMGMAX01");
            //LERROR(testpath);
            //LERROR("Min / Max respectively: " + std::to_string(minvalue) + " / " + std::to_string(maxvalue) + " -- Width: " + std::to_string(imageWidth) + " -- Height: " + std::to_string(imageHeight) + " size: "+ std::to_string(testbild->contents.size()));
            
            

            auto texture = testFBO->texture(oneTexture);

            //set interpolation method
            //textureFits->setFilter(ghoul::opengl::Texture::FilterMode::Nearest); */
            
            return std::make_unique<ghoul::opengl::Texture>(texture, glm::uvec3(360, 180, 1));

            }
        );

        BaseModule::TextureManager.release(t);

        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
        _textureFile->setCallback(
            [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
        );
    }
}

} // namespace openspace
