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

#include <modules/base/rendering/screenspaceimagelocal.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TexturePathInfo = {
        "TexturePath",
        "Texture path",
        "Sets the path of the texture that is displayed on this screen space plane. If "
        "this value is changed, the image at the new path will automatically be loaded "
        "and displayed. The size of the image will also automatically set the default "
        "size of this plane."
    };
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceImageLocal::Documentation() {
    using namespace openspace::documentation;
    return {
        "ScreenSpace Local Image",
        "base_screenspace_image_local",
        {
            {
                KeyName,
                new StringVerifier,
                Optional::Yes,
                "Specifies the GUI name of the ScreenspaceImage"
            },
            {
                TexturePathInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                TexturePathInfo.description
            }
        }
    };
}

ScreenSpaceImageLocal::ScreenSpaceImageLocal(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _texturePath(TexturePathInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceImageLocal"
    );

    std::string identifier;
    if (dictionary.hasKeyAndValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceImageLocal";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(identifier);

    _texturePath.onChange([this]() {
        if (!FileSys.fileExists(FileSys.absolutePath(_texturePath))) {
            LWARNINGC(
                "ScreenSpaceImageLocal",
                fmt::format("Image {} did not exist for {}", _texturePath, _identifier)
            );
        }
        else {
            _textureIsDirty = true;
        }
    });
    addProperty(_texturePath);

    if (dictionary.hasKey(TexturePathInfo.identifier)) {
        std::string path = dictionary.value<std::string>(TexturePathInfo.identifier);
        if (!FileSys.fileExists(FileSys.absolutePath(path))) {
            LWARNINGC(
                "ScreenSpaceImageLocal",
                fmt::format("Image {} did not exist for {}", path, _identifier)
            );
        }
        else {
            _texturePath = path;
        }
    }
}

bool ScreenSpaceImageLocal::deinitializeGL() {
    _texture = nullptr;

    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceImageLocal::update() {
    if (_textureIsDirty && !_texturePath.value().empty()) {
        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));

        if (texture) {
            // Images don't need to start on 4-byte boundaries, for example if the
            // image is only RGB
            glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than
            // linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

            _texture = std::move(texture);
            _objectSize = _texture->dimensions();
            _textureIsDirty = false;
        }
    }
}

void ScreenSpaceImageLocal::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
}

} // namespace openspace
