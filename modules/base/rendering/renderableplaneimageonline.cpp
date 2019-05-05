/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/rendering/renderableplaneimageonline.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "URL",
        "Image URL",
        "Sets the URL of the texture that is displayed on this screen space plane. If "
        "this value is changed, the image at the new path will automatically be loaded "
        "and displayed."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneImageOnline::Documentation() {
    using namespace documentation;
    return {
        "Renderable Plane Image Online",
        "base_renderable_plane_image_online",
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

RenderablePlaneImageOnline::RenderablePlaneImageOnline(
                                                      const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlaneImageOnline"
    );

    _texturePath.onChange([this]() { _textureIsDirty = true; });
    addProperty(_texturePath);

    std::string texturePath;
    if (dictionary.hasKey(TextureInfo.identifier)) {
        _texturePath = dictionary.value<std::string>(TextureInfo.identifier);
    }

    addProperty(_texturePath);
}

void RenderablePlaneImageOnline::deinitializeGL() {
    _texture = nullptr;

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageOnline::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
    else {
        glBindTexture(GL_TEXTURE_2D, 0);
    }
}

void RenderablePlaneImageOnline::update(const UpdateData&) {
    if (_textureIsDirty) {
        if (!_imageFuture.valid()) {
            std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
                _texturePath
            );
            if (future.valid()) {
                _imageFuture = std::move(future);
            }
        }

        if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
            DownloadManager::MemoryFile imageFile = _imageFuture.get();

            if (imageFile.corrupted) {
                LERRORC(
                    "ScreenSpaceImageOnline",
                    fmt::format("Error loading image from URL '{}'", _texturePath)
                );
                return;
            }

            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(
                    reinterpret_cast<void*>(imageFile.buffer),
                    imageFile.size,
                    imageFile.format
                );

            if (texture) {
                // Images don't need to start on 4-byte boundaries, for example if the
                // image is only RGB
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

                texture->uploadTexture();

                // Textures of planets looks much smoother with AnisotropicMipMap rather
                // than linear
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

                _texture = std::move(texture);
                _textureIsDirty = false;
            }
        }
    }
}

std::future<DownloadManager::MemoryFile>
RenderablePlaneImageOnline::downloadImageToMemory(const std::string& url)
{
    return global::downloadManager.fetchFile(
        url,
        [url](const DownloadManager::MemoryFile&) {
            LDEBUGC(
                "ScreenSpaceImageOnline",
                "Download to memory finished for screen space image"
            );
        },
        [url](const std::string& err) {
            LDEBUGC(
                "ScreenSpaceImageOnline",
                "Download to memory failer for screen space image: " + err
            );
        }
    );
}

} // namespace openspace
