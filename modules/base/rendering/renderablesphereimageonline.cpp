/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/base/rendering/renderablesphereimageonline.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/sphere.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "URL",
        "Image URL",
        "A URL to an image to use as a texture for this sphere. The image is expected "
        "to be an equirectangular projection.",
        openspace::properties::Property::Visibility::User
    };

    std::future<openspace::DownloadManager::MemoryFile> downloadImageToMemory(
                                                                   const std::string& url)
    {
        using namespace openspace;

        return global::downloadManager->fetchFile(
            url,
            [url](const DownloadManager::MemoryFile&) {
                LDEBUGC(
                    "RenderableSphereImageOnline",
                    std::format("Download to memory finished for image '{}'", url)
                );
            },
            [url](const std::string& err) {
                LDEBUGC(
                    "RenderableSphereImageOnline",
                    std::format("Download to memory failed for image '{}': {}", url, err)
                );
            }
        );
    }

    // A RenderableSphereImageOnline can be used to show an image from an online source
    // (as a URL) on a sphere in the OpenSpace scene. The image should be provided in an
    // equirectangular projection, if it is a map that is draped over the sphere.
    struct [[codegen::Dictionary(RenderableSphereImageOnline)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::string url [[codegen::key("URL")]];
    };
#include "renderablesphereimageonline_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphereImageOnline::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_sphere_image_online",
        RenderableSphere::Documentation()
    );
}

RenderableSphereImageOnline::RenderableSphereImageOnline(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _textureUrl(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _textureUrl = p.url;
    _textureUrl.onChange([this]() {
        _textureIsDirty = true;
    });
    addProperty(_textureUrl);
}

void RenderableSphereImageOnline::deinitializeGL() {
    _texture = nullptr;

    RenderableSphere::deinitializeGL();
}

void RenderableSphereImageOnline::update(const UpdateData& data) {
    RenderableSphere::update(data);

    if (!_textureIsDirty) {
        return;
    }

    if (!_imageFuture.valid()) {
        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
            _textureUrl
        );
        if (future.valid()) {
            _imageFuture = std::move(future);
        }
    }

    if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();

        if (imageFile.corrupted) {
            LERRORC(
                "RenderableSphereImageOnline",
                std::format("Error loading image from URL '{}'", _textureUrl.value())
            );
            return;
        }

        try {
            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(
                    reinterpret_cast<void*>(imageFile.buffer),
                    imageFile.size,
                    2,
                    imageFile.format
                );

            if (texture) {
                // Images don't need to start on 4-byte boundaries, for example if the
                // image is only RGB
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                _texture = std::move(texture);
                _textureIsDirty = false;
            }
        }
        catch (const ghoul::io::TextureReader::InvalidLoadException& e) {
            _textureIsDirty = false;
            LERRORC(e.component, e.message);
        }
    }
}

void RenderableSphereImageOnline::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
    else {
        unbindTexture();
    }
}

} // namespace openspace
