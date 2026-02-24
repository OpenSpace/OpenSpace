/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/base/rendering/screenspaceimageonline.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>
#include <utility>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "ScreenSpaceImageOnline";

    constexpr Property::PropertyInfo TextureInfo = {
        "URL",
        "Image URL",
        "The URL of the texture to be displayed on this screen space plane. If changed, "
        "the image at the new path will automatically be loaded and displayed. The "
        "default size of the plane will be set based on the size of the image.",
        Property::Visibility::User
    };

    // This `ScreenSpaceRenderable` can be used to display an image from a web URL.
    //
    // To load an image from a local file on disk, see
    // [`ScreenSpaceImageLocal`](#base_screenspace_image_local).
    struct [[codegen::Dictionary(ScreenSpaceImageOnline)]] Parameters {
        std::optional<std::string> identifier;

        // [[codegen::verbatim(TextureInfo.description)]]
        std::optional<std::string> url [[codegen::key("URL")]];
    };
} // namespace
#include "screenspaceimageonline_codegen.cpp"

namespace openspace {

Documentation ScreenSpaceImageOnline::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_image_online");
}

ScreenSpaceImageOnline::ScreenSpaceImageOnline(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _textureIsDirty(false)
    , _texturePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceImageOnline");
    setIdentifier(makeUniqueIdentifier(std::move(identifier)));

    _texturePath.onChange([this]() { _textureIsDirty = true; });
    _texturePath = p.url.value_or(_texturePath);
    addProperty(_texturePath);
}

ScreenSpaceImageOnline::~ScreenSpaceImageOnline() {}

void ScreenSpaceImageOnline::deinitializeGL() {
    _texture = nullptr;

    ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceImageOnline::update() {
    if (!_textureIsDirty) [[likely]] {
        return;
    }

    if (!_imageFuture.valid()) {
        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
            _texturePath
        );
        if (future.valid()) {
            _imageFuture = std::move(future);
        }
    }

    if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();

        if (imageFile.corrupted) {
            LERROR(std::format(
                "Error loading image from URL '{}'", _texturePath.value()
            ));
            return;
        }

        try {
            // @TODO (2026-02-18, abock): This code was settings the swizzle mask only if
            //                            the returned image was having a single Red
            //                            channel. This can't currently be expressed
            //                            unfortunately
            ghoul::opengl::Texture::SamplerInit samplerInit = {
                // TODO: AnisotropicMipMap crashes on ATI cards ---abock
                //.filter = ghoul::opengl::Texture::FilterMode::AnisotropicMipMap,
                .filter = ghoul::opengl::Texture::FilterMode::LinearMipMap,
                //.swizzleMask = std::array<GLenum, 4>{ GL_RED, GL_RED, GL_RED, GL_ONE }
            };

            _texture = ghoul::io::TextureReader::ref().loadTexture(
                reinterpret_cast<void*>(imageFile.buffer),
                imageFile.size,
                2,
                samplerInit,
                imageFile.format
            );

            _objectSize = _texture->dimensions();
            _textureIsDirty = false;
        }
        catch (const ghoul::io::TextureReader::InvalidLoadException& e) {
            _textureIsDirty = false;
            LERRORC(e.component, e.message);
        }
    }
}

std::future<DownloadManager::MemoryFile> ScreenSpaceImageOnline::downloadImageToMemory(
                                                                   const std::string& url)
{
    return global::downloadManager->fetchFile(
        url,
        [](const DownloadManager::MemoryFile&) {
            LDEBUG("Download to memory finished for screen space image");
        },
        [](const std::string& err) {
            LDEBUG(std::format(
                "Download to memory failed for screen space image: {}", err
            ));
        }
    );
}

void ScreenSpaceImageOnline::bindTexture(ghoul::opengl::TextureUnit& unit) {
    if (_texture) [[likely]] {
        unit.bind(*_texture);
    }
}

} // namespace openspace
