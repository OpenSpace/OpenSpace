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

#include <modules/globebrowsing/src/tileprovider/spoutimageprovider.h>

#include <modules/globebrowsing/src/tileindex.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <limits>
#include <optional>

#ifdef OPENSPACE_HAS_SPOUT
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif // WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX
#define NOMINMAX
#endif // NOMINMAX
#include <modules/spout/spoutwrapper.h>
#endif

namespace {
    struct [[codegen::Dictionary(SpoutImageProvider)]] Parameters {
        std::optional<std::string> spoutName;
    };
#include "spoutimageprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation SpoutImageProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_spoutimageprovider");
}

SpoutImageProvider::SpoutImageProvider(
                                     [[maybe_unused]] const ghoul::Dictionary& dictionary)
{
    ZoneScoped;

#ifdef OPENSPACE_HAS_SPOUT
    spoutReceiver = std::make_unique<spout::SpoutReceiverPropertyProxy>(
        *this,
        dictionary
    );

    spoutReceiver->onUpdateTexture([this](int width, int height) {
        for (int i = 0; i < 2; i++) {
            if (!fbo[i]) {
                glCreateFramebuffers(1, &fbo[i]);
            }
            tileTexture[i].release();
            tileTexture[i] = std::make_unique<ghoul::opengl::Texture>(
                glm::uvec3(width / 2, height, 1),
                GL_TEXTURE_2D,
                ghoul::opengl::Texture::Format::RGBA,
                GL_RGBA,
                GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::FilterMode::Linear,
                ghoul::opengl::Texture::WrappingMode::Repeat,
                ghoul::opengl::Texture::AllocateData::No,
                ghoul::opengl::Texture::TakeOwnership::No
            );

            if (!tileTexture[i]) {
                return false;
            }
            tileTexture[i]->uploadTexture();
            tiles[i] = Tile{ tileTexture[i].get(), std::nullopt, Tile::Status::OK };
        }
        return true;
    });


    spoutReceiver->onReleaseTexture([this]() {
        for (int i = 0; i < 2; i++) {
            if (fbo[i]) {
                glDeleteFramebuffers(1, &fbo[i]);
            }
            tileTexture[i].release();
            fbo[i] = 0;
        }
    });


    spoutReceiver->onUpdateReceiver([this](int width, int height, GLuint texture) {
        glNamedFramebufferTexture(fbo[0], GL_COLOR_ATTACHMENT0, texture, 0);
        glNamedFramebufferReadBuffer(fbo[0], GL_COLOR_ATTACHMENT0);
        glNamedFramebufferTexture(fbo[1], GL_COLOR_ATTACHMENT1, *tileTexture[0], 0);
        glNamedFramebufferDrawBuffer(fbo[1], GL_COLOR_ATTACHMENT1);
        glBlitNamedFramebuffer(
            fbo[0],
            fbo[1],
            width / 2,
            0,
            width,
            height,
            0,
            0,
            width / 2,
            height,
            GL_COLOR_BUFFER_BIT,
            GL_NEAREST
        );

        glNamedFramebufferTexture(fbo[1], GL_COLOR_ATTACHMENT1, *tileTexture[1], 0);
        glBlitNamedFramebuffer(
            fbo[0],
            fbo[1],
            0,
            0,
            width / 2,
            height,
            0,
            0,
            width / 2,
            height,
            GL_COLOR_BUFFER_BIT,
            GL_NEAREST
        );
        return true;
    });
#endif

    reset();
}

void SpoutImageProvider::internalInitialize() {
    ZoneScoped;

#ifdef OPENSPACE_HAS_SPOUT
    spoutReceiver->updateReceiver();
#endif // OPENSPACE_HAS_SPOUT
}

void SpoutImageProvider::internalDeinitialize() {
#ifdef OPENSPACE_HAS_SPOUT
    spoutReceiver->release();
#endif // OPENSPACE_HAS_SPOUT
}

Tile SpoutImageProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped;

    _spoutUpdate = true;
    return tiles[tileIndex.x];
}

Tile::Status SpoutImageProvider::tileStatus(const TileIndex&) {
    return Tile::Status::OK;
}

TileDepthTransform SpoutImageProvider::depthTransform() {
    return { 0.f, 1.f };
}

void SpoutImageProvider::update() {
#ifdef OPENSPACE_HAS_SPOUT
    if (!_spoutUpdate) {
        return;
    }

    if (!spoutReceiver->isCreated()) {
        reset();
        if (!spoutReceiver->isCreated()) {
            return;
        }
    }

    spoutReceiver->updateReceiver();
#endif // OPENSPACE_HAS_SPOUT
}

void SpoutImageProvider::reset() {
#ifdef OPENSPACE_HAS_SPOUT
    spoutReceiver->updateReceiver();
#endif // OPENSPACE_HAS_SPOUT
}

int SpoutImageProvider::minLevel() {
    return 0;
}

int SpoutImageProvider::maxLevel() {
    return 1;
}

float SpoutImageProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
