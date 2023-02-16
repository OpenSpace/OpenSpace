/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/video/include/screenspacevideo.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <filesystem>
#include <optional>

namespace {
    struct [[codegen::Dictionary(ScreenSpaceVideo)]] Parameters {

    };
#include "screenspacevideo_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceVideo::Documentation() {
    return codegen::doc<Parameters>("screenspace_video");
}

ScreenSpaceVideo::ScreenSpaceVideo(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _videoPlayer(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // @TODO (abock, 2021-02-02) Should this be the name variable? The identifier wasn't
    // declared in the documentation
    std::string identifier;
    if (dictionary.hasValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceVideo";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(identifier);

    addProperty(_videoPlayer._reset);
    addProperty(_videoPlayer._playAudio);

    if (_videoPlayer.playbackMode() == PlaybackMode::RealTimeLoop) {
        // Video interaction. Only valid for real time looping
        addProperty(_videoPlayer._play);
        addProperty(_videoPlayer._pause);
        addProperty(_videoPlayer._goToStart);
    }
}

void ScreenSpaceVideo::update() {
    if (!_videoPlayer.isInitialized()) {
        return;
    }
    glm::uvec3 texDimensions = _videoPlayer.frameTexture()->dimensions();
    if (_objectSize != glm::ivec2(texDimensions.x, texDimensions.y)) {
        _objectSize = _videoPlayer.frameTexture()->dimensions();
    }
}

bool ScreenSpaceVideo::deinitializeGL() {
    _videoPlayer.destroy();

    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceVideo::bindTexture() {
    _videoPlayer.frameTexture()->bind();
}

} // namespace openspace
