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

#include <modules/video/include/renderablevideosphere.h>

#include <openspace/documentation/documentation.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/dictionary.h>

namespace {
    // This `Renderable` creates a textured 3D sphere where the texture is a video. Per
    // default, the sphere uses an equirectangular projection for the image mapping
    // and hence expects a video in equirectangular format. However, it can also be used
    // to play fisheye videos by changing the `TextureProjection`.
    //
    // The video can either be played back based on a given simulation time
    // (`PlaybackMode` MapToSimulationTime) or through the user interface (for
    // `PlaybackMode` RealTimeLoop). It is also possible to control whether the video
    // should loop or just be played once.
    //
    // Note that, unless playback is mapped to simulation time, the video must be started
    // manually via the user interface.
    struct [[codegen::Dictionary(RenderableVideoSphere)]] Parameters {};
#include "renderablevideosphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableVideoSphere::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>(
        "video_renderablevideosphere",
        RenderableSphere::Documentation()
    );

    documentation::Documentation vp = VideoPlayer::Documentation();
    doc.entries.insert(doc.entries.end(), vp.entries.begin(), vp.entries.end());

    return doc;
}

RenderableVideoSphere::RenderableVideoSphere(const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _videoPlayer(dictionary)
{
    addPropertySubOwner(_videoPlayer);
}

bool RenderableVideoSphere::isReady() const {
    return RenderableSphere::isReady() && _videoPlayer.isInitialized();
}

void RenderableVideoSphere::initializeGL() {
    RenderableSphere::initializeGL();
    _videoPlayer.initialize();
}

void RenderableVideoSphere::deinitializeGL() {
    _videoPlayer.destroy();
    RenderableSphere::deinitializeGL();
}

void RenderableVideoSphere::render(const RenderData& data, RendererTasks& rendererTask) {
    if (_videoPlayer.isInitialized()) {
        RenderableSphere::render(data, rendererTask);
    }
}

void RenderableVideoSphere::update(const UpdateData& data) {
    RenderableSphere::update(data);
    if (!_videoPlayer.isInitialized()) {
        return;
    }

    _videoPlayer.update();
}

void RenderableVideoSphere::bindTexture() {
    _videoPlayer.frameTexture()->bind();
}

} // namespace openspace
