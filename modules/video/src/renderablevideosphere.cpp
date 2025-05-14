/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/documentation/verifier.h>
#include <openspace/util/sphere.h>

namespace openspace {

documentation::Documentation RenderableVideoSphere::Documentation() {
    documentation::Documentation doc = RenderableSphere::Documentation();
    doc.name = "RenderableVideoSphere";
    doc.id = "video_renderablevideosphere";

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

void RenderableVideoSphere::update(const UpdateData&) {
    if (!_videoPlayer.isInitialized()) {
        return;
    }

    _videoPlayer.update();
}

void RenderableVideoSphere::bindTexture() {
    _videoPlayer.frameTexture()->bind();
}

} // namespace openspace
