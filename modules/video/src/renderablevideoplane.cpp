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

#include <modules/video/include/renderablevideoplane.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace openspace {

RenderableVideoPlane::RenderableVideoPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _videoPlayer(dictionary)
{
    addPropertySubOwner(_videoPlayer);
}

void RenderableVideoPlane::initializeGL() {
    RenderablePlane::initializeGL();
    _videoPlayer.initialize();
}

void RenderableVideoPlane::deinitializeGL() {
    _videoPlayer.destroy();
    RenderablePlane::deinitializeGL();
}

bool RenderableVideoPlane::isReady() const {
    return RenderablePlane::isReady() && _videoPlayer.isInitialized();
}

void RenderableVideoPlane::render(const RenderData& data, RendererTasks& rendererTask) {
    if (_videoPlayer.isInitialized()) {
        RenderablePlane::render(data, rendererTask);
    }
}

void RenderableVideoPlane::update(const UpdateData& data) {
    _videoPlayer.update();

    if (!_videoPlayer.isInitialized()) {
        return;
    }

    // Shape the vidoe based on the aspect ration of the film
    glm::vec2 textureDim = glm::vec2(_videoPlayer.frameTexture()->dimensions());
    if (_textureDimensions != textureDim) {
        float aspectRatio = textureDim.x / textureDim.y;
        float planeAspectRatio = _size.value().x / _size.value().y;

        if (std::abs(planeAspectRatio - aspectRatio) >
            std::numeric_limits<float>::epsilon())
        {
            glm::vec2 newSize =
                aspectRatio > 0.f ?
                glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                glm::vec2(_size.value().x, _size.value().y * aspectRatio);
            _size = newSize;
        }

        _textureDimensions = textureDim;
    }

    RenderablePlane::update(data);
}

void RenderableVideoPlane::bindTexture() {
    _videoPlayer.frameTexture()->bind();
}

} // namespace openspace
