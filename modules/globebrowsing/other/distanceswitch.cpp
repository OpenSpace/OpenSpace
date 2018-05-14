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

#include <modules/globebrowsing/other/distanceswitch.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/updatestructures.h>

namespace openspace::globebrowsing {

DistanceSwitch::~DistanceSwitch() {}

void DistanceSwitch::initialize() {
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        renderable->initialize();
    }
}

void DistanceSwitch::initializeGL() {
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        renderable->initializeGL();
    }
}

void DistanceSwitch::deinitialize() {
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        renderable->deinitialize();
    }
}

void DistanceSwitch::deinitializeGL() {
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        renderable->deinitializeGL();
    }
}

void DistanceSwitch::render(const RenderData& data, RendererTasks& tasks) {
    const double distanceToCamera = distance(
        data.camera.positionVec3(),
        data.modelTransform.translation
    );

    // This distance will be enough to render the globe as one pixel if the field of
    // view is 'fov' radians and the screen resolution is 'res' pixels.
    const double fov = 2 * glm::pi<double>() / 6; // 60 degrees
    const int res = 2880;

    // linear search through nodes to find which Renderable to render
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        const double distance = res * renderable->boundingSphere() / tan(fov / 2);
        if (distanceToCamera < distance) {
            renderable->render(data, tasks);
        }
    }
}

void DistanceSwitch::update(const UpdateData& data) {
    for (const std::shared_ptr<Renderable>& renderable : _renderables) {
        renderable->update(data);
    }
}

void DistanceSwitch::addSwitchValue(std::shared_ptr<Renderable> renderable) {
    _renderables.push_back(std::move(renderable));
}

} // namespace openspace::globebrowsing
