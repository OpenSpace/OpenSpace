/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

namespace openspace {
namespace globebrowsing {

DistanceSwitch::~DistanceSwitch() {}

bool DistanceSwitch::initialize() {
    _objectScale = 1.0;
    for (int i = 0; i < _renderables.size(); ++i) {
        _renderables[i]->initialize();
    }
    return true;
}

bool DistanceSwitch::deinitialize() {
    for (int i = 0; i < _renderables.size(); ++i) {
        _renderables[i]->deinitialize();
    }
    return true;
}


void DistanceSwitch::render(const RenderData& data) {
    if (_maxDistances.size() == 0) {
        return;
    }

    pss pssDistanceToCamera = (data.camera.position() - data.position).length();
    double distanceToCamera = pssDistanceToCamera.lengthd();

    if (distanceToCamera > _maxDistances.back() * _objectScale) {
        return;
    }

    // linear search through nodes to find which Renderable to render
    for (int i = 0; i < _renderables.size(); ++i) {
        if (distanceToCamera < _maxDistances[i] * _objectScale) {
            _renderables[i]->render(data);
            return;
        }
    }
}

void DistanceSwitch::update(const UpdateData& data) {
    _objectScale = data.modelTransform.scale;
    for (int i = 0; i < _renderables.size(); ++i) {
        _renderables[i]->update(data);
    }
}

void DistanceSwitch::addSwitchValue(std::shared_ptr<Renderable> renderable,
                                    double maxDistance)
{
    ghoul_assert(maxDistance > 0, "Renderable must have a positive maxDistance");
    if (_maxDistances.size() > 0) {
        ghoul_assert(maxDistance > _maxDistances.back(),
            "Renderables must be inserted in ascending order wrt distance");
    }
    _renderables.push_back(renderable);
    _maxDistances.push_back(maxDistance);
}

} // namespace globebrowsing
} // namespace openspace
