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

#include <modules/base/rendering/renderableswitch.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo AutoScaleInfo = {
        "AutoScale",
        "Auto Scale",
        "When true, the plane will automatically adjust in size to match the aspect "
        "ratio of the content. Otherwise it will remain in the given size."
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "DistanceThreshold",
        "Distance Threshold",
        "Threshold for when the switch happens between the two renderables. "
    };

    struct [[codegen::Dictionary(RenderableSwitch)]] Parameters {
        ghoul::Dictionary renderable1;
        ghoul::Dictionary renderable2;

        std::optional<float> distanceThreshold;
    };
#include "renderableswitch_codegen.cpp"
} // namespace

namespace openspace {

    documentation::Documentation RenderableSwitch::Documentation() {
        return codegen::doc<Parameters>(
            "base_renderable_switch"
        );
    }

    RenderableSwitch::RenderableSwitch(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary), _autoScale(AutoScaleInfo, false)
        , _distanceThreshold(DistanceThresholdInfo, 1.f, 0.f, 1e25f)
    {
        const Parameters p = codegen::bake<Parameters>(dictionary);

        _renderable1 = createFromDictionary(p.Renderable1);

        _renderable2 = createFromDictionary(p.Renderable2);

        _distanceThreshold = p.distanceThreshold.value_or(_distanceThreshold);
        addProperty(_distanceThreshold);

        addProperty(_autoScale);
    }

    void RenderableSwitch::initializeGL() {
        ghoul_assert(_renderable1, "No renderable1");
        ghoul_assert(_renderable2, "No renderable2");
        
        _renderable1->initializeGL();
        _renderable2->initializeGL();
    }

    void RenderableSwitch::deinitializeGL() {
        if (_renderable1) {
            _renderable1->deinitializeGL();
        }
        if (_renderable2) {
            _renderable2->deinitializeGL();
        }
    }

    bool RenderableSwitch::isReady() const {
        return (_renderable1->isReady() && _renderable2->isReady());
    }

    void RenderableSwitch::update(const UpdateData& data) {
        if (_renderable1) {
            _renderable1->update(data);
        }
        if (_renderable2) {
            _renderable2->update(data);
        }
    }

    void RenderableSwitch::render(const RenderData& data, RendererTasks& tasks) {
        if (!_enabled) {
            return;
        }
        glm::dvec3 cameraPosition = data.camera.positionVec3();
        glm::dvec3 modelPosition = data.modelTransform.translation;

        if (glm::distance(cameraPosition, modelPosition) < _distanceThreshold) {
            _renderable1->render(data, tasks);
        }
        else {
            _renderable2->render(data, tasks);
        }
    }
} // namespace openspace
