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

#include <modules/base/lightsource/scenegraphlightsource.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo IntensityInfo = {
        "Intensity",
        "Intensity",
        "The intensity of this light source"
    };

    constexpr openspace::properties::Property::PropertyInfo NodeInfo = {
        "Node",
        "Node",
        "The identifier of the scene graph node to follow"
    };
} // namespace

namespace openspace {

documentation::Documentation SceneGraphLightSource::Documentation() {
    using namespace openspace::documentation;
    return {
        "Scene Graph Light Source",
        "base_scene_graph_light_source",
        {
            {
                "Type",
                new StringEqualVerifier("SceneGraphLightSource"),
                Optional::No,
                "The type of this light source"
            },
            {
                IntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                IntensityInfo.description
            },
            {
                NodeInfo.identifier,
                new StringVerifier,
                Optional::No,
                NodeInfo.description
            },
        }
    };
}

SceneGraphLightSource::SceneGraphLightSource()
    : LightSource()
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
    , _sceneGraphNodeReference(NodeInfo, "")
{
    addProperty(_intensity);
    addProperty(_sceneGraphNodeReference);
}

SceneGraphLightSource::SceneGraphLightSource(const ghoul::Dictionary& dictionary)
    : LightSource(dictionary)
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
    , _sceneGraphNodeReference(NodeInfo, "")
{
    addProperty(_intensity);
    addProperty(_sceneGraphNodeReference);

    documentation::testSpecificationAndThrow(Documentation(),
                                             dictionary,
                                             "SceneGraphLightSource");


    if (dictionary.hasValue<double>(IntensityInfo.identifier)) {
        _intensity = static_cast<float>(
            dictionary.value<double>(IntensityInfo.identifier)
        );
    }

    if (dictionary.hasValue<std::string>(NodeInfo.identifier)) {
        _sceneGraphNodeReference =
            dictionary.value<std::string>(NodeInfo.identifier);
    }

    _sceneGraphNodeReference.onChange([this]() {
        _sceneGraphNode =
            global::renderEngine.scene()->sceneGraphNode(_sceneGraphNodeReference);
    });

}

bool SceneGraphLightSource::initialize() {
    _sceneGraphNode =
        global::renderEngine.scene()->sceneGraphNode(_sceneGraphNodeReference);
    return _sceneGraphNode != nullptr;
}

float SceneGraphLightSource::intensity() const {
    return _intensity;
}

glm::vec3 SceneGraphLightSource::directionViewSpace(const RenderData& renderData) const {
    if (!_sceneGraphNode) {
        return glm::vec3(0.f);
    }

    const glm::dvec3 lightPosition =
        _sceneGraphNode->modelTransform() * glm::dvec4(0.0, 0.0, 0.0, 1.0);

    const glm::dvec3 renderNodePosition = renderData.modelTransform.translation;

    const glm::dvec3 lightDirectionViewSpace = renderData.camera.viewRotationMatrix() *
        glm::dvec4((lightPosition - renderNodePosition), 1.0);

    return glm::normalize(lightDirectionViewSpace);
}


} // namespace openspace
