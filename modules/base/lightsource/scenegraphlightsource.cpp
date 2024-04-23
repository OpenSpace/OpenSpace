/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/misc/profiling.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo IntensityInfo = {
        "Intensity",
        "Intensity",
        "The intensity of this light source.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo NodeCameraStateInfo = {
        "Node",
        "Node",
        "The identifier of the scene graph node to follow.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(SceneGraphLightSource)]] Parameters {
        // [[codegen::verbatim(IntensityInfo.description)]]
        std::optional<float> intensity;

        // [[codegen::verbatim(NodeCameraStateInfo.description)]]
        std::string node [[codegen::identifier()]];
    };
#include "scenegraphlightsource_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SceneGraphLightSource::Documentation() {
    return codegen::doc<Parameters>("base_scene_graph_light_source");
}

SceneGraphLightSource::SceneGraphLightSource()
    : _intensity(IntensityInfo, 1.f, 0.f, 1.f)
    , _sceneGraphNodeReference(NodeCameraStateInfo, "")
{
    addProperty(_intensity);
    _sceneGraphNodeReference.onChange([this]() {
        _sceneGraphNode =
            global::renderEngine->scene()->sceneGraphNode(_sceneGraphNodeReference);
    });
    addProperty(_sceneGraphNodeReference);
}

SceneGraphLightSource::SceneGraphLightSource(const ghoul::Dictionary& dictionary)
    : SceneGraphLightSource()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _intensity = p.intensity.value_or(_intensity);
    _sceneGraphNodeReference = p.node;
}

bool SceneGraphLightSource::initialize() {
    ZoneScoped;

    _sceneGraphNode =
        global::renderEngine->scene()->sceneGraphNode(_sceneGraphNodeReference);
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

    const glm::dvec3 viewSpace = glm::dvec3(renderData.camera.combinedViewMatrix() *
        glm::dvec4((lightPosition - renderNodePosition), 1.0));

    return glm::normalize(viewSpace);
}

} // namespace openspace
