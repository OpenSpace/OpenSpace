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
#include <openspace/util/spicemanager.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo IntensityInfo = {
        "Intensity",
        "Intensity",
        "The intensity of this light source"
    };

    constexpr const openspace::properties::Property::PropertyInfo ReferenceInfo = {
        "Reference",
        "Reference",
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
                IntensityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                IntensityInfo.description
            },
            {
                ReferenceInfo.identifier,
                new StringVerifier,
                Optional::No,
                ReferenceInfo.description
            },
        }
    };
}

SceneGraphLightSource::SceneGraphLightSource()
    : LightSource()
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
    , _sceneGraphNodeReference(IntensityInfo, "")
{
    addProperty(_intensity);
    addProperty(_sceneGraphNodeReference);
}

SceneGraphLightSource::SceneGraphLightSource(const ghoul::Dictionary& dictionary)
    : LightSource(dictionary)
    , _intensity(IntensityInfo, 1.f, 0.f, 1.f)
    , _sceneGraphNodeReference(ReferenceInfo, "")
{
    addProperty(_intensity);
    addProperty(_sceneGraphNodeReference);
 
    documentation::testSpecificationAndThrow(Documentation(),
                                             dictionary,
                                             "SceneGraphLightSource");


    if (dictionary.hasValue<std::string>(IntensityInfo.identifier)) {
        _intensity = static_cast<float>(
            dictionary.value<double>(IntensityInfo.identifier)
        );
    }

    if (dictionary.hasValue<std::string>(ReferenceInfo.identifier)) {
        _sceneGraphNodeReference =
            dictionary.value<std::string>(ReferenceInfo.identifier);
    }
}

bool SceneGraphLightSource::initialize() {
    // Set up pointer to real scene graph node.
    return true;
}

float SceneGraphLightSource::intensity() const {
    return _intensity;
}

glm::vec3 SceneGraphLightSource::positionRelativeTo(
    const SceneGraphNode& node,
    const RenderData& renderData) const
{
    return glm::vec3(0.0);
}

} // namespace openspace
