/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/vislab/rendering/renderabledistancelabel.h>

#include <modules/base/rendering/renderablenodeline.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderableDistanceLabel";

    constexpr openspace::properties::Property::PropertyInfo NodeLineInfo = {
        "NodeLine",
        "Node Line",
        "Property to track a nodeline. When tracking the label text will be updating the "
        "distance from the nodeline start and end."
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceUnitInfo = {
        "DistanceUnit",
        "Distance Unit",
        "Property to define the unit in which the distance should be displayed."
        "Defaults to 'km' if not specified."
    };

    constexpr openspace::properties::Property::PropertyInfo CustomUnitDescriptorInfo = {
        "CustomUnitDescriptor",
        "Custom Unit Descriptor",
        "Property to define a custom unit descriptor to use to describe the distance "
        "value. Defaults to the units SI descriptor if not specified."
    };
}

namespace openspace {

documentation::Documentation RenderableDistanceLabel::Documentation() {
    using namespace documentation;
    return {
        "Renderable Distance Label",
        "vislab_renderable_distance_label",
        {
            {
                NodeLineInfo.identifier,
                new StringVerifier,
                Optional::No,
                NodeLineInfo.description
            },
            {
                DistanceUnitInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                DistanceUnitInfo.description
            },
            {
                CustomUnitDescriptorInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                CustomUnitDescriptorInfo.description
            }
        }
    };
}

RenderableDistanceLabel::RenderableDistanceLabel(const ghoul::Dictionary& dictionary)
    : RenderableLabels(dictionary)
    , _nodelineId(NodeLineInfo)
    , _distanceUnit(DistanceUnitInfo, 1, 0, 11)
    , _customUnitDescriptor(CustomUnitDescriptorInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableDistanceLabel"
    );

    if (dictionary.hasKey(NodeLineInfo.identifier)) {
        _nodelineId = dictionary.value<std::string>(NodeLineInfo.identifier);
        addProperty(_nodelineId);
    }
    if (dictionary.hasKey(DistanceUnitInfo.identifier)) {
        _distanceUnit = static_cast<int>(
            dictionary.value<double>(DistanceUnitInfo.identifier)
        );
        addProperty(_distanceUnit);
    }
    if (dictionary.hasKey(CustomUnitDescriptorInfo.identifier)) {
        _customUnitDescriptor =
            dictionary.value<std::string>(CustomUnitDescriptorInfo.identifier);
        addProperty(_customUnitDescriptor);
    }
}

void RenderableDistanceLabel::update(const UpdateData&) {
    if (_errorThrown) {
        return;
    }

    RenderEngine& RE = global::renderEngine;

    SceneGraphNode* nodelineNode = RE.scene()->sceneGraphNode(_nodelineId);
    if (nodelineNode) {
        RenderableNodeLine* nodeline = dynamic_cast<RenderableNodeLine*>(
            nodelineNode->renderable()
        );
        if (!nodeline) {
            LERROR("Expected renderable to be of type 'RenderableNodeLine'");
            _errorThrown = true;
            return;
        }

        // Get used unit scale
        const float scale = unit(_distanceUnit);

        // Get unit descriptor text
        std::string unitDescriptor = toString(_distanceUnit);
        if (!_customUnitDescriptor.value().empty()) {
            unitDescriptor = _customUnitDescriptor.value();
        }

        // Get distance as string and remove fractional part
        std::string distanceText = std::to_string(
            std::round(nodeline->distance() / scale)
        );
        int pos = static_cast<int>(distanceText.find("."));
        std::string subStr = distanceText.substr(pos);
        distanceText.erase(pos, subStr.size());

        // Create final label text and set it
        const std::string finalText = distanceText + " " + unitDescriptor;
        setLabelText(finalText);

        // Update placement of label with transformation matrix
        SceneGraphNode* startNode = RE.scene()->sceneGraphNode(nodeline->start());
        glm::dvec3 start = startNode->worldPosition();
        SceneGraphNode* endNode = RE.scene()->sceneGraphNode(nodeline->end());
        glm::dvec3 end = endNode->worldPosition();
        glm::dvec3 goalPos = start + (end - start) / 2.0;
        _transformationMatrix = glm::translate(glm::dmat4(1.0), goalPos);
    }
    else {
        LERROR(fmt::format("There is no scenegraph node with id {}", _nodelineId));
        _errorThrown = true;
    }
}

} // namespace openspace

