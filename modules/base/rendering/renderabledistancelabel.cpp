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

#include <modules/base/rendering/renderabledistancelabel.h>

#include <modules/base/rendering/renderablenodeline.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/logging/logmanager.h>
#include <optional>
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "RenderableDistanceLabel";

    constexpr openspace::properties::Property::PropertyInfo NodeLineInfo = {
        "NodeLine",
        "Node Line",
        "The identifier of a scene graph node with a RenderableNodeLine that this label "
        "should track. The label text will be updating based on the distance from the "
        "node line's start and end.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceUnitInfo = {
        "DistanceUnit",
        "Distance Unit",
        "The unit in which the distance value should be displayed. Defaults to 'km' if "
        "not specified.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CustomUnitDescriptorInfo = {
        "CustomUnitDescriptor",
        "Custom Unit Descriptor",
        "Property to define a custom unit descriptor to use to describe the distance "
        "value. Defaults to the selected unit's SI descriptor if not specified.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `Renderable` creates a label that shows the distance between two nodes, based
    // on an existing [RenderableNodeLine](#base_renderable_nodeline). The label
    // will be placed halfway between the two scene graph nodes that the line connects.
    //
    // The unit in which the distance is displayed can be customized.
    struct [[codegen::Dictionary(RenderableDistanceLabel)]] Parameters {
        // The identifier of a scene graph node with a
        // [RenderableNodeLine](#base_renderable_nodeline) that this label
        // should track. The label text will be updating based on the distance from the
        // node line's start and end.
        std::string nodeLine;

        // [[codegen::verbatim(DistanceUnitInfo.description)]]
        std::optional<std::string> distanceUnit
            [[codegen::inlist(openspace::distanceUnitList())]];

        // [[codegen::verbatim(CustomUnitDescriptorInfo.description)]]
        std::optional<std::string> customUnitDescriptor;
    };
#include "renderabledistancelabel_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableDistanceLabel::Documentation() {
    return codegen::doc<Parameters>("base_renderable_distancelabel");
}

RenderableDistanceLabel::RenderableDistanceLabel(const ghoul::Dictionary& dictionary)
    : RenderableLabel(dictionary)
    , _nodelineId(NodeLineInfo)
    , _distanceUnit(DistanceUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _customUnitDescriptor(CustomUnitDescriptorInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _nodelineId = p.nodeLine;
    addProperty(_nodelineId);

    for (const DistanceUnit u : DistanceUnits) {
        _distanceUnit.addOption(
            static_cast<int>(u),
            std::string(nameForDistanceUnit(u))
        );
    }
    _distanceUnit = static_cast<int>(DistanceUnit::Kilometer);
    if (p.distanceUnit.has_value()) {
        const DistanceUnit unit = distanceUnitFromString(*p.distanceUnit);
        _distanceUnit = static_cast<int>(unit);
    }
    addProperty(_distanceUnit);

    _customUnitDescriptor = p.customUnitDescriptor.value_or(_customUnitDescriptor);
    addProperty(_customUnitDescriptor);
}

void RenderableDistanceLabel::update(const UpdateData&) {
    if (_errorThrown) {
        return;
    }

    RenderEngine& RE = *global::renderEngine;

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

        const DistanceUnit unit = static_cast<DistanceUnit>(_distanceUnit.value());

        // Get unit descriptor text
        std::string_view unitDescriptor = abbreviationForDistanceUnit(unit);
        if (!_customUnitDescriptor.value().empty()) {
            unitDescriptor = _customUnitDescriptor;
        }

        // Get distance as string and remove fractional part
        const double convertedDistance = convertMeters(nodeline->distance(), unit);
        std::string distanceText = std::to_string(
            std::round(convertedDistance)
        );
        const int pos = static_cast<int>(distanceText.find('.'));
        const std::string subStr = distanceText.substr(pos);
        distanceText.erase(pos, subStr.size());

        // Create final label text and set it
        const std::string finalText = std::format("{} {}", distanceText, unitDescriptor);
        setLabelText(finalText);

        // Update placement of label with transformation matrix
        SceneGraphNode* startNode = RE.scene()->sceneGraphNode(nodeline->start());
        SceneGraphNode* endNode = RE.scene()->sceneGraphNode(nodeline->end());
        if (startNode && endNode) {
            const glm::dvec3 start = startNode->worldPosition();
            const glm::dvec3 end = endNode->worldPosition();
            const glm::dvec3 goalPos = start + (end - start) / 2.0;
            _transformationMatrix = glm::translate(glm::dmat4(1.0), goalPos);
        }
        else {
            LERROR(std::format(
                "Could not find scene graph node '{}' or '{}'",
                nodeline->start(), nodeline->end()
            ));
        }
    }
    else {
        LERROR(std::format(
            "There is no scenegraph node with id {}", _nodelineId.value()
        ));
        _errorThrown = true;
    }
}

} // namespace openspace

