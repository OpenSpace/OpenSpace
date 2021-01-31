/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/base/dashboard/dashboarditemdistance.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SourceTypeInfo = {
        "SourceType",
        "Source Type",
        "The type of position that is used as the source to calculate the distance. The "
        "default value is 'Camera'."
    };

    constexpr openspace::properties::Property::PropertyInfo SourceNodeNameInfo = {
        "SourceNodeName",
        "Source Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the source for computing the distance."
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationTypeInfo = {
        "DestinationType",
        "Destination Type",
        "The type of position that is used as the destination to calculate the distance. "
        "The default value for this is 'Focus'."
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationNodeNameInfo = {
        "DestinationNodeName",
        "Destination Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the destination for computing the distance."
    };

    constexpr openspace::properties::Property::PropertyInfo SimplificationInfo = {
        "Simplification",
        "Simplification",
        "If this value is enabled, the distance is displayed in nuanced units, such as "
        "km, AU, light years, parsecs, etc. If this value is disabled, the unit can be "
        "explicitly requested."
    };

    constexpr openspace::properties::Property::PropertyInfo RequestedUnitInfo = {
        "RequestedUnit",
        "Requested Unit",
        "If the simplification is disabled, this distance unit is used as a destination "
        "to convert the meters into."
    };

    constexpr openspace::properties::Property::PropertyInfo FormatStringInfo = {
        "FormatString",
        "Format String",
        "The format string that is used for formatting the distance string.  This format "
        "receives four parameters:  The name of the source, the name of the destination "
        "the value of the distance and the unit of the distance"
    };

    std::vector<std::string> unitList() {
        std::vector<std::string> res(openspace::DistanceUnits.size());
        std::transform(
            openspace::DistanceUnits.begin(),
            openspace::DistanceUnits.end(),
            res.begin(),
            [](openspace::DistanceUnit unit) -> std::string {
                return nameForDistanceUnit(unit);
            }
        );
        return res;
    }

    struct [[codegen::Dictionary(DashboardItemDistance)]] Parameters {
        enum class TypeInfo {
            Node,
            NodeSurface [[codegen::key("Node Surface")]],
            Focus,
            Camera
        };

        // [[codegen::verbatim(SourceTypeInfo.description)]]
        std::optional<TypeInfo> sourceType;

        // [[codegen::verbatim(SourceNodeNameInfo.description)]]
        std::optional<std::string> sourceNodeName;

        // [[codegen::verbatim(DestinationTypeInfo.description)]]
        std::optional<TypeInfo> destinationType;

        // [[codegen::verbatim(DestinationNodeNameInfo.description)]]
        std::optional<std::string> destinationNodeName;

        // [[codegen::verbatim(SimplificationInfo.description)]]
        std::optional<bool> simplification;

        // [[codegen::verbatim(RequestedUnitInfo.description)]]
        std::optional<std::string> requestedUnit [[codegen::inlist(unitList())]];

        // [[codegen::verbatim(FormatStringInfo.description)]]
        std::optional<std::string> formatString;
    };
#include "dashboarditemdistance_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemDistance::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "base_dashboarditem_distance";
    return doc;
}

DashboardItemDistance::DashboardItemDistance(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _doSimplification(SimplificationInfo, true)
    , _requestedUnit(RequestedUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _formatString(FormatStringInfo, "Distance from {} to {}: {:f} {}")
    , _source{
        properties::OptionProperty(
            SourceTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(SourceNodeNameInfo),
        nullptr
    }
    , _destination{
        properties::OptionProperty(
            DestinationTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(DestinationNodeNameInfo),
        nullptr
    }
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _source.type.addOptions({
        { Type::Node, "Node" },
        { Type::NodeSurface, "Node Surface" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _source.type.onChange([this]() {
        _source.nodeName.setVisibility(
            properties::Property::Visibility(
                _source.type == Type::Node || _source.type == Type::NodeSurface
            )
        );
    });
    if (p.sourceType.has_value()) {
        switch (*p.sourceType) {
            case Parameters::TypeInfo::Node:
                _source.type = Type::Node;
                break;
            case Parameters::TypeInfo::NodeSurface:
                _source.type = Type::NodeSurface;
                break;
            case Parameters::TypeInfo::Focus:
                _source.type = Type::Focus;
                break;
            case Parameters::TypeInfo::Camera:
                _source.type = Type::Camera;
                break;
        }
    }
    else {
        _source.type = Type::Camera;
    }
    addProperty(_source.type);

    _source.nodeName.onChange([this]() { _source.node = nullptr; });
    if (_source.type == Type::Node || _source.type == Type::NodeSurface) {
        if (p.sourceNodeName.has_value()) {
            _source.nodeName = *p.sourceNodeName;
        }
        else {
            LERRORC(
                "DashboardItemDistance",
                "Node type was selected for source but no node specified"
            );
        }
    }
    addProperty(_source.nodeName);

    _destination.type.addOptions({
        { Type::Node, "Node" },
        { Type::NodeSurface, "Node Surface" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _destination.type.onChange([this]() {
        _destination.nodeName.setVisibility(
            properties::Property::Visibility(
                _source.type == Type::Node || _source.type == Type::NodeSurface
            )
        );
    });
    if (p.destinationType.has_value()) {
        switch (*p.destinationType) {
            case Parameters::TypeInfo::Node:
                _destination.type = Type::Node;
                break;
            case Parameters::TypeInfo::NodeSurface:
                _destination.type = Type::NodeSurface;
                break;
            case Parameters::TypeInfo::Focus:
                _destination.type = Type::Focus;
                break;
            case Parameters::TypeInfo::Camera:
                _destination.type = Type::Camera;
                break;
        }
    }
    else {
        _destination.type = Type::Focus;
    }
    addProperty(_destination.type);
    _destination.nodeName.onChange([this]() { _destination.node = nullptr; });
    if (_destination.type == Type::Node || _destination.type == Type::NodeSurface) {
        if (p.destinationNodeName.has_value()) {
            _destination.nodeName = *p.destinationNodeName;
        }
        else {
            LERRORC(
                "DashboardItemDistance",
                "Node type was selected for destination but no node specified"
            );
        }
    }
    addProperty(_destination.nodeName);

    _doSimplification = p.simplification.value_or(_doSimplification);
    _doSimplification.onChange([this]() {
        _requestedUnit.setVisibility(
            _doSimplification ?
            properties::Property::Visibility::Hidden :
            properties::Property::Visibility::User
        );
    });
    addProperty(_doSimplification);

    for (DistanceUnit u : DistanceUnits) {
        _requestedUnit.addOption(static_cast<int>(u), nameForDistanceUnit(u));
    }
    _requestedUnit = static_cast<int>(DistanceUnit::Meter);
    if (p.requestedUnit.has_value()) {
        DistanceUnit unit = distanceUnitFromString(p.requestedUnit->c_str());
        _requestedUnit = static_cast<int>(unit);
    }
    _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_requestedUnit);

    _formatString = p.formatString.value_or(_formatString);
    addProperty(_formatString);

    _buffer.resize(256);
}

std::pair<glm::dvec3, std::string> DashboardItemDistance::positionAndLabel(
                                                                      Component& mainComp,
                                                               Component& otherComp) const
{
    if ((mainComp.type == Type::Node) || (mainComp.type == Type::NodeSurface)) {
        if (!mainComp.node) {
            mainComp.node = global::renderEngine->scene()->sceneGraphNode(
                mainComp.nodeName
            );

            if (!mainComp.node) {
                LERRORC(
                    "DashboardItemDistance",
                    "Could not find node '" + mainComp.nodeName.value() + "'"
                );
                return { glm::dvec3(0.0), "Node" };
            }
        }
    }

    switch (mainComp.type) {
        case Type::Node:
            return { mainComp.node->worldPosition(), mainComp.node->guiName() };
        case Type::NodeSurface:
        {
            glm::dvec3 otherPos;
            if (otherComp.type == Type::NodeSurface) {
                // We are only interested in the direction, and we want to prevent
                // infinite recursion
                otherPos = otherComp.node->worldPosition();
            }
            else {
                otherPos = positionAndLabel(otherComp, mainComp).first;
            }
            const glm::dvec3 thisPos = mainComp.node->worldPosition();

            const glm::dvec3 dir = glm::normalize(otherPos - thisPos);
            glm::dvec3 dirLength = dir * glm::dvec3(mainComp.node->boundingSphere());

            return { thisPos + dirLength, "surface of " + mainComp.node->guiName() };
        }
        case Type::Focus: {
            const SceneGraphNode* anchor =
                global::navigationHandler->orbitalNavigator().anchorNode();
            if (!anchor) {
                return { glm::dvec3(0.0), "Unknown" };
            }
            else {
                return { anchor->worldPosition(), "focus" };
            }
        }
        case Type::Camera:
            return { global::renderEngine->scene()->camera()->positionVec3(), "camera" };
        default:
            return { glm::dvec3(0.0), "Unknown" };
    }
}

void DashboardItemDistance::render(glm::vec2& penPosition) {
    ZoneScoped

    std::pair<glm::dvec3, std::string> sourceInfo = positionAndLabel(
        _source,
        _destination
    );
    std::pair<glm::dvec3, std::string> destinationInfo = positionAndLabel(
        _destination,
        _source
    );

    const double d = glm::length(sourceInfo.first - destinationInfo.first);
    std::pair<double, std::string> dist;
    if (_doSimplification) {
        dist = simplifyDistance(d);
    }
    else {
        const DistanceUnit unit = static_cast<DistanceUnit>(_requestedUnit.value());
        const double convertedD = convertDistance(d, unit);
        dist = { convertedD, nameForDistanceUnit(unit, convertedD != 1.0) };
    }

    std::fill(_buffer.begin(), _buffer.end(), char(0));
    try {
        char* end = fmt::format_to(
            _buffer.data(),
            _formatString.value().c_str(),
            sourceInfo.second, destinationInfo.second, dist.first, dist.second
        );

        std::string_view text = std::string_view(_buffer.data(), end - _buffer.data());
        RenderFont(*_font, penPosition, text);
    }
    catch (const fmt::format_error&) {
        LERRORC("DashboardItemDate", "Illegal format string");
    }
    penPosition.y -= _font->height();
}

glm::vec2 DashboardItemDistance::size() const {
    ZoneScoped

    const double d = glm::length(1e20);
    std::pair<double, std::string> dist;
    if (_doSimplification) {
        dist = simplifyDistance(d);
    }
    else {
        DistanceUnit unit = static_cast<DistanceUnit>(_requestedUnit.value());
        double convertedD = convertDistance(d, unit);
        dist = { convertedD, nameForDistanceUnit(unit, convertedD != 1.0) };
    }

    return _font->boundingBox(
        fmt::format("Distance from focus: {} {}", dist.first, dist.second)
    );
}

} // namespace openspace
