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

#include <modules/base/dashboard/dashboarditemdistance.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* KeyFontMono = "Mono";

    constexpr const float DefaultFontSize = 10.f;

    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

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
        "If this value is enabled, the distace is displayed in nuanced units, such as "
        "km, AU, light years, parsecs, etc. If this value is disabled, it is always "
        "displayed in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo RequestedUnitInfo = {
        "RequestedUnit",
        "Requested Unit",
        "If the simplification is disabled, this distance unit is used as a destination "
        "to convert the meters into."
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
} // namespace

namespace openspace {

documentation::Documentation DashboardItemDistance::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Distance",
        "base_dashboarditem_distance",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemDistance"),
                Optional::No
            },
            {
                FontNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                FontNameInfo.description
            },
            {
                FontSizeInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                FontSizeInfo.description
            },
            {
                SourceTypeInfo.identifier,
                new StringInListVerifier({
                    "Node", "Node Surface", "Focus", "Camera"
                }),
                Optional::Yes,
                    SourceTypeInfo.description
            },
            {
                SourceNodeNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                SourceNodeNameInfo.description
            },
            {
                DestinationTypeInfo.identifier,
                new StringInListVerifier({
                    "Node", "Node Surface", "Focus", "Camera"
                }),
                Optional::Yes,
                DestinationTypeInfo.description
            },
            {
                DestinationNodeNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                DestinationNodeNameInfo.description
            },
            {
                SimplificationInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                SimplificationInfo.description
            },
            {
                RequestedUnitInfo.identifier,
                new StringInListVerifier(unitList()),
                Optional::Yes,
                RequestedUnitInfo.description
            }
        }
    };
}

DashboardItemDistance::DashboardItemDistance(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
    , _doSimplification(SimplificationInfo, true)
    , _requestedUnit(RequestedUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
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
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemDistance"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(dictionary.value<double>(FontSizeInfo.identifier));
    }

    _fontName.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

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
    if (dictionary.hasKey(SourceTypeInfo.identifier)) {
        const std::string& value = dictionary.value<std::string>(
            SourceTypeInfo.identifier
        );

        if (value == "Node") {
            _source.type = Type::Node;
        }
        else if (value == "Node Surface") {
            _source.type = Type::NodeSurface;
        }
        else if (value == "Focus") {
            _source.type = Type::Focus;
        }
        else {
            _source.type = Type::Camera;
        }
    }
    else {
        _source.type = Type::Camera;
    }
    addProperty(_source.type);

    _source.nodeName.onChange([this]() { _source.node = nullptr; });
    if (_source.type == Type::Node || _source.type == Type::NodeSurface) {
        if (dictionary.hasKey(SourceNodeNameInfo.identifier)) {
            _source.nodeName = dictionary.value<std::string>(
                SourceNodeNameInfo.identifier
            );
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
    if (dictionary.hasKey(DestinationTypeInfo.identifier)) {
        const std::string& value = dictionary.value<std::string>(
            DestinationTypeInfo.identifier
        );
        if (value == "Node") {
            _destination.type = Type::Node;
        }
        else if (value == "Node Surface") {
            _destination.type = Type::NodeSurface;
        }
        else if (value == "Focus") {
            _destination.type = Type::Focus;
        }
        else {
            _destination.type = Type::Camera;
        }
    }
    else {
        _destination.type = Type::Focus;
    }
    addProperty(_destination.type);
    _destination.nodeName.onChange([this]() { _destination.node = nullptr; });
    if (_destination.type == Type::Node || _destination.type == Type::NodeSurface) {
        if (dictionary.hasKey(DestinationNodeNameInfo.identifier)) {
            _destination.nodeName = dictionary.value<std::string>(
                DestinationNodeNameInfo.identifier
            );
        }
        else {
            LERRORC(
                "DashboardItemDistance",
                "Node type was selected for destination but no node specified"
            );
        }
    }
    addProperty(_destination.nodeName);

    if (dictionary.hasKey(SimplificationInfo.identifier)) {
        _doSimplification = dictionary.value<bool>(SimplificationInfo.identifier);
    }
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
    if (dictionary.hasKey(RequestedUnitInfo.identifier)) {
        const std::string& value = dictionary.value<std::string>(
            RequestedUnitInfo.identifier
        );
        DistanceUnit unit = distanceUnitFromString(value.c_str());
        _requestedUnit = static_cast<int>(unit);
    }
    _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_requestedUnit);

    _font = global::fontManager.font(_fontName, _fontSize);
}

std::pair<glm::dvec3, std::string> DashboardItemDistance::positionAndLabel(
                                                                      Component& mainComp,
                                                               Component& otherComp) const
{
    if ((mainComp.type == Type::Node) || (mainComp.type == Type::NodeSurface)) {
        if (!mainComp.node) {
            mainComp.node = global::renderEngine.scene()->sceneGraphNode(
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
        case Type::Focus:
            return {
                global::navigationHandler.focusNode()->worldPosition(),
                "focus"
            };
        case Type::Camera:
            return { global::renderEngine.scene()->camera()->positionVec3(), "camera" };
        default:
            return { glm::dvec3(0.0), "Unknown" };
    }
}

void DashboardItemDistance::render(glm::vec2& penPosition) {
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

    penPosition.y -= _font->height();
    RenderFont(
        *_font,
        penPosition,
        fmt::format(
            "Distance from {} to {}: {:f} {}",
            sourceInfo.second, destinationInfo.second, dist.first, dist.second
        )
    );
}

glm::vec2 DashboardItemDistance::size() const {
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

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        fmt::format("Distance from focus: {} {}", dist.first, dist.second)
    ).boundingBox;
}

} // namespace openspace
