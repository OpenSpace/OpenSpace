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

#include <modules/base/dashboard/dashboarditemangle.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
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
        "The type of position that is used as the triangle apex used to calculate the "
        "angle. The default value is 'Camera'."
    };

    constexpr openspace::properties::Property::PropertyInfo SourceNodeNameInfo = {
        "SourceNodeName",
        "Source Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the apex of the triangle used to calculate the "
        "angle. The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination)."
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceTypeInfo = {
        "ReferenceType",
        "Reference Type",
        "The type of position that is used as the destination of the reference line used "
        "to calculate the angle. The computed angle is the incident angle to Source in "
        "the triangle (Source, Reference, Destination)."
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceNodeNameInfo = {
        "ReferenceNodeName",
        "Reference Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the reference direction to compute the angle."
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationTypeInfo = {
        "DestinationType",
        "Destination Type",
        "The type of position that is used as the destination to calculate the angle. "
        "The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination). The default value for this is 'Focus'."
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationNodeNameInfo = {
        "DestinationNodeName",
        "Destination Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the destination for computing the angle."
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItemAngle::Documentation() {
    using namespace documentation;

    return {
        "DashboardItem Angle",
        "base_dashboarditem_angle",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemAngle"),
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
                    "Node", "Focus", "Camera"
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
                ReferenceTypeInfo.identifier,
                new StringInListVerifier({
                    "Node", "Focus", "Camera"
                }),
                Optional::No,
                ReferenceTypeInfo.description
            },
            {
                ReferenceNodeNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                ReferenceNodeNameInfo.description
            },
            {
                DestinationTypeInfo.identifier,
                new StringInListVerifier({
                    "Node", "Focus", "Camera"
                }),
                Optional::Yes,
                DestinationTypeInfo.description
            },
            {
                DestinationNodeNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                DestinationNodeNameInfo.description
            }
        }
    };
}

DashboardItemAngle::DashboardItemAngle(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _source{
        properties::OptionProperty(
            SourceTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(SourceNodeNameInfo),
        nullptr
    }
    , _reference{
        properties::OptionProperty(
            ReferenceTypeInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::StringProperty(ReferenceNodeNameInfo),
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
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemAngle"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
        );
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
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _source.type.onChange([this]() {
        _source.nodeName.setVisibility(
            properties::Property::Visibility(_source.type == Type::Node)
        );
    });
    if (dictionary.hasKey(SourceTypeInfo.identifier)) {
        std::string value = dictionary.value<std::string>(SourceTypeInfo.identifier);
        if (value == "Node") {
            _source.type = Type::Node;
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

    _source.nodeName.onChange([this]() {
        _source.node = nullptr;
    });
    if (_source.type == Type::Node) {
        if (dictionary.hasKey(SourceNodeNameInfo.identifier)) {
            _source.nodeName = dictionary.value<std::string>(
                SourceNodeNameInfo.identifier
            );
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for source but no node specified"
            );
        }
    }
    addProperty(_source.nodeName);


    _reference.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _reference.type.onChange([this]() {
        _reference.nodeName.setVisibility(
            properties::Property::Visibility(_reference.type == Type::Node)
        );
    });
    std::string value = dictionary.value<std::string>(ReferenceTypeInfo.identifier);
    if (value == "Node") {
        _reference.type = Type::Node;
    }
    else if (value == "Focus") {
        _reference.type = Type::Focus;
    }
    else {
        _reference.type = Type::Camera;
    }
    addProperty(_reference.type);

    _reference.nodeName.onChange([this]() {
        _reference.node = nullptr;
    });
    if (_reference.type == Type::Node) {
        if (dictionary.hasKey(ReferenceNodeNameInfo.identifier)) {
            _reference.nodeName = dictionary.value<std::string>(
                ReferenceNodeNameInfo.identifier
            );
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for reference but no node specified"
            );
        }
    }
    addProperty(_source.nodeName);

    _destination.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _destination.type.onChange([this]() {
        _destination.nodeName.setVisibility(
            properties::Property::Visibility(_source.type == Type::Node)
        );
    });
    if (dictionary.hasKey(DestinationTypeInfo.identifier)) {
        std::string type = dictionary.value<std::string>(DestinationTypeInfo.identifier);
        if (type == "Node") {
            _destination.type = Type::Node;
        }
        else if (type == "Focus") {
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
    if (_destination.type == Type::Node) {
        if (dictionary.hasKey(DestinationNodeNameInfo.identifier)) {
            _destination.nodeName = dictionary.value<std::string>(
                DestinationNodeNameInfo.identifier
            );
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for destination but no node specified"
            );
        }
    }
    addProperty(_destination.nodeName);

    _font = global::fontManager.font(_fontName, _fontSize);
}

std::pair<glm::dvec3, std::string> DashboardItemAngle::positionAndLabel(
                                                                    Component& comp) const
{
    if (comp.type == Type::Node) {
        if (!comp.node) {
            comp.node = global::renderEngine.scene()->sceneGraphNode(comp.nodeName);

            if (!comp.node) {
                LERRORC(
                    "DashboardItemAngle",
                    "Could not find node '" + comp.nodeName.value() + "'"
                );
                return { glm::dvec3(0.0), "Node" };
            }
        }
    }

    switch (comp.type) {
        case Type::Node:
            return { comp.node->worldPosition(), comp.node->guiName() };
        case Type::Focus:
            return { global::navigationHandler.focusNode()->worldPosition(), "focus" };
        case Type::Camera:
            return { global::renderEngine.scene()->camera()->positionVec3(), "camera" };
        default:
            return { glm::dvec3(0.0), "Unknown" };
    }
}

void DashboardItemAngle::render(glm::vec2& penPosition) {
    std::pair<glm::dvec3, std::string> sourceInfo = positionAndLabel(_source);
    std::pair<glm::dvec3, std::string> referenceInfo = positionAndLabel(_reference);
    std::pair<glm::dvec3, std::string> destinationInfo = positionAndLabel(_destination);

    const glm::dvec3 a = referenceInfo.first - sourceInfo.first;
    const glm::dvec3 b = destinationInfo.first - sourceInfo.first;

    if (glm::length(a) == 0.0 || glm::length(b) == 0) {
        penPosition.y -= _font->height();
        RenderFont(
            *_font,
            penPosition,
            fmt::format(
                "Could not compute angle at {} between {} and {}",
                sourceInfo.second, destinationInfo.second, referenceInfo.second
            )
        );
    }
    else {
        const double angle = glm::degrees(
            glm::acos(glm::dot(a, b) / (glm::length(a) * glm::length(b)))
        );

        penPosition.y -= _font->height();
        RenderFont(
            *_font,
            penPosition,
            fmt::format(
                "Angle at {} between {} and {}: {} degrees",
                sourceInfo.second, destinationInfo.second, referenceInfo.second, angle
            )
        );
    }
}

glm::vec2 DashboardItemAngle::size() const {
    constexpr const double Angle = 120;

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        "Angle: " + std::to_string(Angle)
    ).boundingBox;
}

} // namespace openspace
