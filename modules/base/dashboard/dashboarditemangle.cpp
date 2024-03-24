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

#include <modules/base/dashboard/dashboarditemangle.h>

#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    enum Type {
        Node = 0,
        Focus,
        Camera
    };

    constexpr openspace::properties::Property::PropertyInfo SourceTypeInfo = {
        "SourceType",
        "Source Type",
        "The type of position that is used as the triangle apex used to calculate the "
        "angle. The default value is 'Camera'",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SourceNodeNameInfo = {
        "SourceNodeName",
        "Source Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the apex of the triangle used to calculate the "
        "angle. The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination)",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceTypeInfo = {
        "ReferenceType",
        "Reference Type",
        "The type of position that is used as the destination of the reference line used "
        "to calculate the angle. The computed angle is the incident angle to Source in "
        "the triangle (Source, Reference, Destination)",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceNodeNameInfo = {
        "ReferenceNodeName",
        "Reference Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the reference direction to compute the angle",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationTypeInfo = {
        "DestinationType",
        "Destination Type",
        "The type of position that is used as the destination to calculate the angle. "
        "The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination). The default value for this is 'Focus'",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationNodeNameInfo = {
        "DestinationNodeName",
        "Destination Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the destination for computing the angle",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    // This DashboardItem shows the angle between two scenegraph nodes relative to a
    // reference node. The angle is calculated in the plane that is defined by the
    // 'SourceNodeName', 'DestinationNodeName', and the 'ReferenceNodeName'.
    struct [[codegen::Dictionary(DashboardItemAngle)]] Parameters {
        enum class [[codegen::map(Type)]] Type {
            Node,
            Focus,
            Camera
        };

        // [[codegen::verbatim(SourceTypeInfo.description)]]
        std::optional<Type> sourceType;
        // [[codegen::verbatim(SourceNodeNameInfo.description)]]
        std::optional<std::string> sourceNodeName;
        // [[codegen::verbatim(ReferenceTypeInfo.description)]]
        Type referenceType;
        // [[codegen::verbatim(ReferenceNodeNameInfo.description)]]
        std::optional<std::string> referenceNodeName;
        // [[codegen::verbatim(DestinationTypeInfo.description)]]
        std::optional<Type> destinationType;
        // [[codegen::verbatim(DestinationNodeNameInfo.description)]]
        std::optional<std::string> destinationNodeName;
    };
#include "dashboarditemangle_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemAngle::Documentation() {
    documentation::Documentation doc =
        codegen::doc<Parameters>("base_dashboarditem_angle");

    // @TODO cleanup
    // Insert the parent's documentation entries until we have a verifier that can deal
    // with class hierarchy
    documentation::Documentation parentDoc = DashboardTextItem::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );

    return doc;
}

DashboardItemAngle::DashboardItemAngle(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
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
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _source.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    if (p.sourceType.has_value()) {
        _source.type = codegen::map<Type>(*p.sourceType);
    }
    else {
        _source.type = Type::Camera;
    }
    addProperty(_source.type);

    _source.nodeName.onChange([this]() { _source.node = nullptr; });
    if (_source.type == Type::Node) {
        if (p.sourceNodeName.has_value()) {
            _source.nodeName = *p.sourceNodeName;
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
    _reference.type = codegen::map<Type>(p.referenceType);
    addProperty(_reference.type);

    _reference.nodeName.onChange([this]() { _reference.node = nullptr; });
    if (_reference.type == Type::Node) {
        if (p.referenceNodeName.has_value()) {
            _reference.nodeName = *p.referenceNodeName;
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for reference but no node specified"
            );
        }
    }
    addProperty(_reference.nodeName);

    _destination.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    if (p.destinationType.has_value()) {
        _destination.type = codegen::map<Type>(*p.destinationType);
    }
    else {
        _destination.type = Type::Focus;
    }
    addProperty(_destination.type);
    _destination.nodeName.onChange([this]() { _destination.node = nullptr; });
    if (_destination.type == Type::Node) {
        if (p.destinationNodeName.has_value()) {
            _destination.nodeName = *p.destinationNodeName;
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for destination but no node specified"
            );
        }
    }
    addProperty(_destination.nodeName);

    _buffer.resize(128);
}

void DashboardItemAngle::render(glm::vec2& penPosition) {
    ZoneScoped;

    std::pair<glm::dvec3, std::string> sourceInfo = positionAndLabel(_source);
    std::pair<glm::dvec3, std::string> referenceInfo = positionAndLabel(_reference);
    std::pair<glm::dvec3, std::string> destinationInfo = positionAndLabel(_destination);

    const glm::dvec3 a = referenceInfo.first - sourceInfo.first;
    const glm::dvec3 b = destinationInfo.first - sourceInfo.first;

    std::fill(_buffer.begin(), _buffer.end(), char(0));
    if (glm::length(a) == 0.0 || glm::length(b) == 0) {
        char* end = std::format_to(
            _buffer.data(),
            "Could not compute angle at {} between {} and {}",
            sourceInfo.second, destinationInfo.second, referenceInfo.second
        );
        const std::string_view text = std::string_view(
            _buffer.data(),
            end - _buffer.data()
        );
        RenderFont(*_font, penPosition, text);
        penPosition.y -= _font->height();
    }
    else {
        const double angle = glm::degrees(
            glm::acos(glm::dot(a, b) / (glm::length(a) * glm::length(b)))
        );

        char* end = std::format_to(
            _buffer.data(),
            "Angle at {} between {} and {}: {} degrees",
            sourceInfo.second, destinationInfo.second, referenceInfo.second, angle
        );
        const std::string_view text = std::string_view(
            _buffer.data(), end - _buffer.data()
        );
        RenderFont(*_font, penPosition, text);
        penPosition.y -= _font->height();
    }
}

glm::vec2 DashboardItemAngle::size() const {
    ZoneScoped;

    constexpr double Angle = 120;
    return _font->boundingBox("Angle: " + std::to_string(Angle));
}

std::pair<glm::dvec3, std::string> DashboardItemAngle::positionAndLabel(Component& comp) {
    if (comp.type == Type::Node) {
        if (!comp.node) {
            comp.node = global::renderEngine->scene()->sceneGraphNode(comp.nodeName);

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
        {
            const SceneGraphNode* node =
                global::navigationHandler->orbitalNavigator().anchorNode();
            return {
                node->worldPosition(),
                "focus"
            };
        }
        case Type::Camera:
            return { global::renderEngine->scene()->camera()->positionVec3(), "camera" };
        default:
            return { glm::dvec3(0.0), "Unknown" };
    }
}

} // namespace openspace
