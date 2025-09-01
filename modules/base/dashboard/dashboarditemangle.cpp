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
        "angle.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SourceNodeIdentifierInfo = {
        "SourceNodeIdentifier",
        "Source Node Identifier",
        "If a scene graph node is selected as type, this value specifies the identifier "
        "of the node that is to be used as the apex of the triangle used to calculate "
        "the angle. The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceTypeInfo = {
        "ReferenceType",
        "Reference Type",
        "The type of position that is used as the destination of the reference line used "
        "to calculate the angle. The computed angle is the incident angle to Source in "
        "the triangle (Source, Reference, Destination).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceNodeIdentifierInfo =
    {
        "ReferenceNodeIdentifier",
        "Reference Node Identifier",
        "If a scene graph node is selected as type, this value specifies the identifier "
        "of the node that is to be used as the reference direction to compute the angle.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationTypeInfo = {
        "DestinationType",
        "Destination Type",
        "The type of position that is used as the destination to calculate the angle. "
        "The computed angle is the incident angle to Source in the triangle ("
        "Source, Reference, Destination).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo
        DestinationNodeIdentifierInfo =
    {
        "DestinationNodeIdentifier",
        "Destination Node Identifier",
        "If a scene graph node is selected as type, this value specifies the identifier "
        "of the node that is to be used as the destination for computing the angle.",
        openspace::properties::Property::Visibility::User
    };

    // This `DashboardItem` shows the angle between the lines `Source`->`Reference` and
    // `Source`->`Destination`. Each of `Source`, `Reference`, and `Destination` can be
    // either the identifier of a node, the current focus node, or the position of the
    // camera. The angle cannot be calculated if two of these three items are located in
    // the same position, in which case an error message is printed. The `SourceNodeName`,
    // `ReferenceNodeName`, and `DestinationNodeName` parameters are only used if the
    // `SourceType`, `ReferenceType`, or `DestinationType` respectively is set to `Node`.
    struct [[codegen::Dictionary(DashboardItemAngle)]] Parameters {
        enum class [[codegen::map(Type)]] Type {
            Node,
            Focus,
            Camera
        };

        // [[codegen::verbatim(SourceTypeInfo.description)]]
        Type sourceType;
        // [[codegen::verbatim(SourceNodeIdentifierInfo.description)]]
        std::optional<std::string> sourceNodeIdentifier;
        // [[codegen::verbatim(ReferenceTypeInfo.description)]]
        Type referenceType;
        // [[codegen::verbatim(ReferenceNodeIdentifierInfo.description)]]
        std::optional<std::string> referenceNodeIdentifier;
        // [[codegen::verbatim(DestinationTypeInfo.description)]]
        Type destinationType;
        // [[codegen::verbatim(DestinationNodeIdentifierInfo.description)]]
        std::optional<std::string> destinationNodeIdentifier;
    };
#include "dashboarditemangle_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemAngle::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_angle",
        DashboardTextItem::Documentation()
    );
}

DashboardItemAngle::DashboardItemAngle(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _source{
        properties::OptionProperty(SourceTypeInfo),
        properties::StringProperty(SourceNodeIdentifierInfo),
        nullptr
    }
    , _reference{
        properties::OptionProperty(ReferenceTypeInfo),
        properties::StringProperty(ReferenceNodeIdentifierInfo),
        nullptr
    }
    , _destination{
        properties::OptionProperty(DestinationTypeInfo),
        properties::StringProperty(DestinationNodeIdentifierInfo),
        nullptr
    }
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _source.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _source.type = codegen::map<Type>(p.sourceType);
    addProperty(_source.type);

    _source.nodeIdentifier.onChange([this]() { _source.node = nullptr; });
    if (_source.type == Type::Node) {
        if (p.sourceNodeIdentifier.has_value()) {
            _source.nodeIdentifier = *p.sourceNodeIdentifier;
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for source but no node specified"
            );
        }
    }
    addProperty(_source.nodeIdentifier);


    _reference.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _reference.type = codegen::map<Type>(p.referenceType);
    addProperty(_reference.type);

    _reference.nodeIdentifier.onChange([this]() { _reference.node = nullptr; });
    if (_reference.type == Type::Node) {
        if (p.referenceNodeIdentifier.has_value()) {
            _reference.nodeIdentifier = *p.referenceNodeIdentifier;
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for reference but no node specified"
            );
        }
    }
    addProperty(_reference.nodeIdentifier);

    _destination.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _destination.type = codegen::map<Type>(p.destinationType);
    addProperty(_destination.type);
    _destination.nodeIdentifier.onChange([this]() { _destination.node = nullptr; });
    if (_destination.type == Type::Node) {
        if (p.destinationNodeIdentifier.has_value()) {
            _destination.nodeIdentifier = *p.destinationNodeIdentifier;
        }
        else {
            LERRORC(
                "DashboardItemAngle",
                "Node type was selected for destination but no node specified"
            );
        }
    }
    addProperty(_destination.nodeIdentifier);

    _localBuffer.resize(128);
}

void DashboardItemAngle::update() {
    ZoneScoped;

    std::pair<glm::dvec3, std::string> sourceInfo = positionAndLabel(_source);
    std::pair<glm::dvec3, std::string> referenceInfo = positionAndLabel(_reference);
    std::pair<glm::dvec3, std::string> destinationInfo = positionAndLabel(_destination);

    const glm::dvec3 a = referenceInfo.first - sourceInfo.first;
    const glm::dvec3 b = destinationInfo.first - sourceInfo.first;

    std::fill(_localBuffer.begin(), _localBuffer.end(), char(0));
    if (glm::length(a) == 0.0 || glm::length(b) == 0) {
        char* end = std::format_to(
            _localBuffer.data(),
            "Could not compute angle at {} between {} and {}. At least two of the three "
            "items are placed in the same location",
            sourceInfo.second, destinationInfo.second, referenceInfo.second
        );
        _buffer = std::string(_localBuffer.data(), end - _localBuffer.data());
    }
    else {
        const double angle = glm::degrees(
            glm::acos(glm::dot(a, b) / (glm::length(a) * glm::length(b)))
        );

        char* end = std::format_to(
            _localBuffer.data(),
            "Angle at {} between {} and {}: {} degrees",
            sourceInfo.second, destinationInfo.second, referenceInfo.second, angle
        );
        _buffer = std::string(_localBuffer.data(), end - _localBuffer.data());
    }
}

std::pair<glm::dvec3, std::string> DashboardItemAngle::positionAndLabel(Component& comp) {
    if (comp.type == Type::Node) {
        if (!comp.node) {
            comp.node =
                global::renderEngine->scene()->sceneGraphNode(comp.nodeIdentifier);

            if (!comp.node) {
                LERRORC(
                    "DashboardItemAngle",
                    "Could not find node '" + comp.nodeIdentifier.value() + "'"
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
