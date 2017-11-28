/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyFontMono = "Mono";

    const float DefaultFontSize = 10.f;

    static const openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    static const openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

    static const openspace::properties::Property::PropertyInfo SourceTypeInfo = {
        "SourceType",
        "Source Type",
        "The type of position that is used as the source to calculate the distance. The "
        "default value is 'Camera'."
    };

    static const openspace::properties::Property::PropertyInfo SourceNodeNameInfo = {
        "SourceNodeName",
        "Source Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the source for computing the distance."
    };

    static const openspace::properties::Property::PropertyInfo DestinationTypeInfo = {
        "DestinationType",
        "Destination Type",
        "The type of position that is used as the destination to calculate the distance. "
        "The default value for this is 'Focus'."
    };

    static const openspace::properties::Property::PropertyInfo DestinationNodeNameInfo = {
        "DestinationNodeName",
        "Destination Node Name",
        "If a scene graph node is selected as type, this value specifies the name of the "
        "node that is to be used as the destination for computing the distance."
    };
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

DashboardItemDistance::DashboardItemDistance(ghoul::Dictionary dictionary)
    : DashboardItem("Distance")
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
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
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
            );
    }

    _fontName.onChange([this]() {
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize.onChange([this]() {
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _source.type.addOptions({
        { Type::Node, "Node" },
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _source.type.onChange([this]() {
        _source.nodeName.setVisibility(
            properties::Property::Visibility(_source.type.value() == Type::Node)
        );
    });
    if (dictionary.hasKey(SourceTypeInfo.identifier)) {
        std::string value = dictionary.value<std::string>(SourceTypeInfo.identifier);
        if (value == "Node") {
            _source.type.setValue(Type::Node);
        }
        else if (value == "Focus") {
            _source.type.setValue(Type::Focus);
        }
        else {
            _source.type.setValue(Type::Camera);
        }
    }
    else {
        _source.type = Type::Camera;
    }
    addProperty(_source.type);

    _source.nodeName.onChange([this]() {
        _source.node = nullptr;
    });
    if (_source.type.value() == Type::Node) {
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
        { Type::Focus, "Focus" },
        { Type::Camera, "Camera" }
    });
    _destination.type.onChange([this]() {
        _destination.nodeName.setVisibility(
            properties::Property::Visibility(
                _destination.type.value() == static_cast<int>(Type::Node)
            )
        );
    });
    if (dictionary.hasKey(DestinationTypeInfo.identifier)) {
        std::string value = dictionary.value<std::string>(DestinationTypeInfo.identifier);
        if (value == "Node") {
            _destination.type.setValue(Type::Node);
        }
        else if (value == "Focus") {
            _destination.type.setValue(Type::Focus);
        }
        else {
            _destination.type.setValue(Type::Camera);
        }
    }
    else {
        _destination.type = Type::Focus;
    }
    addProperty(_destination.type);
    _destination.nodeName.onChange([this]() {
        _destination.node = nullptr;
    });
    if (_destination.type.value() == Type::Node) {
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

    _font = OsEng.fontManager().font(_fontName, _fontSize);
}

void DashboardItemDistance::render(glm::vec2& penPosition) {
    auto positionAndLabel = [](Component& comp) -> std::pair<glm::dvec3, std::string> {
        switch (comp.type) {
            case Type::Node:
                if (!comp.node) {
                    comp.node = OsEng.renderEngine().scene()->sceneGraphNode(
                        comp.nodeName
                    );

                    if (comp.node) {
                        LERRORC(
                            "DashboardItemDistance",
                            "Could not find node '" + comp.nodeName.value() + "'"
                        );
                        return { glm::dvec3(0.0), "Node" };
                    }
                }

                return { comp.node->worldPosition(), comp.node->name() };
            case Type::Focus:
                return {
                    OsEng.navigationHandler().focusNode()->worldPosition(),
                    "focus"
                };
            case Type::Camera:
                return { OsEng.renderEngine().camera()->positionVec3(), "camera" };
            default:
                return { glm::dvec3(0.0), "Unknown" };
        }
    };

    std::pair<glm::dvec3, std::string> sourceInfo = positionAndLabel(_source);
    std::pair<glm::dvec3, std::string> destinationInfo = positionAndLabel(_destination);

    double distance = glm::length(sourceInfo.first - destinationInfo.first);
    std::pair<double, std::string> dist = simplifyDistance(distance);
    RenderFontCr(
        *_font,
        penPosition,
        "Distance from %s to %s: %f %s",
        sourceInfo.second.c_str(),
        destinationInfo.second.c_str(),
        dist.first,
        dist.second.c_str()
    );
}

glm::vec2 DashboardItemDistance::size() const {
    double distance = 1e20;
    std::pair<double, std::string> dist = simplifyDistance(distance);

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        "Distance from focus: %f %s",
        dist.first,
        dist.second.c_str()
    ).boundingBox;
}

} // namespace openspace
