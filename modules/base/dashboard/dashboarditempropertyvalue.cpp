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

#include <modules/base/dashboard/dashboarditempropertyvalue.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/longproperty.h>
#include <openspace/properties/scalar/shortproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/ulongproperty.h>
#include <openspace/properties/scalar/ushortproperty.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/dvec4property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/ivec4property.h>
#include <openspace/properties/vector/uvec2property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/uvec4property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo PropertyUriInfo = {
        "URI",
        "Property URI",
        "The URI of the property that is displayed in this dashboard item.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayStringInfo = {
        "DisplayString",
        "Display String",
        "The String that is being displayed. It must either be empty (in which case only "
        "the value itself will be displayed), or it must contain extact one or more "
        "instances of {}, which will be replaced with the value(s) of the property "
        "during rendering. For scalar types, there has to be exactly one instance of {}, "
        "for vector types, there need to be as many {} as there are compoents in the "
        "vector, for example two {} for vec2 types, three for vec3 types, etc.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemPropertyValue)]] Parameters {
        // [[codegen::verbatim(PropertyUriInfo.description)]]
        std::optional<std::string> uri [[codegen::key("URI")]];

        // [[codegen::verbatim(DisplayStringInfo.description)]]
        std::optional<std::string> displayString;
    };
#include "dashboarditempropertyvalue_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemPropertyValue::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_propertyvalue",
        DashboardTextItem::Documentation()
    );
}

DashboardItemPropertyValue::DashboardItemPropertyValue(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _propertyUri(PropertyUriInfo)
    , _displayString(DisplayStringInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _propertyUri = p.uri.value_or(_propertyUri);
    _propertyUri.onChange([this]() { _propertyIsDirty = true; });
    addProperty(_propertyUri);

    _displayString = p.displayString.value_or(_displayString);
    addProperty(_displayString);
}

void DashboardItemPropertyValue::render(glm::vec2& penPosition) {
    ZoneScoped;

    if (_propertyIsDirty) {
        _property = openspace::property(_propertyUri);
        _propertyIsDirty = false;
    }

    if (!_property) {
        return;
    }
    const std::string_view type = _property->className();
    penPosition.y -= _font->height();
    if (type == "DoubleProperty") {
        double value = static_cast<properties::DoubleProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
    else if (type == "FloatProperty") {
        float value = static_cast<properties::FloatProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
    else if (type == "IntProperty") {
        int value = static_cast<properties::IntProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
    else if (type == "LongProperty") {
        long value = static_cast<properties::LongProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
    else if (type == "ShortProperty") {
        short value = static_cast<properties::ShortProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
    else if (type == "UIntProperty") {
        unsigned int v = static_cast<properties::UIntProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v))
        );
    }
    else if (type == "ULongProperty") {
        unsigned long v = static_cast<properties::ULongProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v))
        );
    }
    else if (type == "UShortProperty") {
        unsigned short v = static_cast<properties::UShortProperty*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v))
        );
    }
    else if (type == "DVec2Property") {
        glm::dvec2 v = static_cast<properties::DVec2Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y))
        );
    }
    else if (type == "DVec3Property") {
        glm::dvec3 v = static_cast<properties::DVec3Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y, v.z))
        );
    }
    else if (type == "DVec4Property") {
        glm::dvec4 v = static_cast<properties::DVec4Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(
                _displayString.value(),
                std::make_format_args(v.x, v.y, v.z, v.w)
            )
        );
    }
    else if (type == "IVec2Property") {
        glm::ivec2 v = static_cast<properties::IVec2Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y))
        );
    }
    else if (type == "IVec3Property") {
        glm::ivec3 v = static_cast<properties::IVec3Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y, v.z))
        );
    }
    else if (type == "IVec4Property") {
        glm::ivec4 v = static_cast<properties::IVec4Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(
                _displayString.value(),
                std::make_format_args(v.x, v.y, v.z, v.w)
            )
        );
    }
    else if (type == "UVec2Property") {
        glm::uvec2 v = static_cast<properties::UVec2Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y))
        );
    }
    else if (type == "UVec3Property") {
        glm::uvec3 v = static_cast<properties::UVec3Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y, v.z))
        );
    }
    else if (type == "UVec4Property") {
        glm::uvec4 v = static_cast<properties::UVec4Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(
                _displayString.value(),
                std::make_format_args(v.x, v.y, v.z, v.w)
            )
        );
    }
    else if (type == "Vec2Property") {
        glm::vec2 v = static_cast<properties::Vec2Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y))
        );
    }
    else if (type == "Vec3Property") {
        glm::vec3 v = static_cast<properties::Vec3Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(v.x, v.y, v.z))
        );
    }
    else if (type == "Vec4Property") {
        glm::vec4 v = static_cast<properties::Vec4Property*>(_property)->value();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(
                _displayString.value(),
                std::make_format_args(v.x, v.y, v.z, v.w)
            )
        );
    }
    else {
        // Fallback if we don't have a special case above

        std::string value = _property->stringValue();
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_displayString.value(), std::make_format_args(value))
        );
    }
}

glm::vec2 DashboardItemPropertyValue::size() const {
    ZoneScoped;

    return _font->boundingBox(_displayString.value());
}

} // namespace openspace
