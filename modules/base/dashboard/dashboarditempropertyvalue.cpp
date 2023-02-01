/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
        "The URI of the property that is displayed in this dashboarditem"
    };

    constexpr openspace::properties::Property::PropertyInfo DisplayStringInfo = {
        "DisplayString",
        "Display String",
        "The String that is being displayed. It must either be empty (in which case only "
        "the value itself will be displayed), or it must contain extact one instance of "
        "{}, which will be replaced with the value of the property during rendering"
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
    ZoneScoped

    if (_propertyIsDirty) {
        _property = openspace::property(_propertyUri);
        _propertyIsDirty = false;
    }

    if (_property) {
        std::string value = _property->stringValue();
        RenderFont(
            *_font,
            penPosition,
            fmt::format(fmt::runtime(_displayString.value()), value)
        );
        penPosition.y -= _font->height();
    }
}

glm::vec2 DashboardItemPropertyValue::size() const {
    ZoneScoped

    return _font->boundingBox(_displayString.value());
}

} // namespace openspace
