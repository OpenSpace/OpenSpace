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

#include <modules/base/dashboard/dashboarditemtext.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextInfo = {
        "Text",
        "Text",
        "The text to be displayed.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemText)]] Parameters {
        // [[codegen::verbatim(TextInfo.description)]]
        std::optional<std::string> text;
    };
#include "dashboarditemtext_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemText::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_text",
        DashboardTextItem::Documentation()
    );
}

DashboardItemText::DashboardItemText(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _text(TextInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _text = p.text.value_or(_text);
    addProperty(_text);
}

void DashboardItemText::render(glm::vec2& penPosition) {
    ZoneScoped;

    penPosition.y -= _font->height();
    RenderFont(*_font, penPosition, _text.value());
}

glm::vec2 DashboardItemText::size() const {
    ZoneScoped;

    return _font->boundingBox(_text.value());
}

} // namespace openspace
