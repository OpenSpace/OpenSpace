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

#include <openspace/rendering/dashboardtextitem.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <ghoul/font/fontmanager.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the distance.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardTextItem)]] Parameters {
        // [[codegen::verbatim(FontNameInfo.description)]]
        std::optional<std::string> fontName;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;
    };
#include "dashboardtextitem_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardTextItem::Documentation() {
    return codegen::doc<Parameters>("dashboardtextitem");
}

DashboardTextItem::DashboardTextItem(const ghoul::Dictionary& dictionary, float fontSize,
                                     const std::string& fontName)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, fontName)
    , _fontSize(FontSizeInfo, fontSize, 6.f, 144.f, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _fontName = p.fontName.value_or(_fontName);
    _fontName.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = global::fontManager->font(_fontName, _fontSize);
}

} // namespace openspace
