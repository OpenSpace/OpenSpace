/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/rendering/dashboarditem.h>

#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyType = "Type";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "If this value is set to 'true' this dashboard item is shown in the dashboard"
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo IdentifierInfo = {
        "Identifier",
        "Identifier",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo GuiNameInfo = {
        "GuiName",
        "Gui Name",
        ""
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItem::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem",
        "dashboarditem",
        {
            {
                TypeInfo.identifier,
                new StringVerifier,
                Optional::No,
                TypeInfo.description
            },
            {
                IdentifierInfo.identifier,
                new StringVerifier,
                Optional::No,
                IdentifierInfo.description
            },
            {
                GuiNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                GuiNameInfo.description
            }
        }
    };
}

std::unique_ptr<DashboardItem> DashboardItem::createFromDictionary(
                                                             ghoul::Dictionary dictionary)
{
    auto factory = FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(factory, "DashboardItem factory did not exist");

    const std::string& dashboardType = dictionary.value<std::string>(KeyType);

    DashboardItem* item = factory->create(dashboardType, std::move(dictionary));
    return std::unique_ptr<DashboardItem>(item);
}

DashboardItem::DashboardItem(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "", "" })
    , _isEnabled(EnabledInfo, true)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItem"
    );

    std::string identifier = dictionary.value<std::string>(IdentifierInfo.identifier);
    setIdentifier(std::move(identifier));

    if (dictionary.hasValue<std::string>(GuiNameInfo.identifier)) {
        std::string guiName = dictionary.value<std::string>(GuiNameInfo.identifier);
        setGuiName(std::move(guiName));
    }

    addProperty(_isEnabled);
}

bool DashboardItem::isEnabled() const {
    return _isEnabled;
}


namespace {
    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the distance."
    };
} // namespace

documentation::Documentation DashboardTextItem::Documentation() {
    using namespace documentation;
    return {
        "DashboardTextItem",
        "dashboardtextitem",
        {
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
            }
        }
    };
}

DashboardTextItem::DashboardTextItem(const ghoul::Dictionary& dictionary, float fontSize,
                                     const std::string& fontName)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, fontName)
    , _fontSize(FontSizeInfo, fontSize, 6.f, 144.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardTextItem"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(dictionary.value<double>(FontSizeInfo.identifier));
    }
    _fontSize.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = global::fontManager->font(_fontName, _fontSize);
}

} // namespace openspace
