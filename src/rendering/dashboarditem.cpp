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

#include <openspace/rendering/dashboarditem.h>

#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/templatefactory.h>
#include <optional>

namespace {
    constexpr std::string_view KeyType = "Type";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "If this value is set to 'true' this dashboard item is shown in the dashboard.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(DashboardItem)]] Parameters {
        std::string type;

        std::string identifier [[codegen::identifier()]];

        std::optional<std::string> guiName;

        std::optional<bool> enabled;
    };
#include "dashboarditem_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItem::Documentation() {
    return codegen::doc<Parameters>("dashboarditem");
}

std::unique_ptr<DashboardItem> DashboardItem::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    ghoul::TemplateFactory<DashboardItem>* factory =
        FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(factory, "DashboardItem factory did not exist");

    const std::string& dashboardType = dictionary.value<std::string>(KeyType);

    DashboardItem* item = factory->create(dashboardType, dictionary);
    item->_type = dashboardType;

    return std::unique_ptr<DashboardItem>(item);
}

DashboardItem::DashboardItem(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "", "" })
    , _enabled(EnabledInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    setIdentifier(p.identifier);
    if (p.guiName.has_value()) {
        setGuiName(*p.guiName);
    }

    _enabled = p.enabled.value_or(_enabled);
    addProperty(_enabled);
}

bool DashboardItem::isEnabled() const {
    return _enabled;
}

} // namespace openspace
