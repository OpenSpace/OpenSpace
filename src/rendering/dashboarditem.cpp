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

#include <openspace/rendering/dashboarditem.h>

#include <openspace/util/factorymanager.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
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

    return factory->create(dashboardType, std::move(dictionary));
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

    std::string guiName = dictionary.value<std::string>(GuiNameInfo.identifier);
    setGuiName(std::move(guiName));

    addProperty(_isEnabled);
}

bool DashboardItem::isEnabled() const {
    return _isEnabled;
}

} // namespace openspace
