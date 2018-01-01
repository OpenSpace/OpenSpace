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

#include <ghoul/misc/templatefactory.h>

namespace {
    const char* KeyType = "Type";

    static const openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "If this value is set to 'true' this dashboard item is shown in the dashboard"
    };
} // namespace

namespace openspace {

std::unique_ptr<DashboardItem> DashboardItem::createFromDictionary(
                                                             ghoul::Dictionary dictionary)
{
    auto factory = FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(factory, "DashboardItem factory did not exist");

    std::string dashboardType = dictionary.value<std::string>(KeyType);

    return factory->create(dashboardType, dictionary);
}

DashboardItem::DashboardItem(std::string name)
    : properties::PropertyOwner({ std::move(name) })
    , _isEnabled(EnabledInfo, true)
{
    addProperty(_isEnabled);
}

bool DashboardItem::isEnabled() const {
    return _isEnabled;
}

} // namespace openspace
