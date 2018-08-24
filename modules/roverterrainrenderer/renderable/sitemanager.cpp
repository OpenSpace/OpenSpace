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

#include <modules/roverterrainrenderer/renderable/sitemanager.h>
#include <modules/roverterrainrenderer/propertymanager/sitepropertyowner.h>

namespace {
    const std::string _loggerCat = "SiteManager";
    static const openspace::properties::Property::PropertyInfo enabledPropertyInfo = {
        "SiteEnabled",
        "Enable Site",
        "" // @TODO Missing documentation
    };
}

namespace openspace {
    SiteManager::SiteManager(std::string name, std::vector<std::shared_ptr<Subsite>> ss)
        : properties::PropertyOwner(openspace::properties::PropertyOwner::PropertyOwnerInfo{ "siteEnabled", name })
        , _isEnabledProperty(enabledPropertyInfo, false)
{

    std::vector<SiteWithDrives> temp;
    std::vector<std::string> temp2;
    for (size_t i = 0; i < ss.size(); ++i) {
        if (std::find(temp2.begin(), temp2.end(), ss.at(i)->site) == temp2.end()) {
            SiteWithDrives test;
            test._site = ss.at(i)->site;

            temp2.push_back(ss.at(i)->site);
            temp.push_back(test);
        }
    }

    for (size_t i = 0; i < ss.size(); ++i) {
        for (size_t j = 0; j < temp.size(); ++j) {
            if (temp.at(j)._site == ss.at(i)->site) {
                temp.at(j)._drives.push_back(ss.at(i)->drive);
                temp.at(j).sols.push_back(ss.at(i)->sol);
                temp.at(j)._driveCoords.push_back(glm::dvec2(ss.at(i)->geodetic.lat, ss.at(i)->geodetic.lon));
            }
        }
    }
    for (size_t k = 0; k < temp.size(); ++k) {
        _sitePropertyOwner.push_back(std::make_shared<SitePropertyOwner>(temp.at(k)));
    }
    for (auto &s : _sitePropertyOwner) {
        addPropertySubOwner(s.get( ));
    }


    addProperty(_isEnabledProperty);
}

} // namespace openspace
