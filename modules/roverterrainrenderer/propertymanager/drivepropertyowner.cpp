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

#include <modules/roverterrainrenderer/propertymanager/drivepropertyowner.h>

//#include <openspace/interaction/interactionhandler.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/globebrowsing/src/basictypes.h>

#include <ghoul/lua/lua_helper.h>

namespace {
    const std::string _loggerCat = "DrivePropertyOwner";
    static const openspace::properties::Property::PropertyInfo enabledPropertyInfo = {
        "driveEnabled",
        "Drive enabled",
        "" // @TODO Missing documentation
    };
    static const openspace::properties::Property::PropertyInfo triggerPropertyInfo = {
        "trigger",
        "Go to Subsite",
        "" // @TODO Missing documentation
    };
}

namespace openspace {
    DrivePropertyOwner::DrivePropertyOwner(std::string drive, std::string sol, glm::dvec2 driveCoords)
        : properties::PropertyOwner(openspace::properties::PropertyOwner::PropertyOwnerInfo{ "drive" + drive, "Drive" + drive + " (Sol: " + sol + ")" })
    , _enabled(properties::BoolProperty(enabledPropertyInfo, false))
    , _goToSubSite(properties::TriggerProperty(triggerPropertyInfo))
    , _drive(drive)
    , _driveCoords(driveCoords)
{
    _goToSubSite.onChange([&] { goToSubsite(_drive); });

    addProperty(_enabled);
    addProperty(_goToSubSite);
}

void DrivePropertyOwner::goToSubsite(std::string drive) {
    globebrowsing::Geodetic2 tempGeo;
    tempGeo.lat = _driveCoords.x * 180.0 / glm::pi<double>();
    tempGeo.lon = _driveCoords.y * 180.0 / glm::pi<double>();
    
    //OsEng.ref().interactionHandler().goToGeo(tempGeo.lat, tempGeo.lon);
}

} // namespace openspace

