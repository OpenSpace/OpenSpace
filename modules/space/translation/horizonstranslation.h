/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___HORIZONSTRANSLATION___H__
#define __OPENSPACE_MODULE_SPACE___HORIZONSTRANSLATION___H__

#include <openspace/scene/translation.h>

#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/util/timeline.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/lua/luastate.h>
#include <modules/space/horizonsfile.h>
#include <memory>

namespace openspace {

namespace documentation { struct Documentation; }


/**
 * The HorizonsTranslation is based on text files generated from NASA JPL HORIZONS Website
 * (https://ssd.jpl.nasa.gov/horizons.cgi). The implementation supports both Vector
 * and Observer as Horizons data table.
 *
 * In case of Vector table data the implementation expects a file with format:
 * TIME(JulianDayNumber = A.D. YYYY-MM-DD HH:MM:SS TDB)
 *   X(km) Y(km) Z(km)
 * TIME - Only the "YYYY-MM-DD HH:MM:SS" part is of interest, the rest is ignored
 * X - X position in kilometers in Ecliptic J2000 reference frame
 * Y - Y position in kilometers in Ecliptic J2000 reference frame
 * Z - Z position in kilometers in Ecliptic J2000 reference frame
 * Changes required in the "Table Settings" for compatible data:
 *   1. Under "Select Output Quantities" choose option "Position components {x, y, z}
 *      only"
 *   2. Uncheck the "Vector labels" options
 *
 * In case of Observer table data the implementation expects a file with format:
 * TIME(YYYY-MM-DD HH:MM:SS) Range(km) GalLon(degrees) GalLat(degrees)
 * Range - The distance from target to observer in kilometers
 * GalLon - Galactic Longitude in degrees
 * GalLat - Galactic Latitude in degrees
 * Changes required in the "Table Settings" for compatible data:
 *   1. Under "Observer Table Settings" uncheck all options except
 *      "Observer range & range-rate" and "Galactic longitude & latitude"
 *   2. Change "Range units" to "kilometers (km)" instead of "astronomical units (au)"
 *   3. Check the "Suppress range-rate" option
 */
class HorizonsTranslation : public Translation {
public:
    explicit HorizonsTranslation(const ghoul::Dictionary& dictionary);

    glm::dvec3 position(const UpdateData& data) const override;

    static documentation::Documentation Documentation();

private:
    struct CacheKeyframe {
        double timestamp;
        std::array<double, 3> position;
    };

    void loadData();
    bool readHorizonsTextFile(HorizonsFile& horizonsFile);
    bool loadCachedFile(const std::filesystem::path& file);
    void saveCachedFile(const std::filesystem::path& file) const;

    properties::StringListProperty _horizonsFiles;
    ghoul::lua::LuaState _state;
    Timeline<glm::dvec3> _timeline;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___HORIZONSTRANSLATION___H__
