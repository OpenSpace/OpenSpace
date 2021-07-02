/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/properties/stringproperty.h>
#include <openspace/util/timeline.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/lua/luastate.h>
#include <memory>

namespace openspace {

namespace documentation { struct Documentation; }


/**
 * The HorizonsTranslation is based on text files generated from NASA JPL HORIZONS Website
 * (https://ssd.jpl.nasa.gov/horizons.cgi). The implementation expects a file with format:
 * TIME(YYYY-MM-DD HH:MM:SS) Range(km) GalLon(degrees) GalLat(degrees)
 * Range - The distance from target to observer. Chosen as "Observer range & range-rate"
 * in "Table Setting". This also generates a delta that can be suppressed under "Optional
 * observer-table settings" to limit file size. User must set output settings to
 * kilometers.
 * GalLon - Galactic Longitude. User must set output to Degrees in "Table Settings".
 * GalLat - Galactic Latitude. User must set output to Degrees in "Table Settings".
 */
class HorizonsTranslation : public Translation {
public:
    HorizonsTranslation();
    HorizonsTranslation(const ghoul::Dictionary& dictionary);

    glm::dvec3 position(const UpdateData& data) const override;

    static documentation::Documentation Documentation();

private:
    void loadData();
    void readHorizonsTextFile();
    bool loadCachedFile(const std::filesystem::path& file);
    void saveCachedFile(const std::filesystem::path& file) const;

    properties::StringProperty _horizonsTextFile;
    std::unique_ptr<ghoul::filesystem::File> _fileHandle;
    ghoul::lua::LuaState _state;
    Timeline<glm::dvec3> _timeline;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___HORIZONSTRANSLATION___H__
