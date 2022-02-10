/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__
#define __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__

#include <openspace/json.h>
#include <filesystem>

using json = nlohmann::json;

namespace openspace {

/**
 * A Horizons file is a text file generated from NASA JPL HORIZONS Website
 * (https://ssd.jpl.nasa.gov/horizons.cgi). The implementation that reads these files
 * expects a file with format:
 * TIME(YYYY-MM-DD HH:MM:SS) Range(km) GalLon(degrees) GalLat(degrees)
 * Range - The distance from target to observer. Enabled with "Observer range &
 * range-rate" (nr 20) in "Table Setting". This also generates a delta that must be
 * suppressed under "Additional Table Settings". User must set output settings to
 * kilometers under "Range units".
 * GalLon - Galactic Longitude. Enabled with "Galactic longitude & latitude" (nr 33) in
 * "Table Setting".
 * GalLat - Galactic Latitude. Output is by default set to degrees.
 * Make sure that no other settings are enables in the "Table Setting" than the ones
 * descripbed above
 */
class HorizonsFile {
public:
    enum class HorizonsResult {
        Valid,
        Empty,
        ErrorVersion,
        ErrorSource,

        // Erros caught by the error field in the json output
        ErrorSize,
        ErrorSpan,
        ErrorTimeRange,
        ErrorNoObserver,
        ErrorObserverTargetSame,
        ErrorNoData,
        MultipleObserverStations,

        // Erros/problems NOT caught by the error field in the json output
        MultipleObserver,
        ErrorNoTarget,
        MultipleTarget,

        UnknownError
    };

    HorizonsFile();
    HorizonsFile(std::filesystem::path file);

    void setFile(std::filesystem::path file);
    const std::filesystem::path& file() const;
    std::filesystem::path& file();

    static HorizonsResult isValidAnswer(const json& answer);
    HorizonsResult isValidHorizonsFile() const;

private:
    std::filesystem::path _file;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__
