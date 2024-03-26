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

#ifndef __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__
#define __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__

#include <openspace/json.h>
#include <ghoul/glm.h>
#include <filesystem>
#include <string>
#include <vector>

namespace openspace {

/**
 * A Horizons file is a text file generated from NASA JPL HORIZONS Website
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
enum class HorizonsResultCode {
    Valid,
    Empty,

    // Errors caught by the error field in the json output
    ErrorSize,
    ErrorSpan,
    ErrorTimeRange,
    ErrorNoObserver,
    ErrorObserverTargetSame,
    ErrorNoData,
    MultipleObserverStations,

    // Errors/problems NOT caught by the error field in the json output
    MultipleObserver,
    ErrorNoTarget,
    MultipleTarget,

    UnknownError
};

enum class HorizonsType {
    Observer, // Default
    Vector,   // Default for sending for new data
    Invalid   // If errors or empty etc
};

struct HorizonsKeyframe {
    double time;            // J2000 seconds
    glm::dvec3 position;    // GALACTIC cartesian coordinates in meters
};

struct HorizonsResult {
    HorizonsType type = HorizonsType::Invalid;
    HorizonsResultCode errorCode = HorizonsResultCode::UnknownError;
    std::vector<HorizonsKeyframe> data;
};

class HorizonsFile {
public:
    HorizonsFile() = default;
    HorizonsFile(std::filesystem::path file);
    HorizonsFile(std::filesystem::path filePath, std::string result);

    void setFile(std::filesystem::path file);
    const std::filesystem::path& file() const;

    bool hasFile() const;
    void displayErrorMessage(HorizonsResultCode code) const;


    std::vector<std::string> parseMatches(const std::string& startPhrase,
        const std::string& endPhrase, const std::string& altStartPhrase = "") const;
    std::pair<std::string, std::string> parseValidTimeRange(
        const std::string& startPhrase, const std::string& endPhrase,
        const std::string& altStartPhrase = "", bool hasTime = true) const;

private:
    std::filesystem::path _file;
};

// Free functions
std::string constructHorizonsUrl(HorizonsType type, const std::string& target,
    const std::string& observer, const std::string& startTime,
    const std::string& stopTime, const std::string& stepSize,
    const std::string& unit);
nlohmann::json sendHorizonsRequest(const std::string& url,
    const std::filesystem::path& filePath);
nlohmann::json convertHorizonsDownloadToJson(const std::filesystem::path& filePath);
HorizonsResultCode isValidHorizonsAnswer(const nlohmann::json& answer);
HorizonsResultCode isValidHorizonsFile(const std::filesystem::path& file);
HorizonsResult readHorizonsFile(std::filesystem::path file);

HorizonsResult readHorizonsVectorFile(std::filesystem::path file);
HorizonsResult readHorizonsObserverFile(std::filesystem::path file);

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___HORIZONSFILE___H__
