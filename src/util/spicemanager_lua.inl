/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/misc/csvreader.h>
#include <openspace/misc/exception.h>
#include <openspace/misc/stringhelper.h>
#include <openspace/format.h>
#include <openspace/util/timeconstants.h>
#include <openspace/util/timemanager.h>
#include <cstring>
#include <fstream>
#include <ios>
#include <iterator>
#include <memory>
#include <variant>
#include <utility>

using namespace openspace;

namespace {

/**
 * Loads the provided SPICE kernel by name. The name can contain path tokens, which are
 * automatically resolved.
 */
[[codegen::luawrap]] void loadKernel(
                               std::variant<std::string, std::vector<std::string>> kernel)
{
    if (std::holds_alternative<std::string>(kernel)) {
        std::string k = std::get<std::string>(kernel);
        if (!std::filesystem::is_regular_file(k)) {
            throw lua::LuaError(std::format("Kernel file '{}' did not exist", k));
        }
        SpiceManager::ref().loadKernel(k);
    }
    else {
        std::vector<std::string> ks = std::get<std::vector<std::string>>(kernel);
        for (const std::string& k : ks) {
            if (!std::filesystem::is_regular_file(k)) {
                throw lua::LuaError(std::format("Kernel file '{}' did not exist", k));
            }
            SpiceManager::ref().loadKernel(k);
        }
    }
}

/**
 * Unloads the provided SPICE kernel. The name can contain path tokens, which are
 * automatically resolved.
 */
[[codegen::luawrap]] void unloadKernel(
                               std::variant<std::string, std::vector<std::string>> kernel)
{
    if (std::holds_alternative<std::string>(kernel)) {
        SpiceManager::ref().unloadKernel(std::get<std::string>(kernel));
    }
    else {
        for (const std::string& k : std::get<std::vector<std::string>>(kernel)) {
            SpiceManager::ref().unloadKernel(k);
        }
    }
}

/**
 * Returns a list of all loaded kernels.
 */
[[codegen::luawrap]] std::vector<std::filesystem::path> kernels() {
    return SpiceManager::ref().loadedKernels();
}

/**
 * Returns a list of Spice Bodies loaded into the system. Returns SPICE built in frames if
 * builtInFrames. Returns User loaded frames if !builtInFrames.
 */
[[codegen::luawrap]] std::map<std::string, std::string> spiceBodies(bool includeBuiltIn) {
    std::vector<std::pair<int, std::string>> bodies =
        SpiceManager::ref().spiceBodies(includeBuiltIn);

    std::map<std::string, std::string> res;
    for (const std::pair<int, std::string>& p : bodies) {
        res[std::to_string(p.first)] = p.second;
    }
    return res;
}

/**
 * Returns the rotationMatrix for a given body in a frame of reference at a specific time.
 *
 * Example:
 * openspace.spice.rotationMatrix('INSIGHT_LANDER_CRUISE','MARS', '2018 NOV 26 19:45:34')
 */
[[codegen::luawrap]] glm::dmat3 rotationMatrix(std::string body, std::string frame,
                                               std::string date)
{
    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dmat3 rotationMatrix = SpiceManager::ref().frameTransformationMatrix(
        body,
        frame,
        ephemerisTime
    );
    return rotationMatrix;
}

/**
 * Returns the position for a given body relative to another body, in a given frame of
 * reference, at a specific time.
 *
 * Example:
 * openspace.spice.position('INSIGHT', 'MARS',' GALACTIC', '2018 NOV 26 19:45:34')
 */
[[codegen::luawrap]] glm::dvec3 position(std::string target, std::string observer,
                                         std::string frame, std::string date)
{
    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dvec3 position = SpiceManager::ref().targetPosition(
        target,
        observer,
        frame,
        {},
        ephemerisTime
    );
    return position;
}

/**
 * This function converts a TLE file into SPK format and saves it at the provided path.
 * The last parameter is only used if there are multiple craft specified in the provided
 * TLE file and is selecting which (0-based index) of the list to create a kernel from.
 *
 * This function returns the SPICE ID of the object for which the kernel was created.
 */
[[codegen::luawrap]] int convertTLEtoSPK(std::filesystem::path tle,
                                         std::filesystem::path spk,
                                         int elementToExtract = 0)
{
    if (!std::filesystem::exists(tle)) {
        throw RuntimeError(std::format("Could not find TLE file '{}'", tle));
    }

    // Code adopted from
    // https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getelm_c.html
    SpiceInt n = 0;

    //
    // First exact the constants required by a type 10 SPK kernel
    //

    std::array<double, 8> constants;
    // J2 gravitational harmonic for Earth
    bodvcd_c(399, "J2", 1, &n, &constants[0]);

    // J3 gravitational harmonic for Earth
    bodvcd_c(399, "J3", 1, &n, &constants[1]);

    // J4 gravitational harmonic for Earth
    bodvcd_c(399, "J4", 1, &n, &constants[2]);

    // Square root of the GM for Earth
    bodvcd_c(399, "KE", 1, &n, &constants[3]);

    // High altitude bound for atmospheric model
    bodvcd_c(399, "QO", 1, &n, &constants[4]);

    // Low altitude bound for atmospheric model
    bodvcd_c(399, "SO", 1, &n, &constants[5]);

    // Equatorial radius of the Earth
    bodvcd_c(399, "ER", 1, &n, &constants[6]);

    // Distance units/earth radius
    bodvcd_c(399, "AE", 1, &n, &constants[7]);

    //
    // Load the TLE file
    //
    std::ifstream f = std::ifstream(tle, std::ios::binary);
    std::string contents = std::string(
        std::istreambuf_iterator<char>(f),
        std::istreambuf_iterator<char>()
    );

    // The TLE files returned by Celestrak are of the 3-line variant where the first line
    // contains a human-readable name for the spacecraft files are encoded with Windows
    // line endings (CRLF)

    std::vector<std::string> lines = tokenizeString(contents, '\n');

    // Erase carriage return characters
    for (std::string& line : lines) {
        line.erase(std::remove(begin(line), end(line), '\r'), end(line));
    }

    const size_t nElements = lines.size() / 3;
    if (elementToExtract < 0 || elementToExtract >= static_cast<int>(nElements)) {
        throw RuntimeError(std::format(
            "Error loading {}. Element number {} requested, but only {} found",
            tle, elementToExtract, nElements
        ));
    }

    constexpr int TLEColumnWidth = 69;

    std::string line1 = lines[3 * elementToExtract + 1];
    if (line1.size() != TLEColumnWidth) {
        throw RuntimeError(std::format(
            "Illformed TLE file {}, expected {} characters per line, got {}",
            tle, TLEColumnWidth, line1.size()
        ));
    }
    std::string line2 = lines[3 * elementToExtract + 2];
    if (line2.size() != TLEColumnWidth) {
        throw RuntimeError(std::format(
            "Illformed TLE file {}, expected {} characters per line, got {}",
            tle, TLEColumnWidth, line2.size()
        ));
    }

    // Copy the lines into a format that SPICE understands
    SpiceChar spiceLines[2][TLEColumnWidth + 1];
    std::strcpy(spiceLines[0], line1.c_str());
    std::strcpy(spiceLines[1], line2.c_str());


    // Convert the Two Line Elements lines to the element sets
    SpiceDouble epoch;
    std::array<SpiceDouble, 10> elems = {};
    getelm_c(1950, TLEColumnWidth + 1, spiceLines, &epoch, elems.data());

    // The size of a type SPK10 spice kernel is not affected by the time validity, so we
    // just pick the greatest one
    constexpr SpiceDouble first = -std::numeric_limits<double>::max();
    constexpr SpiceDouble last = std::numeric_limits<double>::max();

    // Extract the body id
    std::vector<std::string> tokens = tokenizeString(line2, ' ');
    if (tokens.size() < 2) {
        throw RuntimeError(std::format(
            "Error parsing TLE file {}. Expected 8-9 elements in the second row, got {}",
            tle, tokens.size()
        ));
    }
    // Earth-orbiting spacecraft usually lack a DSN identification code, so the NAIF ID
    // is derived from the tracking ID assigned to it by NORAD via:
    //   NAIF ID = -100000 - NORAD ID
    int bodyId = -100000 - std::stoi(tokens[1]);


    // Write the elements to a new SPK file
    const SpiceInt nCommentCharacters = 0;
    std::string internalFileName = std::format("Type 10 SPK for {}", tle);
    std::string segmentId = "Segment";

    if (std::filesystem::exists(spk)) {
        std::filesystem::remove(spk);
    }

    std::string outFile = spk.string();
    SpiceInt handle;
    spkopn_c(outFile.c_str(), internalFileName.c_str(), nCommentCharacters, &handle);

    spkw10_c(
        handle,
        bodyId,
        399,
        "J2000",
        first,
        last,
        segmentId.c_str(),
        constants.data(),
        1,
        elems.data(),
        &epoch
    );

    spkcls_c(handle);

    return bodyId;
}

/**
 * This function converts a CSV file into SPK format and saves it at the provided path.
 * The last parameter is only used if there are multiple craft specified in the provided
 * CSV file and is selecting which (0-based index) of the list to create a kernel from.
 *
 * This function returns the SPICE ID of the object for which the kernel was created.
 */
[[codegen::luawrap]] int convertCSVtoSPK(std::filesystem::path csv,
                                         std::filesystem::path spk,
                                         int elementToExtract = 0)
{
    if (!std::filesystem::exists(csv)) {
        throw RuntimeError(std::format("Could not find CSV file '{}'", csv));
    }


    // The order of the columns are important as the SPICE conversion method requires the
    // values to be in a specific order. The first block is treated separately from the
    // second block, which is individually processed
    const std::vector<std::string> columns = {
        "MEAN_MOTION_DOT",
        "MEAN_MOTION_DDOT",
        "BSTAR",
        "INCLINATION",
        "RA_OF_ASC_NODE",
        "ECCENTRICITY",
        "ARG_OF_PERICENTER",
        "MEAN_ANOMALY",
        "MEAN_MOTION",

        "EPOCH",
        "NORAD_CAT_ID"
    };
    std::vector<std::vector<std::string>> parameters = loadCSVFile(csv, columns);
    if (elementToExtract >= static_cast<int>(parameters.size())) {
        throw RuntimeError(std::format(
            "Requested element {} but only {} elements were found",
            elementToExtract, parameters.size()
        ));
    }

    std::vector<std::string> p = parameters[elementToExtract];
    std::array<SpiceDouble, 10> elems = {};

    // Converting the strings to numbers
    for (size_t i = 0; i < 9; i++) {
        std::string_view str = p[i];
        double value = 0.0;
        auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), value);
        if (ptr != str.data() + str.size() || ec != std::errc()) {
            throw RuntimeError(std::format(
                "Error parsing '{}' of CSV file with error '{}'",
                columns[i], std::make_error_code(ec).message()
            ));
        }
        elems[i] = value;
    }

    // Need to convert the units from the OMM format into Spice format according to the
    // tables at https://ccsds.org/Pubs/502x0b3e1.pdf and
    // https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getelm_c.html

    constexpr double MinutesPerDay = 24.0 * 60.0;

    // MeanMotionDot:  rev/day^2 -> radians/minute^2
    elems[0] = elems[0] * glm::two_pi<double>() / MinutesPerDay / MinutesPerDay;

    // MeanMotionDotDot:  rev/day^2 -> radians/minute^3
    elems[1] =
        elems[1] * glm::two_pi<double>() / MinutesPerDay / MinutesPerDay / MinutesPerDay;

    // Nothing to do with B*

    // Inclination:  Deg -> rad
    elems[3] = glm::radians(elems[3]);

    // Ra of Ascending Node:  Deg -> rad
    elems[4] = glm::radians(elems[4]);

    // Nothing to do with eccentricity

    // Arg of pericenter:  Deg -> rad
    elems[6] = glm::radians(elems[6]);

    // Mean Anomaly:  Deg -> rad
    elems[7] = glm::radians(elems[7]);

    // Mean Motion:  rev/day -> radians/minute
    elems[8] = elems[8] * glm::two_pi<double>() / MinutesPerDay;


    // Extract the epoch
    SpiceDouble epoch = SpiceManager::ref().ephemerisTimeFromDate(p[9]);
    elems[9] = epoch;


    // Code adopted from
    // https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getelm_c.html
    SpiceInt n = 0;

    //
    // First extract the constants required by a type 10 SPK kernel
    //

    std::array<double, 8> constants;
    // J2 gravitational harmonic for Earth
    bodvcd_c(399, "J2", 1, &n, &constants[0]);

    // J3 gravitational harmonic for Earth
    bodvcd_c(399, "J3", 1, &n, &constants[1]);

    // J4 gravitational harmonic for Earth
    bodvcd_c(399, "J4", 1, &n, &constants[2]);

    // Square root of the GM for Earth
    bodvcd_c(399, "KE", 1, &n, &constants[3]);

    // High altitude bound for atmospheric model
    bodvcd_c(399, "QO", 1, &n, &constants[4]);

    // Low altitude bound for atmospheric model
    bodvcd_c(399, "SO", 1, &n, &constants[5]);

    // Equatorial radius of the Earth
    bodvcd_c(399, "ER", 1, &n, &constants[6]);

    // Distance units/earth radius
    bodvcd_c(399, "AE", 1, &n, &constants[7]);

    // The size of a type SPK10 spice kernel is not affected by the time validity, so we
    // just pick the greatest one
    constexpr SpiceDouble first = -std::numeric_limits<double>::max();
    constexpr SpiceDouble last = std::numeric_limits<double>::max();

    // Earth-orbiting spacecraft usually lack a DSN identification code, so the NAIF ID
    // is derived from the tracking ID assigned to it by NORAD via:
    //   NAIF ID = -100000 - NORAD ID
    int bodyId = -100000 - std::stoi(p[10]);

    // Write the elements to a new SPK file
    const SpiceInt nCommentCharacters = 0;
    std::string internalFileName = std::format("Type 10 SPK for {}", csv);
    std::string segmentId = "Segment";

    if (std::filesystem::exists(spk)) {
        std::filesystem::remove(spk);
    }

    std::string outFile = spk.string();
    SpiceInt handle;
    spkopn_c(outFile.c_str(), internalFileName.c_str(), nCommentCharacters, &handle);

    spkw10_c(
        handle,
        bodyId,
        399,
        "J2000",
        first,
        last,
        segmentId.c_str(),
        constants.data(),
        1,
        elems.data(),
        &epoch
    );

    spkcls_c(handle);

    return bodyId;
}

/**
 * Computes the ephemeris time of a sunrise or sunset on the given date/location. If
 * \p date is empty, the local date at the current simulation time is used. Only the date
 * part of \p date is used.
 */
double solarEventOrThrow(double latitude, double longitude, std::string observer,
                         double altitude, std::string date,
                         SpiceManager::IsSunrise isSunrise)
{
    if (date.empty()) {
        // Convert the simulation time into local mean solar time at the longitude
        const double localTimeOffset =
            longitude / 360.0 * timeconstants::SecondsPerDay;
        date = SpiceManager::ref().dateFromEphemerisTime(
            global::timeManager->time().j2000Seconds() + localTimeOffset,
            "YYYY-MM-DD"
        );
    }
    const std::string day = date.substr(0, date.find('T'));
    const std::optional<double> time = SpiceManager::ref().solarEventTime(
        latitude, longitude, altitude, day, observer, isSunrise
    );
    if (!time.has_value()) {
        throw lua::LuaError(std::format(
            "No {} found for latitude {}, longitude {} on '{}' at '{}'. Possible reasons "
            "are polar day or night, an unknown body, an invalid date or missing SPICE "
            "data. Enable 'Show exceptions' of the Space module to see the SPICE error",
            isSunrise ? "sunrise" : "sunset", latitude, longitude, day, observer
        ));
    }
    return *time;
}

/**
 * Returns the ephemeris time (ET seconds past J2000) of the sunrise for a given date and
 * location. The date (YYYY-MM-DD) is the local solar date at the longitude, which is UTC
 * plus longitude / 15 hours. If no date is specified, the local date at the current
 * simulation time is used. The `observer` is the SPICE name of the body, for example
 * `EARTH`. The sunrise is the moment the center of the Sun crosses the geometric horizon,
 * so atmospheric refraction and the apparent size of the Sun are not taken into account.
 * An error is raised if there is no sunrise on that day, for example during polar day or
 * night.
 *
 * Example:
 * openspace.spice.sunriseTime(40.78, -73.97, 'EARTH', 0.0, '2024-06-21')
 */
[[codegen::luawrap]] double sunriseTime(double latitude, double longitude,
                                        std::string observer, double altitude = 0.0,
                                        std::string date = "")
{
    return solarEventOrThrow(
        latitude,
        longitude,
        std::move(observer),
        altitude,
        std::move(date),
        SpiceManager::IsSunrise::Yes
    );
}

/**
 * Returns the ephemeris time (ET seconds past J2000) of the sunset for a given date and
 * location. The date (YYYY-MM-DD) is the local solar date at the longitude, which is UTC
 * plus longitude / 15 hours. If no date is specified, the local date at the current
 * simulation time is used. The `observer` is the SPICE name of the body, for example
 * `EARTH`. The sunset is the moment the center of the Sun crosses the geometric horizon,
 * so atmospheric refraction and the apparent size of the Sun are not taken into account.
 * An error is raised if there is no sunset on that day, for example during polar day or
 * night.
 *
 * Example:
 * openspace.spice.sunsetTime(40.78, -73.97, 'EARTH', 0.0, '2024-06-21')
 */
[[codegen::luawrap]] double sunsetTime(double latitude, double longitude,
                                       std::string observer, double altitude = 0.0,
                                       std::string date = "")
{
    return solarEventOrThrow(
        latitude,
        longitude,
        std::move(observer),
        altitude,
        std::move(date),
        SpiceManager::IsSunrise::No
    );
}

} // namespace

#include "spicemanager_lua_codegen.cpp"
