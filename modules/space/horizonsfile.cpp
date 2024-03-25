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

#include <modules/space/horizonsfile.h>

#include <openspace/util/httprequest.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <filesystem>
#include <fstream>

using json = nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "HorizonsFile";
    constexpr std::string_view ApiSource = "NASA/JPL Horizons API";
    constexpr std::string_view CurrentMajorVersion = "1";

    // Values needed to construct the url for the http request to JPL Horizons API
    constexpr std::string_view VectorUrl = "https://ssd.jpl.nasa.gov/api/horizons.api?"
        "format=json&MAKE_EPHEM='YES'&EPHEM_TYPE='VECTORS'&VEC_TABLE='1'&VEC_LABELS='NO'&"
        "CSV_FORMAT='NO'";
    constexpr std::string_view ObserverUrl = "https://ssd.jpl.nasa.gov/api/horizons.api?"
        "format=json&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&QUANTITIES='20,33'&"
        "RANGE_UNITS='KM'&SUPPRESS_RANGE_RATE='YES'&CSV_FORMAT='NO'";
    constexpr std::string_view Command = "&COMMAND=";
    constexpr std::string_view Center = "&CENTER=";
    constexpr std::string_view StartTime = "&START_TIME=";
    constexpr std::string_view StopTime = "&STOP_TIME=";
    constexpr std::string_view StepSize = "&STEP_SIZE=";
} // namespace

namespace openspace {

HorizonsFile::HorizonsFile(std::filesystem::path file)
    : _file(std::move(file))
{}

HorizonsFile::HorizonsFile(std::filesystem::path filePath, std::string result)
    : _file(std::move(filePath))
{
    // Write the response into a new file and save it
    std::ofstream file(_file);
    file << ghoul::replaceAll(std::move(result), "\\n", "\n") << '\n';
}

void HorizonsFile::setFile(std::filesystem::path file) {
    _file = std::move(file);
}

const std::filesystem::path& HorizonsFile::file() const {
    return _file;
}

std::string constructHorizonsUrl(HorizonsType type, const std::string& target,
                                 const std::string& observer,
                                 const std::string& startTime,
                                 const std::string& stopTime, const std::string& stepSize,
                                 const std::string& unit)
{
    // Construct url for request
    std::string url;
    switch (type) {
        case HorizonsType::Vector:
            url = VectorUrl;
            break;
        case HorizonsType::Observer:
            url = ObserverUrl;
            break;
        case HorizonsType::Invalid:
            break;
    }

    url += std::format(
        "{}'{}'{}'{}'{}'{}'{}'{}'",
        Command, ghoul::encodeUrl(target),
        Center, ghoul::encodeUrl(observer),
        StartTime, ghoul::encodeUrl(startTime),
        StopTime, ghoul::encodeUrl(stopTime)
    );

    if (unit.empty()) {
        url += std::format("{}'{}'", StepSize, ghoul::encodeUrl(stepSize));
    }
    else {
        url += std::format(
            "{}'{}%20{}'", StepSize, ghoul::encodeUrl(stepSize), unit
        );
    }

    return url;
}

json sendHorizonsRequest(const std::string& url, const std::filesystem::path& filePath) {
    // Set up HTTP request and download result
    const auto download = std::make_unique<HttpFileDownload>(
        url,
        filePath,
        HttpFileDownload::Overwrite::Yes
    );

    HttpFileDownload* dl = download.get();
    dl->start();

    bool failed = false;
    dl->wait();
    if (!dl->hasSucceeded()) {
        LERROR(std::format("Error downloading horizons file with URL '{}'", dl->url()));
        failed = true;
    }

    if (failed) {
        dl->cancel();
    }

    return convertHorizonsDownloadToJson(filePath);
}

nlohmann::json convertHorizonsDownloadToJson(const std::filesystem::path& filePath) {
    // Read the entire file into a string
    constexpr size_t ReadSize = 4096;
    std::ifstream stream = std::ifstream(filePath);
    stream.exceptions(std::ios_base::badbit);

    std::string answer;
    std::string buf = std::string(ReadSize, '\0');
    while (stream.read(buf.data(), ReadSize)) {
        answer.append(buf, 0, stream.gcount());
    }
    answer.append(buf, 0, stream.gcount());

    // convert to a json object
    return json::parse(answer);
}

HorizonsResultCode isValidHorizonsAnswer(const json& answer) {
    // Signature, source and version
    if (auto signature = answer.find("signature");  signature != answer.end()) {
        if (auto source = signature->find("source");  source != signature->end()) {
            if (*source != static_cast<std::string>(ApiSource)) {
                LWARNING(std::format(
                    "Horizons answer from unknown source '{}'", source->dump()
                ));
            }
        }
        else {
            LWARNING("Could not find source information, source might not be acceptable");
        }

        if (auto version = signature->find("version");  version != signature->end()) {
            // Extract the major version from the version string
            std::string v = *version;
            v = v.substr(0, v.find('.'));

            if (v != CurrentMajorVersion) {
                LWARNING(std::format(
                    "Unknown Horizons major version '{}' found. The currently supported "
                    "major version is {}", version->dump(), CurrentMajorVersion
                ));
            }
        }
        else {
            LWARNING(
                "Could not find version information, version might not be supported"
            );
        }
    }
    else {
        LWARNING("Could not find signature information");
    }

    // Errors
    if (auto it = answer.find("error");  it != answer.end()) {
        // There was an error
        const std::string errorMsg = *it;

        // @CPP23 (malej, 2022-04-08) In all cases below, the string function contains
        // should be used instead of find

        // Projected output length (~X) exceeds 90024 line max -- change step-size
        if (errorMsg.find("Projected output length") != std::string::npos) {
            return HorizonsResultCode::ErrorSize;
        }
        // STEP_SIZE too big, exceeds available span.
        else if (errorMsg.find("STEP_SIZE too big") != std::string::npos) {
            return HorizonsResultCode::ErrorSpan;
        }
        // No ephemeris for target "X" after A.D. Y UT
        else if (errorMsg.find("No ephemeris for target") != std::string::npos) {
            return HorizonsResultCode::ErrorTimeRange;
        }
        // No site matches. Use "*@body" to list, "c@body" to enter coords, ?! for help.
        else if (errorMsg.find("No site matches") != std::string::npos ||
                 errorMsg.find("Cannot find central body") != std::string::npos)
        {
            return HorizonsResultCode::ErrorNoObserver;
        }
        // Observer table for X / Y->Y disallowed.
        else if (errorMsg.find("disallowed") != std::string::npos) {
            return HorizonsResultCode::ErrorObserverTargetSame;
        }
        // Insufficient ephemeris data has been loaded to compute the state of X
        // relative to Y at the ephemeris epoch Z;
        else if (errorMsg.find("Insufficient ephemeris data") != std::string::npos) {
            return HorizonsResultCode::ErrorNoData;
        }
        // #   E. Lon    DXY      DZ    Observatory Name;
        // -- - -------- ------ - ------ - ----------------;
        // * Observer station *
        // Multiple matching stations found.
        else if (errorMsg.find("Multiple matching stations found") != std::string::npos) {
            return HorizonsResultCode::MultipleObserverStations;
        }
        // Unknown error
        else {
            LERROR(errorMsg);
            return HorizonsResultCode::UnknownError;
        }
    }
    return HorizonsResultCode::Valid;
}

// Check whether the given Horizons file is valid or not
// Return an error code with what is the problem if there was one
HorizonsResultCode isValidHorizonsFile(const std::filesystem::path& file) {
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        return HorizonsResultCode::Empty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that can tell us if the file is valid or not.
    // The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    std::getline(fileStream, line);
    std::getline(fileStream, line); // First line is just stars (*) no information, skip

    // @CPP23 (malej, 2022-04-08) In all cases below, the string function contains
    // should be used instead of find

    // Valid Target?
    if (fileStream.good() && (line.find("Revised") != std::string::npos ||
        line.find("JPL") != std::string::npos))
    {
        // If the target is valid, the first line is the date it was last revised
        // In case of comets it says the Source in the top
        foundTarget = true;
    }

    HorizonsResultCode result = HorizonsResultCode::UnknownError;
    while (fileStream.good() && line.find("$$SOE") == std::string::npos) {
        // Selected time range too big and step size too small?
        if (line.find("change step-size") != std::string::npos) {
            return HorizonsResultCode::ErrorSize;
        }

        // Selected time range too big for avalable time span?
        if (line.find("STEP_SIZE too big") != std::string::npos) {
            return HorizonsResultCode::ErrorSpan;
        }

        // Outside valid time range?
        if (line.find("No ephemeris for target") != std::string::npos) {
            // Available time range is located several lines before this in the file
            // The avalable time range is persed later
            return HorizonsResultCode::ErrorTimeRange;
        }

        // Valid Observer?
        if (line.find("No site matches") != std::string::npos ||
            line.find("Cannot find central body") != std::string::npos)
        {
            return HorizonsResultCode::ErrorNoObserver;
        }

        // Are observer and target the same?
        if (line.find("disallowed") != std::string::npos) {
            return HorizonsResultCode::ErrorObserverTargetSame;
        }

        // Enough data?
        if (line.find("Insufficient ephemeris data") != std::string::npos) {
            return HorizonsResultCode::ErrorNoData;
        }

        // Multiple Observer stations?
        if (line.find("Multiple matching stations found") != std::string::npos) {
            result = HorizonsResultCode::MultipleObserverStations;
        }

        // Multiple matching major bodies?
        if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target
            if (!foundTarget) {
                // If target was not found then it is the target that has multiple matches
                result = HorizonsResultCode::MultipleTarget;
            }
            // Observer
            else {
                result = HorizonsResultCode::MultipleObserver;
            }
        }

        // Multiple matching small bodies?
        if (line.find("Small-body Index Search Results") != std::string::npos) {
            // Small bodies can only be targets not observers
            result = HorizonsResultCode::MultipleTarget;
        }

        // No Target?
        if (line.find("No matches found") != std::string::npos) {
            return HorizonsResultCode::ErrorNoTarget;
        }

        std::getline(fileStream, line);
    }

    if (result != HorizonsResultCode::UnknownError) {
        return result;
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    if (fileStream.good()) {
        return HorizonsResultCode::Valid;
    }
    else {
        return HorizonsResultCode::UnknownError;
    }
}

bool HorizonsFile::hasFile() const {
    return std::filesystem::is_regular_file(_file);
}

void HorizonsFile::displayErrorMessage(HorizonsResultCode code) const {
    switch (code) {
        case HorizonsResultCode::Valid:
            return;
        case HorizonsResultCode::Empty:
            LERROR("The horizons file is empty");
            break;
        case HorizonsResultCode::ErrorSize:
            LERROR(
                "The selected time range with the selected step size is too big, "
                "try to increase the step size and/or decrease the time range"
            );
            break;
        case HorizonsResultCode::ErrorSpan:
            LERROR("Step size is too big, exceeds available time span for target");
            break;
        case HorizonsResultCode::ErrorTimeRange: {
            LERROR("Time range is outside the valid range for target");

            std::pair<std::string, std::string> validTimeRange = parseValidTimeRange(
                "Trajectory files",
                "************",
                "Trajectory name"
            );
            if (validTimeRange.first.empty()) {
                LERROR("Could not parse the valid time range from file");
                break;
            }

            LINFO(std::format(
                "Valid time range is '{}' to '{}'",
                validTimeRange.first, validTimeRange.second
            ));
            break;
        }
        case HorizonsResultCode::ErrorNoObserver:
            LERROR("No match was found for the observer");
            break;
        case HorizonsResultCode::ErrorObserverTargetSame:
            LERROR("The observer and target are the same");
            break;
        case HorizonsResultCode::ErrorNoData:
            LERROR(
                "There is not enough data to compute the state of the target in "
                "relation to the observer for the selected time range"
            );
            break;
        case HorizonsResultCode::MultipleObserverStations: {
            LWARNING(
                "Multiple matching observer stations were found for the "
                "selected observer"
            );

            const std::vector<std::string> matchingstations =
                parseMatches("Observatory Name", "Multiple matching stations found");
            if (matchingstations.empty()) {
                LERROR("Could not parse the matching stations");
                break;
            }

            std::string matches;
            for (const std::string& station : matchingstations) {
                matches += '\n' + station;
            }
            LINFO(std::format("Matching Observer Stations: {}", matches));
            break;
        }
        case HorizonsResultCode::MultipleObserver: {
            LWARNING("Multiple matches were found for the selected observer");

            const std::vector<std::string> matchingObservers =
                parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingObservers.empty()) {
                LERROR("Could not parse the matching observers");
                break;
            }

            std::string matches;
            for (const std::string& observer : matchingObservers) {
                matches += '\n' + observer;
            }
            LINFO(std::format("Matching Observers: {}", matches));
            break;
        }
        case HorizonsResultCode::ErrorNoTarget:
            LERROR("No match was found for the target");
            break;
        case HorizonsResultCode::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data:
            // (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            LWARNING("Multiple matches were found for the target");

            const std::vector<std::string> matchingTargets =
                parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingTargets.empty()) {
                LERROR("Could not parse the matching targets");
                break;
            }

            std::string matches;
            for (const std::string& target : matchingTargets) {
                matches += '\n' + target;
            }
            LINFO(std::format("Matching targets: {}", matches));
            break;
        }
        case HorizonsResultCode::UnknownError:
            LERROR("An unknown error occured");
            break;
        default:
            LERROR("Unknown result type");
            break;
    }
}

HorizonsResult readHorizonsFile(std::filesystem::path file) {
    // Check if valid
    const HorizonsResultCode code = isValidHorizonsFile(file);
    if (code != HorizonsResultCode::Valid) {
        HorizonsResult result;
        result.errorCode = code;
        return result;
    }

    std::ifstream fileStream(file);

    if (!fileStream.good()) {
        LERROR(std::format("Failed to open Horizons file '{}'", file));
        return HorizonsResult();
    }

    // Identify which type the file is
    // Vector Table type has:"
    // JDTDB
    //   X     Y     Z
    // " Before data starts, Observer table doesn't
    std::string line;
    std::getline(fileStream, line);
    while (line[0] != '$') {
        if (line.starts_with("JDTDB")) {
            fileStream.close();
            return readHorizonsVectorFile(file);
        }

        std::getline(fileStream, line);
    }

    fileStream.close();
    return readHorizonsObserverFile(file);
}

HorizonsResult readHorizonsVectorFile(std::filesystem::path file) {
    HorizonsResult result;
    result.type = HorizonsType::Vector;
    result.errorCode = HorizonsResultCode::Valid;
    std::vector<HorizonsKeyframe> data;

    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        LERROR(std::format("Failed to open Horizons text file {}", file));
        return HorizonsResult();
    }

    // The beginning of a Horizons file has a header with a lot of information about the
    // query that we do not care about. Ignore everything until data starts, including
    // the row marked by $$SOE (i.e. Start Of Ephemerides).
    std::string line;
    do {
        std::getline(fileStream, line);
    } while (line[0] != '$');

    // Read data line by line until $$EOE (i.e. End Of Ephemerides).
    // Skip the rest of the file.
    std::getline(fileStream, line); // Skip the line with the $$EOE
    while (line[0] != '$') {
        HorizonsKeyframe dataPoint;
        std::stringstream str1(line);

        // File is structured as (data over two lines):
        // JulianDayNumber = A.D. YYYY-MM-DD HH:MM:SS TDB
        //   X Y Z
        std::string temp;
        std::string date;
        std::string time;
        str1 >> temp >> temp >> temp >> date >> time >> temp;

        // Get next line of same data point
        std::getline(fileStream, line);
        if (!fileStream.good()) {
            LERROR(std::format("Malformed Horizons file '{}'", file));
            return HorizonsResult();
        }
        std::stringstream str2(line);

        //   X Y Z
        double xPos = 0.0;
        double yPos = 0.0;
        double zPos = 0.0;
        str2 >> xPos >> yPos >> zPos;

        // Convert date and time to seconds after 2000
        const std::string timeString = std::format("{} {}", date, time);
        const double timeInJ2000 = Time::convertTime(timeString);
        glm::dvec3 pos = glm::dvec3(1000 * xPos, 1000 * yPos, 1000 * zPos);
        const glm::dmat3 transform =
            SpiceManager::ref().positionTransformMatrix("ECLIPJ2000", "GALACTIC", 0.0);
        pos = transform * pos;

        // Add position to stored data
        dataPoint.time = timeInJ2000;
        dataPoint.position = pos;
        data.push_back(dataPoint);

        std::getline(fileStream, line);
    }

    result.data = data;
    return result;
}

HorizonsResult readHorizonsObserverFile(std::filesystem::path file) {
    HorizonsResult result;
    result.type = HorizonsType::Observer;
    result.errorCode = HorizonsResultCode::Valid;
    std::vector<HorizonsKeyframe> data;

    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        LERROR(std::format("Failed to open Horizons text file '{}'", file));
        return HorizonsResult();
    }

    // The beginning of a Horizons file has a header with a lot of information about the
    // query that we do not care about. Ignore everything until data starts, including
    // the row marked by $$SOE (i.e. Start Of Ephemerides).
    std::string line;
    do {
        std::getline(fileStream, line);
    } while (line[0] != '$');

    // Read data line by line until $$EOE (i.e. End Of Ephemerides).
    // Skip the rest of the file.
    std::getline(fileStream, line); // Skip the line with the $$EOE
    while (line[0] != '$') {
        HorizonsKeyframe dataPoint;
        std::stringstream str(line);

        // File is structured by (all in one line):
        // YYYY-MM-DD
        // HH:MM:SS
        // Range-to-observer (km)
        // Range-delta (km/s) -- suppressed!
        // Galactic Longitude (degrees)
        // Galactic Latitude (degrees)
        std::string date;
        std::string time;
        double range = 0;
        double gLon = 0;
        double gLat = 0;
        str >> date >> time >> range >> gLon >> gLat;

        // Convert date and time to seconds after 2000
        // and pos to Galactic positions in meter from Observer.
        const std::string timeString = std::format("{} {}", date, time);

        // Add position to stored data
        dataPoint.time = Time::convertTime(timeString);
        dataPoint.position = glm::dvec3(
            1000 * range * cos(glm::radians(gLat)) * cos(glm::radians(gLon)),
            1000 * range * cos(glm::radians(gLat)) * sin(glm::radians(gLon)),
            1000 * range * sin(glm::radians(gLat))
        );
        data.push_back(dataPoint);

        std::getline(fileStream, line);
    }

    LWARNING(
        "Observer table data from Horizons might not align with SPICE data well. "
        "We recommend using Vector table data from Horizons instead"
    );

    result.data = data;
    return result;
}

std::vector<std::string> HorizonsFile::parseMatches(const std::string& startPhrase,
                                                    const std::string& endPhrase,
                                                  const std::string& altStartPhrase) const
{
    std::ifstream fileStream(_file);
    std::vector<std::string> matches;

    if (!fileStream.good()) {
        fileStream.close();
        return matches;
    }

    // @CPP23 (malej, 2022-04-08) In all cases below, the string function contains
    // should be used instead of find

    // Ignore everything until start of matches
    std::string line;
    while (fileStream.good()) {
        // Add the line with the start phrase first, to give context
        if (line.find(startPhrase) != std::string::npos) {
            matches.push_back(line);
            break;
        }
        else if (!altStartPhrase.empty() &&
                 line.find(altStartPhrase) != std::string::npos)
        {
            matches.push_back(line);
            break;
        }

        std::getline(fileStream, line);
    }

    if (!fileStream.good()) {
        fileStream.close();
        return std::vector<std::string>();
    }

    // There will be one empty line before the list of matches, skip
    std::getline(fileStream, line);
    std::getline(fileStream, line);
    while (fileStream.good()) {
        // End of matches or file
        if (line == " " || line.empty() || line.find(endPhrase) != std::string::npos) {
            fileStream.close();
            return matches;
        }

        matches.push_back(line);
        std::getline(fileStream, line);
    }

    fileStream.close();
    return std::vector<std::string>();
}

// Parse the valid time range from the horizons file
// Example of how it can look (MRO):
// Trajectory files (from MRO Nav., JPL)      Start (TDB)         End (TDB)
// --------------------------------------  -----------------  -----------------
// mro_cruise                              2005-Aug-12 12:42  2006-Mar-10 22:06
// mro_ab                                  2006-Mar-10 22:06  2006-Sep-12 06:40
// misc reconstruction(mro_psp1 - 61)      2006-Sep-12 06:40  2022-Jan-01 01:01
// mro_psp_rec                             2022-Jan-01 01:01  2022-Jan-30 22:40
// mro_psp                                 2022-Jan-30 22:40  2022-Apr-04 02:27
// *******************************************************************************
//
// Another example (Gaia):
// Trajectory name                                    Start         Stop
// --------------------------------------------    -----------  -----------
// ORB1_20220201_000001                            2013-Dec-19  2026-Sep-14
// *******************************************************************************
//
// Another example (Tesla):
// Trajectory name                       Start (TDB)          Stop (TDB)
// --------------------------------   -----------------  -----------------
// tesla_s10                          2018-Feb-07 03:00  2090-Jan-01 00:00
//
// So the number of trajectory files can differ and they can have time info or not.
// The first row is parsed for both the start and end time.
// All other lines are only parsed for the end time and updates the previously parsed end
// time. Assumes that there are no gaps in the data coverage and that the files are sorted
// in respect to time.
std::pair<std::string, std::string> HorizonsFile::parseValidTimeRange(
                                                           const std::string& startPhrase,
                                                             const std::string& endPhrase,
                                                        const std::string& altStartPhrase,
                                                                      bool hasTime) const
{
    std::ifstream fileStream(_file);

    if (!fileStream.good()) {
        return { "", "" };
    }

    // @CPP23 (malej, 2022-04-08) In all cases below, the string function contains
    // should be used instead of find

    // Ignore everything until head of time range list
    std::string line;
    std::getline(fileStream, line);
    while (fileStream.good()) {
        // Add the line with the start phrase first, to give context
        if (line.find(startPhrase) != std::string::npos) {
            break;
        }

        if (!altStartPhrase.empty() && line.find(altStartPhrase) != std::string::npos) {
            break;
        }

        std::getline(fileStream, line);
    }

    if (!fileStream.good()) {
        return { "", "" };
    }

    // There will be one empty line before the list of time ranges, skip
    std::getline(fileStream, line);

    // In the first file parse both start and end time
    // From the first line get the start time
    std::string startTime;
    std::string endTime;
    std::getline(fileStream, line);
    if (fileStream.good()) {
        std::stringstream str(line);

        // Read and save each word
        std::vector<std::string> words;
        std::string word;
        while (str >> word) {
            words.push_back(word);
        }

        // Parse time stamps backwards
        // Format: Trajectory file Name, Start, End (yyyy-mon-dd hh:mm)
        if (hasTime && words.size() > 4) {
            startTime = std::format(
                "{} T {}", words[words.size() - 4], words[words.size() - 3]
            );
            endTime = std::format(
                "{} T {}", words[words.size() - 2], words[words.size() - 1]
            );
        }
        else if (words.size() > 2){
            // Sometimes the format can be yyyy-mon-dd without time
            startTime = words[words.size() - 2];
            endTime = words[words.size() - 1];
        }
        else {
            return { "", "" };
        }
    }
    if (startTime.empty() || endTime.empty()) {
        return { "", "" };
    }

    // In the other lines only parse the end time and update it
    // Get the end time from the last trajectery
    while (fileStream.good()) {
        if (line.find(endPhrase) != std::string::npos || line.empty() || line == " ") {
            return { startTime, endTime };
        }

        // Read and save each word.
        std::stringstream str(line);
        std::vector<std::string> words;
        std::string word;
        while (str >> word) {
            words.push_back(word);
        }

        // Parse time stamps backwards
        // Format: Trajectory file Name, Start, End (yyyy-mon-dd hh:mm)
        if (hasTime && words.size() > 4) {
            endTime = std::format(
                "{} T {}", words[words.size() - 2], words[words.size() - 1]
            );
        }
        else if (words.size() > 2) {
            // Sometimes the format can be yyyy-mon-dd without time
            endTime = words[words.size() - 1];
        }
        else {
            return { "", "" };
        }

        std::getline(fileStream, line);
    }

    return { "", "" };
}

} // namespace openspace
