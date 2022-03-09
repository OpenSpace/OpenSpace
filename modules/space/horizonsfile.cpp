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

#include <modules/space/horizonsfile.h>

#include <openspace/util/time.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <fstream>

#include <openspace/util/spicemanager.h>

namespace {
    constexpr const char* _loggerCat = "HorizonsFile";
    constexpr const char* ApiSource = "NASA/JPL Horizons API";
    constexpr const char* CurrentVersion = "1.1";

    // Values needed to construct the url for the http request to JPL Horizons interface
    constexpr const char* VectorUrl = "https://ssd.jpl.nasa.gov/api/horizons.api?format="
        "json&MAKE_EPHEM='YES'&EPHEM_TYPE='VECTORS'&VEC_TABLE='1'&VEC_LABELS='NO'&"
        "CSV_FORMAT='NO'";
    constexpr const char* ObserverUrl = "https://ssd.jpl.nasa.gov/api/horizons.api?"
        "format=json&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&QUANTITIES='20,33'&"
        "RANGE_UNITS='KM'&SUPPRESS_RANGE_RATE='YES'&CSV_FORMAT='NO'";
    constexpr const char* Command = "&COMMAND=";
    constexpr const char* Center = "&CENTER=";
    constexpr const char* StartTime = "&START_TIME=";
    constexpr const char* StopTime = "&STOP_TIME=";
    constexpr const char* StepSize = "&STEP_SIZE=";
    constexpr const char* WhiteSpace = "%20";

    std::string replaceAll(const std::string& string, const std::string& from,
                           const std::string& to)
    {
        if (from.empty())
            return "";

        std::string result = string;
        size_t startPos = 0;
        while ((startPos = result.find(from, startPos)) != std::string::npos) {
            result.replace(startPos, from.length(), to);

            // In case 'to' contains 'from', ex replacing 'x' with 'yx'
            startPos += to.length();
        }
        return result;
    }
} // namespace

namespace openspace {

HorizonsFile::HorizonsFile()
    : _file()
{}

HorizonsFile::HorizonsFile(std::filesystem::path file)
    : _file(std::move(file))
{}

HorizonsFile::HorizonsFile(std::filesystem::path filePath, const std::string& result) {
    // Write the response into a new file and save it
    std::ofstream file(filePath);
    file << replaceAll(result, "\\n", "\n") << std::endl;
    file.close();
    _file = std::move(filePath);
}

void HorizonsFile::setFile(std::filesystem::path file) {
    _file = std::move(file);
}

const std::filesystem::path& HorizonsFile::file() const {
    return _file;
}

std::filesystem::path& HorizonsFile::file() {
    return _file;
}

std::string HorizonsFile::constructUrl(HorizonsFile::Type type, const std::string& target,
    const std::string& observer, const std::string& startTime,
    const std::string& stopTime, const std::string& stepSize, const std::string& unit)
{
    // Construct url for request
    std::string url = "";
    switch (type) {
        case Type::Vector:
            url.append(VectorUrl);
            break;
        case Type::Observer:
            url.append(ObserverUrl);
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    url.append(Command);
    url.append("'");
    url.append(replaceAll(target, " ", WhiteSpace));
    url.append("'");

    url.append(Center);
    url.append("'");
    url.append(replaceAll(observer, " ", WhiteSpace));
    url.append("'");

    url.append(StartTime);
    url.append("'");
    url.append(replaceAll(startTime, " ", WhiteSpace));
    url.append("'");

    url.append(StopTime);
    url.append("'");
    url.append(replaceAll(stopTime, " ", WhiteSpace));
    url.append("'");

    url.append(StepSize);
    url.append("'");
    url.append(stepSize);
    if (!unit.empty()) {
        url.append(WhiteSpace);
        url.append(unit);
    }
    url.append("'");

    return url;
}

HorizonsFile::ResultCode HorizonsFile::isValidAnswer(const json& answer) {
    // Signature, source and version
    auto signatureIt = answer.find("signature");
    if (signatureIt != answer.end()) {
        json signature = *signatureIt;

        auto sourceIt = signature.find("source");
        if (sourceIt != signature.end()) {
            if (*sourceIt != ApiSource) {
                LWARNING(fmt::format("Horizons answer from unkown source '{}'",
                    *sourceIt)
                );
            }
        }
        else {
            LWARNING("Could not find source information, source might not be acceptable");
        }

        auto versionIt = signature.find("version");
        if (versionIt != signature.end()) {
            if (*versionIt != CurrentVersion) {
                LWARNING(fmt::format("Unknown Horizons version '{}' found. The "
                    "currently supported version is {}", *versionIt, CurrentVersion));
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
    auto it = answer.find("error");
    if (it != answer.end()) {
        // There was an error
        std::string errorMessage = *it;

        // Projected output length (~X) exceeds 90024 line max -- change step-size
        if (errorMessage.find("Projected output length") != std::string::npos) {
            return ResultCode::ErrorSize;
        }
        // STEP_SIZE too big, exceeds available span.
        else if (errorMessage.find("STEP_SIZE too big") != std::string::npos) {
            return ResultCode::ErrorSpan;
        }
        // No ephemeris for target "X" after A.D. Y UT
        else if (errorMessage.find("No ephemeris for target") != std::string::npos) {
            return ResultCode::ErrorTimeRange;
        }
        // No site matches. Use "*@body" to list, "c@body" to enter coords, ?! for help.
        else if (errorMessage.find("No site matches") != std::string::npos ||
                 errorMessage.find("Cannot find central body") != std::string::npos)
        {
            return ResultCode::ErrorNoObserver;
        }
        // Observer table for X / Y->Y disallowed.
        else if (errorMessage.find("disallowed") != std::string::npos) {
            return ResultCode::ErrorObserverTargetSame;
        }
        // Insufficient ephemeris data has been loaded to compute the state of X
        // relative to Y at the ephemeris epoch Z;
        else if (errorMessage.find("Insufficient ephemeris data") != std::string::npos) {
            return ResultCode::ErrorNoData;
        }
        // #   E. Lon    DXY      DZ    Observatory Name;
        // -- - -------- ------ - ------ - ----------------;
        // * Observer station *
        // Multiple matching stations found.
        else if (errorMessage.find("Multiple matching stations found") !=
                 std::string::npos)
        {
            return ResultCode::MultipleObserverStations;
        }
        // Unknown error
        else {
            LERROR(errorMessage);
            return  ResultCode::UnknownError;
        }
    }
    return ResultCode::Valid;
}

bool HorizonsFile::isEmpty() const {
    return !std::filesystem::is_regular_file(_file);
}

// Check whether the given Horizons file is valid or not
// Return an error code with what is the problem if there was one
HorizonsFile::ResultCode HorizonsFile::isValidHorizonsFile() const {
    std::ifstream fileStream(_file);
    if (!fileStream.good()) {
        return ResultCode::Empty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that can tell us if the file is valid or not.
    // The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    std::getline(fileStream, line);
    std::getline(fileStream, line); // First line is just stars (*) no information, skip

    // Valid Target?
    if (fileStream.good() &&
        (line.find("Revised") != std::string::npos ||
         line.find("JPL") != std::string::npos))
    {
        // If the target is valid, the first line is the date it was last revised
        // In case of comets it says the Source in the top
        foundTarget = true;
    }

    ResultCode result = ResultCode::UnknownError;
    while (fileStream.good() && line.find("$$SOE") == std::string::npos) {
        // Selected time range too big and step size too small?
        if (line.find("change step-size") != std::string::npos) {
            fileStream.close();
            return ResultCode::ErrorSize;
        }

        // Selected time range too big for avalable time span?
        if (line.find("STEP_SIZE too big") != std::string::npos) {
            fileStream.close();
            return ResultCode::ErrorSpan;
        }

        // Outside valid time range?
        if (line.find("No ephemeris for target") != std::string::npos) {
            // Available time range is located several lines before this in the file
            // The avalable time range is persed later
            fileStream.close();
            return ResultCode::ErrorTimeRange;
        }

        // Valid Observer?
        if (line.find("No site matches") != std::string::npos ||
            line.find("Cannot find central body") != std::string::npos)
        {
            fileStream.close();
            return ResultCode::ErrorNoObserver;
        }

        // Are observer and target the same?
        if (line.find("disallowed") != std::string::npos)
        {
            fileStream.close();
            return ResultCode::ErrorObserverTargetSame;
        }

        // Enough data?
        if (line.find("Insufficient ephemeris data") != std::string::npos)
        {
            fileStream.close();
            return ResultCode::ErrorNoData;
        }

        // Multiple Observer stations?
        if (line.find("Multiple matching stations found") != std::string::npos) {
            result = ResultCode::MultipleObserverStations;
        }

        // Multiple matching major bodies?
        if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target
            if (!foundTarget) {
                // If target was not found then it is the target that has multiple matches
                result = ResultCode::MultipleTarget;
            }
            // Observer
            else {
                result = ResultCode::MultipleObserver;
            }
        }

        // Multiple matching small bodies?
        if (line.find("Small-body Index Search Results") != std::string::npos) {
            // Small bodies can only be targets not observers
            result = ResultCode::MultipleTarget;
        }

        // No Target?
        if (line.find("No matches found") != std::string::npos) {
            fileStream.close();
            return ResultCode::ErrorNoTarget;
        }

        std::getline(fileStream, line);
    }

    if (result != ResultCode::UnknownError) {
        fileStream.close();
        return result;
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    if (fileStream.good()) {
        fileStream.close();
        return ResultCode::Valid;
    }
    else {
        fileStream.close();
        return ResultCode::UnknownError;
    }
}

void HorizonsFile::displayErrorMessage(const ResultCode code) const {
    switch (code) {
        case HorizonsFile::ResultCode::Valid:
            return;

        case ResultCode::Empty:
            LERROR("The horizons file is empty");
            break;

        case ResultCode::InvalidFormat:
            LERROR(fmt::format(
                "Format of file '{}' is invalid. Horizons files must have extension '.dat'",
                _file)
            );
            break;

        case ResultCode::ErrorSize:
            LERROR("The selected time range with the selected step size is too big, "
                "try to increase the step size and/or decrease the time range");
            break;

        case ResultCode::ErrorSpan:
            LERROR("Step size is too big, exceeds available time span for target");
            break;

        case ResultCode::ErrorTimeRange: {
            LERROR("Time range is outside the valid range for target");

            std::pair<std::string, std::string> validTimeRange =
                parseValidTimeRange("Trajectory files", "************");
            if (validTimeRange.first.empty()) {
                LERROR("Could not parse the valid time range from file");
                break;
            }

            LINFO("Valid time range is '" + validTimeRange.first + "' to '" +
                validTimeRange.second + "'");
            break;
        }

        case ResultCode::ErrorNoObserver:
            LERROR("No match was found for the observer");
            break;

        case ResultCode::ErrorObserverTargetSame:
            LERROR("The observer and target are the same");
            break;

        case ResultCode::ErrorNoData:
            LERROR("There is not enough data to compute the state of the target in "
                "relation to the observer for the selected time range.");
            break;

        case ResultCode::MultipleObserverStations: {
            LWARNING("Multiple matching observer stations were found for the "
                "selected observer");

            std::vector<std::string> matchingstations =
                parseMatches("Observatory Name", "Multiple matching stations found");
            if (matchingstations.empty()) {
                LERROR("Could not parse the matching stations");
                break;
            }

            std::string matches;
            for (std::string station : matchingstations) {
                matches += '\n' + station;
            }
            LINFO("Mathing Observer Stations:" + matches);
            break;
        }

        case ResultCode::MultipleObserver: {
            LWARNING("Multiple matches were found for the selected observer");

            std::vector<std::string> matchingObservers =
                parseMatches("Name", "matches");
            if (matchingObservers.empty()) {
                LERROR("Could not parse the matching observers");
                break;
            }

            std::string matches;
            for (std::string observer : matchingObservers) {
                matches += '\n' + observer;
            }
            LINFO("Mathing Observers:" + matches);
            break;
        }

        case ResultCode::ErrorNoTarget:
            LERROR("No match was found for the target");
            break;

        case ResultCode::MultipleTarget: {
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

            std::vector<std::string> matchingTargets =
                parseMatches("Name", "matches");
            if (matchingTargets.empty()) {
                LERROR("Could not parse the matching targets");
                break;
            }

            std::string matches;
            for (std::string target : matchingTargets) {
                matches += '\n' + target;
            }
            LINFO("Mathing targets:" + matches);
            break;
        }

        case ResultCode::UnknownError:
            LERROR("An unknown error occured");
            break;

        default:
            LERROR("Unknown result type");
            break;
    }
}

HorizonsFile::HorizonsResult HorizonsFile::readFile() const {
    // Check if extension is correct first
    if (_file.extension() != ".dat") {
        HorizonsResult result;
        result.errorCode = ResultCode::InvalidFormat;
        return result;
    }

    // Check if valid
    ResultCode code = isValidHorizonsFile();
    if (code != ResultCode::Valid) {
        HorizonsResult result;
        result.errorCode = code;
        return result;
    }

    std::ifstream fileStream(_file);

    if (!fileStream.good()) {
        LERROR(fmt::format(
            "Failed to open Horizons file '{}'", _file
        ));
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
        if (line == "JDTDB") {
            fileStream.close();
            return readVectorFile();
        }

        std::getline(fileStream, line);
    }

    fileStream.close();
    return readObserverFile();
}

HorizonsFile::HorizonsResult HorizonsFile::readVectorFile() const {
    HorizonsResult result;
    result.type = HorizonsFile::Type::Vector;
    result.errorCode = ResultCode::Valid;
    std::vector<HorizonsKeyframe> data;

    std::ifstream fileStream(_file);
    if (!fileStream.good()) {
        LERROR(fmt::format(
            "Failed to open Horizons text file '{}'", _file
        ));
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
        std::string temp;
        std::string date;
        std::string time;
        double xPos;
        double yPos;
        double zPos;

        // File is structured as (data over two lines):
        // JulianDayNumber = A.D. YYYY-MM-DD HH:MM:SS TDB
        //   X Y Z
        str1 >> temp >> temp >> temp >> date >> time >> temp;

        // Get next line of same data point
        std::getline(fileStream, line);
        if (!fileStream.good()) {
            LERROR(fmt::format(
                "Malformed Horizons file '{}'", _file
            ));
            return HorizonsResult();
        }
        std::stringstream str2(line);

        //   X Y Z
        str2 >> xPos >> yPos >> zPos;

        // Convert date and time to seconds after 2000
        std::string timeString = date + " " + time;
        double timeInJ2000 = Time::convertTime(timeString);
        glm::dvec3 pos = glm::dvec3(1000 * xPos, 1000 * yPos, 1000 * zPos);
        glm::dmat3 transform =
            SpiceManager::ref().positionTransformMatrix("ECLIPJ2000", "GALACTIC", 0.0);
        pos = transform * pos;

        // Add position to stored data
        dataPoint.time = timeInJ2000;
        dataPoint.position = pos;
        data.push_back(dataPoint);

        std::getline(fileStream, line);
    }
    fileStream.close();

    result.data = data;
    return result;
}

HorizonsFile::HorizonsResult HorizonsFile::readObserverFile() const {
    HorizonsResult result;
    result.type = HorizonsFile::Type::Observer;
    result.errorCode = ResultCode::Valid;
    std::vector<HorizonsKeyframe> data;

    std::ifstream fileStream(_file);
    if (!fileStream.good()) {
        LERROR(fmt::format(
            "Failed to open Horizons text file '{}'", _file
        ));
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
        std::string date;
        std::string time;
        double range = 0;
        double gLon = 0;
        double gLat = 0;

        // File is structured by (all in one line):
        // YYYY-MM-DD
        // HH:MM:SS
        // Range-to-observer (km)
        // Range-delta (km/s) -- suppressed!
        // Galactic Longitude (degrees)
        // Galactic Latitude (degrees)
        str >> date >> time >> range >> gLon >> gLat;

        // Convert date and time to seconds after 2000
        // and pos to Galactic positions in meter from Observer.
        std::string timeString = date + " " + time;
        double timeInJ2000 = Time::convertTime(timeString);
        glm::dvec3 gPos = glm::dvec3(
            1000 * range * cos(glm::radians(gLat)) * cos(glm::radians(gLon)),
            1000 * range * cos(glm::radians(gLat)) * sin(glm::radians(gLon)),
            1000 * range * sin(glm::radians(gLat))
        );

        // Add position to stored data
        dataPoint.time = timeInJ2000;
        dataPoint.position = gPos;
        data.push_back(dataPoint);

        std::getline(fileStream, line);
    }

    fileStream.close();

    LWARNING("Observer table data from Horizons might not align with SPICE data well. "
        "We recommend using Vector table data from Horizons instead"
    );

    result.data = data;
    return result;
}

std::vector<std::string> HorizonsFile::parseMatches(const std::string& startPhrase,
                                                    const std::string& endPhrase) const
{
    std::ifstream fileStream(_file);
    std::vector<std::string> matches;

    if (!fileStream.good()) {
        fileStream.close();
        return matches;
    }

    // Ignore everything until start of matches
    std::string line;
    while (fileStream.good()) {
        // Add the line with the start phrase first, to give context
        if (line.find(startPhrase) != std::string::npos) {
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

std::pair<std::string, std::string> HorizonsFile::parseValidTimeRange(
                       const std::string& startPhrase, const std::string& endPhrase) const
{
    std::ifstream fileStream(_file);

    if (!fileStream.good()) {
        fileStream.close();
        return std::pair<std::string, std::string>();
    }

    // Ignore everything until head of time range list
    std::string line;
    std::getline(fileStream, line);
    while (fileStream.good() && line.find(startPhrase) == std::string::npos) {
        std::getline(fileStream, line);
    }

    if (!fileStream.good()) {
        fileStream.close();
        return std::pair<std::string, std::string>();
    }

    // There will be one empty line before the list of time rnages, skip
    std::getline(fileStream, line);
    std::string startTime, endTime;

    // From the first line get the start time
    std::getline(fileStream, line);
    if (fileStream.good()) {
        std::stringstream str(line);

        // Read and save each word.
        std::vector<std::string> words;
        std::string word;
        while (str >> word)
            words.push_back(word);

        if (words.empty()) {
            return std::pair<std::string, std::string>();
        }

        // Parse time stamps backwards
        // Format: Trajectory file Name, Start, End (yyyy-mon-dd hh:mm)
        endTime = words[words.size() - 2] + " T " + words[words.size() - 1];
        startTime = words[words.size() - 4] + " T " + words[words.size() - 3];
    }
    if (startTime.empty() || endTime.empty()) {
        fileStream.close();
        return std::pair<std::string, std::string>();
    }

    // Get the end time from the last trajectery
    while (fileStream.good()) {
        if (line.find(endPhrase) != std::string::npos || line.empty() || line == " ") {
            fileStream.close();
            return std::pair<std::string, std::string>(startTime, endTime);
        }

        // Read and save each word.
        std::stringstream str(line);
        std::vector<std::string> words;
        std::string word;
        while (str >> word)
            words.push_back(word);

        if (words.empty()) {
            return std::pair<std::string, std::string>();
        }

        // Parse time stamps backwards
        // Format: Trajectory file Name, Start, End (yyyy-mon-dd hh:mm)
        endTime = words[words.size() - 2] + " T " + words[words.size() - 1];

        std::getline(fileStream, line);
    }

    fileStream.close();
    return std::pair<std::string, std::string>();
}

} // namespace openspace
