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
} // namespace

namespace openspace {

HorizonsFile::HorizonsFile()
    : _file()
{}

HorizonsFile::HorizonsFile(std::filesystem::path file)
    : _file(std::move(file))
{}

void HorizonsFile::setFile(std::filesystem::path file) {
    _file = std::move(file);
}

const std::filesystem::path& HorizonsFile::file() const {
    return _file;
}

std::filesystem::path& HorizonsFile::file() {
    return _file;
}

HorizonsFile::ResultCode HorizonsFile::isValidAnswer(const json& answer) {
    // Signature, source and version
    auto signatureIt = answer.find("signature");
    if (signatureIt != answer.end()) {
        json signature = *signatureIt;

        auto sourceIt = signature.find("source");
        if (sourceIt != signature.end()) {
            if (*sourceIt != ApiSource) {
                LWARNING(fmt::format("Horizons answer from unkown source '{}'", *sourceIt));
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
            LWARNING("Could not find version information, version might not be supported");
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
            return HorizonsFile::ResultCode::ErrorSize;
        }
        // STEP_SIZE too big, exceeds available span.
        else if (errorMessage.find("STEP_SIZE too big") != std::string::npos) {
            return HorizonsFile::ResultCode::ErrorSpan;
        }
        // No ephemeris for target "X" after A.D. Y UT
        else if (errorMessage.find("No ephemeris for target") != std::string::npos) {
            return HorizonsFile::ResultCode::ErrorTimeRange;
        }
        // No site matches. Use "*@body" to list, "c@body" to enter coords, ?! for help.
        else if (errorMessage.find("No site matches") != std::string::npos ||
                 errorMessage.find("Cannot find central body") != std::string::npos)
        {
            return HorizonsFile::ResultCode::ErrorNoObserver;
        }
        // Observer table for X / Y->Y disallowed.
        else if (errorMessage.find("disallowed") != std::string::npos) {
            return HorizonsFile::ResultCode::ErrorObserverTargetSame;
        }
        // Insufficient ephemeris data has been loaded to compute the state of X
        // relative to Y at the ephemeris epoch Z;
        else if (errorMessage.find("Insufficient ephemeris data") != std::string::npos) {
            return HorizonsFile::ResultCode::ErrorNoData;
        }
        // #   E. Lon    DXY      DZ    Observatory Name;
        // -- - -------- ------ - ------ - ----------------;
        // * Observer station *
        // Multiple matching stations found.
        else if (errorMessage.find("Multiple matching stations found") != std::string::npos) {
            return HorizonsFile::ResultCode::MultipleObserverStations;
        }
        // Unknown error
        else {
            LERROR(errorMessage);
            return  HorizonsFile::ResultCode::UnknownError;
        }
    }
    return HorizonsFile::ResultCode::Valid;
}

// Check whether the given Horizons file is valid or not
// Return an error code with what is the problem if there was one
HorizonsFile::ResultCode HorizonsFile::isValidHorizonsFile() const {
    std::ifstream fileStream(_file);
    if (!fileStream.good()) {
        return HorizonsFile::ResultCode::Empty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that can tell us if the file is valid or not.
    // The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    std::getline(fileStream, line);
    std::getline(fileStream, line); // First line is just stars (*) no information, skip

    // Valid Target?
    if (fileStream.good() && (line.find("Revised") != std::string::npos || line.find("JPL") != std::string::npos)) {
        // If the target is valid, the first line is the date it was last revised
        // In case of comets it says the Source in the top
        foundTarget = true;
    }

    HorizonsFile::ResultCode result = HorizonsFile::ResultCode::UnknownError;
    while (fileStream.good() && line.find("$$SOE") == std::string::npos) {
        // Selected time range too big and step size too small?
        if (line.find("change step-size") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorSize;
        }

        // Selected time range too big for avalable time span?
        if (line.find("STEP_SIZE too big") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorSpan;
        }

        // Outside valid time range?
        if (line.find("No ephemeris for target") != std::string::npos) {
            // Available time range is located several lines before this in the file
            // The avalable time range is persed later
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorTimeRange;
        }

        // Valid Observer?
        if (line.find("No site matches") != std::string::npos ||
            line.find("Cannot find central body") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorNoObserver;
        }

        // Are observer and target the same?
        if (line.find("disallowed") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorObserverTargetSame;
        }

        // Enough data?
        if (line.find("Insufficient ephemeris data") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorNoData;
        }

        // Multiple Observer stations?
        if (line.find("Multiple matching stations found") != std::string::npos) {
            result = HorizonsFile::ResultCode::MultipleObserverStations;
        }

        // Multiple matching major bodies?
        if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target
            if (!foundTarget) {
                // If target was not found then it is the target that has multiple matches
                result = HorizonsFile::ResultCode::MultipleTarget;
            }
            // Observer
            else {
                result = HorizonsFile::ResultCode::MultipleObserver;
            }
        }

        // Multiple matching small bodies?
        if (line.find("Small-body Index Search Results") != std::string::npos) {
            // Small bodies can only be targets not observers
            result = HorizonsFile::ResultCode::MultipleTarget;
        }

        // No Target?
        if (line.find("No matches found") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::ResultCode::ErrorNoTarget;
        }

        std::getline(fileStream, line);
    }

    if (result != HorizonsFile::ResultCode::UnknownError) {
        fileStream.close();
        return result;
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    if (fileStream.good()) {
        fileStream.close();
        return HorizonsFile::ResultCode::Valid;
    }
    else {
        fileStream.close();
        return HorizonsFile::ResultCode::UnknownError;
    }
}

void HorizonsFile::displayErrorMessage(HorizonsFile::ResultCode code) const {
    switch (code) {
        case openspace::HorizonsFile::ResultCode::Valid:
            return;

        case openspace::HorizonsFile::ResultCode::Empty:
            LERROR("The horizons file is empty");
            break;

        case openspace::HorizonsFile::ResultCode::ErrorSize: {
            LERROR("The selected time range with the selected step size is too big, "
                "try to increase the step size and/or decrease the time range");
            break;
        }

        case openspace::HorizonsFile::ResultCode::ErrorSpan:
            LERROR("Step size is too big, exceeds available time span for target");
            break;

        case openspace::HorizonsFile::ResultCode::ErrorTimeRange: {
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

        case openspace::HorizonsFile::ResultCode::ErrorNoObserver:
            LERROR("No match was found for the observer");
            break;

        case openspace::HorizonsFile::ResultCode::ErrorObserverTargetSame:
            LERROR("The observer and target are the same");
            break;

        case openspace::HorizonsFile::ResultCode::ErrorNoData:
            LERROR("There is not enough data to compute the state of target in relation "
                "to the observer for the selected time range.");
            break;

        case openspace::HorizonsFile::ResultCode::MultipleObserverStations: {
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

        case openspace::HorizonsFile::ResultCode::MultipleObserver: {
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

        case openspace::HorizonsFile::ResultCode::ErrorNoTarget:
            LERROR("No match was found for the target");
            break;

        case openspace::HorizonsFile::ResultCode::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data: (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            LWARNING("Multiple matches was found for the target");

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

        case openspace::HorizonsFile::ResultCode::UnknownError:
            LERROR("An unknown error occured");
            break;

        default:
            LERROR("Unknown result type");
            break;
    }
}

HorizonsFile::HorizonsResult HorizonsFile::readFile() {
    std::ifstream fileStream(_file);

    if (!fileStream.good()) {
        LERROR(fmt::format(
            "Failed to open Horizons text file '{}'", _file
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

HorizonsFile::HorizonsResult HorizonsFile::readVectorFile() {
    HorizonsResult result;
    result.type = HorizonsFile::Type::Vector;
    result.errorCode = isValidHorizonsFile();
    std::vector<HorizonsKeyframe> data;

    if (result.errorCode != ResultCode::Valid) {
        return result;
    }

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

HorizonsFile::HorizonsResult HorizonsFile::readObserverFile() {
    HorizonsResult result;
    result.type = HorizonsFile::Type::Observer;
    result.errorCode = isValidHorizonsFile();
    std::vector<HorizonsKeyframe> data;

    if (result.errorCode != ResultCode::Valid) {
        return result;
    }

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
