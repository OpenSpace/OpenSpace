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

#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <fstream>

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
    _file = file;
}

const std::filesystem::path& HorizonsFile::file() const {
    return _file;
}

std::filesystem::path& HorizonsFile::file() {
    return _file;
}

HorizonsFile::HorizonsResult HorizonsFile::isValidAnswer(const json& answer) {
    // Signature and version
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
            return HorizonsFile::HorizonsResult::ErrorSize;
        }
        // STEP_SIZE too big, exceeds available span.
        else if (errorMessage.find("STEP_SIZE too big") != std::string::npos) {
            return HorizonsFile::HorizonsResult::ErrorSpan;
        }
        // No ephemeris for target "X" after A.D. Y UT
        else if (errorMessage.find("No ephemeris for target") != std::string::npos) {
            return HorizonsFile::HorizonsResult::ErrorTimeRange;
        }
        // No site matches. Use "*@body" to list, "c@body" to enter coords, ?! for help.
        else if (errorMessage.find("No site matches") != std::string::npos) {
            return HorizonsFile::HorizonsResult::ErrorNoObserver;
        }
        // Observer table for X / Y->Y disallowed.
        else if (errorMessage.find("disallowed") != std::string::npos) {
            return HorizonsFile::HorizonsResult::ErrorObserverTargetSame;
        }
        // Insufficient ephemeris data has been loaded to compute the state of X
        // relative to Y at the ephemeris epoch Z;
        else if (errorMessage.find("Insufficient ephemeris data") != std::string::npos) {
            return HorizonsFile::HorizonsResult::ErrorNoData;
        }
        // #   E. Lon    DXY      DZ    Observatory Name;
        // -- - -------- ------ - ------ - ----------------;
        // * Observer station *
        // Multiple matching stations found.
        else if (errorMessage.find("Multiple matching stations found") != std::string::npos) {
            return HorizonsFile::HorizonsResult::MultipleObserverStations;
        }
        // Unknown error
        else {
            LERROR(errorMessage);
            return  HorizonsFile::HorizonsResult::UnknownError;
        }
    }
    return HorizonsFile::HorizonsResult::Valid;
}

// Check whether the given Horizons file is valid or not
// Return an error code with what is the problem if there was one
HorizonsFile::HorizonsResult HorizonsFile::isValidHorizonsFile() const {
    std::ifstream fileStream(_file);
    if (!fileStream.good()) {
        return HorizonsFile::HorizonsResult::Empty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that can tell us if the file is valid or not.
    // The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    while (fileStream.good() && line.find("$$SOE") == std::string::npos) {
        // Valid Target?
        if (line.find("Revised") != std::string::npos) {
            // If the target is valid, the first line is the date it was last revised
            foundTarget = true;
        }

        // Selected time range too big and step size too small?
        if (line.find("change step-size") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorSize;
        }

        // Selected time range too big for avalable time span?
        if (line.find("STEP_SIZE too big") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorSpan;
        }

        // Outside valid time range?
        if (line.find("No ephemeris for target") != std::string::npos) {
            // Available time range is located several lines before this in the file
            // The avalable time range is persed later
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorTimeRange;
        }

        // Valid Observer?
        if (line.find("No site matches") != std::string::npos ||
            line.find("Cannot find central body") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorNoObserver;
        }

        // Are observer and target the same?
        if (line.find("disallowed") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorObserverTargetSame;
        }

        // Enough data?
        if (line.find("Insufficient ephemeris data") != std::string::npos)
        {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorNoData;
        }

        // Multiple Observer stations?
        if (line.find("Multiple matching stations found") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::HorizonsResult::MultipleObserverStations;
        }

        // Multiple matching major bodies?
        if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target
            if (!foundTarget) {
                // If target was not found then it is the target that has multiple matches
                fileStream.close();
                return HorizonsFile::HorizonsResult::MultipleTarget;
            }
            // Observer
            else {
                fileStream.close();
                return HorizonsFile::HorizonsResult::MultipleObserver;
            }
        }

        // Multiple matching small bodies?
        if (line.find("Small-body Index Search Results") != std::string::npos) {
            // Small bodies can only be targets not observers
            fileStream.close();
            return HorizonsFile::HorizonsResult::MultipleTarget;
        }

        // No Target?
        if (line.find("No matches found") != std::string::npos) {
            fileStream.close();
            return HorizonsFile::HorizonsResult::ErrorNoTarget;
        }

        std::getline(fileStream, line);
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    if (fileStream.good()) {
        fileStream.close();
        return HorizonsFile::HorizonsResult::Valid;
    }
    else {
        fileStream.close();
        return HorizonsFile::HorizonsResult::UnknownError;
    }
}

} // namespace openspace
