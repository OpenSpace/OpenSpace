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

#include <modules/kameleon/include/kameleonhelper.h>

#include <openspace/util/time.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

#ifdef _MSC_VER
#pragma warning (push)
// Boost throws #pragma warning: there is no warning number '4675'
#pragma warning (disable : 4619)
#endif // _MSC_VER

#include <ccmc/Kameleon.h>
#include <ccmc/FileReader.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER


namespace {
    constexpr std::string_view _loggerCat = "KameleonHelper";
} // namespace

namespace openspace::kameleonHelper {

std::unique_ptr<ccmc::Kameleon> createKameleonObject(const std::string& cdfFilePath) {
    auto kameleon = std::make_unique<ccmc::Kameleon>();
    LDEBUG(std::format("Opening the CDF file '{}'", cdfFilePath));
    long kamStatus = kameleon->open(cdfFilePath);

    if (kamStatus != ccmc::FileReader::OK) {
        LERROR(std::format(
            "Failed to create a Kameleon Object from file '{}'",
            cdfFilePath
        ));
       return nullptr;
    }
    LDEBUG(std::format("Successfully opened '{}'", cdfFilePath));
    return kameleon;
}

/**
 * Extract the time for the simulation. Time is returned as a J2000 double.
 *
 * *NOTE!* The function has only been tested for some BATSRUS and ENLIL and may need to
 *         be updated to work with other models!
 */
double getTime(ccmc::Kameleon* kameleon, double manualOffset) {
    // Inspiration from 'void KameleonInterpolator::setEphemTime()' which doesn't seem to
    // exist in the version of Kameleon that is included in OpenSpace. Alterations
    // done to fit here.
    // As a new version of Kameleon is included in OpenSpace this function may prove to be
    // redundant!

    std::string seqStartStr;
    if (kameleon->doesAttributeExist("start_time")){
        seqStartStr =
                kameleon->getGlobalAttribute("start_time").getAttributeString();
    }
    else if (kameleon->doesAttributeExist("tim_rundate_cal")) {
        seqStartStr =
                kameleon->getGlobalAttribute("tim_rundate_cal").getAttributeString();
        const size_t N_CHARS = seqStartStr.length();
        if (N_CHARS < 19) {
            // Fall through to add the required characters
            switch (N_CHARS) {
                case 10: // YYYY-MM-DD             =>       YYYY-MM-DDTHH
                    seqStartStr += "T00";
                    [[fallthrough]];
                case 13: // YYYY-MM-DDTHH          =>       YYYY-MM-DDTHH:
                    seqStartStr += ":";
                    [[fallthrough]];
                case 14: // YYYY-MM-DDTHH:         =>       YYYY-MM-DDTHH:MM
                    seqStartStr += "00";
                    [[fallthrough]];
                case 16: // YYYY-MM-DDTHH:MM       =>       YYYY-MM-DDTHH:MM:
                    seqStartStr += ":";
                    [[fallthrough]];
                case 17: // YYYY-MM-DDTHH:MM:      =>       YYYY-MM-DDTHH:MM:SS
                    seqStartStr += "00";
                    [[fallthrough]];
                // case 19 : // YYYY-MM-DDTHH:MM:SS    =>    YYYY-MM-DDTHH:MM:SS.000
                //     seqStartStr += ".000";
                // case 23 : // YYYY-MM-DDTHH:MM:SS.   =>    YYYY-MM-DDTHH:MM:SS.000Z
                //     seqStartStr += "Z";
                default:
                    break;
            }
        }
    }
    else if (kameleon->doesAttributeExist("tim_obsdate_cal")) {
        seqStartStr =
            kameleon->getGlobalAttribute("tim_obsdate_cal").getAttributeString();
    }
    else if (kameleon->doesAttributeExist("tim_crstart_cal")) {
        seqStartStr =
            kameleon->getGlobalAttribute("tim_crstart_cal").getAttributeString();
    }
    else {
        LWARNING(
            "No starting time attribute could be found in the .cdf file. Starting "
            "time is set to 01.JAN.2000 12:00"
        );
    }

    if (seqStartStr.length() == 19) {
        seqStartStr += ".000Z";
    }

    double seqStartDbl;
    if (seqStartStr.length() == 24) {
        seqStartDbl = Time::convertTime(
            seqStartStr.substr(0, seqStartStr.length() - 2)
        );
    }
    else {
        LWARNING(
            "No starting time attribute could be found in the .cdf file. Starting time "
            "is set to 01.JAN.2000 12:00"
        );
        seqStartDbl = 0.0;
    }

    double stateStartOffset;

    if (kameleon->doesAttributeExist("elapsed_time_in_seconds")) {
        ccmc::Attribute att = kameleon->getGlobalAttribute("elapsed_time_in_seconds");
        stateStartOffset = static_cast<double>(att.getAttributeFloat());
    }
    else if (kameleon->doesAttributeExist("time_physical_time")) {
        ccmc::Attribute att = kameleon->getGlobalAttribute("time_physical_time");
        stateStartOffset = static_cast<double>(att.getAttributeFloat());
    }
    else {
        stateStartOffset = 0.0;
        LWARNING(
            "No time offset attribute could be found in the .cdf file. The current state "
            "starts the same time as the sequence"
        );
    }

    return seqStartDbl + stateStartOffset + manualOffset;
}

} // namespace openspace::kameleonHelper {
