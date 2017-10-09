/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ccmc/Kameleon.h>
#include <ghoul/logging/logmanager.h>


namespace {
    std::string _loggerCat = "KameleonHelper";
}

namespace openspace::kameleonHelper {

/**
 * Opens a ccmc::Kameleon object from the provided path to a .cdf file.
 * Path should be absolute.
 *
 * Returns 'nullptr' if the file fails to open!
 */
std::unique_ptr<ccmc::Kameleon> createKameleonObject(const std::string& CDF_FILE_PATH) {

    // ---------------------------- CREATE KAMELEON OBJECT ---------------------------- //
    std::unique_ptr<ccmc::Kameleon> kameleon = std::make_unique<ccmc::Kameleon>();
    LDEBUG("\tOpening the cdf file: " << CDF_FILE_PATH);
    long kamStatus = kameleon->open(CDF_FILE_PATH);

    if (kamStatus != ccmc::FileReader::OK) {
        LERROR("Failed to create a Kameleon Object from file: " << CDF_FILE_PATH);
       return nullptr;
    }
    LDEBUG("\tSuccessfully opened : " << CDF_FILE_PATH);
    return kameleon;
}

/**
 * Extract the time for the simulation. Time is returned as a J2000 double.
 *
 * *NOTE!* The function has only been tested for some BATSRUS and ENLIL and may need to
 *         be updated to work with other models!
 */
double getTime(ccmc::Kameleon* kameleon) {
    // Inspiration from 'void KameleonInterpolator::setEphemTime()' which doesn't seem to
    // exist in the version of Kameleon that is included in OpenSpace. Alterations
    // done to fit here.
    // As a new version of Kameleon is included in OpenSpace this function may prove to be
    // redundant!

        std::string seqStartStr;
        double seqStartDbl;
        if (kameleon->doesAttributeExist("start_time")){
            seqStartStr =
                    kameleon->getGlobalAttribute("start_time").getAttributeString();
        } else if (kameleon->doesAttributeExist("tim_rundate_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_rundate_cal").getAttributeString();
            const size_t N_CHARS = seqStartStr.length();
            if (N_CHARS < 19) {
                // Fall through to add the required characters
                switch (N_CHARS) {
                    case 10 : // YYYY-MM-DD             =>       YYYY-MM-DDTHH
                        seqStartStr += "T00";
                    case 13 : // YYYY-MM-DDTHH          =>       YYYY-MM-DDTHH:
                        seqStartStr += ":";
                    case 14 : // YYYY-MM-DDTHH:         =>       YYYY-MM-DDTHH:MM
                        seqStartStr += "00";
                    case 16 : // YYYY-MM-DDTHH:MM       =>       YYYY-MM-DDTHH:MM:
                        seqStartStr += ":";
                    case 17 : // YYYY-MM-DDTHH:MM:      =>       YYYY-MM-DDTHH:MM:SS
                        seqStartStr += "00";
                    // case 19 : // YYYY-MM-DDTHH:MM:SS    =>    YYYY-MM-DDTHH:MM:SS.000
                    //     seqStartStr += ".000";
                    // case 23 : // YYYY-MM-DDTHH:MM:SS.   =>    YYYY-MM-DDTHH:MM:SS.000Z
                    //     seqStartStr += "Z";
                    default :
                        break;
                }
            }
            // else if (seqStartStr.length() < 19 && kameleon->doesAttributeExist("tim_crstart_cal")) {
            //     seqStartStr =
            //          kameleon->getGlobalAttribute("tim_crstart_cal").getAttributeString();
            // }
        } else if (kameleon->doesAttributeExist("tim_obsdate_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_obsdate_cal").getAttributeString();
        } else if (kameleon->doesAttributeExist("tim_crstart_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_crstart_cal").getAttributeString();
        } else {
            LWARNING("No starting time attribute could be found in the .cdf file.\n\t" <<
                    "Starting time is set to 01.JAN.2000 12:00.");
            seqStartDbl = 0.0;
        }

        if (seqStartStr.length() == 19){
            seqStartStr += ".000Z";
        }

        if (seqStartStr.length() == 24){
            seqStartDbl =
                    Time::convertTime(
                            seqStartStr.substr(0, seqStartStr.length() - 2));
        } else {
            LWARNING("No starting time attribute could be found in the .cdf file.\n\t" <<
                "Starting time is set to 01.JAN.2000 12:00.");
            seqStartDbl = 0.0;
        }

        double stateStartOffset;

        if (kameleon->doesAttributeExist("elapsed_time_in_seconds")) {
            stateStartOffset = static_cast<double>(
                    kameleon->getGlobalAttribute(
                            "elapsed_time_in_seconds").getAttributeFloat());
        } else if (kameleon->doesAttributeExist("time_physical_time")) {
            stateStartOffset = static_cast<double>(
                    kameleon->getGlobalAttribute(
                            "time_physical_time").getAttributeFloat());
        } else {
            stateStartOffset = 0.0;
            LWARNING("No time offset attribute could be found in the .cdf file.\n\t" <<
                     "The current state starts the same time as the sequence!");
        }

    return seqStartDbl + stateStartOffset;
}

} // namespace openspace::kameleonHelper {
