/****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/space/rendering/renderablesmallbody.h>

#include <modules/space/rendering/renderablesatellites.h>
#include <modules/space/translation/keplertranslation.h>
#include <modules/space/translation/tletranslation.h>
#include <modules/space/spacemodule.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <math.h>
#include <fstream>
#include <vector> 

namespace {
    constexpr const char* ProgramName = "RenderableSmallBody";
    constexpr const char* _loggerCat = "SmallSolarSystemBody";

    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the SBDB .csv file to read"
    };
    static const openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment Quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "100 (highest) that controls the number of line segments in the rendering of the "
        "orbital trail. This does not control the direct number of segments because "
        "these automatically increase according to the eccentricity of the orbit."
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the trail if the selected rendering "
        "method includes lines. If the rendering mode is set to Points, this value is "
        "ignored."
    };
    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB main color for the lines and points of the trail."
    };
    constexpr openspace::properties::Property::PropertyInfo TrailFadeInfo = {
        "TrailFade",
        "Trail Fade",
        "This value determines how fast the trail fades and is an appearance property. "
    };
    static const openspace::properties::Property::PropertyInfo UpperLimitInfo = {
        "UpperLimit",
        "Upper Limit",
        "Upper limit on the number of objects for this renderable, regardless of "
        "how many objects are contained in the data file"
    };
}

namespace openspace {

documentation::Documentation RenderableSmallBody::Documentation() {
    using namespace documentation;
    return {
        "RenderableSmallBody",
        "space_renderable_small_body",
        {
            {
                SegmentQualityInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SegmentQualityInfo.description
            },
            {
                UpperLimitInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                UpperLimitInfo.description
            },
            {
                PathInfo.identifier,
                new StringVerifier,
                Optional::No,
                PathInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                LineColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::No,
                LineColorInfo.description
            },
            {
                TrailFadeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TrailFadeInfo.description
            }
        }
    };
}

RenderableSmallBody::RenderableSmallBody(const ghoul::Dictionary& dictionary)
    : RenderableOrbitalKepler(dictionary)
{
    _upperLimitCallbackHandle = _upperLimit.onChange(reinitializeTrailBuffers);
    addProperty(_upperLimit);
}  
    
void RenderableSmallBody::readDataFile(const std::string& filename) {
    if (!FileSys.fileExists(filename)) {
        throw ghoul::RuntimeError(fmt::format(
            "JPL SBDB file {} does not exist.", filename
        ));
    }

    std::ifstream file;
    file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    file.open(filename);

    std::streamoff numberOfLines = std::count(std::istreambuf_iterator<char>(file), 
                                   std::istreambuf_iterator<char>(), '\n' );
    file.seekg(std::ios_base::beg); // reset iterator to beginning of file
    _data.clear();
    _sbNames.clear();
    _segmentSize.clear();

    std::string line;
    std::streamoff csvLine = -1;
    int fieldCount = 0;
    float lineSkipFraction = 1.0;
    float currLineFraction;
    int currLineCount;
    int lastLineCount = -1;
    const std::string expectedHeaderLine =
        "full_name,epoch_cal,e,a,i,om,w,ma,per";

    try {
        std::getline(file, line); // get rid of first line (header)
        numberOfLines -= 1;
        if (_upperLimit == 0 || _upperLimit > numberOfLines) {
            //If a limit wasn't specified in dictionary, set it to # lines in file
            // minus the header line (but temporarily disable callback to avoid 2nd call)
            _upperLimit.removeOnChange(_upperLimitCallbackHandle);
            _upperLimit.setMaxValue(numberOfLines);
            _upperLimit = static_cast<unsigned int>(numberOfLines);
            _upperLimitCallbackHandle = _upperLimit.onChange(reinitializeTrailBuffers);
        }
        else {
            lineSkipFraction = static_cast<float>(_upperLimit) /
                static_cast<float>(numberOfLines);
        }
        if (line.compare(expectedHeaderLine) != 0) {
            LERROR(fmt::format(
                "File {} does not have the appropriate JPL SBDB header at line 1.",
                filename
            ));
            file.close();
            return;
        }

        unsigned int sequentialLineErrors = 0;
        for (csvLine = 1; csvLine <= numberOfLines; csvLine++, sequentialLineErrors++) {
            currLineFraction = static_cast<float>(csvLine - 1) * lineSkipFraction;
            currLineCount = static_cast<int>(currLineFraction);
            if (currLineCount > lastLineCount) {
                try {
                    readOrbitalParamsFromThisLine(fieldCount, csvLine, file);
                    sequentialLineErrors = 0;
                }
                catch (std::invalid_argument&) {
                    const char* errMsg = "Unable to convert field {} to double value "\
                        "(invalid_argument exception) at line {}/{} of {}";
                    LERROR(fmt::format(
                        errMsg,
                        fieldCount, csvLine + 1, numberOfLines, filename
                    ));
                }
                catch (std::out_of_range&) {
                    const char* errMsg = "Unable to convert field {} to double value "\
                        "(out_of_range exception) at line {}/{} of {}";
                    LERROR(fmt::format(
                        errMsg,
                        fieldCount, csvLine + 1, numberOfLines, filename
                    ));
                }
                catch (std::ios_base::failure& f) {
                    throw f;
                }

                if (sequentialLineErrors == 4) {
                    _data.clear();
                    _sbNames.clear();
                    LERROR(fmt::format(
                        "Abandoning data file {} (too many sequential line errors).",
                        filename
                    ));
                    break;
                }
            }
            lastLineCount = currLineCount;
        }
    }
    catch (std::ios_base::failure&) {
        const char* errMsg = "File read exception (ios_base::failure) while trying "\
            "to read field {} at line {}/{} of {}";
        LERROR(fmt::format(
            errMsg,
            fieldCount, csvLine + 1, numberOfLines, filename
        ));
    }

    file.close();
}

void RenderableSmallBody::readOrbitalParamsFromThisLine(int& fieldCount,
                                                        std::streamoff& csvLine,
                                                        std::ifstream& file)
{
    const int numDataFields = 8;
    std::string name;
    std::string field;
    KeplerParameters keplerElements;

    //If there was a read/conversion error in the previous line, then read the remainder
    // of that line and throw it out first before proceeding with the next line.
    if (fieldCount != (numDataFields + 1) && csvLine != 1) {
        std::getline(file, field);
    }
    fieldCount = 0;

    // Object designator string
    std::getline(file, name, ',');
    fieldCount++;

    // Epoch
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read epoch from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.epoch = epochFromYMDdSubstring(field);
    fieldCount++;

    // Eccentricity (unit-less)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read eccentricity from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.eccentricity = std::stod(field);
    fieldCount++;

    // Semi-major axis (astronomical units - au)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read semi-major axis from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.semiMajorAxis = std::stod(field);
    keplerElements.semiMajorAxis *= convertAuToKm;
    fieldCount++;

    // Inclination (degrees)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read inclination from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.inclination = importAngleValue(field);
    fieldCount++;

    // Longitude of ascending node (degrees)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read ascending node from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.ascendingNode = importAngleValue(field);
    fieldCount++;

    // Argument of Periapsis (degrees)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read arg of periapsis from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.argumentOfPeriapsis = importAngleValue(field);
    fieldCount++;

    // Mean Anomaly (degrees)
    if (!std::getline(file, field, ',')) {
        throw std::invalid_argument("Unable to read mean anomaly from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.meanAnomaly = importAngleValue(field);
    fieldCount++;

    // Period (days)
    if (!std::getline(file, field)) {
        throw std::invalid_argument("Unable to read period from line"
            + std::to_string(csvLine + 1));
    }
    keplerElements.period = std::stod(field);
    keplerElements.period *= convertDaysToSecs;
    fieldCount++;

    _data.push_back(keplerElements);
    _sbNames.push_back(name);
    _segmentSize.push_back(_segmentQuality * 10 * (keplerElements.eccentricity / 0.05));
}

static double importAngleValue(const std::string& angle) {
    double output = std::stod(angle);
    output = std::fmod(output, 360.0);
    if (output < 0.0) {
        output += 360.0;
    }
    return output;
}
  
}
