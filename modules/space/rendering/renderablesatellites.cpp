/*****************************************************************************************
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
    constexpr const char* ProgramName = "RenderableSatellites";
    constexpr const char* _loggerCat = "Satellites";

    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the TLE file to read"
    };
    static const openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments to use for each orbit ellipse"
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
}

namespace openspace {

documentation::Documentation RenderableSatellites::Documentation() {
    using namespace documentation;
    return {
        "RenderableSatellites",
        "space_renderable_satellites",
        {
            {
                SegmentsInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SegmentsInfo.description
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

RenderableSatellites::RenderableSatellites(const ghoul::Dictionary& dictionary)
    : RenderableOrbitalKepler(dictionary)
{}

void RenderableSatellites::readDataFile(const std::string& filename) {
    if (!FileSys.fileExists(filename)) {
        throw ghoul::RuntimeError(fmt::format(
            "Satellite TLE file {} does not exist.", filename
        ));
    }
    _data.clear();
    _segmentSize.clear();

    std::ifstream file;
    file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    file.open(filename);

    std::streamoff numberOfLines = std::count(
        std::istreambuf_iterator<char>(file),
        std::istreambuf_iterator<char>(),
        '\n'
    );
    file.seekg(std::ios_base::beg); // reset iterator to beginning of file

    _numObjects = numberOfLines / nLineEntriesPerSatellite;

    if (!_isFileReadinitialized) {
        _isFileReadinitialized = true;
        initializeFileReading();
    }

    std::string line = "-";
    std::string name;
    long long endElement = _startRenderIdx + _sizeRender - 1;
    endElement = (endElement >= _numObjects) ? _numObjects - 1 : endElement;
    //Burn lines if not starting at first element
    for (unsigned int k = 0; k < _startRenderIdx; ++k) {
        skipSingleEntryInFile(file);
    }
    for (std::streamoff i = _startRenderIdx; i <= endElement; i++) {
        //Read title line
        std::getline(file, name);
        KeplerParameters keplerElements;

        std::getline(file, line);
        if (line[0] == '1') {
            // First line
            // Field Columns   Content
            //     1   01-01   Line number
            //     2   03-07   Satellite number
            //     3   08-08   Classification (U = Unclassified)
            //     4   10-11   International Designator (Last two digits of launch year)
            //     5   12-14   International Designator (Launch number of the year)
            //     6   15-17   International Designator(piece of the launch)    A
            name += " " + line.substr(2, 15);
            if (_startRenderIdx > 0 && _startRenderIdx == i) {
                LINFO(fmt::format(
                    "Set render block to start at object  {}",
                    name
                ));
            }
            //     7   19-20   Epoch Year(last two digits of year)
            //     8   21-32   Epoch(day of the year and fractional portion of the day)
            //     9   34-43   First Time Derivative of the Mean Motion divided by two
            //    10   45-52   Second Time Derivative of Mean Motion divided by six
            //    11   54-61   BSTAR drag term(decimal point assumed)[10] - 11606 - 4
            //    12   63-63   The "Ephemeris type"
            //    13   65-68   Element set  number.Incremented when a new TLE is generated
            //    14   69-69   Checksum (modulo 10)
            keplerElements.epoch = epochFromSubstring(line.substr(18, 14));
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "File {} entry {} does not have '1' header", filename, i + 1
            ));
        }

        std::getline(file, line);
        if (line[0] == '2') {
            // Second line
            // Field    Columns   Content
            //     1      01-01   Line number
            //     2      03-07   Satellite number
            //     3      09-16   Inclination (degrees)
            //     4      18-25   Right ascension of the ascending node (degrees)
            //     5      27-33   Eccentricity (decimal point assumed)
            //     6      35-42   Argument of perigee (degrees)
            //     7      44-51   Mean Anomaly (degrees)
            //     8      53-63   Mean Motion (revolutions per day)
            //     9      64-68   Revolution number at epoch (revolutions)
            //    10      69-69   Checksum (modulo 10)

            std::stringstream stream;
            stream.exceptions(std::ios::failbit);

            // Get inclination
            stream.str(line.substr(8, 8));
            stream >> keplerElements.inclination;
            stream.clear();

            // Get Right ascension of the ascending node
            stream.str(line.substr(17, 8));
            stream >> keplerElements.ascendingNode;
            stream.clear();

            // Get Eccentricity
            stream.str("0." + line.substr(26, 7));
            stream >> keplerElements.eccentricity;
            stream.clear();

            // Get argument of periapsis
            stream.str(line.substr(34, 8));
            stream >> keplerElements.argumentOfPeriapsis;
            stream.clear();

            // Get mean anomaly
            stream.str(line.substr(43, 8));
            stream >> keplerElements.meanAnomaly;
            stream.clear();

            // Get mean motion
            stream.str(line.substr(52, 11));
            stream >> keplerElements.meanMotion;
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "File {} entry {} does not have '2' header", filename, i + 1
            ));
        }

        // Calculate the semi major axis based on the mean motion using kepler's laws
        keplerElements.semiMajorAxis = calculateSemiMajorAxis(keplerElements.meanMotion);

        using namespace std::chrono;
        double period = seconds(hours(24)).count() / keplerElements.meanMotion;
        keplerElements.period = period;

        _data.push_back(keplerElements);
        _segmentSize.push_back(_segmentQuality * 16);
    }
    file.close();
}

void RenderableSatellites::initializeFileReading() {
    _startRenderIdx.removeOnChange(_startRenderIdxCallbackHandle);
    _sizeRender.removeOnChange(_sizeRenderCallbackHandle);
    _startRenderIdx.setMaxValue(static_cast<unsigned int>(_numObjects - 1));
    _sizeRender.setMaxValue(static_cast<unsigned int>(_numObjects));
    _startRenderIdx = static_cast<unsigned int>(0);
    _sizeRender = static_cast<unsigned int>(_numObjects);
    _startRenderIdxCallbackHandle = _startRenderIdx.onChange(
        _updateStartRenderIdxSelect);
    _sizeRenderCallbackHandle = _sizeRender.onChange(
        _updateRenderSizeSelect);
}

void RenderableSatellites::skipSingleEntryInFile(std::ifstream& file) {
    std::string line;
    for (unsigned int i = 0; i < nLineEntriesPerSatellite; i++) {
        std::getline(file, line);
    }
}

}
