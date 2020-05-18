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

#include <modules/space/rendering/renderableorbitalkepler.h>

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
#include <fstream>
#include <math.h>
#include <vector>

namespace {
    constexpr const char* ProgramName = "OrbitalKepler";
    constexpr const char* _loggerCat = "OrbitalKepler";
    constexpr const char* KeyFile = "Path";
    constexpr const char* KeyLineNum = "LineNumber";

    constexpr const std::array<int, 36> LeapYears = {
        1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996,
        2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040,
        2044, 2048, 2052, 2056
    };
    constexpr const std::array<int, 12> DaysOfMonths = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };

    // Find the position of the current year in the vector; its position in
    // the vector gives the number of leap seconds
    struct LeapSecond {
        int year;
        int dayOfYear;
        bool operator<(const LeapSecond& rhs) const {
            return std::tie(year, dayOfYear) < std::tie(rhs.year, rhs.dayOfYear);
        }
    };

    constexpr const LeapSecond LeapEpoch = { 2000, 1 };

    // List taken from: https://www.ietf.org/timezones/data/leap-seconds.list
    constexpr const std::array<LeapSecond, 28> LeapSeconds = {
        LeapSecond{ 1972,   1 },
        LeapSecond{ 1972, 183 },
        LeapSecond{ 1973,   1 },
        LeapSecond{ 1974,   1 },
        LeapSecond{ 1975,   1 },
        LeapSecond{ 1976,   1 },
        LeapSecond{ 1977,   1 },
        LeapSecond{ 1978,   1 },
        LeapSecond{ 1979,   1 },
        LeapSecond{ 1980,   1 },
        LeapSecond{ 1981, 182 },
        LeapSecond{ 1982, 182 },
        LeapSecond{ 1983, 182 },
        LeapSecond{ 1985, 182 },
        LeapSecond{ 1988,   1 },
        LeapSecond{ 1990,   1 },
        LeapSecond{ 1991,   1 },
        LeapSecond{ 1992, 183 },
        LeapSecond{ 1993, 182 },
        LeapSecond{ 1994, 182 },
        LeapSecond{ 1996,   1 },
        LeapSecond{ 1997, 182 },
        LeapSecond{ 1999,   1 },
        LeapSecond{ 2006,   1 },
        LeapSecond{ 2009,   1 },
        LeapSecond{ 2012, 183 },
        LeapSecond{ 2015, 182 },
        LeapSecond{ 2017,   1 }
    };

    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read"
    };
    static const openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment Quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "10 (highest) that controls the number of line segments in the rendering of the "
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
        "how many objects are contained in the data file. Produces an evenly-distributed"
        "sample from the data file."
    };
    static const openspace::properties::Property::PropertyInfo StartRenderIdxInfo = {
        "StartRenderIdx",
        "Starting Index of Render",
        "Index of object in renderable group to start rendering (all prior objects will "
        "be ignored)."
    };
    static const openspace::properties::Property::PropertyInfo RenderSizeInfo = {
        "RenderSizeInfo",
        "Size of Render Block",
        "Number of objects to render sequentially from StartRenderIdx"
    };

    // Count the number of full days since the beginning of 2000 to the beginning of
    // the parameter 'year'
    int countDays(int year) {
        // Find the position of the current year in the vector, the difference
        // between its position and the position of 2000 (for J2000) gives the
        // number of leap years
        constexpr const int Epoch = 2000;
        constexpr const int DaysRegularYear = 365;
        constexpr const int DaysLeapYear = 366;

        if (year == Epoch) {
            return 0;
        }

        // Get the position of the most recent leap year
        const auto lb = std::lower_bound(LeapYears.begin(), LeapYears.end(), year);

        // Get the position of the epoch
        const auto y2000 = std::find(LeapYears.begin(), LeapYears.end(), Epoch);

        // The distance between the two iterators gives us the number of leap years
        const int nLeapYears = static_cast<int>(std::abs(std::distance(y2000, lb)));

        const int nYears = std::abs(year - Epoch);
        const int nRegularYears = nYears - nLeapYears;

        // Get the total number of days as the sum of leap years + non leap years
        const int result = nRegularYears * DaysRegularYear + nLeapYears * DaysLeapYear;
        return result;
    }

    // Returns the number of leap seconds that lie between the {year, dayOfYear}
    // time point and { 2000, 1 }
    int countLeapSeconds(int year, int dayOfYear) {
        // Get the position of the last leap second before the desired date
        LeapSecond date{ year, dayOfYear };
        const auto it = std::lower_bound(LeapSeconds.begin(), LeapSeconds.end(), date);

        // Get the position of the Epoch
        const auto y2000 = std::lower_bound(
            LeapSeconds.begin(),
            LeapSeconds.end(),
            LeapEpoch
        );

        // The distance between the two iterators gives us the number of leap years
        const int nLeapSeconds = static_cast<int>(std::abs(std::distance(y2000, it)));
        return nLeapSeconds;
    }

    int daysIntoGivenYear(int year, int month, int dayOfMonth) {
        //month and dayCount are zero-based.
        month -= 1;
        int dayCount = dayOfMonth - 1;
        const int February = 1;
        const bool isInLeapYear =
            std::find(LeapYears.begin(), LeapYears.end(), year)
            != LeapYears.end();

        for (int m = 0; m < month; ++m) {
            dayCount += DaysOfMonths[m];
            if (m == February && isInLeapYear) {
                dayCount += 1;
            }
        }
        return dayCount;
    }

} // namespace

namespace openspace {

double RenderableOrbitalKepler::calculateSemiMajorAxis(double meanMotion) const {
    constexpr const double GravitationalConstant = 6.6740831e-11;
    constexpr const double MassEarth = 5.9721986e24;
    constexpr const double muEarth = GravitationalConstant * MassEarth;

    // Use Kepler's 3rd law to calculate semimajor axis
    // a^3 / P^2 = mu / (2pi)^2
    // <=> a = ((mu * P^2) / (2pi^2))^(1/3)
    // with a = semimajor axis
    // P = period in seconds
    // mu = G*M_earth
    const double period =
        std::chrono::seconds(std::chrono::hours(24)).count() / meanMotion;

    constexpr const double pisq = glm::pi<double>() * glm::pi<double>();
    const double semiMajorAxis = pow((muEarth * period*period) / (4 * pisq), 1.0 / 3.0);

    // We need the semi major axis in km instead of m
    return semiMajorAxis / 1000.0;
}

double RenderableOrbitalKepler::epochFromSubstring(const std::string& epochString) const {
    // The epochString is in the form:
    // YYDDD.DDDDDDDD
    // With YY being the last two years of the launch epoch, the first DDD the day
    // of the year and the remaning a fractional part of the day

    // The main overview of this function:
    // 1. Reconstruct the full year from the YY part
    // 2. Calculate the number of days since the beginning of the year
    // 3. Convert the number of days to a number of seconds
    // 4. Get the number of leap seconds since January 1st, 2000 and remove them
    // 5. Adjust for the fact the epoch starts on 1st Januaray at 12:00:00, not
    // midnight

    // According to https://celestrak.com/columns/v04n03/
    // Apparently, US Space Command sees no need to change the two-line element
    // set format yet since no artificial earth satellites existed prior to 1957.
    // By their reasoning, two-digit years from 57-99 correspond to 1957-1999 and
    // those from 00-56 correspond to 2000-2056. We'll see each other again in 057!

    // 1. Get the full year
    std::string yearPrefix =
        std::atoi(epochString.substr(0, 2).c_str()) > 57 ? "19" : "20";
    const int year = std::atoi((yearPrefix + epochString.substr(0, 2)).c_str());
    const int daysSince2000 = countDays(year);

    // 2.
    double daysInYear = std::atof(epochString.substr(2).c_str());

    // 3
    using namespace std::chrono;
    const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
    //Need to subtract 1 from daysInYear since it is not a zero-based count
    const double nSecondsSince2000 = (daysSince2000 + daysInYear - 1) * SecondsPerDay;

    // 4
    // We need to remove additional leap seconds past 2000 and add them prior to
    // 2000 to sync up the time zones
    const double nLeapSecondsOffset = -countLeapSeconds(
        year,
        static_cast<int>(std::floor(daysInYear))
    );

    // 5
    const double nSecondsEpochOffset = static_cast<double>(seconds(hours(12)).count());

    // Combine all of the values
    const double epoch = nSecondsSince2000 + nLeapSecondsOffset - nSecondsEpochOffset;
    return epoch;
}

double RenderableOrbitalKepler::epochFromYMDdSubstring(const std::string& epochString) {
    // The epochString is in the form:
    // YYYYMMDD.ddddddd
    // With YYYY as the year, MM the month (1 - 12), DD the day of month (1-31),
    // and dddd the fraction of that day.

    // The main overview of this function:
    // 1. Read the year value
    // 2. Calculate the number of days since the beginning of the year
    // 3. Convert the number of days to a number of seconds
    // 4. Get the number of leap seconds since January 1st, 2000 and remove them
    // 5. Adjust for the fact the epoch starts on 1st January at 12:00:00, not
    // midnight

    // 1
    int year = std::atoi(epochString.substr(0, 4).c_str());
    const int daysSince2000 = countDays(year);

    // 2.
    int monthNum = std::atoi(epochString.substr(4, 2).c_str());
    int dayOfMonthNum = std::atoi(epochString.substr(6, 2).c_str());
    int wholeDaysInto = daysIntoGivenYear(year, monthNum, dayOfMonthNum);
    double fractionOfDay = std::atof(epochString.substr(9, 7).c_str());
    double daysInYear = static_cast<double>(wholeDaysInto) + fractionOfDay;

    // 3
    using namespace std::chrono;
    const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
    //Need to subtract 1 from daysInYear since it is not a zero-based count
    const double nSecondsSince2000 = (daysSince2000 + daysInYear - 1) * SecondsPerDay;

    // 4
    // We need to remove additional leap seconds past 2000 and add them prior to
    // 2000 to sync up the time zones
    const double nLeapSecondsOffset = -countLeapSeconds(
        year,
        static_cast<int>(std::floor(daysInYear))
    );

    // 5
    const double nSecondsEpochOffset = static_cast<double>(seconds(hours(12)).count());

    // Combine all of the values
    const double epoch = nSecondsSince2000 + nLeapSecondsOffset - nSecondsEpochOffset;
    return epoch;
}

RenderableOrbitalKepler::RenderableOrbitalKepler(const ghoul::Dictionary& dict)
    : Renderable(dict)
    , _path(PathInfo)
    , _segmentQuality(SegmentQualityInfo, 2, 1, 10)
    , _upperLimit(UpperLimitInfo, 1000, 1, 1000000)
    , _startRenderIdx(StartRenderIdxInfo, 0, 0, 1)
    , _sizeRender(RenderSizeInfo, 1, 1, 2)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "RenderableOrbitalKepler"
    );

    _path = dict.value<std::string>(PathInfo.identifier);
    _segmentQuality = static_cast<int>(dict.value<double>(SegmentQualityInfo.identifier));

    if (dict.hasKeyAndValue<glm::vec3>(LineColorInfo.identifier)) {
        _appearance.lineColor = dict.value<glm::vec3>(LineColorInfo.identifier);
    }

    _appearance.lineFade = dict.hasKeyAndValue<double>(TrailFadeInfo.identifier) ?
        static_cast<float>(dict.value<double>(TrailFadeInfo.identifier)) :
        20.f;

    _upperLimit = dict.hasKeyAndValue<double>(UpperLimitInfo.identifier) ?
        static_cast<unsigned int>(dict.value<double>(UpperLimitInfo.identifier)) :
        0u;

    _startRenderIdx = dict.hasKeyAndValue<double>(StartRenderIdxInfo.identifier) ?
        static_cast<unsigned int>(dict.value<double>(StartRenderIdxInfo.identifier)) :
        0u;

    _sizeRender = dict.hasKeyAndValue<double>(RenderSizeInfo.identifier) ?
        static_cast<unsigned int>(dict.value<double>(RenderSizeInfo.identifier)) :
        0u;

    _appearance.lineWidth = dict.hasKeyAndValue<double>(LineWidthInfo.identifier) ?
        static_cast<float>(dict.value<double>(LineWidthInfo.identifier)) :
        2.f;

    _reinitializeTrailBuffers = std::function<void()>([this] { initializeGL(); });
    _path.onChange(_reinitializeTrailBuffers);
    _segmentQuality.onChange(_reinitializeTrailBuffers);

    addPropertySubOwner(_appearance);
    addProperty(_path);
    addProperty(_segmentQuality);
    addProperty(_opacity);
    addProperty(_startRenderIdx);
    addProperty(_sizeRender);

    _updateStartRenderIdxSelect = std::function<void()>([this] { initializeGL(); });
    _updateRenderSizeSelect = std::function<void()>([this] { initializeGL(); });
    _startRenderIdxCallbackHandle = _startRenderIdx.onChange(_updateStartRenderIdxSelect);
    _sizeRenderCallbackHandle = _sizeRender.onChange(_updateRenderSizeSelect);

    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableOrbitalKepler::initializeGL() {
    glGenVertexArrays(1, &_vertexArray);
    glGenBuffers(1, &_vertexBuffer);

    _programObject = SpaceModule::ProgramObjectManager.request(
       ProgramName,
       []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
           return global::renderEngine.buildRenderProgram(
               ProgramName,
               absPath("${MODULE_SPACE}/shaders/debrisViz_vs.glsl"),
               absPath("${MODULE_SPACE}/shaders/debrisViz_fs.glsl")
           );
       }
   );

    _uniformCache.modelView = _programObject->uniformLocation("modelViewTransform");
    _uniformCache.projection = _programObject->uniformLocation("projectionTransform");
    _uniformCache.lineFade = _programObject->uniformLocation("lineFade");
    _uniformCache.inGameTime = _programObject->uniformLocation("inGameTime");
    _uniformCache.color = _programObject->uniformLocation("color");
    _uniformCache.opacity = _programObject->uniformLocation("opacity");

    updateBuffers();
}

void RenderableOrbitalKepler::deinitializeGL() {
    glDeleteBuffers(1, &_vertexBuffer);
    glDeleteVertexArrays(1, &_vertexArray);

    SpaceModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableOrbitalKepler::isReady() const {
    return _programObject != nullptr;
}

void RenderableOrbitalKepler::render(const RenderData& data, RendererTasks&) {
    if (_data.empty()) {
        return;
    }

    _programObject->activate();
    _programObject->setUniform(_uniformCache.opacity, _opacity);
    _programObject->setUniform(_uniformCache.inGameTime, data.time.j2000Seconds());

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    _programObject->setUniform(
        _uniformCache.modelView,
        data.camera.combinedViewMatrix() * modelTransform
    );

    // Because we want the property to work similar to the planet trails
    const float fade = static_cast<float>(
        pow(_appearance.lineFade.maxValue() - _appearance.lineFade, 2.0)
    );

    _programObject->setUniform(_uniformCache.projection, data.camera.projectionMatrix());
    _programObject->setUniform(_uniformCache.color, _appearance.lineColor);
    _programObject->setUniform(_uniformCache.lineFade, fade);

    glLineWidth(_appearance.lineWidth);

    const size_t nrOrbits = _data.size();
    gl::GLint vertices = 0;

    //glDepthMask(false);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE)

    glBindVertexArray(_vertexArray);
    for (size_t i = 0; i < nrOrbits; ++i) {
        glDrawArrays(GL_LINE_STRIP, vertices, static_cast<GLsizei>(_segmentSize[i] + 1));
        vertices = vertices + static_cast<GLint>(_segmentSize[i]) + 1;
    }
    glBindVertexArray(0);

    _programObject->deactivate();
}

void RenderableOrbitalKepler::updateBuffers() {
    readDataFile(_path);

    size_t nVerticesTotal = 0;

    int numOrbits = static_cast<int>(_data.size());
    for (size_t i = 0; i < numOrbits; ++i) {
        nVerticesTotal += _segmentSize[i] + 1;
    }
    _vertexBufferData.resize(nVerticesTotal);

    size_t vertexBufIdx = 0;
    for (size_t orbitIdx = 0; orbitIdx < numOrbits; ++orbitIdx) {
        const KeplerParameters& orbit = _data[orbitIdx];

        _keplerTranslator.setKeplerElements(
            orbit.eccentricity,
            orbit.semiMajorAxis,
            orbit.inclination,
            orbit.ascendingNode,
            orbit.argumentOfPeriapsis,
            orbit.meanAnomaly,
            orbit.period,
            orbit.epoch
        );

        for (size_t j = 0 ; j < (_segmentSize[orbitIdx] + 1); ++j) {
            double timeOffset = orbit.period *
                static_cast<double>(j)/ static_cast<double>(_segmentSize[orbitIdx]);

            glm::dvec3 position = _keplerTranslator.position({
                {},
                Time(timeOffset + orbit.epoch),
                Time(0.0),
                false
            });

            _vertexBufferData[vertexBufIdx].x = static_cast<float>(position.x);
            _vertexBufferData[vertexBufIdx].y = static_cast<float>(position.y);
            _vertexBufferData[vertexBufIdx].z = static_cast<float>(position.z);
            _vertexBufferData[vertexBufIdx].time = static_cast<float>(timeOffset);
            _vertexBufferData[vertexBufIdx].epoch = orbit.epoch;
            _vertexBufferData[vertexBufIdx].period = orbit.period;

            vertexBufIdx++;
        }
    }

    glBindVertexArray(_vertexArray);

    glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexBufferData.size() * sizeof(TrailVBOLayout),
        _vertexBufferData.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TrailVBOLayout),  nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_DOUBLE,
        GL_FALSE,
        sizeof(TrailVBOLayout),
        reinterpret_cast<GLvoid*>(4 * sizeof(GL_FLOAT))
    );

    glBindVertexArray(0);
}

} // namespace opensapce
