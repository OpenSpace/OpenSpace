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
#include <math.h>
#include <fstream>
#include <vector> 

namespace {
    constexpr const char* ProgramName = "OrbitalKepler";
    constexpr const char* _loggerCat = "OrbitalKepler";

    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read"
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

    constexpr const char* KeyFile = "Path";
    constexpr const char* KeyLineNum = "LineNumber";
}

namespace openspace {

// Count the number of full days since the beginning of 2000 to the beginning of
// the parameter 'year'
int RenderableOrbitalKepler::countDays(int year) {
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
int RenderableOrbitalKepler::countLeapSeconds(int year, int dayOfYear) {
    // Find the position of the current year in the vector; its position in
    // the vector gives the number of leap seconds
    struct LeapSecond {
        int year;
        int dayOfYear;
        bool operator<(const LeapSecond& rhs) const {
            return std::tie(year, dayOfYear) < std::tie(rhs.year, rhs.dayOfYear);
        }
    };

    const LeapSecond Epoch = { 2000, 1 };

    // List taken from: https://www.ietf.org/timezones/data/leap-seconds.list
    static const std::vector<LeapSecond> LeapSeconds = {
        { 1972,   1 },
        { 1972, 183 },
        { 1973,   1 },
        { 1974,   1 },
        { 1975,   1 },
        { 1976,   1 },
        { 1977,   1 },
        { 1978,   1 },
        { 1979,   1 },
        { 1980,   1 },
        { 1981, 182 },
        { 1982, 182 },
        { 1983, 182 },
        { 1985, 182 },
        { 1988,   1 },
        { 1990,   1 },
        { 1991,   1 },
        { 1992, 183 },
        { 1993, 182 },
        { 1994, 182 },
        { 1996,   1 },
        { 1997, 182 },
        { 1999,   1 },
        { 2006,   1 },
        { 2009,   1 },
        { 2012, 183 },
        { 2015, 182 },
        { 2017,   1 }
    };

    // Get the position of the last leap second before the desired date
    LeapSecond date { year, dayOfYear };
    const auto it = std::lower_bound(LeapSeconds.begin(), LeapSeconds.end(), date);

    // Get the position of the Epoch
    const auto y2000 = std::lower_bound(
        LeapSeconds.begin(),
        LeapSeconds.end(),
        Epoch
    );

    // The distance between the two iterators gives us the number of leap years
    const int nLeapSeconds = static_cast<int>(std::abs(std::distance(y2000, it)));
    return nLeapSeconds;
}

double RenderableOrbitalKepler::calculateSemiMajorAxis(double meanMotion) {
    constexpr const double GravitationalConstant = 6.6740831e-11;
    constexpr const double MassEarth = 5.9721986e24;
    constexpr const double muEarth = GravitationalConstant * MassEarth;

    // Use Kepler's 3rd law to calculate semimajor axis
    // a^3 / P^2 = mu / (2pi)^2
    // <=> a = ((mu * P^2) / (2pi^2))^(1/3)
    // with a = semimajor axis
    // P = period in seconds
    // mu = G*M_earth
    double period = std::chrono::seconds(std::chrono::hours(24)).count() / meanMotion;

    const double pisq = glm::pi<double>() * glm::pi<double>();
    double semiMajorAxis = pow((muEarth * period*period) / (4 * pisq), 1.0 / 3.0);

    // We need the semi major axis in km instead of m
    return semiMajorAxis / 1000.0;
}

double RenderableOrbitalKepler::epochFromSubstring(const std::string& epochString) {
    // The epochString is in the form:
    // YYDDD.DDDDDDDD
    // With YY being the last two years of the launch epoch, the first DDD the day
    // of the year and the remaning a fractional part of the day

    // The main overview of this function:
    // 1. Reconstruct the full year from the YY part
    // 2. Calculate the number of seconds since the beginning of the year
    // 2.a Get the number of full days since the beginning of the year
    // 2.b If the year is a leap year, modify the number of days
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
    std::string yearPrefix = [y = epochString.substr(0, 2)](){
        int year = std::atoi(y.c_str());
        return year >= 57 ? "19" : "20";
    }();
    const int year = std::atoi((yearPrefix + epochString.substr(0, 2)).c_str());
    const int daysSince2000 = countDays(year);

    // 2.
    // 2.a
    double daysInYear = std::atof(epochString.substr(2).c_str());

    // 2.b
    const bool isInLeapYear = std::find(
        LeapYears.begin(),
        LeapYears.end(),
        year
    ) != LeapYears.end();
    if (isInLeapYear && daysInYear >= 60) {
        // We are in a leap year, so we have an effective day more if we are
        // beyond the end of february (= 31+29 days)
        --daysInYear;
    }

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
    const double nSecondsEpochOffset = static_cast<double>(
        seconds(hours(12)).count()
    );

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
    // 2. Calculate the number of seconds since the beginning of the year
    // 2.a Get the number of full days since the beginning of the year
    // 2.b If the year is a leap year, modify the number of days
    // 3. Convert the number of days to a number of seconds
    // 4. Get the number of leap seconds since January 1st, 2000 and remove them
    // 5. Adjust for the fact the epoch starts on 1st January at 12:00:00, not
    // midnight

    // 1
    int year = std::atoi(epochString.substr(0, 4).c_str());
    const int daysSince2000 = countDays(year);

    // 2.
    // 2.a
    int monthNum = std::atoi(epochString.substr(4, 2).c_str());
    int dayOfMonthNum = std::atoi(epochString.substr(6, 2).c_str());
    int wholeDaysInto = daysIntoGivenYear(monthNum, dayOfMonthNum);
    double fractionOfDay = std::atof(epochString.substr(9, 7).c_str());
    double daysInYear = static_cast<double>(wholeDaysInto) + fractionOfDay;

    // 2.b
    const bool isInLeapYear = std::find(
        LeapYears.begin(),
        LeapYears.end(),
        year
    ) != LeapYears.end();
    if (isInLeapYear && daysInYear >= 60) {
        // We are in a leap year, so we have an effective day more if we are
        // beyond the end of february (= 31+29 days)
        --daysInYear;
    }

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
    const double nSecondsEpochOffset = static_cast<double>(
        seconds(hours(12)).count()
        );

    // Combine all of the values
    const double epoch = nSecondsSince2000 + nLeapSecondsOffset - nSecondsEpochOffset;
    return epoch;
}

int RenderableOrbitalKepler::daysIntoGivenYear(int month, int dayOfMonth) {
    //month and dayCount are zero-based. Does NOT account for leap year.
    month -= 1;
    int dayCount = dayOfMonth - 1;

    for (int m = Months::January; m < month; ++m) {
        dayCount += DaysOfMonths[m];
    }
    return dayCount;
}
  
RenderableOrbitalKepler::RenderableOrbitalKepler(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _path(PathInfo)
    , _segmentQuality(SegmentQualityInfo, 10, 1, 100)
    , _upperLimit(UpperLimitInfo, 1000, 1, 1000000)
{
    documentation::testSpecificationAndThrow(
         Documentation(),
         dictionary,
         "RenderableOrbitalKepler"
    );

    _path = dictionary.value<std::string>(PathInfo.identifier);
    _segmentQuality = static_cast<int>(
        dictionary.value<double>(SegmentQualityInfo.identifier)
    );

    if (dictionary.hasKeyAndValue<glm::vec3>(LineColorInfo.identifier)) {
        _appearance.lineColor = dictionary.value<glm::vec3>(LineColorInfo.identifier);
    }
    if (dictionary.hasKeyAndValue<double>(TrailFadeInfo.identifier)) {
        _appearance.lineFade = static_cast<float>(
            dictionary.value<double>(TrailFadeInfo.identifier)
        );
    }
    else {
        _appearance.lineFade = 20;
    }

    if (dictionary.hasKeyAndValue<double>(UpperLimitInfo.identifier)) {
        _upperLimit = static_cast<unsigned int>(
            dictionary.value<double>(UpperLimitInfo.identifier)
        );
    }
    else {
        _upperLimit = 0;
    }

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _appearance.lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
        );
    }
    else {
        _appearance.lineWidth = 2.0;
    }
    reinitializeTrailBuffers = std::function<void()>([this] { initializeGL(); });
    _path.onChange(reinitializeTrailBuffers);
    _segmentQuality.onChange(reinitializeTrailBuffers);

    addPropertySubOwner(_appearance);
    addProperty(_path);
    addProperty(_segmentQuality);
    addProperty(_opacity);

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
    if (_data.empty())
        return;

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
    float fade = static_cast<float>(pow(_appearance.lineFade.maxValue() - _appearance.lineFade, 2.0));

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
        glDrawArrays(GL_LINE_STRIP, vertices, _segmentSize[i] + 1);
        vertices = vertices + _segmentSize[i] + 1;
    }
    glBindVertexArray(0);

    _programObject->deactivate();

}

void RenderableOrbitalKepler::updateBuffers() {
    readDataFile(_path);

    size_t nVerticesPerOrbit = 0;

    int numOrbits = _data.size();
    for (size_t i = 0; i < numOrbits; ++i) {
        nVerticesPerOrbit += _segmentSize[i] + 1;
    }
    _vertexBufferData.resize(nVerticesPerOrbit);

    size_t vertexBufIdx = 0;
    for (size_t orbitIdx = 0; orbitIdx < numOrbits; ++orbitIdx) {
        KeplerParameters orbit = _data[orbitIdx];

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

        for (size_t j = 0 ; j < _segmentSize[orbitIdx]; ++j) {
            double timeOffset = orbit.period * 
                static_cast<double>(j)/ static_cast<double>(_segmentSize[orbitIdx]);

            glm::dvec3 position = _keplerTranslator.position({
                {},
                Time(timeOffset + orbit.epoch),
                Time(0.0),
                false
            });

            double positionX = position.x; 
            double positionY = position.y; 
            double positionZ = position.z; 

            _vertexBufferData[vertexBufIdx].x = static_cast<float>(positionX);
            _vertexBufferData[vertexBufIdx].y = static_cast<float>(positionY);
            _vertexBufferData[vertexBufIdx].z = static_cast<float>(positionZ);
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
    glVertexAttribPointer(1, 2, GL_DOUBLE, GL_FALSE, sizeof(TrailVBOLayout), (GLvoid*)(4 * sizeof(GL_FLOAT)));

    glBindVertexArray(0);

}
    
}
