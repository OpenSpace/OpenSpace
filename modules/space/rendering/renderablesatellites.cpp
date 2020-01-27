 /****************************************************************************************
  *                                                                                       *
  * OpenSpace                                                                             *
  *                                                                                       *
  * Copyright (c) 2014-2018                                                               *
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
    
    constexpr const char* KeyFile = "Path";
    constexpr const char* KeyLineNum = "LineNumber";
}

namespace openspace {
    
    // The list of leap years only goes until 2056 as we need to touch this file then
    // again anyway ;)
    const std::vector<int> LeapYears = {
        1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996,
        2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040,
        2044, 2048, 2052, 2056
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

    double calculateSemiMajorAxis(double meanMotion) {
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

double epochFromSubstring(const std::string& epochString) {
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
        // those from 00-56 correspond to 2000-2056. We'll see each other again in 2057!

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
            }
        }
    };
}
    
RenderableSatellites::RenderableSatellites(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _path(PathInfo)
    , _nSegments(SegmentsInfo, 120, 4, 1024)
{
    documentation::testSpecificationAndThrow(
         Documentation(),
         dictionary,
         "RenderableSatellites"
        );

    _path = dictionary.value<std::string>(PathInfo.identifier);
    _nSegments = static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));

    if (dictionary.hasKeyAndValue<glm::vec3>(LineColorInfo.identifier)) {
        _appearance.lineColor = dictionary.value<glm::vec3>(LineColorInfo.identifier);
    }
    if (dictionary.hasKeyAndValue<double>("FadeInfo")) {
        _appearance.lineFade = static_cast<float>(
            dictionary.value<double>("FadeInfo")
        );
    }
    else {
        _appearance.lineFade = 20;
    }

    auto reinitializeTrailBuffers = [this]() {
        initializeGL();
    };

    _path.onChange(reinitializeTrailBuffers);
    _nSegments.onChange(reinitializeTrailBuffers);

    addPropertySubOwner(_appearance);
    addProperty(_path);
    addProperty(_nSegments);
    addProperty(_opacity);

    setRenderBin(Renderable::RenderBin::Overlay);
}
   
    
void RenderableSatellites::readTLEFile(const std::string& filename) {
    if (!FileSys.fileExists(filename)) {
        throw ghoul::RuntimeError(fmt::format(
            "Satellite TLE file {} does not exist.", filename
        ));
    }

    std::ifstream file;
    file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    file.open(filename);

    std::streamoff numberOfLines = std::count(std::istreambuf_iterator<char>(file), 
                                   std::istreambuf_iterator<char>(), '\n' );
    file.seekg(std::ios_base::beg); // reset iterator to beginning of file

    // 3 because a TLE has 3 lines per element/ object.
    std::streamoff numberOfObjects = numberOfLines / 3;

    std::string line = "-";
    for (std::streamoff i = 0; i < numberOfObjects; i++) {
        std::getline(file, line); // get rid of title
        
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

        _TLEData.push_back(keplerElements);

    }
    file.close();
}

void RenderableSatellites::initializeGL() {
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
    
void RenderableSatellites::deinitializeGL() {
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

bool RenderableSatellites::isReady() const {
    return _programObject != nullptr;
}

void RenderableSatellites::render(const RenderData& data, RendererTasks&) {
    if (_TLEData.empty())
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

    const size_t nrOrbits = _TLEData.size();
    gl::GLint vertices = 0;

    //glDepthMask(false);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE)

    glBindVertexArray(_vertexArray);
    for (size_t i = 0; i < nrOrbits; ++i) {
        glDrawArrays(GL_LINE_STRIP, vertices, _nSegments + 1);
        vertices = vertices + _nSegments + 1;
    }
    glBindVertexArray(0);
    
    _programObject->deactivate();

}

void RenderableSatellites::updateBuffers() {
    readTLEFile(_path);

    const size_t nVerticesPerOrbit = _nSegments + 1;
    _vertexBufferData.resize(_TLEData.size() * nVerticesPerOrbit);
    size_t orbitindex = 0;

    for (const auto& orbit : _TLEData) {
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

        for (size_t i=0 ; i < nVerticesPerOrbit; ++i) {
            size_t index = orbitindex * nVerticesPerOrbit  + i;

            double timeOffset = orbit.period * 
                    static_cast<double>(i)/ static_cast<double>(_nSegments); 
            
            glm::dvec3 position = _keplerTranslator.position({
                {},
                Time(timeOffset + orbit.epoch),
                Time(0.0),
                false
            });
            
            double positionX = position.x; 
            double positionY = position.y; 
            double positionZ = position.z; 

            _vertexBufferData[index].x = static_cast<float>(positionX);
            _vertexBufferData[index].y = static_cast<float>(positionY);
            _vertexBufferData[index].z = static_cast<float>(positionZ);
            _vertexBufferData[index].time = static_cast<float>(timeOffset);
            _vertexBufferData[index].epoch = orbit.epoch;
            _vertexBufferData[index].period = orbit.period;
        }
      
        ++orbitindex;
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
