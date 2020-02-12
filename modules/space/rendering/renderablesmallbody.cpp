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
        "The file path to the SBDB file to read"
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
    constexpr openspace::properties::Property::PropertyInfo FadeInfo = {
        "Fade",
        "Line fade",
        "The fading factor that is applied to the trail if the 'EnableFade' value is "
        "'true'. If it is 'false', this setting has no effect. The higher the number, "
        "the less fading is applied."
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

const std::vector<int> DaysOfMonths = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

enum Months {
    January = 0,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December
};

int daysIntoGivenYear(int year, int month, int dayOfMonth) {
    //month and dayCount are zero-based
    month -= 1;
    int dayCount = dayOfMonth - 1;

    for (int m = Months::January; m < month; ++m) {
        dayCount += DaysOfMonths[m];
    }
    return dayCount;
}

double epochFromYMDdSubstring(const std::string& epochString) {
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
    int wholeDaysInto = daysIntoGivenYear(year, monthNum, dayOfMonthNum);
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

documentation::Documentation RenderableSmallBody::Documentation() {
    using namespace documentation;
    return {
        "RenderableSmallBody",
        "small solar system bodies",
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
                FadeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeInfo.description
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
    
RenderableSmallBody::RenderableSmallBody(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _path(PathInfo)
    , _nSegments(SegmentsInfo)

{
    documentation::testSpecificationAndThrow(
         Documentation(),
         dictionary,
         "RenderableSmallBody"
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

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _appearance.lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
            );
    }
    else {
        _appearance.lineWidth = 2.0;
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
   
    
void RenderableSmallBody::readJplSbDb(const std::string& filename) {
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

    std::string line;
    std::string name;
    std::string field;
    std::streamoff csvLine = -1;
    int fieldCount = 0;
    const std::string expectedHeaderLine =
        "full_name,epoch_cal,e,a,i,om,w,ma,per";

    try {
        std::getline(file, line); // get rid of first line (header)
        numberOfLines -= 1;
        if (line.compare(expectedHeaderLine) != 0) {
            LERROR(fmt::format(
                "File {} does not have the appropriate JPL SBDB header at line 1.",
                filename
            ));
            file.close();
            return;
        }

        for (csvLine = 0; csvLine < numberOfLines; csvLine++) {
            fieldCount = 0;
            KeplerParameters keplerElements;
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
            keplerElements.inclination = std::stod(field);
            fieldCount++;

            // Longitude of ascending node (degrees)
            if (!std::getline(file, field, ',')) {
                throw std::invalid_argument("Unable to read ascending node from line"
                    + std::to_string(csvLine + 1));
            }
            keplerElements.ascendingNode = std::stod(field);
            fieldCount++;

            // Argument of Periapsis (degrees)
            if (!std::getline(file, field, ',')) {
                throw std::invalid_argument("Unable to read arg of periapsis from line"
                    + std::to_string(csvLine + 1));
            }
            keplerElements.argumentOfPeriapsis = std::stod(field);
            fieldCount++;

            // Mean Anomaly (degrees)
            if (!std::getline(file, field, ',')) {
                throw std::invalid_argument("Unable to read mean anomaly from line"
                    + std::to_string(csvLine + 1));
            }
            keplerElements.meanAnomaly = std::stod(field);
            fieldCount++;

            // Period (days)
            if (!std::getline(file, field)) {
                throw std::invalid_argument("Unable to read period from line"
                    + std::to_string(csvLine + 1));
            }
            keplerElements.period = std::stod(field);
            keplerElements.period *= convertDaysToSecs;
            fieldCount++;

            _sbData.push_back(keplerElements);
            _sbNames.push_back(name);
        }
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

void RenderableSmallBody::initializeGL() {
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
    setRenderBin(Renderable::RenderBin::Overlay);
}
    
void RenderableSmallBody::deinitializeGL() {
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

bool RenderableSmallBody::isReady() const {
    return _programObject != nullptr;
}

void RenderableSmallBody::render(const RenderData& data, RendererTasks&) {
    if (_sbData.empty())
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

    const size_t nrOrbits = _sbData.size();
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

void RenderableSmallBody::updateBuffers() {
    readJplSbDb(_path);

    const size_t nVerticesPerOrbit = _nSegments + 1;
    _vertexBufferData.resize(_sbData.size() * nVerticesPerOrbit);
    size_t orbitindex = 0;

    for (const auto& orbit : _sbData) {
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
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(TrailVBOLayout), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_DOUBLE, GL_FALSE, sizeof(TrailVBOLayout), (GLvoid*)(4*sizeof(GL_FLOAT)) );

    glBindVertexArray(0);
}
    
}
