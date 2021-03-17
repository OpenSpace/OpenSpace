/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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


#include <modules/airtraffic/rendering/renderableairtraffichistorical.h>
#include <modules/airtraffic/rendering/renderableairtrafficbound.h>


#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <iostream>
#include <ghoul/misc/csvreader.h>
#include <fstream>
#include <string>
#include <time.h>
#include <charconv>



using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    
    constexpr const std::array<const char*, 8> UniformNames = {
        "modelViewProjection", 
        "maximumColor",
        "minimumColor",
        "opacity", 
        "latitudeThreshold", 
        "longitudeThreshold",
        "dailyFlights",
        "time"
    };

    constexpr openspace::properties::Property::PropertyInfo MaximumColorInfo = {
       "MaximumColor",
       "Maximum Color",
       "The color used to represent the maximum number of flights. This value is used to interpolate a color between max and min."
    };

    constexpr openspace::properties::Property::PropertyInfo MinimumColorInfo = {
       "MinimumColor",
       "Minimum Color",
       "The color used to represent the minimum number of flights. This value is used to interpolate a color between max and min."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
       "Opacity",
       "Opacity",
       "The opacity of the lines used to represent aircrafts."
    };

    constexpr openspace::properties::Property::PropertyInfo DailyFlightsInfo = {
       "DailyFlights",
       "Daily Flights",
       "The daily number of flights. Filtration does not affect this value."
    };

} // namespace


namespace openspace {

documentation::Documentation RenderableAirTrafficHistorical::Documentation() {
    using namespace documentation;
    return {
        "Renderable Air Traffic Historical",
        "RenderableAirTrafficHistorical",
        {
        }
    };
}

RenderableAirTrafficHistorical::RenderableAirTrafficHistorical(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _maximumColor(MaximumColorInfo, glm::vec3(0.7f, 0.f, 1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _minimumColor(MinimumColorInfo, glm::vec3(1.f, 0.7f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _opacity(OpacityInfo, 0.8f, 0.f, 1.f)
    , _nDailyFlights(DailyFlightsInfo, 0, 0, 150000)
    {
        addProperty(_maximumColor);
        addProperty(_minimumColor);
        addProperty(_opacity);

        _nDailyFlights.setReadOnly(true);
        addProperty(_nDailyFlights);

        setRenderBin(RenderBin::PostDeferredTransparent);
    };

    void RenderableAirTrafficHistorical::initialize() {
        return;
    };

    void RenderableAirTrafficHistorical::deinitialize() {
        return;
    };

    void RenderableAirTrafficHistorical::initializeGL() {
        glGenVertexArrays(1, &_vertexArrayA);
        glGenBuffers(1, &_vertexBufferA);

        glGenVertexArrays(1, &_vertexArrayB);
        glGenBuffers(1, &_vertexBufferB);

        _shader = global::renderEngine->buildRenderProgram(
            "AirTrafficHistoricalProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    };

    void RenderableAirTrafficHistorical::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBufferA);
        glDeleteVertexArrays(1, &_vertexArrayA);

        glDeleteBuffers(1, &_vertexBufferB);
        glDeleteVertexArrays(1, &_vertexArrayB);

        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;

        return;
    };

    bool RenderableAirTrafficHistorical::isReady() const {
        return true;
    };

    void RenderableAirTrafficHistorical::render(const RenderData& data, RendererTasks& rendererTask) {
        
        // YYYY-MM-DD
        std::time_t date = data.time.j2000Seconds() + 946728000;
        tm* tempTime = gmtime(&date);
        Date inDate; 
        inDate.year = tempTime->tm_year + 1900;
        inDate.month = tempTime->tm_mon + 1;
        inDate.day = tempTime->tm_mday;


        if (inDate == _nextDate) {
            std::cout << "Fetching next day" << std::endl;
            _currentDate = _nextDate;
            _nextDate = _nextDate.getTomorrow();
            fetchData(_nextDate); updateBuffers(_nextDate);
        }
        else if (inDate != _currentDate) {
            std::cout << "Fetching two days" << std::endl;
            _currentDate = inDate;
            _nextDate = _currentDate.getTomorrow();
            fetchData(_currentDate); updateBuffers(_currentDate);
            fetchData(_nextDate); updateBuffers(_nextDate);
        }


        if (_bufferA.vertexBufferData.empty() && _bufferB.vertexBufferData.empty()) return;

        _shader->activate();

        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            glm::dmat4(data.modelTransform.rotation) *
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        _shader->setUniform(
            _uniformCache.modelViewProjection,
            data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
        );

        _shader->setUniform(_uniformCache.maximumColor, _maximumColor);
        _shader->setUniform(_uniformCache.minimumColor, _minimumColor);
        _shader->setUniform(_uniformCache.opacity, _opacity);
        _shader->setUniform(_uniformCache.latitudeThreshold, RenderableAirTrafficBound::getLatBound());
        _shader->setUniform(_uniformCache.longitudeThreshold, RenderableAirTrafficBound::getLonBound());
        _shader->setUniform(_uniformCache.dailyFlights, _nDailyFlights);
        _shader->setUniform(_uniformCache.time, static_cast<int>(data.time.j2000Seconds() + 946728000));

        glLineWidth(1.f);
        
        glBindVertexArray(_vertexArrayA);
        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferA.vertexBufferData.size()));
        //glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferB.vertexBufferData.size()));
        glBindVertexArray(0);

        glBindVertexArray(_vertexArrayB);
        //glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferA.vertexBufferData.size()));
        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferB.vertexBufferData.size()));
        glBindVertexArray(0);

        _shader->deactivate();
    };

    // Old render function
    /*
    void RenderableAirTrafficHistorical::render(const RenderData& data, RendererTasks& rendererTask) {

        // YYYY-MM-DD
        std::string_view date = data.time.ISO8601().substr(0, 10);
        // Current date will determine when its time for an update
        if (_currentDate != date && !_isDataLoading) {
            _currentDate = date;
            _future = std::async(std::launch::async, &RenderableAirTrafficHistorical::fetchData, this, _currentDate);
            _isDataLoading = true;
        }

        // Check if new data finished loading. Update buffers ONLY if finished
        if (_future.valid() && _future.wait_for(seconds(0)) == std::future_status::ready) {
            if(_future.get()) {
                updateBuffers();
            }
            _isDataLoading = false;
        }

        if (_vertexBufferData.empty()) return;

        _shader->activate();

        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            glm::dmat4(data.modelTransform.rotation) *
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        _shader->setUniform(
            _uniformCache.modelViewProjection,
            data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
        );

        _shader->setUniform(_uniformCache.maximumColor, _maximumColor);
        _shader->setUniform(_uniformCache.minimumColor, _minimumColor);
        _shader->setUniform(_uniformCache.opacity, _opacity);
        _shader->setUniform(_uniformCache.latitudeThreshold, RenderableAirTrafficBound::getLatBound());
        _shader->setUniform(_uniformCache.longitudeThreshold, RenderableAirTrafficBound::getLonBound());
        _shader->setUniform(_uniformCache.dailyFlights, _nDailyFlights);
        int temp = data.time.j2000Seconds() + 946728000;
        _shader->setUniform(_uniformCache.time, temp);

        glBindVertexArray(_vertexArray);
        glLineWidth(1.f);
        
        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_vertexBufferData.size()));

        glBindVertexArray(0);

        _shader->deactivate();
    };
    */

    void RenderableAirTrafficHistorical::update(const UpdateData& data) {
        return;
    };

    bool RenderableAirTrafficHistorical::updateBuffers(const Date& date) {

        std::cout << "Entering update buffer with date: " << date.year << " - " << date.month << " - " << date.day << std::endl;
        //std::cout << "Current date : " << _currentDate.year << " - " << _currentDate.month << " - " << _currentDate.day <<
        //    " Next date: " << _nextDate.year << " - " << _nextDate.month << " - " << _nextDate.day << std::endl;
        int bufIdx = 0;

        if(_bufferA.date != _currentDate && _bufferA.date != _nextDate) {
            // Fill A
            _bufferA.vertexBufferData.clear();
            _bufferA.vertexBufferData.resize(2 * _data.size());

            AircraftVBOLayout startVBO;
            AircraftVBOLayout endVBO;

            std::tm timeFirst = {};
            std::tm timeLast = {};

            

            for (auto dataLine : _data) {
                std::istringstream ssFirst(dataLine[0]);
                ssFirst >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
                std::time_t first = _mkgmtime(&timeFirst);

                std::istringstream ssLast(dataLine[1]);
                ssLast >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
                std::time_t last = _mkgmtime(&timeFirst);

                if (ssFirst.fail() || ssLast.fail()) {
                    throw std::runtime_error{ "Failed to parse time string." };
                    continue;
                }

                startVBO.latitude = dataLine[2] == "" ? _THRESHOLD : std::stof(dataLine[2]);
                startVBO.longitude = dataLine[3] == "" ? _THRESHOLD : std::stof(dataLine[3]);
                startVBO.firstSeen = first;
                startVBO.lastSeen = last;
                _bufferA.vertexBufferData[bufIdx] = startVBO; ++bufIdx;

                endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
                endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
                endVBO.firstSeen = first;
                endVBO.lastSeen = last;
                _bufferA.vertexBufferData[bufIdx] = endVBO; ++bufIdx;
            }

            glBindVertexArray(_vertexArrayA);
            glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferA);

            glBufferData(
                GL_ARRAY_BUFFER,
                _bufferA.vertexBufferData.size() * sizeof(AircraftVBOLayout),
                _bufferA.vertexBufferData.data(),
                GL_STATIC_DRAW
            );

            // lat and long
            glEnableVertexAttribArray(0);
            glVertexAttribPointer(
                0,
                2,
                GL_FLOAT,
                GL_FALSE,
                sizeof(AircraftVBOLayout),
                nullptr
            );

            // firstseen & lastseen as vec2 at pos 1 from AircraftVBOLayout
            glEnableVertexAttribArray(1);
            glVertexAttribIPointer(
                1,
                2,
                GL_INT,
                sizeof(AircraftVBOLayout),
                reinterpret_cast<GLvoid*>(2 * sizeof(GL_FLOAT))
            );

            glBindVertexArray(0);
            
            _bufferA.date = date;
        }
        else {
            // Fill B
            _bufferB.vertexBufferData.clear();
            _bufferB.vertexBufferData.resize(2 * _data.size());

            AircraftVBOLayout startVBO;
            AircraftVBOLayout endVBO;

            std::tm timeFirst = {};
            std::tm timeLast = {};

            for (auto dataLine : _data) {
                std::istringstream ssFirst(dataLine[0]);
                ssFirst >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
                std::time_t first = _mkgmtime(&timeFirst);

                std::istringstream ssLast(dataLine[1]);
                ssLast >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
                std::time_t last = _mkgmtime(&timeFirst);

                if (ssFirst.fail() || ssLast.fail()) {
                    throw std::runtime_error{ "Failed to parse time string." };
                    continue;
                }

                startVBO.latitude = dataLine[2] == "" ? _THRESHOLD : std::stof(dataLine[2]);
                startVBO.longitude = dataLine[3] == "" ? _THRESHOLD : std::stof(dataLine[3]);
                startVBO.firstSeen = first;
                startVBO.lastSeen = last;
                _bufferB.vertexBufferData[bufIdx] = startVBO; ++bufIdx;

                endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
                endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
                endVBO.firstSeen = first;
                endVBO.lastSeen = last;
                _bufferB.vertexBufferData[bufIdx] = endVBO; ++bufIdx;
            }

            glBindVertexArray(_vertexArrayB);
            glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferB);

            glBufferData(
                GL_ARRAY_BUFFER,
                 _bufferB.vertexBufferData.size() * sizeof(AircraftVBOLayout),
                 _bufferB.vertexBufferData.data(),
                GL_STATIC_DRAW
            );

            // lat and long
            glEnableVertexAttribArray(0);
            glVertexAttribPointer(
                0,
                2,
                GL_FLOAT,
                GL_FALSE,
                sizeof(AircraftVBOLayout),
                nullptr
            );

            // firstseen & lastseen as vec2 at pos 1 from AircraftVBOLayout
            glEnableVertexAttribArray(1);
            glVertexAttribIPointer(
                1,
                2,
                GL_INT,
                sizeof(AircraftVBOLayout),
                reinterpret_cast<GLvoid*>(2 * sizeof(GL_FLOAT))
            );

            glBindVertexArray(0);

            _bufferB.date = date;
        }

        _nDailyFlights = (_bufferA.date == _currentDate) ? _bufferA.vertexBufferData.size()/2 : _bufferB.vertexBufferData.size()/2;

        std::cout << "Updated buffer: " << ((_bufferA.date == date) ? "A" : "B") << " with size: " << bufIdx / 2 << std::endl;
        std::cout << "Total buffer size: " << _bufferA.vertexBufferData.size() / 2 + _bufferB.vertexBufferData.size() / 2 << std::endl << std::endl;
        return true;
    }

    // Old update buffers function
    /*
    bool RenderableAirTrafficHistorical::updateBuffers() {

        _vertexBufferData.clear();

        _nDailyFlights = _data.size();
        _vertexBufferData.resize(2 * _nDailyFlights);

        AircraftVBOLayout startVBO;
        AircraftVBOLayout endVBO;

        std::tm timeFirst = {};
        std::tm timeLast = {};
            
        for (auto dataLine : _data) {
            std::istringstream ssFirst(dataLine[0]);
            ssFirst >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
            std::time_t first = _mkgmtime(&timeFirst);

            std::istringstream ssLast(dataLine[1]);
            ssLast >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
            std::time_t last = _mkgmtime(&timeFirst);

            if (ssFirst.fail() || ssLast.fail()) {
                throw std::runtime_error{ "Failed to parse time string." };
                continue;
            }

            startVBO.latitude = dataLine[2] == "" ? _THRESHOLD : std::stof(dataLine[2]);
            startVBO.longitude = dataLine[3] == "" ? _THRESHOLD : std::stof(dataLine[3]);
            startVBO.firstSeen = first;
            startVBO.lastSeen = last;
            _vertexBufferData.push_back(startVBO);

            endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
            endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
            endVBO.firstSeen = first;
            endVBO.lastSeen = last;
            _vertexBufferData.push_back(endVBO);
        }

        glBindVertexArray(_vertexArray);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);

        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexBufferData.size() * sizeof(AircraftVBOLayout),
            _vertexBufferData.data(),
            GL_STATIC_DRAW
        );

        // lat and long
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0, 
            2, 
            GL_FLOAT, 
            GL_FALSE, 
            sizeof(AircraftVBOLayout),  
            nullptr
        );

        // firstseen & lastseen as vec2 at pos 1 from AircraftVBOLayout
        glEnableVertexAttribArray(1);
        glVertexAttribIPointer(
            1,
            2,
            GL_INT,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(2 * sizeof(GL_FLOAT))
        );

        glBindVertexArray(0);
        
        return true;
    }
    */

    bool RenderableAirTrafficHistorical::fetchData( const Date& date ){

        std::string strMonth = std::to_string(date.month);
        std::string strDay = std::to_string(date.day);

        if (date.month < 10) strMonth = "0" + std::to_string(date.month);
        if (date.day < 10) strDay = "0" + std::to_string(date.day);

        std::string fileName = std::to_string(date.year) + "/"
        + strMonth + "/" + strDay + ".csv";

        std::cout << "Fetching data: " << fileName << std::endl; 
        //std::string fileName = date.substr(0, 4) + "/" 
        //+ date.substr(5, 2) + "/" + date.substr(8, 2) + ".csv";

        _data.clear();

        try {
            _data = ghoul::loadCSVFile(absPath(_PATH + fileName), {
                "firstseen",
                "lastseen",
                "latitude_1",
                "longitude_1",
                "latitude_2",
                "longitude_2"
            }, false);
        }
        catch(ghoul::RuntimeError&){
            std::cout << "Invalid date" << std::endl;
        }

        return !_data.empty();
    }

} // namespace openspace

