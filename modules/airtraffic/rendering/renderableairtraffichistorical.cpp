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

#include <openspace/query/query.h>
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
#include <mutex>


std::mutex mutexLock;

using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    
    constexpr const std::array<const char*, 8> UniformNames = {
        "modelViewProjection", 
        "opacity", 
        "latitudeThreshold", 
        "longitudeThreshold",
        "time",
        "cameraPosition",
        "modelTransform",
        "clipping"
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
       "Opacity",
       "Opacity",
       "The opacity of the lines used to represent aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderedFlightsInfo = {
       "RenderedFlights",
       "Rendered Flights",
       "The rendered number of flights."
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
    , _opacity(OpacityInfo, 0.4f, 0.f, 1.f)
    , _nRenderedFlights(RenderedFlightsInfo, 0, 0, 10000)
    {
        addProperty(_opacity);

        _nRenderedFlights.setReadOnly(true);
        addProperty(_nRenderedFlights);

        setRenderBin(RenderBin::PostDeferredTransparent);
    };

    void RenderableAirTrafficHistorical::initializeGL() {

        glGenVertexArrays(1, &_bufferA.vertexArray);
        glGenBuffers(1, &_bufferA.vertexBuffer);

        glGenVertexArrays(1, &_bufferB.vertexArray);
        glGenBuffers(1, &_bufferB.vertexBuffer);

        glGenVertexArrays(1, &_bufferC.vertexArray);
        glGenBuffers(1, &_bufferC.vertexBuffer);

        // SSBO
        glGenBuffers(1, &_ssbo);

        _shader = global::renderEngine->buildRenderProgram(
            "AirTrafficHistoricalProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    };

    void RenderableAirTrafficHistorical::deinitializeGL() {
        glDeleteBuffers(1, &_bufferA.vertexBuffer);
        glDeleteVertexArrays(1, &_bufferA.vertexArray);

        glDeleteBuffers(1, &_bufferB.vertexBuffer);
        glDeleteVertexArrays(1, &_bufferB.vertexArray);

        glDeleteBuffers(1, &_bufferC.vertexBuffer);
        glDeleteVertexArrays(1, &_bufferC.vertexArray);

        glDeleteBuffers(1, &_ssbo);

        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;

        return;
    };

    bool RenderableAirTrafficHistorical::isReady() const {
        return _shader != nullptr;
    };

    // The main render function
    void RenderableAirTrafficHistorical::render(const RenderData& data, RendererTasks& rendererTask) {

        // Keep track of the current time in openspace to fetch correct data
        // YYYY-MM-DD
        double timeNow = data.time.j2000Seconds();
        Date inDate = Date(timeNow);

        bool reverseTime = _lastUpdate > timeNow;

        // SSBO
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssbo);
        glBufferData(GL_SHADER_STORAGE_BUFFER, sizeof(int), &_nFilteredAircraft, GL_STATIC_DRAW);
        glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 5, _ssbo);

        // Update one buffer if time moves forward
        if (inDate == _nextDate && !_isDataLoading && !reverseTime) {
            
            _currentDate = _nextDate;
            _nextDate = _currentDate.getTomorrow();
            _nextNextDate = _nextDate.getTomorrow();

            _isDataLoading = true;

            // Fetch day after next day async to avoid lag
            _future = std::async(std::launch::async, &RenderableAirTrafficHistorical::updateBuffers, this, _nextNextDate, true);
            _lastUpdate = timeNow;
        }
        // Update one buffer if time moves in reverse
        else if(inDate == _nextNextDate && !_isDataLoading && reverseTime){
            _currentDate = _nextDate;
            _nextDate = _currentDate.getYesterday();
            _nextNextDate = _nextDate.getYesterday();

            _isDataLoading = true;

            // Fetch day after next day async to avoid lag
            _future = std::async(std::launch::async, &RenderableAirTrafficHistorical::updateBuffersReverse, this, _nextNextDate, true);
            _lastUpdate = timeNow;
        }
        // Update all three buffers
        // This is performed if the date changes 
        // with more than one day in one step
        else if ((inDate != _currentDate && !_isDataLoading && !reverseTime) || (inDate != _nextDate && !_isDataLoading && reverseTime)) {
            _bufferA.date = Date(); _bufferB.date = Date(); _bufferC.date = Date();

            if(!reverseTime) {
                _currentDate = inDate;
                _nextDate = _currentDate.getTomorrow(); 
                _nextNextDate = _nextDate.getTomorrow();
            }
            else {
                _currentDate = inDate.getTomorrow();
                _nextDate = inDate;
                _nextNextDate = _nextDate.getYesterday();
            }

            updateBuffers(_currentDate);
            updateBuffers(_nextDate);
            updateBuffers(_nextNextDate);

            _lastUpdate = timeNow;

        }

        // Check if new data finished loading. Update buffers ONLY if finished
        if (_future.valid() && _future.wait_for(seconds(0)) == std::future_status::ready) {
            _future.get();
            // As the asynchronous stage is finished, send to glBindBuffer on the main thread and context
            sendToGLBuffer(_bufferA.date == _nextNextDate ? _bufferA : _bufferB.date == _nextNextDate ? _bufferB : _bufferC);
            _isDataLoading = false;
        }

        if (_bufferA.vertexBufferData.empty() && _bufferB.vertexBufferData.empty() && _bufferC.vertexBufferData.empty()) return;

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

        _shader->setUniform(_uniformCache.opacity, _opacity);
        _shader->setUniform(_uniformCache.latitudeThreshold, RenderableAirTrafficBound::getLatBound());
        _shader->setUniform(_uniformCache.longitudeThreshold, RenderableAirTrafficBound::getLonBound());
        // Time is sent to the shaders as a UNIX timestamp for easy interpolation
        _shader->setUniform(_uniformCache.time, static_cast<int>(data.time.j2000Seconds() + 365.25 * 24 * 60 * 60 * 30));
        _shader->setUniform(_uniformCache.cameraPosition, glm::vec3(data.camera.positionVec3()));
        _shader->setUniform(_uniformCache.modelTransform, glm::mat4(modelTransform));


        // Check if Earth is enabled and if clipping should be enabled or not
        const Renderable* Earth = renderable("Earth");
        if (Earth != nullptr) {
            _shader->setUniform(_uniformCache.clipping, Earth->isEnabled());
        }
        else {
            _shader->setUniform(_uniformCache.clipping, true);
        }

        glLineWidth(1.f);

        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_ALWAYS);
        
        // Draw the two buffers with dates that are either current or next
        if (_bufferA.date ==_currentDate || _bufferA.date == _nextDate) {
            // Draw A
            glBindVertexArray(_bufferA.vertexArray);
            glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferA.vertexBufferData.size()));
            glBindVertexArray(0);
        }
        if (_bufferB.date == _currentDate || _bufferB.date == _nextDate) {
            // Draw B
            glBindVertexArray(_bufferB.vertexArray);
            glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferB.vertexBufferData.size()));
            glBindVertexArray(0);
           
        }
        if (_bufferC.date == _currentDate || _bufferC.date == _nextDate) {
            // Draw C
            glBindVertexArray(_bufferC.vertexArray);
            glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_bufferC.vertexBufferData.size()));
            glBindVertexArray(0);
        }

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssbo);
        _ptr = (int*)glMapBuffer(
            GL_SHADER_STORAGE_BUFFER,
            GL_READ_ONLY
        );
        _nRenderedFlights = *_ptr;

        glDepthFunc(GL_LESS);

        _shader->deactivate();
        
    };

    // Updating buffers and handles both async and sync cases
    void RenderableAirTrafficHistorical::updateBuffersReverse(const Date& date, const bool async) {

        // Load data for the specific date
        fetchData(date);

        // Fill the un-used buffer
        if (_bufferA.date != _nextDate && _bufferA.date != _nextNextDate) {
            // Fill A
            fillBuffer(_bufferA);
            if (!async) sendToGLBuffer(_bufferA);
            _bufferA.date = date;
        }
        else if (_bufferB.date != _nextDate && _bufferB.date != _nextNextDate) {
            // Fill B
            fillBuffer(_bufferB);
            if (!async) sendToGLBuffer(_bufferB);
            _bufferB.date = date;
        }
        else {
            // Fill C
            fillBuffer(_bufferC);
            if (!async) sendToGLBuffer(_bufferC);
            _bufferC.date = date;
        }

        // Update the number of flights according to what date it is
        _nRenderedFlights = _bufferA.date == _nextDate ? _bufferA.vertexBufferData.size() / 2 :
            _bufferB.date == _nextDate ? _bufferB.vertexBufferData.size() / 2 : _bufferC.vertexBufferData.size() / 2;
    }

    // Updating buffers and handles both async and sync cases
    void RenderableAirTrafficHistorical::updateBuffers(const Date& date, const bool async) {

        // Load data for the specific date
        fetchData(date);

        // Fill the buffer
        if(_bufferA.date != _currentDate && _bufferA.date != _nextDate) {
            // Fill A
            fillBuffer(_bufferA); 
            if(!async) sendToGLBuffer(_bufferA);
            _bufferA.date = date;
        }
        else if(_bufferB.date != _currentDate && _bufferB.date != _nextDate) {
            // Fill B
            fillBuffer(_bufferB); 
            if (!async) sendToGLBuffer(_bufferB);
            _bufferB.date = date;
        } 
        else {
            // Fill C
            fillBuffer(_bufferC); 
            if (!async) sendToGLBuffer(_bufferC);
            _bufferC.date = date;
        }

        // Update the number of flights according to what date it is
        _nRenderedFlights = _bufferA.date == _currentDate ? _bufferA.vertexBufferData.size() / 2 : 
            _bufferB.date == _currentDate ? _bufferB.vertexBufferData.size() / 2 : _bufferC.vertexBufferData.size() / 2;
    }

    // This function will fill the backend storage with data
    void RenderableAirTrafficHistorical::fillBuffer(Buffer& buffer){
        int bufIdx = 0;

        buffer.vertexBufferData.clear();
        buffer.vertexBufferData.resize(2 * _data.size());

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
            buffer.vertexBufferData[bufIdx] = startVBO; ++bufIdx;

            endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
            endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
            endVBO.firstSeen = first;
            endVBO.lastSeen = last;
            buffer.vertexBufferData[bufIdx] = endVBO; ++bufIdx;
        }
    }

    // glBindBuffer is a critical section and needs to be performed 
    // on the same thread and context which is why this code is not 
    // executed directly in updateBuffers()
    void RenderableAirTrafficHistorical::sendToGLBuffer(Buffer &buffer) {
        glBindVertexArray(buffer.vertexArray);
        glBindBuffer(GL_ARRAY_BUFFER, buffer.vertexBuffer);

        glBufferData(
            GL_ARRAY_BUFFER,
            buffer.vertexBufferData.size() * sizeof(AircraftVBOLayout),
            buffer.vertexBufferData.data(),
            GL_STATIC_DRAW
        );

        // lat & long
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
    }

    // Fetches a new day of flight data from csv file in 
    // directory structure
    bool RenderableAirTrafficHistorical::fetchData( const Date& date ){

        std::string strMonth = std::to_string(date.month);
        std::string strDay = std::to_string(date.day);

        // Two digit month and day 
        if (date.month < 10) strMonth = "0" + std::to_string(date.month);
        if (date.day < 10) strDay = "0" + std::to_string(date.day);

        std::string fileName = std::to_string(date.year) + "/"
        + strMonth + "/" + strDay + ".csv";

        std::cout << "Fetching data: " << fileName << std::endl; 

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

