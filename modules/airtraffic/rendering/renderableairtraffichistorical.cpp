/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/documentation/documentation.h>
#include <openspace/query/query.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/csvreader.h>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    constexpr const std::array<const char*, 8> UniformNames = {
        "modelViewProjection", "opacity", "latitudeThreshold", "longitudeThreshold",
        "time", "cameraPosition", "modelTransform", "clipping"
    };

    constexpr openspace::properties::Property::PropertyInfo RenderedFlightsInfo = {
       "RenderedFlights",
       "Rendered Flights",
       "The rendered number of flights."
    };
} // namespace

namespace openspace {

RenderableAirTrafficHistorical::Date::Date(double timeNow) {
    std::time_t date = static_cast<time_t>(timeNow + 365.25 * 24 * 60 * 60 * 30);
    tm* tempTime = gmtime(&date);

    year = tempTime->tm_year + 1900;
    month = tempTime->tm_mon + 1;
    day = tempTime->tm_mday;
}

RenderableAirTrafficHistorical::Date RenderableAirTrafficHistorical::Date::getTomorrow() {
    const int days[12] = {
        31,28,31,30,31,30,31,31,30,31,30,31
    };

    // Just blindly add a day with no checks.
    Date tomorrow;
    tomorrow.year = year;
    tomorrow.month = month;
    tomorrow.day = day + 1;

    // Allow Feb 29 in leap year if needed.
    if (tomorrow.month == 2 && tomorrow.day == 29) {
        if (tomorrow.year % 400 == 0) {
            return tomorrow;
        }
        if ((tomorrow.year % 4 == 0) && (tomorrow.year % 100 != 0)) {
            return tomorrow;
        }
    }

    // Catch rolling into new month.
    if (tomorrow.day > days[tomorrow.month - 1]) {
        tomorrow.day = 1;
        tomorrow.month++;

        // Catch rolling into new year.
        if (tomorrow.month == 13) {
            tomorrow.month = 1;
            tomorrow.year++;
        }
    }

    return tomorrow;
}

RenderableAirTrafficHistorical::Date RenderableAirTrafficHistorical::Date::getYesterday()
{
    const int days[12] = {
        31,28,31,30,31,30,31,31,30,31,30,31
    };

    // Just blindly add a day with no checks
    Date yesterday;
    yesterday.year = year;
    yesterday.month = month;
    yesterday.day = day - 1;

    // Catch rolling into new month
    if (yesterday.day == 0) {
        yesterday.month--;
        yesterday.day = days[yesterday.month];

        // Allow Feb 29 in leap year if needed
        if (yesterday.month == 2) {
            if (yesterday.year % 400 == 0) {
                yesterday.day = 29;
                return yesterday;
            }
            if ((yesterday.year % 4 == 0) && (yesterday.year % 100 != 0)) {
                yesterday.day = 29;
                return yesterday;
            }
        }

        // Catch rolling into new year
        if (yesterday.month == 0) {
            yesterday.month = 12;
            yesterday.year--;
        }
    }

    return yesterday;
}

bool RenderableAirTrafficHistorical::Date::operator==(const Date& d) const {
    return (year == d.year && month == d.month && day == d.day);
}

bool RenderableAirTrafficHistorical::Date::operator!=(const Date& d) const {
    return !(*this == d);
}

RenderableAirTrafficHistorical::Date&
RenderableAirTrafficHistorical::Date::operator=(const Date& d)
{
    year = d.year;
    month = d.month;
    day = d.day;
    return *this;
}

documentation::Documentation RenderableAirTrafficHistorical::Documentation() {
    return {
        "Renderable Air Traffic Historical",
        "airtraffic_renderableairtraffichistorical",
        {}
    };
}

RenderableAirTrafficHistorical::RenderableAirTrafficHistorical(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _nRenderedFlights(RenderedFlightsInfo, 0, 0, 10000)
{
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
    _bufferA.vertexBuffer = 0;
    glDeleteVertexArrays(1, &_bufferA.vertexArray);
    _bufferA.vertexArray = 0;

    glDeleteBuffers(1, &_bufferB.vertexBuffer);
    _bufferB.vertexBuffer = 0;
    glDeleteVertexArrays(1, &_bufferB.vertexArray);
    _bufferB.vertexArray = 0;

    glDeleteBuffers(1, &_bufferC.vertexBuffer);
    _bufferC.vertexBuffer = 0;
    glDeleteVertexArrays(1, &_bufferC.vertexArray);
    _bufferC.vertexArray = 0;

    glDeleteBuffers(1, &_ssbo);
    _ssbo = 0;

    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;
};

bool RenderableAirTrafficHistorical::isReady() const {
    return _shader != nullptr;
};

// The main render function
void RenderableAirTrafficHistorical::render(const RenderData& data, RendererTasks&) {
    // Keep track of the current time in openspace to fetch correct data
    // YYYY-MM-DD
    double timeNow = data.time.j2000Seconds();
    Date inDate = Date(timeNow);

    const bool reverseTime = _lastUpdate > timeNow;

    // SSBO
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssbo);
    glBufferData(
        GL_SHADER_STORAGE_BUFFER,
        sizeof(int),
        &_nFilteredAircraft,
        GL_STATIC_DRAW
    );
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 5, _ssbo);

    // Update one buffer if time moves forward
    if (inDate == _nextDate && !_isDataLoading && !reverseTime) {
        _currentDate = _nextDate;
        _nextDate = _currentDate.getTomorrow();
        _nextNextDate = _nextDate.getTomorrow();

        _isDataLoading = true;

        // Fetch day after next day async to avoid lag
        _future = std::async(
            std::launch::async,
            &RenderableAirTrafficHistorical::updateBuffers,
            this,
            _nextNextDate,
            true
        );
        _lastUpdate = timeNow;
    }
    // Update one buffer if time moves in reverse
    else if (inDate == _nextNextDate && !_isDataLoading && reverseTime) {
        _currentDate = _nextDate;
        _nextDate = _currentDate.getYesterday();
        _nextNextDate = _nextDate.getYesterday();

        _isDataLoading = true;

        // Fetch day after next day async to avoid lag
        _future = std::async(
            std::launch::async,
            &RenderableAirTrafficHistorical::updateBuffersReverse,
            this,
            _nextNextDate,
            true
        );
        _lastUpdate = timeNow;
    }
    // Update all three buffers
    // This is performed if the date changes 
    // with more than one day in one step
    else if ((inDate != _currentDate && !_isDataLoading && !reverseTime) ||
             (inDate != _nextDate && !_isDataLoading && reverseTime))
    {
        _bufferA.date = Date();
        _bufferB.date = Date();
        _bufferC.date = Date();

        if (!reverseTime) {
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
    if (_future.valid() &&
        _future.wait_for(std::chrono::seconds(0)) == std::future_status::ready)
    {
        _future.get();
        // As the asynchronous stage is finished, send to glBindBuffer on the main thread
        // and context
        sendToGLBuffer(
            _bufferA.date == _nextNextDate ?
                _bufferA :
                _bufferB.date == _nextNextDate ?
                    _bufferB :
                    _bufferC
        );
        _isDataLoading = false;
    }

    if (_bufferA.vertexBufferData.empty() && _bufferB.vertexBufferData.empty() &&
        _bufferC.vertexBufferData.empty())
    {
        return;
    }

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
    _shader->setUniform(
        _uniformCache.latitudeThreshold,
        RenderableAirTrafficBound::getLatBound()
    );
    _shader->setUniform(
        _uniformCache.longitudeThreshold,
        RenderableAirTrafficBound::getLonBound()
    );
    // Time is sent to the shaders as a UNIX timestamp for easy interpolation
    _shader->setUniform(
        _uniformCache.time,
        static_cast<int>(data.time.j2000Seconds() + 365.25 * 24 * 60 * 60 * 30)
    );
    _shader->setUniform(
        _uniformCache.cameraPosition,
        glm::vec3(data.camera.positionVec3())
    );
    _shader->setUniform(_uniformCache.modelTransform, glm::mat4(modelTransform));

    // Check if Earth is enabled and if clipping should be enabled or not
    const Renderable* earth = renderable("Earth");
    if (earth != nullptr) {
        _shader->setUniform(_uniformCache.clipping, earth->isEnabled());
    }
    else {
        _shader->setUniform(_uniformCache.clipping, true);
    }

    glLineWidth(1.f);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_ALWAYS);
        
    // Draw the two buffers with dates that are either current or next
    if (_bufferA.date == _currentDate || _bufferA.date == _nextDate) {
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
    _ptr = reinterpret_cast<int*>(
        glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_READ_ONLY)
    );
    _nRenderedFlights = *_ptr;

    glDepthFunc(GL_LESS);

    _shader->deactivate();
};

// Updating buffers and handles both async and sync cases
void RenderableAirTrafficHistorical::updateBuffersReverse(const Date& date, bool async) {
    // Load data for the specific date
    fetchData(date);

    // Fill the un-used buffer
    if (_bufferA.date != _nextDate && _bufferA.date != _nextNextDate) {
        // Fill A
        fillBuffer(_bufferA);
        if (!async) {
            sendToGLBuffer(_bufferA);
        }
        _bufferA.date = date;
    }
    else if (_bufferB.date != _nextDate && _bufferB.date != _nextNextDate) {
        // Fill B
        fillBuffer(_bufferB);
        if (!async) {
            sendToGLBuffer(_bufferB);
        }
        _bufferB.date = date;
    }
    else {
        // Fill C
        fillBuffer(_bufferC);
        if (!async) {
            sendToGLBuffer(_bufferC);
        }
        _bufferC.date = date;
    }

    // Update the number of flights according to what date it is
    _nRenderedFlights =
        _bufferA.date == _nextDate ?
            _bufferA.vertexBufferData.size() / 2 :
            _bufferB.date == _nextDate ?
                _bufferB.vertexBufferData.size() / 2 :
                _bufferC.vertexBufferData.size() / 2;
}

// Updating buffers and handles both async and sync cases
void RenderableAirTrafficHistorical::updateBuffers(const Date& date, bool async) {
    // Load data for the specific date
    fetchData(date);

    // Fill the buffer
    if (_bufferA.date != _currentDate && _bufferA.date != _nextDate) {
        // Fill A
        fillBuffer(_bufferA); 
        if (!async) {
            sendToGLBuffer(_bufferA);
        }
        _bufferA.date = date;
    }
    else if (_bufferB.date != _currentDate && _bufferB.date != _nextDate) {
        // Fill B
        fillBuffer(_bufferB); 
        if (!async) {
            sendToGLBuffer(_bufferB);
        }
        _bufferB.date = date;
    } 
    else {
        // Fill C
        fillBuffer(_bufferC); 
        if (!async) {
            sendToGLBuffer(_bufferC);
        }
        _bufferC.date = date;
    }

    // Update the number of flights according to what date it is
    _nRenderedFlights =
        _bufferA.date == _currentDate ?
            _bufferA.vertexBufferData.size() / 2 : 
            _bufferB.date == _currentDate ?
                _bufferB.vertexBufferData.size() / 2 :
                _bufferC.vertexBufferData.size() / 2;
}

// This function will fill the backend storage with data
void RenderableAirTrafficHistorical::fillBuffer(Buffer& buffer) {
    int bufIdx = 0;

    buffer.vertexBufferData.clear();
    buffer.vertexBufferData.resize(2 * _data.size());

    AircraftVBOLayout startVBO;
    AircraftVBOLayout endVBO;

    std::tm timeFirst = {};
    std::tm timeLast = {};

    for (const std::vector<std::string>& dataLine : _data) {
        std::istringstream ssFirst(dataLine[0]);
        ssFirst >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
        std::time_t first = _mkgmtime(&timeFirst);

        std::istringstream ssLast(dataLine[1]);
        ssLast >> std::get_time(&timeFirst, "%Y-%m-%d %H:%M:%S");
        std::time_t last = _mkgmtime(&timeFirst);

        if (ssFirst.fail() || ssLast.fail()) {
            throw std::runtime_error("Failed to parse time string");
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

// glBindBuffer is a critical section and needs to be performed on the same thread and
// context which is why this code is not executed directly in updateBuffers()
void RenderableAirTrafficHistorical::sendToGLBuffer(Buffer& buffer) {
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
        reinterpret_cast<GLvoid*>(2 * sizeof(float))
    );

    glBindVertexArray(0);
}

// Fetches a new day of flight data from csv file in 
// directory structure
bool RenderableAirTrafficHistorical::fetchData(const Date& date) {
    std::string strMonth = std::to_string(date.month);
    std::string strDay = std::to_string(date.day);

    // Two digit month and day 
    if (date.month < 10) {
        strMonth = "0" + std::to_string(date.month);
    }
    if (date.day < 10) {
        strDay = "0" + std::to_string(date.day);
    }

    std::string fileName = fmt::format("{}/{}/{}.csv", date.year, strMonth, strDay);
    LINFOC("RenderableAirTrafficHistorical", fmt::format("Fetching data: {}", fileName));

    _data = ghoul::loadCSVFile(absPath(_PATH + fileName).string(), {
        "firstseen",
        "lastseen",
        "latitude_1",
        "longitude_1",
        "latitude_2",
        "longitude_2"
    }, false);

    return !_data.empty();
}

} // namespace openspace
