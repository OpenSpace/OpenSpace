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



using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    
    constexpr const std::array<const char*, 7> UniformNames = {
        "modelViewProjection", 
        "maximumColor",
        "minimumColor",
        "opacity", 
        "latitudeThreshold", 
        "longitudeThreshold",
        "totalFlights"
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

    constexpr openspace::properties::Property::PropertyInfo LatitudeThresholdInfo = {
       "latitudeThreshold",
       "Latitude Threshold",
       "Minimum and maximum latitude for aircrafts."
    };

    constexpr openspace::properties::Property::PropertyInfo LongitudeThresholdInfo = {
       "longitudeThreshold",
       "Longitude Threshold",
       "Minimum and maximum longitude for aircrafts."
    };

    constexpr openspace::properties::Property::PropertyInfo TotalFlightsInfo = {
       "TotalFlights",
       "Total Flights",
       "The total number of flights displayed over the duration of the chosen date. Filtration does not affect this value."
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
    , _opacity(OpacityInfo, 0.05f, 0.f, 1.f)
    , _latitudeThreshold(LatitudeThresholdInfo, glm::vec2(-90.f, 90.f), glm::vec2(-90.f), glm::vec2(90.f))
    , _longitudeThreshold(LongitudeThresholdInfo, glm::vec2(-180.f, 180.f), glm::vec2(-180.f), glm::vec2(180.f))
    , _nTotalFlights(TotalFlightsInfo, 0, 0, 150000)
    {
        addProperty(_maximumColor);
        addProperty(_minimumColor);
        addProperty(_opacity);
        addProperty(_latitudeThreshold);
        addProperty(_longitudeThreshold);

        _nTotalFlights.setReadOnly(true);
        addProperty(_nTotalFlights);

        setRenderBin(RenderBin::PostDeferredTransparent);
    };

    void RenderableAirTrafficHistorical::initialize() {
        return;
    };

    void RenderableAirTrafficHistorical::deinitialize() {
        return;
    };

    void RenderableAirTrafficHistorical::initializeGL() {
        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

        _shader = global::renderEngine->buildRenderProgram(
            "DensityMapProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffichistorical_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    };

    void RenderableAirTrafficHistorical::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBuffer);
        glDeleteVertexArrays(1, &_vertexArray);

        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;

        return;
    };

    bool RenderableAirTrafficHistorical::isReady() const {
        return true;
    };

    void RenderableAirTrafficHistorical::render(const RenderData& data, RendererTasks& rendererTask) {


        // YYYY-MM-DD
        std::string_view date = data.time.ISO8601().substr(0, 10);
        
        //std::cout << "Current date: " << _currentDate << "        date: " << date << std::endl; 
        if (_currentDate != date && !_isDataLoading) {
            std::cout << "Async..." << std::endl; 
            _currentDate = date;
            _future = std::async(std::launch::async, &RenderableAirTrafficHistorical::fetchData, this);
            _isDataLoading = true;
        }

        // Check if new data finished loading. Update buffers ONLY if finished
        if (_future.valid() && _future.wait_for(seconds(0)) == std::future_status::ready) { // NOT SO STABLE SOLUTION
            if(_future.get()) {
                updateBuffers();
                std::cout << "New data loaded" << std::endl;
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
        _shader->setUniform(_uniformCache.latitudeThreshold, _latitudeThreshold);
        _shader->setUniform(_uniformCache.longitudeThreshold, _longitudeThreshold);
        _shader->setUniform(_uniformCache.totalFlights, _nTotalFlights);

        glBindVertexArray(_vertexArray);
        glLineWidth(1.f);
        
        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_vertexBufferData.size()));

        glBindVertexArray(0);

        _shader->deactivate();
    };

    void RenderableAirTrafficHistorical::update(const UpdateData& data) {
        return;
    };

    bool RenderableAirTrafficHistorical::updateBuffers() {

        _vertexBufferData.clear();

        _nTotalFlights = _data.size();
        _vertexBufferData.resize(2 * _nTotalFlights + 2);

        AircraftVBOLayout bBoxVBO;
        bBoxVBO.identifier = 1;
        _vertexBufferData.push_back(bBoxVBO);
        _vertexBufferData.push_back(bBoxVBO);

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

            if (ssFirst.fail() || ssLast.fail()) throw std::runtime_error{ "Failed to parse time string." };

            startVBO.latitude = dataLine[2] == "" ? _THRESHOLD : std::stof(dataLine[2]);
            startVBO.longitude = dataLine[3] == "" ? _THRESHOLD : std::stof(dataLine[3]);
            startVBO.firstSeen = first;
            startVBO.lastSeen = last;
            _vertexBufferData.push_back(startVBO);//[vertexBufIdx] = startVBO;

            endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
            endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
            endVBO.firstSeen = first;
            endVBO.lastSeen = last;
            _vertexBufferData.push_back(endVBO);//[vertexBufIdx + 1] = endVBO;
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

        // interval . 
        glEnableVertexAttribArray(2);
        glVertexAttribIPointer(
            2,
            1,
            GL_INT,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(2 * sizeof(GL_FLOAT) + 2 * sizeof(GL_INT))
        );

        glBindVertexArray(0);
        
        return true;
    }

    bool RenderableAirTrafficHistorical::fetchData(){

        std::string fileName = _currentDate.substr(0, 4) + "/" 
        + _currentDate.substr(5, 2) + "/" + _currentDate.substr(8, 2) + ".csv";

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

