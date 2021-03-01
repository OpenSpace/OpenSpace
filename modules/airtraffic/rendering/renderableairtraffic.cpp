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


#include <modules/airtraffic/rendering/renderableairtraffic.h>

#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/httprequest.h>
#include <iostream>
#include <future>    // std::async, std::future
#include <chrono> // time stuff


using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    
    constexpr const std::array<const char*, 2> UniformNames = {
        "modelViewProjection", "trailSize"
    };

    constexpr openspace::properties::Property::PropertyInfo URLPathInfo = {
       "URL",
       "URL Path",
       "The URL used for aircraft data."
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
       "PointSize",
       "Point Size",
       "The size of the points used to represent aircrafts."
    };
} // namespace


namespace openspace {

documentation::Documentation RenderableAirTraffic::Documentation() {
    using namespace documentation;
    return {
        "Renderable Air Traffic",
        "renderableairtraffic",
        {
            {
                URLPathInfo.identifier,
                new StringVerifier,
                Optional::No,
                URLPathInfo.description
            },
            {
                PointSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                PointSizeInfo.description
            },
        }
    };
}

RenderableAirTraffic::RenderableAirTraffic(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _pointSize(PointSizeInfo, 2.f, 1.f, 10.f)
    {}

    void RenderableAirTraffic::initialize() {
        return;
    };
    void RenderableAirTraffic::deinitialize() {
        return;
    };

    void RenderableAirTraffic::initializeGL() {
        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

  
        // Setup shaders
        _shader = global::renderEngine->buildRenderProgram(
            "AirTrafficProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffic_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffic_fs.glsl")
            //absPath("${MODULE_AIRTRAFFIC}/shaders/airtraffic_ge.glsl")
        );
        
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
        
        // First data fetch 
        _data = fetchData();
        updateBuffers(true);
    };

    void RenderableAirTraffic::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBuffer);
        glDeleteVertexArrays(1, &_vertexArray);
        
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
        
        return;
    };

    bool RenderableAirTraffic::isReady() const {
        return true;
    };

    void RenderableAirTraffic::render(const RenderData& data, RendererTasks& rendererTask) {

        if (_data.empty()) return;

        // Trigger data update
        if ( abs(data.time.j2000Seconds() - _deltaTime) > 10.0 && !_dataLoading) {
            fut = std::async(std::launch::async, &RenderableAirTraffic::fetchData, this);
            _dataLoading = true;
        }

        // Check if new data finished loading. Update buffers ONLY if finished
        if (fut.valid() && fut.wait_for(seconds(0)) == std::future_status::ready) { // NOT SO STABLE SOLUTION
            _data = fut.get();
            updateBuffers();
            std::cout << " BUFFERS UPDATED " << abs(_deltaTime - data.time.j2000Seconds()) << std::endl;
            _dataLoading = false;
            _deltaTime = data.time.j2000Seconds();
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

        
        const GLsizei nAircrafts = static_cast<GLsizei>(_data["states"].size());
        _shader->setUniform(
            _uniformCache.trailSize,
            static_cast<float>(_TRAILSIZE)
        );

        glBindVertexArray(_vertexArray);

        //glPointSize(2.0);
        glLineWidth(2.0);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);



        //glDrawArrays(GL_LINE_STRIP, 0, _TRAILSIZE * nAircrafts);


        for (size_t i = 0; i < nAircrafts; ++i) {
            glDrawArrays(GL_LINE_STRIP, _TRAILSIZE * i, static_cast<GLsizei>(_TRAILSIZE)/*+ 1*/);   
        }
    
        //glVertexPointer(_TRAILSIZE, GL_FLOAT, 0, _vertexBufferData.data());
        //glDrawArrays(GL_LINE_STRIP, 0, nAircrafts);

        glBindVertexArray(0);

        _shader->deactivate();
    };

    void RenderableAirTraffic::update(const UpdateData& data) {
        return;
    };

    json RenderableAirTraffic::parseData(SyncHttpMemoryDownload& response) {

        // Callback to handle NULL data
        json::parser_callback_t ReplaceNullCallBack = [](int depth, json::parse_event_t event, json& parsed) {
            if (event == json::parse_event_t::value and parsed == nullptr) parsed = 0;
            return true;
        };

        //_data = json::parse(response.downloadedData().begin(), response.downloadedData().end(), ReplaceNullCallBack);
        return json::parse(response.downloadedData().begin(), response.downloadedData().end(), ReplaceNullCallBack);

        // JSON structure:
        // time: int,
        // states: [    
        //      (0: icao24)             string,
        //      (1: callsign)           string, can be null
        //      (2: origin_country)     string,
        //      (3: time_position)      int,    can be null
        //      (4: last_contact)       int,
        //      (5: longitude)          float,  can be null
        //      (6: latitude)           float,  can be null
        //      (7: baro_altitude)      float,  can be null
        //      (8: on_ground)          bool,
        //      (9: velocity)           float,  can be null
        //      (10: true_track)        float,  can be null
        //      (11: vertical_rate)     float,  can be null
        //      (12:sensors)            int[],  can be null 
        //      (13: geo_altitude)      float,  can be null
        //      (14: squawk)            string, can be null
        //      (15: spi)               bool,
        //      (16: position_source)   int,
        // ],
        // .
        // .
        // .
        // Example, get latitude for flight#: jsonData["states"][flight#][6]
    }

    json RenderableAirTraffic::fetchData() {

        /*const double secondsInAYear = 365.25 * 24 * 60 * 60;
        const double secondsBetween1970And2000 = 30 * secondsInAYear;
        std::cout << "SecondsBetween1970And2000: " << secondsBetween1970And2000 << std::endl;
        std::cout << "_deltaTime: " << _deltaTime << std::endl;
        
        int t = static_cast<int>(_deltaTime + secondsBetween1970And2000);
        std::cout << "_deltaTime + secondsBetween1970And2000: " << t << std::endl;
        std::string time = std::to_string(t);*/

        SyncHttpMemoryDownload response(_url);
        HttpRequest::RequestOptions timeout;
        timeout.requestTimeoutSeconds = 0; // No timeout limit
        response.download(timeout);
        
        return parseData(response);

        //return response.hasSucceeded();

    }

    void RenderableAirTraffic::updateBuffers(bool _firstFetch) {

        int nAircrafts = static_cast<int>(_data["states"].size());
        
        _vertexBufferData.clear();
        _vertexBufferData.resize(_TRAILSIZE * nAircrafts);

        size_t vertexBufIdx = 0;

        AircraftVBOLayout temp;

        for (auto& aircraft : _data["states"]) {
            // Extract data and add to vertex buffer
            //---
            std::string icao24 = aircraft[0];

            temp.lastContact = static_cast<int>(aircraft[4]);
            temp.latitude = static_cast<float>(aircraft[6]);  
            temp.longitude = static_cast<float>(aircraft[5]);
            temp.barometricAltitude = static_cast<float>(aircraft[7]);
            temp.velocity = static_cast<float>(aircraft[9]);
            temp.flightDirection = static_cast<float>(aircraft[10]);
            
            if(temp.latitude != 0.f && temp.longitude != 0.f) {
                if(aircraftMap[icao24].list.size() >= _TRAILSIZE) aircraftMap[icao24].list.pop_back();
                aircraftMap[icao24].list.push_front(temp);
            }

            for (auto& ac : aircraftMap[icao24].list) {
                _vertexBufferData[vertexBufIdx] = ac;
                vertexBufIdx++;
            }  
        }

        glBindVertexArray(_vertexArray);

        glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);

        //if (_firstFetch) {
            glBufferData(
                GL_ARRAY_BUFFER,
                _vertexBufferData.size() * sizeof(AircraftVBOLayout),
                _vertexBufferData.data(),
                GL_STATIC_DRAW
            );
       /* }
        else {  
            glBufferSubData(
                GL_ARRAY_BUFFER,
                0,
                _vertexBufferData.size() * sizeof(AircraftVBOLayout),
                _vertexBufferData.data()
            );
        }*/

        // Lat, long, alt: at pos 0 send 3 float from AircraftVBOLayout
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0, 
            3, 
            GL_FLOAT, 
            GL_FALSE, 
            sizeof(AircraftVBOLayout),  
            nullptr
        );

        // Velocity & flight direction
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(
            1,
            2,
            GL_FLOAT,
            GL_FALSE,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(3 * sizeof(GL_FLOAT)) // start at pos 3
        );

        // Last contact
        glEnableVertexAttribArray(2);
        glVertexAttribPointer(
            2,
            1,
            GL_INT,
            GL_FALSE,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(5 * sizeof(GL_FLOAT))
        );

        glBindVertexArray(0);
    }
} // namespace openspace

