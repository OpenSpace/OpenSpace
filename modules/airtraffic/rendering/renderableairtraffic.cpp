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


namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    constexpr const std::array<const char*, 1> UniformNames = {
        "modelViewProjection"//, "timeStamp"
    };
} // namespace


namespace openspace {

namespace documentation { struct Documentation; }

RenderableAirTraffic::RenderableAirTraffic(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    {}

    void RenderableAirTraffic::initialize() {
        //fetchData();
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

        updateBuffers();
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

        
        //_shader->setUniform(_uniformCache.timeStamp, static_cast<int>(_data["time"]));

        glBindVertexArray(_vertexArray);

        glPointSize(2.0);
        
        const GLsizei nAircrafts = static_cast<GLsizei>(_data["states"].size());
        glDrawArrays(GL_POINTS, 0, nAircrafts);

        glBindVertexArray(0);

        _shader->deactivate();

    };
    void RenderableAirTraffic::update(const UpdateData& data) {
        return;
    };

    bool RenderableAirTraffic::fetchData() {

        SyncHttpMemoryDownload response(_url);
        HttpRequest::RequestOptions timeout;
        timeout.requestTimeoutSeconds = 0; // No timeout limit
        response.download(timeout);

        // Replace null data with 0
        json::parser_callback_t ReplaceNullCallBack = [](int depth, json::parse_event_t event, json& parsed) {
            if (event == json::parse_event_t::value and parsed == nullptr) parsed = 0;
            return true;
        };

        _data = json::parse(response.downloadedData().begin(), response.downloadedData().end(), ReplaceNullCallBack);
        
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

        return response.hasSucceeded();
    }

    void RenderableAirTraffic::updateBuffers() {

        fetchData(); 

        int nAircrafts = static_cast<int>(_data["states"].size());
        
        _vertexBufferData.resize(nAircrafts);

        size_t vertexBufIdx = 0;

        for (auto& aircraft : _data["states"]){
            // Extract data and add to vertex buffer
            _vertexBufferData[vertexBufIdx].lastContact = static_cast<int>(aircraft[4]);
            _vertexBufferData[vertexBufIdx].longitude = static_cast<float>(aircraft[5]);
            _vertexBufferData[vertexBufIdx].latitude = static_cast<float>(aircraft[6]);
            _vertexBufferData[vertexBufIdx].barometricAltitude = static_cast<float>(aircraft[7]);
            _vertexBufferData[vertexBufIdx].velocity = static_cast<float>(aircraft[9]);
            _vertexBufferData[vertexBufIdx].flightDirection = static_cast<float>(aircraft[10]);

            vertexBufIdx++;
        }

        glBindVertexArray(_vertexArray);

        glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexBufferData.size() * sizeof(AircraftVBOLayout),
            _vertexBufferData.data(),
            GL_STATIC_DRAW
        );


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

    documentation::Documentation RenderableAirTraffic::Documentation() {
        using namespace documentation;
        return {
            "RenderableAirTraffic",
            "renderableairtraffic",
            {
            DocumentationEntry("placeholder",
                new StringInListVerifier({
                    "placeholder", "placeholder", "placeholder"
                }),
                Optional::Yes)
            }
        };
    }
} // namespace openspace

