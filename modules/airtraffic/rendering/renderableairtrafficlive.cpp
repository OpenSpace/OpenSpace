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


#include <modules/airtraffic/rendering/renderableairtrafficlive.h>
#include <modules/airtraffic/rendering/renderableairtrafficbound.h>

#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/httprequest.h>
#include <iostream>
#include <future>
#include <chrono>



using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {
    
    constexpr const std::array<const char*, 8> UniformNames = {
        "modelViewProjection", 
        "trailSize", 
        "resolution", 
        "lineWidth", 
        "color",
        "opacity",
        "latitudeThreshold",
        "longitudeThreshold"
    };

    constexpr openspace::properties::Property::PropertyInfo URLPathInfo = {
       "URL",
       "URL Path",
       "The URL used for aircraft data."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
       "LineWidth",
       "Line Width",
       "The width of the lines used to represent aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
       "Color",
       "Color",
       "The color used to represent aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
       "Opacity",
       "Opacity",
       "The opacity of the lines used to represent aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderedAircraftsInfo = {
       "RenderedAircrafts",
       "Live Aircraft",
       "The number of live aircraft in traffic right now."
    };
} // namespace


namespace openspace {

documentation::Documentation RenderableAirTrafficLive::Documentation() {
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
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                ColorInfo.identifier,
                new Vector3Verifier<double>,
                Optional::Yes,
                ColorInfo.description
            },
            {
                OpacityInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                OpacityInfo.description
            },
        }
    };
}

RenderableAirTrafficLive::RenderableAirTrafficLive(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth(LineWidthInfo, 14.14f, 1.f, 30.f) // default, min, max 
    , _color(ColorInfo, glm::vec3(1.f, 0.f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _nRenderedAircraft(RenderedAircraftsInfo, 0, 0, 10000)
    {
        addProperty(_lineWidth);
        addProperty(_color);
        addProperty(_opacity);
        _nRenderedAircraft.setReadOnly(true);
        addProperty(_nRenderedAircraft);
 
        setRenderBin(RenderBin::PostDeferredTransparent);
    }

    void RenderableAirTrafficLive::initializeGL() {
        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

        // Setup shaders
        _shader = global::renderEngine->buildRenderProgram(
            "AirTrafficLiveProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficlive_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficlive_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficlive_ge.glsl")
        );
        
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
        
        // First data fetch 
        _data = fetchData();
        updateBuffers();
    };

    void RenderableAirTrafficLive::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBuffer);
        glDeleteVertexArrays(1, &_vertexArray);
        
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
        
        return;
    };

    bool RenderableAirTrafficLive::isReady() const {
        return _shader != nullptr;
    };

    void RenderableAirTrafficLive::render(const RenderData& data, RendererTasks& rendererTask) {

        // Return if data is empty or time is from more than 3 minutes ago
        if (_data.empty() || abs(Time::now().j2000Seconds() - data.time.j2000Seconds()) > 60 * 3) { 
            _nRenderedAircraft = 0;
            return;
        }

        // Trigger data update
        if (abs(data.time.j2000Seconds() - _deltaTime) > 10.0 && !_isDataLoading) {
            std::cout << "Data loading initialized... ";
            _future = std::async(std::launch::async, &RenderableAirTrafficLive::fetchData, this);
            _isDataLoading = true;
        }

        // Check if new data finished loading. Update buffers ONLY if finished
        if (_future.valid() && _future.wait_for(seconds(0)) == std::future_status::ready) { 
            _data = _future.get();
            updateBuffers();
            std::cout << "finished. Time since last update: " << abs(_deltaTime - data.time.j2000Seconds()) << " seconds." << std::endl;
            _isDataLoading = false;
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

        _shader->setUniform(_uniformCache.color, _color);
        _shader->setUniform(_uniformCache.opacity, _opacity);
        _shader->setUniform(_uniformCache.latitudeThreshold, RenderableAirTrafficBound::getLatBound());
        _shader->setUniform(_uniformCache.longitudeThreshold, RenderableAirTrafficBound::getLonBound());

  
        const GLsizei nAircrafts = static_cast<GLsizei>(_data["states"].size());
        _shader->setUniform(
            _uniformCache.trailSize,
            static_cast<float>(_TRAILSIZE)
        );

// Fix else statement
//#if !defined(__APPLE__)
        glm::ivec2 resolution = global::renderEngine->renderingResolution();
        _shader->setUniform(_uniformCache.resolution, resolution);
        _shader->setUniform(_uniformCache.lineWidth, _lineWidth);
//#endif

        glBindVertexArray(_vertexArray);
        glLineWidth(_lineWidth);


        for (size_t i = 0; i < nAircrafts; ++i) {
            glDrawArrays(GL_LINE_STRIP, _TRAILSIZE * i, static_cast<GLsizei>(_TRAILSIZE));   
        }

        glBindVertexArray(0);

        _shader->deactivate();
    };

    json RenderableAirTrafficLive::parseData(SyncHttpMemoryDownload& response) {

        // Callback to handle NULL data
        json::parser_callback_t ReplaceNullCallBack = [this](int depth, json::parse_event_t event, json& parsed) {
            if (event == json::parse_event_t::value and parsed == nullptr) parsed = _THRESHOLD;
            return true;
        };

        //_data = json::parse(response.downloadedData().begin(), response.downloadedData().end(), ReplaceNullCallBack);
        return json::parse(response.downloadedData().begin(), response.downloadedData().end(), ReplaceNullCallBack);

        // JSON structure:
        // time: int,
        // states: 
        // [[    
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
        //      (12: sensors)           int[],  can be null 
        //      (13: geo_altitude)      float,  can be null
        //      (14: squawk)            string, can be null
        //      (15: spi)               bool,
        //      (16: position_source)   int,
        //  ],
        //  [
        //      .
        //      .
        //      .
        //  ],
        //  .
        //  .
        //  .
        // ]
        // Example, get latitude for flight#: jsonData["states"][flight#][6]
    }

    json RenderableAirTrafficLive::fetchData() {

        // Start timer
        auto start = std::chrono::steady_clock::now();
    
        SyncHttpMemoryDownload response(_url);
        HttpRequest::RequestOptions timeout;
        timeout.requestTimeoutSeconds = 0; // No timeout limit
        
        response.download(timeout);
        
        auto end = std::chrono::steady_clock::now();
        // End timer
        auto t = duration_cast<milliseconds>(end - start);
        std::cout << "fetchData: " << t.count() << " milliseconds." << std::endl;

        if(response.hasSucceeded())
            return parseData(response);

        // If new data is inaccessible, return current data
        return _data;
    }

    void RenderableAirTrafficLive::updateBuffers() {

        _nRenderedAircraft = _data["states"].size();
        
        _vertexBufferData.clear();
        _vertexBufferData.resize(_TRAILSIZE * _nRenderedAircraft);

        size_t vertexBufIdx = 0;

        AircraftVBOLayout temp;

        for (auto& aircraft : _data["states"]) {
            // Extract data and add to vertex buffer
            // ---
            std::string icao24 = aircraft[0];

            temp.latitude = static_cast<float>(aircraft[6]);  
            temp.longitude = static_cast<float>(aircraft[5]);
            temp.barometricAltitude = static_cast<float>(aircraft[7]);
            
            if(temp.latitude > _THRESHOLD && temp.longitude > _THRESHOLD) {
                if(_aircraftMap[icao24].list.size() >= _TRAILSIZE) _aircraftMap[icao24].list.pop_back();
                _aircraftMap[icao24].list.push_front(temp);
            }

            for (auto& ac : _aircraftMap[icao24].list) {
                _vertexBufferData[vertexBufIdx] = ac;
                vertexBufIdx++;
            }  
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

        glBindVertexArray(0);
    }
} // namespace openspace

