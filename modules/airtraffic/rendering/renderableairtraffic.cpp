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
#include <openspace/rendering/renderengine.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>


namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

RenderableAirTraffic::RenderableAirTraffic(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {}

    void RenderableAirTraffic::initialize() {
        fetchData();
        return;
    };
    void RenderableAirTraffic::deinitialize() {
        return;
    };

    void RenderableAirTraffic::initializeGL() {
        return;
    };
    void RenderableAirTraffic::deinitializeGL() {
        return;
    };

    bool RenderableAirTraffic::isReady() const {
        return true;
    };

    void RenderableAirTraffic::render(const RenderData& data, RendererTasks& rendererTask) {
        return;
    };
    void RenderableAirTraffic::update(const UpdateData& data) {
        return;
    };

    // Use if we need to go by file 
    void RenderableAirTraffic::readDataFile(const std::string& filename){
        if(!FileSys.fileExists(filename)){
            throw ghoul::RuntimeError(fmt::format(
                "Air traffic data file does not exist.", filename
            ));
        }
    }

    bool RenderableAirTraffic::fetchData() {

        SyncHttpMemoryDownload response(_url);
        HttpRequest::RequestOptions timeout;
        timeout.requestTimeoutSeconds = 0; // No timeout limit
        response.download(timeout);

        _data = json::parse(response.downloadedData().begin(), response.downloadedData().end());

        // JSON structure:
        // time: int,
        // states: [    
        //      (0: icao24)             string,
        //      (1: callsign)           string,
        //      (2: origin_country)     string,
        //      (3: time_position)      int,
        //      (4: last_contact)       int,
        //      (5: longitude)          float,
        //      (6: latitude)           float,
        //      (7: baro_altitude)      float,
        //      (8: on_ground)          bool,
        //      (9: velocity)           float,
        //      (10: true_track)        float,
        //      (11: vertical_rate)     float,
        //      (12:sensors)            int[],
        //      (13: geo_altitude)      float,
        //      (14: squawk)            string,
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

        size_t nVerticesTotal = 0;

        int nAircrafts = static_cast<int>(_data["states"].size());
        
        _vertexBufferData.resize(nAircrafts);

        size_t vertexBufIdx = 0;


        for (auto& aircraft : _data["states"]){
            float latitude = 0.f;
            float longitude = 0.f;
            float barometricAltitude = 0.f;
            //float geometricAltitude = 0.f; // Not used at the moment
            float velocity = 0.f;
            int lastContact = 0; // Draw only if changed
            float flightDirection = 0.f; // true_track in data
            float time = 0.f; // Maybe not needed

            // Extract data for shader
            _vertexBufferData[vertexBufIdx].lastContact = _data["states"][vertexBufIdx][4];
            _vertexBufferData[vertexBufIdx].longitude = _data["states"][vertexBufIdx][5];
            _vertexBufferData[vertexBufIdx].latitude = _data["states"][vertexBufIdx][6];
            _vertexBufferData[vertexBufIdx].barometricAltitude = _data["states"][vertexBufIdx][7];
            _vertexBufferData[vertexBufIdx].velocity = _data["states"][vertexBufIdx][9];
            _vertexBufferData[vertexBufIdx].flightDirection = _data["states"][vertexBufIdx][10];
            _vertexBufferData[vertexBufIdx].time = _data["time"];

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

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(AircraftVBOLayout),  nullptr);

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(
            1,
            2,
            GL_DOUBLE,
            GL_FALSE,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(4 * sizeof(GL_FLOAT))
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

