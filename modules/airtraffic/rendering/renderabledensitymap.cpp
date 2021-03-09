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


#include <modules/airtraffic/rendering/RenderableDensityMap.h>

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
    
    constexpr const std::array<const char*, 1> UniformNames = {
        "modelViewProjection"
    };
} // namespace


namespace openspace {

documentation::Documentation RenderableDensityMap::Documentation() {
    using namespace documentation;
    return {
        "Renderable Density Map",
        "renderabledensitymap",
        {
        }
    };
}

RenderableDensityMap::RenderableDensityMap(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    {
        setRenderBin(RenderBin::PostDeferredTransparent);
    };

    void RenderableDensityMap::initialize() {
        return;
    };

    void RenderableDensityMap::deinitialize() {
        return;
    };

    void RenderableDensityMap::initializeGL() {
        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

        _shader = global::renderEngine->buildRenderProgram(
            "DensityMapProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/densitymap_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/densitymap_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/densitymap_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

        updateBuffers();
    };

    void RenderableDensityMap::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBuffer);
        glDeleteVertexArrays(1, &_vertexArray);

        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;

        return;
    };

    bool RenderableDensityMap::isReady() const {
        return true;
    };

    void RenderableDensityMap::render(const RenderData& data, RendererTasks& rendererTask) {
        
        if (_data.empty()) return;

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

        glBindVertexArray(_vertexArray);
        glLineWidth(3.f);
        

        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_vertexBufferData.size()));

        glBindVertexArray(0);

        _shader->deactivate();
    };

    void RenderableDensityMap::update(const UpdateData& data) {
        return;
    };

    bool RenderableDensityMap::updateBuffers() {

        bool success = fetchData();

        if (success) {
            std::cout << "Successful data loading" << std::endl; 

            _vertexBufferData.clear();
            _vertexBufferData.resize(2 * _data.size());

            size_t vertexBufIdx = 0;

            AircraftVBOLayout startVBO;
            AircraftVBOLayout endVBO;

            std::tm timeFirst = {};
            std::tm timeLast = {};

            // 2020-12-31 06:22:20+00:00
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
                _vertexBufferData[vertexBufIdx] = startVBO;

                endVBO.latitude = dataLine[4] == "" ? _THRESHOLD : std::stof(dataLine[4]);
                endVBO.longitude = dataLine[5] == "" ? _THRESHOLD : std::stof(dataLine[5]);
                endVBO.firstSeen = first;
                endVBO.lastSeen = last;
                _vertexBufferData[vertexBufIdx + 1] = endVBO;

                vertexBufIdx += 2;
            }
        }
        else { 
            std::cout << "ERROR loading data" << std::endl;
            return success;
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
        glVertexAttribPointer(
            1,
            2,
            GL_FLOAT,
            GL_FALSE,
            sizeof(AircraftVBOLayout),
            reinterpret_cast<GLvoid*>(2 * sizeof(GL_FLOAT))
        );

        glBindVertexArray(0);
        
        return success;
    }

    bool RenderableDensityMap::fetchData(){

        _data.clear();

        std::string fileName = "flightlist_20190101_20190131.csv";
        _data = ghoul::loadCSVFile(absPath(_PATH + fileName), {
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

