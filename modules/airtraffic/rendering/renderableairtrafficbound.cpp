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

using namespace std::chrono;

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace {

    constexpr const std::array<const char*, 8> UniformNames = {
        "modelViewProjection",
        "color",
        "opacity",
        "latitudeThreshold",
        "longitudeThreshold",
        "cameraPosition",
        "modelTransform",
        "clipping"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
       "Color",
       "Color",
       "The color used to represent the lines"
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
       "Opacity",
       "Opacity",
       "The opacity of the lines used to represent the bounding box."
    };

    constexpr openspace::properties::Property::PropertyInfo LatitudeThresholdInfo = {
       "latitudeThreshold",
       "Latitude Threshold",
       "Minimum and maximum latitude for all aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo LongitudeThresholdInfo = {
       "longitudeThreshold",
       "Longitude Threshold",
       "Minimum and maximum longitude for all aircraft."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
       "lineWidth",
       "Line Width",
       "Controls the line width of the bounding box."
    };

} // namespace

namespace openspace {

    documentation::Documentation RenderableAirTrafficBound::Documentation() {
        using namespace documentation;
        return {
            "Renderable Air Traffic Bound",
            "RenderableAirTrafficBound",
            {
            }
        };
    }

    RenderableAirTrafficBound::RenderableAirTrafficBound(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec3(1.f, 0.f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 5.f)
    , _latitudeThreshold(LatitudeThresholdInfo, glm::vec2(-90.f, 90.f), glm::vec2(-90.f), glm::vec2(90.f))
    , _longitudeThreshold(LongitudeThresholdInfo, glm::vec2(-180.f, 180.f), glm::vec2(-180.f), glm::vec2(180.f))
    {
        addProperty(_color);
        addProperty(_opacity);
        addProperty(_lineWidth);
        addProperty(_latitudeThreshold);
        addProperty(_longitudeThreshold);


        auto onChange = [&](bool enabled) {
            if (enabled) {
                RenderableAirTrafficBound::_lat = _latitudeThreshold;
                RenderableAirTrafficBound::_lon = _longitudeThreshold;
            }
            else {
                RenderableAirTrafficBound::_lat = glm::vec2(-90.f, 90.f);
                RenderableAirTrafficBound::_lon = glm::vec2(-180.f, 180.f);
            }
        };

        onEnabledChange(onChange);
        
        setRenderBin(RenderBin::PostDeferredTransparent);

        _latitudeThreshold.onChange([&]() {RenderableAirTrafficBound::_lat = _latitudeThreshold; });
        _longitudeThreshold.onChange([&]() {RenderableAirTrafficBound::_lon = _longitudeThreshold; });
    };

    glm::vec2 RenderableAirTrafficBound::_lat;
    glm::vec2 RenderableAirTrafficBound::_lon;

    void RenderableAirTrafficBound::initializeGL() {
        
        glGenVertexArrays(1, &_vertexArray);
        glGenBuffers(1, &_vertexBuffer);

        _shader = global::renderEngine->buildRenderProgram(
            "AirTrafficBoundProgram",
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficbound_vs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficbound_fs.glsl"),
            absPath("${MODULE_AIRTRAFFIC}/shaders/airtrafficbound_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

        _lat = glm::vec2(-90.f, 90.f);
        _lon = glm::vec2(-180.f, 180.f);

        updateBuffers();
    };

    void RenderableAirTrafficBound::deinitializeGL() {
        glDeleteBuffers(1, &_vertexBuffer);
        glDeleteVertexArrays(1, &_vertexArray);

        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
        
        return;
    };

    bool RenderableAirTrafficBound::isReady() const {
        return _shader != nullptr;
    }

    void RenderableAirTrafficBound::render(const RenderData& data, RendererTasks& rendererTask) {
        
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
        _shader->setUniform(_uniformCache.latitudeThreshold, _latitudeThreshold);
        _shader->setUniform(_uniformCache.longitudeThreshold, _longitudeThreshold);
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

        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_ALWAYS);

        glBindVertexArray(_vertexArray);
        glLineWidth(_lineWidth);

        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_vertexBufferData.size()));

        glDepthFunc(GL_LESS);

        glBindVertexArray(0);

        _shader->deactivate();
    };

    glm::vec2 RenderableAirTrafficBound::getLatBound() {
        return _lat;
    }

    glm::vec2 RenderableAirTrafficBound::getLonBound() {
        return _lon;
    }

    void RenderableAirTrafficBound::updateBuffers() {
        
        _vertexBufferData.clear();

        BoundVBOLayout bBoxVBO;
        _vertexBufferData.push_back(bBoxVBO);
        _vertexBufferData.push_back(bBoxVBO);

        glBindVertexArray(_vertexArray);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);


        glBufferData(
            GL_ARRAY_BUFFER,
            _vertexBufferData.size() * sizeof(BoundVBOLayout),
            _vertexBufferData.data(),
            GL_STATIC_DRAW
        );

        // lat long
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0,
            2,
            GL_FLOAT,
            GL_FALSE,
            sizeof(BoundVBOLayout),
            nullptr
        );

        glBindVertexArray(0);
    }

} // namespace openspace

