/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/softwareintegration/rendering/renderablepointscloud.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "shaderProgram";
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "TODO"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "TODO"
    };
} // namespace

namespace openspace {

    documentation::Documentation RenderablePointsCloud::Documentation() {
        using namespace documentation;
        return {
            "RenderablePointsCloud",
            "softwareintegration_renderable_pointscloud",
            {
                {
                    ColorInfo.identifier,
                    new DoubleVector3Verifier,
                    Optional::Yes,
                    ColorInfo.description
                },
                {
                    SizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    SizeInfo.description
                }
            }
        };
    }

    RenderablePointsCloud::RenderablePointsCloud(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _color(
            ColorInfo,
            glm::vec3(0.5f, 0.5, 0.5f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        )
        , _size(SizeInfo, 0.5f, 0.f, 20.f)
    {
        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderablePointsCloud"
        );

        if (dictionary.hasKey(ColorInfo.identifier)) {
            _color = dictionary.value<glm::vec3>(ColorInfo.identifier);
        }
        _color.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_color);

        if (dictionary.hasKey(SizeInfo.identifier)) {
            _size = dictionary.value<float>(SizeInfo.identifier);
        }
        addProperty(_size);
    }

    bool RenderablePointsCloud::isReady() const {
        return _shaderProgram != nullptr;
    }

    void RenderablePointsCloud::initializeGL() {
        _shaderProgram = global::renderEngine.buildRenderProgram(
            "PointsCloud",
            absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_vs.glsl"),
            absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_fs.glsl")
        );

        glGenVertexArrays(1, &_vaoID);
        glGenBuffers(1, &_vBufferID);

        glBindVertexArray(_vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
        glEnableVertexAttribArray(0);
        glBindVertexArray(0);
    }

    void RenderablePointsCloud::deinitializeGL() {
        glDeleteVertexArrays(1, &_vaoID);
        _vaoID = 0;

        glDeleteBuffers(1, &_vBufferID);
        _vBufferID = 0;

        if (_shaderProgram) {
            global::renderEngine.removeRenderProgram(_shaderProgram.get());
            _shaderProgram = nullptr;
        }
    }

    void RenderablePointsCloud::render(const RenderData& data, RendererTasks&) {
        _shaderProgram->activate();

        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        _shaderProgram->setUniform("modelViewTransform", modelViewTransform);
        _shaderProgram->setUniform(
            "MVPTransform",
            glm::dmat4(data.camera.projectionMatrix()) * modelViewTransform
        );

        _shaderProgram->setUniform("color", _color);
        _shaderProgram->setUniform("size", _size);

        // Changes GL state:
        glEnablei(GL_BLEND, 0);
        glBlendEquation(GL_FUNC_ADD);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex

        glBindVertexArray(_vaoID);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_varray.size()));
        glBindVertexArray(0);

        _shaderProgram->deactivate();

    }

    void RenderablePointsCloud::update(const UpdateData&) {
        if (!_isDirty) {
            return;
        }
        std::string planets = absPath("${MODULE_SOFTWAREINTEGRATION}/testdata/testPointsCloud.csv");
        std::vector<std::vector<std::string>> dataString = ghoul::loadCSVFile(planets);
        //LWARNING(fmt::format("dataString: {}", dataString[0][0]));

        for (int i = 0; i < dataString.size(); i++ ) { // rows
                float x = std::stof(dataString[i][0]) * 3.0857E16;
                float y = std::stof(dataString[i][1]) * 3.0857E16;
                float z = std::stof(dataString[i][2]) * 3.0857E16;

                _varray.push_back({ x, y, z });
        }

        glBindVertexArray(_vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _varray.size() * sizeof(Vertex),
            _varray.data(),
            GL_STATIC_DRAW
        );

        glVertexAttribPointer(
            0,
            3,
            GL_FLOAT,
            GL_FALSE,
            sizeof(Vertex),
            nullptr
        );

        glBindVertexArray(0);

        _isDirty = false;
    }

} // namespace openspace
