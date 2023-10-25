/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/base/rendering/renderabletube.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTube";
    constexpr int8_t DataFileVersion = 0;
    constexpr std::array<const char*, 2> UniformNames = {
        "modelViewProjectionTransform", "vs_color"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width",
        // @VISIBILITY(2.0)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB color for the line",
        // @VISIBILITY(1.2)
        openspace::properties::Property::Visibility::NoviceUser
    };

    struct [[codegen::Dictionary(RenderableTube)]] Parameters {
        // The input file with data for the tube
        std::string file;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];
    };
#include "renderabletube_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTube::Documentation() {
    return codegen::doc<Parameters>("base_renderable_tube");
}

RenderableTube::RenderableTube(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _dataFile = p.file;

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    _lineColor = p.color.value_or(_lineColor);
    addProperty(_lineColor);

    addProperty(Fadeable::_opacity);
}

bool RenderableTube::isReady() const {
    return _shader != nullptr;
}

void RenderableTube::initialize() {
    updateTubeData();
}

void RenderableTube::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "PrismProgram",
        absPath("${MODULE_BASE}/shaders/prism_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/prism_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vboId);
    glGenBuffers(1, &_iboId);

    glBindVertexArray(_vaoId);

    updateBufferData();

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glBindVertexArray(0);
}

void RenderableTube::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vboId);
    _vboId = 0;

    glDeleteBuffers(1, &_iboId);
    _iboId = 0;
}

void RenderableTube::readDataFile() {
    std::filesystem::path file = absPath(_dataFile);
    if (!std::filesystem::is_regular_file(file)) {
        LWARNING(fmt::format("The data file '{}' could not be found", file));
        return;
    }

    // Open file
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        LERROR(fmt::format("Failed to open data file '{}'", file));
        return;
    }

    // Read 1 item


    // Loop over all items until stream is not good

    fileStream.close();
}

RenderableTube::TimePolygon RenderableTube::readDataLine(std::ifstream& file) {
    RenderableTube::TimePolygon timePolygon;
    int nPoints = 0;
    std::string date, time;

    file >> date >> time >> nPoints;

    if (!file) {
        return std::move(RenderableTube::TimePolygon());
    }

    // Convert to J2000 seconds
    std::string timeString = fmt::format("{} {}", date, time);
    timePolygon.timestamp = Time::convertTime(timeString);

    for (int i = 0; i < nPoints; ++i) {
        RenderableTube::Coordinate coord;
        file >> coord.x >> coord.y >> coord.z;

        if (file) {
            timePolygon.points.push_back(coord);
        }
        else {
            break;
        }
    }

    return std::move(timePolygon);
}

void RenderableTube::updateTubeData() {
    /*_vertexArray.clear();
    _indexArray.clear();

    using namespace rendering::helper;

    // Get unit circle vertices on the XY-plane
    std::vector<VertexXYZ> unitVertices = createRingXYZ(_nShapeSegments.value(), 1.f);
    std::vector<VertexXYZ> unitVerticesLines = createRingXYZ(_nLines.value(), 1.f);

    // Put base vertices into array
    for (int j = 0; j < _nShapeSegments; ++j) {
        float ux = unitVertices[j].xyz[0];
        float uy = unitVertices[j].xyz[1];

        _vertexArray.push_back(ux * _baseRadius); // x
        _vertexArray.push_back(uy * _baseRadius); // y
        _vertexArray.push_back(0.f);              // z
    }

    // Put top shape vertices into array
    for (int j = 0; j < _nShapeSegments; ++j) {
        float ux = unitVertices[j].xyz[0];
        float uy = unitVertices[j].xyz[1];

        _vertexArray.push_back(ux * _radius); // x
        _vertexArray.push_back(uy * _radius); // y
        _vertexArray.push_back(_length);      // z
    }

    // Put the vertices for the connecting lines into array
    if (_nLines == 1) {
        // In the case of just one line then connect the center points instead
        // Center for base shape
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);

        // Center for top shape
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(_length);
    }
    else {
        for (int j = 0; j < _nLines; ++j) {
            float ux = unitVerticesLines[j].xyz[0];
            float uy = unitVerticesLines[j].xyz[1];

            // Base
            _vertexArray.push_back(ux * _baseRadius); // x
            _vertexArray.push_back(uy * _baseRadius); // y
            _vertexArray.push_back(0.f);              // z

            // Top
            _vertexArray.push_back(ux * _radius); // x
            _vertexArray.push_back(uy * _radius); // y
            _vertexArray.push_back(_length);      // z
        }
    }

    // Indices for Base shape
    ghoul_assert(
        _nShapeSegments.value() <= std::numeric_limits<uint8_t>::max(),
        "Too many shape segments"
    );
    for (uint8_t i = 0; i < _nShapeSegments; ++i) {
        _indexArray.push_back(i);
    }

    // Reset
    _indexArray.push_back(255);

    // Indices for Top shape
    for (int i = _nShapeSegments; i < 2 * _nShapeSegments; ++i) {
        _indexArray.push_back(static_cast<uint8_t>(i));
    }

    // Indices for connecting lines
    for (int i = 0, k = 0; i < _nLines; ++i, k += 2) {
        // Reset
        _indexArray.push_back(255);

        _indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k));
        _indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k + 1));
    }*/
}

void RenderableTube::updateBufferData() {
    glBindBuffer(GL_ARRAY_BUFFER, _vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indexArray.size() * sizeof(uint8_t),
        _indexArray.data(),
        GL_STREAM_DRAW
    );
}

void RenderableTube::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data);

    // Uniforms
    _shader->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(modelViewProjectionTransform)
    );
    _shader->setUniform(_uniformCache.color, glm::vec4(_lineColor.value(), opacity()));

    // Render
    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(255);
    glLineWidth(_lineWidth);
    glBindVertexArray(_vaoId);

    glDrawElements(
        GL_LINE_LOOP,
        static_cast<GLsizei>(_indexArray.size()),
        GL_UNSIGNED_BYTE,
        nullptr
    );

    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();
    glDisable(GL_PRIMITIVE_RESTART);

    _shader->deactivate();
}

void RenderableTube::update(const UpdateData& data) {
    /*if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }
    if (_prismIsDirty) {
        updateVertexData();
        updateBufferData();
        setBoundingSphere(_length * glm::compMax(data.modelTransform.scale));
        _prismIsDirty = false;
    }*/
}

} // namespace openspace
