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

using json = nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "RenderableTube";
    constexpr int8_t CurrentMajorVersion = 0;
    constexpr int8_t CurrentMinorVersion = 1;
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
    readDataFile();
    updateTubeData();
}

void RenderableTube::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "TubeProgram",
        absPath("${MODULE_BASE}/shaders/tube_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/tube_fs.glsl")
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

    // Read the entire file into a string
    constexpr size_t readSize = std::size_t(4096);
    fileStream.exceptions(std::ios_base::badbit);

    std::string data;
    std::string buf = std::string(readSize, '\0');
    while (fileStream.read(buf.data(), readSize)) {
        data.append(buf, 0, fileStream.gcount());
    }
    data.append(buf, 0, fileStream.gcount());
    fileStream.close();

    // convert to a json object
    json jsonData = json::parse(data);

    // Ceck version
    bool foundVersion = false;
    if (auto version = jsonData.find("version"); version != jsonData.end()) {
        auto major = version->find("major");
        auto minor = version->find("minor");

        if (major != version->end() && minor != version->end()) {
            foundVersion = true;
            if (*major != CurrentMajorVersion || *minor != CurrentMinorVersion) {
                LWARNING(fmt::format(
                    "Unknown data version '{}.{}' found. The currently supported version "
                    "is {}.{}", major->dump(), minor->dump(), CurrentMajorVersion,
                    CurrentMinorVersion
                ));
            }
        }
    }

    if (!foundVersion) {
        LWARNING("Could not find version information, version might not be supported");
    }

    // Find polygons
    auto polygons = jsonData.find("polygons");
    if (polygons == jsonData.end() || polygons->size() < 1) {
        LERROR("Could not find any polygon in the data");
        return;
    }

    // Loop throught json object to fill the datastructure for the polygons
    for (auto it = polygons->begin(); it < polygons->end(); ++it) {
        TimePolygon timePolygon;

        // Timestamp
        auto time = it->find("time");
        if (time == it->end()) {
            LERROR("Could not find time for polygon in data");
            return;
        }
        std::string timeString = time->dump();
        timeString.erase(
            std::remove(timeString.begin(), timeString.end(), '\"'),
            timeString.end()
        );
        timePolygon.timestamp = Time::convertTime(timeString);

        // Coordinates
        auto points = it->find("points");
        if (points == it->end() || points->size() < 1) {
            LERROR("Could not find points for polygon in data");
            return;
        }
        for (auto pt = points->begin(); pt < points->end(); ++pt) {
            auto px = pt->find("x");
            auto py = pt->find("y");
            auto pz = pt->find("z");

            if (px == pt->end() || py == pt->end() || pz == pt->end()) {
                LERROR("Could not find coordinate component for polygon in data");
                return;
            }

            double x, y, z;
            pt->at("x").get_to(x);
            pt->at("y").get_to(y);
            pt->at("z").get_to(z);

            glm::dvec3 point(x, y, z);
            timePolygon.points.push_back(point);
        }
        _data.push_back(timePolygon);
    }
}


void RenderableTube::updateTubeData() {
    if (_data.size() < 2) {
        LWARNING("Tube is empty");
        return;
    }

    _vertexArray.clear();
    _indexArray.clear();

    using namespace rendering::helper;
    int nShapeSegments = 3;
    int nLines = 3;
    int baseRadius = 1000000;
    int radius = 1000000;
    int length = 10000000;

    // Get unit circle vertices on the XY-plane
    std::vector<VertexXYZ> unitVertices = createRingXYZ(nShapeSegments, 1.f);
    std::vector<VertexXYZ> unitVerticesLines = createRingXYZ(nLines, 1.f);

    // Put base vertices into array
    for (const glm::dvec3& coord : _data.front().points) {
        _vertexArray.push_back(coord.x);
        _vertexArray.push_back(coord.y);
        _vertexArray.push_back(coord.z);
    }

    // Put top shape vertices into array
    for (const glm::dvec3& coord : _data.back().points) {
        _vertexArray.push_back(coord.x);
        _vertexArray.push_back(coord.y);
        _vertexArray.push_back(coord.z);
    }

    // Put the vertices for the connecting lines into array
    // NOTE: assumes all polygons have the same number of points
    for (int j = 0; j < _data.front().points.size(); ++j) {
        // Base
        _vertexArray.push_back(_data.front().points[j].x);
        _vertexArray.push_back(_data.front().points[j].y);
        _vertexArray.push_back(_data.front().points[j].z);

        // Top
        _vertexArray.push_back(_data.back().points[j].x);
        _vertexArray.push_back(_data.back().points[j].y);
        _vertexArray.push_back(_data.back().points[j].z);
    }

    // Indices for Base shape
    ghoul_assert(
        nShapeSegments.value() <= std::numeric_limits<uint16_t>::max(),
        "Too many shape segments"
    );
    for (uint16_t i = 0; i < nShapeSegments; ++i) {
        _indexArray.push_back(i);
    }

    // Reset
    _indexArray.push_back(std::numeric_limits<uint16_t>::max());

    // Indices for Top shape
    for (int i = nShapeSegments; i < 2 * nShapeSegments; ++i) {
        _indexArray.push_back(static_cast<uint16_t>(i));
    }

    // Indices for connecting lines
    for (int i = 0, k = 0; i < nLines; ++i, k += 2) {
        // Reset
        _indexArray.push_back(std::numeric_limits<uint16_t>::max());

        _indexArray.push_back(static_cast<uint16_t>(2 * nShapeSegments + k));
        _indexArray.push_back(static_cast<uint16_t>(2 * nShapeSegments + k + 1));
    }
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
        _indexArray.size() * sizeof(uint16_t),
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
    glPrimitiveRestartIndex(std::numeric_limits<uint16_t>::max());
    glLineWidth(_lineWidth);
    glBindVertexArray(_vaoId);

    glDrawElements(
        GL_LINE_LOOP,
        static_cast<GLsizei>(_indexArray.size()),
        GL_UNSIGNED_SHORT,
        nullptr
    );

    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();
    glDisable(GL_PRIMITIVE_RESTART);

    _shader->deactivate();
}

void RenderableTube::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }
    if (_tubeIsDirty) {
        updateTubeData();
        updateBufferData();
        //setBoundingSphere(_length * glm::compMax(data.modelTransform.scale));
        _tubeIsDirty = false;
    }
}

} // namespace openspace
