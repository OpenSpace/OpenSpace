/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/exoplanetsexperttool/rendering/renderablepointdata.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr const std::array<const char*, 4> UniformNames = {
        "modelViewTransform", "MVPTransform", "opacity", "size",
    };

    constexpr openspace::properties::Property::PropertyInfo HighlightColorInfo = {
        "HighlightColor",
        "Highlight Color",
        "The color of the highlighted/selected points."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo SelectedSizeScaleInfo = {
        "SelectedSizeScale",
        "Selected Size Scale Factor",
        "The scaling factor applied to the size of the higlighted/selected points."
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "Selection",
        "Selection",
        "A list of indices of selected points to be highlighted."
    };

    struct [[codegen::Dictionary(RenderablePointData)]] Parameters {
        // [[codegen::verbatim(HighlightColorInfo.description)]]
        std::optional<glm::vec3> highlightColor [[codegen::color()]];

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(SelectedSizeScaleInfo.description)]]
        std::optional<float> selectedSizeScale;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<int>> selection;

        //The file to read the point data from
        std::filesystem::path dataFile;
    };
#include "renderablepointdata_codegen.cpp"
} // namespace

namespace openspace::exoplanets {

documentation::Documentation RenderablePointData::Documentation() {
    return codegen::doc<Parameters>("exoplanetsexperttool_renderable_pointdata");
}

RenderablePointData::RenderablePointData(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _highlightColor(HighlightColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _size(SizeInfo, 1.f, 0.f, 150.f)
    , _selectedSizeScale(SelectedSizeScaleInfo, 2.f, 1.f, 5.f)
    , _selectedIndices(SelectionInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _highlightColor = p.highlightColor.value_or(_highlightColor);
    _highlightColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_highlightColor);

    _size = p.size.value_or(_size);
    addProperty(_size);

    _selectedSizeScale = p.selectedSizeScale.value_or(_selectedSizeScale);
    addProperty(_selectedSizeScale);

    addProperty(_opacity);

    _selectedIndices = p.selection.value_or(_selectedIndices);
    _selectedIndices.onChange([this]() { _selectionChanged = true; });
    _selectedIndices.setReadOnly(true);
    addProperty(_selectedIndices);

    _dataFile = std::make_unique<ghoul::filesystem::File>(p.dataFile);
    _dataFile->setCallback([&]() { updateDataFromFile(); });

    updateDataFromFile();
}

bool RenderablePointData::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderablePointData::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "ExoPointsCloud",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/points_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/points_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
}

void RenderablePointData::deinitializeGL() {
    glDeleteVertexArrays(1, &_primaryPointsVAO);
    _primaryPointsVAO = 0;

    glDeleteBuffers(1, &_primaryPointsVBO);
    _primaryPointsVBO = 0;

    glDeleteVertexArrays(1, &_selectedPointsVAO);
    _selectedPointsVAO = 0;

    glDeleteBuffers(1, &_selectedPointsVBO);
    _selectedPointsVBO = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

void RenderablePointData::render(const RenderData& data, RendererTasks&) {
    if (_fullPointData.empty()) {
        return;
    }

    _shaderProgram->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shaderProgram->setUniform(_uniformCache.modelViewTransform, modelViewTransform);
    _shaderProgram->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::dmat4(data.camera.projectionMatrix()) * modelViewTransform
    );

    _shaderProgram->setUniform(_uniformCache.opacity, _opacity);
    _shaderProgram->setUniform(_uniformCache.size, _size);

    // Changes GL state:
    glEnablei(GL_BLEND, 0);
    glDepthMask(false);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader

    glBindVertexArray(_primaryPointsVAO);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_fullPointData.size()));

    // Selected points
    const size_t nSelected = _selectedIndices.value().size();
    if (nSelected > 0) {
        _shaderProgram->setUniform(_uniformCache.size, _selectedSizeScale * _size);
        glBindVertexArray(_selectedPointsVAO);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(nSelected));
    }

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderablePointData::update(const UpdateData&) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_shaderProgram,
            _uniformCache,
            UniformNames
        );
    }

    if (_isDirty) {
        if (_primaryPointsVAO == 0) {
            glGenVertexArrays(1, &_primaryPointsVAO);
            LDEBUG(std::format("Generating Vertex Array id '{}'", _primaryPointsVAO));
        }
        if (_primaryPointsVBO == 0) {
            glGenBuffers(1, &_primaryPointsVBO);
            LDEBUG(std::format(
                "Generating Vertex Buffer Object id '{}'", _primaryPointsVBO
            ));
        }

        glBindVertexArray(_primaryPointsVAO);
        glBindBuffer(GL_ARRAY_BUFFER, _primaryPointsVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            _fullPointData.size() * sizeof(Point),
            _fullPointData.data(),
            GL_STATIC_DRAW
        );

        GLint positionAttribute = _shaderProgram->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttribute);
        glVertexAttribPointer(
            positionAttribute,
            3,
            GL_FLOAT,
            GL_FALSE,
            _nValuesPerPoint * sizeof(float),
            nullptr
        );

        GLint colorAttribute = _shaderProgram->attributeLocation("in_color");
        glEnableVertexAttribArray(colorAttribute);
        glVertexAttribPointer(
            colorAttribute,
            4,
            GL_FLOAT,
            GL_FALSE,
            _nValuesPerPoint * sizeof(float),
            reinterpret_cast<void*>(3 * sizeof(float))
        );

        glBindVertexArray(0);
    }

    if (_selectionChanged) {
        if (_selectedPointsVAO == 0) {
            glGenVertexArrays(1, &_selectedPointsVAO);
            LDEBUG(std::format("Generating Vertex Array id '{}'", _selectedPointsVAO));
        }
        if (_selectedPointsVBO == 0) {
            glGenBuffers(1, &_selectedPointsVBO);
            LDEBUG(std::format(
                "Generating Vertex Buffer Object id '{}'", _selectedPointsVBO
            ));
        }

        const glm::vec3 color = _highlightColor;

        const int nSelected = static_cast<int>(_selectedIndices.value().size());
        std::vector<Point> selectedPoints;
        std::vector<int> newIndices;
        selectedPoints.reserve(nSelected);
        newIndices.reserve(nSelected);

        // For each of the selected indices, find the corresponding point
        for (int i : _selectedIndices.value()) {
            std::vector<int>::iterator pos =
                std::find(_pointIndices.begin(), _pointIndices.end(), i);

            if (pos != _pointIndices.end()) {
                const int index = pos - _pointIndices.begin();
                const Point& p = _fullPointData.at(index);
                Point newP = {
                    p.xyz[0], p.xyz[1], p.xyz[2], color.r, color.g, color.b, 1.f
                };
                selectedPoints.push_back(newP);
                newIndices.push_back(i);
            }
            else {
                LINFO(std::format("No 3D point matching selected index '{}'", i));
            }
        }
        selectedPoints.shrink_to_fit();

        _selectedIndices = newIndices;

        if (selectedPoints.size() > 0) {
            glBindVertexArray(_selectedPointsVAO);
            glBindBuffer(GL_ARRAY_BUFFER, _selectedPointsVBO);
            glBufferData(
                GL_ARRAY_BUFFER,
                selectedPoints.size() * _nValuesPerPoint * sizeof(float),
                selectedPoints.data(),
                GL_STATIC_DRAW
            );

            GLint positionAttribute = _shaderProgram->attributeLocation("in_position");
            glEnableVertexAttribArray(positionAttribute);
            glVertexAttribPointer(
                positionAttribute,
                3,
                GL_FLOAT,
                GL_FALSE,
                _nValuesPerPoint * sizeof(float),
                nullptr
            );

            GLint colorAttribute = _shaderProgram->attributeLocation("in_color");
            glEnableVertexAttribArray(colorAttribute);
            glVertexAttribPointer(
                colorAttribute,
                4,
                GL_FLOAT,
                GL_FALSE,
                _nValuesPerPoint * sizeof(float),
                reinterpret_cast<void*>(3 * sizeof(float))
            );

            glBindVertexArray(0);
        }
    }

    _isDirty = false;
    _selectionChanged = false;
}

void RenderablePointData::updateDataFromFile() {
    LDEBUG(std::format("Updating point data from file: {}", _dataFile->path()));

    std::ifstream file(_dataFile->path(), std::ios::binary);
    if (!file) {
        LERROR(std::format("Could not open file for reading: {}", _dataFile->path()));
        return;
    }

    _fullPointData.clear();
    _pointIndices.clear();

    // Read number of data points
    unsigned int nPoints;
    file.read(reinterpret_cast<char*>(&nPoints), sizeof(unsigned int));

    _fullPointData.reserve(nPoints);
    _pointIndices.reserve(nPoints);

    // OBS: this reading must match the writing in the dataviewer
    for (int i = 0; i < nPoints; i++) {
        size_t index;
        file.read(reinterpret_cast<char*>(&index), sizeof(size_t));

        glm::dvec3 position;
        file.read(reinterpret_cast<char*>(&position.x), sizeof(double));
        file.read(reinterpret_cast<char*>(&position.y), sizeof(double));
        file.read(reinterpret_cast<char*>(&position.z), sizeof(double));

        glm::vec4 color;
        file.read(reinterpret_cast<char*>(&color.x), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.y), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.z), sizeof(float));
        file.read(reinterpret_cast<char*>(&color.w), sizeof(float));

        const glm::vec3 scaledPos = glm::vec3(position * distanceconstants::Parsec);
        _fullPointData.push_back({
            scaledPos.x, scaledPos.y, scaledPos.z, color.x, color.y, color.z, color.w
        });
        _pointIndices.push_back(static_cast<int>(index));
    }

    _isDirty = true;
}

} // namespace openspace::exoplanets
