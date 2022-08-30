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

#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "ExoplanetGlyphCloud";

    constexpr const std::array<const char*, 9> UniformNames = {
        "modelMatrix", "cameraViewProjectionMatrix", "onTop", "useFixedRingWidth",
        "opacity", "size", "screenSize", "minBillboardSize", "maxBillboardSize"
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

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "Selection",
        "Selection",
        "A list of indices of selected points to be highlighted."
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMinMaxSizeInfo = {
        "BillboardMinMaxSize",
        "Billboard Min/Max Size in Pixels",
        "The minimum and maximum size (in pixels) for the glyph billboard."
    };

    constexpr openspace::properties::Property::PropertyInfo UseFixedWidthInfo = {
        "UseFixedWidth",
        "Use Fixed Width",
        "If true, all the rings representing the planets will have the very same width. "
        "Otherwise, the width of each ring decreases a bit as the radius gets larger."
    };

    struct [[codegen::Dictionary(RenderablePointData)]] Parameters {
        // [[codegen::verbatim(HighlightColorInfo.description)]]
        std::optional<glm::vec3> highlightColor [[codegen::color()]];

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<int>> selection;

        // [[codegen::verbatim(BillboardMinMaxSizeInfo.description)]]
        std::optional<glm::vec2> billboardMinMaxSize;

        // The file to read the point data from
        std::filesystem::path dataFile;

        // [[codegen::verbatim(UseFixedWidthInfo.description)]]
        std::optional<bool> useFixedWidth;
    };
#include "renderableexoplanetglyphcloud_codegen.cpp"
} // namespace

namespace openspace::exoplanets {

documentation::Documentation RenderableExoplanetGlyphCloud::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetsexperttool_renderable_exoplanetglyphcloud"
    );
}

RenderableExoplanetGlyphCloud::RenderableExoplanetGlyphCloud(
                                                     const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _highlightColor(HighlightColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _size(SizeInfo, 100.f, 0.f, 500.f)
    , _selectedIndices(SelectionInfo)
    , _billboardMinMaxSize(
        BillboardMinMaxSizeInfo,
        glm::vec2(0.f, 400.f),
        glm::vec2(0.f),
        glm::vec2(1000.f)
    ),
    _useFixedRingWidth(UseFixedWidthInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _highlightColor = p.highlightColor.value_or(_highlightColor);
    _highlightColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_highlightColor);

    _size = p.size.value_or(_size);
    addProperty(_size);

    addProperty(_opacity);
    // Use renderbin set from property
    //registerUpdateRenderBinFromOpacity();
    //setRenderBin(RenderBin::PostDeferredTransparent);

    _selectedIndices = p.selection.value_or(_selectedIndices);
    _selectedIndices.onChange([this]() { _selectionChanged = true; });
    _selectedIndices.setReadOnly(true);
    addProperty(_selectedIndices);

    _billboardMinMaxSize = p.billboardMinMaxSize.value_or(_billboardMinMaxSize);
    _billboardMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_billboardMinMaxSize);

    _useFixedRingWidth = p.useFixedWidth.value_or(_useFixedRingWidth);
    addProperty(_useFixedRingWidth);

    _dataFile = std::make_unique<ghoul::filesystem::File>(p.dataFile);
    _dataFile->setCallback([&]() { updateDataFromFile(); });

    updateDataFromFile();
}

bool RenderableExoplanetGlyphCloud::isReady() const {
    return _program != nullptr;
}

void RenderableExoplanetGlyphCloud::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "ExoGlyphCloud",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_fs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_gs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
}

void RenderableExoplanetGlyphCloud::deinitializeGL() {
    glDeleteVertexArrays(1, &_primaryPointsVAO);
    _primaryPointsVAO = 0;

    glDeleteBuffers(1, &_primaryPointsVBO);
    _primaryPointsVBO = 0;

    glDeleteVertexArrays(1, &_selectedPointsVAO);
    _selectedPointsVAO = 0;

    glDeleteBuffers(1, &_selectedPointsVBO);
    _selectedPointsVBO = 0;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableExoplanetGlyphCloud::render(const RenderData& data, RendererTasks&) {
    if (_fullGlyphData.empty()) {
        return;
    }

    _program->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    _program->setUniform(_uniformCache.modelMatrix, modelTransform);
    _program->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
    );

    _program->setUniform(_uniformCache.opacity, opacity());
    _program->setUniform(_uniformCache.size, _size);
    _program->setUniform(_uniformCache.onTop, false);

    const float minBillboardSize = _billboardMinMaxSize.value().x; // in pixels
    const float maxBillboardSize = _billboardMinMaxSize.value().y; // in pixels
    _program->setUniform(_uniformCache.minBillboardSize, minBillboardSize);
    _program->setUniform(_uniformCache.maxBillboardSize, maxBillboardSize);

    _program->setUniform(_uniformCache.useFixedRingWidth, _useFixedRingWidth);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    // Changes GL state:
    glEnablei(GL_BLEND, 0);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    //glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader

    glBindVertexArray(_primaryPointsVAO);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_fullGlyphData.size()));

    // Selected points
    const size_t nSelected = _selectedIndices.value().size();
    if (nSelected > 0) {
        _program->setUniform(_uniformCache.opacity, 1.f);
        _program->setUniform(_uniformCache.onTop, true);
        glBindVertexArray(_selectedPointsVAO);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(nSelected));
        // OBS!! Does not work if we are alreday rendering points. Why??
    }

    glBindVertexArray(0);
    _program->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableExoplanetGlyphCloud::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_program,
            _uniformCache,
            UniformNames
        );
    }

    if (_isDirty) {
        if (_primaryPointsVAO == 0) {
            glGenVertexArrays(1, &_primaryPointsVAO);
            LDEBUG(fmt::format("Generating Vertex Array id '{}'", _primaryPointsVAO));
        }
        if (_primaryPointsVBO == 0) {
            glGenBuffers(1, &_primaryPointsVBO);
            LDEBUG(fmt::format(
                "Generating Vertex Buffer Object id '{}'", _primaryPointsVBO
            ));
        }

        glBindVertexArray(_primaryPointsVAO);
        glBindBuffer(GL_ARRAY_BUFFER, _primaryPointsVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            _fullGlyphData.size() * sizeof(GlyphData),
            _fullGlyphData.data(),
            GL_STATIC_DRAW
        );

        GLint positionAttribute = _program->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttribute);
        glVertexAttribPointer(
            positionAttribute,
            3,
            GL_FLOAT,
            GL_FALSE,
            sizeof(GlyphData),
            nullptr
        );

        GLint componentAttribute = _program->attributeLocation("in_component");
        glEnableVertexAttribArray(componentAttribute);
        glVertexAttribPointer(
            componentAttribute,
            1,
            GL_FLOAT,
            GL_FALSE,
            sizeof(GlyphData),
            reinterpret_cast<void*>(offsetof(GlyphData, component))
        );

        GLint nColorsAttribute = _program->attributeLocation("in_nColors");
        glEnableVertexAttribArray(nColorsAttribute);
        glVertexAttribIPointer(
            nColorsAttribute,
            1,
            GL_INT,
            sizeof(GlyphData),
            reinterpret_cast<void*>(offsetof(GlyphData, nColors))
        );

        GLint colorAttribute = _program->attributeLocation("in_colors");
        for (int i = 0; i < MaxNumberColors; i++) {
            glEnableVertexAttribArray(colorAttribute + i);
            glVertexAttribPointer(
                colorAttribute + i,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(GlyphData),
                reinterpret_cast<void*>(offsetof(GlyphData, colors) + i * 4 * sizeof(float))
            );
        }

        glBindVertexArray(0);
    }

    if (_selectionChanged) {
        if (_selectedPointsVAO == 0) {
            glGenVertexArrays(1, &_selectedPointsVAO);
            LDEBUG(fmt::format("Generating Vertex Array id '{}'", _selectedPointsVAO));
        }
        if (_selectedPointsVBO == 0) {
            glGenBuffers(1, &_selectedPointsVBO);
            LDEBUG(fmt::format(
                "Generating Vertex Buffer Object id '{}'", _selectedPointsVBO
            ));
        }


        const int nSelected = static_cast<int>(_selectedIndices.value().size());
        std::vector<GlyphData> selectedPoints;
        std::vector<int> newIndices;
        selectedPoints.reserve(nSelected);
        newIndices.reserve(nSelected);

        // For each of the selected indices, find the corresponding point
        for (int i : _selectedIndices.value()) {
            std::vector<int>::iterator pos =
                std::find(_glyphIndices.begin(), _glyphIndices.end(), i);

            if (pos != _glyphIndices.end()) {
                const int index = static_cast<int>(pos - _glyphIndices.begin());
                const GlyphData& p = _fullGlyphData.at(index);
                GlyphData newP = p;
                newP.colors[0] = glm::vec4(_highlightColor.value(), 1.f);
                newP.nColors = 1;

                selectedPoints.push_back(newP);
                newIndices.push_back(i);
            }
            else {
                LINFO(fmt::format("No 3D point matching selected index '{}'", i));
            }
        }
        selectedPoints.shrink_to_fit();

        _selectedIndices = newIndices;

        if (selectedPoints.size() > 0) {
            glBindVertexArray(_selectedPointsVAO);
            glBindBuffer(GL_ARRAY_BUFFER, _selectedPointsVBO);
            glBufferData(
                GL_ARRAY_BUFFER,
                selectedPoints.size() * sizeof(GlyphData),
                selectedPoints.data(),
                GL_STATIC_DRAW
            );

            GLint positionAttribute = _program->attributeLocation("in_position");
            glEnableVertexAttribArray(positionAttribute);
            glVertexAttribPointer(
                positionAttribute,
                3,
                GL_FLOAT,
                GL_FALSE,
                sizeof(GlyphData),
                nullptr
            );

            GLint componentAttribute = _program->attributeLocation("in_component");
            glEnableVertexAttribArray(componentAttribute);
            glVertexAttribPointer(
                componentAttribute,
                1,
                GL_FLOAT,
                GL_FALSE,
                sizeof(GlyphData),
                reinterpret_cast<void*>(offsetof(GlyphData, component))
            );

            GLint nColorsAttribute = _program->attributeLocation("in_nColors");
            glEnableVertexAttribArray(nColorsAttribute);
            glVertexAttribIPointer(
                nColorsAttribute,
                1,
                GL_INT,
                sizeof(GlyphData),
                reinterpret_cast<void*>(offsetof(GlyphData, nColors))
            );

            GLint colorAttribute = _program->attributeLocation("in_colors");
            for (int i = 0; i < MaxNumberColors; i++) {
                glEnableVertexAttribArray(colorAttribute + i);
                glVertexAttribPointer(
                    colorAttribute + i,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    sizeof(GlyphData),
                    reinterpret_cast<void*>(offsetof(GlyphData, colors) + i * 4 * sizeof(float))
                );
            }

            glBindVertexArray(0);
        }
    }

    _isDirty = false;
    _selectionChanged = false;
}

void RenderableExoplanetGlyphCloud::updateDataFromFile() {
    LDEBUG(fmt::format("Updating point data from file: {}", _dataFile->path()));

    std::ifstream file(_dataFile->path(), std::ios::binary);
    if (!file) {
        LERROR(fmt::format("Could not open file for reading: {}", _dataFile->path()));
        return;
    }

    _fullGlyphData.clear();
    _glyphIndices.clear();

    // Read number of data points
    unsigned int nPoints;
    file.read(reinterpret_cast<char*>(&nPoints), sizeof(unsigned int));

    _fullGlyphData.reserve(nPoints);
    _glyphIndices.reserve(nPoints);

    // OBS: this reading must match the writing in the dataviewer
    for (unsigned int i = 0; i < nPoints; i++) {
        GlyphData d;

        size_t index;
        file.read(reinterpret_cast<char*>(&index), sizeof(size_t));

        size_t nColors;
        file.read(reinterpret_cast<char*>(&nColors), sizeof(size_t));
        d.nColors = static_cast<int>(nColors);

        glm::dvec3 position;
        file.read(reinterpret_cast<char*>(&position.x), sizeof(double));
        file.read(reinterpret_cast<char*>(&position.y), sizeof(double));
        file.read(reinterpret_cast<char*>(&position.z), sizeof(double));
        const glm::vec3 scaledPos = glm::vec3(position * distanceconstants::Parsec);

        d.position = scaledPos;

        size_t temp = nColors > MaxNumberColors ? MaxNumberColors : nColors;

        for (size_t j = 0; j < temp; j++) {
            glm::vec4 color;
            file.read(reinterpret_cast<char*>(&color.r), sizeof(float));
            file.read(reinterpret_cast<char*>(&color.g), sizeof(float));
            file.read(reinterpret_cast<char*>(&color.b), sizeof(float));
            file.read(reinterpret_cast<char*>(&color.a), sizeof(float));
            d.colors[j] = color;
        }

        int component;
        file.read(reinterpret_cast<char*>(&component), sizeof(int));
        d.component = static_cast<float>(component);

        _fullGlyphData.push_back(std::move(d));
        _glyphIndices.push_back(static_cast<int>(index));
    }

    _isDirty = true;
}

} // namespace openspace::exoplanets
