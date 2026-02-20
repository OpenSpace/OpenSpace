/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/keys.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetGlyphCloud";

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

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo CurrentIndexInfo = {
        "CurrentlyHoveredIndex",
        "Currently Hovered Index",
        "The index of the currently hovered planet. Is -1 if no planet is being hovered."
    };

    struct [[codegen::Dictionary(RenderableExoplanetGlyphCloud)]] Parameters {
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

        // [[codegen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("labelscomponent")]];

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
    , _size(SizeInfo, 100.f, 0.f, 500.f)
    , _selectedIndices(SelectionInfo)
    , _billboardMinMaxSize(
        BillboardMinMaxSizeInfo,
        glm::vec2(0.f, 400.f),
        glm::vec2(0.f),
        glm::vec2(1000.f)
    )
    , _useFixedRingWidth(UseFixedWidthInfo, true)
    , _currentlyHoveredIndex(CurrentIndexInfo, -1)
    , _drawLabels(DrawLabelInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _size = p.size.value_or(_size);
    addProperty(_size);

    addProperty(Fadeable::_opacity);

    _selectedIndices = p.selection.value_or(_selectedIndices);
    _selectedIndices.onChange([this]() { _selectionChanged = true; });
    _selectedIndices.setReadOnly(true);
    addProperty(_selectedIndices);

    _billboardMinMaxSize = p.billboardMinMaxSize.value_or(_billboardMinMaxSize);
    _billboardMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_billboardMinMaxSize);

    _useFixedRingWidth = p.useFixedWidth.value_or(_useFixedRingWidth);
    addProperty(_useFixedRingWidth);

    if (p.labels.has_value()) {
        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
    }

    _dataFile = std::make_unique<ghoul::filesystem::File>(p.dataFile);
    _dataFile->setCallback([&]() { _dataFileIsDirty = true; });

    updateDataFromFile();

    // Picking callbacks
    global::callback::keyboard->emplace_back(
        [&](Key key, KeyModifier, KeyAction action, bool) -> bool {
            if (!_enabled) {
                return false;
            }

            bool isCtrl = key == Key::LeftControl;
            if (isCtrl && action == KeyAction::Press) {
                _isInSelectionMode = true;
            }
            else if (isCtrl && action == KeyAction::Release) {
                _isInSelectionMode = false;
                _currentlyHoveredIndex = -1;
            }

            // Do not capture
            return false;
        }
    );
    global::callback::mousePosition->emplace_back(
        [&](double x, double y, bool) {
            if (!_enabled || !_isInSelectionMode) {
                return; // do nothing
            }

            // Convert mouse position to pixel position
            glm::dvec2 lastViewportSize = glm::dvec2(_lastViewPortSize);
            double normalizedX = x / lastViewportSize.x;
            double normalizedY = (lastViewportSize.y - y) / lastViewportSize.y;

            if (_glyphIdTexture) {
                glm::uvec3 textureDim = _glyphIdTexture->dimensions();
                glm::uvec2 pos = glm::uvec2(
                    normalizedX * static_cast<double>(textureDim.x),
                    normalizedY * static_cast<double>(textureDim.y)
                );

                _glyphIdTexture->downloadTexture();

                // TODO: make sure pos is within texture
                if (pos.x < textureDim.x && pos.y < textureDim.y) {
                    glm::vec4 pixelValue = _glyphIdTexture->texelAsFloat({ pos, 0 });
                    _currentlyHoveredIndex =
                        static_cast<int>(std::round(pixelValue.r * _maxIndex)) - 1;
                }
            }
        }
    );

    _currentlyHoveredIndex.setReadOnly(true);
    addProperty(_currentlyHoveredIndex);
}

bool RenderableExoplanetGlyphCloud::isReady() const {
    return _program != nullptr;
}

void RenderableExoplanetGlyphCloud::initialize() {
    if (_hasLabels) {
        _labels->initialize();
    }
}

void RenderableExoplanetGlyphCloud::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "ExoGlyphCloud",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_fs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/glyphs_gs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

    // Generate texture and frame buffer for rendering glyph id
    glCreateFramebuffers(1, &_glyphIdFbo);
    createGlyphIdTexture(glm::uvec3(1080, 720, 1));

    // Give the framebuffer a reasonable name (for RonderDoc debugging)
    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _glyphIdFbo, -1, "Glyph ID Framebuffer");
    }

    // Check status
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Framebuffer is not complete!");
    }

    glCreateVertexArrays(1, &_pointsVao);
    glCreateBuffers(1, &_pointsVbo);

    glCreateVertexArrays(1, &_selectedVao);
    glCreateBuffers(1, &_selectedVbo);
}

void RenderableExoplanetGlyphCloud::deinitializeGL() {
    glDeleteVertexArrays(1, &_pointsVao);
    glDeleteBuffers(1, &_pointsVbo);

    glDeleteVertexArrays(1, &_selectedVao);
    glDeleteBuffers(1, &_selectedVbo);

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

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 viewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();

    const glm::dmat4 modelViewProjectionMatrix = viewProjectionMatrix * modelTransform;

    _program->setUniform(_uniformCache.modelMatrix, modelTransform);
    _program->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
    );

    _program->setUniform(_uniformCache.opacity, opacity());
    _program->setUniform(_uniformCache.size, _size);
    _program->setUniform(_uniformCache.onTop, false);
    _program->setUniform(_uniformCache.isRenderIndexStep, false);
    _program->setUniform(_uniformCache.maxIndex, _maxIndex);
    _program->setUniform(_uniformCache.currentIndex, _currentlyHoveredIndex + 1); // Plus one to map offset in shader

    const float minBillboardSize = _billboardMinMaxSize.value().x; // in pixels
    const float maxBillboardSize = _billboardMinMaxSize.value().y; // in pixels
    _program->setUniform(_uniformCache.minBillboardSize, minBillboardSize);
    _program->setUniform(_uniformCache.maxBillboardSize, maxBillboardSize);

    _program->setUniform(_uniformCache.useFixedRingWidth, _useFixedRingWidth);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    // 1st rendering pass: render the glyphs normally, with correct color
    {
        glEnablei(GL_BLEND, 0);
        glDepthMask(true);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        glBindVertexArray(_pointsVao);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_fullGlyphData.size()));
    }

    // 2nd rendering pass: Render IDs to a separate texture every frame as well
    // To use for picking
    {
        _program->setUniform(_uniformCache.isRenderIndexStep, true);
        GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();

        glBindFramebuffer(GL_FRAMEBUFFER, _glyphIdFbo);
        GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, drawBuffers);

        glm::uvec3 textureDim = _glyphIdTexture->dimensions();

        // Potentially upate texture size
        if (static_cast<unsigned int>(viewport[2]) != textureDim.x ||
            static_cast<unsigned int>(viewport[3]) != textureDim.y)
        {
            createGlyphIdTexture(glm::uvec3(viewport[2], viewport[3], 1));
        }

        // Clear the previous values from the texture
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // No blending please
        glDisablei(GL_BLEND, 0);

        // Draw again! And specify viewport size
        glViewport(viewport[0], viewport[1], textureDim.x, textureDim.y);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_fullGlyphData.size()));

        // Reset index rendering, viewport size and frame buffer
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
        _program->setUniform(_uniformCache.isRenderIndexStep, false);
        glEnablei(GL_BLEND, 0);

        // Save viewport size
        _lastViewPortSize = glm::ivec2(viewport[2], viewport[3]);
    }

    // Selected points
    const size_t nSelected = _selectedIndices.value().size();
    if (nSelected > 0) {
        _program->setUniform(_uniformCache.opacity, 1.f);
        _program->setUniform(_uniformCache.onTop, true);
        glBindVertexArray(_selectedVao);
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(nSelected));
    }

    glBindVertexArray(0);
    _program->deactivate();

    // Restores GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();

    // Draw labels
    if (_drawLabels && _hasLabels) {
        const glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
        const glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
        glm::vec3 right = glm::cross(viewDirection, lookup);
        const glm::vec3 up = glm::cross(right, viewDirection);

        const glm::dmat4 worldToModelTransform = glm::inverse(modelTransform);
        glm::vec3 orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
        );

        if (orthoRight == glm::vec3(0.0)) {
            glm::vec3 otherVector = glm::vec3(lookup.y, lookup.x, lookup.z);
            right = glm::cross(viewDirection, otherVector);
            orthoRight = glm::normalize(
                glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
            );
        }
        const glm::vec3 orthoUp = glm::normalize(
            glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
        );
        _labels->render(data, modelViewProjectionMatrix, orthoRight, orthoUp);
    }
}

void RenderableExoplanetGlyphCloud::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }

    if (_dataFileIsDirty) {
        updateDataFromFile();
    }

    if (_renderDataIsDirty) {
        glNamedBufferData(
            _pointsVbo,
            _fullGlyphData.size() * sizeof(GlyphData),
            _fullGlyphData.data(),
            GL_STATIC_DRAW
        );
        mapVertexAttributes(_pointsVao);
        glVertexArrayVertexBuffer(_pointsVao, 0, _pointsVbo, 0, sizeof(GlyphData));
    }

    if (_selectionChanged) {
        const int nSelected = static_cast<int>(_selectedIndices.value().size());
        std::vector<GlyphData> selectedPoints;
        std::vector<int> newIndices;
        selectedPoints.reserve(nSelected);
        newIndices.reserve(nSelected);

        // For each of the selected indices, find the corresponding point
        for (int i : _selectedIndices.value()) {
            std::vector<size_t>::iterator pos =
                std::find(_glyphIndices.begin(), _glyphIndices.end(), i);

            if (pos != _glyphIndices.end()) {
                const int index = static_cast<int>(pos - _glyphIndices.begin());
                const GlyphData& p = _fullGlyphData.at(index);
                selectedPoints.push_back(p);
                newIndices.push_back(i);
            }
            else {
                LINFO(std::format("No 3D point matching selected index '{}'", i));
            }
        }
        selectedPoints.shrink_to_fit();

        _selectedIndices = newIndices;

        if (selectedPoints.size() > 0) {
            glNamedBufferData(
                _selectedVbo,
                selectedPoints.size() * sizeof(GlyphData),
                selectedPoints.data(),
                GL_STATIC_DRAW
            );
            mapVertexAttributes(_selectedVao);
            glVertexArrayVertexBuffer(_selectedVao, 0, _selectedVbo, 0, sizeof(GlyphData));
        }
    }

    _renderDataIsDirty = false;
    _selectionChanged = false;
}

void RenderableExoplanetGlyphCloud::createGlyphIdTexture(const glm::uvec3 dimensions) {
    // TODO (emmbr, 2022-11-15): at some point try using a integer value for the texture instead.
    // But for now, just make it work!
    _glyphIdTexture = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = dimensions,
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::RGBA,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .filter = ghoul::opengl::Texture::FilterMode::Nearest
        }
    );

    // And a depth buffer of the same dimension
    _depthTexture = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = dimensions,
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::DepthComponent,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{
            .filter = ghoul::opengl::Texture::FilterMode::Linear
        }
    );

    glNamedFramebufferTexture(_glyphIdFbo, GL_COLOR_ATTACHMENT0, *_glyphIdTexture, 0);
    glNamedFramebufferTexture(_glyphIdFbo, GL_DEPTH_ATTACHMENT, *_depthTexture, 0);
}

void RenderableExoplanetGlyphCloud::mapVertexAttributes(GLuint vao) {
    GLint positionAttribute = _program->attributeLocation("in_position");
    glEnableVertexArrayAttrib(vao, positionAttribute);
    glVertexArrayAttribBinding(vao, positionAttribute, 0);
    glVertexArrayAttribFormat(
        vao,
        positionAttribute,
        3,
        GL_FLOAT,
        GL_FALSE,
        offsetof(GlyphData, position)
    );

    GLint componentAttribute = _program->attributeLocation("in_component");
    glEnableVertexArrayAttrib(vao, componentAttribute);
    glVertexArrayAttribBinding(vao, componentAttribute, 0);
    glVertexArrayAttribFormat(
        vao,
        componentAttribute,
        1,
        GL_FLOAT,
        GL_FALSE,
        offsetof(GlyphData, component)
    );

    GLint indexAttribute = _program->attributeLocation("in_glyphIndex");
    glEnableVertexArrayAttrib(vao, indexAttribute);
    glVertexArrayAttribBinding(vao, indexAttribute, 0);
    glVertexArrayAttribIFormat(
        vao,
        indexAttribute,
        1,
        GL_INT,
        offsetof(GlyphData, index)
    );

    GLint nColorsAttribute = _program->attributeLocation("in_nColors");
    glEnableVertexArrayAttrib(vao, nColorsAttribute);
    glVertexArrayAttribBinding(vao, nColorsAttribute, 0);
    glVertexArrayAttribIFormat(
        vao,
        nColorsAttribute,
        1,
        GL_INT,
        offsetof(GlyphData, nColors)
    );

    GLint colorAttribute = _program->attributeLocation("in_colors");
    for (int i = 0; i < MaxNumberColors; i++) {
        GLint currentColorIndex = colorAttribute + i;
        glEnableVertexArrayAttrib(vao, currentColorIndex);
        glVertexArrayAttribBinding(vao, currentColorIndex, 0);
        glVertexArrayAttribFormat(
            vao,
            currentColorIndex,
            4,
            GL_FLOAT,
            GL_FALSE,
            offsetof(GlyphData, colors) + i * 4 * sizeof(float)
        );
    }
}

void RenderableExoplanetGlyphCloud::updateDataFromFile() {
    LDEBUG(std::format("Updating point data from file: {}", _dataFile->path()));

    std::ifstream file(_dataFile->path(), std::ios::binary);
    if (!file) {
        LERROR(std::format("Could not open file for reading: {}", _dataFile->path()));
        return;
    }

    _fullGlyphData.clear();
    _glyphIndices.clear();

    // Read number of data points
    size_t nPoints;
    file.read(reinterpret_cast<char*>(&nPoints), sizeof(size_t));

    _fullGlyphData.reserve(nPoints);
    _glyphIndices.reserve(nPoints);

    int maxIndex = -1;

    // OBS: this reading must match the writing in the dataviewer
    for (size_t i = 0; i < nPoints; i++) {
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

        int indexInSystem;
        file.read(reinterpret_cast<char*>(&indexInSystem), sizeof(int));
        d.component = static_cast<float>(indexInSystem + 1);

        d.index = index + 1;

        if (static_cast<int>(d.index) > maxIndex) {
            maxIndex = static_cast<int>(d.index);
        }

        _fullGlyphData.push_back(std::move(d));
        _glyphIndices.push_back(index);
    }

    _maxIndex = maxIndex;

    _dataFileIsDirty = false;
    _renderDataIsDirty = true;
}

} // namespace openspace::exoplanets
