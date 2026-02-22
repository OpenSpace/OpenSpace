
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

#include <modules/base/rendering/grids/renderableradialgrid.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <optional>
#include <utility>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color used for the grid lines.",
        Property::Visibility::NoviceUser
    };

    constexpr Property::PropertyInfo GridSegmentsInfo = {
        "GridSegments",
        "Number of grid segments",
        "Specifies the number of segments for the grid, in the radial and angular "
        "direction respectively.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo CircleSegmentsInfo = {
        "CircleSegments",
        "Number of circle segments",
        "The number of segments that is used to render each circle in the grid.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line width",
        "The width of the grid lines. The larger number, the thicker the lines.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo RadiiInfo = {
        "Radii",
        "Inner and outer radius",
        "The radii values that determine the size of the circular grid. The first value "
        "is the radius of the inmost ring and the second is the radius of the outmost "
        "ring.",
        Property::Visibility::User
    };

    const PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the grid."
    };

    // This `Renderable` creates a planar circular grid with a given size. Optionally, it
    // may have a hole in the center.
    //
    // The size is determined by two radii values: The first (inner) radius defines the
    // hole in the center. The second (outer) radius defines the full grid size. To create
    // a solid circle that connects at the center, set the inner radius to zero (default).
    struct [[codegen::Dictionary(RenderableRadialGrid)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(GridSegmentsInfo.description)]]
        std::optional<glm::ivec2> gridSegments;

        // [[codegen::verbatim(CircleSegmentsInfo.description)]]
        std::optional<int> circleSegments;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(RadiiInfo.description)]]
        std::optional<glm::vec2> radii;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("labelscomponent")]];
    };
} // namespace
#include "renderableradialgrid_codegen.cpp"

namespace openspace {

Documentation RenderableRadialGrid::Documentation() {
    return codegen::doc<Parameters>("base_renderable_radialgrid");
}

RenderableRadialGrid::RenderableRadialGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , _gridSegments(GridSegmentsInfo, glm::ivec2(10), glm::ivec2(1), glm::ivec2(200))
    , _circleSegments(CircleSegmentsInfo, 36, 4, 200)
    , _lineWidth(LineWidthInfo, 0.5f, 1.f, 20.f)
    , _radii(RadiiInfo, glm::vec2(0.f, 1.f), glm::vec2(0.f), glm::vec2(20.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _color = p.color.value_or(_color);
    _color.setViewOption(Property::ViewOptions::Color);
    addProperty(_color);

    _gridSegments = p.gridSegments.value_or(_gridSegments);
    _gridSegments.onChange([this]() { _gridIsDirty = true; });
    addProperty(_gridSegments);

    _circleSegments = p.circleSegments.value_or(_circleSegments);
    _circleSegments.onChange([this]() {
        if (_circleSegments.value() % 2 == 1) {
            _circleSegments = _circleSegments - 1;
        }
        _gridIsDirty = true;
    });
    addProperty(_circleSegments);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _radii = p.radii.value_or(_radii);
    _radii.setViewOption(Property::ViewOptions::MinMaxRange);
    _radii.onChange([this]() { _gridIsDirty = true; });

    addProperty(_radii);

    if (p.labels.has_value()) {
        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
        // Fading of the labels should also depend on the fading of the renderable
        _labels->setParentFadeable(this);
    }
}

bool RenderableRadialGrid::isReady() const {
    return _gridProgram && (_hasLabels ? _labels->isReady() : true);
}

void RenderableRadialGrid::initialize() {
    if (_hasLabels) {
        _labels->initialize();
    }
}

void RenderableRadialGrid::initializeGL() {
    _gridProgram = BaseModule::ProgramObjectManager.request(
        "GridProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "GridProgram",
                absPath("${MODULE_BASE}/shaders/grid_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/grid_fs.glsl")
            );
        }
    );
}

void RenderableRadialGrid::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        "GridProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableRadialGrid::render(const RenderData& data, RendererTasks&) {
    _gridProgram->activate();

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data);

    _gridProgram->setUniform("modelView", modelViewTransform);
    _gridProgram->setUniform("modelViewProjection", modelViewProjectionTransform);
    _gridProgram->setUniform("opacity", opacity());
    _gridProgram->setUniform("gridColor", _color);

    // Change GL state:
    glLineWidth(_lineWidth);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LINE_SMOOTH);
    glDepthMask(false);

    for (const GeometryData& c : _circles) {
        c.render();
    }

    _lines.render();

    _gridProgram->deactivate();

    // Restore GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
    global::renderEngine->openglStateCache().resetDepthState();

    // Draw labels
    if (_hasLabels && _labels->enabled()) {
        const glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
        const glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
        glm::vec3 right = glm::cross(viewDirection, lookup);
        const glm::vec3 up = glm::cross(right, viewDirection);

        const glm::dmat4 worldToModelTransform = glm::inverse(modelTransform);
        glm::vec3 orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
        );

        if (orthoRight == glm::vec3(0.0)) {
            const glm::vec3 otherVector = glm::vec3(lookup.y, lookup.x, lookup.z);
            right = glm::cross(viewDirection, otherVector);
            orthoRight = glm::normalize(
                glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
            );
        }
        const glm::vec3 orthoUp = glm::normalize(
            glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
        );
        _labels->render(data, modelViewProjectionTransform, orthoRight, orthoUp);
    }
}

void RenderableRadialGrid::update(const UpdateData&) {
    if (!_gridIsDirty) [[likely]] {
        return;
    }

    const float innerRadius = _radii.value().x;
    const float outerRadius = _radii.value().y;

    // Circles
    const int nRadialSegments = _gridSegments.value()[0];
    const float fnCircles = static_cast<float>(nRadialSegments);
    const float deltaRadius = (outerRadius - innerRadius) / fnCircles;

    const bool hasInnerRadius = innerRadius > 0.f;
    const int nCircles = hasInnerRadius ? nRadialSegments : nRadialSegments + 1;

    _circles.clear();
    _circles.reserve(nCircles);

    auto addRing = [this](int nSegments, float radius) {
        std::vector<rendering::Vertex> vertices =
            rendering::createRing(nSegments, radius);

        _circles.emplace_back(GL_LINE_STRIP);
        std::vector<rendering::VertexXYZ> data = rendering::convert(std::move(vertices));
        _circles.back().update(data);
    };

    // add an extra inmost circle
    if (hasInnerRadius) {
        addRing(_circleSegments, innerRadius);
    }

    for (int i = 0; i < nRadialSegments; i++) {
        const float ri = static_cast<float>(i + 1) * deltaRadius + innerRadius;
        addRing(_circleSegments, ri);
    }

    // Lines
    const int nLines = _gridSegments.value()[1];
    const int nVertices = 2 * nLines;

    std::vector<rendering::VertexXYZ> data;
    data.reserve(nVertices);

    if (nLines > 1) {
        std::vector<rendering::Vertex> outerVertices =
            rendering::createRing(nLines, outerRadius);

        std::vector<rendering::Vertex> innerVertices =
            rendering::createRing(nLines, innerRadius);

        for (int i = 0; i < nLines; i++) {
            const rendering::VertexXYZ vOut =
                rendering::convertToXYZ(outerVertices[i]);

            const rendering::VertexXYZ vIn =
                rendering::convertToXYZ(innerVertices[i]);

            data.push_back(vOut);
            data.push_back(vIn);
        }
    }
    _lines.update(data);

    setBoundingSphere(static_cast<double>(outerRadius));

    _gridIsDirty = false;
}

RenderableRadialGrid::GeometryData::GeometryData(GLenum renderMode)
    : mode(renderMode)
{
    glCreateVertexArrays(1, &vao);
    glEnableVertexArrayAttrib(vao, 0);
    glVertexArrayAttribFormat(vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vao, 0, 0);
}

RenderableRadialGrid::GeometryData::GeometryData(GeometryData&& other) noexcept {
    if (this == &other) {
        return;
    }

    vao = other.vao;
    vbo = other.vbo;
    size = other.size;
    mode = other.mode;

    other.vao = 0;
    other.vbo = 0;
}

RenderableRadialGrid::GeometryData&
RenderableRadialGrid::GeometryData::operator=(GeometryData&& other) noexcept
{
    if (this != &other) {
        vao = other.vao;
        vbo = other.vbo;
        size = other.size;
        mode = other.mode;

        other.vao = 0;
        other.vbo = 0;
    }
    return *this;
}

RenderableRadialGrid::GeometryData::~GeometryData() {
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
}

void RenderableRadialGrid::GeometryData::update(
                                            const std::vector<rendering::VertexXYZ>& data)
{
    glDeleteBuffers(1, &vbo);
    glCreateBuffers(1, &vbo);
    glVertexArrayVertexBuffer(vao, 0, vbo, 0, sizeof(rendering::VertexXYZ));

    glNamedBufferStorage(
        vbo,
        data.size() * sizeof(rendering::VertexXYZ),
        data.data(),
        GL_NONE_BIT
    );

    size = static_cast<GLsizei>(data.size());
}

void RenderableRadialGrid::GeometryData::render() const {
    glBindVertexArray(vao);
    glDrawArrays(mode, 0, size);
    glBindVertexArray(0);
}

} // namespace openspace
