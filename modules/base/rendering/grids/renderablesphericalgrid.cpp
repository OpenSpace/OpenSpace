/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/rendering/grids/renderablesphericalgrid.h>

#include <modules/base/basemodule.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the grid lines.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LongSegmentsInfo = {
        "LongSegments",
        "Number of Longitudinal Segments",
        "The number of longitudinal segments the sphere is split into. Determines the "
        "resolution of the rendered sphere in a left/right direction when looking "
        "straight at the equator. Should be an even value (if an odd value is provided, "
        "the value will be set to the new value minus one). If the `Segments` value is "
        "provided as well, it will have precedence over this value",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LatSegmentsInfo = {
        "LatSegments",
        "Number of Latitudinal Segments",
        "The number of latitudinal segments the sphere is split into. Determines the "
        "resolution of the rendered sphere in a up/down direction when looking "
        "straight at the equator. Should be an even value (if an odd value is provided, "
        "the value will be set to the new value minus one). If the `Segments` value is "
        "provided as well, it will have precedence over this value",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of the grid lines. The larger number, the thicker the lines.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the grid."
    };

    // This `Renderable` creates a grid in the shape of a sphere. Note that the sphere
    // will always be given a radius of one meter. To change its size, use a `Scale`
    // transform, such as the [StaticScale](#base_transform_scale_static).
    //
    // The grid may be split up into equal segments in both directions using the
    // `Segments` parameter, or different number of segments in the latitudal and
    // longtudal direction using the `LatSegments` and `LongSegments` parameters.
    struct [[codegen::Dictionary(RenderableSphericalGrid)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LongSegmentsInfo.description)]]
        std::optional<int> longSegments;

        // [[codegen::verbatim(LatSegmentsInfo.description)]]
        std::optional<int> latSegments;

        // The number of segments the sphere is split into. Determines the resolution
        // of the rendered sphere. Should be an even value (if an odd value is provided,
        // the value will be set to the new value minus one). Setting this value is equal
        // to setting `LongSegments` and `LatSegments` to the same value. If this value is
        // specified, it will overwrite the values provided in `LongSegments` and
        //`LatSegments`.
        std::optional<int> segments;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("labelscomponent")]];
    };
#include "renderablesphericalgrid_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphericalGrid::Documentation() {
    return codegen::doc<Parameters>("base_renderable_sphericalgrid");
}

RenderableSphericalGrid::RenderableSphericalGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridProgram(nullptr)
    , _color(ColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , _longSegments(LongSegmentsInfo, 36, 4, 200)
    , _latSegments(LatSegmentsInfo, 36, 4, 200)
    , _lineWidth(LineWidthInfo, 0.5f, 1.f, 20.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    auto gridDirty = [this]() {
        _longSegments = std::max<int>(_longSegments, 3);
        _latSegments = std::max<int>(_latSegments, 3);
        _gridIsDirty = true;
    };
    _longSegments = p.segments.value_or(p.longSegments.value_or(_longSegments));
    _longSegments.onChange(gridDirty);
    addProperty(_longSegments);

    _latSegments = p.segments.value_or(p.latSegments.value_or(_latSegments));
    _latSegments.onChange(gridDirty);
    addProperty(_latSegments);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    // Radius is always 1
    setBoundingSphere(1.0);

    if (p.labels.has_value()) {
        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
        // Fading of the labels should also depend on the fading of the renderable
        _labels->setParentFadeable(this);
    }
}

bool RenderableSphericalGrid::isReady() const {
    return _gridProgram && (_hasLabels ? _labels->isReady() : true);
}

void RenderableSphericalGrid::initialize() {
    if (_hasLabels) {
        _labels->initialize();
    }
}

void RenderableSphericalGrid::initializeGL() {
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

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

void RenderableSphericalGrid::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoID);
    _vaoID = 0;

    glDeleteBuffers(1, &_vBufferID);
    _vBufferID = 0;

    BaseModule::ProgramObjectManager.release(
        "GridProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableSphericalGrid::render(const RenderData& data, RendererTasks&) {
    _gridProgram->activate();

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data);

    _gridProgram->setUniform("modelViewTransform", modelViewTransform);
    _gridProgram->setUniform("MVPTransform", modelViewProjectionTransform);
    _gridProgram->setUniform("opacity", opacity());
    _gridProgram->setUniform("gridColor", _color);

    // Change GL state:
#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else // ^^^^ !__APPLE__ // __APPLE__ vvvv
    glLineWidth(1.f);
#endif // __APPLE__

    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LINE_SMOOTH);

    glBindVertexArray(_vaoID);

    // Render latitude rings
    glMultiDrawArrays(
        GL_LINE_LOOP,
        _latitudeRenderInfo.first.data(),
        _latitudeRenderInfo.count.data(),
        _latSegments
    );

    // Render longitude segments
    glMultiDrawArrays(
        GL_LINE_STRIP,
        _longitudeRenderInfo.first.data(),
        _longitudeRenderInfo.count.data(),
        _longSegments
    );

    glBindVertexArray(0);

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
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
        );

        if (orthoRight == glm::vec3(0.f)) {
            const glm::vec3 otherVector = glm::vec3(lookup.y, lookup.x, lookup.z);
            right = glm::cross(viewDirection, otherVector);
            orthoRight = glm::normalize(
                glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
            );
        }
        const glm::vec3 orthoUp = glm::normalize(
            glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
        );
        _labels->render(data, modelViewProjectionTransform, orthoRight, orthoUp);
    }

    // Reset
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableSphericalGrid::update(const UpdateData&) {
    if (!_gridIsDirty) [[likely]] {
        return;
    }

    // Instead of using an element buffer which didn't save that much memory and just
    // caused some indirections, we are creating two sets of vertices in the same vertex
    // buffer in this function. First all of the vertices for the longitudinal rings, one
    // ring after another. After that it is all the vertices for the latitudinal arcs, one
    // arc after another

    // * 2 since we store all vertices twice
    const unsigned int vertSize = _longSegments * _latSegments * 2;
    std::vector<Vertex> vert;
    vert.reserve(vertSize);
    for (int lat = 0; lat < _latSegments; lat++) {
        for (int lng = 0; lng < _longSegments; lng++) {
            // inclination angle (north to south)
            const float theta =
                static_cast<float>(lat) / static_cast<float>(_latSegments - 1) *
                glm::pi<float>(); // 0 -> PI

            // azimuth angle (east to west)
            // Dividing by one segment more as the points for 0 and 2*pi are identical
            const float phi =
                static_cast<float>(lng) / static_cast<float>(_longSegments) *
                2.f * glm::pi<float>();  // 0 -> 2*PI

            const float x = std::sin(phi) * std::sin(theta);  //
            const float y = std::cos(phi) * std::sin(theta);  //
            const float z = std::cos(theta);                  // up
            vert.push_back({ x, y, z });
        }
    }

    // Create the render info struct to be able to render the longitude rings using
    // glMultiDrawArrays in the render function
    _latitudeRenderInfo.first.clear();
    _latitudeRenderInfo.count.clear();
    for (int i = 0; i < _latSegments; i++) {
        _latitudeRenderInfo.first.push_back(i * _longSegments);
        _latitudeRenderInfo.count.push_back(_longSegments);
    }

    // Create the duplicate vertex entries to efficiently render the latitude arcs. We
    // take every vertex in a longitude segment and connect it to the same index along all
    // latitude arcs
    for (int lng = 0; lng < _longSegments; lng++) {
        for (int lat = 0; lat < _latSegments; lat++) {
            Vertex v = vert[lat * _longSegments + lng];
            vert.push_back(v);
        }
    }

    // Create the render info struct to be able to render the latitude arcs using
    // glMultiDrawArrays in the render function. The `base` is the offset to make this
    // render call use the vertices that are in the second "block" of the VBO
    _longitudeRenderInfo.first.clear();
    _longitudeRenderInfo.count.clear();
    const int base = _longSegments * _latSegments;
    for (int i = 0; i < _longSegments; i++) {
        _longitudeRenderInfo.first.push_back(i * _latSegments + base);
        _longitudeRenderInfo.count.push_back(_latSegments);
    }


    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferData(GL_ARRAY_BUFFER, vertSize * sizeof(Vertex), vert.data(), GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);

    _gridIsDirty = false;
}

} // namespace openspace
