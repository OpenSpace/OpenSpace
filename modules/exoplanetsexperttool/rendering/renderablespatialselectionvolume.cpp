#include <modules/exoplanetsexperttool/rendering/renderablespatialselectionvolume.h>

#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanetsexperttool/rendering/selectionvolumemesh.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo ShapeParametersInfo = {
        "ShapeParameters",
        "Shape parameters",
        "Selection geometry in parsecs: sphere [0, x, y, z, radius], box "
        "[1, x, y, z, width, height, depth], sky rectangle "
        "[2, raMin, raMax, decMin, decMax, useDistanceFilter, distMin, distMax]. "
        "The straight-sided sky cone approximates the angular selection; wide "
        "ranges use tessellated angular boundaries.",
        Property::Visibility::Hidden
    };

    constexpr Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The selection volume's fill and outline color."
    };

    constexpr Property::PropertyInfo OutlineOpacityInfo = {
        "OutlineOpacity",
        "Outline opacity",
        "The opacity of the selection outline."
    };

    struct [[codegen::Dictionary(RenderableSpatialSelectionVolume)]] Parameters {
        std::optional<std::vector<double>> shapeParameters;
        std::optional<glm::vec3> color [[codegen::color()]];
        std::optional<float> outlineOpacity [[codegen::inrange(0.f, 1.f)]];
    };
} // namespace
#include "renderablespatialselectionvolume_codegen.cpp"

namespace openspace::exoplanets {

openspace::Documentation RenderableSpatialSelectionVolume::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetsexperttool_renderable_spatialselectionvolume",
        Renderable::Documentation()
    );
}

RenderableSpatialSelectionVolume::RenderableSpatialSelectionVolume(
                                            const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _shapeParameters(ShapeParametersInfo)
    , _color(ColorInfo, glm::vec3(0.2f, 0.85f, 0.65f), glm::vec3(0.f), glm::vec3(1.f))
    , _outlineOpacity(OutlineOpacityInfo, 0.65f, 0.f, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _shapeParameters = p.shapeParameters.value_or(std::vector<double>{});
    _shapeParameters.onChange([this]() { _meshIsDirty = true; });
    addProperty(_shapeParameters);
    _color = p.color.value_or(_color.value());
    _color.setViewOption(Property::ViewOptions::Color);
    addProperty(_color);
    _outlineOpacity = p.outlineOpacity.value_or(_outlineOpacity.value());
    addProperty(_outlineOpacity);
    addProperty(Fadeable::_opacity);
    setRenderBin(RenderBin::PostDeferredTransparent);
}

void RenderableSpatialSelectionVolume::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "SpatialSelectionVolume",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/selectionvolume_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/selectionvolume_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);
    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glEnableVertexAttribArray(0);
    glVertexAttribLPointer(0, 3, GL_DOUBLE, sizeof(glm::dvec3), nullptr);
    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    _meshIsDirty = true;
}

void RenderableSpatialSelectionVolume::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
    _vao = 0;
    _vbo = 0;
    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableSpatialSelectionVolume::updateMesh() {
    SelectionVolumeMesh mesh;
    const std::vector<double>& parameters = _shapeParameters.value();
    if (parameters.size() == 5 && parameters[0] == 0.0) {
        mesh = selectionVolumeMesh(SphereVolume{
            .center = glm::dvec3(parameters[1], parameters[2], parameters[3]),
            .radius = parameters[4]
        });
    }
    else if (parameters.size() == 7 && parameters[0] == 1.0) {
        mesh = selectionVolumeMesh(BoxVolume{
            .center = glm::dvec3(parameters[1], parameters[2], parameters[3]),
            .dimensions = glm::dvec3(parameters[4], parameters[5], parameters[6])
        });
    }
    else if (parameters.size() == 8 && parameters[0] == 2.0) {
        mesh = selectionVolumeMesh(SkyMapRect{
            .raMin = parameters[1],
            .raMax = parameters[2],
            .decMin = parameters[3],
            .decMax = parameters[4],
            .useDistanceFilter = parameters[5] != 0.0,
            .distMin = parameters[6],
            .distMax = parameters[7]
        });
    }
    _nTriangleVertices = static_cast<GLsizei>(mesh.triangles.size());
    _nLineVertices = static_cast<GLsizei>(mesh.lines.size());
    mesh.triangles.insert(mesh.triangles.end(), mesh.lines.begin(), mesh.lines.end());
    double radius = 0.0;
    for (const glm::dvec3& vertex : mesh.triangles) {
        radius = std::max(radius, glm::length(vertex));
    }
    setBoundingSphere(radius * distanceconstants::Parsec);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        mesh.triangles.size() * sizeof(glm::dvec3),
        mesh.triangles.data(),
        GL_DYNAMIC_DRAW
    );
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    _meshIsDirty = false;
}

void RenderableSpatialSelectionVolume::update(const UpdateData&) {
    if (_program && _program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }
    if (_meshIsDirty && _program) {
        updateMesh();
    }
}

void RenderableSpatialSelectionVolume::render(const RenderData& data, RendererTasks&) {
    const auto module = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    if (!module || !module->enabled() || _nTriangleVertices == 0) {
        return;
    }
    _program->activate();
    _program->setUniform(_uniformCache.modelViewTransform, calcModelViewTransform(data));
    _program->setUniform(_uniformCache.projectionTransform, data.camera.projectionMatrix());
    _program->setUniform(_uniformCache.parsec, distanceconstants::Parsec);

    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_FALSE);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_CULL_FACE);
    glBindVertexArray(_vao);

    _program->setUniform(_uniformCache.color, glm::vec4(_color.value(), opacity()));
    glCullFace(GL_FRONT);
    glDrawArrays(GL_TRIANGLES, 0, _nTriangleVertices);
    glCullFace(GL_BACK);
    glDrawArrays(GL_TRIANGLES, 0, _nTriangleVertices);

    glDisable(GL_CULL_FACE);
    glLineWidth(1.f);
    _program->setUniform(
        _uniformCache.color, glm::vec4(_color.value(), _outlineOpacity * _fade)
    );
    glDrawArrays(GL_LINES, _nTriangleVertices, _nLineVertices);

    glBindVertexArray(0);
    _program->deactivate();
    ghoul::opengl::OpenGLStateCache& state = global::renderEngine->openglStateCache();
    state.resetBlendState();
    state.resetDepthState();
    state.resetLineState();
    state.resetPolygonAndClippingState();
}

} // namespace openspace::exoplanets