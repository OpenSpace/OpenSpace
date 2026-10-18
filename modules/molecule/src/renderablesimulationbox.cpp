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

#include <modules/molecule/src/renderablesimulationbox.h>

#include <modules/molecule/moleculemodule.h>
#include <modules/molecule/src/cache.h>
#include <modules/molecule/src/coloring.h>
#include <modules/molecule/src/loader.h>
#include <modules/molecule/src/postprocessing.h>
#include <modules/molecule/src/util.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/profiling.h>
#include <openspace/opengl/gl.h>
#include <openspace/opengl/openglstatecache.h>
#include <openspace/opengl/textureunit.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>
#include <glm/gtc/random.hpp>
#include <core/md_allocator.h>
#include <core/md_bitset.h>
#include <md_filter.h>
#include <md_gl.h>
#include <md_system.h>
#include <md_util.h>
#include <cmath>
#include <string_view>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableSimulationBox";

    constexpr OptionPropertyInfo RepresentationInfo = {
        .identifier = "Representation",
        .name = "Representation type",
        .description = "The visual representation type to use for the molecule."
    };

    constexpr OptionPropertyInfo ColoringInfo = {
        .identifier = "Coloring",
        .name = "Coloring",
        .description = "The color mapping for the atoms."
    };

    constexpr FloatPropertyInfo RepScaleInfo = {
        .identifier = "RepScale",
        .name = "Representation scale",
        .description = "Thickness of the atoms when using the Space Fill or Licorice "
            "representations.",
        .defaults = {
            .value = 1.f,
            .minMax = { 0.1f, 10.f }
        }
    };

    constexpr FloatPropertyInfo AnimationSpeedInfo = {
        .identifier = "AnimationSpeed",
        .name = "Animation speed",
        .description = "Playback speed of the animation (in frames per second).",
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 100.f }
        }
    };

    constexpr FloatPropertyInfo SimulationSpeedInfo = {
        .identifier = "SimulationSpeed",
        .name = "Simulation speed",
        .description = "Adjust the speed of the simulation (seconds per second).",
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 1000.f }
        }
    };

    constexpr FloatPropertyInfo LinearVelocityInfo = {
        .identifier = "LinearVelocity",
        .name = "Linear velocity",
        .description = "Average linear velocity at the start of the simulation (m/s)."
    };

    constexpr FloatPropertyInfo AngularVelocityInfo = {
        .identifier = "AngularVelocity",
        .name = "Angular velocity",
        .description = "Average angular velocity at the start of the simulation "
            "(radians/s)."
    };

    constexpr DVec3PropertyInfo SimulationBoxInfo = {
        .identifier = "SimulationBox",
        .name = "Simulation box",
        .description = "Size of the periodic simulation box."
    };

    constexpr FloatPropertyInfo CollisionRadiusInfo = {
        .identifier = "CollisionRadius",
        .name = "Collision radius",
        .description = "Radius of the collision sphere around molecules."
    };

    constexpr StringPropertyInfo FilterInfo = {
        .identifier = "Filter",
        .name = "Filter",
        .description = "The filter used to remove parts of the dataset."
    };

    constexpr Vec4PropertyInfo CircleColorInfo = {
        .identifier = "CircleColor",
        .name = "Circle color",
        .description = "Color of the circle outlining the simulation.",
        .defaults = {
            .value = glm::vec4(1.f, 1.f, 1.f, 0.25f),
            .minMax = { glm::vec4(0.f), glm::vec4(1.f) }
        }
    };

    constexpr FloatPropertyInfo CircleWidthInfo = {
        .identifier = "CircleWidth",
        .name = "Circle width",
        .description = "Width of the circle outlining the simulation.",
        .defaults = {
            .value = 1.f,
            .minMax = { 0.f, 10.f }
        }
    };

    constexpr FloatPropertyInfo CircleFalloffInfo = {
        .identifier = "CircleFalloff",
        .name = "Circle falloff",
        .description = "Falloff exponent of the circle outlining the simulation.",
        .defaults = {
            .value = 0.f,
            .minMax = { 0.f, 1.f }
        }
    };

    struct [[codegen::Dictionary(RenderableSimulationBox)]] Parameters {
        struct MoleculeData {
            std::string moleculeFile;
            std::optional<std::string> trajectoryFile;
            int count [[codegen::greaterequal(0)]];
        };

        std::vector<MoleculeData> molecules;

        enum class [[codegen::map(molecule::rep::Type)]] Representation {
            SpaceFill,
            Licorice,
            Ribbons,
            Cartoon
        };

        std::optional<Representation> representation
            [[codegen::Info(RepresentationInfo)]];

        enum class [[codegen::map(molecule::rep::Color)]] Coloring {
            Cpk,
            AtomIndex,
            ResId,
            ResIndex,
            ChainId,
            ChainIndex,
            SecondaryStructure
        };

        std::optional<Coloring> coloring [[codegen::Info(ColoringInfo)]];

        std::optional<float> repScale [[codegen::Info(RepScaleInfo)]];

        std::optional<float> animationSpeed [[codegen::Info(AnimationSpeedInfo)]];

        std::optional<float> simulationSpeed [[codegen::Info(SimulationSpeedInfo)]];

        float linearVelocity [[codegen::Info(LinearVelocityInfo)]];

        float angularVelocity [[codegen::Info(AngularVelocityInfo)]];

        glm::dvec3 simulationBox [[codegen::Info(SimulationBoxInfo)]];

        float collisionRadius [[codegen::Info(CollisionRadiusInfo)]];

        std::optional<std::string> filter [[codegen::Info(FilterInfo)]];

        std::optional<glm::vec4> circleColor [[codegen::Info(CircleColorInfo)]];

        std::optional<float> circleWidth [[codegen::Info(CircleWidthInfo)]];

        std::optional<float> circleFalloff [[codegen::Info(CircleFalloffInfo)]];
    };
} // namespace
#include "renderablesimulationbox_codegen.cpp"

namespace openspace {

Documentation RenderableSimulationBox::Documentation() {
    return codegen::doc<Parameters>(
        "molecule_renderable_simulationbox",
        Renderable::Documentation()
    );
}

RenderableSimulationBox::RenderableSimulationBox(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _representation(RepresentationInfo)
    , _coloring(ColoringInfo)
    , _repScale(RepScaleInfo)
    , _animationSpeed(AnimationSpeedInfo)
    , _simulationSpeed(SimulationSpeedInfo)
    , _linearVelocity(LinearVelocityInfo)
    , _angularVelocity(AngularVelocityInfo)
    , _simulationBox(SimulationBoxInfo)
    , _collisionRadius(CollisionRadiusInfo)
    , _filter(FilterInfo)
    , _circleColor(CircleColorInfo)
    , _circleWidth(CircleWidthInfo)
    , _circleFalloff(CircleFalloffInfo)
{
    addProperty(Fadeable::_opacity);

    auto onUpdateRep = [this]() {
        // Representation type/scale is applied during draw operation construction
    };

    auto onUpdateCol = [this]() {
        for (Molecules& mol : _molecules) {
            if (!mol.data.system || !mol.data.drawRep.id) {
                continue;
            }

            md_bitmask_t mask = md_bitmask_create(
                0,
                static_cast<uint32_t>(mol.data.system->atom.count),
                md_get_heap_allocator()
            );

            const std::string& filter = _filter;
            if (!filter.empty() && filter != "all") {
                str_t str = { filter.data(), static_cast<size_t>(filter.length()) };
                md_filter_context_t ctx = {
                    .sys = mol.data.system,
                    .state = &mol.data.state,
                    .ir = nullptr,
                    .atoms = nullptr
                };
                md_bitset_t set = {0};
                md_temp_scope_t temp = md_temp_begin();
                md_allocator_i* tempAlloc = md_temp_allocator(temp);
                md_filter_result_t res =
                    md_filter(&set, str, &ctx, tempAlloc);
                if (res.success) {
                    md_bitmask_assign(&mask, set);
                }
                else {
                    LERROR(std::format("Invalid filter expression: {}", res.error));
                    md_bitmask_set_range(
                        &mask,
                        0,
                        static_cast<uint32_t>(mol.data.system->atom.count)
                    );
                }
                md_temp_end(temp);
            }
            else {
                md_bitmask_set_range(
                    &mask,
                    0,
                    static_cast<uint32_t>(mol.data.system->atom.count)
                );
            }

            const molecule::rep::Color c =
                static_cast<molecule::rep::Color>(_coloring.value());
            molecule::updateRepColor(mol.data.drawRep, *mol.data.system, c, mask);
            md_bitmask_free(&mask);
        }
    };

    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (const Parameters::MoleculeData& mod : p.molecules) {
        _molecules.emplace_back(mod.moleculeFile, mod.trajectoryFile, mod.count);
    }

    _representation.addOptions({
        { static_cast<int>(molecule::rep::Type::SpaceFill), "Space Fill" },
        { static_cast<int>(molecule::rep::Type::Ribbons), "Ribbons" },
        { static_cast<int>(molecule::rep::Type::Cartoon), "Cartoon" },
        { static_cast<int>(molecule::rep::Type::Licorice), "Licorice" }
    });
    _representation = static_cast<int>(codegen::map<molecule::rep::Type>(
        p.representation.value_or(Parameters::Representation::SpaceFill)
    ));
    _representation.onChange(onUpdateRep);
    addProperty(_representation);

    _coloring.addOptions({
        { static_cast<int>(molecule::rep::Color::Cpk), "CPK" },
        { static_cast<int>(molecule::rep::Color::AtomIndex), "Atom Index" },
        { static_cast<int>(molecule::rep::Color::ResId), "Residue ID" },
        { static_cast<int>(molecule::rep::Color::ResIndex), "Residue Index" },
        { static_cast<int>(molecule::rep::Color::ChainId), "Chain ID" },
        { static_cast<int>(molecule::rep::Color::ChainIndex), "Chain Index" },
        {
            static_cast<int>(molecule::rep::Color::SecondaryStructure),
            "Secondary Structure"
        }
    });
    _coloring = static_cast<int>(codegen::map<molecule::rep::Color>(
        p.coloring.value_or(Parameters::Coloring::Cpk)
    ));
    _coloring.onChange(onUpdateCol);
    addProperty(_coloring);

    _repScale = p.repScale.value_or(_repScale);
    _repScale.onChange(onUpdateRep);
    addProperty(_repScale);

    _animationSpeed = p.animationSpeed.value_or(_animationSpeed);
    addProperty(_animationSpeed);

    _simulationSpeed = p.simulationSpeed.value_or(_simulationSpeed);
    addProperty(_simulationSpeed);

    _linearVelocity = p.linearVelocity;
    _angularVelocity = p.angularVelocity;

    _simulationBox = p.simulationBox;
    addProperty(_simulationBox);

    _collisionRadius = p.collisionRadius;

    _filter = p.filter.value_or(_filter);
    _filter.onChange(onUpdateCol);
    addProperty(_filter);

    _circleColor = p.circleColor.value_or(_circleColor);
    _circleColor.setViewOption(Property::ViewOptions::Color);
    addProperty(_circleColor);

    _circleWidth = p.circleWidth.value_or(_circleWidth);
    addProperty(_circleWidth);

    _circleFalloff = p.circleFalloff.value_or(_circleFalloff);
    addProperty(_circleFalloff);

    for (Molecules& molecule : _molecules) {
        Molecules::Data mol;
        for (int i = 0; i < molecule.count; i++) {
            mol.states.emplace_back(
                i == 0 ? _simulationBox.value() / 2.0 :
                glm::linearRand(glm::dvec3(0.0), _simulationBox.value()),
                glm::linearRand(0.0, glm::two_pi<double>()),
                glm::sphericalRand(_linearVelocity.value()),
                glm::sphericalRand(_angularVelocity.value())
            );
        }
        molecule.data = std::move(mol);
    }

    setRenderBin(RenderBin::Overlay);
}

RenderableSimulationBox::~RenderableSimulationBox() {
    for (Molecules& mol : _molecules) {
        freeMolecule(mol.data);
    }
}

void RenderableSimulationBox::initializeGL() {
    ZoneScoped;

    global::moduleEngine->module<MoleculeModule>()->initializeShaders();

    _billboard.program = opengl::ProgramObject::Build(
        "Simulationbox Billboard",
        absPath("${MODULE_MOLECULE}/shaders/billboard_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/billboard_fs.glsl")
    );
    opengl::updateUniformLocations(*_billboard.program, _billboard.uniforms);

    glGenVertexArrays(1, &_billboard.vao);

    for (Molecules& mol : _molecules) {
        initMolecule(mol.data, mol.moleculeFile, mol.trajectoryFile.value_or(""));
        if (!mol.data.system) {
            continue;
        }

        const std::string& filter = _filter;
        md_bitmask_t mask = md_bitmask_create(
            0,
            static_cast<uint32_t>(mol.data.system->atom.count),
            md_get_heap_allocator()
        );

        if (!filter.empty() && filter != "all") {
            str_t str = { filter.data(), static_cast<size_t>(filter.length()) };
            md_filter_context_t ctx = {
                .sys = mol.data.system,
                .state = &mol.data.state,
                .ir = nullptr,
                .atoms = nullptr
            };
            md_bitset_t set = {0};
            md_temp_scope_t temp = md_temp_begin();
            md_allocator_i* tempAlloc = md_temp_allocator(temp);
            md_filter_result_t res =
                md_filter(&set, str, &ctx, tempAlloc);
            if (res.success) {
                md_bitmask_assign(&mask, set);
            }
            else {
                LERROR(std::format("Invalid filter expression: {}", res.error));
                md_bitmask_set_range(
                    &mask,
                    0,
                    static_cast<uint32_t>(mol.data.system->atom.count)
                );
            }
            md_temp_end(temp);
        }
        else {
            md_bitmask_set_range(
                &mask,
                0,
                static_cast<uint32_t>(mol.data.system->atom.count)
            );
        }

        const molecule::rep::Color c =
            static_cast<molecule::rep::Color>(_coloring.value());
        molecule::updateRepColor(mol.data.drawRep, *mol.data.system, c, mask);
        md_bitmask_free(&mask);
    }
}

void RenderableSimulationBox::deinitializeGL() {
    global::moduleEngine->module<MoleculeModule>()->deinitializeShaders();

    glDeleteBuffers(1, &_billboard.vao);
    _billboard.program = nullptr;
}

void RenderableSimulationBox::update(const UpdateData& data) {
    if (!_renderableInView) {
        return;
    }

    _renderableInView = false;

    const double tCur = data.time.j2000Seconds();
    const double dt = tCur - data.previousFrameTime.j2000Seconds();

    for (Molecules& mol : _molecules) {
        if (mol.data.trajectory) {
            const int64_t numFrames = molecule::trajectoryNumFrames(mol.data.trajectory);
            double frame = std::fmod(tCur * _animationSpeed, 2.0 * numFrames);
            if (frame > numFrames) {
                frame = 2.0 * numFrames - frame;
            }

            molecule::interpolateFrame(
                mol.data.state,
                mol.data.trajectory,
                molecule::InterpolationType::Cubic,
                frame
            );
            md_gl_mol_set_atom_position(
                mol.data.drawMol,
                0,
                static_cast<uint32_t>(mol.data.system->atom.count),
                mol.data.state.xyz
            );
        }

        updateSimulation(mol.data, dt * _simulationSpeed);
    }
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks&) {
    global::renderEngine->openglStateCache().loadCurrentGLState();

    const glm::vec3 forward = data.modelTransform.translation - data.camera.position();
    const glm::vec3 dir = data.camera.viewDirectionWorldSpace();

    const float distance = length(forward) * sign(glm::dot(dir, forward));
    const float fakeScaling = 100.f / distance;

    if (distance < 0.f || distance > 1E4) {
        return;
    }

    _renderableInView = true;

    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    const glm::mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(fakeScaling)) *
        glm::translate(glm::dmat4(1.0), -_simulationBox.value() / 2.0);

    const glm::mat4 projMatrix = glm::dmat4(camCopy.sgctInternal.projectionMatrix());

    size_t count = 0;
    for (const Molecules& mol : _molecules) {
        count += mol.data.states.size();
    }

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve(count);

    std::vector<glm::mat4> transforms;
    transforms.reserve(count);

    for (const Molecules& mol : _molecules) {
        for (const Molecules::Data::State& state : mol.data.states) {
            const glm::dmat4 transform =
                glm::translate(glm::dmat4(1.0), state.position) *
                glm::rotate(glm::dmat4(1.0), state.angle, state.rotationAxis);

            transforms.emplace_back(transform);

            md_gl_draw_op_t op = {};
            op.rep = mol.data.drawRep;
            op.model_matrix = glm::value_ptr(transforms.back());
            const molecule::rep::Type t =
                static_cast<molecule::rep::Type>(_representation.value());
            molecule::setDrawOpTypeAndArgs(op, t, _repScale);
            drawOps.push_back(op);
        }
    }

    MoleculeModule* mod = global::moduleEngine->module<MoleculeModule>();

    const md_gl_draw_args_t args = {
        .shaders = mod->shaders(),
        .draw_operations = {
            .count = drawOps.size(),
            .ops = drawOps.data()
        },
        .view_transform = {
            .view_matrix = glm::value_ptr(viewMatrix),
            .proj_matrix = glm::value_ptr(projMatrix),
            .prev_view_matrix = nullptr,
            .prev_proj_matrix = nullptr
        },
        .picking_offset = {},
        .atom_mask = 0,
        .max_bond_length = 0.0f
    };

    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
    glBindFramebuffer(GL_FRAMEBUFFER, mod->fbo());
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    glEnablei(GL_BLEND, 0);
    glDisablei(GL_BLEND, 1);
    glBlendColor(0.f, 0.f, 0.f, opacity());
    glBlendEquationi(0, GL_FUNC_ADD);
    glBlendFuncSeparatei(
        0,
        GL_SRC_ALPHA,
        GL_ONE_MINUS_SRC_ALPHA,
        GL_CONSTANT_ALPHA,
        GL_ONE_MINUS_CONSTANT_ALPHA
    );

    md_gl_draw(&args);

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

    const glm::dmat4 billboardModel =
        camCopy.combinedViewMatrix() *
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::scale(
            glm::dmat4(1.0),
            data.modelTransform.scale * _simulationBox.value() * glm::dvec3(fakeScaling)
        );
    const glm::mat4 faceCamera = inverse(camCopy.viewRotationMatrix());
    const glm::mat4 transform = projMatrix * glm::mat4(billboardModel) * faceCamera;
    const double width =
        distance / glm::compMax(_simulationBox.value() * data.modelTransform.scale) *
        0.01 * _circleWidth;

    const glm::dvec4 depthVec = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
        billboardModel * glm::dvec4(0.0, 0.0, 0.0, 1.0);

    _billboard.program->activate();
    _billboard.program->setUniform(_billboard.uniforms.transform, transform);
    _billboard.program->setUniform(
        _billboard.uniforms.strokeWidth,
        static_cast<float>(width)
    );
    _billboard.program->setUniform(
        _billboard.uniforms.strokeFalloffExp,
        _circleFalloff == 0.f ? std::numeric_limits<float>::max() : 1.f / _circleFalloff
    );
    _billboard.program->setUniform(
        _billboard.uniforms.fragDepth,
        static_cast<float>(depthVec.w)
    );
    _billboard.program->setUniform(_billboard.uniforms.strokeColor, _circleColor);
    _billboard.program->setUniform(_billboard.uniforms.opacity, opacity());

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_billboard.vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    glEnable(GL_CULL_FACE);

    global::renderEngine->openglStateCache().resetBlendState();
}

void RenderableSimulationBox::updateSimulation(Molecules::Data& mol, double dt) {
    for (Molecules::Data::State& molecule : mol.states) {
        molecule.position += molecule.direction * dt;
        molecule.position = glm::mod(molecule.position, _simulationBox.value());
        molecule.angle += glm::length(molecule.rotationAxis) * dt;
    }

    const double collRadiusSquared = _collisionRadius * _collisionRadius;

    for (auto it1 = mol.states.begin(); it1 != mol.states.end(); it1++) {
        for (auto it2 = std::next(it1); it2 != mol.states.end(); it2++) {
            Molecules::Data::State& m1 = *it1;
            Molecules::Data::State& m2 = *it2;

            const glm::dvec3 distVec = m2.position - m1.position;
            const double distSquared = glm::dot(distVec, distVec);

            if (distSquared >= collRadiusSquared ||
                glm::dot(m1.direction, m2.direction) >= 0.f)
            {
                continue;
            }

            const double dist = std::sqrt(distSquared);
            const double intersection = 2.0 * _collisionRadius - dist;
            const glm::dvec3 dir = distVec / dist;
            const glm::dvec3 compM1 = dir * glm::dot(m1.direction, dir);
            const glm::dvec3 compM2 = -dir * glm::dot(m2.direction, -dir);
            m1.direction = m1.direction - compM1 + compM2;
            m2.direction = m2.direction - compM2 + compM1;

            m1.position += -dir * intersection;
            m2.position += dir * intersection;
        }
    }
}

void RenderableSimulationBox::initMolecule(Molecules::Data& mol,
                                           std::filesystem::path molFile,
                                           std::filesystem::path trajFile)
{
    LDEBUG(std::format("Loading molecule file '{}'", molFile.string()));

    freeMolecule(mol);

    const molecule::Molecule* molecule = molecule::loadMolecule(molFile);
    if (!molecule) {
        return;
    }

    mol.system = &molecule->system;
    mol.state.alloc = md_get_heap_allocator();
    md_system_state_copy(&mol.state, &molecule->state);

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile.string()));
        mol.trajectory = molecule::loadTrajectory(trajFile, molecule);

        if (!mol.trajectory) {
            LERROR("Failed to initialize trajectory: failed to load file");
            return;
        }
    }

    const double sphere = glm::compMax(_simulationBox.value()) / 2.0;
    setBoundingSphere(sphere);

    mol.drawMol = md_gl_mol_create(mol.system);
    mol.drawRep = md_gl_rep_create(mol.drawMol);
}

void RenderableSimulationBox::freeMolecule(Molecules::Data& mol) {
    if (mol.drawRep.id) {
        md_gl_rep_destroy(mol.drawRep);
        mol.drawRep = {};
    }
    if (mol.drawMol.id) {
        md_gl_mol_destroy(mol.drawMol);
        mol.drawMol = {};
    }
    if (mol.state.alloc) {
        md_system_state_free(&mol.state);
    }

    mol.system = nullptr;
    mol.trajectory = nullptr;
}

} // namespace openspace
