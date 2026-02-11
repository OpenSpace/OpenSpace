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
#include <modules/molecule/src/util.h>
#include <modules/molecule/src/coloring.h>
#include <modules/molecule/src/loader.h>
#include <modules/molecule/src/postprocessing.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/random.hpp>
#include <core/md_allocator.h>
#include <core/md_array.h>
#include <core/md_bitfield.h>
#include <md_util.h>
#include <md_script.h>
#include <md_filter.h>
#include <cmath>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "RenderableSimulationBox";

    constexpr openspace::properties::Property::PropertyInfo RepresentationInfo = {
        "Representation",
        "Representation Type",
        "The visual representation type to use for the molecule."
    };

    constexpr openspace::properties::Property::PropertyInfo ColoringInfo = {
        "Coloring",
        "Coloring",
        "The color mapping for the atoms."
    };

    constexpr openspace::properties::Property::PropertyInfo RepScaleInfo = {
        "RepScale",
        "Representation Scale",
        "Thickness of the atoms when using the Space Fill or Licorice representations."
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)."
    };

    constexpr openspace::properties::Property::PropertyInfo SimulationSpeedInfo = {
        "SimulationSpeed",
        "Simulation Speed",
        "Adjust the speed of the simulation (seconds per second)."
    };

    constexpr openspace::properties::Property::PropertyInfo LinearVelocityInfo = {
        "LinearVelocity",
        "Linear Velocity",
        "Average linear velocity at the start of the simulation (m/s)."
    };

    constexpr openspace::properties::Property::PropertyInfo AngularVelocityInfo = {
        "AngularVelocity",
        "Angular Velocity",
        "Average angular velocity at the start of the simulation (radians/s)."
    };

    constexpr openspace::properties::Property::PropertyInfo SimulationBoxInfo = {
        "SimulationBox",
        "Simulation Box",
        "Size of the periodic simulation box."
    };

    constexpr openspace::properties::Property::PropertyInfo CollisionRadiusInfo = {
        "CollisionRadius",
        "Collision Radius",
        "Radius of the collision sphere around molecules."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterInfo = {
        "Filter",
        "Filter",
        "The filter used to remove parts of the dataset."
    };

    constexpr openspace::properties::Property::PropertyInfo CircleColorInfo = {
        "CircleColor",
        "Circle Color",
        "Color of the circle outlining the simulation."
    };

    constexpr openspace::properties::Property::PropertyInfo CircleWidthInfo = {
        "CircleWidth",
        "Circle Width",
        "Width of the circle outlining the simulation."
    };

    constexpr openspace::properties::Property::PropertyInfo CircleFalloffInfo = {
        "CircleFalloff",
        "Circle Falloff",
        "Falloff exponent of the circle outlining the simulation."
    };

    // This `Renderable` type is capable of rendering a number of different molecules on a
    // moving path using periodic boundary conditions. This can be used to show, for
    // example the distribution of different molecules or atoms in a specific spatial
    // region, such as the atmosphere of a planet.
    //
    // Multiple molecules can be provided and for each the path containing the structural
    // data and the count of molecules has to be provided. Specifying the trajectory file
    // that describes the movement of each individual molecule in its own relative frame,
    // is optional.
    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        struct MoleculeData {
            // The path to a molecule file that contains the structural information for
            // the molecule.
            std::string moleculeFile;

            // If provided, this trajectory file is used to move the individual atoms of
            // the molecule. In addition, each molecule as a whole will be moving through
            // the simulation box on a straight path, even if no trajectory file is
            // provided.
            std::optional<std::string> trajectoryFile;

            // How many of this type of molecule should be rendered in the simulation box.
            int count;
        };

        // The list of files that are used to initiate this simulation box.
        std::vector<MoleculeData> molecules;

        enum class [[codegen::map(molecule::rep::Type)]] Representation {
            SpaceFill,
            Licorice,
            Ribbons,
            Cartoon
        };

        // [[codegen::verbatim(RepresentationInfo.description)]]
        std::optional<Representation> representation;

        enum class [[codegen::map(molecule::rep::Color)]] Coloring {
            // Uniform,
            Cpk,
            AtomIndex,
            ResId,
            ResIndex,
            ChainId,
            ChainIndex,
            SecondaryStructure
        };

        // [[codegen::verbatim(ColoringInfo.description)]]
        std::optional<Coloring> coloring;

        // [[codegen::verbatim(RepScaleInfo.description)]]
        std::optional<float> repScale;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<float> animationSpeed;

        // [[codegen::verbatim(SimulationSpeedInfo.description)]]
        std::optional<float> simulationSpeed;

        // [[codegen::verbatim(LinearVelocityInfo.description)]]
        float linearVelocity;

        // [[codegen::verbatim(AngularVelocityInfo.description)]]
        float angularVelocity;

        // [[codegen::verbatim(SimulationBoxInfo.description)]]
        glm::dvec3 simulationBox;

        // [[codegen::verbatim(CollisionRadiusInfo.description)]]
        float collisionRadius;

        // [[codegen::verbatim(FilterInfo.description)]]
        std::optional<std::string> filter;

        // [[codegen::verbatim(CircleColorInfo.description)]]
        std::optional<glm::vec4> circleColor;

        // [[codegen::verbatim(CircleWidthInfo.description)]]
        std::optional<float> circleWidth;

        // [[codegen::verbatim(CircleFalloffInfo.description)]]
        std::optional<float> circleFalloff;
    };

#include "renderablesimulationbox_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSimulationBox::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablesimulationbox");
}

RenderableSimulationBox::RenderableSimulationBox(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _representation(RepresentationInfo)
    , _coloring(ColoringInfo)
    , _repScale(RepScaleInfo, 1.f, 0.1f, 10.f)
    , _animationSpeed(AnimationSpeedInfo, 1.f, 0.f, 100.f)
    , _simulationSpeed(SimulationSpeedInfo, 1.f, 0.f, 1000.f)
    , _linearVelocity(LinearVelocityInfo)
    , _angularVelocity(AngularVelocityInfo)
    , _simulationBox(SimulationBoxInfo)
    , _collisionRadius(CollisionRadiusInfo)
    , _filter(FilterInfo)
    , _circleColor(
        CircleColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 0.25f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _circleWidth(CircleWidthInfo, 1.f, 0.f, 10.f)
    , _circleFalloff(CircleFalloffInfo, 0.f, 0.f, 1.f)
{
    addProperty(Fadeable::_opacity);

    auto onUpdateRep = [this]() {
        for (Molecules& mol : _molecules) {
            const molecule::rep::Type t =
                static_cast<molecule::rep::Type>(_representation.value());
            molecule::util::updateRepType(mol.data.drawRep, t, _repScale);
        }
    };

    auto onUpdateCol = [this]() {
        for (Molecules& mol : _molecules) {
            md_bitfield_t mask = md_bitfield_create(default_allocator);

            const std::string& filter = _filter;
            if (!filter.empty() && filter != "" && filter != "all") {
                str_t str = { filter.data(), static_cast<int64_t>(filter.length()) };
                char errBuf[1024];

                const bool success = md_filter(
                    &mask,
                    str,
                    &mol.data.molecule,
                    nullptr,
                    nullptr,
                    errBuf,
                    sizeof(errBuf)
                );
                if (!success) {
                    LERROR(std::format("Invalid filter expression: {}", errBuf));
                    md_bitfield_clear(&mask);
                    md_bitfield_set_range(&mask, 0, mol.data.molecule.atom.count);
                }
            }
            else {
                md_bitfield_set_range(&mask, 0, mol.data.molecule.atom.count);
            }

            const molecule::rep::Type t =
                static_cast<molecule::rep::Type>(_representation.value());
            molecule::util::updateRepType(mol.data.drawRep, t, _repScale);
            const molecule::rep::Color c =
                static_cast<molecule::rep::Color>(_coloring.value());
            molecule::util::updateRepColor(mol.data.drawRep, mol.data.molecule, c, mask);
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
    _circleColor.setViewOption(properties::Property::ViewOptions::Color);
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

    // Initialize billboard
    _billboard.program = ghoul::opengl::ProgramObject::Build(
        "Simulationbox Billboard",
        absPath("${MODULE_MOLECULE}/shaders/billboard_vs.glsl"),
        absPath("${MODULE_MOLECULE}/shaders/billboard_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_billboard.program, _billboard.uniforms);

    glGenVertexArrays(1, &_billboard.vao);

    for (Molecules& mol : _molecules) {
        initMolecule(mol.data, mol.moleculeFile, mol.trajectoryFile.value_or(""));
        const std::string& filter = _filter;

        md_bitfield_t mask = md_bitfield_create(default_allocator);
        if (!filter.empty() && filter != "all") {
            str_t str = { filter.data(), static_cast<int64_t>(filter.length()) };
            char errBuf[1024];

            const bool success = md_filter(
                &mask,
                str,
                &mol.data.molecule,
                nullptr,
                nullptr,
                errBuf,
                sizeof(errBuf)
            );
            if (!success) {
                LERROR(std::format("Invalid filter expression: {}", errBuf));
                md_bitfield_clear(&mask);
                md_bitfield_set_range(&mask, 0, mol.data.molecule.atom.count);
            }
        }
        else {
            md_bitfield_set_range(&mask, 0, mol.data.molecule.atom.count);
        }

        const molecule::rep::Type t =
            static_cast<molecule::rep::Type>(_representation.value());
        molecule::util::updateRepType(mol.data.drawRep, t, _repScale);
        const molecule::rep::Color c =
            static_cast<molecule::rep::Color>(_coloring.value());
        molecule::util::updateRepColor(mol.data.drawRep, mol.data.molecule, c, mask);
    }
}

void RenderableSimulationBox::deinitializeGL() {
    global::moduleEngine->module<MoleculeModule>()->deinitializeShaders();

    glDeleteBuffers(1, &_billboard.vao);
    _billboard.program = nullptr;
}

bool RenderableSimulationBox::isReady() const {
    return true;
}

void RenderableSimulationBox::update(const UpdateData& data) {
    // avoid updating if not in view, as it can be quite expensive.
    if (!_renderableInView) {
        return;
    }

    _renderableInView = false;

    double tCur = data.time.j2000Seconds();
    double dt = tCur - data.previousFrameTime.j2000Seconds();

    for (Molecules& mol : _molecules) {
        // update animation
        if (mol.data.trajectory) {
            // Emulate PingPong animation by manipulating the local time t_local per
            // trajectory
            const int64_t numFrames = md_trajectory_num_frames(mol.data.trajectory);
            double frame = std::fmod(tCur * _animationSpeed, 2.0 * numFrames);
            if (frame > numFrames) {
                frame = 2.0 * numFrames - frame;
            }

            molecule::util::interpolateFrame(
                mol.data.molecule,
                mol.data.trajectory,
                molecule::util::InterpolationType::Cubic,
                frame
            );
            md_gl_molecule_set_atom_position(
                &mol.data.drawMol,
                0,
                static_cast<uint32_t>(mol.data.molecule.atom.count),
                mol.data.molecule.atom.x,
                mol.data.molecule.atom.y,
                mol.data.molecule.atom.z,
                0
            );
        }

        // update simulation
        updateSimulation(mol.data, dt * _simulationSpeed);
    }
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks&) {
    global::renderEngine->openglStateCache().loadCurrentGLState();

    // compute distance from camera to molecule
    glm::vec3 forward = data.modelTransform.translation - data.camera.positionVec3();
    glm::vec3 dir = data.camera.viewDirectionWorldSpace();

    // "signed" distance from camera to object.
    const float distance = length(forward) * sign(glm::dot(dir, forward));
    // we apply artificial scaling to everything to cheat a bit with the unit system:
    const float fakeScaling = 100.f / distance;

    // distance < 0 means behind the camera, 1E4 is arbitrary.
    if (distance < 0.f || distance > 1E4) {
        return;
    }

    _renderableInView = true;

    // Because the molecule is small, a scaling of the view matrix causes the molecule to
    // be moved out of view in clip space. Resetting the scaling for the molecule is fine
    // for now. This will have an impact on stereoscopic depth though.
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

    // We want to preallocate this to avoid reallocations
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
            glm::dmat4 transform =
                glm::translate(glm::dmat4(1.0), state.position) *
                glm::rotate(glm::dmat4(1.0), state.angle, state.rotationAxis);

            transforms.emplace_back(transform);
            drawOps.emplace_back(&mol.data.drawRep, glm::value_ptr(transforms.back()));
        }
    }

    MoleculeModule* mod = global::moduleEngine->module<MoleculeModule>();

    md_gl_draw_args_t args = {
        .shaders = &mod->shaders(),
        .draw_operations = { static_cast<uint32_t>(drawOps.size()), drawOps.data() },
        .view_transform = { glm::value_ptr(viewMatrix), glm::value_ptr(projMatrix) },
        .options = 0
    };

    // draw molecule offscreen
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
    glBindFramebuffer(GL_FRAMEBUFFER, mod->fbo());
    // shading rendering of mold
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    md_gl_draw(&args);

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);


    // draw billboard pre-rendered with molecule inside
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
    // update positions / rotations
    for (Molecules::Data::State& molecule : mol.states) {
        molecule.position += molecule.direction * dt;
        molecule.position = glm::mod(molecule.position, _simulationBox.value());
        molecule.angle += glm::length(molecule.rotationAxis) * dt;
    }

    const double collRadiusSquared = _collisionRadius * _collisionRadius;

    // compute collisions
    // those collisions are really simplistic, they assume spherical boundaries, equal
    // mass, and are order-dependent.
    for (auto it1 = mol.states.begin(); it1 != mol.states.end(); it1++) {
        for (auto it2 = std::next(it1); it2 != mol.states.end(); it2++) {
            Molecules::Data::State& m1 = *it1;
            Molecules::Data::State& m2 = *it2;

            glm::dvec3 distVec = m2.position - m1.position;
            double distSquared = glm::dot(distVec, distVec);

            // collision detected
            if (distSquared < collRadiusSquared &&
                glm::dot(m1.direction, m2.direction) < 0.f)
            {
                double dist = std::sqrt(distSquared);
                double intersection = 2.0 * _collisionRadius - dist;
                // swap the direction components normal to the collision plane from the 2
                // molecules. (simplistic elastic collision of 2 spheres with same mass)
                glm::dvec3 dir = distVec / dist;
                glm::dvec3 compM1 = dir * glm::dot(m1.direction, dir);
                glm::dvec3 compM2 = -dir * glm::dot(m2.direction, -dir);
                m1.direction = m1.direction - compM1 + compM2;
                m2.direction = m2.direction - compM2 + compM1;

                // move the spheres away from each other (not intersecting)
                m1.position += -dir * intersection;
                m2.position += dir * intersection;
            }
        }
    }
}

void RenderableSimulationBox::initMolecule(Molecules::Data& mol,
                                           std::filesystem::path molFile,
                                           std::filesystem::path trajFile)
{
    LDEBUG(std::format("Loading molecule file '{}'", molFile));

    // free previously loaded molecule
    freeMolecule(mol);

    const md_molecule_t* molecule = molecule::loadMolecule(molFile);
    if (!molecule) {
        return;
    }

    md_molecule_copy(&mol.molecule, molecule, default_allocator);

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
        mol.trajectory = molecule::loadTrajectory(trajFile);

        if (!mol.trajectory) {
            LERROR("Failed to initialize trajectory: failed to load file");
            return;
        }
    }

    double sphere = glm::compMax(_simulationBox.value()) / 2.0;
    setBoundingSphere(sphere);

    md_gl_molecule_init(&mol.drawMol, &mol.molecule);
    md_gl_representation_init(&mol.drawRep, &mol.drawMol);
}

void RenderableSimulationBox::freeMolecule(Molecules::Data& mol) {
    md_gl_representation_free(&mol.drawRep);
    md_gl_molecule_free(&mol.drawMol);
    md_molecule_free(&mol.molecule, default_allocator);

    mol.molecule = {};
    mol.trajectory = nullptr;
    mol.drawMol = {};
    mol.drawRep = {};
}

} // namespace openspace
