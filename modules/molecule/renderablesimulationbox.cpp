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

#include <modules/molecule/renderablesimulationbox.h>

#include <modules/molecule/moleculemodule.h>
#include <modules/molecule/mol/cache.h>
#include <modules/molecule/mol/util.h>
#include <modules/molecule/mol/viamd/coloring.h>
#include <modules/molecule/mol/viamd/loader.h>
#include <modules/molecule/mol/viamd/postprocessing.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/ghoul_gl.h>
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

    constexpr const char* BillboardVertShader = R"(
#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 uTransform;
out vec2 pos;

void main() {
  gl_Position = uTransform * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  gl_Position.z = -1.0; // always visible
  pos = aPos.xy;
}
)";

    constexpr const char* BillboardFragShader = R"(
#version 330 core
out vec4 out_color;
in vec2 pos;

uniform float uStrokeWidth;
uniform float uStrokeFalloffExp;
uniform float uFragDepth;
uniform vec4 uStrokeColor;
uniform sampler2D uColorTex;
uniform sampler2D uDepthTex;

void main() {
  float len = length(pos) * 2.0;

  if (len > 1) {
    discard;
  }

  float depth = texelFetch(uDepthTex, ivec2(gl_FragCoord.xy), 0).r;
  vec4  tex = texelFetch(uColorTex, ivec2(gl_FragCoord.xy), 0);

  if (depth == 1.0) {
	tex.a = 0.0;
  }
    
  float falloff = clamp(1.0 - pow(len / (1.0 - uStrokeWidth), uStrokeFalloffExp), 0.0, 1.0);
    
  gl_FragDepth = uFragDepth;
  out_color = mix(uStrokeColor, tex, falloff);
}
)";

    const float Vertices[] = {
         0.5f,  0.5f, 0.f,  // top right
         0.5f, -0.5f, 0.f,  // bottom right
        -0.5f,  0.5f, 0.f,  // top left 

         0.5f, -0.5f, 0.f,  // bottom right
        -0.5f, -0.5f, 0.f,  // bottom left
        -0.5f,  0.5f, 0.f,  // top left 
    };

    constexpr double normalizeDouble(double input) {
        if (input > 1.0) {
            return input / pow(10, 30);
        }
        else {
            return input - 1.0;
        }
    }

    constexpr mat4_t mat4_from_glm(glm::mat4 const& src) {
        mat4_t dst;
        memcpy(&dst, &src, 4 * 4 * sizeof(float));
        return dst;
    }

    constexpr openspace::properties::Property::PropertyInfo MoleculeFilesInfo = {
        "MoleculeFiles",
        "Molecule Files",
        "Molecule file paths"
    };

    constexpr openspace::properties::Property::PropertyInfo TrajectoryFilesInfo = {
        "TrajectoryFiles",
        "Trajectory Files",
        "Trajectory file paths"
    };

    constexpr openspace::properties::Property::PropertyInfo RepTypeInfo = {
        "RepType",
        "Representation Type",
        "How to draw the molecule"
    };

    constexpr openspace::properties::Property::PropertyInfo ColoringInfo = {
        "Coloring",
        "Coloring",
        "Select a color mapping for the atoms"
    };

    constexpr openspace::properties::Property::PropertyInfo RepScaleInfo = {
        "RepScale",
        "Representation Scale",
        "Thickness of the atoms in Space Fill or Licorice representation"
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)"
    };

    constexpr openspace::properties::Property::PropertyInfo SimulationSpeedInfo = {
        "SimulationSpeed",
        "Simulation Speed",
        "Adjust the speed of the simulation (seconds per second)"
    };

    constexpr openspace::properties::Property::PropertyInfo MoleculeCountsInfo = {
        "MoleculeCounts",
        "Molecule Counts",
        "Count of molecules to simulate"
    };

    constexpr openspace::properties::Property::PropertyInfo LinearVelocityInfo = {
        "LinearVelocity",
        "Linear Velocity",
        "Average linear velocity at the start of the simulation (m/s)"
    };

    constexpr openspace::properties::Property::PropertyInfo AngularVelocityInfo = {
        "AngularVelocity",
        "Angular Velocity",
        "Average angular velocity at the start of the simulation (radians/s)"
    };

    constexpr openspace::properties::Property::PropertyInfo SimulationBoxInfo = {
        "SimulationBox",
        "Simulation Box",
        "Size of the periodic simulation box (x/y/z)"
    };

    constexpr openspace::properties::Property::PropertyInfo CollisionRadiusInfo = {
        "CollisionRadius",
        "Collision Radius",
        "Radius of the collision sphere around molecules"
    };

    constexpr openspace::properties::Property::PropertyInfo ViamdFilterInfo = {
        "ViamdFilter",
        "Viamd Filter",
        "VIAMD script filter for atom visibility"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAOEnabledInfo = {
        "SSAOEnabled",
        "Enable SSAO",
        "Enable SSAO"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAOIntensityInfo = {
        "SSAOIntensity",
        "SSAO Intensity",
        "SSAO Intensity"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAORadiusInfo = {
        "SSAORadius",
        "SSAO Radius",
        "SSAO Radius"
    };

    constexpr openspace::properties::Property::PropertyInfo SSAOBiasInfo = {
        "SSAOBias",
        "SSAO Bias",
        "SSAO Bias"
    };

    constexpr openspace::properties::Property::PropertyInfo ExposureInfo = {
        "Exposure",
        "Exposure",
        "Exposure, Controls the Exposure setting for the tonemap"
    };

    constexpr openspace::properties::Property::PropertyInfo CircleColorInfo = {
        "CircleColor",
        "Circle Color",
        "Color of the circle outlining the simulation"
    };

    constexpr openspace::properties::Property::PropertyInfo CircleWidthInfo = {
        "CircleWidth",
        "Circle Width",
        "Width of the circle outlining the simulation"
    };

    constexpr openspace::properties::Property::PropertyInfo CircleFalloffInfo = {
        "CircleFalloff",
        "Circle Falloff",
        "Falloff exponent of the circle outlining the simulation"
    };

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        enum class [[codegen::map(mol::rep::Type)]] RepresentationType {
            SpaceFill,
            Licorice,
            Ribbons,
            Cartoon,
        };

        enum class [[codegen::map(mol::rep::Color)]] Coloring {
            // Uniform,
            Cpk,
            AtomIndex,
            ResId,
            ResIndex,
            ChainId,
            ChainIndex,
            SecondaryStructure,
            // Property
        };

        // [[codegen::verbatim(MoleculeFilesInfo.description)]]
        std::vector<std::string> moleculeFiles;

        // [[codegen::verbatim(TrajectoryFilesInfo.description)]]
        std::vector<std::string> trajectoryFiles;

        // [[codegen::verbatim(RepTypeInfo.description)]]
        std::optional<RepresentationType> repType;

        // [[codegen::verbatim(ColoringInfo.description)]]
        std::optional<Coloring> coloring;

        // [[codegen::verbatim(RepScaleInfo.description)]]
        std::optional<float> repScale;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<float> animationSpeed;

        // [[codegen::verbatim(SimulationSpeedInfo.description)]]
        std::optional<float> simulationSpeed;

        // [[codegen::verbatim(MoleculeCountsInfo.description)]]
        std::vector<int> moleculeCounts;

        // [[codegen::verbatim(LinearVelocityInfo.description)]]
        float linearVelocity;

        // [[codegen::verbatim(AngularVelocityInfo.description)]]
        float angularVelocity;

        // [[codegen::verbatim(SimulationBoxInfo.description)]]
        glm::dvec3 simulationBox;

        // [[codegen::verbatim(CollisionRadiusInfo.description)]]
        float collisionRadius;

        // [[codegen::verbatim(ViamdFilterInfo.description)]]
        std::optional<std::string> viamdFilter;
        
        // [[codegen::verbatim(SSAOEnabledInfo.description)]]
        std::optional<bool> ssaoEnabled;
        
        // [[codegen::verbatim(SSAOIntensityInfo.description)]]
        std::optional<float> ssaoIntensity;
        
        // [[codegen::verbatim(SSAORadiusInfo.description)]]
        std::optional<float> ssaoRadius;

        // [[codegen::verbatim(SSAOBiasInfo.description)]]
        std::optional<float> ssaoBias;

        // [[codegen::verbatim(ExposureInfo.description)]]
        std::optional<float> exposure;

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
    , _moleculeFiles(MoleculeFilesInfo)
    , _trajectoryFiles(TrajectoryFilesInfo)
    , _repType(RepTypeInfo)
    , _coloring(ColoringInfo)
    , _repScale(RepScaleInfo, 1.f, 0.1f, 10.f)
    , _animationSpeed(AnimationSpeedInfo, 1.f, 0.f, 100.f)
    , _simulationSpeed(SimulationSpeedInfo, 1.f, 0.f, 1000.f)
    , _moleculeCounts(MoleculeCountsInfo)
    , _linearVelocity(LinearVelocityInfo)
    , _angularVelocity(AngularVelocityInfo)
    , _simulationBox(SimulationBoxInfo)
    , _collisionRadius(CollisionRadiusInfo)
    , _viamdFilter(ViamdFilterInfo)
    , _ssaoEnabled(SSAOEnabledInfo)
    , _ssaoIntensity(SSAOIntensityInfo, 12.f, 0.f, 100.f)
    , _ssaoRadius(SSAORadiusInfo, 12.f, 0.1f, 100.f)
    , _ssaoBias(SSAOBiasInfo, 0.1f, 0.f, 1.f)
    , _exposure(ExposureInfo, 0.3f, 0.1f, 10.f)
    , _circleColor(
        CircleColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 0.25f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _circleWidth(CircleWidthInfo, 0.f, 0.f, 10.f)
    , _circleFalloff(CircleFalloffInfo, 0.f, 0.f, 1.f)
{
    _repType.addOptions({
        { static_cast<int>(mol::rep::Type::SpaceFill), "Space Fill" },
        { static_cast<int>(mol::rep::Type::Ribbons), "Ribbons" },
        { static_cast<int>(mol::rep::Type::Cartoon), "Cartoon" },
        { static_cast<int>(mol::rep::Type::Licorice), "Licorice" },
    });
    
    _coloring.addOptions({
        { static_cast<int>(mol::rep::Color::Cpk), "CPK" },
        { static_cast<int>(mol::rep::Color::AtomIndex), "Atom Index" },
        { static_cast<int>(mol::rep::Color::ResId), "Residue ID" },
        { static_cast<int>(mol::rep::Color::ResIndex), "Residue Index" },
        { static_cast<int>(mol::rep::Color::ChainId), "Chain ID" },
        { static_cast<int>(mol::rep::Color::ChainIndex), "Chain Index" },
        { static_cast<int>(mol::rep::Color::SecondaryStructure), "Secondary Structure" },
    });

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFiles = p.moleculeFiles;
    _trajectoryFiles = p.trajectoryFiles;
    _repScale = p.repScale.value_or(1.f);
    _animationSpeed = p.animationSpeed.value_or(1.f);
    _simulationSpeed = p.simulationSpeed.value_or(1.f);
    _moleculeCounts = p.moleculeCounts;
    _linearVelocity = p.linearVelocity;
    _angularVelocity = p.angularVelocity;
    _simulationBox = p.simulationBox;
    _collisionRadius = p.collisionRadius;
    _viamdFilter = p.viamdFilter.value_or("");
    _ssaoEnabled = p.ssaoEnabled.value_or(true);
    _ssaoIntensity = p.ssaoIntensity.value_or(12.f);
    _ssaoRadius = p.ssaoRadius.value_or(12.f);
    _ssaoBias = p.ssaoBias.value_or(0.1f);
    _exposure = p.exposure.value_or(0.2f);
    _circleColor = p.circleColor.value_or(glm::vec4(1.f));
    _circleColor.setViewOption(properties::Property::ViewOptions::Color);
    _circleWidth = p.circleWidth.value_or(1.f);
    _circleFalloff = p.circleFalloff.value_or(0.f);


    if (p.repType.has_value()) {
        _repType = static_cast<int>(codegen::map<mol::rep::Type>(*p.repType));
    } else {
        _repType = static_cast<int>(mol::rep::Type::SpaceFill);
    }
    
    if (p.coloring.has_value()) {
        _coloring = static_cast<int>(codegen::map<mol::rep::Color>(*p.coloring));
    } else {
        _coloring = static_cast<int>(mol::rep::Color::Cpk);
    }
    
    for (int count : _moleculeCounts.value()) {
        molecule_data_t mol {
            {},      // states
            {},      // concatMolecule
            nullptr, // trajectory
            {},      // drawRep
            {},      // drawMol
        };
        
        for (int i = 0; i < count; i++) {
            MoleculeState demoMolecule {
                i == 0 ?
                    _simulationBox.value() / 2.0 :
                    glm::linearRand(glm::dvec3(0.0), _simulationBox.value()), // position
                    glm::linearRand(0.0, glm::two_pi<double>()),              // angle
                    glm::sphericalRand(_linearVelocity.value()),              // direction
                    glm::sphericalRand(_angularVelocity.value())              // rotation
            };
            mol.states.push_back(demoMolecule);
        }
        
        _molecules.push_back(mol);
    }
    
    auto onUpdateRep = [this]() {
        for (molecule_data_t& mol : _molecules) {
            const mol::rep::Type t = static_cast<mol::rep::Type>(_repType.value());
            mol::util::updateRepType(mol.drawRep, t, _repScale);
        }
    };
    _repType.onChange(onUpdateRep);
    _repScale.onChange(onUpdateRep);

    auto onUpdateCol = [this]() {
        for (molecule_data_t& mol : _molecules) {
            const auto& filter = _viamdFilter.value();

            md_bitfield_t mask = md_bitfield_create(default_allocator);
            if (!filter.empty() && filter != "" && filter != "all") {
                str_t str = {filter.data(), (int64_t)filter.length()};
                char errBuf[1024];

                const bool success = md_filter(
                    &mask,
                    str,
                    &mol.molecule,
                    nullptr,
                    nullptr,
                    errBuf,
                    sizeof(errBuf)
                );
                if (!success) {
                    LERROR(std::format("Invalid filter expression: {}", errBuf));
                    md_bitfield_clear(&mask);
                    md_bitfield_set_range(&mask, 0, mol.molecule.atom.count);
                }
            }
            else {
                md_bitfield_set_range(&mask, 0, mol.molecule.atom.count);
            }

            const mol::rep::Type t = static_cast<mol::rep::Type>(_repType.value());
            mol::util::updateRepType(mol.drawRep, t, _repScale);
            const mol::rep::Color c = static_cast<mol::rep::Color>(_coloring.value());
            mol::util::updateRepColor(mol.drawRep, mol.molecule, c, mask);
        }
    };
    _coloring.onChange(onUpdateCol);
    _viamdFilter.onChange(onUpdateCol);
    
    addProperty(_repType);
    addProperty(_coloring);
    addProperty(_repScale);
    addProperty(_animationSpeed);
    addProperty(_simulationBox);
    addProperty(_simulationSpeed);
    addProperty(_viamdFilter);
    addProperty(_ssaoEnabled);
    addProperty(_ssaoIntensity);
    addProperty(_ssaoRadius);
    addProperty(_ssaoBias);
    addProperty(_exposure);

    addProperty(_circleColor);
    addProperty(_circleWidth);
    addProperty(_circleFalloff);

    setRenderBin(RenderBin::Overlay);
}

RenderableSimulationBox::~RenderableSimulationBox() {
    for (molecule_data_t& mol : _molecules) {
        freeMolecule(mol);
    }
}

void RenderableSimulationBox::initializeGL() {
    ZoneScoped

    // Initialize billboard
    GLuint vs = glCreateShader(GL_VERTEX_SHADER);
    GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(vs, 1, &BillboardVertShader, nullptr);
    glCompileShader(vs);
    glShaderSource(fs, 1, &BillboardFragShader, nullptr);
    glCompileShader(fs);

    _billboard.program = glCreateProgram();
    glAttachShader(_billboard.program, vs);
    glAttachShader(_billboard.program, fs);
    glLinkProgram(_billboard.program);

    glDeleteShader(vs);
    glDeleteShader(fs);

    glGenVertexArrays(1, &_billboard.vao);
    glGenBuffers(1, &_billboard.vbo);
    glBindVertexArray(_billboard.vao);
    glBindBuffer(GL_ARRAY_BUFFER, _billboard.vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glEnableVertexAttribArray(0);

    size_t i = 0;
    for (molecule_data_t& mol : _molecules) {
        initMolecule(mol, _moleculeFiles.value().at(i), _trajectoryFiles.value().at(i));
        const auto& filter = _viamdFilter.value();

        md_bitfield_t mask = md_bitfield_create(default_allocator);
        if (!filter.empty() && filter != "" && filter != "all") {
            str_t str = { filter.data(), static_cast<int64_t>(filter.length()) };
            char errBuf[1024];

            const bool success = md_filter(
                &mask,
                str,
                &mol.molecule,
                nullptr,
                nullptr,
                errBuf,
                sizeof(errBuf)
            );
            if (!success) {
                LERROR(std::format("Invalid filter expression: {}", errBuf));
                md_bitfield_clear(&mask);
                md_bitfield_set_range(&mask, 0, mol.molecule.atom.count);
            }
        }
        else {
            md_bitfield_set_range(&mask, 0, mol.molecule.atom.count);
        }

        const mol::rep::Type t = static_cast<mol::rep::Type>(_repType.value());
        mol::util::updateRepType(mol.drawRep, t, _repScale);
        const mol::rep::Color c = static_cast<mol::rep::Color>(_coloring.value());
        mol::util::updateRepColor(mol.drawRep, mol.molecule, c, mask);
        i++;
    }
}

void RenderableSimulationBox::deinitializeGL() {
    // Billboard
    glDeleteBuffers(1, &_billboard.vao);
    glDeleteBuffers(1, &_billboard.vbo);
    glDeleteProgram(_billboard.program);
}

bool RenderableSimulationBox::isReady() const {
    return true;
}

void RenderableSimulationBox::updateSimulation(molecule_data_t& mol, double dt) {
    // update positions / rotations
    for (MoleculeState& molecule : mol.states) {
        molecule.position += molecule.direction * dt;
        molecule.position = mod(molecule.position, _simulationBox.value());
        molecule.angle += glm::length(molecule.rotationAxis) * dt;
    }

    const double collRadiusSquared = _collisionRadius * _collisionRadius;

    // compute collisions
    // those collisions are really simplistic, they assume spherical boundaries, equal
    // mass, and are order-dependent.
    for (auto it1 = mol.states.begin(); it1 != mol.states.end(); ++it1) {
        for (auto it2 = std::next(it1); it2 != mol.states.end(); ++it2) {

            MoleculeState& m1 = *it1;
            MoleculeState& m2 = *it2;

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

void RenderableSimulationBox::update(const UpdateData& data) {
    // avoid updating if not in view, as it can be quite expensive.
    if (!_renderableInView) {
        return;
    }
    else {
        _renderableInView = false;
    }

    double tCur = data.time.j2000Seconds();
    double dt = tCur - data.previousFrameTime.j2000Seconds();
    
    for (molecule_data_t& mol : _molecules) {
        // update animation
        if (mol.trajectory) {
            // Emulate PingPong animation by manipulating the local time t_local per
            // trajectory
            const int64_t numFrames = md_trajectory_num_frames(mol.trajectory);
            double frame = std::fmod(tCur * _animationSpeed, 2.0 * numFrames);
            if (frame > numFrames) {
                frame = 2.0 * numFrames - frame;
            }

            mol::util::interpolateFrame(
                mol.molecule,
                mol.trajectory,
                mol::util::InterpolationType::Cubic,
                frame
            );
            md_gl_molecule_set_atom_position(
                &mol.drawMol,
                0,
                static_cast<uint32_t>(mol.molecule.atom.count),
                mol.molecule.atom.x,
                mol.molecule.atom.y,
                mol.molecule.atom.z,
                0
            );
        }
    
        // update simulation
        updateSimulation(mol, dt * _simulationSpeed);
    }
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks&) {
    global::renderEngine->openglStateCache().loadCurrentGLState();

    // compute distance from camera to molecule
    glm::vec3 forward = data.modelTransform.translation - data.camera.positionVec3();
    glm::vec3 dir = data.camera.viewDirectionWorldSpace();

    // "signed" distance from camera to object.
    float distance = length(forward) * sign(dot(dir, forward));
    // we apply artificial scaling to everything to cheat a bit with the unit system:
    float fakeScaling = 100.f / distance;

    // distance < 0 means behind the camera, 1E4 is arbitrary.
    if (distance < 0.f || distance > 1E4) {
        return;
    }
    else {
        _renderableInView = true;
    }

    // Because the molecule is small, a scaling of the view matrix causes the molecule to
    // be moved out of view in clip space. Resetting the scaling for the molecule is fine
    // for now. This will have an impact on stereoscopic depth though.
    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    glm::mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        translate(glm::dmat4(1.0), data.modelTransform.translation) *
        scale(glm::dmat4(1.0), data.modelTransform.scale) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(fakeScaling)) *
        translate(glm::dmat4(1.0), -_simulationBox.value() / 2.0);

    glm::mat4 projMatrix = glm::dmat4(camCopy.sgctInternal.projectionMatrix());

    // We want to preallocate this to avoid reallocations
    size_t count = 0;
    for (const auto& mol : _molecules) {
        count += mol.states.size();
    }

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve(count);

    std::vector<glm::mat4> transforms;
    transforms.reserve(count);

    for (const molecule_data_t& mol : _molecules) {
        for (size_t i = 0; i < mol.states.size(); i++) {
            const MoleculeState& state = mol.states[i];
            glm::dmat4 transform =
                glm::translate(glm::dmat4(1.0), state.position) *
                glm::rotate(glm::dmat4(1.0), state.angle, state.rotationAxis) *
                glm::dmat4(1.0);

            transforms.push_back(glm::mat4(transform));

            md_gl_draw_op_t drawOp = {};
            // This is safe because we store it in a vector and preallocate the memory
            drawOp.model_matrix = glm::value_ptr(transforms.back());
            drawOp.rep = &mol.drawRep;
            drawOps.push_back(drawOp);
        }
    }

    MoleculeModule* mod = global::moduleEngine->module<MoleculeModule>();

    md_gl_draw_args_t args = {};
    args.shaders = &mod->shaders();
    args.view_transform = { glm::value_ptr(viewMatrix), glm::value_ptr(projMatrix) };
    args.options = 0;
    args.draw_operations = { static_cast<uint32_t>(drawOps.size()), drawOps.data() };

    // draw molecule offscreen
    {
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
        glBindFramebuffer(GL_FRAMEBUFFER, mod->fbo());
        // shading rendering of mold
        const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, bufs);

        // resize the fbo if needed
        if (global::windowDelegate->windowHasResized()) {
            glm::ivec2 size = global::windowDelegate->currentWindowSize();
            glBindTexture(GL_TEXTURE_2D, mod->colorTexture());
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_RGBA8,
                size.x,
                size.y,
                0,
                GL_RGBA,
                GL_UNSIGNED_BYTE,
                nullptr
            );

            glBindTexture(GL_TEXTURE_2D, mod->normalTexture());
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_RG16,
                size.x,
                size.y,
                0,
                GL_RG,
                GL_UNSIGNED_SHORT,
                nullptr
            );

            glBindTexture(GL_TEXTURE_2D, mod->depthTexture());
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_DEPTH_COMPONENT32F,
                size.x,
                size.y,
                0,
                GL_DEPTH_COMPONENT,
                GL_FLOAT,
                nullptr
            );
        }

        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        md_gl_draw(&args);

        // postprocessing
        {
            postprocessing::Settings settings;
            settings.background.enabled = false;
            settings.ambientOcclusion[0].enabled = _ssaoEnabled;
            settings.ambientOcclusion[0].intensity = _ssaoIntensity;
            settings.ambientOcclusion[0].radius = _ssaoRadius;
            settings.ambientOcclusion[0].horizonBias = _ssaoBias;
            settings.ambientOcclusion[1].enabled = false;
            settings.bloom.enabled = false;
            settings.depthOfField.enabled = false;
            settings.temporalReprojection.enabled = false;
            settings.tonemapping.enabled = true;
            settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
            settings.tonemapping.exposure = _exposure;
            settings.inputTextures.depth = mod->depthTexture();
            settings.inputTextures.color = mod->colorTexture();
            settings.inputTextures.normal = mod->normalTexture();

            postprocessing::postprocess(
                settings,
                mat4_from_glm(data.camera.combinedViewMatrix()),
                mat4_from_glm(projMatrix)
            );

            // restore state after postprocess
            glEnable(GL_DEPTH_TEST);
        }

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    }

    // draw billboard pre-rendered with molecule inside
    {
        glm::dmat4 billboardModel =
            camCopy.combinedViewMatrix() *
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            glm::scale(glm::dmat4(1.0), data.modelTransform.scale) *
            glm::scale(glm::dmat4(1.0), glm::dvec3(_simulationBox)) *
            glm::scale(glm::dmat4(1.0), glm::dvec3(fakeScaling));
        glm::mat4 faceCamera = inverse(camCopy.viewRotationMatrix());
        glm::mat4 transform = projMatrix * glm::mat4(billboardModel) * faceCamera;
        double width =
            distance / glm::compMax(_simulationBox.value() * data.modelTransform.scale) *
            0.01 * _circleWidth;

        glm::dvec4 depth_ = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
            billboardModel * glm::dvec4(0.0, 0.0, 0.0, 1.0);
        double depth = normalizeDouble(depth_.w);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glUseProgram(_billboard.program);
        glBindVertexArray(_billboard.vao);
        glDisable(GL_CULL_FACE);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, mod->colorTexture());
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, mod->depthTexture());
        glUniform1i(glGetUniformLocation(_billboard.program, "uColorTex"), 0);
        glUniform1i(glGetUniformLocation(_billboard.program, "uDepthTex"), 1);
        glUniformMatrix4fv(
            glGetUniformLocation(_billboard.program, "uTransform"),
            1,
            false,
            glm::value_ptr(transform)
        );
        glUniform1f(glGetUniformLocation(_billboard.program, "uStrokeWidth"), static_cast<float>(width));
        glUniform1f(
            glGetUniformLocation(_billboard.program, "uStrokeFalloffExp"),
            _circleFalloff == 0.f ? std::numeric_limits<float>::max() : 1.f / _circleFalloff
        );
        glUniform1f(glGetUniformLocation(_billboard.program, "uFragDepth"), static_cast<float>(depth));
        glm::vec4 stroke = _circleColor;
        glUniform4fv(glGetUniformLocation(_billboard.program, "uStrokeColor"), 1, glm::value_ptr(stroke));
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glEnable(GL_CULL_FACE);
        glBindVertexArray(0);
        glUseProgram(0);
    }

    global::renderEngine->openglStateCache().resetBlendState();
}

void RenderableSimulationBox::initMolecule(molecule_data_t& mol, std::string_view molFile,
                                           std::string_view trajFile)
{
    LDEBUG(std::format("Loading molecule file '{}'", molFile));

    // free previously loaded molecule
    freeMolecule(mol);

    const md_molecule_t* molecule = mol::loadMolecule(molFile);
    if (!molecule) {
        return;
    }

    md_molecule_copy(&mol.molecule, molecule, default_allocator);

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
        mol.trajectory = mol::loadTrajectory(trajFile);

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

void RenderableSimulationBox::freeMolecule(molecule_data_t& mol) {
    md_gl_representation_free(&mol.drawRep);
    md_gl_molecule_free(&mol.drawMol);
    md_molecule_free(&mol.molecule, default_allocator);

    mol.molecule = {};
    mol.trajectory = nullptr;
    mol.drawMol = {};
    mol.drawRep = {};
}

} // namespace openspace
