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

#include "renderablesimulationbox.h"

#include "billboard.h"

#include "glbinding/gl/bitfield.h"
#include "glbinding/gl/enum.h"
#include "glbinding/gl/functions.h"

#include <core/md_allocator.h>
#include <core/md_array.h>
#include <md_util.h>
#include <md_script.h>
#include <md_filter.h>

#include "mol/viamd/coloring.h"
#include "mol/viamd/loader.h"
#include "mol/viamd/postprocessing.h"
#include "mol/cache.h"
#include "mol/util.h"

#include <glm/gtc/random.hpp>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include "openspace/engine/windowdelegate.h"
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>

// COMBAK: because my ide complains
#ifndef ZoneScoped
#define ZoneScoped
#endif

using namespace glm;
using namespace gl;

constexpr const char* shader_output_snippet = R"(
layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_normal;

vec2 encode_normal (vec3 n) {
   float p = sqrt(n.z * 8 + 8);
   return n.xy / p + 0.5;
}

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
    out_normal = vec4(encode_normal(view_normal), 0, 0);
    out_color = color;
}
)";

namespace {
    constexpr const char* _loggerCat = "RenderableSimulationBox";

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
    };

#include "renderablesimulationbox_codegen.cpp"
}

static GLuint fbo = 0;
static GLuint colorTex = 0;
static GLuint normalTex = 0;
static GLuint depthTex = 0;
static int glUseCount = 0;
static md_gl_shaders_t shaders;

namespace openspace {

documentation::Documentation RenderableSimulationBox::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablemolecule");
}

RenderableSimulationBox::RenderableSimulationBox(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
    _moleculeFiles(MoleculeFilesInfo),
    _trajectoryFiles(TrajectoryFilesInfo),
    _repType(RepTypeInfo),
    _coloring(ColoringInfo),
    _repScale(RepScaleInfo, 1.f, 0.1f, 10.f),
    _animationSpeed(AnimationSpeedInfo, 1.f, 0.f, 100.f),
    _simulationSpeed(SimulationSpeedInfo, 1.f, 0.f, 1000.f),
    _moleculeCounts(MoleculeCountsInfo),
    _linearVelocity(LinearVelocityInfo),
    _angularVelocity(AngularVelocityInfo),
    _simulationBox(SimulationBoxInfo),
    _collisionRadius(CollisionRadiusInfo),
    _viamdFilter(ViamdFilterInfo),
    _ssaoEnabled(SSAOEnabledInfo),
    _ssaoIntensity(SSAOIntensityInfo, 12.f, 0.f, 100.f),
    _ssaoRadius(SSAORadiusInfo, 12.f, 0.1f, 100.f),
    _ssaoBias(SSAOBiasInfo, 0.1f, 0.0f, 1.0f),
    _exposure(ExposureInfo, 0.3f, 0.1f, 10.f)
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
    _exposure = p.exposure.value_or(0.3f);

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
            molecule_state_t demoMolecule {
                i == 0 ?
                    _simulationBox.value() / 2.0 :
                    linearRand(dvec3(0.0), _simulationBox.value()), // position
                linearRand(0.0, two_pi<double>()),                  // angle
                sphericalRand(_linearVelocity.value()),             // direction
                sphericalRand(_angularVelocity.value()),            // rotation
            };
            mol.states.push_back(demoMolecule);
        }
        
        _molecules.push_back(mol);
    }
    
    auto onUpdateRep = [this]() {
        for (molecule_data_t& mol: _molecules) {
            mol::util::update_rep_type(mol.drawRep, static_cast<mol::rep::Type>(_repType.value()), _repScale);
        }
    };

    auto onUpdateCol = [this]() {
        for (molecule_data_t& mol: _molecules) {
            mol::util::update_rep_color(mol.drawRep, mol.molecule, static_cast<mol::rep::Color>(_coloring.value()), _viamdFilter);
        }
    };
    
    _repType.onChange(onUpdateRep);
    _repScale.onChange(onUpdateRep);
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

    setRenderBin(RenderBin::PostDeferredTransparent);
}

RenderableSimulationBox::~RenderableSimulationBox() {
    for (molecule_data_t& mol : _molecules) {
        freeMolecule(mol);
    }
}

void RenderableSimulationBox::initialize() {
    ZoneScoped
}

void RenderableSimulationBox::initializeGL() {
    ZoneScoped
    
    if (!fbo) { // initialize static gl things (common to all renderable instances)
        glGenFramebuffers(1, &fbo);
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        ivec2 size = global::windowDelegate->currentWindowSize();
        
        glGenTextures(1, &colorTex);
        glBindTexture(GL_TEXTURE_2D, colorTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, size.x, size.y, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTex, 0);
        glBindTexture(GL_TEXTURE_2D, 0);

        glGenTextures(1, &normalTex);
        glBindTexture(GL_TEXTURE_2D, normalTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16, size.x, size.y, 0, GL_RG, GL_UNSIGNED_SHORT, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, normalTex, 0);
        glBindTexture(GL_TEXTURE_2D, 0);

        glGenTextures(1, &depthTex);
        glBindTexture(GL_TEXTURE_2D, depthTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, depthTex, 0);
        glBindTexture(GL_TEXTURE_2D, 0);

        if(glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            LERROR("Mold Framebuffer is not complete");

        md_gl_initialize();
        md_gl_shaders_init(&shaders, shader_output_snippet);
        
        postprocessing::initialize(size.x, size.y);

        glBindFramebuffer(GL_FRAMEBUFFER, 0);

        billboardGlInit();
    }

    size_t i = 0;
    for (molecule_data_t& mol : _molecules) {
        initMolecule(mol, _moleculeFiles.value().at(i), _trajectoryFiles.value().at(i));
        mol::util::update_rep_color(mol.drawRep, mol.molecule, static_cast<mol::rep::Color>(_coloring.value()), _viamdFilter);
        i++;
    }
    
    glUseCount++;
}

void RenderableSimulationBox::deinitializeGL() {
    glUseCount--;
    if (glUseCount == 0 && fbo) {
        glDeleteTextures(1, &depthTex);
        glDeleteTextures(1, &normalTex);
        glDeleteTextures(1, &colorTex);
        glDeleteFramebuffers(1, &fbo);
        depthTex = 0;
        colorTex = 0;
        fbo = 0;
        billboardGlDeinit();
        postprocessing::shutdown();
    }

    billboardGlDeinit();
    md_gl_shaders_free(&shaders);
    md_gl_shutdown();
}

bool RenderableSimulationBox::isReady() const {
    return true;
}

void RenderableSimulationBox::updateSimulation(molecule_data_t& mol, double dt) {
    // update positions / rotations
    for (auto& molecule : mol.states) {
        molecule.position += molecule.direction * dt;
        molecule.position = mod(molecule.position, _simulationBox.value());
        molecule.angle += length(molecule.rotationAxis) * dt;
    }

    double collRadiusSquared = _collisionRadius * _collisionRadius;

    // compute collisions
    // those collisions are really simplistic, they assume spherical boundaries, equal
    // mass, and are order-dependent.
    for (auto it1 = mol.states.begin(); it1 != mol.states.end(); ++it1) {
        for (auto it2 = std::next(it1); it2 != mol.states.end(); ++it2) {

            molecule_state_t& m1 = *it1;
            molecule_state_t& m2 = *it2;

            dvec3 distVec = m2.position - m1.position;
            double distSquared = dot(distVec, distVec);

            if (distSquared < collRadiusSquared && dot(m1.direction, m2.direction) < 0) { // collision detected
                double dist = sqrt(distSquared);
                double intersection = 2.0 * _collisionRadius - dist;
                // swap the direction components normal to the collision plane from the 2
                // molecules. (simplistic elastic collision of 2 spheres with same mass)
                dvec3 dir = distVec / dist;
                dvec3 compM1 = dir * dot(m1.direction, dir);
                dvec3 compM2 = -dir * dot(m2.direction, -dir);
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
    if (!_renderableInView)
        return;
    else
        _renderableInView = false;

    double t_cur= data.time.j2000Seconds();
    double dt = t_cur - data.previousFrameTime.j2000Seconds();
    
    for (molecule_data_t& mol : _molecules) {
        // update animation
        if (mol.trajectory) {
            // Emulate PingPong animation by manipulating the local time t_local per trajectory
            const double t_dur = static_cast<double>(MAX(0, md_trajectory_num_frames(mol.trajectory) - 1));
            double t_loc = fract(t_cur / (2.0 * t_dur));
            double t = t_loc < 0.5 ? (t_loc * 2.0) : (1.0 - t_loc * 2.0);

            mol::util::interpolate_coords(mol.molecule, mol.trajectory, mol::util::InterpolationType::Cubic, t);
            md_gl_molecule_set_atom_position(&mol.drawMol, 0, uint32_t(mol.molecule.atom.count), mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z, 0);
        }
    
        // update simulation
        updateSimulation(mol, dt * _simulationSpeed);
    }
}

static double normalizeDouble(double input) {
    if (input > 1.0) {
        return input / pow(10, 30);
    } else {
        return input - 1.0;
    }
}

static mat4_t mat4_from_glm(glm::mat4 const& src) {
    mat4_t dst;
    memcpy(&dst, &src, 4 * 4 * sizeof(float));
    return dst;
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks&) {
    global::renderEngine->openglStateCache().loadCurrentGLState();

    using namespace glm;
    const dmat4 I(1.0);
    
    // compute distance from camera to molecule
    vec3 forward = data.modelTransform.translation - data.camera.positionVec3();
    vec3 dir = data.camera.viewDirectionWorldSpace();
    float distance = length(forward) * sign(dot(dir, forward)); // "signed" distance from camera to object.
    // we apply artificial scaling to everything to cheat a bit with the unit system:
    float fakeScaling = 100.f / distance;

    if (distance < 0.f || distance > 1E4) // distance < 0 means behind the camera, 1E4 is arbitrary.
        return;
    else
        _renderableInView = true;

    // because the molecule is small, a scaling of the view matrix causes the molecule
    // to be moved out of view in clip space. Resetting the scaling for the molecule
    // is fine for now. This will have an impact on stereoscopic depth though.
    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        translate(I, data.modelTransform.translation) *
        scale(I, data.modelTransform.scale) *
        dmat4(data.modelTransform.rotation) *
        scale(I, dvec3(fakeScaling)) *
        translate(I, -_simulationBox.value() / 2.0) *
        I;

    mat4 projMatrix =
        dmat4(camCopy.sgctInternal.projectionMatrix()) *
        I;

    // We want to preallocate this to avoid reallocations
    size_t count = 0;
    for (const auto& mol : _molecules) {
        count += mol.states.size();
    }

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve(count);

    std::vector<mat4> transforms;
    transforms.reserve(count);

    for (const molecule_data_t& mol : _molecules) {
        for (size_t i = 0; i < mol.states.size(); i++) {
            const molecule_state_t& state = mol.states[i];
            dmat4 transform =
                translate(dmat4(1.0), state.position) *
                rotate(dmat4(1.0), state.angle, state.rotationAxis) *
                dmat4(1.0);

            transforms.push_back(mat4(transform));

            md_gl_draw_op_t drawOp = {};
            drawOp.model_matrix = value_ptr(transforms.back()); // This is only safe, because we store it in a vector and preallocate the memory
            drawOp.rep = &mol.drawRep;
            drawOps.push_back(drawOp);
        }
    }

    md_gl_draw_args_t args = {};
    args.shaders = &shaders;
    args.view_transform = {
        value_ptr(viewMatrix),
        value_ptr(projMatrix),
    };
    args.options = 0;
        
    args.draw_operations = {
        static_cast<uint32_t>(drawOps.size()),
        drawOps.data(),
    };
    
    { // draw molecule offscreen
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        // shading rendering of mold
        const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, bufs);

        glClearColor(0.f, 0.f, 0.f, 1.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        // resize the fbo if needed
        if (global::windowDelegate->windowHasResized()) {
            ivec2 size = global::windowDelegate->currentWindowSize();
            glBindTexture(GL_TEXTURE_2D, colorTex);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, size.x, size.y, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
            glBindTexture(GL_TEXTURE_2D, 0);

            glBindTexture(GL_TEXTURE_2D, normalTex);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16, size.x, size.y, 0, GL_RG, GL_UNSIGNED_SHORT, nullptr);
            glBindTexture(GL_TEXTURE_2D, 0);

            glBindTexture(GL_TEXTURE_2D, depthTex);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
            glBindTexture(GL_TEXTURE_2D, 0);
        }
        
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        md_gl_draw(&args);
        
        { // postprocessing
            postprocessing::Settings settings;
            settings.background.intensity = {{ 0, 0, 0 }};
            settings.ambient_occlusion.enabled = _ssaoEnabled;
            settings.ambient_occlusion.intensity = _ssaoIntensity;
            settings.ambient_occlusion.radius = _ssaoRadius;
            settings.ambient_occlusion.bias = _ssaoBias;
            settings.bloom.enabled = false;
            settings.depth_of_field.enabled = false;
            settings.temporal_reprojection.enabled = false;
            settings.tonemapping.enabled = true;
            settings.tonemapping.mode = postprocessing::Tonemapping::ACES;
            settings.tonemapping.exposure = _exposure;
            settings.input_textures.depth = depthTex;
            settings.input_textures.color = colorTex;
            settings.input_textures.normal = normalTex;

            postprocessing::postprocess(settings, mat4_from_glm(projMatrix));

            glEnable(GL_DEPTH_TEST); // restore state after postprocess
            glDisable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        }

        glDrawBuffer(GL_FRONT_AND_BACK);
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    }

    { // draw billboard pre-rendered with molecule inside
        dmat4 billboardModel = 
            camCopy.combinedViewMatrix() *
            translate(I, data.modelTransform.translation) *
            scale(I, data.modelTransform.scale) *
            scale(I, dvec3(_simulationBox)) *
            scale(I, dvec3(1.0)) * // billboard circle radius (wrt. simbox)
            scale(I, dvec3(fakeScaling)) *
            I;
        mat4 faceCamera = inverse(camCopy.viewRotationMatrix());
        mat4 transform = projMatrix * mat4(billboardModel) * faceCamera;
        double circleWidth = distance / compMax(_simulationBox.value() * data.modelTransform.scale) * 1E-2;

        dvec4 depth_ = dmat4(data.camera.sgctInternal.projectionMatrix()) * billboardModel * dvec4(0.0, 0.0, 0.0, 1.0);
        double depth = normalizeDouble(depth_.w);

        billboardDraw(transform, colorTex, depthTex, vec4(1.0), static_cast<float>(circleWidth), static_cast<float>(depth));
    }

    global::renderEngine->openglStateCache().resetBlendState();
}

void RenderableSimulationBox::initMolecule(molecule_data_t& mol, std::string_view molFile, std::string_view trajFile) {
    LDEBUG(fmt::format("Loading molecule file '{}'", molFile));

    // free previously loaded molecule
    freeMolecule(mol);

    const md_molecule_t* molecule = mol_manager::get_molecule(molFile);
    if (!molecule) {
        return;
    }

    // We don't really append per-se, we use this to perform a deep copy of the molecule from the manager so we can modify its state (coordinates).
    md_molecule_append(&mol.molecule, molecule, default_allocator);

    if (!trajFile.empty() && trajFile != "") {
        LDEBUG(fmt::format("Loading trajectory file '{}'", trajFile));
        mol.trajectory = mol_manager::get_trajectory(trajFile);

        if (!mol.trajectory) {
            LERROR("failed to initialize trajectory: failed to load file");
            return;
        }
    }
    
    double sphere = glm::compMax(_simulationBox.value()) / 2.0;
    setBoundingSphere(sphere);

    md_gl_molecule_init(&mol.drawMol, &mol.molecule);
    md_gl_representation_init(&mol.drawRep, &mol.drawMol);
    
    mol::util::update_rep_type(mol.drawRep, static_cast<mol::rep::Type>(_repType.value()), _repScale);
    mol::util::update_rep_color(mol.drawRep, mol.molecule, static_cast<mol::rep::Color>(_coloring.value()), _viamdFilter);
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
