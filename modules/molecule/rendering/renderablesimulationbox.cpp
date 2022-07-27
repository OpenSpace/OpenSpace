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

#include "../md_concat.h"
#include "billboard.h"

#include "glbinding/gl/bitfield.h"
#include "glbinding/gl/enum.h"
#include "glbinding/gl/functions.h"
#include <md_util.h>
#include <core/md_array.inl>
#include <md_script.h>
#include <md_filter.h>
#include "viamd/coloring.h"
#include "viamd/loader.h"

#include <glm/gtc/random.hpp>

#include <ghoul/logging/logmanager.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/httprequest.h>

//#include <openspace/rendering/renderengine.h>
//#include <openspace/util/boxgeometry.h>
//#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
//#include <openspace/engine/downloadmanager.h>

#include <md_pdb.h>
#include <md_gro.h>
#include <md_xyz.h>
#include <md_frame_cache.h>
#include <core/md_allocator.h>

// COMBAK: because my ide complains
#ifndef ZoneScoped
#define ZoneScoped
#endif

using namespace glm;

constexpr const char* shader_output_snippet = R"(
layout(location = 0) out vec4 out_color;

// this is a basic blinn-phong taken from learnopengl.com.

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
    vec3 viewPos = vec3(0.0, 0.0, 0.0); // in view space the camera is at origin.
    vec3 lightPos = viewPos; // place the light on the camera.

    // ambient
    vec3 ambient = 0.05 * color.rgb;

    // diffuse
    vec3 lightDir = normalize(lightPos - view_coord);
    vec3 normal = normalize(view_normal);
    float diff = max(dot(lightDir, normal), 0.0);
    vec3 diffuse = diff * color.rgb;

    // specular
    vec3 viewDir = normalize(viewPos - view_coord);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float spec = pow(max(dot(normal, halfwayDir), 0.0), 32.0);
    vec3 specular = vec3(0.3) * spec; // assuming bright white light color

    out_color = vec4(ambient + diffuse + specular, color.a);
}
)";

namespace {
    constexpr const char* _loggerCat = "RenderableSimulationBox";

    enum class RepresentationType {
        SpaceFill = MD_GL_REP_SPACE_FILL,
        Ribbons = MD_GL_REP_RIBBONS,
        Cartoon = MD_GL_REP_CARTOON,
        Licorice = MD_GL_REP_LICORICE,
    };

    enum class Coloring {
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

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        // [[codegen::verbatim(MoleculeFilesInfo.description)]]
        std::vector<std::string> moleculeFiles;

        // [[codegen::verbatim(TrajectoryFilesInfo.description)]]
        std::vector<std::string> trajectoryFiles;

        enum class [[codegen::map(RepresentationType)]] RepresentationType {
            SpaceFill,
            Ribbons,
            Cartoon,
            Licorice,
        };

        // [[codegen::verbatim(RepTypeInfo.description)]]
        std::optional<RepresentationType> repType;

        enum class [[codegen::map(Coloring)]] Coloring {
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
    };

#include "renderablesimulationbox_codegen.cpp"
}

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
    _animationSpeed(AnimationSpeedInfo, 1.f, 0.f, 1000.f),
    _simulationSpeed(SimulationSpeedInfo, 1.f, 0.f, 1000.f),
    _moleculeCounts(MoleculeCountsInfo),
    _linearVelocity(LinearVelocityInfo),
    _angularVelocity(AngularVelocityInfo),
    _simulationBox(SimulationBoxInfo),
    _collisionRadius(CollisionRadiusInfo)
{
    _repType.addOptions({
        { static_cast<int>(RepresentationType::SpaceFill), "Space Fill" },
        { static_cast<int>(RepresentationType::Ribbons), "Ribbons" },
        { static_cast<int>(RepresentationType::Cartoon), "Cartoon" },
        { static_cast<int>(RepresentationType::Licorice), "Licorice" },
    });
    
    _coloring.addOptions({
        { static_cast<int>(Coloring::Cpk), "CPK" },
        { static_cast<int>(Coloring::AtomIndex), "Atom Index" },
        { static_cast<int>(Coloring::ResId), "Residue ID" },
        { static_cast<int>(Coloring::ResIndex), "Residue Index" },
        { static_cast<int>(Coloring::ChainId), "Chain ID" },
        { static_cast<int>(Coloring::ChainIndex), "Chain Index" },
        { static_cast<int>(Coloring::SecondaryStructure), "Secondary Structure" },
    });

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFiles = p.moleculeFiles;
    _trajectoryFiles = p.trajectoryFiles;
    _simulationSpeed = p.simulationSpeed.value_or(1.f);
    _moleculeCounts = p.moleculeCounts;
    _linearVelocity = p.linearVelocity;
    _angularVelocity = p.angularVelocity;
    _simulationBox = p.simulationBox;
    _collisionRadius = p.collisionRadius;

    if (p.repType.has_value()) {
        _repType = static_cast<int>(codegen::map<RepresentationType>(*p.repType));
    } else {
        _repType = static_cast<int>(RepresentationType::SpaceFill);
    }
    
    if (p.coloring.has_value()) {
        _coloring = static_cast<int>(codegen::map<Coloring>(*p.coloring));
    } else {
        _coloring = static_cast<int>(Coloring::Cpk);
    }
    
    _repScale = p.repScale.value_or(1.f);
    _animationSpeed = p.animationSpeed.value_or(1.f);
    
    for (int count : _moleculeCounts.value()) {
        molecule_data_t mol {
            {},      // states
            nullptr, //moleculeApi
            nullptr, //trajectoryApi
            {},      //molecule
            {},      //concatMolecule
            nullptr, //trajectory
            {},      //drawRep
            {},      //drawMol
        };
        
        for (int i = 0; i < count; i++) {
            molecule_state_t demoMolecule {
                linearRand(dvec3(0.0), _simulationBox.value()),  // position
                0.0,                                             // angle
                sphericalRand(_linearVelocity.value()),          // direction
                sphericalRand(_angularVelocity.value()),         // rotation
            };
            mol.states.push_back(demoMolecule);
        }
        
        _molecules.push_back(mol);
    }
    
    _repScale.onChange([this] () {
        for (molecule_data_t& mol: _molecules) {
            if (mol.moleculeApi) {
                updateRepresentation(mol);
            }
        }
    });

    addProperty(_repType);
    addProperty(_coloring);
    addProperty(_repScale);
    addProperty(_animationSpeed);
    addProperty(_simulationBox);
    addProperty(_simulationSpeed);
}

RenderableSimulationBox::~RenderableSimulationBox() {
    for (molecule_data_t& mol : _molecules) {
        if (mol.moleculeApi)
            freeMolecule(mol);
        if (mol.trajectoryApi)
            freeTrajectory(mol);
    }
}

void RenderableSimulationBox::initialize() {
    ZoneScoped
}

void RenderableSimulationBox::initializeGL() {
    ZoneScoped
    md_gl_initialize();
    md_gl_shaders_init(&_shaders, shader_output_snippet);
    billboardGlInit();

    std::string viamdFilter =
        "not(element('O'))";

    size_t i = 0;
    for (molecule_data_t& mol : _molecules) {
        std::string molFile = _moleculeFiles.value().at(i);
        std::string trajFile = _trajectoryFiles.value().at(i);

        initMolecule(mol, molFile);
        if (trajFile != "")
            initTrajectory(mol, trajFile);

        if (mol.trajectory)
            applyViamdFilter(mol, viamdFilter);

        i++;
    }
    
}

void RenderableSimulationBox::deinitializeGL() {
    billboardGlDeinit();
    md_gl_shaders_free(&_shaders);
    md_gl_shutdown();
}

bool RenderableSimulationBox::isReady() const {
    return true;
}
void RenderableSimulationBox::updateAnimation(molecule_data_t& mol, double time) {
    int64_t nFrames = md_trajectory_num_frames(mol.trajectory);
    if (nFrames >= 4) {
        double t = fract(time);
        int64_t frames[4];

        // The animation is played forward and back (bouncing), the first and last
        // frame are repeated.

        if ((int64_t(time) / nFrames) % 2 == 0) { // animation forward
            int64_t frame = int64_t(time) % nFrames;
            if (frame < 0) frame += nFrames;
            frames[0] = std::max<int64_t>(0, frame - 1);
            frames[1] = frame;
            frames[2] = std::min<int64_t>(nFrames - 1, frame + 1);
            frames[3] = std::min<int64_t>(nFrames - 1, frame + 2);
        }
        else { // animation backward
            t = 1.0 - t;
            int64_t frame = nFrames - 1 - (int64_t(time) % nFrames);
            if (frame < 0) frame += nFrames;
            frames[0] = std::max<int64_t>(0, frame - 2);
            frames[1] = std::max<int64_t>(0, frame - 1);
            frames[2] = frame;
            frames[3] = std::min<int64_t>(nFrames - 1, frame + 1);
        }

        // nearest
        // mdtraj_frame_header_t header{};
        // mdtraj_load_frame(traj, frame, &header, mol.atom.x, mol.atom.y, mol.atom.z);
        // md_gl_molecule_set_atom_position(&_drawMol, 0,static_cast<uint32_t>(mol.atom.count), mol.atom.x, mol.atom.y, mol.atom.z, 0);
        
        // cubic
        md_trajectory_frame_header_t header[4];
        mat3_t boxes[4];
        int64_t stride = ROUND_UP(mol.molecule.atom.count, md_simd_widthf);    // The interploation uses SIMD vectorization without bounds, so we make sure there is no overlap between the data segments
        int64_t bytes = stride * sizeof(float) * 3 * 4;
        float* mem = static_cast<float*>(malloc(bytes));
        {
            md_vec3_soa_t src[4] = {
                {mem + stride * 0, mem + stride *  1, mem + stride *  2},
                {mem + stride * 3, mem + stride *  4, mem + stride *  5},
                {mem + stride * 6, mem + stride *  7, mem + stride *  8},
                {mem + stride * 9, mem + stride * 10, mem + stride * 11},
            };
            md_vec3_soa_t dst = {
                mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z,
            };

            md_trajectory_load_frame(mol.trajectory, frames[0], &header[0], src[0].x, src[0].y, src[0].z);
            md_trajectory_load_frame(mol.trajectory, frames[1], &header[1], src[1].x, src[1].y, src[1].z);
            md_trajectory_load_frame(mol.trajectory, frames[2], &header[2], src[2].x, src[2].y, src[2].z);
            md_trajectory_load_frame(mol.trajectory, frames[3], &header[3], src[3].x, src[3].y, src[3].z);

            memcpy(&boxes[0], header[0].box, sizeof(boxes[0]));
            memcpy(&boxes[1], header[1].box, sizeof(boxes[1]));
            memcpy(&boxes[2], header[2].box, sizeof(boxes[2]));
            memcpy(&boxes[3], header[3].box, sizeof(boxes[3]));
            mat3_t box = cubic_spline(boxes[0], boxes[1], boxes[2], boxes[3], t, 1.0);
            vec3_t pbc_ext = box * vec3_t{{1.0, 1.0, 1.0}};
            
            md_util_cubic_interpolation(dst, src, mol.molecule.atom.count, pbc_ext, t, 1.0f);
        }
        free(mem);
    }
    else {
        LERROR("Molecule trajectory contains less than 4 frames. Cannot interpolate.");
    }
}

void RenderableSimulationBox::applyTransforms() {
    for (const molecule_data_t& mol : _molecules) {
        for (size_t i = 0; i < mol.states.size(); i++) {
            const molecule_state_t& state = mol.states[i];
            dmat4 transform =
                translate(dmat4(1.0), state.position) *
                rotate(dmat4(1.0), state.angle, state.rotation) *
                dmat4(1.0);
        
            for (int j = 0; j < mol.molecule.atom.count; j++) {
                dvec4 pos(*(mol.molecule.atom.x + j), *(mol.molecule.atom.y + j), *(mol.molecule.atom.z + j), 1.0);
                pos = transform * pos;
                *(mol.concatMolecule.atom.x + (i * mol.molecule.atom.count) + j) = pos.x;
                *(mol.concatMolecule.atom.y + (i * mol.molecule.atom.count) + j) = pos.y;
                *(mol.concatMolecule.atom.z + (i * mol.molecule.atom.count) + j) = pos.z;
            }
        }
    }
}

void RenderableSimulationBox::updateSimulation(molecule_data_t& mol, double dt) {
    // update positions / rotations
    for (auto& molecule : mol.states) {
        molecule.position += molecule.direction * dt;
        molecule.position = mod(molecule.position, _simulationBox.value());
        molecule.angle += length(molecule.rotation) * dt;
    }

    double collRadiusSquared = _collisionRadius * _collisionRadius;

    // compute collisions
    for (auto it1 = mol.states.begin(); it1 != mol.states.end(); ++it1) {
        for (auto it2 = std::next(it1); it2 != mol.states.end(); ++it2) {

            molecule_state_t& m1 = *it1;
            molecule_state_t& m2 = *it2;

            double distSquared = dot(m1.position, m2.position);

            if (distSquared < collRadiusSquared) { // collision detected
                double dist = sqrt(distSquared);
                double intersection = 2.0 * _collisionRadius - dist;
                // swap the direction components normal to the collision plane from the 2
                // molecules. (simplistic elastic collision of 2 spheres with same mass)
                dvec3 dir = (m2.position - m1.position) / dist;
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
    double t = data.time.j2000Seconds();
    double dt = t - data.previousFrameTime.j2000Seconds();
    
    for (molecule_data_t& mol : _molecules) {
        // update animation
        if (mol.trajectoryApi) {
            updateAnimation(mol, t * _animationSpeed);
        }
    
        // update simulation
        updateSimulation(mol, dt * _simulationSpeed);
    
        // update gl repr
        applyTransforms();
        md_gl_molecule_set_atom_position(&mol.drawMol, 0, uint32_t(mol.concatMolecule.atom.count),
            mol.concatMolecule.atom.x, mol.concatMolecule.atom.y, mol.concatMolecule.atom.z, 0);
    }
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks&) {
    using namespace glm;
    const dmat4 I(1.0);
    
    // compute distance from camera to molecule
    // we apply artificial scaling to everything to cheat a bit with the unit system:
    // TODO explain
    float distance = length(data.modelTransform.translation - data.camera.positionVec3());
    float fakeScaling = 100.f / distance;

    // because the molecule is small, a scaling of the view matrix causes the molecule
    // to be moved out of view in clip space. Reset the scaling for the molecule
    // is fine for now.
    Camera camCopy = data.camera;
    camCopy.setScaling(1.f);

    // having the view matrix in the model matrix is better because
    // mold uses single precision but that's not enough
    mat4 modelMatrix =
        camCopy.combinedViewMatrix() *
        translate(I, data.modelTransform.translation) *
        scale(I, data.modelTransform.scale) *
        dmat4(data.modelTransform.rotation) *
        scale(I, dvec3(fakeScaling)) *
        translate(I, -_simulationBox.value() / 2.0) *
        I;

    mat4 viewMatrix =
        I;

    mat4 projMatrix =
        dmat4(camCopy.sgctInternal.projectionMatrix()) *
        I;

    std::vector<md_gl_draw_op_t> drawOps;
    for (molecule_data_t& mol : _molecules) {
        if (mol.concatMolecule.atom.count) {
            md_gl_draw_op_t drawOp = {};
            drawOp.model_matrix = value_ptr(modelMatrix);
            drawOp.rep = &mol.drawRep;
            drawOps.push_back(drawOp);
        }
    }

    md_gl_draw_args_t args = {};
    args.shaders = &_shaders;
    args.view_transform = {
        value_ptr(viewMatrix),
        value_ptr(projMatrix),
        nullptr, nullptr
    };
    args.atom_mask = 0;
    args.options = 0;
        
    args.draw_operations = {
        static_cast<uint32_t>(drawOps.size()),
        drawOps.data(),
    };

    { // draw billboard & molecule
        mat4 billboardModel = 
            camCopy.combinedViewMatrix() *
            translate(I, data.modelTransform.translation) *
            scale(I, data.modelTransform.scale) *
            scale(I, dvec3(_simulationBox)) *
            scale(I, dvec3(3.0)) * // billboard circle radius (wrt. simbox)
            scale(I, dvec3(fakeScaling)) *
            I;
        mat4 faceCamera = inverse(camCopy.viewRotationMatrix());
        mat4 transform = projMatrix * billboardModel * faceCamera;
        float circleWidth = distance / compMax(_simulationBox.value()) * 1E-2;

        billboardDraw(transform, circleWidth, 1.f); // write depth=1 (clear depth buffer) in billboard
        md_gl_draw(&args);                               // draw molecule
        billboardDraw(transform, circleWidth, 0.f); // write depth=0 (lock depth buffer) in billboard
    }

}

void RenderableSimulationBox::initMolecule(molecule_data_t& mol, const std::string& file) {
    LDEBUG("Loading molecule file '" + file + "'");

    // free previously loaded molecule
    if (mol.moleculeApi) {
        freeMolecule(mol);
    }
    
    str_t molFileStr = str_from_cstr(file.data());
    mol.moleculeApi = load::mol::get_api(molFileStr);

    if (!mol.moleculeApi) {
        LERROR("failed to initialize molecule: unknown file type");
        return;
    }

    bool init = mol.moleculeApi->init_from_file(&mol.molecule, molFileStr, default_allocator);
    
    if (!init) {
        LERROR("failed to initialize molecule: malformed file");
        mol.moleculeApi = nullptr;
        return;
    }
    
    // duplicate the molecule data n times. This is done for performance reasons, because
    // the molecule simulation can be treated as 1 big molecule with a single draw call.
    mol.concatMolecule = concat_molecule_init(&mol.molecule, mol.states.size(), default_allocator);
    
    double sphere = glm::compMax(_simulationBox.value()) / 2.0;
    setBoundingSphere(sphere);
    setInteractionSphere(sphere);

    md_gl_molecule_init(&mol.drawMol, &mol.concatMolecule);
    md_gl_representation_init(&mol.drawRep, &mol.drawMol);
    updateRepresentation(mol);
}

void RenderableSimulationBox::initTrajectory(molecule_data_t& mol, const std::string& file) {
    LDEBUG("Loading trajectory file '" + file + "'");
    
    // need to keep the string in scope to keep the str pointer valid
    str_t trajFileStr = str_from_cstr(file.data());

    mol.trajectoryApi = load::traj::get_api(trajFileStr);

    if (!mol.trajectoryApi) {
        LERROR("failed to initialize trajectory: unknown file type");
        return;
    }
    
    mol.trajectory = load::traj::open_file(trajFileStr, &mol.molecule, default_allocator);

    if (!mol.trajectory) {
        LERROR("failed to initialize trajectory: failed to load file");
        mol.trajectoryApi = nullptr;
        return;
    }
}

void RenderableSimulationBox::updateRepresentation(molecule_data_t& mol) {
    { // REPRESENTATION TYPE
        md_gl_representation_args_t rep_args{};
    
        switch (static_cast<RepresentationType>(_repType.value())) {
            case RepresentationType::SpaceFill:
                rep_args.space_fill.radius_scale = _repScale;
                break;
            case RepresentationType::Cartoon:
                rep_args.cartoon.width_scale = _repScale;
                rep_args.cartoon.thickness_scale = _repScale;
                break;
            case RepresentationType::Ribbons:
                rep_args.cartoon.width_scale = _repScale;
                rep_args.cartoon.thickness_scale = _repScale;
                break;
            case RepresentationType::Licorice:
                rep_args.licorice.radius = _repScale;
                break;
        }

        md_gl_representation_set_type_and_args(&mol.drawRep, _repType, rep_args);
    }
    { // COLORING
        uint32_t* colors = static_cast<uint32_t*>(md_alloc(default_temp_allocator, sizeof(uint32_t) * mol.concatMolecule.atom.count));
        uint32_t count = static_cast<uint32_t>(mol.concatMolecule.atom.count);

        switch (static_cast<Coloring>(_coloring.value())) {
            case Coloring::Cpk:
                color_atoms_cpk(colors, count, mol.concatMolecule);
                break;
            case Coloring::AtomIndex:
                color_atoms_idx(colors, count, mol.concatMolecule);
                break;
            case Coloring::ResId:
                color_atoms_residue_id(colors, count, mol.concatMolecule);
                break;
            case Coloring::ResIndex:
                color_atoms_residue_index(colors, count, mol.concatMolecule);
                break;
            case Coloring::ChainId:
                color_atoms_chain_id(colors, count, mol.concatMolecule);
                break;
            case Coloring::ChainIndex:
                color_atoms_chain_index(colors, count, mol.concatMolecule);
                break;
            case Coloring::SecondaryStructure:
                color_atoms_secondary_structure(colors, count, mol.concatMolecule);
                break;
            default:
                ghoul_assert(false, "unexpected molecule coloring");
                break;
        }

        md_gl_representation_set_color(&mol.drawRep, 0, static_cast<uint32_t>(mol.concatMolecule.atom.count), colors, 0);
    }
}

void RenderableSimulationBox::applyViamdFilter(molecule_data_t& mol, const std::string& filter) {
    str_t str = str_from_cstr(filter.data());
    md_filter_result_t res{};

    md_script_ir_t* filterIR = md_script_ir_create(default_temp_allocator);

    bool success = md_filter_evaluate(&res, str, &mol.molecule, filterIR, default_temp_allocator);
    
    if (success) {
        LINFO("Compiled filter");
        // md_bitfield_clear(mol.drawRep);
        // for (int64_t i = 0; i < res.num_bitfields; ++i) {
        //     md_bitfield_or_inplace(mask, &res.bitfields[i]);
        // }
    }

    else {
        LERROR("Failed to evaluate IR from source");
    }

    md_filter_free(&res, default_temp_allocator);
    md_script_ir_free(filterIR);
}

void RenderableSimulationBox::applyViamdScript(molecule_data_t& mol, const std::string& script) {
    str_t str = str_from_cstr(script.data());
    int numFrames = md_trajectory_num_frames(mol.trajectory);

    md_script_ir_t* selectionIr = md_script_ir_create(default_temp_allocator);
    md_script_ir_t* mainIr = md_script_ir_create(default_temp_allocator);
    md_script_eval_t* fullEval = md_script_eval_create(numFrames, mainIr, default_temp_allocator);
    // md_script_eval_t* filtEval = md_script_eval_create(numFrames, main_ir, default_temp_allocator);

    bool res = md_script_ir_compile_source(mainIr, str, &mol.molecule, selectionIr);
    
    if (res) {
        res = md_script_eval_compute(fullEval, mainIr, &mol.molecule, mol.trajectory, nullptr);
        // md_script_eval_compute(filtEval, main_ir, &mol.molecule, &mol.trajectory, nullptr);
    } else {
        LERROR("Failed to compile IR from source");
    }

    md_script_ir_free(mainIr);
    md_script_ir_free(selectionIr);
    md_script_eval_free(fullEval);
}

void RenderableSimulationBox::freeMolecule(molecule_data_t& mol) {
    md_gl_representation_free(&mol.drawRep);
    md_gl_molecule_free(&mol.drawMol);
    mol.moleculeApi->free(&mol.molecule, default_allocator);
    mol.moleculeApi = nullptr;
    concat_molecule_free(&mol.concatMolecule, default_allocator);
    mol.concatMolecule = {};
    mol.molecule = {};
    mol.drawMol = {};
    mol.drawRep = {};
}

void RenderableSimulationBox::freeTrajectory(molecule_data_t& mol) {
    mol.trajectoryApi->destroy(mol.trajectory);
    mol.trajectoryApi = nullptr;
    mol.trajectory = nullptr;
}

} // namespace openspace
