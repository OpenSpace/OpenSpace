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

#include "glbinding/gl/bitfield.h"
#include "glbinding/gl/functions.h"
#include "md_util.h"
#include "viamd/coloring.h"
#include "viamd/loader.h"
#include <modules/molecule/rendering/renderablemolecule.h>

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
    constexpr const char* _loggerCat = "RenderableMolecule";

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

    constexpr openspace::properties::Property::PropertyInfo MoleculeFileInfo = {
        "MoleculeFile",
        "Molecule File",
        "Molecule file path"
    };

    constexpr openspace::properties::Property::PropertyInfo TrajectoryFileInfo = {
        "TrajectoryFile",
        "Trajectory File",
        "Trajectory file path"
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

    constexpr openspace::properties::Property::PropertyInfo AnimSpeedInfo = {
        "AnimSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)"
    };

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::optional<std::string> trajectoryFile;

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

        // [[codegen::verbatim(AnimSpeedInfo.description)]]
        std::optional<float> animSpeed;
    };

#include "renderablemolecule_codegen.cpp"
}

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablemolecule");
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
    _deferredTask(GlDeferredTask::None),
    _moleculeApi(nullptr),
    _trajectoryApi(nullptr),
    _moleculeFile(MoleculeFileInfo),
    _trajectoryFile(TrajectoryFileInfo),
    _repType(RepTypeInfo),
    _coloring(ColoringInfo),
    _repScale(RepScaleInfo, 1.f, 0.1f, 10.f),
    _animSpeed(AnimSpeedInfo, 1.f, 0.1f, 1000.f)
{
    _molecule = {};
    _trajectory = {};
    _drawMol = {};
    _drawRep = {};
    
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

    _moleculeFile = p.moleculeFile;
    _trajectoryFile = p.trajectoryFile.value_or("");

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
    _animSpeed = p.animSpeed.value_or(1.f);

    const auto loadMolecule = [this]() {
        _deferredTask = GlDeferredTask::LoadMolecule;
    };

    _moleculeFile.onChange(loadMolecule);
    _trajectoryFile.onChange(loadMolecule);
    _repType.onChange(loadMolecule);
    loadMolecule();
    
    addProperty(_moleculeFile);
    addProperty(_trajectoryFile);
    addProperty(_repType);
    addProperty(_coloring);
    addProperty(_repScale);
    addProperty(_animSpeed);
}

RenderableMolecule::~RenderableMolecule() {
    freeMolecule();
    freeTrajectory();
}

void RenderableMolecule::initialize() {
    ZoneScoped
}

void RenderableMolecule::initializeGL() {
    ZoneScoped
    md_gl_initialize();
    md_gl_shaders_init(&_shaders, shader_output_snippet);
}

void RenderableMolecule::deinitializeGL() {
    md_gl_shaders_free(&_shaders);
    md_gl_shutdown();
}

bool RenderableMolecule::isReady() const {
    return true;
}

void RenderableMolecule::update(const UpdateData& data) {
    // This is a dirty hack to load molecules in a gl context.
    switch (_deferredTask) {
    case GlDeferredTask::None:
        break;
    case GlDeferredTask::LoadMolecule:
        initMolecule();
        if (_trajectoryFile.value() != "")
            initTrajectory();
        break;
    }
    _deferredTask = GlDeferredTask::None;
    
    // update animation
    if (_trajectoryApi) {
        int64_t nFrames = md_trajectory_num_frames(_trajectory);
        if (nFrames >= 4) {
            double time = data.time.j2000Seconds() * _animSpeed;
            double t = fract(time);
            int64_t frames[4];

            // The animation is played forward and back (bouncing), the first and last
            // frame are repeated.

            if ((int64_t(time) / nFrames) % 2 == 0) { // animation forward
                int64_t frame = int64_t(time) % nFrames;
                if (frame < 0) frame += nFrames;
                frames[0] = std::max(0l, frame - 1);
                frames[1] = frame;
                frames[2] = std::min(nFrames - 1, frame + 1);
                frames[3] = std::min(nFrames - 1, frame + 2);
            }
            else { // animation backward
                t = 1.0 - t;
                int64_t frame = nFrames - 1 - (int64_t(time) % nFrames);
                if (frame < 0) frame += nFrames;
                frames[0] = std::max(0l, frame - 2);
                frames[1] = std::max(0l, frame - 1);
                frames[2] = frame;
                frames[3] = std::min(nFrames - 1, frame + 1);
            }

            // nearest
            // md_trajectory_frame_header_t header{};
            // md_trajectory_load_frame(_trajectory, frame, &header, _molecule.atom.x, _molecule.atom.y, _molecule.atom.z);
            // md_gl_molecule_set_atom_position(&_drawMol, 0,static_cast<uint32_t>(_molecule.atom.count), _molecule.atom.x, _molecule.atom.y, _molecule.atom.z, 0);
            
            // cubic
            md_trajectory_frame_header_t header[4];
            mat3_t boxes[4];
            int64_t stride = ROUND_UP(_molecule.atom.count, md_simd_widthf);    // The interploation uses SIMD vectorization without bounds, so we make sure there is no overlap between the data segments
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
                    _molecule.atom.x, _molecule.atom.y, _molecule.atom.z,
                };

                md_trajectory_load_frame(_trajectory, frames[0], &header[0], src[0].x, src[0].y, src[0].z);
                md_trajectory_load_frame(_trajectory, frames[1], &header[1], src[1].x, src[1].y, src[1].z);
                md_trajectory_load_frame(_trajectory, frames[2], &header[2], src[2].x, src[2].y, src[2].z);
                md_trajectory_load_frame(_trajectory, frames[3], &header[3], src[3].x, src[3].y, src[3].z);

                memcpy(&boxes[0], header[0].box, sizeof(boxes[0]));
                memcpy(&boxes[1], header[1].box, sizeof(boxes[1]));
                memcpy(&boxes[2], header[2].box, sizeof(boxes[2]));
                memcpy(&boxes[3], header[3].box, sizeof(boxes[3]));
                mat3_t box = cubic_spline(boxes[0], boxes[1], boxes[2], boxes[3], t, 1.0);
                vec3_t pbc_ext = box * vec3_t{{1.0, 1.0, 1.0}};

                md_util_cubic_interpolation(dst, src, _molecule.atom.count, pbc_ext, t, 1.0f);
            }
            free(mem);
            md_gl_molecule_set_atom_position(&_drawMol, 0, uint32_t(_molecule.atom.count), _molecule.atom.x, _molecule.atom.y, _molecule.atom.z, 0);
        }
        else {
            LERROR("Molecule trajectory contains less than 4 frames. Cannot interpolate.");
        }
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
    using namespace glm;
    const dmat4 I(1.0);

    if (_molecule.atom.count) {
        
        // _repScale = data.modelTransform.scale.x;

        // zoom out and move closer the camera
        dmat4 camView =
            translate(I, {0.f, 0.f, -100.f}) *
            scale(I, dvec3(1.0 / 215.0)) *
            translate(I, { 0, 0, 21500 }) *
            data.camera.combinedViewMatrix() *
            I;
        
        // having the view matrix in the model matrix is better because
        // mold uses single precision but that's not enough
        mat4 model_matrix =
            camView *
            translate(I, data.modelTransform.translation) *
            scale(I, data.modelTransform.scale) *
            dmat4(data.modelTransform.rotation) *
            translate(I, -dvec3(_center)) *
            I;
        
        mat4 view_matrix =
            I;

        mat4 proj_matrix =
            dmat4(data.camera.sgctInternal.projectionMatrix()) *
            I;

        md_gl_draw_op_t draw_op = {};

        draw_op.rep = &_drawRep;
        draw_op.model_matrix = value_ptr(model_matrix);

        md_gl_draw_args_t args = {};
        args.shaders = &_shaders;
        args.draw_operations = {
            1,
            &draw_op,
        };
        args.view_transform = {
            value_ptr(view_matrix),
            value_ptr(proj_matrix),
            nullptr, nullptr
        };
        args.atom_mask = 0;
        args.options = 0;
        
        md_gl_draw(&args);
    }
}

float RenderableMolecule::computeRadius() {
    glm::vec3 min_aabb {FLT_MAX};
    glm::vec3 max_aabb {-FLT_MAX};

    for (int64_t i = 0; i < _molecule.atom.count; ++i) {
        glm::vec3 p {_molecule.atom.x[i], _molecule.atom.y[i], _molecule.atom.z[i]};
        min_aabb = glm::min(min_aabb, p);
        max_aabb = glm::max(max_aabb, p);
    }

    _extent = max_aabb - min_aabb;
    _center = (min_aabb + max_aabb) * 0.5f;
    float radius = glm::compMax(_extent) * 0.5f;
    return radius;
}

void RenderableMolecule::initMolecule() {
    LDEBUG("Loading molecule file '" + _trajectoryFile.value() + "'");

    // set the updateRepresentation hook once, when the molecule is first initialized
    if (!_moleculeApi) {
        const auto updateRep = [this]() { updateRepresentation(); };
        _repType.onChange(updateRep);
        _repScale.onChange(updateRep);
        _coloring.onChange(updateRep);
    }
    
    // free previously loaded molecule
    else {
        freeMolecule();
    }

    // need to keep the string in scope to keep the str pointer valid
    std::string molFile = _moleculeFile.value();
    str_t molFileStr = str_from_cstr(molFile.data());
    
    _moleculeApi = load::mol::get_api(molFileStr);

    if (!_moleculeApi) {
        LERROR("failed to initialize molecule: unknown file type");
        return;
    }

    _moleculeApi->init_from_file(&_molecule, molFileStr, default_allocator);

    float radius = computeRadius();
    setBoundingSphere(radius);
    setInteractionSphere(radius);

    md_gl_molecule_init(&_drawMol, &_molecule);
    md_gl_representation_init(&_drawRep, &_drawMol);
    updateRepresentation();
}

void RenderableMolecule::initTrajectory() {
    LDEBUG("Loading trajectory file '" + _trajectoryFile.value() + "'");
    
    // need to keep the string in scope to keep the str pointer valid
    std::string trajFile = _trajectoryFile.value();
    str_t trajFileStr = str_from_cstr(trajFile.data());

    _trajectoryApi = load::traj::get_api(trajFileStr);

    if (!_trajectoryApi) {
        LERROR("failed to initialize trajectory: unknown file type");
        return;
    }
    
    _trajectory = load::traj::open_file(trajFileStr, &_molecule, default_allocator);

    if (!_trajectory) {
        LERROR("failed to initialize trajectory: failed to load file");
        return;
    }
}

void RenderableMolecule::updateRepresentation() {
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

        md_gl_representation_set_type_and_args(&_drawRep, _repType, rep_args);
    }
    { // COLORING
        uint32_t* colors = static_cast<uint32_t*>(md_alloc(default_temp_allocator, sizeof(uint32_t) * _molecule.atom.count));
        uint32_t count = static_cast<uint32_t>(_molecule.atom.count);

        switch (static_cast<Coloring>(_coloring.value())) {
            case Coloring::Cpk:
                color_atoms_cpk(colors, count, _molecule);
                break;
            case Coloring::AtomIndex:
                color_atoms_idx(colors, count, _molecule);
                break;
            case Coloring::ResId:
                color_atoms_residue_id(colors, count, _molecule);
                break;
            case Coloring::ResIndex:
                color_atoms_residue_index(colors, count, _molecule);
                break;
            case Coloring::ChainId:
                color_atoms_chain_id(colors, count, _molecule);
                break;
            case Coloring::ChainIndex:
                color_atoms_chain_index(colors, count, _molecule);
                break;
            case Coloring::SecondaryStructure:
                color_atoms_secondary_structure(colors, count, _molecule);
                break;
            default:
                ghoul_assert(false, "unexpected molecule coloring");
                break;
        }

        md_gl_representation_set_color(&_drawRep, 0, static_cast<uint32_t>(_molecule.atom.count), colors, 0);
    }
}

void RenderableMolecule::freeMolecule() {
    md_gl_representation_free(&_drawRep);
    md_gl_molecule_free(&_drawMol);
    _moleculeApi->free(&_molecule, default_allocator);
    _moleculeApi = nullptr;
    _molecule = {};
    _drawMol = {};
    _drawRep = {};
}

void RenderableMolecule::freeTrajectory() {
    _trajectoryApi->destroy(_trajectory);
    _trajectoryApi = nullptr;
    _trajectory = nullptr;
}

} // namespace openspace
