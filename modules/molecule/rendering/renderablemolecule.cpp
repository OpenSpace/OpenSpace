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

#include "renderablemolecule.h"

#include "billboard.h"

#include "glbinding/gl/bitfield.h"
#include "glbinding/gl/enum.h"
#include "glbinding/gl/functions.h"
#include <md_util.h>
#include <core/md_array.inl>
#include <md_script.h>
#include <md_filter.h>
#include "openspace/engine/windowdelegate.h"
#include "viamd/coloring.h"
#include "viamd/loader.h"
#include "viamd/gfx/conetracing_utils.h"
#include "viamd/gfx/postprocessing_utils.h"

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
using namespace gl;

constexpr const char* shader_output_snippet = R"(
layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_normal;

vec2 encode_normal (vec3 n) {
   float p = sqrt(n.z * 8 + 8);
   return n.xy / p + 0.5;
}

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
    out_normal = vec4(encode_normal(view_normal), 0, 0);
    // out_color = color;
}
)";

static void compute_aabb(vec3_t* aabb_min, vec3_t* aabb_max, const float* x, const float* y, const float* z, int64_t count) {
    ASSERT(count >= 0);

    if (count < 1) {
        *aabb_min = *aabb_max = {{0, 0, 0}};
        return;
    }

    *aabb_min = {{x[0], y[0], z[0]}};
    *aabb_max = {{x[0], y[0], z[0]}};
    for (int64_t i = 1; i < count; ++i) {
        aabb_min->x = MIN(aabb_min->x, x[i]);
        aabb_max->x = MAX(aabb_max->x, x[i]);
        aabb_min->y = MIN(aabb_min->y, y[i]);
        aabb_max->y = MAX(aabb_max->y, y[i]);
        aabb_min->z = MIN(aabb_min->z, z[i]);
        aabb_max->z = MAX(aabb_max->z, z[i]);
    }
}
enum AtomBit {
    AtomBitVisible = 0x4,
};

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

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)"
    };

    constexpr openspace::properties::Property::PropertyInfo ViamdFilterInfo = {
        "ViamdFilter",
        "Viamd Filter",
        "VIA-MD script filter for atom visibility"
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

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::string trajectoryFile;

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

        // [[codegen::verbatim(ViamdFilterInfo.description)]]
        std::optional<std::string> viamdFilter;
        
        // [[codegen::verbatim(SSAOEnabledInfo.description)]]
        std::optional<bool> ssaoEnabled;
        
        // [[codegen::verbatim(SSAOIntensityInfo.description)]]
        std::optional<float> ssaoIntensity;
        
        // [[codegen::verbatim(SSAORadiusInfo.description)]]
        std::optional<float> ssaoRadius;
    };

#include "renderablemolecule_codegen.cpp"
}

static GLuint fbo = 0;
static GLuint colorTex = 0;
static GLuint normalTex = 0;
static GLuint depthTex = 0;
static int glUseCount = 0;
static md_gl_shaders_t shaders;

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablemolecule");
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
    _moleculeFile(MoleculeFileInfo),
    _trajectoryFile(TrajectoryFileInfo),
    _repType(RepTypeInfo),
    _coloring(ColoringInfo),
    _repScale(RepScaleInfo, 1.f, 0.1f, 10.f),
    _animationSpeed(AnimationSpeedInfo, 1.f, 0.f, 100.f),
    _viamdFilter(ViamdFilterInfo),
    _ssaoEnabled(SSAOEnabledInfo),
    _ssaoIntensity(SSAOIntensityInfo, 12.f, 0.f, 100.f),
    _ssaoRadius(SSAORadiusInfo, 12.f, 0.f, 100.f)
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

    _moleculeFile = p.moleculeFile;
    _trajectoryFile = p.trajectoryFile;
    _repScale = p.repScale.value_or(1.f);
    _animationSpeed = p.animationSpeed.value_or(1.f);
    _viamdFilter = p.viamdFilter.value_or("");
    _ssaoEnabled = p.ssaoEnabled.value_or(true);
    _ssaoIntensity = p.ssaoIntensity.value_or(12.f);
    _ssaoRadius = p.ssaoRadius.value_or(12.f);


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
    
    _molecule = molecule_data_t{
        {},      // extent
        {},      // center
        0.f,     // radius
        nullptr, // moleculeApi
        nullptr, // trajectoryApi
        {},      // molecule
        nullptr, // trajectory
        {},      // drawRep
        {},      // drawMol
        {},      // visibilityMask
        {},      // occupancy volume
    };
    
    auto onUpdateRepr = [this]() {
        if (_molecule.moleculeApi) {
            updateRepresentation(_molecule);
        }
    };
    
    _repType.onChange(onUpdateRepr);
    _coloring.onChange(onUpdateRepr);
    _repScale.onChange(onUpdateRepr);

    _viamdFilter.onChange([this] () {
        if (_molecule.moleculeApi) {
            applyViamdFilter(_molecule, _viamdFilter);
        }
    });
    
    addProperty(_repType);
    addProperty(_coloring);
    addProperty(_repScale);
    addProperty(_animationSpeed);
    addProperty(_viamdFilter);
    addProperty(_ssaoEnabled);
    addProperty(_ssaoIntensity);
    addProperty(_ssaoRadius);
}

RenderableMolecule::~RenderableMolecule() {
    if (_molecule.moleculeApi)
        freeMolecule(_molecule);
    if (_molecule.trajectoryApi)
        freeTrajectory(_molecule);
}

void RenderableMolecule::initialize() {
    ZoneScoped
}

void RenderableMolecule::initializeGL() {
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
        
        cone_trace::initialize();
        postprocessing::initialize(size.x, size.y);

        glBindFramebuffer(GL_FRAMEBUFFER, 0);

        billboardGlInit();
    }

    std::string molFile = _moleculeFile.value();
    std::string trajFile = _trajectoryFile.value();

    initMolecule(_molecule, molFile);
    if (trajFile != "")
        initTrajectory(_molecule, trajFile);

    applyViamdFilter(_molecule, _viamdFilter);

    glUseCount++;
}

void RenderableMolecule::deinitializeGL() {
    glUseCount--;
    if (glUseCount == 0 && fbo) {
        glDeleteTextures(1, &depthTex);
        glDeleteTextures(1, &normalTex);
        glDeleteTextures(1, &colorTex);
        glDeleteFramebuffers(1, &fbo);
        depthTex = 0;
        colorTex = 0;
        fbo = 0;
        cone_trace::shutdown();
        postprocessing::shutdown();
    }

    billboardGlDeinit();
    md_gl_shaders_free(&shaders);
    md_gl_shutdown();
}

bool RenderableMolecule::isReady() const {
    return true;
}
void RenderableMolecule::updateAnimation(molecule_data_t& mol, double time) {
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
            if (frame >= nFrames) frame -= nFrames;
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

void RenderableMolecule::update(const UpdateData& data) {
    // avoid updating if not in view, as it can be quite expensive.
    if (!_renderableInView)
        return;
    else
        _renderableInView = false;

    double t = data.time.j2000Seconds();
    
    // update animation
    if (_molecule.trajectoryApi) {
        updateAnimation(_molecule, t * _animationSpeed);
    }

    // update gl repr
    md_gl_molecule_set_atom_position(&_molecule.drawMol, 0, uint32_t(_molecule.molecule.atom.count),
        _molecule.molecule.atom.x, _molecule.molecule.atom.y, _molecule.molecule.atom.z, 0);
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

void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
    using namespace glm;
    const dmat4 I(1.0);
    
    // compute distance from camera to molecule
    vec3 forward = data.modelTransform.translation - data.camera.positionVec3();
    vec3 dir = data.camera.viewDirectionWorldSpace();
    float distance = length(forward) * sign(dot(dir, forward)); // "signed" distance from camera to object.

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
        translate(I, dvec3(-_molecule.center)) *
        I;

    mat4 projMatrix =
        dmat4(camCopy.sgctInternal.projectionMatrix()) *
        I;

    std::vector<md_gl_draw_op_t> drawOps;
    if (_molecule.molecule.atom.count) {
        // the modelMatrix was put in the viewMatrix to perform multiplication in
        // double precision (dmat4).
        mat4 modelMatrix(1.f);
        md_gl_draw_op_t drawOp = {};
        drawOp.model_matrix = value_ptr(modelMatrix);
        drawOp.rep = &_molecule.drawRep;
        drawOps.push_back(drawOp);
    }

    md_gl_draw_args_t args = {};
    args.shaders = &shaders;
    args.view_transform = {
        value_ptr(viewMatrix),
        value_ptr(projMatrix),
        nullptr, nullptr
    };
    args.atom_mask = AtomBitVisible;
    args.options = 0;
        
    args.draw_operations = {
        static_cast<uint32_t>(drawOps.size()),
        drawOps.data(),
    };
    
    { // draw molecule offscreen
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        // deferred rendering of mold
        const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, bufs);

        glClearColor(0.f, 0.f, 0.f, 1.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_DEPTH_TEST);
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
        
        md_gl_draw(&args);
        
        // conetracing
        // glDrawBuffer(GL_COLOR_ATTACHMENT0);
        // for (molecule_data_t& mol : _molecules) {

        //     cone_trace::free_volume(&mol.occupancyVolume);
        //     vec3_t min, max;
        //     compute_aabb(&min, &max, mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z, mol.molecule.atom.count);
        //     std::cout << min[0] << " " << min[1] << " " << min[2] << std::endl;
        //     std::cout << max[0] << " " << max[1] << " " << max[2] << std::endl;
        //     cone_trace::init_occlusion_volume(&mol.occupancyVolume, min, max, 8.0f);

        //     cone_trace::compute_occupancy_volume(
        //         mol.occupancyVolume, mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z,
        //         mol.molecule.atom.radius, mol.molecule.atom.count
        //     );

        //     cone_trace::render_directional_occlusion(
        //         depthTex, normalTex, mol.occupancyVolume,
        //         mat4_from_glm(viewMatrix), mat4_from_glm(projMatrix),
        //         1.0, 1.0
        //     );

        // }
        
        { // postprocessing
            postprocessing::Descriptor desc;
            desc.background.intensity = {{ 0, 0, 0 }};
            desc.ambient_occlusion.enabled = _ssaoEnabled;
            desc.ambient_occlusion.intensity = _ssaoIntensity;
            desc.ambient_occlusion.radius = _ssaoRadius;
            desc.bloom.enabled = false;
            desc.depth_of_field.enabled = false;
            desc.temporal_reprojection.enabled = false;
            desc.tonemapping.enabled = false;
            desc.input_textures.depth = depthTex;
            desc.input_textures.color = colorTex;
            desc.input_textures.normal = normalTex;

            ViewParam param;
            // camCopy.maxFov()
            param.clip_planes.near = 0.1f;
            param.clip_planes.far = 1000.f;
            // param.fov_y
            param.jitter.current = vec2_t{{ 0, 0 }};
            param.jitter.next = vec2_t{{ 0, 0 }};
            param.jitter.previous = vec2_t{{ 0, 0 }};
            param.matrix.current.proj = mat4_from_glm(projMatrix);
            param.matrix.current.proj_jittered = mat4_from_glm(projMatrix);
            // param.matrix.current.norm = mat4_transpose(mat4_inverse(param.matrix.current.proj));
            // param.matrix.current.view = 
            param.matrix.inverse.proj = mat4_inverse(param.matrix.current.proj);
            param.matrix.inverse.proj_jittered = mat4_inverse(param.matrix.current.proj_jittered);
            // param.resolution
            
            postprocessing::postprocess(desc, param);
            glEnable(GL_DEPTH_TEST); // restore state after postprocess
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        }

        glDrawBuffer(GL_FRONT_AND_BACK);
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    }

    { // draw billboard pre-rendered with molecule inside
        dvec3 sca = data.modelTransform.scale + dvec3(_molecule.radius * 2.f + _repScale * 2.f);
        sca *= 2.0; // COMBAK: why? molecule is sometimes slightly out of view otherwise
        dmat4 billboardModel = 
            camCopy.combinedViewMatrix() *
            translate(I, data.modelTransform.translation) *
            scale(I, sca) *
            I;
        mat4 faceCamera = inverse(camCopy.viewRotationMatrix());
        mat4 transform = projMatrix * mat4(billboardModel) * faceCamera;
        float circleWidth = distance / compMax(sca) * 1E-2;
        circleWidth = 0.f;

        dvec4 depth_ = dmat4(data.camera.sgctInternal.projectionMatrix()) * billboardModel * dvec4(0.0, 0.0, 0.0, 1.0);
        double depth = normalizeDouble(depth_.w);

        billboardDraw(transform, colorTex, depthTex, vec4(1.0), circleWidth, depth);
    }
}

void RenderableMolecule::computeAABB(molecule_data_t& mol) {
    glm::vec3 min_aabb {FLT_MAX};
    glm::vec3 max_aabb {-FLT_MAX};

    for (int64_t i = 0; i < mol.molecule.atom.count; ++i) {
        glm::vec3 p {mol.molecule.atom.x[i], mol.molecule.atom.y[i], mol.molecule.atom.z[i]};
        min_aabb = glm::min(min_aabb, p);
        max_aabb = glm::max(max_aabb, p);
    }
    
    mol.extent = max_aabb - min_aabb;
    mol.center = (min_aabb + max_aabb) * 0.5f;
    mol.radius = length(mol.extent) * 0.5f;
}

void RenderableMolecule::initMolecule(molecule_data_t& mol, const std::string& file) {
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
    
    md_bitfield_init(&mol.visibilityMask, default_allocator);
    computeAABB(mol);
    setBoundingSphere(mol.radius * 2.f);
    // setInteractionSphere(sphere);

    md_gl_molecule_init(&mol.drawMol, &mol.molecule);
    md_gl_representation_init(&mol.drawRep, &mol.drawMol);
    updateRepresentation(mol);

    vec3_t min, max;
    compute_aabb(&min, &max, mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z, mol.molecule.atom.count);

    // conetracing
    cone_trace::init_occlusion_volume(&mol.occupancyVolume, min, max, 8.0f);

    // vec3 box = _simulationBox.value();
    // cone_trace::init_occlusion_volume(&mol.occupancyVolume, { {0, 0, 0} }, {{ box.x, box.y, box.z }}, 8.0f);

    cone_trace::compute_occupancy_volume(
        mol.occupancyVolume, mol.molecule.atom.x, mol.molecule.atom.y, mol.molecule.atom.z,
        mol.molecule.atom.radius, mol.molecule.atom.count
    );
}

void RenderableMolecule::initTrajectory(molecule_data_t& mol, const std::string& file) {
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

void RenderableMolecule::updateRepresentation(molecule_data_t& mol) {
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
        uint32_t* colors = static_cast<uint32_t*>(md_alloc(default_temp_allocator, sizeof(uint32_t) * mol.molecule.atom.count));
        uint32_t count = static_cast<uint32_t>(mol.molecule.atom.count);

        switch (static_cast<Coloring>(_coloring.value())) {
            case Coloring::Cpk:
                color_atoms_cpk(colors, count, mol.molecule);
                break;
            case Coloring::AtomIndex:
                color_atoms_idx(colors, count, mol.molecule);
                break;
            case Coloring::ResId:
                color_atoms_residue_id(colors, count, mol.molecule);
                break;
            case Coloring::ResIndex:
                color_atoms_residue_index(colors, count, mol.molecule);
                break;
            case Coloring::ChainId:
                color_atoms_chain_id(colors, count, mol.molecule);
                break;
            case Coloring::ChainIndex:
                color_atoms_chain_index(colors, count, mol.molecule);
                break;
            case Coloring::SecondaryStructure:
                color_atoms_secondary_structure(colors, count, mol.molecule);
                break;
            default:
                ghoul_assert(false, "unexpected molecule coloring");
                break;
        }

        md_gl_representation_set_color(&mol.drawRep, 0, static_cast<uint32_t>(mol.molecule.atom.count), colors, 0);
    }
}

void RenderableMolecule::applyViamdFilter(molecule_data_t& mol, const std::string& filter) {
    md_bitfield_clear(&mol.visibilityMask);
    
    if (!mol.molecule.atom.flags)
        return;
    
    if (!filter.empty()) {
        str_t str = str_from_cstr(filter.data());
        md_filter_result_t res{};
        md_script_ir_t* filterIR = md_script_ir_create(default_temp_allocator);
        bool success = md_filter_evaluate(&res, str, &mol.molecule, filterIR, default_temp_allocator);
    
        if (success) {
            LDEBUG("Compiled viamd filter");
            for (int64_t i = 0; i < res.num_bitfields; ++i) {
                md_bitfield_or_inplace(&mol.visibilityMask, &res.bitfields[i]);
            }
        }
        else {
            LERROR("Failed to evaluate viamd filter");
        }

        for (int64_t i = 0; i < mol.molecule.atom.count; ++i) {
            uint8_t& flags = mol.molecule.atom.flags[i];
            flags = md_bitfield_test_bit(&mol.visibilityMask, i) ? AtomBitVisible : 0; 
        }

        md_filter_free(&res, default_temp_allocator);
        md_script_ir_free(filterIR);
    }

    else {
        for (int64_t i = 0; i < mol.molecule.atom.count; ++i) {
            mol.molecule.atom.flags[i] = AtomBitVisible;
        }
    }

    md_gl_molecule_set_atom_flags(&mol.drawMol, 0, static_cast<uint32_t>(mol.molecule.atom.count), mol.molecule.atom.flags, 0);
}

void RenderableMolecule::applyViamdScript(molecule_data_t& mol, const std::string& script) {
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

void RenderableMolecule::freeMolecule(molecule_data_t& mol) {
    cone_trace::free_volume(&mol.occupancyVolume);
    md_gl_representation_free(&mol.drawRep);
    md_gl_molecule_free(&mol.drawMol);
    mol.moleculeApi->free(&mol.molecule, default_allocator);
    mol.moleculeApi = nullptr;
    md_bitfield_free(&mol.visibilityMask);
    mol.visibilityMask = {};
    mol.molecule = {};
    mol.molecule = {};
    mol.drawMol = {};
    mol.drawRep = {};
    mol.occupancyVolume = {};
}

void RenderableMolecule::freeTrajectory(molecule_data_t& mol) {
    mol.trajectoryApi->destroy(mol.trajectory);
    mol.trajectoryApi = nullptr;
    mol.trajectory = nullptr;
}

} // namespace openspace
