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
#include "openspace/engine/windowdelegate.h"
#include "mol/viamd/postprocessing_utils.h"
#include "mol/cache.h"
#include "mol/util.h"

#include <glm/gtc/random.hpp>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>

#include <core/md_array.inl>
#include <md_util.h>
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

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
    out_normal = vec4(encode_normal(view_normal), 0, 0);
    out_color = color;
}
)";

namespace {
    constexpr const char* _loggerCat = "RenderableMolecule";

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
        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::string trajectoryFile;

        enum class [[codegen::map(mol::rep::Type)]] RepType {
            SpaceFill,
            Licorice,
            Ribbons,
            Cartoon,
        };

        // [[codegen::verbatim(RepTypeInfo.description)]]
        std::optional<RepType> repType;

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

        // [[codegen::verbatim(SSAOBiasInfo.description)]]
        std::optional<float> ssaoBias;

        // [[codegen::verbatim(ExposureInfo.description)]]
        std::optional<float> exposure;
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
        _ssaoRadius(SSAORadiusInfo, 12.f, 0.f, 100.f),
        _ssaoBias(SSAOBiasInfo, 0.1f, 0.0f, 1.0f),
        _exposure(ExposureInfo, 0.3f, 0.1f, 10.f)
    {
        _repType.addOptions({
            { static_cast<int>(mol::rep::Type::SpaceFill), "Space Fill" },
            { static_cast<int>(mol::rep::Type::Licorice),  "Licorice" },
            { static_cast<int>(mol::rep::Type::Ribbons),   "Ribbons" },
            { static_cast<int>(mol::rep::Type::Cartoon),   "Cartoon" },
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

        _moleculeFile = p.moleculeFile;
        _trajectoryFile = p.trajectoryFile;
        _repScale = p.repScale.value_or(1.f);
        _animationSpeed = p.animationSpeed.value_or(1.f);
        _viamdFilter = p.viamdFilter.value_or("");
        _ssaoEnabled = p.ssaoEnabled.value_or(true);
        _ssaoIntensity = p.ssaoIntensity.value_or(12.f);
        _ssaoRadius = p.ssaoRadius.value_or(12.f);
        _ssaoBias = p.ssaoBias.value_or(0.1f);
        _exposure = p.exposure.value_or(0.3f);

        if (p.repType.has_value()) {
            _repType = static_cast<int>(codegen::map<mol::rep::Type>(*p.repType));
        }
        else {
            _repType = static_cast<int>(mol::rep::Type::SpaceFill);
        }

        if (p.coloring.has_value()) {
            _coloring = static_cast<int>(codegen::map<mol::rep::Color>(*p.coloring));
        }
        else {
            _coloring = static_cast<int>(mol::rep::Color::Cpk);
        }

        _molecule = molecule_data_t{
            {},      // extent
            {},      // center
            0.f,     // radius
            {},      // molecule
            nullptr, // trajectory
            {},      // drawRep
            {},      // drawMol
        };

        auto onUpdateRepr = [this]() {
            mol::util::update_rep_type(_molecule.drawRep, static_cast<mol::rep::Type>(_repType.value()), _repScale);
        };

        auto onUpdateCol = [this]() {
            mol::util::update_rep_colors(_molecule.drawRep, _molecule.molecule, static_cast<mol::rep::Color>(_coloring.value()), _viamdFilter);
        };

        _repType.onChange(onUpdateRepr);
        _repScale.onChange(onUpdateRepr);
        _coloring.onChange(onUpdateCol);
        _viamdFilter.onChange(onUpdateCol);
    
        addProperty(_repType);
        addProperty(_coloring);
        addProperty(_repScale);
        addProperty(_animationSpeed);
        addProperty(_viamdFilter);
        addProperty(_ssaoEnabled);
        addProperty(_ssaoIntensity);
        addProperty(_ssaoRadius);
        addProperty(_ssaoBias);
        addProperty(_exposure);
    }

    RenderableMolecule::~RenderableMolecule() {
        freeMolecule(_molecule);
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

                if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
                    LERROR("Mold Framebuffer is not complete");

                md_gl_initialize();
                md_gl_shaders_init(&shaders, shader_output_snippet);

                postprocessing::initialize(size.x, size.y);

                glBindFramebuffer(GL_FRAMEBUFFER, 0);

                billboardGlInit();
            }

        initMolecule(_molecule, _moleculeFile.value(), _trajectoryFile.value());

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
            postprocessing::shutdown();
        }

        billboardGlDeinit();
        md_gl_shaders_free(&shaders);
        md_gl_shutdown();
    }

    bool RenderableMolecule::isReady() const {
        return true;
    }

    void RenderableMolecule::update(const UpdateData& data) {
        // avoid updating if not in view, as it can be quite expensive.
        if (!_renderableInView)
            return;
        else
            _renderableInView = false;

        double t = data.time.j2000Seconds();

        // update animation
        if (_molecule.trajectory) {
            mol::util::interpolate_coords(_molecule.molecule.atom.x, _molecule.molecule.atom.y, _molecule.molecule.atom.z, _molecule.molecule.atom.count, t, mol::util::Interpolation::Cubic, _molecule.trajectory);
        }

        // update gl repr
        md_gl_molecule_set_atom_position(&_molecule.drawMol, 0, (uint32_t)_molecule.molecule.atom.count, _molecule.molecule.atom.x, _molecule.molecule.atom.y, _molecule.molecule.atom.z, 0);
    }

    static double normalizeDouble(double input) {
        if (input > 1.0) {
            return input / pow(10, 30);
        }
        else {
            return input - 1.0;
        }
    }

    static mat4_t mat4_from_glm(glm::mat4 const& src) {
        mat4_t dst;
        memcpy(&dst, &src, 4 * 4 * sizeof(float));
        return dst;
    }

    void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
        global::renderEngine->openglStateCache().loadCurrentGLState();

        using namespace glm;
        const dmat4 I(1.0);

        // compute distance from camera to molecule
        dvec3 forward = data.modelTransform.translation - data.camera.positionVec3();
        dvec3 dir = data.camera.viewDirectionWorldSpace();
        double distance = length(forward) * sign(dot(dir, forward)); // "signed" distance from camera to object.

        if (distance < 0.0 || distance > 1E4) // distance < 0 means behind the camera, 1E4 is arbitrary.
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
            glDisable(GL_CULL_FACE);
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
                settings.background.intensity = { { 0, 0, 0 } };
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

                // restore state after postprocess
                glEnable(GL_DEPTH_TEST); 
                glDisable(GL_BLEND);
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

            dvec4 depth_ = dmat4(data.camera.sgctInternal.projectionMatrix()) * billboardModel * dvec4(0.0, 0.0, 0.0, 1.0);
            double depth = normalizeDouble(depth_.w);

            billboardDraw(transform, colorTex, depthTex, vec4(1.0), 0.0, static_cast<float>(depth));
        }

        global::renderEngine->openglStateCache().resetBlendState();
    }

    void RenderableMolecule::computeAABB(molecule_data_t& mol) {
        glm::vec3 min_aabb{ FLT_MAX };
        glm::vec3 max_aabb{ -FLT_MAX };

        for (int64_t i = 0; i < mol.molecule.atom.count; ++i) {
            glm::vec3 p{ mol.molecule.atom.x[i], mol.molecule.atom.y[i], mol.molecule.atom.z[i] };
            min_aabb = glm::min(min_aabb, p);
            max_aabb = glm::max(max_aabb, p);
        }

        mol.extent = max_aabb - min_aabb;
        mol.center = (min_aabb + max_aabb) * 0.5f;
        mol.radius = length(mol.extent) * 0.5f;
    }

    void RenderableMolecule::initMolecule(molecule_data_t& mol, std::string_view molFile, std::string_view trajFile) {
        LDEBUG(fmt::format("Loading molecule file '{}'", molFile));

        // free previously loaded molecule
        freeMolecule(mol);

        const md_molecule_t* molecule = mol_manager::get_molecule(molFile);
        if (molecule) {
            // We deep copy the contents, so we can freely modify the fields (coordinates etc)
            md_molecule_append(&mol.molecule, molecule, default_allocator);
        }

        if (!trajFile.empty() && trajFile != "") {
            LDEBUG(fmt::format("Loading trajectory file '{}'", trajFile));
            mol.trajectory = mol_manager::get_trajectory(trajFile);

            if (!mol.trajectory) {
                LERROR("failed to initialize trajectory: failed to load file");
                return;
            }
        }

        computeAABB(mol);
        setBoundingSphere(mol.radius * 2.f);
        // setInteractionSphere(sphere);

        md_gl_molecule_init(&mol.drawMol, &mol.molecule);
        md_gl_representation_init(&mol.drawRep, &mol.drawMol);
        
        mol::util::update_rep_type(mol.drawRep, static_cast<mol::rep::Type>(_repType.value()), _repScale);
        mol::util::update_rep_colors(mol.drawRep, mol.molecule, static_cast<mol::rep::Color>(_coloring.value()), _viamdFilter);
    }

    void RenderableMolecule::freeMolecule(molecule_data_t& mol) {
        md_gl_representation_free(&mol.drawRep);
        md_gl_molecule_free(&mol.drawMol);
        md_molecule_free(&mol.molecule, default_allocator);

        mol.molecule = {};
        mol.trajectory = nullptr;
        mol.drawMol = {};
        mol.drawRep = {};
    }

} // namespace openspace
