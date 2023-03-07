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
#include "mol/viamd/postprocessing.h"
#include "mol/cache.h"
#include "mol/util.h"

#include <glm/gtc/random.hpp>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>

#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timemanager.h>

#include <openspace/scripting/scriptengine.h>

#include <core/md_array.h>
#include <core/md_allocator.h>
#include <md_util.h>

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

enum class AnimationRepeatMode {
    PingPong,
    Wrap
};

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

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Representation Type",
        "How to draw the molecule"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Representation Color",
        "Select a color mapping for the atoms"
    };

    constexpr openspace::properties::Property::PropertyInfo FilterInfo = {
        "Filter",
        "Representation Filter",
        "Filter for atom visibility"
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Representation Scale",
        "Scale for the Geometric representation of atoms"
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

    constexpr openspace::properties::Property::PropertyInfo EnsurePbcInfo = {
        "EnsurePbc",
        "Ensure PBC",
        "Ensure Periodic Boundry Constraints (Ensures post interpolation that structures remain within the simulation volume)"
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationBaseScaleInfo = {
        "AnimationBaseScale",
        "Animation Base Scale",
        "Base scale for the animation, tune this from its implicit value of 1.0 to sync up its animation with other trajectories"
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)"
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationRepeatModeInfo = {
        "AnimationRepeatMode",
        "Animation Repeat Mode",
        "Controls how the animation should be repeated"
    };

    constexpr const char* RepresentationsDescription = "Representations";

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        struct Representation {
            enum class [[codegen::map(mol::rep::Type)]] Type {
                SpaceFill,
                Licorice,
                Ribbons,
                Cartoon,
            };

            enum class [[codegen::map(mol::rep::Color)]] Color {
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

            std::optional<Type> type;
            std::optional<Color> color;
            std::optional<std::string> filter;
            std::optional<float> scale;
        };

        std::optional<std::vector<Representation>> representations;

        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::string trajectoryFile;

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

        // [[codegen::verbatim(EnsurePbcInfo.description)]]
        std::optional<bool> ensurePbc;

        // [[codegen::verbatim(AnimationBaseScaleInfo.description)]]
        std::optional<double> animationBaseScale;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<double> animationSpeed;

        enum class [[codegen::map(AnimationRepeatMode)]] AnimationRepeatMode {
            PingPong,
            Wrap
        };

        // [[codegen::verbatim(AnimationRepeatModeInfo.description)]]
        std::optional<AnimationRepeatMode> animationRepeatMode;
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

    void RenderableMolecule::addRepresentation(mol::rep::Type type, mol::rep::Color color, std::string filter, float scale) {
        properties::OptionProperty* pType = new properties::OptionProperty(TypeInfo);
        pType->addOptions({
            { static_cast<int>(mol::rep::Type::SpaceFill), "SpaceFill" },
            { static_cast<int>(mol::rep::Type::Licorice),  "Licorice" },
            { static_cast<int>(mol::rep::Type::Ribbons),   "Ribbons" },
            { static_cast<int>(mol::rep::Type::Cartoon),   "Cartoon" },
            });
        pType->setValue(static_cast<int>(type));    

        properties::OptionProperty* pColor = new properties::OptionProperty(ColorInfo);
        pColor->addOptions({
            { static_cast<int>(mol::rep::Color::Cpk), "CPK" },
            { static_cast<int>(mol::rep::Color::AtomIndex), "Atom Index" },
            { static_cast<int>(mol::rep::Color::ResId), "Residue ID" },
            { static_cast<int>(mol::rep::Color::ResIndex), "Residue Index" },
            { static_cast<int>(mol::rep::Color::ChainId), "Chain ID" },
            { static_cast<int>(mol::rep::Color::ChainIndex), "Chain Index" },
            { static_cast<int>(mol::rep::Color::SecondaryStructure), "Secondary Structure" },
            });
        pColor->setValue(static_cast<int>(color));

        properties::StringProperty* pFilter = new properties::StringProperty(FilterInfo);
        pFilter->setValue(filter);

        properties::FloatProperty* pScale = new properties::FloatProperty(ScaleInfo, 1.0f, 0.0f, 10.0f);
        pScale->setValue(scale);

        size_t i = _representations.propertySubOwners().size();
        PropertyOwner* prop = new PropertyOwner({std::to_string(i + 1)});
        prop->addProperty(pType);
        prop->addProperty(pColor);
        prop->addProperty(pFilter);
        prop->addProperty(pScale);
        _representations.addPropertySubOwner(prop);

        auto updateRep = [this, i, pType, pScale]() mutable {
            if (i >= _gl_representations.size()) {
                return;
            }
            mol::util::update_rep_type(_gl_representations[i], static_cast<mol::rep::Type>(pType->value()), pScale->value());
        };

        auto updateCol = [this, i, pColor, pFilter]() mutable {
            if (i >= _gl_representations.size()) {
                return;
            }
            mol::util::update_rep_color(_gl_representations[i], _molecule, static_cast<mol::rep::Color>(pColor->value()), pFilter->value());
        };

        pType->onChange(updateRep);
        pScale->onChange(updateRep);
        pColor->onChange(updateCol);
        pFilter->onChange(updateCol);
    }

    void RenderableMolecule::updateRepresentationsGL() {
        for (size_t i = 0; i < _gl_representations.size(); ++i) {
            md_gl_representation_free(&_gl_representations[i]);
        }
        _gl_representations.resize(_representations.propertySubOwners().size());
        for (size_t i = 0; i < _gl_representations.size(); ++i) {
            _gl_representations[i] = {0};
            md_gl_representation_init(&_gl_representations[i], &_gl_molecule);
            
            auto rep = _representations.propertySubOwners()[i];
            auto pType   = rep->property("Type");
            auto pColor  = rep->property("Color");
            auto pFilter = rep->property("Filter");
            auto pScale  = rep->property("Scale");

            if (pType && pColor && pFilter && pScale) {
                auto type   = static_cast<mol::rep::Type> (std::any_cast<int>(pType->get()));
                auto color  = static_cast<mol::rep::Color>(std::any_cast<int>(pColor->get()));
                auto filter = std::any_cast<std::string>(pFilter->get());
                auto scale  = std::any_cast<float>(pScale->get());

                mol::util::update_rep_type (_gl_representations[i], type, scale);
                mol::util::update_rep_color(_gl_representations[i], _molecule, color, filter);
            }
        }
    }

    void RenderableMolecule::updateTrajectoryFrame(double current_time) {
        if (_trajectory) {
            const double scl = _animationBaseScale * _animationSpeed;
            double time = (current_time - _localEpoch) * scl;
            double frame = 0;

            const int64_t num_frames = md_trajectory_num_frames(_trajectory);
            
            switch (static_cast<AnimationRepeatMode>(_animationRepeatMode.value())) {
            case AnimationRepeatMode::PingPong:
                frame = std::fmod(time, 2 * num_frames);
                if (frame >= num_frames) {
                    frame = 2 * num_frames - frame;
                }
                break;
            case AnimationRepeatMode::Wrap:
                frame = std::fmod(time, num_frames);
                break;
            default:
                ghoul_assert(false, "Don't end up here!");
            }

            if (frame != _frame) {
                _frame = frame;
                mol::util::interpolate_coords(_molecule, _trajectory, mol::util::InterpolationType::Cubic, frame, _ensurePbc);
                md_gl_molecule_set_atom_position(&_gl_molecule, 0, (uint32_t)_molecule.atom.count, _molecule.atom.x, _molecule.atom.y, _molecule.atom.z, 0);
            }
        }
    }

    documentation::Documentation RenderableMolecule::Documentation() {
        return codegen::doc<Parameters>("molecule_renderablemolecule");
    }

    RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary),
        _representations({"Representations"}),
        _moleculeFile(MoleculeFileInfo),
        _trajectoryFile(TrajectoryFileInfo),
        _ssaoEnabled(SSAOEnabledInfo),
        _ssaoIntensity(SSAOIntensityInfo, 12.f, 0.f, 100.f),
        _ssaoRadius(SSAORadiusInfo, 6.f, 0.f, 100.f),
        _ssaoBias(SSAOBiasInfo, 0.1f, 0.0f, 1.0f),
        _exposure(ExposureInfo, 0.3f, 0.1f, 10.f),
        _ensurePbc(EnsurePbcInfo, false),
        _animationBaseScale(AnimationBaseScaleInfo, 1.0, 0.0, 1e20),
        _animationSpeed(AnimationSpeedInfo, 1.f, -100.f, 100.f),
        _animationRepeatMode(AnimationRepeatModeInfo)
    {
        
        _localEpoch = openspace::global::timeManager->time().j2000Seconds();
        const Parameters p = codegen::bake<Parameters>(dictionary);

        _moleculeFile = p.moleculeFile;
        _trajectoryFile = p.trajectoryFile;
        _ssaoEnabled = p.ssaoEnabled.value_or(true);
        _ssaoIntensity = p.ssaoIntensity.value_or(12.f);
        _ssaoRadius = p.ssaoRadius.value_or(12.f);
        _ssaoBias = p.ssaoBias.value_or(0.1f);
        _exposure = p.exposure.value_or(0.3f);
        _ensurePbc = p.ensurePbc.value_or(false);

        if (p.representations.has_value()) {
            const auto& reps = p.representations.value();
            _gl_representations.reserve(reps.size());

            for (size_t i = 0; i < reps.size(); ++i) {
                const auto& rep = reps[i];

                // @NOTE: (Robin) value_or does not seem to cut it when we map it
                mol::rep::Type type = rep.type.has_value() ? codegen::map<mol::rep::Type>(rep.type.value()) : mol::rep::Type::SpaceFill;
                mol::rep::Color color = rep.color.has_value() ? codegen::map<mol::rep::Color>(rep.color.value()) : mol::rep::Color::Cpk;
                std::string filter = rep.filter.value_or("");
                float scale = rep.scale.value_or(1.0f);

                addRepresentation(type, color, filter, scale);
            }
        } else {
            // Add a default representation if none were supplied
            addRepresentation(mol::rep::Type::SpaceFill, mol::rep::Color::Cpk, "", 1.0f);
        }

        _animationBaseScale = p.animationBaseScale.value_or(1.0);
        _animationSpeed = p.animationSpeed.value_or(1.0);
        _animationRepeatMode.addOptions({
            { static_cast<int>(AnimationRepeatMode::PingPong), "PingPong" },
            { static_cast<int>(AnimationRepeatMode::Wrap), "Wrap" },
        });

        if (p.animationRepeatMode.has_value()) {
            _animationRepeatMode = static_cast<int>(codegen::map<AnimationRepeatMode>(*p.animationRepeatMode));
        } else {
            _animationRepeatMode = static_cast<int>(AnimationRepeatMode::PingPong);
        }

        // Remove read only if we have a valid trajectory
        _animationSpeed.setReadOnly(true);
        _animationRepeatMode.setReadOnly(true);
    
        addPropertySubOwner(_representations);
        addProperty(_ssaoEnabled);
        addProperty(_ssaoIntensity);
        addProperty(_ssaoRadius);
        addProperty(_ssaoBias);
        addProperty(_exposure);
        addProperty(_ensurePbc);
        addProperty(_animationSpeed);
        addProperty(_animationRepeatMode);
    }

    RenderableMolecule::~RenderableMolecule() {
        freeMolecule();
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

        initMolecule(_moleculeFile.value(), _trajectoryFile.value());

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

        // update animation
        if (_trajectory) {
            updateTrajectoryFrame(data.time.j2000Seconds());
        }
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
            translate(I, dvec3(-_center)) *
            I;

        mat4 projMatrix =
            dmat4(camCopy.sgctInternal.projectionMatrix()) *
            I;

        mat4_t model_mat = mat4_ident();
        std::vector<md_gl_draw_op_t> drawOps(_gl_representations.size());
        if (_molecule.atom.count) {
            for (size_t i = 0; i < _gl_representations.size(); ++i) {
                drawOps[i] = {&_gl_representations[i], &model_mat[0][0]};
            }
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

        { // draw into mdlib shared fbo
            GLint defaultFbo;
            glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
            glBindFramebuffer(GL_FRAMEBUFFER, fbo);
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
                settings.background_intensity = { 0, 0, 0 };
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
            const double pad = 2.0f; // This should represent the additional space when applying scaling within representations
            dvec3 sca = data.modelTransform.scale + dvec3(_radius * 2.f + pad);
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

    void RenderableMolecule::initMolecule(std::string_view molFile, std::string_view trajFile) {
        LDEBUG(fmt::format("Loading molecule file '{}'", molFile));

        // free previously loaded molecule
        freeMolecule();

        const md_molecule_t* molecule = mol_manager::get_molecule(molFile);
        if (molecule) {
            // We deep copy the contents, so we can freely modify the fields (coordinates etc)
            md_molecule_copy(&_molecule, molecule, default_allocator);

            md_gl_molecule_init(&_gl_molecule, &_molecule);
            updateRepresentationsGL();

            vec3_t aabb_min, aabb_max;
            md_util_compute_aabb_xyzr(&aabb_min, &aabb_max, _molecule.atom.x, _molecule.atom.y, _molecule.atom.z, _molecule.atom.radius, _molecule.atom.count);
            vec3_t c = (aabb_min + aabb_max) * 0.5f;
            _center = {c.x, c.y, c.z};
            _radius = vec3_distance(aabb_min, aabb_max) * 0.5f;
            setBoundingSphere(_radius * 2.f);
        }

        if (!trajFile.empty() && trajFile != "") {
            LDEBUG(fmt::format("Loading trajectory file '{}'", trajFile));
            _trajectory = mol_manager::get_trajectory(trajFile);

            if (!_trajectory) {
                LERROR("failed to initialize trajectory: failed to load file");
                return;
            }
            
            _animationSpeed.setReadOnly(false);
            _animationRepeatMode.setReadOnly(false);
        }
    }

    void RenderableMolecule::freeMolecule() {
        for (auto& rep : _gl_representations) {
            md_gl_representation_free(&rep);
        }
        _gl_representations.clear();

        md_gl_molecule_free(&_gl_molecule);
        md_molecule_free(&_molecule, default_allocator);

        _molecule = {};
        _trajectory = nullptr;
        _gl_molecule = {};
        _gl_representations.clear();
    }

} // namespace openspace
