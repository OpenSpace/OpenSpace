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
#include "moleculemodule.h"

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
#include <openspace/engine/moduleengine.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timemanager.h>

#include <openspace/scripting/scriptengine.h>

#include <core/md_array.h>
#include <core/md_allocator.h>
#include <md_filter.h>
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

    constexpr openspace::properties::Property::PropertyInfo CoarseGrainedInfo = {
        "CoarseGrained",
        "Coarse Grained",
        "Enable if the dataset should be interpreted as coarse grained"
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyPbcOnLoadInfo = {
        "ApplyPbcOnLoad",
        "Apply PBC On Load",
        "Applies Periodic Boundary Constraints upon loading trajectory frames"
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyPbcPerFrameInfo = {
        "ApplyPbcPerFrame",
        "Apply PBC Per Frame",
        "Applies Periodic Boundary Constraints for each interpolated frame (Can be CPU-intensive!)"
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

    constexpr openspace::properties::Property::PropertyInfo UniformColorInfo = {
        "UniformColor",
        "Uniform Color",
        "The uniform color to apply for the representation"
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
                Uniform,
                // Property
            };

            std::optional<Type> type;
            std::optional<Color> color;
            std::optional<std::string> filter;
            std::optional<float> scale;
            std::optional<glm::vec4> uniformColor;
        };

        std::optional<std::vector<Representation>> representations;

        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::string trajectoryFile;

        // [[codegen::verbatim(CoarseGrainedInfo.description)]]
        std::optional<bool> coarseGrained;

        // [[codegen::verbatim(ApplyPbcOnLoadInfo.description)]]
        std::optional<bool> applyPbcOnLoad;

        // [[codegen::verbatim(ApplyPbcPerFrameInfo.description)]]
        std::optional<bool> applyPbcPerFrame;

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

static void compute_mask(md_bitfield_t& mask, std::string_view filter, const md_molecule_t& mol) {
    if (!filter.empty() && filter != "" && filter != "all") {
        str_t str = {filter.data(), (int64_t)filter.length()};
        char err_buf[1024];

        if (md_filter(&mask, str, &mol, NULL, NULL, err_buf, sizeof(err_buf))) {
            return;
        }
        LERROR(fmt::format("Invalid filter expression '{}': {}", filter, err_buf));
    }
    md_bitfield_clear(&mask);
    md_bitfield_set_range(&mask, 0, mol.atom.count);
}

namespace openspace {

    void RenderableMolecule::addRepresentation(mol::rep::Type type, mol::rep::Color color, std::string filter, float scale, glm::vec4 uniform_color) {
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
            { static_cast<int>(mol::rep::Color::Uniform), "Uniform" },
            });
        pColor->setValue(static_cast<int>(color));

        properties::Vec4Property* pUniformColor = new properties::Vec4Property(UniformColorInfo);
        pUniformColor->setViewOption(openspace::properties::Property::ViewOptions::Color);
        pUniformColor->setValue(uniform_color);

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
        prop->addProperty(pUniformColor);
        
        _representations.addPropertySubOwner(prop);

        _representation_mask.emplace_back(md_bitfield_create(default_allocator));

        auto updateRep = [this, i, pType, pScale]() mutable {
            if (i >= _gl_representations.size()) {
                return;
            }
            mol::util::update_rep_type(_gl_representations[i], static_cast<mol::rep::Type>(pType->value()), pScale->value());
        };

        auto updateFilt = [this, i, pFilter]() mutable {
            if (i >= _representation_mask.size()) {
                return;
            }
            const auto& filter = pFilter->value();
            compute_mask(_representation_mask[i], filter, _molecule);
            mol::util::update_rep_color(_gl_representations[i], _molecule, mol::rep::Color::Cpk, _representation_mask[i], glm::vec4(1.0f));
        };

        auto updateCol = [this, i, pColor, pUniformColor]() mutable {
            if (i >= _gl_representations.size()) {
                return;
            }
            if (i >= _representation_mask.size()) {
                return;
            }
                
            mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());
            const md_bitfield_t& mask = _representation_mask[i];

            if (color == mol::rep::Color::Uniform) {
                pUniformColor->setVisibility(openspace::properties::Property::Visibility::Always);
            } else {
                pUniformColor->setVisibility(openspace::properties::Property::Visibility::Hidden);
            }
            mol::util::update_rep_color(_gl_representations[i], _molecule, color, mask, pUniformColor->value());
        };

        pType->onChange(updateRep);
        pScale->onChange(updateRep);
        pColor->onChange(updateCol);
        pFilter->onChange(updateCol);
        pUniformColor->onChange(updateCol);
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
            auto pUniformColor = rep->property("UniformColor");

            if (pType && pColor && pFilter && pScale && pUniformColor) {
                auto type   = static_cast<mol::rep::Type> (std::any_cast<int>(pType->get()));
                auto color  = static_cast<mol::rep::Color>(std::any_cast<int>(pColor->get()));
                auto filter = std::any_cast<std::string>(pFilter->get());
                auto scale  = std::any_cast<float>(pScale->get());
                auto uniform_color = std::any_cast<glm::vec4>(pUniformColor->get());

                compute_mask(_representation_mask[i], filter, _molecule);

                mol::util::update_rep_type (_gl_representations[i], type, scale);
                mol::util::update_rep_color(_gl_representations[i], _molecule, color, _representation_mask[i], uniform_color);
            }
        }
    }

    static double time_to_frame(double time, int64_t num_frames, AnimationRepeatMode mode) {
        double frame = 0;
        switch (static_cast<AnimationRepeatMode>(mode)) {
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
        return frame;
    }

    void RenderableMolecule::updateTrajectoryFrame(const UpdateData& data) {
        if (_trajectory) {
            double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
            double curr_t = data.time.j2000Seconds();
            double next_t = data.time.j2000Seconds() + dt;
            
            const double scl = _animationBaseScale * _animationSpeed;
            double curr_time = (curr_t - _localEpoch) * scl;
            double next_time = (next_t - _localEpoch) * scl;

            const int64_t num_frames = md_trajectory_num_frames(_trajectory);
            AnimationRepeatMode mode = static_cast<AnimationRepeatMode>(_animationRepeatMode.value());
            double frame = time_to_frame(curr_time, num_frames, mode);
            
            if (frame != _frame) {
                _frame = frame;
                mol::util::interpolate_coords(_molecule, _trajectory, mol::util::InterpolationType::Cubic, frame, _applyPbcPerFrame);
                md_gl_molecule_set_atom_position(&_gl_molecule, 0, (uint32_t)_molecule.atom.count, _molecule.atom.x, _molecule.atom.y, _molecule.atom.z, 0);
                
                std::vector<int64_t> indices;
                
                for (int64_t i = 0; i < _representations.propertySubOwners().size(); ++i) {
                    auto pColor  = _representations.propertySubOwners()[i]->property("Color");
                    if (pColor) {
                        auto color  = static_cast<mol::rep::Color>(std::any_cast<int>(pColor->get()));
                        if (color == mol::rep::Color::SecondaryStructure) {
                            indices.push_back(i);
                        }
                    }
                }
                
                if (!indices.empty()) {
                    mol::util::interpolate_secondary_structure(_molecule, _trajectory, mol::util::InterpolationType::Cubic, frame);
                    md_gl_molecule_set_backbone_secondary_structure(&_gl_molecule, 0, (uint32_t)_molecule.backbone.count, _molecule.backbone.secondary_structure, 0);
                    
                    for (int64_t i : indices) {
                        mol::util::update_rep_color(_gl_representations[i], _molecule, mol::rep::Color::SecondaryStructure, _representation_mask[i]);
                    }
                }
            }
            
            // Prefetch next frames
            const int64_t frame_idx = static_cast<int64_t>(time_to_frame(next_time, num_frames, mode));
            const int64_t beg_idx = std::max(0LL, frame_idx - 1);
            const int64_t end_idx = std::min(num_frames - 1, frame_idx + 2);
            mol_manager::prefetch_frame_range(_trajectory, beg_idx, end_idx);
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
        _coarseGrained(CoarseGrainedInfo, false),
        _applyPbcOnLoad(ApplyPbcOnLoadInfo, true),
        _applyPbcPerFrame(ApplyPbcPerFrameInfo, false),
        _animationBaseScale(AnimationBaseScaleInfo, 1.0, 0.0, 1e20),
        _animationSpeed(AnimationSpeedInfo, 1.f, -100.f, 100.f),
        _animationRepeatMode(AnimationRepeatModeInfo)
    {
        
        _localEpoch = openspace::global::timeManager->time().j2000Seconds();
        const Parameters p = codegen::bake<Parameters>(dictionary);

        _moleculeFile     = p.moleculeFile;
        _trajectoryFile   = p.trajectoryFile;
        _coarseGrained    = p.coarseGrained.value_or(false);
        _applyPbcOnLoad   = p.applyPbcOnLoad.value_or(true);
        _applyPbcPerFrame = p.applyPbcPerFrame.value_or(false);

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
                glm::vec4 uniform_color = rep.uniformColor.value_or(glm::vec4(1.0f));

                addRepresentation(type, color, filter, scale, uniform_color);
            }
        } else {
            // Add a default representation if none were supplied
            addRepresentation(mol::rep::Type::SpaceFill, mol::rep::Color::Cpk, "", 1.0f, glm::vec4(1.0f));
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
        //addProperty(_applyPbcOnLoad);
        addProperty(_applyPbcPerFrame);
        addProperty(_animationSpeed);
        addProperty(_animationRepeatMode);
    }

    RenderableMolecule::~RenderableMolecule() {
        freeMolecule();
        for (auto& mask : _representation_mask) {
            md_bitfield_free(&mask);
        }
    }

    void RenderableMolecule::initialize() {
        ZoneScoped
    }

    void RenderableMolecule::initializeGL() {
        ZoneScoped
        initMolecule(_moleculeFile.value(), _trajectoryFile.value());
    }

    void RenderableMolecule::deinitializeGL() {
        for (auto& rep : _gl_representations) {
            md_gl_representation_free(&rep);
        }
    }

    bool RenderableMolecule::isReady() const {
        return true;
    }

    void RenderableMolecule::update(const UpdateData& data) {
        if (!this->isEnabled()) {
            return;
        }
        
        // avoid updating if not in view, as it can be quite expensive.
        if (!_renderableInView)
            return;
        else
            _renderableInView = false;

        // update animation
        if (_trajectory) {
            updateTrajectoryFrame(data);
        }
    }

    void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
        GLuint fbo = global::moduleEngine->module<MoleculeModule>()->fbo();
        const md_gl_shaders_t& shaders = global::moduleEngine->module<MoleculeModule>()->shaders();
        
        const glm::dmat4 I(1.0);

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

        glm::mat4 viewMatrix =
            camCopy.combinedViewMatrix() *
            translate(I, data.modelTransform.translation) *
            scale(I, data.modelTransform.scale) *
            dmat4(data.modelTransform.rotation) *
            translate(I, glm::dvec3(-_center)) *
            I;

        glm::mat4 projMatrix =
            glm::dmat4(camCopy.sgctInternal.projectionMatrix()) *
            I;
        
        const mat4_t model_mat = mat4_ident();
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

        GLint last_fbo;
        GLint  last_draw_buffer_count = 0;
        GLenum last_draw_buffers[8];
        glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &last_fbo);
        for (int i = 0; i < ARRAY_SIZE(last_draw_buffers); ++i) {
            GLint draw_buf;
            glGetIntegerv(GL_DRAW_BUFFER0+i, &draw_buf);
            if (!draw_buf) {
                break;
            }
            last_draw_buffers[last_draw_buffer_count++] = (GLenum)draw_buf;
        }
            
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, bufs);

        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        md_gl_draw(&args);

        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, last_fbo);
        glDrawBuffers(last_draw_buffer_count, last_draw_buffers);
        
        global::moduleEngine->module<MoleculeModule>()->setProjectionMatrix(projMatrix);
    }

    void RenderableMolecule::initMolecule(std::string_view molFile, std::string_view trajFile) {
        LDEBUG(fmt::format("Loading molecule file '{}'", molFile));

        // free previously loaded molecule
        freeMolecule();

        const md_molecule_t* molecule = mol_manager::load_molecule(molFile, _coarseGrained);
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
        } else {
            LERROR("failed to initialize molecule: failed to load file");
            return;
        }

        if (!trajFile.empty() && trajFile != "") {
            LDEBUG(fmt::format("Loading trajectory file '{}'", trajFile));
            _trajectory = mol_manager::load_trajectory(trajFile, molecule, _applyPbcOnLoad);

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
