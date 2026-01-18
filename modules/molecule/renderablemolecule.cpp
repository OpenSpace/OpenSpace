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

#include <modules/molecule/renderablemolecule.h>
#include <modules/molecule/moleculemodule.h>

#include <modules/molecule/billboard.h>

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
    Wrap,
    Clamp
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

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Representation Enabled",
        "Enable the representation"
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Representation Type",
        "Visual representation type of the molecule"
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

            std::optional<bool> enabled;
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
            Wrap,
            Clamp
        };

        // [[codegen::verbatim(AnimationRepeatModeInfo.description)]]
        std::optional<AnimationRepeatMode> animationRepeatMode;
    };

#include "renderablemolecule_codegen.cpp"
}

static void compute_mask(md_bitfield_t& mask, std::string_view filter, const md_molecule_t& mol, bool& is_dynamic) {
    if (!filter.empty() && filter != "" && filter != "all") {
        str_t str = {filter.data(), (int64_t)filter.length()};
        char err_buf[1024];

        if (md_filter(&mask, str, &mol, NULL, &is_dynamic, err_buf, sizeof(err_buf))) {
            return;
        }
        LERROR(std::format("Invalid filter expression '{}': {}", filter, err_buf));
    }
    md_bitfield_clear(&mask);
    md_bitfield_set_range(&mask, 0, mol.atom.count);
}

namespace openspace {

    void RenderableMolecule::addRepresentation(bool enabled, mol::rep::Type type, mol::rep::Color color, std::string filter, float scale, glm::vec4 uniform_color) {
        properties::BoolProperty* pEnabled = new properties::BoolProperty(EnabledInfo);
        *pEnabled = enabled;

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

        size_t i = _repProps.propertySubOwners().size();

        PropertyOwnerInfo prop_info {
            std::format("rep{}", i),
            std::format("Representation {}", i),
            std::format("Visual representation of molecule", i),
        };

        PropertyOwner* prop = new PropertyOwner(prop_info);
        prop->addProperty(pEnabled);
        prop->addProperty(pType);
        prop->addProperty(pColor);
        prop->addProperty(pFilter);
        prop->addProperty(pScale);
        prop->addProperty(pUniformColor);
        
        _repProps.addPropertySubOwner(prop);

        _repData.emplace_back();
        
        auto enableRep = [this, i, pEnabled]() mutable {
            if (i >= _repData.size()) {
                return;
            }
            _repData[i].enabled = pEnabled->value();
        };

        auto updateRep = [this, i, pType, pScale]() mutable {
            if (i >= _repData.size()) {
                return;
            }
            mol::util::updateRepType(_repData[i].gl_rep, static_cast<mol::rep::Type>(pType->value()), pScale->value());
        };

        auto updateCol = [this, i, pColor, pUniformColor]() mutable {
            if (i >= _repData.size()) {
                return;
            }

            const mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());
            mol::util::updateRepColor(_repData[i].gl_rep, _molecule, color, _repData[i].mask, pUniformColor->value());
        };

        auto updateFilt = [this, i, pFilter, updateCol]() mutable {
            if (i >= _repData.size()) {
                return;
            }
            const auto& filter = pFilter->value();
            compute_mask(_repData[i].mask, filter, _molecule, _repData[i].dynamic);
            updateCol();
        };

        pEnabled->onChange(enableRep);
        pType->onChange(updateRep);
        pScale->onChange(updateRep);
        pColor->onChange(updateCol);
        pFilter->onChange(updateFilt);
        pUniformColor->onChange(updateCol);
    }

    void RenderableMolecule::updateRepresentations() {
        for (auto& rep : _repData) {
            md_gl_representation_free(&rep.gl_rep);
        }
        
        _repData.resize(_repProps.propertySubOwners().size());
        for (size_t i = 0; i < _repData.size(); ++i) {
            auto& rep = _repData[i];
            rep.gl_rep = {0};
            md_gl_representation_init(&rep.gl_rep, &_gl_molecule);

            using namespace properties;
            auto pRep = _repProps.propertySubOwners()[i];
            auto pEnabled = dynamic_cast<BoolProperty*>(pRep->property("Enabled"));
            auto pType = dynamic_cast<OptionProperty*>(pRep->property("Type"));
            auto pColor = dynamic_cast<OptionProperty*>(pRep->property("Color"));
            auto pFilter = dynamic_cast<StringProperty*>(pRep->property("Filter"));
            auto pScale = dynamic_cast<FloatProperty*>(pRep->property("Scale"));
            auto pUniformColor = dynamic_cast<Vec4Property*>(pRep->property("UniformColor"));

            if (pEnabled && pType && pColor && pFilter && pScale && pUniformColor) {
                mol::rep::Type type = static_cast<mol::rep::Type>(pType->value());
                mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());

                if (*pEnabled) {
                    compute_mask(rep.mask, *pFilter, _molecule, rep.dynamic);

                    mol::util::updateRepType(rep.gl_rep, type, *pScale);
                    mol::util::updateRepColor(rep.gl_rep, _molecule, color, rep.mask, *pUniformColor);
                }
            }
        }
    }

    static double time_to_frame(double time, int64_t num_frames, AnimationRepeatMode mode) {
        double frame = 0;
        const double last_frame = num_frames - 1.0;
        switch (static_cast<AnimationRepeatMode>(mode)) {
        case AnimationRepeatMode::PingPong:
            frame = std::fmod(time, 2.0 * last_frame);
            if (frame > last_frame) {
                frame = 2.0 * last_frame - frame;
            }
            break;
        case AnimationRepeatMode::Wrap:
            frame = std::fmod(time, num_frames);
            break;
        case AnimationRepeatMode::Clamp:
            frame = std::clamp(time, 0.0, last_frame - 1.0);
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
            
            const double scl = _animationBaseScale * _animationSpeed;
            double curr_time = (curr_t - _localEpoch) * scl;
            double next_time = (curr_t + dt - _localEpoch) * scl;

            const int64_t num_frames = md_trajectory_num_frames(_trajectory);
            AnimationRepeatMode mode = static_cast<AnimationRepeatMode>(_animationRepeatMode.value());
            double frame = time_to_frame(curr_time, num_frames, mode);
            
            if (frame != _frame) {
                _frame = frame;
                mol::util::interpolateFrame(_molecule, _trajectory, mol::util::InterpolationType::Cubic, frame, _applyPbcPerFrame);

                const uint64_t size = _molecule.atom.count * sizeof(vec3_t);
                md_allocator_i* alloc = default_temp_allocator_max_allocation_size() < size ? default_allocator : default_temp_allocator;
                vec3_t* xyz = (vec3_t*)md_alloc(alloc, size);
                
                for (int64_t i = 0; i < _molecule.atom.count; ++i) {
                    xyz[i] = { _molecule.atom.x[i], _molecule.atom.y[i], _molecule.atom.z[i] };
                }
                md_gl_molecule_set_atom_position_xyz(&_gl_molecule, 0, (uint32_t)_molecule.atom.count, xyz);
                
                md_free(alloc, xyz, size);
                
                std::vector<size_t> rep_has_ss_indices;
                std::vector<size_t> rep_update_col_indices;
                
                for (size_t i = 0; i < _repProps.propertySubOwners().size(); ++i) {
                    if (!_repData[i].enabled) continue;

                    using namespace properties;
                    auto pColor = dynamic_cast<OptionProperty*>(_repProps.propertySubOwners()[i]->property("Color"));
                    auto pFilter = dynamic_cast<StringProperty*>(_repProps.propertySubOwners()[i]->property("Filter"));
                    if (pColor) {
                        auto color  = static_cast<mol::rep::Color>(pColor->value());
                        if (color == mol::rep::Color::SecondaryStructure) {
                            rep_has_ss_indices.push_back(i);
                        }
                    }
                    if (pFilter) {
                        if (_repData[i].dynamic) {
                            compute_mask(_repData[i].mask, *pFilter, _molecule, _repData[i].dynamic);
                            rep_update_col_indices.push_back(i);
                        }
                    }
                }
                
                if (!rep_has_ss_indices.empty()) {
                    //mol::util::interpolate_secondary_structure(_molecule, _trajectory, mol::util::InterpolationType::Cubic, frame);
                    md_gl_molecule_set_backbone_secondary_structure(&_gl_molecule, 0, (uint32_t)_molecule.backbone.count, _molecule.backbone.secondary_structure, 0);
                    
                    for (size_t i : rep_has_ss_indices) {
                        rep_update_col_indices.push_back(i);
                    }
                }

                for (size_t i : rep_update_col_indices) {
                    using namespace properties;
                    auto pColor = dynamic_cast<OptionProperty*>(_repProps.propertySubOwners()[i]->property("Color"));
                    auto color = static_cast<mol::rep::Color>(pColor->value());

                    auto pUniformColor = dynamic_cast<Vec4Property*>(_repProps.propertySubOwners()[i]->property("UniformColor"));
                    mol::util::updateRepColor(_repData[i].gl_rep, _molecule, color, _repData[i].mask, *pUniformColor);
                }
            }

            using FrameSet = std::array<int64_t, 4>;

            auto frame_set = [num_frames, mode](double time) -> FrameSet {
                const int64_t frame_idx = static_cast<int64_t>(time_to_frame(time, num_frames, mode));
                FrameSet set;

                auto wrap = [num_frames](int64_t idx) -> int64_t {
                    idx = idx < 0 ? num_frames - 1 : idx;
                    idx = idx >= num_frames ? 0 : idx;
                    return idx;
                };
                
                set[0] = wrap(frame_idx - 1);
                set[1] = wrap(frame_idx + 0);
                set[2] = wrap(frame_idx + 1);
                set[3] = wrap(frame_idx + 2);
                
                return set;
            };

            // Prefetch next set of frames
            FrameSet curr = frame_set(curr_time);
            FrameSet nxt1 = frame_set(next_time);

            std::set<int64_t> unique_frames;
            unique_frames.insert(nxt1.begin(), nxt1.end());
            for (int64_t i : curr) {
                unique_frames.erase(i);
            }
            
            std::vector<int64_t> frames;
            frames.reserve(unique_frames.size());
            for (int64_t i : unique_frames) {
                frames.push_back(i);
            }
            
            mol_manager::prefetch_frames(_trajectory, frames);
        }
    }

    documentation::Documentation RenderableMolecule::Documentation() {
        return codegen::doc<Parameters>("molecule_renderablemolecule");
    }

    RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary),
        _repProps({"Representations"}),
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
            _repData.reserve(reps.size());

            for (size_t i = 0; i < reps.size(); ++i) {
                const auto& rep = reps[i];

                // @NOTE: (Robin) value_or does not seem to cut it when we map it
                bool enabled = rep.enabled.value_or(true);
                mol::rep::Type type = rep.type.has_value() ? codegen::map<mol::rep::Type>(rep.type.value()) : mol::rep::Type::SpaceFill;
                mol::rep::Color color = rep.color.has_value() ? codegen::map<mol::rep::Color>(rep.color.value()) : mol::rep::Color::Cpk;
                std::string filter = rep.filter.value_or("");
                float scale = rep.scale.value_or(1.0f);
                glm::vec4 uniform_color = rep.uniformColor.value_or(glm::vec4(1.0f));

                addRepresentation(enabled, type, color, filter, scale, uniform_color);
            }
        } else {
            // Add a default representation if none were supplied
            addRepresentation(true, mol::rep::Type::SpaceFill, mol::rep::Color::Cpk, "", 1.0f, glm::vec4(1.0f));
        }

        _animationBaseScale = p.animationBaseScale.value_or(1.0);
        _animationSpeed = p.animationSpeed.value_or(1.0);
        _animationRepeatMode.addOptions({
            { static_cast<int>(AnimationRepeatMode::PingPong), "PingPong" },
            { static_cast<int>(AnimationRepeatMode::Wrap), "Wrap" },
            { static_cast<int>(AnimationRepeatMode::Clamp), "Clamp" },
        });

        if (p.animationRepeatMode.has_value()) {
            _animationRepeatMode = static_cast<int>(codegen::map<AnimationRepeatMode>(*p.animationRepeatMode));
        } else {
            _animationRepeatMode = static_cast<int>(AnimationRepeatMode::PingPong);
        }

        // Remove read only if we have a valid trajectory
        _animationSpeed.setReadOnly(true);
        _animationRepeatMode.setReadOnly(true);
    
        addPropertySubOwner(_repProps);
        addProperty(_applyPbcPerFrame);
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
        initMolecule(_moleculeFile.value(), _trajectoryFile.value());
    }

    void RenderableMolecule::deinitializeGL() {
        for (auto& rep : _repData) {
            md_gl_representation_free(&rep.gl_rep);
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
            I;

        glm::mat4 projMatrix =
            glm::dmat4(camCopy.sgctInternal.projectionMatrix()) *
            I;
        
        // Center the molecule with respect to its cell
        const vec3_t trans = _molecule.cell.basis * vec3_set1(0.5f);
        const mat4_t model_mat = mat4_translate(-trans.x, -trans.y, -trans.z);

        std::vector<md_gl_draw_op_t> drawOps;
        drawOps.reserve((_repData.size()));

        if (_molecule.atom.count) {
            for (size_t i = 0; i < _repData.size(); ++i) {
                if (_repData[i].enabled) {
                    drawOps.emplace_back(&_repData[i].gl_rep, &model_mat[0][0]);
                }
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
        
        global::moduleEngine->module<MoleculeModule>()->setViewMatrix(data.camera.combinedViewMatrix());
        global::moduleEngine->module<MoleculeModule>()->setProjectionMatrix(projMatrix);
    }

    void RenderableMolecule::initMolecule(std::string_view molFile, std::string_view trajFile) {
        LDEBUG(std::format("Loading molecule file '{}'", molFile));

        // free previously loaded molecule
        freeMolecule();

        const md_molecule_t* molecule = mol_manager::load_molecule(molFile, _coarseGrained);
        if (molecule) {
            // We deep copy the contents, so we can freely modify the fields (coordinates etc)
            md_molecule_copy(&_molecule, molecule, default_allocator);

            md_gl_molecule_init(&_gl_molecule, &_molecule);
            updateRepresentations();

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
            LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
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
        _repData.clear();

        md_gl_molecule_free(&_gl_molecule);
        md_molecule_free(&_molecule, default_allocator);

        _molecule = {};
        _trajectory = nullptr;
        _gl_molecule = {};
    }

    RenderableMolecule::RepData::RepData() {
        //md_gl_representation_init(&gl_rep, &mol);
        enabled = true;
        mask = md_bitfield_create(default_allocator);
    }

    RenderableMolecule::RepData::~RepData() {
        //md_gl_representation_free(&gl_rep);
        md_bitfield_free(&mask);
    }

} // namespace openspace
