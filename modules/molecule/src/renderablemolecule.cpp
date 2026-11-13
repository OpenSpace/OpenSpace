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

#include <modules/molecule/src/renderablemolecule.h>

#include <modules/molecule/moleculemodule.h>
#include <modules/molecule/src/cache.h>
#include <modules/molecule/src/util.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/glm.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/profiling.h>
#include <openspace/opengl/gl.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <core/md_allocator.h>
#include <core/md_bitset.h>
#include <md_filter.h>
#include <md_gl.h>
#include <md_util.h>
#include <algorithm>
#include <cmath>
#include <set>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableMolecule";

    enum class AnimationRepeatMode {
        PingPong,
        Wrap,
        Clamp
    };

    void computeMask(md_bitmask_t& mask, std::string_view filter,
                     const md_system_t& sys, const md_system_state_t* state,
                     bool& isDynamic)
    {
        if (!filter.empty() && filter != "all") {
            str_t str = { filter.data(), static_cast<size_t>(filter.length()) };
            md_filter_context_t ctx = {
                .sys = &sys,
                .state = state,
                .ir = nullptr,
                .atoms = nullptr
            };
            md_bitset_t set = {0};
            md_temp_scope_t temp = md_temp_begin();
            md_allocator_i* tempAlloc = md_temp_allocator(temp);
            md_filter_result_t res = md_filter(&set, str, &ctx, tempAlloc);
            if (res.success) {
                md_bitmask_assign(&mask, set);
                isDynamic = res.is_dynamic;
                md_temp_end(temp);
                return;
            }
            md_temp_end(temp);
            LERROR(std::format("Invalid filter expression '{}': {}", filter, res.error));
        }
        md_bitmask_clear(&mask);
        md_bitmask_set_range(&mask, 0, static_cast<uint32_t>(sys.atom.count));
        isDynamic = false;
    }

    double timeToFrame(double time, int64_t numFrames, AnimationRepeatMode mode) {
        double frame = 0;
        const double lastFrame = numFrames - 1.0;
        switch (mode) {
            case AnimationRepeatMode::PingPong:
                frame = std::fmod(time, 2.0 * lastFrame);
                if (frame > lastFrame) {
                    frame = 2.0 * lastFrame - frame;
                }
                break;
            case AnimationRepeatMode::Wrap:
                frame = std::fmod(time, numFrames);
                break;
            case AnimationRepeatMode::Clamp:
                frame = std::clamp(time, 0.0, lastFrame - 1.0);
                break;
        }
        return frame;
    }

    constexpr StringPropertyInfo MoleculeFileInfo = {
        .identifier = "MoleculeFile",
        .name = "Molecule file",
        .description = "The path to the file from which the molecular structure is read.",
        .visibility = Visibility::AdvancedUser,
        .isReadOnly = IsReadOnly::Yes
    };

    constexpr StringPropertyInfo TrajectoryFileInfo = {
        .identifier = "TrajectoryFile",
        .name = "Trajectory file",
        .description = "The path to the file from which the trajectory information is "
            "read.",
        .visibility = Visibility::AdvancedUser,
        .isReadOnly = IsReadOnly::Yes
    };

    constexpr BoolPropertyInfo CoarseGrainedInfo = {
        .identifier = "CoarseGrained",
        .name = "Coarse grained",
        .description = "Enable if the dataset should be interpreted as coarse grained.",
        .visibility = Visibility::AdvancedUser,
        .isReadOnly = IsReadOnly::Yes,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo ApplyPbcOnLoadInfo = {
        .identifier = "ApplyPbcOnLoad",
        .name = "Apply PBC on load",
        .description = "Applies Periodic Boundary Constraints upon loading trajectory "
            "frames.",
        .visibility = Visibility::AdvancedUser,
        .isReadOnly = IsReadOnly::Yes,
        .defaults = {
            .value = true
        }
    };

    constexpr BoolPropertyInfo ApplyPbcPerFrameInfo = {
        .identifier = "ApplyPbcPerFrame",
        .name = "Apply PBC per frame",
        .description = "Applies Periodic Boundary Constraints for each interpolated "
            "frame (can be CPU-intensive).",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = false
        }
    };

    constexpr BoolPropertyInfo EnabledInfo = {
        .identifier = "Enabled",
        .name = "Enabled",
        .description = "Enables the representation.",
        .visibility = Visibility::User,
    };

    constexpr OptionPropertyInfo TypeInfo = {
        .identifier = "Type",
        .name = "Type",
        .description = "The visual representation type to use for the molecule.",
        .visibility = Visibility::User
    };

    constexpr OptionPropertyInfo ColorInfo = {
        .identifier = "Color",
        .name = "Color",
        .description = "The color mapping for the atoms.",
        .visibility = Visibility::User
    };

    constexpr Vec4PropertyInfo UniformColorInfo = {
        .identifier = "UniformColor",
        .name = "Uniform color",
        .description = "The uniform color to apply for the representation if that color "
            "mode is selected.",
        .visibility = Visibility::User,
        .defaults = {
            .value = glm::vec4(0.f),
            .minMax = { glm::vec4(0.f), glm::vec4(1.f) }
        }
    };

    constexpr StringPropertyInfo FilterInfo = {
        .identifier = "Filter",
        .name = "Filter",
        .description = "The filter used to remove parts of the dataset.",
        .visibility = Visibility::AdvancedUser
    };

    constexpr FloatPropertyInfo ScaleInfo = {
        .identifier = "Scale",
        .name = "Scale",
        .description = "Scale for the geometric representation of atoms.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.f,
            .minMax = { 0.f, 10.f }
        }
    };

    constexpr DoublePropertyInfo AnimationBaseScaleInfo = {
        .identifier = "AnimationBaseScale",
        .name = "Animation base scale",
        .description = "Base scale for the animation. Tune this to sync up its animation "
            "with other trajectories.",
        .visibility = Visibility::AdvancedUser,
        .isReadOnly = IsReadOnly::Yes,
        .defaults = {
            .value = 1.0,
            .minMax = { 0.0, 1e20 }
        }
    };

    constexpr DoublePropertyInfo AnimationSpeedInfo = {
        .identifier = "AnimationSpeed",
        .name = "Animation speed",
        .description = "Playback speed of the animation (in frames per second).",
        .visibility = Visibility::AdvancedUser,
        .defaults = {
            .value = 1.0,
            .minMax = { -100.0, 100.0 }
        }
    };

    constexpr OptionPropertyInfo AnimationRepeatModeInfo = {
        .identifier = "AnimationRepeatMode",
        .name = "Animation repeat mode",
        .description = "Controls how the animation should be repeated when the end of "
            "the animation is reached.",
        .visibility = Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        std::filesystem::path moleculeFile [[codegen::Info(MoleculeFileInfo)]];

        std::optional<std::filesystem::path> trajectoryFile
            [[codegen::Info(TrajectoryFileInfo)]];

        std::optional<bool> coarseGrained [[codegen::Info(CoarseGrainedInfo)]];

        struct Representation {
            enum class [[codegen::map(molecule::rep::Type)]] Type {
                SpaceFill,
                Licorice,
                Ribbons,
                Cartoon
            };

            enum class [[codegen::map(molecule::rep::Color)]] Color {
                Cpk,
                AtomIndex,
                ResId,
                ResIndex,
                ChainId,
                ChainIndex,
                SecondaryStructure,
                Uniform
            };

            std::optional<bool> enabled;
            std::optional<Type> type;
            std::optional<Color> color;
            std::optional<std::string> filter;
            std::optional<float> scale;
            std::optional<glm::vec4> uniformColor;
        };

        std::optional<std::vector<Representation>> representations;

        std::optional<bool> applyPbcOnLoad [[codegen::Info(ApplyPbcOnLoadInfo)]];

        std::optional<bool> applyPbcPerFrame [[codegen::Info(ApplyPbcPerFrameInfo)]];

        std::optional<double> animationBaseScale
            [[codegen::Info(AnimationBaseScaleInfo)]];

        std::optional<double> animationSpeed [[codegen::Info(AnimationSpeedInfo)]];

        enum class [[codegen::map(AnimationRepeatMode)]] AnimationRepeatMode {
            PingPong,
            Wrap,
            Clamp
        };

        std::optional<AnimationRepeatMode> animationRepeatMode
            [[codegen::Info(AnimationRepeatModeInfo)]];
    };
} // namespace
#include "renderablemolecule_codegen.cpp"

namespace openspace {

Documentation RenderableMolecule::Documentation() {
    return codegen::doc<Parameters>(
        "molecule_renderable_molecule",
        Renderable::Documentation()
    );
}

RenderableMolecule::RenderableMolecule(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _localEpoch(global::timeManager->time().j2000Seconds())
    , _repProps({ "Representations" })
    , _moleculeFile(MoleculeFileInfo)
    , _trajectoryFile(TrajectoryFileInfo)
    , _coarseGrained(CoarseGrainedInfo)
    , _applyPbcOnLoad(ApplyPbcOnLoadInfo)
    , _applyPbcPerFrame(ApplyPbcPerFrameInfo)
    , _animationBaseScale(AnimationBaseScaleInfo)
    , _animationSpeed(AnimationSpeedInfo)
    , _animationRepeatMode(AnimationRepeatModeInfo)
{
    addProperty(Fadeable::_opacity);

    Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFile = p.moleculeFile.string();
    addProperty(_moleculeFile);

    if (p.trajectoryFile.has_value()) {
        _trajectoryFile = p.trajectoryFile->string();
    }
    addProperty(_trajectoryFile);

    _coarseGrained = p.coarseGrained.value_or(_coarseGrained);
    addProperty(_coarseGrained);

    _applyPbcOnLoad = p.applyPbcOnLoad.value_or(_applyPbcOnLoad);
    addProperty(_applyPbcOnLoad);

    if (!p.representations.has_value()) {
        Parameters::Representation rep = {
            .enabled = true,
            .type = Parameters::Representation::Type::SpaceFill,
            .color = Parameters::Representation::Color::Cpk,
            .filter = "",
            .scale = 1.f,
            .uniformColor = glm::vec4(1.f)
        };
        p.representations = { rep };
    }

    for (Parameters::Representation& rep : *p.representations) {
        rep.type = rep.type.value_or(Parameters::Representation::Type::SpaceFill);
        rep.color = rep.color.value_or(Parameters::Representation::Color::Cpk);

        auto r = std::make_unique<Representation>(
            _repProps.propertySubOwners().size(),
            nullptr,
            rep.enabled.value_or(true),
            codegen::map<molecule::rep::Type>(*rep.type),
            codegen::map<molecule::rep::Color>(rep.color.value()),
            rep.filter.value_or(""),
            rep.scale.value_or(1.f),
            rep.uniformColor.value_or(glm::vec4(1.f))
        );
        _repProps.addPropertySubOwner(r.get());
        _repData.push_back(std::move(r));
    }

    _animationBaseScale = p.animationBaseScale.value_or(_animationBaseScale);
    addProperty(_animationBaseScale);

    addPropertySubOwner(_repProps);

    _applyPbcPerFrame = p.applyPbcPerFrame.value_or(_applyPbcPerFrame);
    addProperty(_applyPbcPerFrame);

    _animationSpeed = p.animationSpeed.value_or(_animationSpeed);
    _animationSpeed.setReadOnly(!p.trajectoryFile.has_value());
    addProperty(_animationSpeed);

    _animationRepeatMode.addOptions({
        { static_cast<int>(AnimationRepeatMode::PingPong), "PingPong" },
        { static_cast<int>(AnimationRepeatMode::Wrap), "Wrap" },
        { static_cast<int>(AnimationRepeatMode::Clamp), "Clamp" }
    });
    _animationRepeatMode = static_cast<int>(
        codegen::map<AnimationRepeatMode>(
            p.animationRepeatMode.value_or(Parameters::AnimationRepeatMode::PingPong)
        )
    );
    _animationRepeatMode.setReadOnly(!p.trajectoryFile.has_value());
    addProperty(_animationRepeatMode);
}

RenderableMolecule::~RenderableMolecule() {}

void RenderableMolecule::initializeGL() {
    global::moduleEngine->module<MoleculeModule>()->initializeShaders();
    initMolecule(_moleculeFile, _trajectoryFile);
}

void RenderableMolecule::deinitializeGL() {
    global::moduleEngine->module<MoleculeModule>()->deinitializeShaders();

    for (const std::unique_ptr<Representation>& rep : _repData) {
        if (rep->glRep.id) {
            md_gl_rep_destroy(rep->glRep);
            rep->glRep = {};
        }
    }

    if (_glMolecule.id) {
        md_gl_mol_destroy(_glMolecule);
        _glMolecule = {};
    }

    if (_state.alloc) {
        md_system_state_free(&_state);
    }

    _system = nullptr;
    _trajectory = nullptr;
}

void RenderableMolecule::update(const UpdateData& data) {
    if (!_renderableInView) {
        return;
    }

    _renderableInView = false;

    if (_trajectory) {
        updateTrajectoryFrame(data);
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
    ZoneScoped;

    const glm::dvec3 frwd = data.modelTransform.translation - data.camera.position();
    const glm::dvec3 dir = data.camera.viewDirectionWorldSpace();
    const double distance = glm::length(frwd) * sign(glm::dot(dir, frwd));

    if (distance < 0.0 || distance > 10000.0) {
        return;
    }

    _renderableInView = true;

    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    const glm::mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale) *
        glm::dmat4(data.modelTransform.rotation);
    const glm::mat4 projMatrix = glm::dmat4(camCopy.sgctInternal.projectionMatrix());

    vec3_t trans = { 0.f, 0.f, 0.f };
    if (_state.unitcell.flags != 0) {
        trans.x = static_cast<float>(
            _state.unitcell.x + _state.unitcell.xy + _state.unitcell.xz
        ) * 0.5f;
        trans.y = static_cast<float>(_state.unitcell.y + _state.unitcell.yz) * 0.5f;
        trans.z = static_cast<float>(_state.unitcell.z) * 0.5f;
    }
    const mat4_t modelMat = mat4_translate(-trans.x, -trans.y, -trans.z);

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve(_repData.size());

    if (_system && _system->atom.count > 0) {
        for (const std::unique_ptr<Representation>& rep : _repData) {
            if (rep->enabled) {
                md_gl_draw_op_t op = {};
                op.rep = rep->glRep;
                op.model_matrix = &modelMat[0][0];
                molecule::setDrawOpTypeAndArgs(
                    op,
                    static_cast<molecule::rep::Type>(rep->type.value()),
                    rep->scale
                );
                drawOps.push_back(op);
            }
        }
    }

    const md_gl_draw_args_t args = {
        .shaders = global::moduleEngine->module<MoleculeModule>()->shaders(),
        .draw_operations = {
            .count = drawOps.size(),
            .ops = drawOps.data()
        },
        .view_transform = {
            .view_matrix = glm::value_ptr(viewMatrix),
            .proj_matrix = glm::value_ptr(projMatrix),
            .prev_view_matrix = nullptr,
            .prev_proj_matrix = nullptr
        },
        .picking_offset = {},
        .atom_mask = 0,
        .max_bond_length = 0.0f
    };

    GLint lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);

    GLint lastDrawBufferCount = 0;
    std::array<GLenum, 8> lastDrawBuffers;
    for (int i = 0; i < static_cast<int>(lastDrawBuffers.size()); i++) {
        GLint drawBuf;
        glGetIntegerv(GL_DRAW_BUFFER0 + i, &drawBuf);
        if (!drawBuf) {
            break;
        }
        lastDrawBuffers[lastDrawBufferCount] = static_cast<GLenum>(drawBuf);
        lastDrawBufferCount++;
    }

    GLuint fbo = global::moduleEngine->module<MoleculeModule>()->fbo();
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    md_gl_draw(&args);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
    glDrawBuffers(lastDrawBufferCount, lastDrawBuffers.data());
}

void RenderableMolecule::initMolecule(std::string_view molFile, std::string_view trajFile)
{
    ZoneScoped;

    LDEBUG(std::format("Loading molecule file '{}'", molFile));

    const molecule::Molecule* mol = molecule::loadMolecule(molFile, _coarseGrained);
    if (!mol) {
        throw RuntimeError("Failed to initialize molecule: Failed to load file");
    }

    _system = &mol->system;
    _state.alloc = md_get_heap_allocator();
    md_system_state_copy(&_state, &mol->state);

    _glMolecule = md_gl_mol_create(_system);

    for (const std::unique_ptr<Representation>& rep : _repData) {
        if (rep->glRep.id) {
            md_gl_rep_destroy(rep->glRep);
        }
        rep->glRep = md_gl_rep_create(_glMolecule);

        rep->system = _system;
        md_bitmask_set_domain(&rep->mask, 0, static_cast<uint32_t>(_system->atom.count));

        if (rep->enabled) {
            computeMask(rep->mask, rep->filter, *_system, &_state, rep->isDynamic);

            molecule::rep::Color color =
                static_cast<molecule::rep::Color>(rep->color.value());
            molecule::updateRepColor(
                rep->glRep,
                *_system,
                color,
                rep->mask,
                rep->uniformColor,
                nullptr
            );
        }
    }

    float aabbMin[3];
    float aabbMax[3];
    md_util_aabb_compute(
        aabbMin,
        aabbMax,
        _state.xyz,
        nullptr,
        nullptr,
        _system->atom.count
    );
    const vec3_t minPt = { aabbMin[0], aabbMin[1], aabbMin[2] };
    const vec3_t maxPt = { aabbMax[0], aabbMax[1], aabbMax[2] };
    _radius = vec3_distance(minPt, maxPt) * 0.5f;
    setBoundingSphere(_radius * 2.f);

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
        _trajectory = molecule::loadTrajectory(trajFile, mol, _applyPbcOnLoad);
    }
}

void RenderableMolecule::updateTrajectoryFrame(const UpdateData& data) {
    const double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
    const double currT = data.time.j2000Seconds();

    const double scl = _animationBaseScale * _animationSpeed;
    const double currTime = (currT - _localEpoch) * scl;
    const double nextTime = (currT + dt - _localEpoch) * scl;

    const int64_t numFrames = molecule::trajectoryNumFrames(_trajectory);
    const AnimationRepeatMode mode = static_cast<AnimationRepeatMode>(
        _animationRepeatMode.value()
    );
    const double frame = timeToFrame(currTime, numFrames, mode);

    if (frame != _frame) {
        _frame = frame;
        molecule::interpolateFrame(
            _state,
            _trajectory,
            molecule::InterpolationType::Cubic,
            frame,
            _applyPbcPerFrame
        );

        md_gl_mol_set_atom_position(
            _glMolecule,
            0,
            static_cast<uint32_t>(_system->atom.count),
            _state.xyz
        );

        const int64_t nearestFrame =
            std::clamp<int64_t>(static_cast<int64_t>(frame + 0.5), 0, numFrames - 1);
        molecule::FrameData frameData = molecule::frameData(_trajectory, nearestFrame);

        std::vector<Representation*> repHasSsIndices;
        std::vector<Representation*> repUpdateColIndices;

        for (const std::unique_ptr<Representation>& rep : _repData) {
            if (!rep->enabled) {
                continue;
            }

            molecule::rep::Color color =
                static_cast<molecule::rep::Color>(rep->color.value());
            if (color == molecule::rep::Color::SecondaryStructure) {
                repHasSsIndices.push_back(rep.get());
            }
            if (rep->isDynamic) {
                computeMask(rep->mask, rep->filter, *_system, &_state, rep->isDynamic);
                repUpdateColIndices.push_back(rep.get());
            }
        }

        if (!repHasSsIndices.empty()) {
            if (_system->protein_backbone.segment.count > 0 && !frameData.ss.empty()) {
                std::vector<md_gl_secondary_structure_t> glSs(
                    _system->protein_backbone.segment.count
                );
                for (size_t i = 0; i < glSs.size(); ++i) {
                    glSs[i] = md_gl_secondary_structure_convert(frameData.ss[i]);
                }
                md_gl_mol_set_backbone_secondary_structure(
                    _glMolecule,
                    0,
                    static_cast<uint32_t>(glSs.size()),
                    glSs.data(),
                    0
                );
            }
            repUpdateColIndices.insert(
                repUpdateColIndices.end(),
                repHasSsIndices.begin(),
                repHasSsIndices.end()
            );
        }

        for (Representation* rep : repUpdateColIndices) {
            molecule::updateRepColor(
                rep->glRep,
                *_system,
                static_cast<molecule::rep::Color>(rep->color.value()),
                rep->mask,
                rep->uniformColor,
                frameData.ss.data()
            );
        }
    }

    using FrameSet = std::array<int64_t, 4>;

    auto frameSet = [](double time, int64_t nFrames, AnimationRepeatMode m) {
        const int64_t frameIdx = static_cast<int64_t>(timeToFrame(time, nFrames, m));

        auto wrap = [](int64_t idx, int64_t nf) -> int64_t {
            idx = idx < 0 ? nf - 1 : idx;
            idx = idx >= nf ? 0 : idx;
            return idx;
        };

        return FrameSet {
            wrap(frameIdx - 1, nFrames),
            wrap(frameIdx + 0, nFrames),
            wrap(frameIdx + 1, nFrames),
            wrap(frameIdx + 2, nFrames)
        };
    };

    // Prefetch next set of frames
    FrameSet curr = frameSet(currTime, numFrames, mode);
    FrameSet nxt1 = frameSet(nextTime, numFrames, mode);

    std::set<int64_t> frames;
    frames.insert(nxt1.begin(), nxt1.end());
    for (int64_t i : curr) {
        frames.erase(i);
    }

    std::vector<int64_t> f(frames.begin(), frames.end());
    molecule::prefetchFrames(_trajectory, f);
}

RenderableMolecule::Representation::Representation(size_t number,
                                                   const md_system_t* system_,
                                                   bool enabled_,
                                                   molecule::rep::Type type_,
                                                   molecule::rep::Color color_,
                                                   std::string filter_, float scale_,
                                                   glm::vec4 uniformColor_)
    : PropertyOwner({
        .identifier = std::format("rep{}", number),
        .name = std::format("Representation {}", number),
        .description = std::format("Visual representation of molecule {}", number),
    })
    , mask(md_bitmask_create(0, 0, md_get_heap_allocator()))
    , system(system_)
    , enabled(EnabledInfo)
    , type(TypeInfo)
    , color(ColorInfo)
    , filter(FilterInfo)
    , scale(ScaleInfo)
    , uniformColor(UniformColorInfo)
{
    enabled = enabled_;
    filter = std::move(filter_);
    scale = scale_;
    uniformColor = uniformColor_;

    addProperty(enabled);

    type.addOptions({
        { static_cast<int>(molecule::rep::Type::SpaceFill), "SpaceFill" },
        { static_cast<int>(molecule::rep::Type::Licorice), "Licorice" },
        { static_cast<int>(molecule::rep::Type::Ribbons), "Ribbons" },
        { static_cast<int>(molecule::rep::Type::Cartoon), "Cartoon" }
    });
    type = static_cast<int>(type_);
    type.onChange([this]() {
        // Representation type/scale is applied during draw operation construction
    });
    addProperty(type);

    color.addOptions({
        { static_cast<int>(molecule::rep::Color::Cpk), "CPK" },
        { static_cast<int>(molecule::rep::Color::AtomIndex), "Atom Index" },
        { static_cast<int>(molecule::rep::Color::ResId), "Residue ID" },
        { static_cast<int>(molecule::rep::Color::ResIndex), "Residue Index" },
        { static_cast<int>(molecule::rep::Color::ChainId), "Chain ID" },
        { static_cast<int>(molecule::rep::Color::ChainIndex), "Chain Index" },
        {
            static_cast<int>(molecule::rep::Color::SecondaryStructure),
            "Secondary Structure"
        },
        { static_cast<int>(molecule::rep::Color::Uniform), "Uniform" }
    });
    color = static_cast<int>(color_);
    color.onChange([this]() {
        if (system && system->atom.count > 0 && glRep.id) {
            molecule::updateRepColor(
                glRep,
                *system,
                static_cast<molecule::rep::Color>(color.value()),
                mask,
                uniformColor
            );
        }
    });
    addProperty(color);

    filter.onChange([this]() {
        if (system && system->atom.count > 0 && glRep.id) {
            computeMask(mask, filter, *system, nullptr, isDynamic);
            molecule::updateRepColor(
                glRep,
                *system,
                static_cast<molecule::rep::Color>(color.value()),
                mask,
                uniformColor
            );
        }
    });
    addProperty(filter);

    scale.onChange([this]() {
        // Scale is applied during draw operation construction
    });
    addProperty(scale);

    uniformColor.setViewOption(Property::ViewOptions::Color);
    uniformColor.onChange([this]() {
        if (system && system->atom.count > 0 && glRep.id) {
            molecule::updateRepColor(
                glRep,
                *system,
                static_cast<molecule::rep::Color>(color.value()),
                mask,
                uniformColor
            );
        }
    });
    addProperty(uniformColor);
}

RenderableMolecule::Representation::~Representation() {
    md_bitmask_free(&mask);
}

} // namespace openspace
