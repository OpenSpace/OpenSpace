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
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <md_filter.h>
#include <md_util.h>

namespace {
    constexpr std::string_view _loggerCat = "RenderableMolecule";

    enum class AnimationRepeatMode {
        PingPong,
        Wrap,
        Clamp
    };

    void computeMask(md_bitfield_t& mask, std::string_view filter,
                     const md_molecule_t& mol, bool& isDynamic)
    {
        if (!filter.empty() && filter != "all") {
            str_t str = { filter.data(), static_cast<int64_t>(filter.length()) };
            char errBuf[1024];

            const bool success =
                md_filter(&mask, str, &mol, nullptr, &isDynamic, errBuf, sizeof(errBuf));
            if (success) {
                return;
            }
            LERROR(std::format("Invalid filter expression '{}': {}", filter, errBuf));
        }
        md_bitfield_clear(&mask);
        md_bitfield_set_range(&mask, 0, mol.atom.count);
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

    constexpr openspace::properties::Property::PropertyInfo MoleculeFileInfo = {
        "MoleculeFile",
        "Molecule File",
        "The path to the file from which the molecular structure is read."
    };

    constexpr openspace::properties::Property::PropertyInfo TrajectoryFileInfo = {
        "TrajectoryFile",
        "Trajectory File",
        "The path to the file from which the trajectory informatoin is read."
    };

    constexpr openspace::properties::Property::PropertyInfo CoarseGrainedInfo = {
        "CoarseGrained",
        "Coarse Grained",
        "Enable if the dataset should be interpreted as coarse grained."
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyPbcOnLoadInfo = {
        "ApplyPbcOnLoad",
        "Apply PBC On Load",
        "Applies Periodic Boundary Constraints upon loading trajectory frames."
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyPbcPerFrameInfo = {
        "ApplyPbcPerFrame",
        "Apply PBC Per Frame",
        "Applies Periodic Boundary Constraints for each interpolated frame (can be "
        "CPU-intensive)."
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Enables the representation"
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "Visual representation type of the molecule."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "Select a color mapping for the atoms."
    };

    constexpr openspace::properties::Property::PropertyInfo UniformColorInfo = {
        "UniformColor",
        "Uniform Color",
        "The uniform color to apply for the representation if that color mode is "
        "selected."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterInfo = {
        "Filter",
        "Filter",
        "The filter used to remove parts of the dataset."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "Scale for the geometric representation of atoms."
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationBaseScaleInfo = {
        "AnimationBaseScale",
        "Animation Base Scale",
        "Base scale for the animation, tune this to sync up its animation with other "
        "trajectories."
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Playback speed of the animation (in frames per second)."
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationRepeatModeInfo = {
        "AnimationRepeatMode",
        "Animation Repeat Mode",
        "Controls how the animation should be repeated when the end of the animation is "
        "reached."
    };

    /**
     * This Renderable class is used to render a single molecular system, which can be
     * either static or dynamic. The rendering is done using the rendering engine of the
     * [ViaMD](https://github.com/scanberg/viamd) framework. Many of the parameters are
     * described in greater detail on their [Wiki](https://github.com/scanberg/viamd/wiki)
     * page. It is possible to assign multiple representations to a molecular structure
     * and specify individual settings per representation, including the ability to filter
     * using ViaMD's powerful filtering scripting language.
     *
     * The current implementation supports the loading of "PDB", "Gromacs", "XYZ", "XMOL",
     * and "ARC" files for the molecular structure and "PDB", "XTC", "TRR", "XYZ", "XMOL",
     * and "ARC" files for the optional trajectories.
     *
     * If no trajectory file is provided only the structural information is shown.
     */
    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::optional<std::string> trajectoryFile;

        // [[codegen::verbatim(CoarseGrainedInfo.description)]]
        std::optional<bool> coarseGrained;

        struct Representation {
            enum class [[codegen::map(mol::rep::Type)]] Type {
                SpaceFill,
                Licorice,
                Ribbons,
                Cartoon
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
                Uniform
            };

            std::optional<bool> enabled;
            std::optional<Type> type;
            std::optional<Color> color;
            std::optional<std::string> filter;
            std::optional<float> scale;
            std::optional<glm::vec4> uniformColor;
        };

        // Repro
        std::optional<std::vector<Representation>> representations;

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
} // namespace

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablemolecule");
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _localEpoch(global::timeManager->time().j2000Seconds())
    , _repProps({ "Representations" })
    , _moleculeFile(MoleculeFileInfo)
    , _trajectoryFile(TrajectoryFileInfo)
    , _coarseGrained(CoarseGrainedInfo, false)
    , _applyPbcOnLoad(ApplyPbcOnLoadInfo, true)
    , _applyPbcPerFrame(ApplyPbcPerFrameInfo, false)
    , _animationBaseScale(AnimationBaseScaleInfo, 1.0, 0.0, 1e20)
    , _animationSpeed(AnimationSpeedInfo, 1.0, -100.0, 100.0)
    , _animationRepeatMode(AnimationRepeatModeInfo)
{
    Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFile = p.moleculeFile;
    _moleculeFile.setReadOnly(true);
    addProperty(_moleculeFile);

    _trajectoryFile = p.trajectoryFile.value_or(_trajectoryFile);
    _trajectoryFile.setReadOnly(true);
    addProperty(_trajectoryFile);

    _coarseGrained = p.coarseGrained.value_or(_coarseGrained);
    _coarseGrained.setReadOnly(true);
    addProperty(_coarseGrained);

    _applyPbcOnLoad = p.applyPbcOnLoad.value_or(_applyPbcOnLoad);
    _applyPbcOnLoad.setReadOnly(true);
    addProperty(_applyPbcOnLoad);

    if (!p.representations.has_value()) {
        // Add a default representation if none were supplied
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
            _molecule,
            rep.enabled.value_or(true),
            codegen::map<mol::rep::Type>(*rep.type),
            codegen::map<mol::rep::Color>(rep.color.value()),
            rep.filter.value_or(""),
            rep.scale.value_or(1.f),
            rep.uniformColor.value_or(glm::vec4(1.f))
        );
        _repProps.addPropertySubOwner(r.get());
        _repData.push_back(std::move(r));
    }

    _animationBaseScale = p.animationBaseScale.value_or(_animationBaseScale);
    _animationBaseScale.setReadOnly(true);
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
        { static_cast<int>(AnimationRepeatMode::Clamp), "Clamp" },
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
        md_gl_representation_free(&rep->glRep);
    }

    md_gl_molecule_free(&_glMolecule);
    md_molecule_free(&_molecule, default_allocator);

    _molecule = {};
    _trajectory = nullptr;
    _glMolecule = {};
}

bool RenderableMolecule::isReady() const {
    return true;
}

void RenderableMolecule::update(const UpdateData& data) {
    // avoid updating if not in view, as it can be quite expensive.
    if (!_renderableInView) {
        return;
    }

    _renderableInView = false;

    // update animation
    if (_trajectory) {
        updateTrajectoryFrame(data);
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
    ZoneScoped

    // compute distance from camera to molecule
    const glm::dvec3 frwd = data.modelTransform.translation - data.camera.positionVec3();
    const glm::dvec3 dir = data.camera.viewDirectionWorldSpace();
    // "signed" distance from camera to object
    const double distance = glm::length(frwd) * sign(glm::dot(dir, frwd));

    // distance < 0 means behind the camera, 10000 is arbitrary
    if (distance < 0.0 || distance > 10000.0) {
        return;
    }

    _renderableInView = true;

    // because the molecule is small, a scaling of the view matrix causes the molecule
    // to be moved out of view in clip space. Resetting the scaling for the molecule
    // is fine for now. This will have an impact on stereoscopic depth though
    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    const glm::mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale) *
        glm::dmat4(data.modelTransform.rotation);
    const glm::mat4 projMatrix = glm::dmat4(camCopy.sgctInternal.projectionMatrix());

    // Center the molecule with respect to its cell
    const vec3_t trans = _molecule.cell.basis * vec3_set1(0.5f);
    const mat4_t modelMat = mat4_translate(-trans.x, -trans.y, -trans.z);

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve(_repData.size());

    if (_molecule.atom.count > 0) {
        for (const std::unique_ptr<Representation>& rep : _repData) {
            if (rep->enabled) {
                drawOps.emplace_back(&rep->glRep, &modelMat[0][0]);
            }
        }
    }

    const md_gl_draw_args_t args = {
        .shaders = &global::moduleEngine->module<MoleculeModule>()->shaders(),
        .draw_operations = { static_cast<uint32_t>(drawOps.size()), drawOps.data() },
        .view_transform = {
            glm::value_ptr(viewMatrix),
            glm::value_ptr(projMatrix),
            nullptr,
            nullptr
        },
        .options = 0
    };

    GLint lastFbo;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);

    GLint lastDrawBufferCount = 0;
    std::array<GLenum, 8> lastDrawBuffers;
    for (int i = 0; i < lastDrawBuffers.size(); i++) {
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
    ZoneScoped

    LDEBUG(std::format("Loading molecule file '{}'", molFile));

    const md_molecule_t* molecule = mol::loadMolecule(molFile, _coarseGrained);
    if (!molecule) {
        throw ghoul::RuntimeError("Failed to initialize molecule: Failed to load file");
    }

    // We deep copy the contents, so we can freely modify the fields (coordinates etc)
    md_molecule_copy(&_molecule, molecule, default_allocator);
    md_gl_molecule_init(&_glMolecule, &_molecule);

    // Update representations
    for (const std::unique_ptr<Representation>& rep : _repData) {
        md_gl_representation_free(&rep->glRep);

        rep->glRep = { 0 };
        md_gl_representation_init(&rep->glRep, &_glMolecule);

        if (rep->enabled) {
            computeMask(rep->mask, rep->filter, _molecule, rep->isDynamic);

            mol::rep::Type type = static_cast<mol::rep::Type>(rep->type.value());
            mol::rep::Color color = static_cast<mol::rep::Color>(rep->color.value());
            mol::util::updateRepType(rep->glRep, type, rep->scale);
            mol::util::updateRepColor(
                rep->glRep,
                _molecule,
                color,
                rep->mask,
                rep->uniformColor
            );
        }
    }

    vec3_t aabbMin;
    vec3_t aabbMax;
    md_util_compute_aabb_xyzr(
        &aabbMin,
        &aabbMax,
        _molecule.atom.x,
        _molecule.atom.y,
        _molecule.atom.z,
        _molecule.atom.radius,
        _molecule.atom.count
    );
    _radius = vec3_distance(aabbMin, aabbMax) * 0.5f;
    setBoundingSphere(_radius * 2.f);

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
        _trajectory = mol::loadTrajectory(trajFile, molecule, _applyPbcOnLoad);
    }
}

void RenderableMolecule::updateTrajectoryFrame(const UpdateData& data) {
    double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
    double currT = data.time.j2000Seconds();

    const double scl = _animationBaseScale * _animationSpeed;
    double currTime = (currT - _localEpoch) * scl;
    double nextTime = (currT + dt - _localEpoch) * scl;

    const int64_t numFrames = md_trajectory_num_frames(_trajectory);
    AnimationRepeatMode mode = static_cast<AnimationRepeatMode>(
        _animationRepeatMode.value()
    );
    double frame = timeToFrame(currTime, numFrames, mode);

    if (frame != _frame) {
        _frame = frame;
        mol::util::interpolateFrame(
            _molecule,
            _trajectory,
            mol::util::InterpolationType::Cubic,
            frame,
            _applyPbcPerFrame
        );

        const uint64_t size = _molecule.atom.count * sizeof(vec3_t);
        md_allocator_i* alloc =
            default_temp_allocator_max_allocation_size() < size ?
            default_allocator :
            default_temp_allocator;
        vec3_t* xyz = reinterpret_cast<vec3_t*>(md_alloc(alloc, size));

        for (int64_t i = 0; i < _molecule.atom.count; i++) {
            xyz[i] = { _molecule.atom.x[i], _molecule.atom.y[i], _molecule.atom.z[i] };
        }
        md_gl_molecule_set_atom_position_xyz(
            &_glMolecule,
            0,
            static_cast<uint32_t>(_molecule.atom.count),
            xyz
        );

        md_free(alloc, xyz, size);

        std::vector<Representation*> repHasSsIndices;
        std::vector<Representation*> repUpdateColIndices;

        for (const std::unique_ptr<Representation>& rep : _repData) {
            if (!rep->enabled) {
                continue;
            }

            mol::rep::Color color = static_cast<mol::rep::Color>(rep->color.value());
            if (color == mol::rep::Color::SecondaryStructure) {
                repHasSsIndices.push_back(rep.get());
            }
            if (rep->isDynamic) {
                computeMask(rep->mask, rep->filter, _molecule, rep->isDynamic);
                repUpdateColIndices.push_back(rep.get());
            }
        }

        if (!repHasSsIndices.empty()) {
            md_gl_molecule_set_backbone_secondary_structure(
                &_glMolecule,
                0,
                static_cast<uint32_t>(_molecule.backbone.count),
                _molecule.backbone.secondary_structure,
                0
            );
            repUpdateColIndices.insert(
                repUpdateColIndices.end(),
                repHasSsIndices.begin(),
                repHasSsIndices.end()
            );
        }

        for (Representation* rep : repUpdateColIndices) {
            mol::util::updateRepColor(
                rep->glRep,
                _molecule,
                static_cast<mol::rep::Color>(rep->color.value()),
                rep->mask,
                rep->uniformColor
            );
        }
    }

    using FrameSet = std::array<int64_t, 4>;

    auto frameSet = [](double time, int64_t numFrames, AnimationRepeatMode mode) {
        const int64_t frameIdx = static_cast<int64_t>(timeToFrame(time, numFrames, mode));

        auto wrap = [](int64_t idx, int64_t numFrames) -> int64_t {
            idx = idx < 0 ? numFrames - 1 : idx;
            idx = idx >= numFrames ? 0 : idx;
            return idx;
        };

        return FrameSet {
            wrap(frameIdx - 1, numFrames),
            wrap(frameIdx + 0, numFrames),
            wrap(frameIdx + 1, numFrames),
            wrap(frameIdx + 2, numFrames)
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

    std::vector<int64_t> f =
        std::vector<int64_t>(frames.begin(), frames.end());
    mol::prefetchFrames(_trajectory, f);
}

RenderableMolecule::Representation::Representation(size_t number,
                                                   const md_molecule_t& molecule_,
                                                   bool enabled_,
                                                   mol::rep::Type type_,
                                                   mol::rep::Color color_,
                                                   std::string filter_,
                                                   float scale_,
                                                   glm::vec4 uniformColor_)
    : properties::PropertyOwner({
        .identifier = std::format("rep{}", number),
        .guiName = std::format("Representation {}", number),
        .description = std::format("Visual representation of molecule {}", number),
    })
    , mask(md_bitfield_create(default_allocator))
    , molecule(molecule_)
    , enabled(EnabledInfo, enabled_)
    , type(TypeInfo)
    , color(ColorInfo)
    , filter(FilterInfo, filter_)
    , scale(ScaleInfo, scale_, 0.f, 10.f)
    , uniformColor(UniformColorInfo, uniformColor_, glm::vec4(0.f), glm::vec4(1.f))
{
    addProperty(enabled);

    type.addOptions({
        { static_cast<int>(mol::rep::Type::SpaceFill), "SpaceFill" },
        { static_cast<int>(mol::rep::Type::Licorice), "Licorice" },
        { static_cast<int>(mol::rep::Type::Ribbons), "Ribbons" },
        { static_cast<int>(mol::rep::Type::Cartoon), "Cartoon" }
    });
    type = static_cast<int>(type_);
    type.onChange([&]() {
        mol::util::updateRepType(glRep, static_cast<mol::rep::Type>(type.value()), scale);
    });
    addProperty(type);

    color.addOptions({
        { static_cast<int>(mol::rep::Color::Cpk), "CPK" },
        { static_cast<int>(mol::rep::Color::AtomIndex), "Atom Index" },
        { static_cast<int>(mol::rep::Color::ResId), "Residue ID" },
        { static_cast<int>(mol::rep::Color::ResIndex), "Residue Index" },
        { static_cast<int>(mol::rep::Color::ChainId), "Chain ID" },
        { static_cast<int>(mol::rep::Color::ChainIndex), "Chain Index" },
        { static_cast<int>(mol::rep::Color::SecondaryStructure), "Secondary Structure" },
        { static_cast<int>(mol::rep::Color::Uniform), "Uniform" }
    });
    color = static_cast<int>(color_);
    color.onChange([&]() {
        mol::util::updateRepColor(
            glRep,
            molecule,
            static_cast<mol::rep::Color>(color.value()),
            mask,
            uniformColor
        );
    });
    addProperty(color);

    filter.onChange([&]() {
        computeMask(mask, filter, molecule, isDynamic);
        mol::util::updateRepColor(
            glRep,
            molecule,
            static_cast<mol::rep::Color>(color.value()),
            mask,
            uniformColor
        );
    });
    addProperty(filter);

    scale.onChange([&]() {
        mol::util::updateRepType(glRep, static_cast<mol::rep::Type>(type.value()), scale);
    });
    addProperty(scale);

    uniformColor.setViewOption(properties::Property::ViewOptions::Color);
    uniformColor.onChange([&]() {
        mol::util::updateRepColor(
            glRep,
            molecule,
            static_cast<mol::rep::Color>(color.value()),
            mask,
            uniformColor
        );
    });
    addProperty(uniformColor);
}

RenderableMolecule::Representation::~Representation() {
    md_bitfield_free(&mask);
}

} // namespace openspace
