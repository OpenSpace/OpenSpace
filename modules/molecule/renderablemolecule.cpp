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
#include <modules/molecule/mol/viamd/postprocessing.h>
#include <modules/molecule/mol/cache.h>
#include <modules/molecule/mol/util.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/openglstatecache.h>
#include <core/md_array.h>
#include <core/md_allocator.h>
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
        if (!filter.empty() && filter != "" && filter != "all") {
            str_t str = { filter.data(), (int64_t)filter.length() };
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

    double timeToFrame(double time, int64_t num_frames, AnimationRepeatMode mode) {
        double frame = 0;
        const double lastFrame = num_frames - 1.0;
        switch (mode) {
            case AnimationRepeatMode::PingPong:
                frame = std::fmod(time, 2.0 * lastFrame);
                if (frame > lastFrame) {
                    frame = 2.0 * lastFrame - frame;
                }
                break;
            case AnimationRepeatMode::Wrap:
                frame = std::fmod(time, num_frames);
                break;
            case AnimationRepeatMode::Clamp:
                frame = std::clamp(time, 0.0, lastFrame - 1.0);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
        return frame;
    }

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
        "Applies Periodic Boundary Constraints for each interpolated frame (Can be "
        "CPU-intensive!)"
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
        "Base scale for the animation, tune this from its implicit value of 1.0 to sync "
        "up its animation with other trajectories"
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

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
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
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFile = p.moleculeFile;
    _trajectoryFile = p.trajectoryFile;
    _coarseGrained = p.coarseGrained.value_or(false);
    _applyPbcOnLoad = p.applyPbcOnLoad.value_or(true);
    _applyPbcPerFrame = p.applyPbcPerFrame.value_or(false);

    if (p.representations.has_value()) {
        for (const Parameters::Representation& rep : *p.representations) {
            bool enabled = rep.enabled.value_or(true);
            mol::rep::Type type =
                rep.type.has_value() ?
                codegen::map<mol::rep::Type>(rep.type.value()) :
                mol::rep::Type::SpaceFill;
            mol::rep::Color color =
                rep.color.has_value() ?
                codegen::map<mol::rep::Color>(rep.color.value()) :
                mol::rep::Color::Cpk;
            std::string filter = rep.filter.value_or("");
            float scale = rep.scale.value_or(1.f);
            glm::vec4 uniformColor = rep.uniformColor.value_or(glm::vec4(1.f));

            addRepresentation(enabled, type, color, filter, scale, uniformColor);
        }
    }
    else {
        // Add a default representation if none were supplied
        addRepresentation(
            true,
            mol::rep::Type::SpaceFill,
            mol::rep::Color::Cpk,
            "",
            1.f,
            glm::vec4(1.f)
        );
    }

    _animationBaseScale = p.animationBaseScale.value_or(1.0);
    _animationSpeed = p.animationSpeed.value_or(1.0);
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

    addPropertySubOwner(_repProps);
    addProperty(_applyPbcPerFrame);
    _animationSpeed.setReadOnly(true);
    addProperty(_animationSpeed);
    _animationRepeatMode.setReadOnly(true);
    addProperty(_animationRepeatMode);
}

RenderableMolecule::~RenderableMolecule() {
    freeMolecule();
}

void RenderableMolecule::initializeGL() {
    ZoneScoped
    initMolecule(_moleculeFile, _trajectoryFile);
}

void RenderableMolecule::deinitializeGL() {
    for (RepData& rep : _repData) {
        md_gl_representation_free(&rep.gl_rep);
    }
}

bool RenderableMolecule::isReady() const {
    return true;
}

void RenderableMolecule::update(const UpdateData& data) {
    // avoid updating if not in view, as it can be quite expensive.
    if (!_renderableInView) {
        return;
    }
    else {
        _renderableInView = false;
    }

    // update animation
    if (_trajectory) {
        updateTrajectoryFrame(data);
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks&) {
    GLuint fbo = global::moduleEngine->module<MoleculeModule>()->fbo();
        
    // compute distance from camera to molecule
    glm::dvec3 forward = data.modelTransform.translation - data.camera.positionVec3();
    glm::dvec3 dir = data.camera.viewDirectionWorldSpace();
    // "signed" distance from camera to object.
    double distance = glm::length(forward) * sign(glm::dot(dir, forward));

    // distance < 0 means behind the camera, 1E4 is arbitrary.
    if (distance < 0.0 || distance > 10000.0) {
        return;
    }
    else {
        _renderableInView = true;
    }

    // because the molecule is small, a scaling of the view matrix causes the molecule
    // to be moved out of view in clip space. Resetting the scaling for the molecule
    // is fine for now. This will have an impact on stereoscopic depth though.
    Camera camCopy = data.camera;
    camCopy.setScaling(0.1f);

    glm::mat4 viewMatrix =
        camCopy.combinedViewMatrix() *
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale) *
        glm::dmat4(data.modelTransform.rotation);

    glm::mat4 projMatrix = glm::dmat4(camCopy.sgctInternal.projectionMatrix());
        
    // Center the molecule with respect to its cell
    const vec3_t trans = _molecule.cell.basis * vec3_set1(0.5f);
    const mat4_t modelMat = mat4_translate(-trans.x, -trans.y, -trans.z);

    std::vector<md_gl_draw_op_t> drawOps;
    drawOps.reserve((_repData.size()));

    if (_molecule.atom.count) {
        for (const RepData rep : _repData) {
            if (rep.enabled) {
                drawOps.emplace_back(&rep.gl_rep, &modelMat[0][0]);
            }
        }
    }

    md_gl_draw_args_t args = {};
    args.shaders = &global::moduleEngine->module<MoleculeModule>()->shaders();
    args.view_transform = {
        glm::value_ptr(viewMatrix),
        glm::value_ptr(projMatrix),
        nullptr,
        nullptr
    };
    args.options = 0;

    args.draw_operations = {
        static_cast<uint32_t>(drawOps.size()),
        drawOps.data(),
    };

    GLint lastFbo;
    GLint lastDrawBufferCount = 0;
    GLenum lastDrawBuffers[8];
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &lastFbo);
    for (int i = 0; i < ARRAY_SIZE(lastDrawBuffers); i++) {
        GLint drawBuf;
        glGetIntegerv(GL_DRAW_BUFFER0 + i, &drawBuf);
        if (!drawBuf) {
            break;
        }
        lastDrawBuffers[lastDrawBufferCount++] = static_cast<GLenum>(drawBuf);
    }
            
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    const GLenum bufs[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
    glDrawBuffers(2, bufs);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    md_gl_draw(&args);

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, lastFbo);
    glDrawBuffers(lastDrawBufferCount, lastDrawBuffers);
        
    global::moduleEngine->module<MoleculeModule>()->setViewMatrix(
        data.camera.combinedViewMatrix()
    );
    global::moduleEngine->module<MoleculeModule>()->setProjectionMatrix(projMatrix);
}

void RenderableMolecule::initMolecule(std::string_view molFile, std::string_view trajFile)
{
    LDEBUG(std::format("Loading molecule file '{}'", molFile));

    // free previously loaded molecule
    freeMolecule();

    const md_molecule_t* molecule = mol::loadMolecule(molFile, _coarseGrained);
    if (molecule) {
        // We deep copy the contents, so we can freely modify the fields (coordinates etc)
        md_molecule_copy(&_molecule, molecule, default_allocator);

        md_gl_molecule_init(&_gl_molecule, &_molecule);
        updateRepresentations();

        vec3_t aabb_min, aabb_max;
        md_util_compute_aabb_xyzr(
            &aabb_min,
            &aabb_max,
            _molecule.atom.x,
            _molecule.atom.y,
            _molecule.atom.z,
            _molecule.atom.radius,
            _molecule.atom.count
        );
        vec3_t c = (aabb_min + aabb_max) * 0.5f;
        _center = { c.x, c.y, c.z };
        _radius = vec3_distance(aabb_min, aabb_max) * 0.5f;
        setBoundingSphere(_radius * 2.f);
    }
    else {
        throw ghoul::RuntimeError("Failed to initialize molecule: Failed to load file");
    }

    if (!trajFile.empty()) {
        LDEBUG(std::format("Loading trajectory file '{}'", trajFile));
        _trajectory = mol::loadTrajectory(trajFile, molecule, _applyPbcOnLoad);

        if (!_trajectory) {
            throw ghoul::RuntimeError(
                "Failed to initialize trajectory: Failed to load file"
            );
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

void RenderableMolecule::addRepresentation(bool enabled, mol::rep::Type type,
                                           mol::rep::Color color, std::string filter,
                                           float scale, glm::vec4 uniformColor)
{
    using namespace properties;

    size_t i = _repProps.propertySubOwners().size();
    PropertyOwner* prop = new PropertyOwner({
        .identifier = std::format("rep{}", i),
        .guiName = std::format("Representation {}", i),
        .description = std::format("Visual representation of molecule", i),
    });

    BoolProperty* pEnabled = new BoolProperty(EnabledInfo, enabled);
    prop->addProperty(pEnabled);

    OptionProperty* pType = new OptionProperty(TypeInfo);
    pType->addOptions({
        { static_cast<int>(mol::rep::Type::SpaceFill), "SpaceFill" },
        { static_cast<int>(mol::rep::Type::Licorice), "Licorice" },
        { static_cast<int>(mol::rep::Type::Ribbons), "Ribbons" },
        { static_cast<int>(mol::rep::Type::Cartoon), "Cartoon" }
    });
    pType->setValue(static_cast<int>(type));
    prop->addProperty(pType);

    OptionProperty* pColor = new OptionProperty(ColorInfo);
    pColor->addOptions({
        { static_cast<int>(mol::rep::Color::Cpk), "CPK" },
        { static_cast<int>(mol::rep::Color::AtomIndex), "Atom Index" },
        { static_cast<int>(mol::rep::Color::ResId), "Residue ID" },
        { static_cast<int>(mol::rep::Color::ResIndex), "Residue Index" },
        { static_cast<int>(mol::rep::Color::ChainId), "Chain ID" },
        { static_cast<int>(mol::rep::Color::ChainIndex), "Chain Index" },
        { static_cast<int>(mol::rep::Color::SecondaryStructure), "Secondary Structure" },
        { static_cast<int>(mol::rep::Color::Uniform), "Uniform" }
    });
    pColor->setValue(static_cast<int>(color));
    prop->addProperty(pColor);

    Vec4Property* pUniformColor = new Vec4Property(
        UniformColorInfo,
        uniformColor,
        glm::vec4(0.f),
        glm::vec4(1.f)
    );
    pUniformColor->setViewOption(Property::ViewOptions::Color);
    prop->addProperty(pUniformColor);

    StringProperty* pFilter = new StringProperty(FilterInfo, filter);
    prop->addProperty(pFilter);

    FloatProperty* pScale = new FloatProperty(ScaleInfo, scale, 0.f, 10.f);
    prop->addProperty(pScale);

    _repProps.addPropertySubOwner(prop);

    _repData.emplace_back();

    auto enableRep = [this, i, pEnabled]() mutable {
        if (i >= _repData.size()) {
            return;
        }
        _repData[i].enabled = *pEnabled;
    };

    auto updateRep = [this, i, pType, pScale]() mutable {
        if (i >= _repData.size()) {
            return;
        }
        mol::util::updateRepType(
            _repData[i].gl_rep,
            static_cast<mol::rep::Type>(pType->value()),
            *pScale
        );
    };

    auto updateCol = [this, i, pColor, pUniformColor]() mutable {
        if (i >= _repData.size()) {
            return;
        }

        const mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());
        mol::util::updateRepColor(
            _repData[i].gl_rep,
            _molecule,
            color,
            _repData[i].mask,
            *pUniformColor
        );
    };

    auto updateFilt = [this, i, pFilter, updateCol]() mutable {
        if (i >= _repData.size()) {
            return;
        }
        computeMask(_repData[i].mask, *pFilter, _molecule, _repData[i].dynamic);
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
    for (RepData& rep : _repData) {
        md_gl_representation_free(&rep.gl_rep);
    }

    _repData.resize(_repProps.propertySubOwners().size());
    for (size_t i = 0; i < _repData.size(); ++i) {
        auto& rep = _repData[i];
        rep.gl_rep = { 0 };
        md_gl_representation_init(&rep.gl_rep, &_gl_molecule);

        using namespace properties;
        auto pRep = _repProps.propertySubOwners()[i];
        auto pEnabled = dynamic_cast<BoolProperty*>(pRep->property("Enabled"));
        auto pType = dynamic_cast<OptionProperty*>(pRep->property("Type"));
        auto pColor = dynamic_cast<OptionProperty*>(pRep->property("Color"));
        auto pFilter = dynamic_cast<StringProperty*>(pRep->property("Filter"));
        auto pScale = dynamic_cast<FloatProperty*>(pRep->property("Scale"));
        auto pUniformColor = dynamic_cast<Vec4Property*>(pRep->property("UniformColor"));

        if (pEnabled && *pEnabled && pType && pColor && pFilter && pScale && pUniformColor) {
            computeMask(rep.mask, *pFilter, _molecule, rep.dynamic);

            mol::rep::Type type = static_cast<mol::rep::Type>(pType->value());
            mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());
            mol::util::updateRepType(rep.gl_rep, type, *pScale);
            mol::util::updateRepColor(
                rep.gl_rep,
                _molecule,
                color,
                rep.mask,
                *pUniformColor
            );
        }
    }
}

void RenderableMolecule::updateTrajectoryFrame(const UpdateData& data) {
    if (!_trajectory) {
        return;
    }

    double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
    double currT = data.time.j2000Seconds();

    const double scl = _animationBaseScale * _animationSpeed;
    double currTime = (currT - _localEpoch) * scl;
    double nextTime = (currT + dt - _localEpoch) * scl;

    const int64_t numFrames = md_trajectory_num_frames(_trajectory);
    AnimationRepeatMode mode = static_cast<AnimationRepeatMode>(_animationRepeatMode.value());
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
            &_gl_molecule,
            0,
            static_cast<uint32_t>(_molecule.atom.count),
            xyz
        );

        md_free(alloc, xyz, size);

        std::vector<size_t> repHasSsIndices;
        std::vector<size_t> repUpdateColIndices;

        for (size_t i = 0; i < _repProps.propertySubOwners().size(); i++) {
            if (!_repData[i].enabled) {
                continue;
            }

            using namespace properties;
            auto pColor = dynamic_cast<OptionProperty*>(
                _repProps.propertySubOwners()[i]->property("Color")
            );
            auto pFilter = dynamic_cast<StringProperty*>(
                _repProps.propertySubOwners()[i]->property("Filter")
            );
            if (pColor) {
                mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());
                if (color == mol::rep::Color::SecondaryStructure) {
                    repHasSsIndices.push_back(i);
                }
            }
            if (pFilter && _repData[i].dynamic) {
                computeMask(_repData[i].mask, *pFilter, _molecule, _repData[i].dynamic);
                repUpdateColIndices.push_back(i);
            }
        }

        if (!repHasSsIndices.empty()) {
            md_gl_molecule_set_backbone_secondary_structure(
                &_gl_molecule,
                0,
                static_cast<uint32_t>(_molecule.backbone.count),
                _molecule.backbone.secondary_structure,
                0
            );

            for (size_t i : repHasSsIndices) {
                repUpdateColIndices.push_back(i);
            }
        }

        for (size_t i : repUpdateColIndices) {
            using namespace properties;
            auto pColor = dynamic_cast<OptionProperty*>(
                _repProps.propertySubOwners()[i]->property("Color")
            );
            mol::rep::Color color = static_cast<mol::rep::Color>(pColor->value());

            auto pUniformColor = dynamic_cast<Vec4Property*>(
                _repProps.propertySubOwners()[i]->property("UniformColor")
            );
            mol::util::updateRepColor(
                _repData[i].gl_rep,
                _molecule,
                color,
                _repData[i].mask,
                *pUniformColor
            );
        }
    }

    using FrameSet = std::array<int64_t, 4>;

    auto frameSet = [numFrames, mode](double time) -> FrameSet {
        const int64_t frameIdx = static_cast<int64_t>(timeToFrame(time, numFrames, mode));

        auto wrap = [numFrames](int64_t idx) -> int64_t {
            idx = idx < 0 ? numFrames - 1 : idx;
            idx = idx >= numFrames ? 0 : idx;
            return idx;
        };

        return {
            wrap(frameIdx - 1),
            wrap(frameIdx + 0),
            wrap(frameIdx + 1),
            wrap(frameIdx + 2)
        };
    };

    // Prefetch next set of frames
    FrameSet curr = frameSet(currTime);
    FrameSet nxt1 = frameSet(nextTime);

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

    mol::prefetchFrames(_trajectory, frames);
}

RenderableMolecule::RepData::RepData()
    : mask(md_bitfield_create(default_allocator))
{}

RenderableMolecule::RepData::~RepData() {
    md_bitfield_free(&mask);
}

} // namespace openspace
