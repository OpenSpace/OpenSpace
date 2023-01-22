/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/scene/scenegraphnode.h>

#include <modules/base/scale/staticscale.h>
#include <modules/base/rotation/staticrotation.h>
#include <modules/base/translation/statictranslation.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/timeframe.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace {
    constexpr std::string_view _loggerCat = "SceneGraphNode";

    constexpr openspace::properties::Property::PropertyInfo ComputeScreenSpaceInfo = {
        "ComputeScreenSpaceData",
        "Compute Screen Space Data",
        "If this value is set to 'true', the screenspace-based properties are calculated "
        "at regular intervals. If these values are set to 'false', they are not updated"
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenSpacePositionInfo = {
        "ScreenSpacePosition",
        "ScreenSpacePosition",
        "The x,y position in screen space. Can be used for placing GUI elements",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenVisibilityInfo = {
        "ScreenVisibility",
        "ScreenVisibility",
        "Determines if the node is currently visible on screen",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceFromCamToNodeInfo = {
        "DistanceFromCamToNode",
        "DistanceFromCamToNode",
        "The distance from the camera to the node surface",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenSizeRadiusInfo = {
        "ScreenSizeRadius",
        "ScreenSizeRadius",
        "The screen size of the radius of the node",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo VisibilityDistanceInfo = {
        "VisibilityDistance",
        "VisibilityDistance",
        "The distace in world coordinates between node and camera at which the "
        "screenspace object will become visible",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo BoundingSphereInfo = {
        "BoundingSphere",
        "Bounding Sphere",
        "The bounding sphere of the scene graph node meaning that everything that this "
        "scene graph node renders must be contained within this sphere. This value is "
        "only used as an override to the bounding sphere calculated by the Renderable, "
        "if present. If this value is -1, the Renderable's computed bounding sphere is "
        "used",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo InteractionSphereInfo = {
        "InteractionSphere",
        "Interaction Sphere",
        "The minimum radius that the camera is allowed to get close to this scene graph "
        "node. This value is only used as an override to the bounding sphere calculated "
        "by the Renderable, if present. If this value is -1, the Renderable's computed "
        "interaction sphere is used",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo ApproachFactorInfo = {
        "ApproachFactor",
        "Approach Factor",
        "This value is a multiplication factor for the interaction sphere that "
        "determines when the camera is 'approaching' the scene graph node"
    };

    constexpr openspace::properties::Property::PropertyInfo ReachFactorInfo = {
        "ReachFactor",
        "Reach Factor",
        "This value is a multiplication factor for the interaction sphere that "
        "determines when the camera has 'reached' the scene graph node"
    };

    constexpr openspace::properties::Property::PropertyInfo GuiPathInfo = {
        "GuiPath",
        "Gui Path",
        "This is the path for the scene graph node in the gui example: "
        "/Solar System/Planets/Earth",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo GuiNameInfo = {
        "GuiName",
        "Gui Name",
        "This is the name for the scene graph node in the gui. Example: Earth",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo GuiDescriptionInfo = {
        "GuiDescription",
        "Gui Description",
        "This is the description for the scene graph node to be shown in the gui. "
        "Example: Earth is a special place",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo GuiHiddenInfo = {
        "GuiHidden",
        "Gui Hidden",
        "This represents if the scene graph node should be shown in the gui. "
        "Example: false",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo ShowDebugSphereInfo = {
        "ShowDebugSphere",
        "Show Debug Sphere",
        "If enabled the bounding sphere of this scene graph node is rendered as a debug "
        "method",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(SceneGraphNode)]] Parameters {
        // The identifier of this scenegraph node. This name must be unique among all
        // scene graph nodes that are loaded in a specific scene. If a duplicate is
        // detected the loading of the node will fail, as will all childing that depend on
        // the node. The identifier must not contain any whitespaces or '.'
        std::string identifier;

        // This names the parent of the currently specified scenegraph node. The parent
        // must already exist in the scene graph. If not specified, the node will be
        // attached to the root of the scenegraph
        std::optional<std::string> parent
            [[codegen::annotation(
                "If specified, this must be a name for another scenegraph node"
            )]];

        // The renderable that is to be created for this scenegraph node. A renderable is
        // a component of a scenegraph node that will lead to some visual result on the
        // screen. The specifics heavily depend on the 'Type' of the renderable. If no
        // Renderable is specified, this scenegraph node is an internal node and can be
        // used for either group children, or apply common transformations to a group of
        // children
        std::optional<ghoul::Dictionary> renderable [[codegen::reference("renderable")]];

        // [[codegen::verbatim(BoundingSphereInfo.description)]]
        std::optional<double> boundingSphere;

        // [[codegen::verbatim(InteractionSphereInfo.description)]]
        std::optional<double> interactionSphere;

        struct Transform {
            // This node describes a translation that is applied to the scenegraph node
            // and all its children. Depending on the 'Type' of the translation, this can
            // either be a static translation or a time-varying one
            std::optional<ghoul::Dictionary> translation
                [[codegen::reference("core_transform_translation")]];

            // This nodes describes a rotation that is applied to the scenegraph node and
            // all its children. Depending on the 'Type' of the rotation, this can either
            // be a static rotation or a time-varying one
            std::optional<ghoul::Dictionary> rotation
                [[codegen::reference("core_transform_rotation")]];

            // This node describes a scaling that is applied to the scenegraph node and
            // all its children. Depending on the 'Type' of the scaling, this can either
            // be a static scaling or a time-varying one
            std::optional<ghoul::Dictionary> scale
                [[codegen::reference("core_transform_scaling")]];
        };

        // This describes a set of transformations that are applied to this scenegraph
        // node and all of its children. There are only three possible values
        // corresponding to a 'Translation', a 'Rotation', and a 'Scale'
        std::optional<Transform> transform;

        // This value is a multiplication factor for the interaction sphere that
        // determines when the camera is 'approaching' the scene graph node. If this value
        // is not specified, a default value of 5 is used instead. This value must be
        // larger than the reachFactor or unexpected things might happen
        std::optional<double> approachFactor [[codegen::greaterequal(0.0)]];

        // This value is a multiplication factor for the interaction sphere that
        // determines when the camera has 'reached' the scene graph node. If this value is
        // not specified, a default value of 1.25 is used instead. This value must be
        // smaller than the approachFactor or unexpected things might happen
        std::optional<double> reachFactor [[codegen::greaterequal(0.0)]];

        // One or multiple actions that are executed whenever the camera is focused on
        // this scene graph node and if it enters the interaction sphere of the node
        std::optional<std::variant<std::string, std::vector<std::string>>> onApproach;

        // One or multiple actions that are executed whenever the camera is focused on
        // this scene graph node and if it transitions from the approach distance to the
        // reach distance of the node
        std::optional<std::variant<std::string, std::vector<std::string>>> onReach;

        // One or multiple actions that are executed whenever the camera is focused on
        // this scene graph node and if it transitions from the reach distance to the
        // approach distance of the node
        std::optional<std::variant<std::string, std::vector<std::string>>> onRecede;

        // One or multiple actions that are executed whenever the camera is focused on
        // this scene graph node and if it exits the interaction sphere of the node
        std::optional<std::variant<std::string, std::vector<std::string>>> onExit;

        // Specifies the time frame for when this node should be active
        std::optional<ghoul::Dictionary> timeFrame
            [[codegen::reference("core_time_frame")]];

        // A tag or list of tags that can be used to reference to a group of scenegraph
        // nodes.
        std::optional<std::variant<std::string, std::vector<std::string>>> tag;

        struct Gui {
            // An optional user-facing name for this SceneGraphNode, which does not have
            // to be unique, though it is recommended, and can contain any characters
            std::optional<std::string> name;

            // If this value is specified, this '/' separated URI specifies the location
            // of this scenegraph node in a GUI representation, for instance
            // '/SolarSystem/Earth/Moon'
            std::optional<std::string> path;

            // A user-facing description about this scene graph node
            std::optional<std::string> description;

            // If this value is specified, GUI applications are incouraged to ignore this
            // scenegraph node. This is most useful to trim collective lists of nodes and
            // not display, for example, barycenters
            std::optional<bool> hidden;
        };
        // Additional information that is passed to GUI applications. These are all hints
        // and do not have any impact on the actual function of the scenegraph node
        std::optional<Gui> gui [[codegen::key("GUI")]];
    };
#include "scenegraphnode_codegen.cpp"
} // namespace

namespace openspace {

#ifdef Debugging_Core_SceneGraphNode_Indices
int SceneGraphNode::nextIndex = 0;
#endif // Debugging_Core_SceneGraphNode_Indices

ghoul::mm_unique_ptr<SceneGraphNode> SceneGraphNode::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    SceneGraphNode* n = global::memoryManager->PersistentMemory.alloc<SceneGraphNode>();
    ghoul::mm_unique_ptr<SceneGraphNode> result = ghoul::mm_unique_ptr<SceneGraphNode>(n);

#ifdef Debugging_Core_SceneGraphNode_Indices
    result->index = nextIndex++;
#endif // Debugging_Core_SceneGraphNode_Indices

    result->setIdentifier(p.identifier);

    if (p.gui.has_value()) {
        if (p.gui->name.has_value()) {
            result->setGuiName(*p.gui->name);
            result->_guiDisplayName = result->guiName();
            result->addProperty(result->_guiDisplayName);
        }

        if (p.gui->description.has_value()) {
            result->setDescription(*p.gui->description);
            result->_guiDescription = result->description();
            result->addProperty(result->_guiDescription);
        }

        if (p.gui->hidden.has_value()) {
            result->_guiHidden = *p.gui->hidden;
            result->addProperty(result->_guiHidden);
        }

        if (p.gui->path.has_value()) {
            result->_guiPath = *p.gui->path;
            result->addProperty(result->_guiPath);
        }
    }

    result->_boundingSphere = p.boundingSphere.value_or(result->_boundingSphere);
    result->_interactionSphere = p.interactionSphere.value_or(result->_interactionSphere);

    result->_approachFactor = p.approachFactor.value_or(result->_approachFactor);
    result->_reachFactor = p.reachFactor.value_or(result->_reachFactor);

    if (p.transform.has_value()) {
        if (p.transform->translation.has_value()) {
            result->_transform.translation = Translation::createFromDictionary(
                *p.transform->translation
            );

            // @TODO(abock, 2021-03-05)  I don't think this is necessary anymore as we
            // transitioned to throwing exceptions when the construction fails
            if (result->_transform.translation == nullptr) {
                LERROR(fmt::format(
                    "Failed to create ephemeris for SceneGraphNode '{}'",
                    result->identifier()
                ));
                return nullptr;
            }
            LDEBUG(fmt::format(
                "Successfully created ephemeris for '{}'", result->identifier()
            ));
        }

        if (p.transform->rotation.has_value()) {
            result->_transform.rotation = Rotation::createFromDictionary(
                *p.transform->rotation
            );

            // @TODO(abock, 2021-03-05)  I don't think this is necessary anymore as we
            // transitioned to throwing exceptions when the construction fails
            if (result->_transform.rotation == nullptr) {
                LERROR(fmt::format(
                    "Failed to create rotation for SceneGraphNode '{}'",
                    result->identifier()
                ));
                return nullptr;
            }
            LDEBUG(fmt::format(
                "Successfully created rotation for '{}'", result->identifier()
            ));
        }

        if (p.transform->scale.has_value()) {
            result->_transform.scale = Scale::createFromDictionary(*p.transform->scale);

            // @TODO(abock, 2021-03-05)  I don't think this is necessary anymore as we
            // transitioned to throwing exceptions when the construction fails
            if (result->_transform.scale == nullptr) {
                LERROR(fmt::format(
                    "Failed to create scale for SceneGraphNode '{}'",
                    result->identifier()
                ));
                return nullptr;
            }
            LDEBUG(fmt::format(
                "Successfully created scale for '{}'", result->identifier()
            ));
        }
    }
    result->addPropertySubOwner(result->_transform.translation.get());
    result->addPropertySubOwner(result->_transform.rotation.get());
    result->addPropertySubOwner(result->_transform.scale.get());


    if (p.timeFrame.has_value()) {
        result->_timeFrame = TimeFrame::createFromDictionary(*p.timeFrame);

        // @TODO(abock, 2021-03-05)  I don't think this is necessary anymore as we
        // transitioned to throwing exceptions when the construction fails
        if (result->_timeFrame == nullptr) {
            LERROR(fmt::format(
                "Failed to create time frame for SceneGraphNode '{}'",
                result->identifier()
            ));
            return nullptr;
        }
        LDEBUG(fmt::format(
            "Successfully created time frame for '{}'", result->identifier()
        ));
        result->addPropertySubOwner(result->_timeFrame.get());
    }

    // We initialize the renderable last as it probably has the most dependencies
    if (p.renderable.has_value()) {
        result->_renderable = Renderable::createFromDictionary(*p.renderable);
        ghoul_assert(result->_renderable, "Failed to create Renderable");
        result->_renderable->_parent = result.get();
        result->addPropertySubOwner(result->_renderable.get());
        LDEBUG(fmt::format(
            "Successfully created renderable for '{}'", result->identifier()
        ));
    }

    // Extracting the actions from the dictionary
    if (p.onApproach.has_value()) {
        if (std::holds_alternative<std::string>(*p.onApproach)) {
            result->_onApproachAction = { std::get<std::string>(*p.onApproach) };
        }
        else {
            result->_onApproachAction = std::get<std::vector<std::string>>(*p.onApproach);
        }
    }

    if (p.onReach.has_value()) {
        if (std::holds_alternative<std::string>(*p.onReach)) {
            result->_onReachAction = { std::get<std::string>(*p.onReach) };
        }
        else {
            result->_onReachAction = std::get<std::vector<std::string>>(*p.onReach);
        }
    }

    if (p.onRecede.has_value()) {
        if (std::holds_alternative<std::string>(*p.onRecede)) {
            result->_onRecedeAction = { std::get<std::string>(*p.onRecede) };
        }
        else {
            result->_onRecedeAction = std::get<std::vector<std::string>>(*p.onRecede);
        }
    }

    if (p.onExit.has_value()) {
        if (std::holds_alternative<std::string>(*p.onExit)) {
            result->_onExitAction = { std::get<std::string>(*p.onExit) };
        }
        else {
            result->_onExitAction = std::get<std::vector<std::string>>(*p.onExit);
        }
    }

    if (p.tag.has_value()) {
        if (std::holds_alternative<std::string>(*p.tag)) {
            result->addTag(std::get<std::string>(*p.tag));
        }
        else if (std::holds_alternative<std::vector<std::string>>(*p.tag)) {
            for (const std::string& tag : std::get<std::vector<std::string>>(*p.tag)) {
                if (!tag.empty()) {
                    result->addTag(tag);
                }
            }
        }
        else {
            throw ghoul::MissingCaseException();
        }
    }

    LDEBUG(fmt::format("Successfully created SceneGraphNode '{}'", result->identifier()));

    result->_lastScreenSpaceUpdateTime = std::chrono::high_resolution_clock::now();
    return result;
}

documentation::Documentation SceneGraphNode::Documentation() {
    return codegen::doc<Parameters>("core_scene_node");
}

ghoul::opengl::ProgramObject* SceneGraphNode::_debugSphereProgram = nullptr;

SceneGraphNode::SceneGraphNode()
    : properties::PropertyOwner({ "" })
    , _guiHidden(GuiHiddenInfo)
    , _guiPath(GuiPathInfo)
    , _guiDisplayName(GuiNameInfo)
    , _guiDescription(GuiDescriptionInfo)
    , _transform {
        ghoul::mm_unique_ptr<Translation>(
            global::memoryManager->PersistentMemory.alloc<StaticTranslation>()
        ),
        ghoul::mm_unique_ptr<Rotation>(
            global::memoryManager->PersistentMemory.alloc<StaticRotation>()
        ),
        ghoul::mm_unique_ptr<Scale>(
            global::memoryManager->PersistentMemory.alloc<StaticScale>()
        )
    }
    , _boundingSphere(BoundingSphereInfo, -1.0, -1.0, 1e12)
    , _interactionSphere(InteractionSphereInfo, -1.0, -1.0, 1e12)
    , _approachFactor(ApproachFactorInfo, 5.0, 0.0, 1e4)
    , _reachFactor(ReachFactorInfo, 1.25, 0.0, 1e4)
    , _computeScreenSpaceValues(ComputeScreenSpaceInfo, false)
    , _screenSpacePosition(ScreenSpacePositionInfo, glm::ivec2(-1, -1))
    , _screenVisibility(ScreenVisibilityInfo, false)
    , _distFromCamToNode(DistanceFromCamToNodeInfo, -1.0)
    , _screenSizeRadius(ScreenSizeRadiusInfo, 0)
    , _visibilityDistance(VisibilityDistanceInfo, 6e10f)
    , _showDebugSphere(ShowDebugSphereInfo, false)
{
    addProperty(_computeScreenSpaceValues);
    addProperty(_screenSpacePosition);
    addProperty(_screenVisibility);
    addProperty(_distFromCamToNode);
    addProperty(_screenSizeRadius);
    addProperty(_visibilityDistance);
    _boundingSphere.onChange([this]() {
        if (_boundingSphere >= 0.0) {
            _overrideBoundingSphere = _boundingSphere;
        }
        else {
            _overrideBoundingSphere = std::nullopt;
        }
    });
    // @TODO (2021-06-30, emmbr) Uncomment this when exponential sliders support
    // negative values
    //_boundingSphere.setExponent(10.f);
    addProperty(_boundingSphere);
    _interactionSphere.onChange([this]() {
        if (_interactionSphere >= 0.0) {
            _overrideInteractionSphere = _interactionSphere;
        }
        else {
            _overrideInteractionSphere = std::nullopt;
        }
    });
    // @TODO (2021-06-30, emmbr) Uncomment this when exponential sliders support
    // negative values
    //_interactionSphere.setExponent(10.f);
    addProperty(_interactionSphere);

    _reachFactor.setExponent(3.f);
    addProperty(_reachFactor);

    _approachFactor.setExponent(3.f);
    addProperty(_approachFactor);

    addProperty(_showDebugSphere);
}

SceneGraphNode::~SceneGraphNode() {}

void SceneGraphNode::initialize() {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    LDEBUG(fmt::format("Initializing: {}", identifier()));

    if (_renderable) {
        _renderable->initialize();
    }

    if (_transform.translation) {
        _transform.translation->initialize();
    }
    if (_transform.rotation) {
        _transform.rotation->initialize();
    }
    if (_transform.scale) {
        _transform.scale->initialize();
    }
    _state = State::Initialized;

    LDEBUG(fmt::format("Finished initializing: {}", identifier()));
}

void SceneGraphNode::initializeGL() {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    LDEBUG(fmt::format("Initializing GL: {}", identifier()));

    if (_renderable) {
        _renderable->initializeGL();
    }

    // The first one to get here will create program shared between all scene graph nodes
    if (_debugSphereProgram == nullptr) {
        std::unique_ptr<ghoul::opengl::ProgramObject> shader =
            global::renderEngine->buildRenderProgram(
                "DebugSphere",
                absPath("${SHADERS}/core/xyzuvrgba_vs.glsl"),
                absPath("${SHADERS}/core/xyzuvrgba_fs.glsl")
            );
        // Since we are only going to create a single of these shaders for the lifetime of
        // the program, we are not bothering with freeing it as the overhead of detecting
        // when the last scenegraph node will be destroyed would be a bit too much for the
        // benefit that we would gain from it
        _debugSphereProgram = shader.release();
        _debugSphereProgram->setIgnoreUniformLocationError(
            ghoul::opengl::ProgramObject::IgnoreError::Yes
        );
    }

    _state = State::GLInitialized;

    LDEBUG(fmt::format("Finished initializating GL: {}", identifier()));
}

void SceneGraphNode::deinitialize() {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    LDEBUG(fmt::format("Deinitializing: {}", identifier()));

    setScene(nullptr);

    if (_renderable) {
        _renderable->deinitialize();
    }
    clearChildren();
    _parent = nullptr;

    LDEBUG(fmt::format("Finished deinitializing: {}", identifier()));
}

void SceneGraphNode::deinitializeGL() {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    LDEBUG(fmt::format("Deinitializing GL: {}", identifier()));

    if (_renderable) {
        _renderable->deinitializeGL();
    }

    LDEBUG(fmt::format("Finished deinitializing GL: {}", identifier()));
}

void SceneGraphNode::traversePreOrder(const std::function<void(SceneGraphNode*)>& fn) {
    fn(this);
    for (ghoul::mm_unique_ptr<SceneGraphNode>& child : _children) {
        child->traversePreOrder(fn);
    }
}

void SceneGraphNode::traversePostOrder(const std::function<void(SceneGraphNode*)>& fn) {
    for (ghoul::mm_unique_ptr<SceneGraphNode>& child : _children) {
        child->traversePostOrder(fn);
    }
    fn(this);
}

void SceneGraphNode::update(const UpdateData& data) {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    State s = _state;
    if (s != State::Initialized && _state != State::GLInitialized) {
        return;
    }
    if (!isTimeFrameActive(data.time)) {
        return;
    }

    if (_transform.translation) {
        _transform.translation->update(data);
    }

    if (_transform.rotation) {
        _transform.rotation->update(data);
    }

    if (_transform.scale) {
        _transform.scale->update(data);
    }
    UpdateData newUpdateData = data;

    // Assumes _worldRotationCached and _worldScaleCached have been calculated for parent
    _worldPositionCached = calculateWorldPosition();
    _worldRotationCached = calculateWorldRotation();
    _worldScaleCached = calculateWorldScale();

    newUpdateData.modelTransform.translation = _worldPositionCached;
    newUpdateData.modelTransform.rotation = _worldRotationCached;
    newUpdateData.modelTransform.scale = _worldScaleCached;

    glm::dmat4 translation = glm::translate(
        glm::dmat4(1.0),
        newUpdateData.modelTransform.translation
    );
    glm::dmat4 rotation = glm::dmat4(newUpdateData.modelTransform.rotation);
    glm::dmat4 scaling = glm::scale(glm::dmat4(1.0), newUpdateData.modelTransform.scale);

    _modelTransformCached = translation * rotation * scaling;

    if (_renderable && _renderable->isReady() &&
        (_renderable->isEnabled() || _renderable->shouldUpdateIfDisabled()))
    {
        _renderable->update(newUpdateData);
    }
}

void SceneGraphNode::render(const RenderData& data, RendererTasks& tasks) {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    if (_state != State::GLInitialized) {
        return;
    }

    const bool visible = _renderable && _renderable->isVisible() &&
        _renderable->isReady() && _renderable->matchesRenderBinMask(data.renderBinMask);

    if (!visible) {
        return;
    }

    if (!isTimeFrameActive(data.time)) {
        return;
    }

    {
        TracyGpuZone("Render")

        RenderData newData = {
            .camera = data.camera,
            .time = data.time,
            .renderBinMask = data.renderBinMask,
            .modelTransform = {
                .translation = _worldPositionCached,
                .rotation = _worldRotationCached,
                .scale = _worldScaleCached
            }
        };

        _renderable->render(newData, tasks);
        if (_computeScreenSpaceValues) {
            computeScreenSpaceData(newData);
        }
    }

    if (_showDebugSphere) {
        if (const double bs = boundingSphere();  bs > 0.0) {
            renderDebugSphere(data.camera, bs, glm::vec4(0.5f, 0.15f, 0.5f, 0.75f));
        }

        if (const double is = interactionSphere();  is > 0.0) {
            renderDebugSphere(data.camera, is, glm::vec4(0.15f, 0.35f, 0.85f, 0.75f));
        }
    }
}

void SceneGraphNode::renderDebugSphere(const Camera& camera, double size, glm::vec4 color)
{
    glm::dvec3 scaleVec = _worldScaleCached * size;
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), _worldPositionCached) *
        glm::dmat4(_worldRotationCached) *
        glm::scale(glm::dmat4(1.0), scaleVec);

    glm::mat4 modelViewProjection = camera.projectionMatrix() *
        glm::mat4(camera.combinedViewMatrix() * modelTransform);

    _debugSphereProgram->activate();
    _debugSphereProgram->setUniform("hasTexture", 0);
    _debugSphereProgram->setUniform("proj", modelViewProjection);
    _debugSphereProgram->setUniform("color", color);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    glBindVertexArray(rendering::helper::vertexObjects.sphere.vao);
    glDrawElements(
        GL_TRIANGLES,
        rendering::helper::vertexObjects.sphere.nElements,
        GL_UNSIGNED_SHORT,
        nullptr
    );

    glLineWidth(2.0);
    _debugSphereProgram->setUniform("color", glm::vec4(1.f, 1.f, 1.f, 1.f));
    glDrawElements(
        GL_LINES,
        rendering::helper::vertexObjects.sphere.nElements,
        GL_UNSIGNED_SHORT,
        nullptr
    );

    glBindVertexArray(0);

    _debugSphereProgram->deactivate();
}

void SceneGraphNode::setParent(SceneGraphNode& parent) {
    ghoul_assert(_parent != nullptr, "Node must be attached to a parent");

    parent.attachChild(_parent->detachChild(*this));
}

void SceneGraphNode::attachChild(ghoul::mm_unique_ptr<SceneGraphNode> child) {
    ghoul_assert(child != nullptr, "Child may not be null");
    ghoul_assert(child->parent() == nullptr, "Child may not already have a parent");

    // Create link between parent and child
    child->_parent = this;
    SceneGraphNode* childRaw = child.get();
    _children.push_back(std::move(child));

    // Set scene of child (and children recursively)
    childRaw->setScene(_scene);
}

ghoul::mm_unique_ptr<SceneGraphNode> SceneGraphNode::detachChild(SceneGraphNode& child) {
    ghoul_assert(
        child._dependentNodes.empty(),
        "Nodes cannot depend on a node being detached"
    );
    ghoul_assert(child._parent != nullptr, "Node must be attached to a parent");

    const auto iter = std::find_if(
        _children.begin(),
        _children.end(),
        [&child] (const ghoul::mm_unique_ptr<SceneGraphNode>& c) {
            return &child == c.get();
        }
    );

    if (iter == _children.end()) {
        LERROR("Trying to detach a non-existing child");
    }

    traversePreOrder([](SceneGraphNode* node) {
        node->clearDependencies();
    });

    // Unset scene of child (and children recursively)
    if (_scene) {
        child.setScene(nullptr);
    }

    // Remove link between parent and child
    child._parent = nullptr;
    ghoul::mm_unique_ptr<SceneGraphNode> c = std::move(*iter);
    _children.erase(iter);

    return c;
}

void SceneGraphNode::clearChildren() {
    traversePreOrder([](SceneGraphNode* node) {
        node->clearDependencies();
    });
    for (const ghoul::mm_unique_ptr<SceneGraphNode>& c : _children) {
        if (_scene) {
            c->setScene(nullptr);
        }
        c->_parent = nullptr;
    }
    _children.clear();
}

void SceneGraphNode::addDependency(SceneGraphNode& dependency) {
    dependency._dependentNodes.push_back(this);
    _dependencies.push_back(&dependency);
    if (_scene) {
        _scene->markNodeRegistryDirty();
    }
}

void SceneGraphNode::removeDependency(SceneGraphNode& dependency) {
    dependency._dependentNodes.erase(
        std::remove(
            dependency._dependentNodes.begin(),
            dependency._dependentNodes.end(),
            this
        ),
        dependency._dependentNodes.end()
    );
    _dependencies.erase(
        std::remove(_dependencies.begin(), _dependencies.end(), &dependency),
        _dependencies.end()
    );

    if (_scene) {
        _scene->markNodeRegistryDirty();
    }
}

void SceneGraphNode::clearDependencies() {
    for (SceneGraphNode* dependency : _dependencies) {
        dependency->_dependentNodes.erase(
            std::remove(
                dependency->_dependentNodes.begin(),
                dependency->_dependentNodes.end(),
                this
            ),
            dependency->_dependentNodes.end()
        );
    }
    _dependencies.clear();

    if (_scene) {
        _scene->markNodeRegistryDirty();
    }
}

void SceneGraphNode::setDependencies(const std::vector<SceneGraphNode*>& dependencies) {
    clearDependencies();

    _dependencies = dependencies;
    for (SceneGraphNode* dependency : dependencies) {
        dependency->_dependentNodes.push_back(this);
    }

    if (_scene) {
        _scene->markNodeRegistryDirty();
    }
}

void SceneGraphNode::computeScreenSpaceData(RenderData& newData) {
    // Purposely slow the update rate of screen space position in order to reduce the
    // effects of jittering in the position of information icon markers in web gui.
    auto now = std::chrono::high_resolution_clock::now();
    if ((now - _lastScreenSpaceUpdateTime) < std::chrono::milliseconds(100)) {
        return;
    }
    _lastScreenSpaceUpdateTime = now;

    // Calculate ndc
    const Camera& cam = newData.camera;
    const glm::dvec3& worldPos = _worldPositionCached;
    const glm::dvec4 clipSpace = glm::dmat4(cam.projectionMatrix()) *
                                 cam.combinedViewMatrix() * glm::vec4(worldPos, 1.0);
    const glm::dvec2 worldPosNDC = glm::dvec2(clipSpace / clipSpace.w);

    const bool visible = worldPosNDC.x >= -1.0 && worldPosNDC.x <= 1.0 &&
                         worldPosNDC.y >= -1.0 && worldPosNDC.y <= 1.0 && clipSpace.z > 0;

    // If not on the screen, we want to reset it or don't update it
    if (!visible) {
        _screenVisibility = false;
        return;
    }

    glm::ivec2 res = global::windowDelegate->currentSubwindowSize();

    // Get the radius of node
    double nodeRadius = boundingSphere();

    // Distance from the camera to the node
    double distFromCamToNode = glm::distance(cam.positionVec3(), worldPos) - nodeRadius;

    // Fix to limit the update of properties
    if (distFromCamToNode >= _visibilityDistance) {
        _screenVisibility = false;
        return;
    }

    _screenVisibility = true;

    // Calculate the node radius to screensize pixels
    const glm::dvec3 lookUp = normalize(cam.lookUpVectorWorldSpace());
    const glm::dvec3 radiusPos = worldPos + (nodeRadius * lookUp);
    const glm::dvec4 clipSpaceRadius = glm::dmat4(cam.projectionMatrix()) *
                                    cam.combinedViewMatrix() * glm::vec4(radiusPos, 1.0);
    const glm::dvec3 radiusNDC = clipSpaceRadius / clipSpaceRadius.w;

    const glm::ivec2 centerScreenSpace = glm::ivec2(
        (worldPosNDC.x + 1.0) * res.x / 2,
        (worldPosNDC.y + 1.0) * res.y / 2
    );
    const glm::ivec2 radiusScreenSpace = glm::ivec2(
        (radiusNDC.x + 1.0) * res.x / 2,
        (radiusNDC.y + 1.0) * res.y / 2
    );
    const double screenSpaceRadius = length(
        glm::vec2(centerScreenSpace) - glm::vec2(radiusScreenSpace)
    );

    constexpr double RadiusThreshold = 2.0;
    const double r = std::fabs(_screenSizeRadius - screenSpaceRadius);
    if (r > RadiusThreshold) {
        _screenSizeRadius = screenSpaceRadius;
    }

    constexpr double ZoomThreshold = 0.1;
    const double d = std::fabs(_distFromCamToNode - distFromCamToNode);
    if (d > (ZoomThreshold * distFromCamToNode)) {
        _distFromCamToNode = distFromCamToNode;
    }

    constexpr double MoveThreshold = 1.0;
    const glm::ivec2 ssp = _screenSpacePosition;
    const glm::dvec2 c = glm::abs(ssp - centerScreenSpace);
    if (c.x > MoveThreshold || c.y > MoveThreshold) {
        _screenSpacePosition = centerScreenSpace;
    }
}

SurfacePositionHandle SceneGraphNode::calculateSurfacePositionHandle(
                                                 const glm::dvec3& targetModelSpace) const
{
    ghoul_assert(glm::length(targetModelSpace) > 0.0, "Cannot have degenerate vector");

    if (_renderable) {
        return _renderable->calculateSurfacePositionHandle(targetModelSpace);
    }
    else {
        const glm::dvec3 directionFromCenterToTarget = glm::normalize(targetModelSpace);
        return {
            directionFromCenterToTarget * interactionSphere(),
            directionFromCenterToTarget,
            0.0
        };
    }
}

const std::vector<SceneGraphNode*>& SceneGraphNode::dependencies() const {
    return _dependencies;
}

const std::vector<SceneGraphNode*>& SceneGraphNode::dependentNodes() const {
    return _dependentNodes;
}

glm::dvec3 SceneGraphNode::position() const {
    return _transform.translation->position();
}

const glm::dmat3& SceneGraphNode::rotationMatrix() const {
    return _transform.rotation->matrix();
}

glm::dvec3 SceneGraphNode::scale() const {
    return _transform.scale->scaleValue();
}

glm::dvec3 SceneGraphNode::worldPosition() const {
    return _worldPositionCached;
}

const glm::dmat3& SceneGraphNode::worldRotationMatrix() const {
    return _worldRotationCached;
}

glm::dmat4 SceneGraphNode::modelTransform() const {
    return _modelTransformCached;
}

glm::dvec3 SceneGraphNode::worldScale() const {
    return _worldScaleCached;
}

std::string SceneGraphNode::guiPath() const {
    return _guiPath;
}

bool SceneGraphNode::hasGuiHintHidden() const {
    return _guiHidden;
}

glm::dvec3 SceneGraphNode::calculateWorldPosition() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        const glm::dvec3 wp = _parent->worldPosition();
        const glm::dmat3 wrot = _parent->worldRotationMatrix();
        const glm::dvec3 ws = _parent->worldScale();
        const glm::dvec3 p = position();

        return wp + wrot * (ws * p);
    }
    else {
        return position();
    }
}

bool SceneGraphNode::isTimeFrameActive(const Time& time) const {
    for (SceneGraphNode* dep : _dependencies) {
        if (!dep->isTimeFrameActive(time)) {
            return false;
        }
    }

    if (_parent && !_parent->isTimeFrameActive(time)) {
        return false;
    }

    return !_timeFrame || _timeFrame->isActive(time);
}

glm::dmat3 SceneGraphNode::calculateWorldRotation() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        return _parent->worldRotationMatrix() * rotationMatrix();
    }
    else {
        return rotationMatrix();
    }
}

glm::dvec3 SceneGraphNode::calculateWorldScale() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        return _parent->worldScale() * scale();
    }
    else {
        return scale();
    }
}

SceneGraphNode* SceneGraphNode::parent() const {
    return _parent;
}

Scene* SceneGraphNode::scene() {
    return _scene;
}

void SceneGraphNode::setScene(Scene* scene) {
    ZoneScoped

    // Unregister from previous scene, bottom up
    traversePostOrder([](SceneGraphNode* node) {
        if (node->_scene) {
            node->_scene->unregisterNode(node);
        }
        node->_scene = nullptr;
    });

    if (!scene) {
        return;
    }

    // Register on new scene, top down
    traversePreOrder([scene](SceneGraphNode* node) {
        node->_scene = scene;
        if (scene) {
            scene->registerNode(node);
        }
    });
}

std::vector<SceneGraphNode*> SceneGraphNode::children() const {
    std::vector<SceneGraphNode*> nodes;
    for (const ghoul::mm_unique_ptr<SceneGraphNode>& child : _children) {
        nodes.push_back(child.get());
    }
    return nodes;
}

const std::vector<std::string>& SceneGraphNode::onApproachAction() const {
    return _onApproachAction;
}

const std::vector<std::string>& SceneGraphNode::onReachAction() const {
    return _onReachAction;
}

const std::vector<std::string>& SceneGraphNode::onRecedeAction() const {
    return _onRecedeAction;
}

const std::vector<std::string>& SceneGraphNode::onExitAction() const {
    return _onExitAction;
}

double SceneGraphNode::boundingSphere() const {
    if (_overrideBoundingSphere.has_value()) {
        return glm::compMax(scale() * *_overrideBoundingSphere);
    }

    if (_renderable) {
        return glm::compMax(scale() * _renderable->boundingSphere());
    }
    else {
        return 0.0;
    }
}

double SceneGraphNode::interactionSphere() const {
    if (_overrideInteractionSphere.has_value()) {
        return glm::compMax(scale() * *_overrideInteractionSphere);
    }

    if (_renderable) {
        return glm::compMax(scale() * _renderable->interactionSphere());
    }
    else {
        return 0.0;
    }
}

double SceneGraphNode::reachFactor() const {
    return _reachFactor;
}

double SceneGraphNode::approachFactor() const {
    return _approachFactor;
}

const Renderable* SceneGraphNode::renderable() const {
    return _renderable.get();
}

Renderable* SceneGraphNode::renderable() {
    return _renderable.get();
}

SceneGraphNode* SceneGraphNode::childNode(const std::string& id) {
    if (identifier() == id) {
        return this;
    }
    else {
        for (ghoul::mm_unique_ptr<SceneGraphNode>& it : _children) {
            SceneGraphNode* tmp = it->childNode(id);
            if (tmp) {
                return tmp;
            }
        }
    }
    return nullptr;
}

}  // namespace openspace
