/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/timeframe.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include "scenegraphnode_doc.inl"

namespace {
    constexpr const char* _loggerCat = "SceneGraphNode";
    constexpr const char* KeyRenderable = "Renderable";
    constexpr const char* KeyGuiName = "GUI.Name";
    constexpr const char* KeyGuiPath = "GUI.Path";
    constexpr const char* KeyGuiHidden = "GUI.Hidden";

    constexpr const char* KeyTransformTranslation = "Transform.Translation";
    constexpr const char* KeyTransformRotation = "Transform.Rotation";
    constexpr const char* KeyTransformScale = "Transform.Scale";

    constexpr const char* KeyTimeFrame = "TimeFrame";

    constexpr openspace::properties::Property::PropertyInfo ComputeScreenSpaceInfo =
    {
        "ComputeScreenSpaceData",
        "Screen Space Data",
        "If this value is set to 'true', the screenspace-based properties are calculated "
        "at regular intervals. If these values are set to 'false', they are not updated."
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenSpacePositionInfo = {
        "ScreenSpacePosition",
        "ScreenSpacePosition",
        "" // @TODO Missing documentation
    };
    constexpr openspace::properties::Property::PropertyInfo ScreenVisibilityInfo = {
        "ScreenVisibility",
        "ScreenVisibility",
        "" // @TODO Missing documentation
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceFromCamToNodeInfo = {
        "DistanceFromCamToNode",
        "DistanceFromCamToNode",
        "" // @TODO Missing documentation
    };
    constexpr openspace::properties::Property::PropertyInfo ScreenSizeRadiusInfo = {
        "ScreenSizeRadius",
        "ScreenSizeRadius",
        "" // @TODO Missing documentation
    };
    constexpr openspace::properties::Property::PropertyInfo VisibilityDistanceInfo = {
        "VisibilityDistance",
        "VisibilityDistance",
        "" // @TODO Missing documentation
    };
    constexpr const char* KeyFixedBoundingSphere = "FixedBoundingSphere";

    constexpr openspace::properties::Property::PropertyInfo BoundingSphereInfo = {
        "BoundingSphere",
        "Bounding Sphere",
        "The bounding sphere of the scene graph node. This can be the "
        "bounding sphere of a renderable or a fixed bounding sphere. "
    };

    constexpr openspace::properties::Property::PropertyInfo GuiPathInfo = {
        "GuiPath",
        "Gui Path",
        "This is the path for the scene graph node in the gui "
        "example: /Solar System/Planets/Earth",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo GuiNameInfo = {
        "GuiName",
        "Gui Name",
        "This is the name for the scene graph node in the gui "
        "example: Earth",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo GuiHiddenInfo = {
        "GuiHidden",
        "Gui Hidden",
        "This represents if the scene graph node should be shown in the gui "
        "example: false",
        openspace::properties::Property::Visibility::Hidden
    };

} // namespace

namespace openspace {

#ifdef Debugging_Core_SceneGraphNode_Indices
int SceneGraphNode::nextIndex = 0;
#endif // Debugging_Core_SceneGraphNode_Indices

std::unique_ptr<SceneGraphNode> SceneGraphNode::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        SceneGraphNode::Documentation(),
        dictionary,
        "SceneGraphNode"
    );

    std::unique_ptr<SceneGraphNode> result = std::make_unique<SceneGraphNode>();
#ifdef Debugging_Core_SceneGraphNode_Indices
    result->index = nextIndex++;
#endif // Debugging_Core_SceneGraphNode_Indices

    std::string identifier = dictionary.value<std::string>(KeyIdentifier);
    result->setIdentifier(std::move(identifier));

    if (dictionary.hasKey(KeyGuiName)) {
        result->setGuiName(dictionary.value<std::string>(KeyGuiName));
        result->_guiDisplayName = result->guiName();
        result->addProperty(result->_guiDisplayName);
    }

    if (dictionary.hasKey(KeyGuiHidden)) {
        result->_guiHidden = dictionary.value<bool>(KeyGuiHidden);
        result->addProperty(result->_guiHidden);
    }

    if (dictionary.hasKey(KeyTransformTranslation)) {
        ghoul::Dictionary translationDictionary;
        dictionary.getValue(KeyTransformTranslation, translationDictionary);
        result->_transform.translation = Translation::createFromDictionary(
            translationDictionary
        );
        if (result->_transform.translation == nullptr) {
            LERROR(fmt::format(
                "Failed to create ephemeris for SceneGraphNode '{}'", result->identifier()
            ));
            return nullptr;
        }
        LDEBUG(fmt::format(
            "Successfully created ephemeris for '{}'", result->identifier()
        ));
    }
    if (result->_transform.translation) {
        result->addPropertySubOwner(result->_transform.translation.get());
    }

    if (dictionary.hasKey(KeyTransformRotation)) {
        ghoul::Dictionary rotationDictionary;
        dictionary.getValue(KeyTransformRotation, rotationDictionary);
        result->_transform.rotation = Rotation::createFromDictionary(rotationDictionary);
        if (result->_transform.rotation == nullptr) {
            LERROR(fmt::format(
                "Failed to create rotation for SceneGraphNode '{}'", result->identifier()
            ));
            return nullptr;
        }
        LDEBUG(fmt::format(
            "Successfully created rotation for '{}'", result->identifier()
        ));
    }
    if (result->_transform.rotation) {
        result->addPropertySubOwner(result->_transform.rotation.get());
    }

    if (dictionary.hasKey(KeyTransformScale)) {
        ghoul::Dictionary scaleDictionary;
        dictionary.getValue(KeyTransformScale, scaleDictionary);
        result->_transform.scale = Scale::createFromDictionary(scaleDictionary);
        if (result->_transform.scale == nullptr) {
            LERROR(fmt::format(
                "Failed to create scale for SceneGraphNode '{}'", result->identifier()
            ));
            return nullptr;
        }
        LDEBUG(fmt::format("Successfully created scale for '{}'", result->identifier()));
    }
    if (result->_transform.scale) {
        result->addPropertySubOwner(result->_transform.scale.get());
    }

    if (dictionary.hasKey(KeyTimeFrame)) {
        ghoul::Dictionary timeFrameDictionary;
        dictionary.getValue(KeyTimeFrame, timeFrameDictionary);
        result->_timeFrame = TimeFrame::createFromDictionary(timeFrameDictionary);
        if (result->_timeFrame == nullptr) {
            LERROR(fmt::format(
                "Failed to create time frame for SceneGraphNode '{}'",
                result->identifier()
            ));
            return nullptr;
        }
        result->addPropertySubOwner(result->_timeFrame.get());
        LDEBUG(fmt::format(
            "Successfully created time frame for '{}'",
            result->identifier()
        ));
    }

    // We initialize the renderable last as it probably has the most dependencies
    if (dictionary.hasValue<ghoul::Dictionary>(KeyRenderable)) {
        ghoul::Dictionary renderableDictionary;
        dictionary.getValue(KeyRenderable, renderableDictionary);

        renderableDictionary.setValue(KeyIdentifier, result->_identifier);

        result->_renderable = Renderable::createFromDictionary(renderableDictionary);
        if (result->_renderable == nullptr) {
            LERROR(fmt::format(
                "Failed to create renderable for SceneGraphNode '{}'",
                result->identifier()
            ));
            return nullptr;
        }
        result->addPropertySubOwner(result->_renderable.get());
        LDEBUG(fmt::format(
            "Successfully created renderable for '{}'", result->identifier()
        ));
    }

    if (dictionary.hasKey(KeyTag)) {
        if (dictionary.hasKeyAndValue<std::string>(KeyTag)) {
            std::string tagName = dictionary.value<std::string>(KeyTag);
            if (!tagName.empty()) {
                result->addTag(std::move(tagName));
            }
        }
        else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyTag)) {
            ghoul::Dictionary tagNames = dictionary.value<ghoul::Dictionary>(KeyTag);
            std::vector<std::string> keys = tagNames.keys();
            std::string tagName;
            for (const std::string& key : keys) {
                tagName = tagNames.value<std::string>(key);
                if (!tagName.empty()) {
                    result->addTag(std::move(tagName));
                }
            }
        }
    }

    if (dictionary.hasKey(KeyGuiPath)) {
        result->_guiPath = dictionary.value<std::string>(KeyGuiPath);
        result->addProperty(result->_guiPath);
    }

    if (dictionary.hasKey(KeyFixedBoundingSphere)) {
        result->_fixedBoundingSphere = static_cast<float>(
            dictionary.value<double>(KeyFixedBoundingSphere)
        );
    }

    LDEBUG(fmt::format("Successfully created SceneGraphNode '{}'", result->identifier()));

    result->_lastScreenSpaceUpdateTime = std::chrono::high_resolution_clock::now();
    return result;
}

SceneGraphNode::SceneGraphNode()
    : properties::PropertyOwner({ "" })
    , _guiHidden(GuiHiddenInfo)
    , _guiPath(GuiPathInfo)
    , _guiDisplayName(GuiNameInfo)
    , _transform {
        std::make_unique<StaticTranslation>(),
        std::make_unique<StaticRotation>(),
        std::make_unique<StaticScale>()
    }
    , _computeScreenSpaceValues(ComputeScreenSpaceInfo, false)
    , _screenSpacePosition(
        properties::IVec2Property(ScreenSpacePositionInfo, glm::ivec2(-1, -1))
    )
    , _screenVisibility(properties::BoolProperty(ScreenVisibilityInfo, false))
    , _distFromCamToNode(properties::DoubleProperty(DistanceFromCamToNodeInfo, -1.0))
    , _screenSizeRadius(properties::DoubleProperty(ScreenSizeRadiusInfo, 0))
    , _visibilityDistance(properties::FloatProperty(VisibilityDistanceInfo, 6e10f))
{
    addProperty(_computeScreenSpaceValues);
    addProperty(_screenSpacePosition);
    addProperty(_screenVisibility);
    addProperty(_distFromCamToNode);
    addProperty(_screenSizeRadius);
    addProperty(_visibilityDistance);
}

SceneGraphNode::~SceneGraphNode() {} // NOLINT

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
    for (std::unique_ptr<SceneGraphNode>& child : _children) {
        child->traversePreOrder(fn);
    }
}

void SceneGraphNode::traversePostOrder(const std::function<void(SceneGraphNode*)>& fn) {
    for (std::unique_ptr<SceneGraphNode>& child : _children) {
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
        if (data.doPerformanceMeasurement) {
            glFinish();
            const auto start = std::chrono::high_resolution_clock::now();

            _transform.translation->update(data);

            glFinish();
            const auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeTranslation = (end - start).count();
        }
        else {
            _transform.translation->update(data);
        }
    }

    if (_transform.rotation) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            const auto start = std::chrono::high_resolution_clock::now();

            _transform.rotation->update(data);

            glFinish();
            const auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeRotation = (end - start).count();
        }
        else {
            _transform.rotation->update(data);
        }
    }

    if (_transform.scale) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            const auto start = std::chrono::high_resolution_clock::now();

            _transform.scale->update(data);

            glFinish();
            const auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeScaling = (end - start).count();
        }
        else {
            _transform.scale->update(data);
        }
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
    _inverseModelTransformCached = glm::inverse(_modelTransformCached);

    if (_renderable && _renderable->isReady()) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            auto start = std::chrono::high_resolution_clock::now();

            _renderable->update(newUpdateData);

            glFinish();
            auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeRenderable = (end - start).count();
        }
        else {
            _renderable->update(newUpdateData);
        }
    }
}

void SceneGraphNode::render(const RenderData& data, RendererTasks& tasks) {
    ZoneScoped
    ZoneName(identifier().c_str(), identifier().size())

    if (_state != State::GLInitialized) {
        return;
    }

    RenderData newData = {
        data.camera,
        data.time,
        data.doPerformanceMeasurement,
        data.renderBinMask,
        { _worldPositionCached, _worldRotationCached, _worldScaleCached }
    };

    if (!isTimeFrameActive(data.time)) {
        return;
    }

    bool visible = _renderable &&
                   _renderable->isVisible() &&
                   _renderable->isReady() &&
                   _renderable->isEnabled() &&
                   _renderable->matchesRenderBinMask(data.renderBinMask);

    if (!visible) {
        return;
    }

    if (data.doPerformanceMeasurement) {
        glFinish();
        auto start = std::chrono::high_resolution_clock::now();

        _renderable->render(newData, tasks);
        if (_computeScreenSpaceValues) {
            computeScreenSpaceData(newData);
        }

        glFinish();
        auto end = std::chrono::high_resolution_clock::now();
        _performanceRecord.renderTime = (end - start).count();
    }
    else {
        TracyGpuZone("Render")

        _renderable->render(newData, tasks);
        if (_computeScreenSpaceValues) {
            computeScreenSpaceData(newData);
        }
    }
}

void SceneGraphNode::setParent(SceneGraphNode& parent) {
    ghoul_assert(_parent != nullptr, "Node must be attached to a parent");

    parent.attachChild(_parent->detachChild(*this));
}

void SceneGraphNode::attachChild(std::unique_ptr<SceneGraphNode> child) {
    ghoul_assert(child != nullptr, "Child may not be null");
    ghoul_assert(child->parent() == nullptr, "Child may not already have a parent");

    // Create link between parent and child
    child->_parent = this;
    SceneGraphNode* childRaw = child.get();
    _children.push_back(std::move(child));

    // Set scene of child (and children recursively)
    childRaw->setScene(_scene);
}

std::unique_ptr<SceneGraphNode> SceneGraphNode::detachChild(SceneGraphNode& child) {
    ghoul_assert(
        child._dependentNodes.empty(),
        "Nodes cannot depend on a node being detached"
    );
    ghoul_assert(child._parent != nullptr, "Node must be attached to a parent");

    const auto iter = std::find_if(
        _children.begin(),
        _children.end(),
        [&child] (const std::unique_ptr<SceneGraphNode>& c) {
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
    std::unique_ptr<SceneGraphNode> c = std::move(*iter);
    _children.erase(iter);

    return c;
}

void SceneGraphNode::clearChildren() {
    traversePreOrder([](SceneGraphNode* node) {
        node->clearDependencies();
    });
    for (const std::unique_ptr<SceneGraphNode>& c : _children) {
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
        std::remove(
            _dependencies.begin(),
            _dependencies.end(),
            &dependency
        ),
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

    glm::ivec2 res = global::windowDelegate.currentSubwindowSize();

    // Get the radius of node
    double nodeRadius = static_cast<double>(this->boundingSphere());

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

    constexpr const double RadiusThreshold = 2.0;
    const double r = abs(_screenSizeRadius - screenSpaceRadius);
    if (r > RadiusThreshold) {
        _screenSizeRadius = screenSpaceRadius;
    }

    constexpr const double ZoomThreshold = 0.1;
    const double d = abs(_distFromCamToNode - distFromCamToNode);
    if (d > (ZoomThreshold * distFromCamToNode)) {
        _distFromCamToNode = distFromCamToNode;
    }

    constexpr const double MoveThreshold = 1.0;
    const glm::ivec2 ssp = _screenSpacePosition;
    const glm::dvec2 c = glm::abs(ssp - centerScreenSpace);
    if (c.x > MoveThreshold || c.y > MoveThreshold) {
        _screenSpacePosition = centerScreenSpace;
    }
}

SurfacePositionHandle SceneGraphNode::calculateSurfacePositionHandle(
                                                 const glm::dvec3& targetModelSpace) const
{
    if (_renderable) {
        return _renderable->calculateSurfacePositionHandle(targetModelSpace);
    }
    else {
        return { glm::dvec3(0.0), glm::normalize(targetModelSpace), 0.0 };
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

glm::dmat4 SceneGraphNode::inverseModelTransform() const {
    return _inverseModelTransformCached;
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

        return wp + wrot * ws * p;
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
    for (const std::unique_ptr<SceneGraphNode>& child : _children) {
        nodes.push_back(child.get());
    }
    return nodes;
}

float SceneGraphNode::boundingSphere() const {
    if (_renderable) {
        return _renderable->boundingSphere();
    }
    return _fixedBoundingSphere;
}

// renderable
void SceneGraphNode::setRenderable(std::unique_ptr<Renderable> renderable) {
    _renderable = std::move(renderable);
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
        for (std::unique_ptr<SceneGraphNode>& it : _children) {
            SceneGraphNode* tmp = it->childNode(id);
            if (tmp) {
                return tmp;
            }
        }
    }
    return nullptr;
}

const SceneGraphNode::PerformanceRecord& SceneGraphNode::performanceRecord() const {
    return _performanceRecord;
}

}  // namespace openspace
