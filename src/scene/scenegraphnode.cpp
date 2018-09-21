/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/timeframe.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
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
    }

    if (dictionary.hasKey(KeyGuiHidden)) {
        result->_guiHintHidden = dictionary.value<bool>(KeyGuiHidden);
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
        result->addPropertySubOwner(result->_transform.translation.get());
        LDEBUG(fmt::format(
            "Successfully created ephemeris for '{}'", result->identifier()
        ));
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
        result->addPropertySubOwner(result->_transform.rotation.get());
        LDEBUG(fmt::format(
            "Successfully created rotation for '{}'", result->identifier()
        ));
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
        result->addPropertySubOwner(result->_transform.scale.get());
        LDEBUG(fmt::format("Successfully created scale for '{}'", result->identifier()));
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

        renderableDictionary.setValue(KeyIdentifier, identifier);

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
        } else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyTag)) {
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
    }

    LDEBUG(fmt::format("Successfully created SceneGraphNode '{}'", result->identifier()));
    return result;
}

SceneGraphNode::SceneGraphNode()
    : properties::PropertyOwner({ "" })
    , _transform {
        std::make_unique<StaticTranslation>(),
        std::make_unique<StaticRotation>(),
        std::make_unique<StaticScale>()
    }
{}

SceneGraphNode::~SceneGraphNode() {} // NOLINT

void SceneGraphNode::initialize() {
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
    LDEBUG(fmt::format("Initializing GL: {}", identifier()));

    if (_renderable) {
        _renderable->initializeGL();
    }
    _state = State::GLInitialized;

    LDEBUG(fmt::format("Finished initializating GL: {}", identifier()));
}

void SceneGraphNode::deinitialize() {
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

    _worldRotationCached = calculateWorldRotation();
    _worldScaleCached = calculateWorldScale();
    // Assumes _worldRotationCached and _worldScaleCached have been calculated for parent
    _worldPositionCached = calculateWorldPosition();

    newUpdateData.modelTransform.translation = worldPosition();
    newUpdateData.modelTransform.rotation = worldRotationMatrix();
    newUpdateData.modelTransform.scale = worldScale();

    glm::dmat4 translation = glm::translate(
        glm::dmat4(1.0),
        newUpdateData.modelTransform.translation
    );
    glm::dmat4 rotation = glm::dmat4(newUpdateData.modelTransform.rotation);
    glm::dmat4 scaling = glm::scale(
        glm::dmat4(1.0),
        glm::dvec3(
            newUpdateData.modelTransform.scale,
            newUpdateData.modelTransform.scale,
            newUpdateData.modelTransform.scale
        )
    );

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
    if (_state != State::GLInitialized) {
        return;
    }
    const psc thisPositionPSC = psc::CreatePowerScaledCoordinate(
        _worldPositionCached.x,
        _worldPositionCached.y,
        _worldPositionCached.z
    );

    RenderData newData = {
        data.camera,
        thisPositionPSC,
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

        glFinish();
        auto end = std::chrono::high_resolution_clock::now();
        _performanceRecord.renderTime = (end - start).count();
    }
    else {
        _renderable->render(newData, tasks);
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

SurfacePositionHandle SceneGraphNode::calculateSurfacePositionHandle(
                                                       const glm::dvec3& targetModelSpace)
{
    if (_renderable) {
        return _renderable->calculateSurfacePositionHandle(targetModelSpace);
    }
    else {
        return {
            glm::dvec3(0.0, 0.0, 0.0),
            glm::normalize(targetModelSpace),
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

double SceneGraphNode::scale() const {
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

double SceneGraphNode::worldScale() const {
    return _worldScaleCached;
}

const std::string& SceneGraphNode::guiPath() const {
    return _guiPath;
}

bool SceneGraphNode::hasGuiHintHidden() const {
    return _guiHintHidden;
}

glm::dvec3 SceneGraphNode::calculateWorldPosition() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        const glm::dvec3 wp = _parent->calculateWorldPosition();
        const glm::dmat3 wrot = _parent->worldRotationMatrix();
        const double ws = _parent->worldScale();
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
        return rotationMatrix() * _parent->calculateWorldRotation();
    }
    else {
        return rotationMatrix();
    }
}

double SceneGraphNode::calculateWorldScale() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        return _parent->calculateWorldScale() * scale();
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

float SceneGraphNode::boundingSphere() const{
    if (_renderable) {
        return _renderable->boundingSphere();
    }
    return 0.0;
}

// renderable
void SceneGraphNode::setRenderable(std::unique_ptr<Renderable> renderable) {
    _renderable = std::move(renderable);
}

const Renderable* SceneGraphNode::renderable() const {
    return _renderable.get();
}

//Renderable* SceneGraphNode::renderable() {
//    return _renderable.get();
//}

/*
bool SceneGraphNode::sphereInsideFrustum(const psc& s_pos, const PowerScaledScalar& s_rad,
                                         const Camera* camera)
{
    // direction the camera is looking at in power scale
    psc psc_camdir = psc(glm::vec3(camera->viewDirectionWorldSpace()));

    // the position of the camera, moved backwards in the view direction to encapsulate
    // the sphere radius
    psc U = camera->position() - psc_camdir * s_rad * (1.0 / camera->sinMaxFov());

    // the vector to the object from the new position
    psc D = s_pos - U;

    const double a = psc_camdir.angle(D);
    if (a < camera->maxFov()) {
        // center is inside K''
        D = s_pos - camera->position();
        if (D.length() * psc_camdir.length() * camera->sinMaxFov()
            <= -psc_camdir.dot(D)) {
            // center is inside K'' and inside K'
            return D.length() <= s_rad;
        } else {
            // center is inside K'' and outside K'
            return true;
        }
    } else {
        // outside the maximum angle
        return false;
    }
}
*/

SceneGraphNode* SceneGraphNode::childNode(const std::string& identifier) {
    if (this->identifier() == identifier) {
        return this;
    }
    else {
        for (std::unique_ptr<SceneGraphNode>& it : _children) {
            SceneGraphNode* tmp = it->childNode(identifier);
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

void SceneGraphNode::updateCamera(Camera* camera) const {
    psc origin(worldPosition());
    //int i = 0;
    // the camera position

    psc relative = camera->position();
    psc focus = camera->focusPosition();
    psc relative_focus = relative - focus;

    psc target = origin + relative_focus;

    camera->setPosition(target);
    camera->setFocusPosition(origin);
}

}  // namespace openspace
