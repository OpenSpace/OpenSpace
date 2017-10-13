/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/base/translation/statictranslation.h>
#include <modules/base/rotation/staticrotation.h>
#include <modules/base/scale/staticscale.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/translation.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/shadermanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/shaderobject.h>

#include <cctype>
#include <chrono>

#include "scenegraphnode_doc.inl"

namespace {
    const char* _loggerCat = "SceneGraphNode";
    const char* KeyRenderable = "Renderable";
    const char* KeyGuiGrouping = "GuiGrouping";

    const char* keyTransformTranslation = "Transform.Translation";
    const char* keyTransformRotation = "Transform.Rotation";
    const char* keyTransformScale = "Transform.Scale";
} // namespace

namespace openspace {

// Constants used outside of this file
const std::string SceneGraphNode::RootNodeName = "Root";
const std::string SceneGraphNode::KeyName = "Name";
const std::string SceneGraphNode::KeyParentName = "Parent";
const std::string SceneGraphNode::KeyDependencies = "Dependencies";
const std::string SceneGraphNode::KeyTag = "Tag";

std::unique_ptr<SceneGraphNode> SceneGraphNode::createFromDictionary(const ghoul::Dictionary& dictionary){
    openspace::documentation::testSpecificationAndThrow(
        SceneGraphNode::Documentation(),
        dictionary,
        "SceneGraphNode"
    );

    std::unique_ptr<SceneGraphNode> result = std::make_unique<SceneGraphNode>();

    std::string name = dictionary.value<std::string>(KeyName);
    result->setName(name);

    if (dictionary.hasValue<ghoul::Dictionary>(KeyRenderable)) {
        ghoul::Dictionary renderableDictionary;
        dictionary.getValue(KeyRenderable, renderableDictionary);

        renderableDictionary.setValue(KeyName, name);

        result->_renderable = Renderable::createFromDictionary(renderableDictionary);
        if (result->_renderable == nullptr) {
            LERROR("Failed to create renderable for SceneGraphNode '"
                   << result->name() << "'");
            return nullptr;
        }
        result->addPropertySubOwner(result->_renderable.get());
        LDEBUG("Successfully created renderable for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformTranslation)) {
        ghoul::Dictionary translationDictionary;
        dictionary.getValue(keyTransformTranslation, translationDictionary);
        result->_transform.translation = Translation::createFromDictionary(
            translationDictionary
        );
        if (result->_transform.translation == nullptr) {
            LERROR("Failed to create ephemeris for SceneGraphNode '"
                << result->name() << "'");
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.translation.get());
        LDEBUG("Successfully created ephemeris for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformRotation)) {
        ghoul::Dictionary rotationDictionary;
        dictionary.getValue(keyTransformRotation, rotationDictionary);
        result->_transform.rotation = Rotation::createFromDictionary(rotationDictionary);
        if (result->_transform.rotation == nullptr) {
            LERROR("Failed to create rotation for SceneGraphNode '"
                << result->name() << "'");
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.rotation.get());
        LDEBUG("Successfully created rotation for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformScale)) {
        ghoul::Dictionary scaleDictionary;
        dictionary.getValue(keyTransformScale, scaleDictionary);
        result->_transform.scale = Scale::createFromDictionary(scaleDictionary);
        if (result->_transform.scale == nullptr) {
            LERROR("Failed to create scale for SceneGraphNode '"
                << result->name() << "'");
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.scale.get());
        LDEBUG("Successfully created scale for '" << result->name() << "'");
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

    if (dictionary.hasKey(KeyGuiGrouping)) {
        result->_guiGroup = dictionary.value<std::string>(KeyGuiGrouping);
    }

    LDEBUG("Successfully created SceneGraphNode '"
                   << result->name() << "'");
    return result;
}

SceneGraphNode::SceneGraphNode()
    : properties::PropertyOwner({ "" })
    , _parent(nullptr)
    , _scene(nullptr)
    , _performanceRecord({0, 0, 0, 0, 0})
    , _renderable(nullptr)
    , _transform {
        std::make_unique<StaticTranslation>(),
        std::make_unique<StaticRotation>(),
        std::make_unique<StaticScale>()
    }
{}

SceneGraphNode::~SceneGraphNode() {
    deinitialize();
}

void SceneGraphNode::initialize() {
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
}

void SceneGraphNode::deinitialize() {
    LDEBUG("Deinitialize: " << name());

    if (_renderable) {
        _renderable->deinitialize();
        _renderable = nullptr;
    }
    _children.clear();

    _parent = nullptr;
}

void SceneGraphNode::traversePreOrder(std::function<void(SceneGraphNode*)> fn) {
    fn(this);
    for (std::unique_ptr<SceneGraphNode>& child : _children) {
        child->traversePreOrder(fn);
    }
}

void SceneGraphNode::traversePostOrder(std::function<void(SceneGraphNode*)> fn) {
    for (std::unique_ptr<SceneGraphNode>& child : _children) {
        child->traversePostOrder(fn);
    }
    fn(this);
}

void SceneGraphNode::update(const UpdateData& data) {
    if (_transform.translation) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            auto start = std::chrono::high_resolution_clock::now();

            _transform.translation->update(data);

            glFinish();
            auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeTranslation = (end - start).count();
        }
        else {
            _transform.translation->update(data);
        }
    }

    if (_transform.rotation) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            auto start = std::chrono::high_resolution_clock::now();

            _transform.rotation->update(data);

            glFinish();
            auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.updateTimeRotation = (end - start).count();
        }
        else {
            _transform.rotation->update(data);
        }
    }

    if (_transform.scale) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            auto start = std::chrono::high_resolution_clock::now();

            _transform.scale->update(data);

            glFinish();
            auto end = std::chrono::high_resolution_clock::now();
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

    glm::dmat4 translation =
        glm::translate(glm::dmat4(1.0), newUpdateData.modelTransform.translation);
    glm::dmat4 rotation = glm::dmat4(newUpdateData.modelTransform.rotation);
    glm::dmat4 scaling =
        glm::scale(glm::dmat4(1.0), glm::dvec3(newUpdateData.modelTransform.scale,
            newUpdateData.modelTransform.scale, newUpdateData.modelTransform.scale));

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
        else
            _renderable->update(newUpdateData);
    }
}

void SceneGraphNode::render(const RenderData& data, RendererTasks& tasks) {
    const psc thisPositionPSC = psc::CreatePowerScaledCoordinate(_worldPositionCached.x, _worldPositionCached.y, _worldPositionCached.z);

    RenderData newData = {
        data.camera,
        thisPositionPSC,
        data.time,
        data.doPerformanceMeasurement,
        data.renderBinMask,
        { _worldPositionCached, _worldRotationCached, _worldScaleCached }
    };

    //_performanceRecord.renderTime = 0;

    bool visible = _renderable &&
        _renderable->isVisible() &&
        _renderable->isReady() &&
        _renderable->isEnabled() &&
        _renderable->matchesRenderBinMask(data.renderBinMask);

    if (visible) {
        if (data.doPerformanceMeasurement) {
            glFinish();
            auto start = std::chrono::high_resolution_clock::now();

            _renderable->render(newData, tasks);

            glFinish();
            auto end = std::chrono::high_resolution_clock::now();
            _performanceRecord.renderTime = (end - start).count();
        }
        else
            _renderable->render(newData, tasks);
    }

    // evaluate all the children, tail-recursive function(?)

    //for (SceneGraphNode* child : _children)
    //    child->render(newData);
}

void SceneGraphNode::setParent(SceneGraphNode& parent, UpdateScene updateScene) {
    ghoul_assert(_parent != nullptr, "Node must be attached to a parent");
    ghoul_assert(
        !updateScene || _scene == parent._scene,
        "For the scene to be updated, this object must belong to the same scene as the parent"
    );
    ghoul_assert(
        !updateScene || _parent->_scene == parent._scene,
        "Old and new parent cannot belong to separate scenes"
    );

    parent.attachChild(_parent->detachChild(*this, UpdateScene::No), UpdateScene::No);

    if (_scene && updateScene) {
        _scene->updateDependencies();
    }
}

void SceneGraphNode::attachChild(std::unique_ptr<SceneGraphNode> child, UpdateScene updateScene) {
    ghoul_assert(child->parent() == nullptr, "Child may not already have a parent");

    child->_parent = this;
    if (_scene) {
        child->setScene(_scene);
    }

   _children.push_back(std::move(child));

   if (_scene && updateScene) {
       _scene->addNode(child.get());
   }
}

std::unique_ptr<SceneGraphNode> SceneGraphNode::detachChild(SceneGraphNode& child, UpdateScene updateScene) {
    ghoul_assert(child._dependentNodes.empty(), "Nodes cannot depend on a node being detached");
    ghoul_assert(child._parent != nullptr, "Node must be attached to a parent");
    
    // Update of deps is deffered to the removal of the node from the scene
    clearDependencies(UpdateScene::No);

    auto iter = std::find_if(
        _children.begin(),
        _children.end(),
        [&child] (const auto& c) {
            return &child == c.get();
        }
    );

    std::unique_ptr<SceneGraphNode> c = std::move(*iter);
    _children.erase(iter);

    if (_scene && updateScene) {
        _scene->removeNode(&child);
    }

    if (_scene) {
        setScene(nullptr);
    }
    return c;
}

void SceneGraphNode::addDependency(SceneGraphNode& dependency, UpdateScene updateScene) {
    dependency._dependentNodes.push_back(this);
    _dependencies.push_back(&dependency);

    if (_scene && updateScene) {
        _scene->updateDependencies();
    }
}

void SceneGraphNode::removeDependency(SceneGraphNode& dependency, UpdateScene updateScene) {
    dependency._dependentNodes.erase(std::remove_if(
        dependency._dependentNodes.begin(),
        dependency._dependentNodes.end(),
        [this](const auto& d) {
            return this == d;
        }
    ), dependency._dependentNodes.end());
    _dependencies.erase(std::remove_if(
        _dependencies.begin(),
        _dependencies.end(),
        [&dependency](const auto& d) {
            return &dependency == d;
        }
    ), _dependencies.end());

    if (_scene && updateScene) {
        _scene->updateDependencies();
    }
}

void SceneGraphNode::clearDependencies(UpdateScene updateScene) {
    for (auto dependency : _dependencies) {
        dependency->_dependentNodes.erase(std::remove_if(
            dependency->_dependentNodes.begin(),
            dependency->_dependentNodes.end(),
            [this](const auto& d) {
                return this == d;
            }
        ), dependency->_dependentNodes.end());
    }
    _dependencies.clear();

    if (_scene && updateScene) {
        _scene->updateDependencies();
    }
}

void SceneGraphNode::setDependencies(const std::vector<SceneGraphNode*>& dependencies, UpdateScene updateScene) {
    clearDependencies(UpdateScene::No);

    _dependencies = dependencies;
    for (auto dependency : dependencies) {
        dependency->_dependentNodes.push_back(this);
    }

    if (_scene && updateScene) {
        _scene->updateDependencies();
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

const std::string& SceneGraphNode::guiGroup() const {
    return _guiGroup;
}

glm::dvec3 SceneGraphNode::calculateWorldPosition() const {
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        return
            _parent->calculateWorldPosition() +
            _parent->worldRotationMatrix() *
            _parent->worldScale() *
            position();
    }
    else {
        return position();
    }
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
    traversePreOrder([scene](SceneGraphNode* node) {
        node->_scene = scene;
    });
}

std::vector<SceneGraphNode*> SceneGraphNode::children() const {
    std::vector<SceneGraphNode*> nodes;
    for (auto& child : _children) {
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

const Renderable* SceneGraphNode::renderable() const
{
    return _renderable.get();
}

Renderable* SceneGraphNode::renderable() {
    return _renderable.get();
}

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

SceneGraphNode* SceneGraphNode::childNode(const std::string& name)
{
    if (this->name() == name)
        return this;
    else
        for (std::unique_ptr<SceneGraphNode>& it : _children) {
            SceneGraphNode* tmp = it->childNode(name);
            if (tmp)
                return tmp;
        }
    return nullptr;
}

void SceneGraphNode::updateCamera(Camera* camera) const{

    psc origin(worldPosition());
    //int i = 0;
    // the camera position
    
    psc relative = camera->position();
    psc focus = camera->focusPosition();
    psc relative_focus = relative - focus;

    psc target = origin + relative_focus;
    
    camera->setPosition(target);
    camera->setFocusPosition(origin);

    //printf("target: %f, %f, %f, %f\n", target.vec4().x, target.vec4().y, target.vec4().z, target.vec4().w);
    
}

}  // namespace openspace
