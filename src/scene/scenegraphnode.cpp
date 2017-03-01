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

#include <openspace/query/query.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/shadermanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/shaderobject.h>

#include <modules/base/translation/statictranslation.h>
#include <modules/base/rotation/staticrotation.h>
#include <modules/base/scale/staticscale.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/factorymanager.h>

#include <cctype>
#include <chrono>

#include "scenegraphnode_doc.inl"

namespace {
    const std::string _loggerCat = "SceneGraphNode";
    const std::string KeyRenderable = "Renderable";

    const std::string keyTransformTranslation = "Transform.Translation";
    const std::string keyTransformRotation = "Transform.Rotation";
    const std::string keyTransformScale = "Transform.Scale";
}

namespace openspace {

// Constants used outside of this file
const std::string SceneGraphNode::RootNodeName = "Root";
const std::string SceneGraphNode::KeyName = "Name";
const std::string SceneGraphNode::KeyParentName = "Parent";
const std::string SceneGraphNode::KeyDependencies = "Dependencies";

SceneGraphNode* SceneGraphNode::createFromDictionary(const ghoul::Dictionary& dictionary){
    openspace::documentation::testSpecificationAndThrow(
        SceneGraphNode::Documentation(),
        dictionary,
        "SceneGraphNode"
    );

    SceneGraphNode* result = new SceneGraphNode;

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
            delete result;
            return nullptr;
        }
        result->addPropertySubOwner(result->_renderable);
        LDEBUG("Successfully created renderable for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformTranslation)) {
        ghoul::Dictionary translationDictionary;
        dictionary.getValue(keyTransformTranslation, translationDictionary);
        result->_transform.translation = 
            std::unique_ptr<Translation>(Translation::createFromDictionary(translationDictionary));
        if (result->_transform.translation == nullptr) {
            LERROR("Failed to create ephemeris for SceneGraphNode '"
                << result->name() << "'");
            delete result;
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.translation.get());
        LDEBUG("Successfully created ephemeris for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformRotation)) {
        ghoul::Dictionary rotationDictionary;
        dictionary.getValue(keyTransformRotation, rotationDictionary);
        result->_transform.rotation =
            std::unique_ptr<Rotation>(Rotation::createFromDictionary(rotationDictionary));
        if (result->_transform.rotation == nullptr) {
            LERROR("Failed to create rotation for SceneGraphNode '"
                << result->name() << "'");
            delete result;
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.rotation.get());
        LDEBUG("Successfully created rotation for '" << result->name() << "'");
    }

    if (dictionary.hasKey(keyTransformScale)) {
        ghoul::Dictionary scaleDictionary;
        dictionary.getValue(keyTransformScale, scaleDictionary);
        result->_transform.scale =
            std::unique_ptr<Scale>(Scale::createFromDictionary(scaleDictionary));
        if (result->_transform.scale == nullptr) {
            LERROR("Failed to create scale for SceneGraphNode '"
                << result->name() << "'");
            delete result;
            return nullptr;
        }
        result->addPropertySubOwner(result->_transform.scale.get());
        LDEBUG("Successfully created scale for '" << result->name() << "'");
    }

    LDEBUG("Successfully created SceneGraphNode '"
                   << result->name() << "'");
    return result;
}

SceneGraphNode::SceneGraphNode()
    : _parent(nullptr)
    , _transform {
        std::make_unique<StaticTranslation>(),
        std::make_unique<StaticRotation>(),
        std::make_unique<StaticScale>()
    }
    , _performanceRecord({0, 0, 0})
    , _renderable(nullptr)
    , _renderableVisible(false)
    , _boundingSphereVisible(false)
{
}

SceneGraphNode::~SceneGraphNode() {
    deinitialize();
}

bool SceneGraphNode::initialize() {
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

    return true;
}

bool SceneGraphNode::deinitialize() {
    LDEBUG("Deinitialize: " << name());

    if (_renderable) {
        _renderable->deinitialize();
        delete _renderable;
        _renderable = nullptr;
    }
    _children.clear();

    // reset variables
    _parent = nullptr;
    _renderableVisible = false;
    _boundingSphereVisible = false;
    _boundingSphere = PowerScaledScalar(0.0, 0.0);

    return true;
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
    newUpdateData.modelTransform .scale = worldScale();

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

void SceneGraphNode::evaluate(const Camera* camera, const psc& parentPosition) {
    //const psc thisPosition = parentPosition + _ephemeris->position();
    //const psc camPos = camera->position();
    //const psc toCamera = thisPosition - camPos;

    // init as not visible
    //_boundingSphereVisible = false;
    _renderableVisible = false;

#ifndef OPENSPACE_VIDEO_EXPORT
    // check if camera is outside the node boundingsphere
  /*  if (toCamera.length() > _boundingSphere) {
        // check if the boudningsphere is visible before avaluating children
        if (!sphereInsideFrustum(thisPosition, _boundingSphere, camera)) {
            // the node is completely outside of the camera view, stop evaluating this
            // node
            //LFATAL(_nodeName << " is outside of frustum");
            return;
        }
    }
    */
#endif

    // inside boudningsphere or parts of the sphere is visible, individual
    // children needs to be evaluated
    _boundingSphereVisible = true;

    // this node has an renderable
    if (_renderable) {
        //  check if the renderable boundingsphere is visible
        // _renderableVisible = sphereInsideFrustum(
        //       thisPosition, _renderable->getBoundingSphere(), camera);
        _renderableVisible = true;
    }

    // evaluate all the children, tail-recursive function(?)
    //for (SceneGraphNode* child : _children)
    //    child->evaluate(camera, psc());
}

void SceneGraphNode::render(const RenderData& data, RendererTasks& tasks) {
    const psc thisPositionPSC = psc::CreatePowerScaledCoordinate(_worldPositionCached.x, _worldPositionCached.y, _worldPositionCached.z);

    RenderData newData = {
        data.camera,
        thisPositionPSC,
        data.doPerformanceMeasurement,
        data.renderBinMask,
        _worldPositionCached,
        _worldRotationCached,
        _worldScaleCached};

    //_performanceRecord.renderTime = 0;

    bool visible = _renderableVisible &&
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


// not used anymore @AA
//void SceneGraphNode::addNode(SceneGraphNode* child)
//{
//    // add a child node and set this node to be the parent
//    child->setParent(this);
//    _children.push_back(child);
//}

void SceneGraphNode::setParent(SceneGraphNode* parent) {
    _parent = parent;
}

void SceneGraphNode::addChild(SceneGraphNode* child) {
    _children.push_back(child);
}


//not used anymore @AA
//bool SceneGraphNode::abandonChild(SceneGraphNode* child) {
//    std::vector < SceneGraphNode* >::iterator it = std::find(_children.begin(), _children.end(), child);
//
//    if (it != _children.end()){
//        _children.erase(it);
//        return true;
//    }
//
//    return false;
//}

glm::dvec3 SceneGraphNode::position() const
{
    return _transform.translation->position();
}

const glm::dmat3& SceneGraphNode::rotationMatrix() const
{
    return _transform.rotation->matrix();
}

double SceneGraphNode::scale() const
{
    return _transform.scale->scaleValue();
}

glm::dvec3 SceneGraphNode::worldPosition() const
{
    return _worldPositionCached;
}

const glm::dmat3& SceneGraphNode::worldRotationMatrix() const
{
    return _worldRotationCached;
}

double SceneGraphNode::worldScale() const
{
    return _worldScaleCached;
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

SceneGraphNode* SceneGraphNode::parent() const
{
    return _parent;
}
const std::vector<SceneGraphNode*>& SceneGraphNode::children() const{
    return _children;
}

// bounding sphere
PowerScaledScalar SceneGraphNode::calculateBoundingSphere(){
    // set the bounding sphere to 0.0
    _boundingSphere = 0.0;
    /*
    This is not how to calculate a bounding sphere, better to leave it at 0 if not a
    renderable. --KB
    if (!_children.empty()) {  // node
        PowerScaledScalar maxChild;

        // loop though all children and find the one furthest away/with the largest
        // bounding sphere
        for (size_t i = 0; i < _children.size(); ++i) {
            // when positions is dynamic, change this part to fins the most distant
            // position
            //PowerScaledScalar child = _children.at(i)->position().length()
            //            + _children.at(i)->calculateBoundingSphere();
            PowerScaledScalar child = _children.at(i)->calculateBoundingSphere();
            if (child > maxChild) {
                maxChild = child;
            }
        }
        _boundingSphere += maxChild;
    } 
    */
    // if has a renderable, use that boundingsphere
    if (_renderable ) {
        PowerScaledScalar renderableBS = _renderable->getBoundingSphere();
        if(renderableBS > _boundingSphere)
            _boundingSphere = renderableBS;
    }
    //LINFO("Bounding Sphere of '" << name() << "': " << _boundingSphere);
    
    return _boundingSphere;
}

PowerScaledScalar SceneGraphNode::boundingSphere() const{
    return _boundingSphere;
}

// renderable
void SceneGraphNode::setRenderable(Renderable* renderable) {
    _renderable = renderable;
}

const Renderable* SceneGraphNode::renderable() const
{
    return _renderable;
}

Renderable* SceneGraphNode::renderable() {
    return _renderable;
}

// private helper methods
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

SceneGraphNode* SceneGraphNode::childNode(const std::string& name)
{
    if (this->name() == name)
        return this;
    else
        for (SceneGraphNode* it : _children) {
            SceneGraphNode* tmp = it->childNode(name);
            if (tmp != nullptr)
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
