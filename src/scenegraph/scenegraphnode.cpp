/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// open space includes
#include <openspace/scenegraph/scenegraphnode.h>
#include <openspace/util/spice.h>
#include <openspace/query/query.h>
#include <openspace/util/constants.h>

// ghoul includes
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/shadermanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/shaderobject.h>

#include <openspace/scenegraph/staticephemeris.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/factorymanager.h>

namespace {
const std::string _loggerCat = "SceneGraphNode";
}

namespace openspace {
    
std::string SceneGraphNode::RootNodeName = "Root";

SceneGraphNode* SceneGraphNode::createFromDictionary(const ghoul::Dictionary& dictionary)
{
    using namespace constants::scenegraph;
    using namespace constants::scenegraphnode;

    SceneGraphNode* result = new SceneGraphNode;

    std::string path;
    dictionary.getValue(keyPathModule, path);

    if (!dictionary.hasValue<std::string>(keyName)) {
        LERROR("SceneGraphNode in '" << path << "' did not contain a '"
                                     << keyName << "' key");
        return nullptr;
    }
    dictionary.getValue(keyName, result->_nodeName);

    if (dictionary.hasValue<ghoul::Dictionary>(keyRenderable)) {
        ghoul::Dictionary renderableDictionary;
        dictionary.getValue(keyRenderable, renderableDictionary);
        renderableDictionary.setValue(keyPathModule, path);
        renderableDictionary.setValue(keyName, result->_nodeName);

        result->_renderable = Renderable::createFromDictionary(renderableDictionary);
        if (result->_renderable == nullptr) {
            LERROR("Failed to create renderable for SceneGraphNode '"
                   << result->_nodeName << "'");
            return nullptr;
        }
        LDEBUG("Successfully create renderable for '" << result->_nodeName << "'");
    }
    if (dictionary.hasKey(keyEphemeris)) {
        ghoul::Dictionary ephemerisDictionary;
        dictionary.getValue(keyEphemeris, ephemerisDictionary);
        ephemerisDictionary.setValue(keyPathModule, path);
        result->_ephemeris = Ephemeris::createFromDictionary(ephemerisDictionary);
        if (result->_ephemeris == nullptr) {
            LERROR("Failed to create ephemeris for SceneGraphNode '"
                   << result->_nodeName << "'");
            return nullptr;
        }
        LDEBUG("Successfully create ephemeris for '" << result->_nodeName << "'");
    }

    std::string parentName;
    if (!dictionary.getValue(constants::scenegraphnode::keyParentName, parentName)) {
        LWARNING("Could not find 'Parent' key, using 'Root'.");
        parentName = "Root";
    }

    SceneGraphNode* parentNode = getSceneGraphNode(parentName);
    if (parentNode == nullptr) {
        LFATAL("Could not find parent named '"
               << parentName << "' for '" << result->_nodeName << "'."
               << " Check module definition order. Skipping module.");
    }

    parentNode->addNode(result);

    LDEBUG("Successfully created SceneGraphNode '"
                   << result->_nodeName << "'");
    return result;
}

SceneGraphNode::SceneGraphNode()
    : _parent(nullptr)
    , _nodeName("")
    , _ephemeris(new StaticEphemeris)
    , _renderable(nullptr)
    , _renderableVisible(false)
    , _boundingSphereVisible(false)
{
}

SceneGraphNode::~SceneGraphNode()
{
    deinitialize();
}

bool SceneGraphNode::initialize()
{
    if (_renderable != nullptr)
        _renderable->initialize();

    if (_ephemeris != nullptr)
        _ephemeris->initialize();
    return true;
}

bool SceneGraphNode::deinitialize()
{
    LDEBUG("Deinitialize: " << _nodeName);

    delete _renderable;
    _renderable = nullptr;
    delete _ephemeris;
    _ephemeris = nullptr;

    // deallocate the child nodes and delete them, iterate c++11 style
    for (SceneGraphNode* child : _children)
        delete child;
    _children.clear();

    // reset variables
    _parent = nullptr;
    _nodeName = "";
    _renderableVisible = false;
    _boundingSphereVisible = false;
    _boundingSphere = PowerScaledScalar(0.0, 0.0);

    return true;
}

// essential
void SceneGraphNode::update()
{
    _ephemeris->update();
}

void SceneGraphNode::evaluate(const Camera* camera, const psc& parentPosition)
{
    const psc thisPosition = parentPosition + _ephemeris->position();
    const psc camPos = camera->position();
    const psc toCamera = thisPosition - camPos;

    // init as not visible
    _boundingSphereVisible = false;
    _renderableVisible = false;

    // check if camera is outside the node boundingsphere
    if (toCamera.length() > _boundingSphere) {
        // check if the boudningsphere is visible before avaluating children
        if (!sphereInsideFrustum(thisPosition, _boundingSphere, camera)) {
            // the node is completely outside of the camera view, stop evaluating this
            // node
            return;
        }
    }

    // inside boudningsphere or parts of the sphere is visible, individual
    // children needs to be evaluated
    _boundingSphereVisible = true;

    // this node has an renderable
    if (_renderable) {
        //  check if the renderable boundingsphere is visible
        _renderableVisible = sphereInsideFrustum(
              thisPosition, _renderable->getBoundingSphere(), camera);
    }

    // evaluate all the children, tail-recursive function(?)
    for (auto& child : _children) {
        child->evaluate(camera, psc());
    }
}

void SceneGraphNode::render(const Camera* camera, const psc& parentPosition)
{
    const psc thisPosition = parentPosition + _ephemeris->position();

    // check if camera is outside the node boundingsphere
    if (!_boundingSphereVisible) {
        return;
    }
    if (_renderableVisible) {
        // LDEBUG("Render");
        _renderable->render(camera, thisPosition);
    }

    // evaluate all the children, tail-recursive function(?)

    for (auto& child : _children) {
        child->render(camera, thisPosition);
    }
}

// set & get
void SceneGraphNode::addNode(SceneGraphNode* child)
{
    // add a child node and set this node to be the parent
    child->setParent(this);
    _children.push_back(child);
}

void SceneGraphNode::setName(const std::string& name)
{
    _nodeName = name;
}

void SceneGraphNode::setParent(SceneGraphNode* parent)
{
    _parent = parent;
}

const psc& SceneGraphNode::getPosition() const
{
    return _ephemeris->position();
}

psc SceneGraphNode::getWorldPosition() const
{
    // recursive up the hierarchy if there are parents available
    if (_parent) {
        return _ephemeris->position() + _parent->getWorldPosition();
    } else {
        return _ephemeris->position();
    }
}

std::string SceneGraphNode::nodeName() const
{
    return _nodeName;
}

SceneGraphNode* SceneGraphNode::parent() const
{
    return _parent;
}
const std::vector<SceneGraphNode*>& SceneGraphNode::children() const
{
    return _children;
}

// bounding sphere
PowerScaledScalar SceneGraphNode::calculateBoundingSphere()
{
    // set the bounding sphere to 0.0
    _boundingSphere = 0.0;

    if (_children.size() > 0) {  // node
        PowerScaledScalar maxChild;

        // loop though all children and find the one furthest away/with the largest
        // bounding sphere
        for (size_t i = 0; i < _children.size(); ++i) {
            // when positions is dynamic, change this part to fins the most distant
            // position
            PowerScaledScalar child = _children.at(i)->getPosition().length()
                        + _children.at(i)->calculateBoundingSphere();
            if (child > maxChild) {
                maxChild = child;
            }
        }
        _boundingSphere += maxChild;
    } else {  // leaf

        // if has a renderable, use that boundingsphere
        if (_renderable)
            _boundingSphere += _renderable->getBoundingSphere();
    }

    return _boundingSphere;
}

// renderable
void SceneGraphNode::setRenderable(Renderable* renderable)
{
    _renderable = renderable;
    update();
}

const Renderable* SceneGraphNode::getRenderable() const
{
    return _renderable;
}

// private helper methods
bool SceneGraphNode::sphereInsideFrustum(const psc s_pos, const PowerScaledScalar& s_rad,
                                         const Camera* camera)
{
    // direction the camera is looking at in power scale
    psc psc_camdir = psc(camera->viewDirection());

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

SceneGraphNode* SceneGraphNode::get(const std::string& name)
{
    if (_nodeName == name)
        return this;
    else
        for (auto it : _children) {
            SceneGraphNode* tmp = it->get(name);
            if (tmp != nullptr) {
                return tmp;
            }
        }
    return nullptr;
}

void SceneGraphNode::print() const
{
    std::cout << _nodeName << std::endl;
    for (auto it : _children) {
        it->print();
    }
}

}  // namespace openspace