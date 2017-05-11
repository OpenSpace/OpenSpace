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
 
#ifndef __OPENSPACE_CORE___SCENE___H__
#define __OPENSPACE_CORE___SCENE___H__

#include <vector>
#include <map>
#include <set>
#include <mutex>

#include <openspace/util/camera.h>
#include <openspace/util/documented.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/opengl/programobject.h>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

class SceneGraphNode;

// Notifications:
// SceneGraphFinishedLoading
class Scene : public Documented {
public:

    using UpdateDependencies = ghoul::Boolean;

    struct InvalidSceneError : ghoul::RuntimeError {
        /**
        * \param message The reason that caused this exception to be thrown
        * \param component The optional compoment that caused this exception to be thrown
        * \pre message may not be empty
        */
        explicit InvalidSceneError(const std::string& message, const std::string& component = "");
    };

    // constructors & destructor
    Scene();

    /**
     * Initalizes the SceneGraph
     */
    void initialize();

    /**
     * Clear the scene graph,
     * i.e. set the root node to nullptr and deallocate all scene graph nodes.
     */
    void clear();

    /**
     * Set the root node of the scene
     */
    void setRoot(std::unique_ptr<SceneGraphNode> root);

    /**
     * Set the camera of the scene
     */
    void setCamera(std::unique_ptr<Camera> camera);

    /**
     * Return the camera
     */
    Camera* camera() const;

    /**
     * Updates all SceneGraphNodes relative positions
     */
    void update(const UpdateData& data);

    /**
     * Render visible SceneGraphNodes using the provided camera.
     */
    void render(const RenderData& data, RendererTasks& tasks);

    /**
     * Return the root SceneGraphNode.
     */
    SceneGraphNode* root() const;

    /**
     * Return the scenegraph node with the specified name or <code>nullptr</code> if that
     * name does not exist.
     */
    SceneGraphNode* sceneGraphNode(const std::string& name) const;

    /**
     * Add a node and all its children to the scene.
     */
    void addNode(SceneGraphNode* node, UpdateDependencies updateDeps = UpdateDependencies::Yes);

    /**
     * Remove a node and all its children from the scene.
     */
    void removeNode(SceneGraphNode* node, UpdateDependencies updateDeps = UpdateDependencies::Yes);

    /**
     * Update dependencies.
     */
    void updateDependencies();

    /**
     * Return a vector of all scene graph nodes in the scene.
     */
    const std::vector<SceneGraphNode*>& allSceneGraphNodes() const;

    /**
     * Return a a map from name to scene graph node.
     */
    const std::map<std::string, SceneGraphNode*>& nodesByName() const;

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * scene graph. The functions contained are
     * - openspace::luascriptfunctions::property_setValue
     * - openspace::luascriptfunctions::property_getValue
     * \return The Lua library that contains all Lua functions available to change the
     * scene graph
     */
    static scripting::LuaLibrary luaLibrary();

    static documentation::Documentation Documentation();

private:
    std::string generateJson() const override;

    void sortTopologically();

    std::unique_ptr<SceneGraphNode> _root;
    std::unique_ptr<Camera> _camera;
    std::vector<SceneGraphNode*> _topologicallySortedNodes;
    std::vector<SceneGraphNode*> _circularNodes;
    std::map<std::string, SceneGraphNode*> _nodesByName;

    std::mutex _programUpdateLock;
    std::set<ghoul::opengl::ProgramObject*> _programsToUpdate;
    std::vector<std::unique_ptr<ghoul::opengl::ProgramObject>> _programs;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENE___H__
