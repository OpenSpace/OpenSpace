/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/documentation/documentation.h>

#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/dictionary.h>

namespace openspace {

class SceneGraphNode;

// Notifications:
// SceneGraphFinishedLoading
class Scene {
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
    ~Scene();

    /**
     * Initalizes the SceneGraph
     */
    void initialize();

    /*
     * Load the scenegraph from the provided folder
     */
    //void scheduleLoadSceneFile(const std::string& sceneDescriptionFilePath);
    void clear();

    /*
     * Set the root node of the scene
     */
    void setRoot(std::unique_ptr<SceneGraphNode> root);

    /*
     * Set the root node of the scene
     */
    void setCamera(std::unique_ptr<Camera> camera);

    /**
     * Return the camera
     */
    Camera* camera() const;
   
    /*
     * Updates all SceneGraphNodes relative positions
     */
    void update(const UpdateData& data);

    /*
     * Evaluates if the SceneGraphNodes are visible to the provided camera
     */
    void evaluate(Camera* camera);

    /*
     * Render visible SceneGraphNodes using the provided camera
     */
    void render(const RenderData& data, RendererTasks& tasks);

    /*
     * Returns the root SceneGraphNode
     */
    SceneGraphNode* root() const;

    /**
     * Return the scenegraph node with the specified name or <code>nullptr</code> if that
     * name does not exist
     */
    SceneGraphNode* sceneGraphNode(const std::string& name) const;

    void addNode(SceneGraphNode* node, UpdateDependencies updateDeps = UpdateDependencies::Yes);

    void removeNode(SceneGraphNode* node, UpdateDependencies updateDeps = UpdateDependencies::Yes);

    void updateDependencies();

    void sortTopologically();

    const std::vector<SceneGraphNode*>& allSceneGraphNodes() const;

    const std::map<std::string, SceneGraphNode*>& nodesByName() const;

    void writePropertyDocumentation(const std::string& filename, const std::string& type, const std::string& sceneFilename);

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
