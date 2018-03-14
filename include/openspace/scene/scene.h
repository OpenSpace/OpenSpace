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

#ifndef __OPENSPACE_CORE___SCENE___H__
#define __OPENSPACE_CORE___SCENE___H__

#include <openspace/properties/propertyowner.h>

#include <vector>
#include <unordered_map>
#include <set>
#include <mutex>

#include <openspace/scene/sceneinitializer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicense.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/misc/easing.h>
#include <ghoul/opengl/programobject.h>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

// Notifications:
// SceneGraphFinishedLoading
class Scene
    : public properties::PropertyOwner
{
public:
    using UpdateDependencies = ghoul::Boolean;

    struct InvalidSceneError : ghoul::RuntimeError {
        /**
        * \param message The reason that caused this exception to be thrown
        * \param component The optional compoment that caused this exception to be thrown
        * \pre message may not be empty
        */
        explicit InvalidSceneError(const std::string& message,
            const std::string& component = "");
    };

    // constructors & destructor
    Scene(std::unique_ptr<SceneInitializer> initializer);
    ~Scene();

    /**
     * Clear the scene graph,
     * i.e. set the root node to nullptr and deallocate all scene graph nodes.
     */
    void clear();

    /**
     * Attach node to the root
     */
    void attachNode(std::unique_ptr<SceneGraphNode> node);

    /**
     * Detach node from the root
     */
    std::unique_ptr<SceneGraphNode> detachNode(SceneGraphNode& node);

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
    SceneGraphNode* root();

    /**
    * Return the root SceneGraphNode.
    */
    const SceneGraphNode* root() const;

    /**
     * Return the scenegraph node with the specified name or <code>nullptr</code> if that
     * name does not exist.
     */
    SceneGraphNode* sceneGraphNode(const std::string& name) const;

    /**
     * Add a node and all its children to the scene.
     */
    void registerNode(SceneGraphNode* node);

    /**
     * Remove a node and all its children from the scene.
     */
    void unregisterNode(SceneGraphNode* node);

    void addSceneLicense(SceneLicense license);

    /**
    * Mark the node registry as dirty
    */
    void markNodeRegistryDirty();

    /**
     * Return a vector of all scene graph nodes in the scene.
     */
    const std::vector<SceneGraphNode*>& allSceneGraphNodes() const;

    /**
     * Write information about the license information for the scenegraph nodes that are
     * contained in this scene
     * \param path The file path that will contain the documentation about the licenses
     * used in this scene
     */
    void writeSceneLicenseDocumentation(const std::string& path) const;

    /**
     * Return a a map from name to scene graph node.
     */
    const std::unordered_map<std::string, SceneGraphNode*>& nodesByName() const;

    /**
     * Load a scene graph node from a dictionary and return it.
     */
    SceneGraphNode* loadNode(const ghoul::Dictionary& nodeDictionary);

    /**
     * Initialize a scene graph node.
     */
    void initializeNode(SceneGraphNode* node);

    /**
     * Return true if the scene is initializing
     */
    bool isInitializing() const;

    /**
     * Adds an interpolation request for the passed \p prop that will run for
     * \p durationSeconds seconds. Every time the #updateInterpolations method is called
     * the Property will be notified that it has to update itself using the stored
     * interpolation values. If an interpolation record already exists for the passed 
     * \p prop, the previous record will be overwritten and the remaining time of the old
     * interpolation is ignored.
     *
     * \param prop The property that should be called to update itself every frame until
     *        \p durationSeconds seconds have passed
     * \param durationSeconds The number of seconds that the interpolation will run for
     *
     * \pre \p prop must not be \c nullptr
     * \pre \p durationSeconds must be positive and not 0
     * \post A new interpolation record exists for \p that is not expired 
     */
    void addInterpolation(properties::Property* prop, float durationSeconds,
        ghoul::EasingFunction easingFunction = ghoul::EasingFunction::Linear);

    /**
     * Removes the passed \p prop from the list of Property%s that are update each time
     * the #updateInterpolations method is called
     *
     * \param prop The Property that should not longer be updated
     *
     * \pre \prop must not be nullptr
     * \post No interpolation record exists for \p prop
     */
    void removeInterpolation(properties::Property* prop);

    /**
     * Informs all Property%s with active interpolations about applying a new update tick
     * using the Property::interpolateValue method, passing a parameter \c t which is \c 0
     * if no time has passed between the #addInterpolation method and \c 1 if an amount of
     * time equal to the requested interpolation time has passed. The parameter \c t is
     * updated with a resolution of 1 microsecond, which means that if this function is
     * called twice within 1 microsecond, the passed parameter \c t might be the same for
     * both calls
     */
    void updateInterpolations();

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * scene graph. The functions contained are
     * - openspace::luascriptfunctions::property_setValue
     * - openspace::luascriptfunctions::property_getValue
     * \return The Lua library that contains all Lua functions available to change the
     * scene graph
     */
    static scripting::LuaLibrary luaLibrary();

private:
    /**
     * Update dependencies.
     */
    void updateNodeRegistry();

    void sortTopologically();

    std::unique_ptr<Camera> _camera;
    std::vector<SceneGraphNode*> _topologicallySortedNodes;
    std::vector<SceneGraphNode*> _circularNodes;
    std::unordered_map<std::string, SceneGraphNode*> _nodesByName;
    bool _dirtyNodeRegistry;
    SceneGraphNode _rootDummy;
    std::unique_ptr<SceneInitializer> _initializer;

    std::vector<SceneLicense> _licenses;

    std::mutex _programUpdateLock;
    std::set<ghoul::opengl::ProgramObject*> _programsToUpdate;
    std::vector<std::unique_ptr<ghoul::opengl::ProgramObject>> _programs;

    struct InterpolationInfo {
        properties::Property* prop;
        std::chrono::time_point<std::chrono::steady_clock> beginTime;
        float durationSeconds;
        ghoul::EasingFunc<float> easingFunction;
        bool isExpired = false;
    };
    std::vector<InterpolationInfo> _interpolationInfos;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENE___H__
