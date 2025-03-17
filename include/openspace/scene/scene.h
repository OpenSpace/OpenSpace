/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/easing.h>
#include <ghoul/misc/memorypool.h>
#include <mutex>
#include <set>
#include <unordered_map>
#include <vector>

namespace ghoul {

class Dictionary;
namespace lua { class LuaState; }
namespace opengl { class ProgramObject; }

} // namespace ghoul

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }

enum class PropertyValueType {
    Boolean = 0,
    Float,
    String,
    Table,
    Nil
};

class Profile;
class SceneInitializer;

// Notifications:
// SceneGraphFinishedLoading
class Scene : public properties::PropertyOwner {
public:
    BooleanType(UpdateDependencies);

    /**
     * This struct describes a time that has some intrinsic interesting-ness to this
     * scene.
     */
    struct InterestingTime {
        std::string name;
        std::string time;
    };

    explicit Scene(std::unique_ptr<SceneInitializer> initializer);
    virtual ~Scene() override;

    /**
     * Attach node to the root.
     */
    void attachNode(ghoul::mm_unique_ptr<SceneGraphNode> node);

    /**
     * Detach node from the root.
     */
    ghoul::mm_unique_ptr<SceneGraphNode> detachNode(SceneGraphNode& node);

    /**
     * Return the camera.
     */
    Camera* camera() const;

    /**
     * Updates all SceneGraphNodes relative positions.
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
     * Return the scenegraph node with the specified name or `nullptr` if that name does
     * not exist.
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

    /**
     * Mark the node registry as dirty.
     */
    void markNodeRegistryDirty();

    /**
     * Return a vector of all scene graph nodes in the scene.
     */
    const std::vector<SceneGraphNode*>& allSceneGraphNodes() const;

    /**
     * Returns a map from identifier to scene graph node.
     */
    const std::unordered_map<std::string, SceneGraphNode*>& nodesByIdentifier() const;

    /**
     * Load a scene graph node from a dictionary and return it.
     */
    SceneGraphNode* loadNode(const ghoul::Dictionary& nodeDictionary);

    /**
     * Initialize a scene graph node.
     */
    void initializeNode(SceneGraphNode* node);

    /**
     * Return true if the scene is initializing.
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
     * \param postScript A Lua script that will be executed when the interpolation
     *        finishes
     * \param easingFunction A function that determines who the interpolation occurs
     *
     * \pre \p prop must not be `nullptr`
     * \pre \p durationSeconds must be positive and not 0
     * \post A new interpolation record exists for \p that is not expired
     */
    void addPropertyInterpolation(properties::Property* prop, float durationSeconds,
        std::string postScript = "",
        ghoul::EasingFunction easingFunction = ghoul::EasingFunction::Linear);

    /**
     * Removes the passed \p prop from the list of Property%s that are update each time
     * the #updateInterpolations method is called.
     *
     * \param prop The Property that should not longer be updated
     *
     * \pre \p prop must not be nullptr
     * \post No interpolation record exists for \p prop
     */
    void removePropertyInterpolation(properties::Property* prop);

    /**
     * Informs all Property%s with active interpolations about applying a new update tick
     * using the Property::interpolateValue method, passing a parameter `t` which is `0`
     * if no time has passed between the #addPropertyInterpolation method and `1` if an
     * amount of time equal to the requested interpolation time has passed. The parameter
     * `t` is updated with a resolution of 1 microsecond, which means that if this
     * function is called twice within 1 microsecond, the passed parameter `t` might be
     * the same for both calls.
     */
    void updateInterpolations();

    /**
     * Returns the Lua library that contains all Lua functions available to change the
     * scene graph.
     *
     * \return The Lua library that contains all Lua functions available to change the
     *         scene graph
     */
    static scripting::LuaLibrary luaLibrary();

    /**
     * Sets a property using the 'properties' contents of a profile. The function will
     * loop through each setProperty command. A property may be set to a bool, float, or
     * string value (which must be converted because a Profile stores all values as
     * strings).
     *
     * \param p The Profile to be read.
     */
    void setPropertiesFromProfile(const Profile& p);

    /**
     * Searches for any properties that match the regex propertyString, and returns
     * the results in a vector.
     *
     * \param propertyString The regex string that is intended to match one or more
     *        properties in the currently-available properties
     * \return Vector of Property objs containing property names that matched the regex
     */
    std::vector<properties::Property*> propertiesMatchingRegex(
        std::string_view propertyString);

    /**
     * Returns a list of all unique tags that are used in the currently loaded scene.
     *
     * \return A list of all unique tags that are used in the currently loaded scene.
     */
    std::vector<std::string> allTags() const;

    /**
     * Set a custom order for items in a given branch in the Scene GUI tree.
     *
     * \param guiPath The GUI path for which to set the order
     * \param list A list of names of scene graph nodes or subgroups in the GUI, in the
     *             order of which they should appear in the tree.
     */
    void setGuiTreeOrder(const std::string& guiPath,
        const std::vector<std::string>& list);

    /**
     * Returns a dictionary containing all the currently set custom orderings for the
     * Scene GUI tree.
     *
     * \return A dictionary containing key value pairs with custom item orderings for
     *         specific paths in the Scene GUI tree
     */
    ghoul::Dictionary guiTreeOrder() const;

private:
    /**
     * Accepts string version of a property value from a profile, converts it to the
     * appropriate type, and then pushes the value onto the Lua state.
     *
     * \param L The Lua state to push value to
     * \param value String representation of the value with which to set property
     */
    void propertyPushProfileValueToLua(ghoul::lua::LuaState& L, const std::string& value);


    /**
     * Update dependencies.
     */
    void updateNodeRegistry();
    void sortTopologically();

    std::unique_ptr<Camera> _camera;
    std::vector<SceneGraphNode*> _topologicallySortedNodes;
    std::vector<SceneGraphNode*> _circularNodes;
    std::unordered_map<std::string, SceneGraphNode*> _nodesByIdentifier;
    bool _dirtyNodeRegistry = false;
    SceneGraphNode _rootNode;
    std::unique_ptr<SceneInitializer> _initializer;
    std::string _profilePropertyName;
    bool _valueIsTable = false;

    std::mutex _programUpdateLock;
    std::set<ghoul::opengl::ProgramObject*> _programsToUpdate;
    std::vector<std::unique_ptr<ghoul::opengl::ProgramObject>> _programs;

    struct PropertyInterpolationInfo {
        properties::Property* prop;
        std::chrono::time_point<std::chrono::steady_clock> beginTime;
        float durationSeconds;
        std::string postScript;

        ghoul::EasingFunc<float> easingFunction;
        bool isExpired = false;
    };
    std::vector<PropertyInterpolationInfo> _propertyInterpolationInfos;

    std::unordered_map<std::string, std::vector<std::string>> _guiTreeOrderMap;
};

// Convert the input string to a format that is valid as an identifier
std::string makeIdentifier(std::string str);

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENE___H__
