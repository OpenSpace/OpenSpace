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

#ifndef __OPENSPACE_CORE___SCENELOADER___H__
#define __OPENSPACE_CORE___SCENELOADER___H__

#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/assetloader.h>
#include <openspace/util/camera.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

#include <memory>
#include <string>

namespace openspace {

class Scene;

class SceneLoader {
public:
    SceneLoader(AssetLoader* assetLoader);
    ~SceneLoader() = default;

    /**
     * Load a scene file.
     */
    std::unique_ptr<Scene> loadScene(const std::string& path);

    /**
     * Import a directory of scene contents into an existing scene.
     */
    std::vector<SceneGraphNode*> importDirectory(Scene& scene, const std::string& directory);

    /**
     * Import a scene graph node from a dictionary into an existing scene.
     */
    SceneGraphNode* importNodeDictionary(Scene& scene, const ghoul::Dictionary& dictionary);

private:
    struct LoadedNode {
        LoadedNode(
            const std::string& nodeName,
            const std::string& parentName,
            const std::vector<std::string>& deps,
            std::unique_ptr<SceneGraphNode> n
            )
            : name(nodeName)
            , parent(parentName)
            , dependencies(deps)
            , node(std::move(n)) {}

        std::string name;
        std::string parent;
        std::vector<std::string> dependencies;
        std::unique_ptr<SceneGraphNode> node;
    };

    struct LoadedCamera {
        LoadedCamera(
            const std::string& parentName,
            std::unique_ptr<Camera> c
            )
            : parent(parentName)
            , camera(std::move(c)) {}
        std::string parent;
        std::unique_ptr<Camera> camera;
    };

    /**
     * Load a scene graph node from a dictionary
     */
    SceneLoader::LoadedNode loadNode(const ghoul::Dictionary& dictionary);

    /**
     * Load a mod file.
     */
    std::vector<SceneLoader::LoadedNode> loadAsset(const std::string& path, lua_State* luaState);

    /**
     * Load a directory.
     */
    std::vector<SceneLoader::LoadedNode> loadDirectory(const std::string& path, lua_State* luaState);

    /**
     * Load a camera from a dictionary
     */
    SceneLoader::LoadedCamera loadCamera(const ghoul::Dictionary& dictionary);

    /**
     * Add loaded nodes to an existing scene
     */
    std::vector<SceneGraphNode*> addLoadedNodes(Scene& scene, std::vector<SceneLoader::LoadedNode>&& nodes);

    AssetLoader* _assetLoader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENELOADER___H__
