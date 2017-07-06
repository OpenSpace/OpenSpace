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
    void loadScene(Scene* scene, const std::string& path);

private:
    
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
     * Load a directory.
     */
    void loadDirectory(const std::string& path);

    /**
     * Load a camera from a dictionary
     */
    std::unique_ptr<SceneLoader::LoadedCamera> loadCamera(const ghoul::Dictionary& dictionary);

    AssetLoader* _assetLoader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENELOADER___H__
