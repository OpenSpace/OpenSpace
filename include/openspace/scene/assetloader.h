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

#ifndef __OPENSPACE_CORE___ASSETLOADER___H__
#define __OPENSPACE_CORE___ASSETLOADER___H__

#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/filesystem/directory.h>

#include <memory>
#include <string>

namespace openspace {

namespace assetloader {
int importAsset(lua_State* state);
int resolveLocalResource(lua_State* state);
int resolveSyncedResource(lua_State* state);
}

class AssetLoader {
public:

    class Asset {
    public:
        Asset(AssetLoader* loader, std::string directory);
        Asset(AssetLoader* loader, std::string directory, std::string name);
        std::string id();
        std::string assetFilepath();
        std::string directory();
        ghoul::Dictionary dictionary();
        AssetLoader* loader();

        void addDependency(Asset* asset);
        void addDependant(Asset* asset);

    private:
        std::string resolveLocalResource(std::string resourceName);
        std::string resolveSyncedResource(std::string resourceName);

        // lua methods
        friend int assetloader::resolveLocalResource(lua_State* state);
        int resolveLocalResourceLua();

        friend int assetloader::resolveSyncedResource(lua_State* state);
        int resolveSyncedResourceLua();

        AssetLoader* _loader;
        std::string _directory;
        std::vector<Asset*> _dependencies;
        std::vector<Asset*> _dependants;
    };

    AssetLoader(ghoul::lua::LuaState* _luaState, std::string assetRoot, std::string syncRoot);
    ~AssetLoader() = default;


    /**
     * Load an asset file.
     */
    Asset* loadAsset(const std::string& identifier);
    ghoul::lua::LuaState* luaState();
    ghoul::filesystem::Directory currentDirectory();
    Asset* rootAsset();
    const std::string& syncRoot();

private:
    void pushAsset(Asset* asset);
    void popAsset();
    void updateLuaGlobals();
   
    std::unique_ptr<Asset> _rootAsset;
    std::map<std::string, std::unique_ptr<Asset>> _loadedAssets;
    std::vector<Asset*> _assetStack;

    std::string _syncRoot;

    friend int assetloader::importAsset(lua_State* state);
    int importAssetLua();

    ghoul::lua::LuaState* _luaState;
};




} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETLOADER___H__
