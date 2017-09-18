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

#include <openspace/scripting/lualibrary.h>
#include <openspace/util/resourcesynchronizer.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/luastate.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/filesystem/directory.h>

#include <memory>
#include <string>

namespace openspace {

class Asset;

namespace assetloader {
int importDependency(lua_State* state);
int importOptional(lua_State* state);
int resolveLocalResource(lua_State* state);
int resolveSyncedResource(lua_State* state);
int onFinishSynchronization(lua_State* state);
} // namespace assetloader

class AssetLoader {
public:
    /**
     * Constructor
     */
    AssetLoader(
        ghoul::lua::LuaState& luaState,
        ResourceSynchronizer& resourceSynchronizer,
        std::string assetRoot,
        std::string syncRoot
    );

    /**
     * Destructor
     */
    ~AssetLoader() = default;

    /**
     * Start synchronization recursively
     */
    void synchronizeEnabledAssets();

    /**
     * Load single asset.
     *  - Import one asset
     *  - Unimport all other assets
     */
    void loadSingleAsset(const std::string& identifier);

    /**
     * Import an asset
     * Add the asset as an optional on the root asset
     */
    void importAsset(const std::string& identifier);

    /**
     * Unimport an asset
     * Remove the asset as an optional on the root asset
     */
    void unimportAsset(const std::string& identifier);

    /**
     * Return the lua library
     */
    scripting::LuaLibrary luaLibrary();
    
    /**
     * Return the lua state
     */
    ghoul::lua::LuaState* luaState();

    /**
     * Return the root asset
     */
    Asset* rootAsset() const;

    /**
     * Return the sync root directory
     */
    const std::string& syncRootDirectory();

    void callOnSynchronize(Asset* asset);

    void callOnInitialize(Asset* asset);

    void callOnDeinitialize(Asset* asset);

    void callOnDependantInitialize(Asset* asset, Asset* dependant);

    void callOnDependantDeinitialize(Asset* asset, Asset* dependant);

    void synchronizeResource(const ghoul::Dictionary& d, std::function<void(bool)> onFinish);

    std::string generateAssetPath(const std::string& baseDirectory, const std::string& path) const;

private:
    Asset* importDependency(const std::string& identifier);
    Asset* importOptional(const std::string& identifier, bool enabled = true);
    Asset* loadAsset(std::string name);
    Asset* getAsset(std::string name);
    ghoul::filesystem::Directory currentDirectory();

    void pushAsset(Asset* asset);
    void popAsset();
    void updateLuaGlobals();
   
    std::unique_ptr<Asset> _rootAsset;
    std::map<std::string, std::unique_ptr<Asset>> _importedAssets;
    std::vector<Asset*> _assetStack;

    ResourceSynchronizer* _resourceSynchronizer;
    std::string _assetRootDirectory;
    std::string _syncRootDirectory;

    int importDependencyLua(std::string assetName);
    int importOptionalLua(std::string assetName, bool enabled);
    int resolveLocalResourceLua(Asset* asset);
    int resolveSyncedResourceLua(Asset* asset);
    int onFinishSynchronizationLua(Asset* asset);
    int createLuaTableEntries(const Asset* importer, const Asset* importedAsset);

    ghoul::lua::LuaState* _luaState;

    friend int assetloader::importDependency(lua_State* state);
    friend int assetloader::importOptional(lua_State* state);
    friend int assetloader::resolveLocalResource(lua_State* state);
    friend int assetloader::resolveSyncedResource(lua_State* state);
    friend int assetloader::onFinishSynchronization(lua_State* state);
};




} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETLOADER___H__
