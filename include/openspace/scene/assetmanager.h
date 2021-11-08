/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___ASSETMANAGER___H__
#define __OPENSPACE_CORE___ASSETMANAGER___H__

#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <memory>
#include <mutex>
#include <unordered_set>
#include <vector>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Asset;
class AssetLoader;
class SynchronizationWatcher;

/**
 * Interface for managing assets.
 * The asset manager interface is only concerned with "top level" assets, and not their
 * dependencies. However, an asset is not considered synchronized before all its deps are
 * synchronized. Also, setting a target state of an asset to Unloaded will only unload an
 * asset from the system if it is not a dependency of a loaded asset.
 */
class AssetManager {
public:
    AssetManager(ghoul::lua::LuaState* state, std::string assetRootDirectory);
    ~AssetManager();

    void initialize();
    void deinitialize();
    void add(const std::string& path);
    void remove(const std::string& path);
    void removeAll();
    const Asset& rootAsset() const;
    Asset& rootAsset();

    bool update();
    scripting::LuaLibrary luaLibrary();

    /**
     * Load an asset
     */
    bool loader_loadAsset(Asset* asset);

    /**
     * Unload an asset
     */
    void loader_unloadAsset(Asset* asset);

    /**
     * Call the onInitialize function specified in the asset file
     */
    void loader_callOnInitialize(Asset* asset);

    /**
     * Call the onDeinitialize function specified in the asset file
     */
    void loader_callOnDeinitialize(Asset* asset);

private:
    /**
     * Add the asset as a request of the root asset
     */
    std::shared_ptr<Asset> loader_add(const std::string& identifier);

    /**
     * Remove the asset as a request of the root asset
     */
    void loader_remove(const std::string& identifier);
    /**
     * Return the asset identified by the identifier, if the asset is tracked. Otherwise 
     * return nullptr.
     */
    std::shared_ptr<Asset> loader_has(const std::string& name) const;

    /// Return the root asset
    const Asset& loader_rootAsset() const;

    /// Return the root asset
    Asset& loader_rootAsset();

    void loader_setUpAssetLuaTable(Asset* asset);
    void loader_tearDownAssetLuaTable(Asset* asset);

    std::shared_ptr<Asset> loader_asset(const std::string& name);
    std::filesystem::path loader_currentDirectory() const;

    void loader_setCurrentAsset(Asset* asset);


    std::unordered_set<std::string> _assetAddQueue;
    std::unordered_set<std::string> _assetRemoveQueue;
    std::mutex _pendingInitializationsMutex;
    std::vector<std::shared_ptr<Asset>> _pendingInitializations;

    SynchronizationWatcher _synchronizationWatcher;
    //AssetLoader _assetLoader;


    // Member variables
    std::shared_ptr<Asset> _rootAsset;
    Asset* _currentAsset = nullptr;
    std::unordered_map<std::string, std::weak_ptr<Asset>> _trackedAssets;
    std::string _assetRootDirectory;
    ghoul::lua::LuaState* _luaState;

    // References to Lua values
    std::unordered_map<Asset*, std::vector<int>> _onInitializeFunctionRefs;
    std::unordered_map<Asset*, std::vector<int>> _onDeinitializeFunctionRefs;

    int _assetsTableRef = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
