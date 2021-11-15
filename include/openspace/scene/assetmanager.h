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
#include <ghoul/lua/luastate.h>
#include <filesystem>
#include <unordered_set>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Asset;

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

    void deinitialize();
    void add(const std::string& path);
    void remove(const std::string& path);

    void update();
    scripting::LuaLibrary luaLibrary();

    std::vector<const Asset*> allAssets() const;

    /**
     * Load an asset
     */
    bool loadAsset(Asset* asset, Asset* parent);

    /**
     * Unload an asset
     */
    void unloadAsset(Asset* asset);

    /**
     * Call the onInitialize function specified in the asset file
     */
    void callOnInitialize(Asset* asset) const;

    /**
     * Call the onDeinitialize function specified in the asset file
     */
    void callOnDeinitialize(Asset* asset) const;

private:
    void setUpAssetLuaTable(Asset* asset);

    Asset* retrieveAsset(const std::string& name, Asset* base);
    void setCurrentAsset(Asset* asset);

    // The authoratative list of all assets that have been loaded through the AssetManager
    std::vector<std::unique_ptr<Asset>> _assets;

    // A list of all root assets that have been loaded directly through the `add` function
    std::vector<Asset*> _rootAssets;

    // This list contains all of the assets that are queued to be loading in the next
    // update call
    std::unordered_set<std::string> _assetAddQueue;

    // The list contains all of the assets that should be removed in the next update call
    std::unordered_set<std::string> _assetRemoveQueue;

    // This list contains all assets that need to be initializaed in the next update call
    std::vector<Asset*> _toBeInitialized;

    // This list contains all of the assets that should be deleted in the next update call
    std::vector<std::unique_ptr<Asset>> _toBeDeleted;
    
    SynchronizationWatcher _synchronizationWatcher;

    std::string _assetRootDirectory;
    
    ghoul::lua::LuaState* _luaState = nullptr;

    // References to the onInitialize and the onDeinitialize functions for each Asset
    std::unordered_map<Asset*, std::vector<int>> _onInitializeFunctionRefs;
    std::unordered_map<Asset*, std::vector<int>> _onDeinitializeFunctionRefs;

    int _assetsTableRef = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
