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

#ifndef __OPENSPACE_CORE___ASSETMANAGER___H__
#define __OPENSPACE_CORE___ASSETMANAGER___H__

#include <openspace/scene/asset.h>

#include <openspace/scene/assetloader.h>
#include <openspace/util/resourcesynchronizer.h>

#include <openspace/scripting/lualibrary.h>

#include <vector>
#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace openspace {

class Asset;

/**
 * Interface for managing assets.
 * The asset manager interface is only concerned with "top level" assets,
 * i.e. assets that are loaded using setTargetAssetState, and not their dependencies.
 * However, an asset is not considered synchronized before all its deps are
 * synchronized.
 * Also, setting a target state of an asset to Unloaded will only unload an asset
 * from the system if it is not a dependency of a loaded asset.
 */

class AssetManager {
public:
    AssetManager(
        std::unique_ptr<AssetLoader> loader,
        std::unique_ptr<AssetSynchronizer> synchronizer
    );

    enum class AssetState : int {
        Unloaded,
        Loaded,
        LoadingFailed,
        Synchronized,
        SynchronizationFailed,
        Initialized,
        InitializationFailed
    };

    struct ManagedAsset {
        std::string path;
        std::shared_ptr<Asset> asset;
        AssetState state;
    };

    bool update();

    void setTargetAssetState(const std::string& path, AssetState targetState);
    void setTargetAssetState(Asset* asset, AssetState targetState);
    AssetState currentAssetState(Asset* asset);
    AssetState currentAssetState(const std::string& path);

    void clearAllTargetAssets();
    std::vector<std::shared_ptr<Asset>> loadedAssets();
    scripting::LuaLibrary luaLibrary();
    bool isDone();
private:
    std::shared_ptr<Asset> tryLoadAsset(const std::string& path);
    void unloadAsset(Asset* asset);
    bool tryInitializeAsset(Asset& asset);
    
    std::vector<ManagedAsset> _managedAssets;
    

    std::unordered_map<std::string, AssetState> _pendingStateChangeCommands;
    std::unordered_map<Asset*, AssetState> _stateChangesInProgress;

    std::unordered_map<Asset*, std::unordered_set<Asset*>> _syncAncestors;
    std::unordered_map<Asset*, std::unordered_set<Asset*>> _syncDependencies;
    
    std::unique_ptr<AssetLoader> _assetLoader;
    std::unique_ptr<AssetSynchronizer> _assetSynchronizer;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
