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

#include <openspace/scene/assetmanager.h>

#include <openspace/scripting/script_helper.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/exception.h>

#include "assetmanager_lua.inl"

namespace {
    const char* _loggerCat = "AssetManager";
}

namespace openspace {
AssetManager::AssetManager(std::unique_ptr<AssetLoader> loader, 
   std::unique_ptr<AssetSynchronizer> synchronizer)
    : _assetLoader(std::move(loader))
    , _assetSynchronizer(std::move(synchronizer))
{}


bool AssetManager::update() {
    
    // Load assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const AssetState targetState = c.second;
        if (targetState != AssetState::Unloaded && !_assetLoader->loadedAsset(path)) {
            std::shared_ptr<Asset> asset = tryLoadAsset(path);
        }
    }

    // Start synchronizations
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const AssetState targetState = c.second;
        
        std::shared_ptr<Asset> asset = _assetLoader->loadedAsset(path);
        if (!asset) {
            continue;
        }

        AssetSynchronizer::SynchronizationState state =
            _assetSynchronizer->assetState(asset.get());
        
        const bool alreadySyncedOrSyncing =
            (state == AssetSynchronizer::SynchronizationState::Resolved ||
             state == AssetSynchronizer::SynchronizationState::Synchronizing);
        
        const bool shouldSync =
            (targetState == AssetState::Synchronized ||
             targetState == AssetState::Initialized);
        
        if (shouldSync && !alreadySyncedOrSyncing) {
            //startSynchronization(asset);
        }
    }
    
    
            
    
    
    // Start synchronizations
    /*for (const auto& loadedAsset : loadedAssets) {
        const std::string& path = loadedAsset.first;
        Asset* asset = loadedAsset.second.get();
        const AssetState targetState = _pendingStateChangeCommands[path];
        updateSyncState(asset, targetState);
    }*/

    // Unload assets
    
    
    // Collect assets that were resolved and rejected.
    // Initialize if requested.
    // Update current state accordingly.
    /*for (const auto& stateChange : _assetSynchronizer->getStateChanges()) {
        handleSyncStateChange(stateChange);
    }*/

    _pendingStateChangeCommands.clear();
    return false;
}

/**
 * Load or unload asset depening on target state
 * Return shared pointer to asset if this loads the asset
 */
/*std::shared_ptr<Asset> AssetManager::updateLoadState(std::string path, AssetState targetState) {
    const bool shouldBeLoaded = targetState != AssetState::Unloaded;

    const std::shared_ptr<Asset> asset = _assetLoader->loadedAsset(path);
    const bool isLoaded = asset != nullptr;

    if (isLoaded && !shouldBeLoaded) {
        _managedAssets.erase(asset.get());
        _assetLoader->unloadAsset(asset.get());
    }
    else if (!isLoaded && shouldBeLoaded) {
        std::shared_ptr<Asset> loadedAsset = tryLoadAsset(path);
        
    }
    return nullptr;
}

/**
 * Start or cancel synchronizations depending on target state
 */
/*void AssetManager::updateSyncState(Asset* asset, AssetState targetState) {
    const bool shouldSync =
        targetState == AssetState::Synchronized ||
        targetState == AssetState::Initialized;

    if (shouldSync) {
        std::vector<std::shared_ptr<Asset>> importedAssets =
            asset->allAssets();

        for (const auto& a : importedAssets) {
            _assetSynchronizer->startSync(a);
            _syncAncestors[a.get()].insert(asset);
            if (a.get() != asset) {
                _syncDependencies[asset].insert(a.get());
            }
        }
        _stateChangesInProgress.emplace(
            asset,
            _pendingStateChangeCommands[asset->assetFilePath()]
        );
    } else {
        _assetSynchronizer->cancelSync(asset);
        _syncDependencies[asset].
        
        // Todo: Also cancel syncing of dependendencies
    }
}

void AssetManager::handleSyncStateChange(AssetSynchronizer::StateChange stateChange) {

    // Retrieve ancestors that were waiting for this asset to sync
    const auto it = _syncAncestors.find(stateChange.asset.get());
    if (it == _syncAncestors.end()) {
        return; // Should not happen. (No ancestor to this synchronization)
    }
    std::unordered_set<Asset*>& ancestors = it->second;

    if (stateChange.state ==
        AssetSynchronizer::SynchronizationState::Resolved)
    {

        for (const auto& ancestor : ancestors) {
            const bool initReady = ancestor->isInitReady();
            const bool shouldInit =
                _stateChangesInProgress[ancestor] == AssetState::Initialized;

            if (initReady) {
                _stateChangesInProgress.erase(ancestor);
                if (shouldInit) {
                    if (tryInitializeAsset(*ancestor)) {
                        //changedInititializations = true;
                        _managedAssets[ancestor].state = AssetState::Initialized;
                    }
                    else {
                        _managedAssets[ancestor].state = AssetState::InitializationFailed;
                    }
                }
                else {
                    _managedAssets[ancestor].state = AssetState::Synchronized;
                }
            }
        }

    }
    else if (stateChange.state ==
        AssetSynchronizer::SynchronizationState::Rejected)
    {
        for (const auto& ancestor : ancestors) {
            _managedAssets[ancestor].state = AssetState::SynchronizationFailed;
        }
    }

    _syncAncestors.erase(stateChange.asset.get());
}*/

void AssetManager::setTargetAssetState(const std::string& path, AssetState targetState) {
    _pendingStateChangeCommands[path] = targetState;
}

AssetManager::AssetState AssetManager::currentAssetState(Asset* asset) {
    const auto it = std::find_if(
        _managedAssets.begin(),
        _managedAssets.end(),
        [&asset](const ManagedAsset& ma){
            return ma.asset.get() == asset;
        }
    );
    if (it == _managedAssets.end()) {
        return AssetManager::AssetState::Unloaded;
    }
    return it->state;
}

void AssetManager::clearAllTargetAssets() {
    _pendingStateChangeCommands.clear();
    for (const auto& i : _assetLoader->loadedAssets()) {
        _pendingStateChangeCommands[i->id()] = AssetState::Unloaded;
    }
}

std::vector<std::shared_ptr<Asset>> AssetManager::loadedAssets() {
    std::vector<std::shared_ptr<Asset>> assets;
    assets.reserve(_managedAssets.size());
    for (auto ma : _managedAssets) {
        if (ma.asset != nullptr) {
            assets.push_back(ma.asset);
        }
    }
    return assets;
}

scripting::LuaLibrary AssetManager::luaLibrary() {
    return {
        "",
        {
            {
                "loadAsset",
                &luascriptfunctions::loadAsset,
                {this},
                "string",
                ""
            },
            {
                "unloadAsset",
                &luascriptfunctions::unloadAsset,
                {this},
                "string",
                ""
            }
        }
    };
}

bool AssetManager::isDone() {
    return _stateChangesInProgress.size() == 0;
}

void AssetManager::unloadAsset(Asset* asset) {

}
    
std::shared_ptr<Asset> AssetManager::tryLoadAsset(const std::string& path) {
    std::shared_ptr<Asset> asset;
    try {
        asset = _assetLoader->loadAsset(path);
    } catch (const ghoul::RuntimeError& e) {
        LERROR(e.component << ": " << e.message);
    }
    if (asset == nullptr) {
        LERROR("Could not load asset from " << path);
        _managedAssets.push_back(ManagedAsset{
            path,
            nullptr,
            AssetState::LoadingFailed
        });
        return nullptr;
    }
    const auto it = std::find_if(
        _managedAssets.begin(),
        _managedAssets.end(),
        [&asset](const ManagedAsset& ma){
            return ma.asset == asset;
        }
    );
    if (it == _managedAssets.end()) {
        _managedAssets.push_back(ManagedAsset{
            path,
            asset,
            AssetState::Loaded
        });
        return asset;
    }
    return nullptr;
}

bool AssetManager::tryInitializeAsset(Asset& asset) {
    try {
        asset.initialize();
        return true;
    } catch (const ghoul::RuntimeError& e) {
        LERROR(e.component << ": " << e.message);
        return false;
    }
}


}
