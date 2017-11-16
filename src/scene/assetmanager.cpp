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
AssetManager::AssetManager(std::unique_ptr<AssetLoader> loader)
    : _assetLoader(std::move(loader))
{}

bool AssetManager::update() {
    // 1. Load assets.
    // 2. Start/cancel synchronizations
    // 3. Unload assets.

    // Load assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const Asset::State targetState = c.second;
        if (targetState != Asset::State::Unloaded && !_assetLoader->loadedAsset(path)) {
            std::shared_ptr<Asset> asset = tryLoadAsset(path);
        }
    }

    // Start/cancel synchronizations and/or deinitialize
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const Asset::State targetState = c.second;
        
        std::shared_ptr<Asset> asset = _assetLoader->loadedAsset(path);
        if (!asset) {
            continue;
        }

        Asset::State assetState = asset->state();
        
        const bool syncedOrSyncing =
            (assetState == Asset::State::SyncResolved ||
             assetState == Asset::State::Synchronizing);
        
        const bool shouldBeSynced =
            (targetState == Asset::State::SyncResolved ||
             targetState == Asset::State::Initialized);

        if (shouldBeSynced && !syncedOrSyncing) {
            startSynchronization(*asset);
        } else if (!shouldBeSynced && syncedOrSyncing) {
            cancelSynchronization(*asset);
        }
        
        const bool shouldBeInitialized = targetState == Asset::State::Initialized;
        const bool isInitialized = asset->state() == Asset::State::Initialized;
        
        if (shouldBeInitialized && !isInitialized) {
            _pendingInitializations.insert(asset.get());
        } else {
            _pendingInitializations.erase(asset.get());
            tryDeinitializeAsset(*asset);
        }
    }
    
    // TODO: Handle state changes
    /*std::vector<AssetSynchronizer::StateChange> stateChanges =
        _assetSynchronizer->getStateChanges();
    
    for (AssetSynchronizer::StateChange& stateChange : stateChanges) {
        Asset* a = stateChange.asset.get();
        auto it = _pendingInitializations.find(a);
        if (it == _pendingInitializations.end()) {
            continue;
        }
        Asset::ReadyState currentState = a->readyState();
        if (currentState != Asset::ReadyState::Initialized)
        {
            _pendingInitializations.erase(it);
            tryInitializeAsset(*a);
        }
    }*/

    _pendingStateChangeCommands.clear();
    return false;
}

void AssetManager::startSynchronization(Asset&) {
    // todo: implement 
}

void AssetManager::cancelSynchronization(Asset&) {
    // todo: implement this
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

void AssetManager::setTargetAssetState(const std::string& path, Asset::State targetState) {
    _pendingStateChangeCommands[path] = targetState;
}

Asset::State AssetManager::currentAssetState(Asset* asset) {
    const auto it = std::find_if(
        _managedAssets.begin(),
        _managedAssets.end(),
        [&asset](const ManagedAsset& ma){
            return ma.asset.get() == asset;
        }
    );
    if (it == _managedAssets.end()) {
        return Asset::State::Unloaded;
    }
    return it->asset->state();
}

Asset::State AssetManager::currentAssetState(const std::string& assetIdentifier) {
    const auto it = std::find_if(
        _managedAssets.begin(),
        _managedAssets.end(),
        [&assetIdentifier](const ManagedAsset& ma) {
        return ma.path == assetIdentifier;
    }
    );
    if (it == _managedAssets.end()) {
        return Asset::State::Unloaded;
    }
    return it->asset->state();
}


void AssetManager::clearAllTargetAssets() {
    _pendingStateChangeCommands.clear();
    for (const auto& ma : _managedAssets) {
        _pendingStateChangeCommands[ma.path] = Asset::State::Unloaded;
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
                "synchronizeAsset",
                &luascriptfunctions::synchronizeAsset,
                {this},
                "string",
                ""
            },
            {
                "initializeAsset",
                &luascriptfunctions::initializeAsset,
                { this },
                "string",
                ""
            },
            {
                "unloadAsset",
                &luascriptfunctions::unloadAsset,
                { this },
                "string",
                ""
            }
        }
    };
}

bool AssetManager::isDone() {
    return _pendingStateChangeCommands.size() == 0 && _pendingInitializations.size() == 0;
}

void AssetManager::unloadAsset(Asset* asset) {

}
    
std::shared_ptr<Asset> AssetManager::tryLoadAsset(const std::string& path) {
    /*
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
            Asset::State::LoadingFailed
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
    } */
    return nullptr;
   
}

bool AssetManager::tryInitializeAsset(Asset& asset) {
    try {
        asset.initialize();
    } catch (const ghoul::RuntimeError& e) {
        LERROR("Error when initializing asset. " << e.component << ": " << e.message);
        return false;
    }
    return true;
}
    
bool AssetManager::tryDeinitializeAsset(Asset& asset) {
    try {
        asset.deinitialize();
    } catch (const ghoul::RuntimeError& e) {
        LERROR("Error when deinitializing asset. " << e.component << ": " << e.message);
        return false;
    }
    return true;
}

}
