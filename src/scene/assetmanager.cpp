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
    std::unordered_map<std::string, std::shared_ptr<Asset>> loadedAssets;
    // Load and unload assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const AssetState targetState = c.second;

        std::shared_ptr<Asset> asset = updateLoadState(path, targetState);
        if (asset) {
            loadedAssets.emplace(path, asset);
        }
    }

    // Collect all assets for synchronization
    for (const auto& loadedAsset : loadedAssets) {
        const std::string& path = loadedAsset.first;
        const AssetState targetState = _pendingStateChangeCommands[path];

        updateSyncState(path, targetState);
    }

    // Collect assets that were resolved and rejected.
    // Initialize if requested.
    // Update current state accordingly.
    for (const auto& stateChange : _assetSynchronizer->getStateChanges()) {
        handleSyncStateChange(stateChange);
    }

    _pendingStateChangeCommands.clear();
    return false;
}

/**
 * Load or unload asset depening on target state
 * Return shared pointer to asset if this loads the asset
 */
std::shared_ptr<Asset> AssetManager::updateLoadState(std::string path, AssetState targetState) {
    const bool shouldBeLoaded = targetState != AssetState::Unloaded;

    const std::shared_ptr<Asset> asset = _assetLoader->loadedAsset(path);
    const bool isLoaded = asset != nullptr;

    if (isLoaded && !shouldBeLoaded) {
        _currentStates.erase(asset.get());
        _assetLoader->unloadAsset(asset.get());
    }
    else if (!isLoaded && shouldBeLoaded) {
        std::shared_ptr<Asset> loadedAsset = tryLoadAsset(path);
        if (loadedAsset) {
            _currentStates[loadedAsset.get()] = AssetState::Loaded;
            return loadedAsset;
        }
        else {
            _currentStates[loadedAsset.get()] = AssetState::LoadingFailed;
        }
    }
    return nullptr;
}

/**
 * Start or cancel synchronizations depending on target state
 */
void AssetManager::updateSyncState(Asset* asset, AssetState targetState) {
    const bool shouldSync =
        targetState == AssetState::Synchronized ||
        targetState == AssetState::Initialized;

    if (shouldSync) {
        std::vector<std::shared_ptr<Asset>> importedAssets =
            loadedAsset.second->allAssets();

        for (const auto& a : importedAssets) {
            _assetSynchronizer->startSync(a);
            _syncAncestors[a].insert(loadedAsset.second);
            //_syncDependencies[loadedAsset.second].insert(a);
        }
        _stateChangesInProgress.emplace(
            loadedAsset.second,
            _pendingStateChangeCommands[loadedAsset.first]
        );
    } else {
        _assetSynchronizer->cancelSync(a);
        // Todo: Also cancel syncing of dependendencies
    }
}

void handleSyncStateChange(AssetSynchronizer::StateChange stateChange) {

    // Retrieve ancestors that were waiting for this asset to sync
    const auto it = _syncAncestors.find(stateChange.asset);
    if (it == _syncAncestors.end()) {
        continue; // Should not happen. (No ancestor to this synchronization)
    }
    std::unordered_set<std::shared_ptr<Asset>>& ancestors = it->second;

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
                        changedInititializations = true;
                        _currentStates[ancestor.get()] = AssetState::Initialized;
                    }
                    else {
                        _currentStates[ancestor.get()] = AssetState::InitializationFailed;
                    }
                }
                else {
                    _currentStates[ancestor.get()] = AssetState::Synchronized;
                }
            }
        }

    }
    else if (stateChange.state ==
        AssetSynchronizer::SynchronizationState::Rejected)
    {
        for (const auto& ancestor : ancestors) {
            _currentStates[ancestor.get()] = AssetState::SynchronizationFailed;
        }
    }

    _syncAncestors.erase(stateChange.asset);
}

void AssetManager::setTargetAssetState(const std::string& path, AssetState targetState) {
    _pendingStateChangeCommands[path] = targetState;
}

AssetManager::AssetState AssetManager::currentAssetState(Asset* asset) {
    const auto it = _currentStates.find(asset);
    if (it == _currentStates.end()) {
        return AssetManager::AssetState::Unloaded;
    }
    return it->second;
}

void AssetManager::clearAllTargetAssets() {
    _pendingStateChangeCommands.clear();
    for (const auto& i : _assetLoader->loadedAssets()) {
        _pendingStateChangeCommands[i->id()] = AssetState::Unloaded;
    }
}

std::vector<std::shared_ptr<Asset>> AssetManager::allAssets()
{
    return std::vector<std::shared_ptr<Asset>>();
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

std::shared_ptr<Asset> AssetManager::tryLoadAsset(const std::string& path) {
    try {
        return _assetLoader->loadAsset(path);
    } catch (const ghoul::RuntimeError& e) {
        LERROR(e.component << ": " << e.message);
        return nullptr;
    }
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
