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

void AssetManager::update() {
    std::unordered_map<std::string, std::shared_ptr<Asset>> loadedAssets;

    // Load and unload assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const AssetState targetState = c.second;

        const bool shouldBeLoaded = targetState != AssetState::Unloaded;
        const bool isLoaded = _assetLoader->hasLoadedAsset(path);

        if (isLoaded && !shouldBeLoaded) {
            _assetLoader->unloadAsset(path);
        } else if (!isLoaded && shouldBeLoaded) {
            std::shared_ptr<Asset> loadedAsset = _assetLoader->loadAsset(path);
            loadedAssets.emplace(path, loadedAsset);
        }
    }

    // Collect all assets for synchronization
    for (const auto& loadedAsset : loadedAssets) {
        const AssetState targetState = _pendingStateChangeCommands[loadedAsset.first];
        bool shouldSync = targetState == AssetState::Synchronized || targetState == AssetState::Initialized;

        if (!shouldSync) {
            continue;
        }

        std::vector<std::shared_ptr<Asset>> importedAssets = loadedAsset.second->allAssets();
        for (const auto& a : importedAssets) {
            _assetSynchronizer->addAsset(a);
            _syncAncestors[a].insert(loadedAsset.second);
        }
        _stateChangesInProgress.emplace(
            std::make_pair(loadedAsset.second, _pendingStateChangeCommands[loadedAsset.first])
        );
    }

    // Start asset synchronization. (Async operation)
    _assetSynchronizer->syncUnsynced();
    
    // Collect finished synchronizations and initialize assets
    std::vector<std::shared_ptr<Asset>> syncedAssets = _assetSynchronizer->getSynchronizedAssets();
    for (const auto& syncedAsset : syncedAssets) {
        // Retrieve ancestors that were waiting for this asset to sync
        const auto& it = _syncAncestors.find(syncedAsset);
        if (it == _syncAncestors.end()) {
            continue; // Should not happen. (No ancestor to this synchronization)
        }
        std::unordered_set<std::shared_ptr<Asset>>& ancestors = it->second;

        for (const auto& ancestor : ancestors) {
            const bool initReady = ancestor->isInitReady();
            const bool shouldInit = _stateChangesInProgress[ancestor] == AssetState::Initialized;
            if (initReady) {
                _stateChangesInProgress.erase(ancestor);
                if (shouldInit) {
                    ancestor->initialize();
                }
            }
        }
        _syncAncestors.erase(syncedAsset);
    }

    _pendingStateChangeCommands.clear();
}

void AssetManager::setTargetAssetState(const std::string& path, AssetState targetState) {
    _pendingStateChangeCommands[path] = targetState;
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
                "unimportAsset",
                &luascriptfunctions::unloadAsset,
                {this},
                "string",
                ""
            }
        }
    };
}


}
