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

void AssetManager::initialize() {
    _addAssetCallbackHandle = _assetLoader->addAssetLoadCallback(
        [this] (std::shared_ptr<Asset> a) {
            a->addStateChangeCallback([a, this] (Asset::State state) {
                assetStateChanged(a, state);
            });
        }
     );
    std::shared_ptr<Asset> rootAsset = _assetLoader->rootAsset();
    rootAsset->addStateChangeCallback([&rootAsset, this] (Asset::State state) {
        assetStateChanged(rootAsset, state);
    });
    rootAsset->initialize();
}

void AssetManager::deinitialize() {
    _assetLoader->rootAsset()->deinitialize();
    _assetLoader->removeAssetLoadCallback(_addAssetCallbackHandle);
}

bool AssetManager::update() {
    // Initialize assets
    {
        std::lock_guard<std::mutex> guard(_pendingInitializationsMutex);
        for (const auto& a : _pendingInitializations) {
            a->initialize();
        }
        _pendingInitializations.clear();
    }

    // Add assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const bool add = c.second;
        if (add) {
            std::shared_ptr<Asset> asset = tryAddAsset(path);
        }
    }

    // Remove assets
    for (const auto& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const bool remove = !c.second;
        if (remove && _assetLoader->has(path)) {
            tryRemoveAsset(path);
        }
    }

    _pendingStateChangeCommands.clear();
    return false;
}

void AssetManager::assetStateChanged(std::shared_ptr<Asset> asset, Asset::State state) {
    if (rootAsset()->state() == Asset::State::Initialized) {
        if (state == Asset::State::Loaded) {
            asset->startSynchronizations();
        }
        if (state == Asset::State::SyncResolved) {
            std::lock_guard<std::mutex> guard(_pendingInitializationsMutex);
            _pendingInitializations.push_back(asset);
        }
    } else {
        asset->deinitialize();
    }
    // Todo: Check if assets should start syncing or if they should init.
    // flags: autoSync, autoInit ?
}

void AssetManager::add(const std::string& path) {
    _pendingStateChangeCommands[path] = true;
}

void AssetManager::remove(const std::string& path) {
    _pendingStateChangeCommands[path] = false;
}

void AssetManager::removeAll() {
    _pendingStateChangeCommands.clear();
    std::vector<std::shared_ptr<Asset>> allAssets =
        _assetLoader->rootAsset()->requestedAssets();

    for (const auto& a : allAssets) {
        _pendingStateChangeCommands[a->assetFilePath()] = false;
    }
}

std::shared_ptr<Asset> AssetManager::rootAsset() {
    return _assetLoader->rootAsset();
}

scripting::LuaLibrary AssetManager::luaLibrary() {
    return {
        "asset",
        {
            // Functions for adding/removing assets
            {
                "add",
                &luascriptfunctions::asset::add,
                {this},
                "string",
                ""
            },
            {
                "remove",
                &luascriptfunctions::asset::remove,
                {this},
                "string",
                ""
            }
        }
    };
}
    
std::shared_ptr<Asset> AssetManager::tryAddAsset(const std::string& path) {
    try {
        _assetLoader->add(path);
    } catch (const ghoul::RuntimeError& e) {
        LERROR("Error adding asset: " << e.component << ": " << e.message);
    }
    return nullptr;
}

bool AssetManager::tryRemoveAsset(const std::string& path) {
    try {
        _assetLoader->remove(path);
    } catch (const ghoul::RuntimeError& e) {
        LERROR("Error removing asset: " << e.component << ": " << e.message);
        return false;
    }
    return true;
}

}
