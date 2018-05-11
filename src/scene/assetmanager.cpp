/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/scene/assetloader.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>

#include "assetmanager_lua.inl"

namespace openspace {

AssetManager::AssetManager(std::unique_ptr<AssetLoader> loader,
                           std::unique_ptr<SynchronizationWatcher> syncWatcher)
    : _synchronizationWatcher(std::move(syncWatcher))
    , _assetLoader(std::move(loader))
{}

void AssetManager::initialize() {
    _assetLoader->addAssetListener(this);
    std::shared_ptr<Asset> rootAsset = _assetLoader->rootAsset();
    rootAsset->initialize();
}

void AssetManager::deinitialize() {
    _assetLoader->rootAsset()->deinitialize();
    _assetLoader->rootAsset()->unload();
    _assetLoader->removeAssetListener(this);
    _assetLoader = nullptr;
}

bool AssetManager::update() {
    // Add assets
    for (const std::pair<const std::string, bool>& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const bool add = c.second;
        if (add) {
            std::shared_ptr<Asset> asset = _assetLoader->add(path);
        }
    }
    // Remove assets
    for (const std::pair<const std::string, bool>& c : _pendingStateChangeCommands) {
        const std::string& path = c.first;
        const bool remove = !c.second;
        if (remove && _assetLoader->has(path)) {
            _assetLoader->remove(path);
        }
    }
    _pendingStateChangeCommands.clear();

    // Change state based on synchronizations
    _synchronizationWatcher->notify();

    return false;
}

void AssetManager::assetStateChanged(std::shared_ptr<Asset>, Asset::State) {
    // Potential todo: notify user about asset stage change
    //LINFO(asset->id() << " changed state to " << static_cast<int>(state));
}

void AssetManager::assetRequested(std::shared_ptr<Asset>, std::shared_ptr<Asset>) {
    // Potential todo: notify user about asset request
    //LINFO(parent->id() << " requested " << child->id());
}

void AssetManager::assetUnrequested(std::shared_ptr<Asset>, std::shared_ptr<Asset>) {
    // Potential todo: notify user about asset unrequest
    //LINFO(parent->id() << " unrequested " << child->id());
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

    for (const std::shared_ptr<Asset>& a : allAssets) {
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
                { this },
                "string",
                ""
            },
            {
                "remove",
                &luascriptfunctions::asset::remove,
                { this },
                "string",
                ""
            }
        }
    };
}

}
