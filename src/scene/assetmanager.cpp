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

#include <openspace/scene/assetmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/scene/assetloader.h>
#include <openspace/scene/profile.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>

#include "assetmanager_lua.inl"

namespace openspace {

AssetManager::AssetManager(ghoul::lua::LuaState* state, std::string assetRootDirectory)
    : _assetLoader(state, &_synchronizationWatcher, std::move(assetRootDirectory))
{}

void AssetManager::initialize() {
    ZoneScoped

    _assetLoader.rootAsset().initialize();
}

void AssetManager::deinitialize() {
    _assetLoader.rootAsset().deinitialize();
    _assetLoader.rootAsset().unload();
}

bool AssetManager::update() {
    ZoneScoped

    // Add assets
    for (const std::string& asset : _assetAddQueue) {
        ZoneScopedN("(add) Pending State Change")
        _assetLoader.add(asset);
        global::profile->addAsset(asset);
    }
    _assetAddQueue.clear();

    // Remove assets
    for (const std::string& asset : _assetRemoveQueue) {
        ZoneScopedN("(remove) Pending State change")
        if (_assetLoader.has(asset)) {
            _assetLoader.remove(asset);
            global::profile->removeAsset(asset);
        }
    }
    _assetRemoveQueue.clear();

    // Change state based on synchronizations
    _synchronizationWatcher.notify();

    return false;
}

void AssetManager::add(const std::string& path) {
    // First check if the path is already in the remove queue. If so, remove it from there
    const auto it = _assetRemoveQueue.find(path);
    if (it != _assetRemoveQueue.end()) {
        _assetRemoveQueue.erase(it);
    }

    _assetAddQueue.insert(path);
}

void AssetManager::remove(const std::string& path) {
    // First check if the path is already in the add queue. If so, remove it from there
    const auto it = _assetAddQueue.find(path);
    if (it != _assetAddQueue.end()) {
        _assetAddQueue.erase(it);
    }

    _assetRemoveQueue.insert(path);
}

void AssetManager::removeAll() {
    ZoneScoped

    _assetAddQueue.clear();
    _assetRemoveQueue.clear();
    for (const Asset* a : _assetLoader.rootAsset().requestedAssets()) {
        _assetRemoveQueue.insert(a->assetFilePath().string());
    }
}

const Asset& AssetManager::rootAsset() const {
    return _assetLoader.rootAsset();
}

Asset& AssetManager::rootAsset() {
    return _assetLoader.rootAsset();
}

scripting::LuaLibrary AssetManager::luaLibrary() {
    return {
        "asset",
        {
            // Functions for adding/removing assets
            {
                "add",
                &luascriptfunctions::asset::add,
                "string",
                "Adds an asset to the current scene. The parameter passed into this "
                "function is the path to the file that should be loaded"
            },
            {
                "remove",
                &luascriptfunctions::asset::remove,
                "string",
                "Removes the asset with the specfied name from the scene. The parameter "
                "to this function is the same that was originally used to load this "
                "asset, i.e. the path to the asset file"
            }
        }
    };
}

} // namespace openspace
