/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <string>

using namespace openspace;

namespace {

const Asset* findAsset(const std::string& assetName) {
    const AssetManager& manager = global::openSpaceEngine->assetManager();
    std::vector<const Asset*> allAssets = manager.allAssets();
    const std::filesystem::path path = manager.resolveAssetPath(assetName);
    auto it = std::find_if(
        allAssets.begin(),
        allAssets.end(),
        [&path](const Asset* a) { return a->path() == path; }
    );
    return it != allAssets.end() ? *it : nullptr;
}

/**
 * Adds an asset to the current scene. The parameter passed into this function is the path
 * to the file that should be loaded.
 */
[[codegen::luawrap]] void add(std::string assetName) {
    global::openSpaceEngine->assetManager().add(assetName);
}

/**
 * Removes the asset with the specified name from the scene. The parameter to this
 * function is the same that was originally used to load this asset, i.e. the path to the
 * asset file.
 */
[[codegen::luawrap]] void remove(std::string assetName) {
    global::openSpaceEngine->assetManager().remove(assetName);
}

/**
 * Reloads the asset with the specified name. If the asset was previously loaded
 * explicitly it will be removed and then re-added. If the asset was not previously
 * loaded, it will only be loaded instead.
 */
[[codegen::luawrap]] void reload(std::string assetName) {
    global::openSpaceEngine->assetManager().reload(assetName);
}

/**
 * Removes all assets that are currently loaded.
 */
[[codegen::luawrap]] void removeAll() {
    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().rootAssets();
    std::reverse(as.begin(), as.end());
    for (const Asset* asset : as) {
        global::openSpaceEngine->assetManager().remove(asset->path().string());
    }
}

/**
 * Returns true if the referenced asset already has been loaded. Otherwise false is
 * returned. The parameter to this function is the path of the asset that should be
 * tested.
 */
[[codegen::luawrap]] bool isLoaded(std::string assetName) {
    return findAsset(assetName) != nullptr;
}

/**
 * Returns the paths to all loaded assets, loaded directly or indirectly, as a table
 * containing the paths to all loaded assets.
 */
[[codegen::luawrap]] std::vector<std::filesystem::path> allAssets() {
    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().allAssets();
    std::vector<std::filesystem::path> res;
    res.reserve(as.size());
    for (const Asset* a : as) {
        res.push_back(a->path());
    }
    return res;
}

/**
 * Returns the paths to all loaded root assets, which are assets that are loaded directly
 * either through a profile or by calling the `openspace.asset.add` method.
 */
[[codegen::luawrap]] std::vector<std::filesystem::path> rootAssets() {
    return global::openSpaceEngine->assetManager().rootAssetPaths();
}

/**
 * Returns the path to all parents that are still interested in this Asset e.g., through
 * 'asset.require()'.
 */
[[codegen::luawrap]] std::vector<std::filesystem::path> parents(std::string assetName) {
    const Asset* asset = findAsset(assetName);
    return asset ? asset->initializedParents() : std::vector<std::filesystem::path>();
}

} // namespace

#include "assetmanager_lua_codegen.cpp"
