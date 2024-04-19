/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

namespace {

/**
 * Adds an asset to the current scene. The parameter passed into this function is the path
 * to the file that should be loaded.
 */
[[codegen::luawrap]] void add(std::string assetName) {
    openspace::global::openSpaceEngine->assetManager().add(assetName);
}

/**
 * Removes the asset with the specfied name from the scene. The parameter to this function
 * is the same that was originally used to load this asset, i.e. the path to the asset
 * file.
 */
[[codegen::luawrap]] void remove(std::string assetName) {
    openspace::global::openSpaceEngine->assetManager().remove(assetName);
}

/**
 * Removes all assets that are currently loaded
 */
[[codegen::luawrap]] void removeAll() {
    using namespace openspace;
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
    using namespace openspace;
    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().allAssets();
    for (const Asset* a : as) {
        if (a->path() == assetName) {
            return true;
        }
    }
    return false;
}

/**
 * Returns the paths to all loaded assets, loaded directly or indirectly, as a table
 * containing the paths to all loaded assets.
 */
[[codegen::luawrap]] std::vector<std::filesystem::path> allAssets() {
    using namespace openspace;
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
    using namespace openspace;
    std::vector<const Asset*> as = global::openSpaceEngine->assetManager().rootAssets();
    std::vector<std::filesystem::path> res;
    res.reserve(as.size());
    for (const Asset* a : as) {
        res.push_back(a->path());
    }
    return res;
}

#include "assetmanager_lua_codegen.cpp"

} // namespace
