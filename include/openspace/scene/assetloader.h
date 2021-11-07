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

#ifndef __OPENSPACE_CORE___ASSETLOADER___H__
#define __OPENSPACE_CORE___ASSETLOADER___H__

#include <openspace/scene/asset.h>
#include <filesystem>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>

struct lua_State;

namespace ghoul::filesystem { class Directory; }
namespace ghoul::lua { class LuaState; }

namespace openspace {

class ResourceSynchronization;
class SynchronizationWatcher;

class AssetLoader {
public:
    AssetLoader(ghoul::lua::LuaState* luaState, SynchronizationWatcher* syncWatcher,
        std::string assetRootDirectory);

    ~AssetLoader();

    /**
     * Add the asset as a request of the root asset
     */
    std::shared_ptr<Asset> add(const std::string& identifier);

    /**
     * Remove the asset as a request of the root asset
     */
    void remove(const std::string& identifier);

    /**
     * Return the asset identified by the identifier, if the asset is tracked. Otherwise 
     * return nullptr.
     */
    std::shared_ptr<Asset> has(const std::string& name) const;

    /// Return the root asset
    const Asset& rootAsset() const;

    /// Return the root asset
    Asset& rootAsset();

    /**
     * Load an asset
     */
    bool loadAsset(Asset* asset);

    /**
     * Unload an asset
     */
    void unloadAsset(Asset* asset);

    /**
     * Call the onInitialize function specified in the asset file
     */
    void callOnInitialize(Asset* asset);

    /**
     * Call the onDeinitialize function specified in the asset file
     */
    void callOnDeinitialize(Asset* asset);

private:
    void setUpAssetLuaTable(Asset* asset);
    void tearDownAssetLuaTable(Asset* asset);

    std::shared_ptr<Asset> asset(const std::string& name);
    std::filesystem::path currentDirectory() const;

    void setCurrentAsset(Asset* asset);

    // Member variables
    std::shared_ptr<Asset> _rootAsset;
    Asset* _currentAsset = nullptr;
    std::unordered_map<std::string, std::weak_ptr<Asset>> _trackedAssets;
    SynchronizationWatcher* _synchronizationWatcher;
    std::string _assetRootDirectory;
    ghoul::lua::LuaState* _luaState;

    // References to Lua values
    std::unordered_map<Asset*, std::vector<int>> _onInitializeFunctionRefs;
    std::unordered_map<Asset*, std::vector<int>> _onDeinitializeFunctionRefs;

    int _assetsTableRef = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETLOADER___H__
