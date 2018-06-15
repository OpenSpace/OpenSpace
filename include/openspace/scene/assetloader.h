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

#ifndef __OPENSPACE_CORE___ASSETLOADER___H__
#define __OPENSPACE_CORE___ASSETLOADER___H__

#include <openspace/scene/asset.h>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>

struct lua_State;

namespace ghoul::filesystem { class Directory; }
namespace ghoul::lua { class LuaState; }

namespace openspace {

namespace assetloader {

int onInitialize(lua_State* state);
int onDeinitialize(lua_State* state);
int onInitializeDependency(lua_State* state);
int onDeinitializeDependency(lua_State* state);
int require(lua_State* state);
int request(lua_State* state);
int exists(lua_State* state);
int localResource(lua_State* state);
int syncedResource(lua_State* state);
int noOperation(lua_State* state);
int exportAsset(lua_State* state);

} // namespace assetloader

class Asset;
class ResourceSynchronization;
class SynchronizationWatcher;
class AssetListener;

class AssetLoader {
public:
    AssetLoader(ghoul::lua::LuaState& luaState, SynchronizationWatcher* syncWatcher,
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
     * Enable the asset to be reused when the same path is required/requested again
     */
    void trackAsset(std::shared_ptr<Asset> asset);

    /**
     * Disable the asset from being reused when the same path is required/requested again
     */
    void untrackAsset(Asset* asset);

    /**
    * Return the asset identified by the identifier,
    * if the asset is tracked. Otherwise return nullptr.
    */
    std::shared_ptr<Asset> has(const std::string& identifier) const;

    /**
     * Return the lua state
     */
    ghoul::lua::LuaState* luaState();

    /**
     * Return the root asset
     */
    std::shared_ptr<Asset> rootAsset() const;

    /**
    * Return the asset root directory
    */
    const std::string& assetRootDirectory() const;

    /**
     * Load an asset
     */
    bool loadAsset(std::shared_ptr<Asset> asset);

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

    /**
     * Call the dependency.onInitialize function specified in the asset file
     */
    void callOnDependencyInitialize(Asset* asset, Asset* dependant);

    /**
     * Call the dependency.onDeinitialize function specified in the asset file
     */
    void callOnDependencyDeinitialize(Asset* asset, Asset* dependant);

    /**
     * Generate the absolute path for an asset specified as `path`
     * relative to `baseDirectory`
     */
    std::string generateAssetPath(const std::string& baseDirectory,
        const std::string& assetPath) const;

    /**
     * Add listener to asset state changes
     */
    void addAssetListener(AssetListener* listener);

    /**
     * Remove listener to asset state changes
     */
    void removeAssetListener(AssetListener* listener);

    /**
     * Notify listeners about asset state change
     */
    void assetStateChanged(std::shared_ptr<Asset> asset, Asset::State state);

    /**
     * Notify listeners about new requests
     */
    void assetRequested(std::shared_ptr<Asset> parent, std::shared_ptr<Asset> child);

    /**
     * Notify listeners about removed requests
     */
    void assetUnrequested(std::shared_ptr<Asset> parent, std::shared_ptr<Asset> child);

private:
    std::shared_ptr<Asset> require(const std::string& identifier);
    std::shared_ptr<Asset> request(const std::string& identifier);
    void unrequest(const std::string& identifier);

    void setUpAssetLuaTable(Asset* asset);
    void tearDownAssetLuaTable(Asset* asset);

    std::shared_ptr<Asset> getAsset(std::string name);
    ghoul::filesystem::Directory currentDirectory() const;

    void setCurrentAsset(std::shared_ptr<Asset> asset);
    void addLuaDependencyTable(Asset* dependant, Asset* dependency);

    // Lua functions
    int onInitializeLua(Asset* asset);
    int onDeinitializeLua(Asset* asset);
    int onInitializeDependencyLua(Asset* dependant, Asset* dependency);
    int onDeinitializeDependencyLua(Asset* dependant, Asset* dependency);
    int requireLua(Asset* dependant);
    int requestLua(Asset* parent);
    int existsLua(Asset* asset);
    int localResourceLua(Asset* asset);
    int syncedResourceLua(Asset* asset);
    int exportAssetLua(Asset* asset);

    // Friend c closures (callable from lua, and maps to lua functions above)
    friend int assetloader::onInitialize(lua_State* state);
    friend int assetloader::onDeinitialize(lua_State* state);
    friend int assetloader::onInitializeDependency(lua_State* state);
    friend int assetloader::onDeinitializeDependency(lua_State* state);
    friend int assetloader::require(lua_State* state);
    friend int assetloader::request(lua_State* state);
    friend int assetloader::exists(lua_State* state);
    friend int assetloader::localResource(lua_State* state);
    friend int assetloader::syncedResource(lua_State* state);
    friend int assetloader::exportAsset(lua_State* state);

    // Member variables
    std::shared_ptr<Asset> _rootAsset;
    std::shared_ptr<Asset> _currentAsset;
    std::unordered_map<std::string, std::weak_ptr<Asset>> _trackedAssets;
    SynchronizationWatcher* _synchronizationWatcher;
    std::string _assetRootDirectory;
    ghoul::lua::LuaState* _luaState;

    // State change listeners
    std::vector<AssetListener*> _assetListeners;

    // References to lua values
    std::unordered_map<Asset*, std::vector<int>> _onInitializationFunctionRefs;
    std::unordered_map<Asset*, std::vector<int>> _onDeinitializationFunctionRefs;
    std::unordered_map<Asset*, std::map<Asset*, std::vector<int>>>
        _onDependencyInitializationFunctionRefs;
    std::unordered_map<Asset*, std::map<Asset*, std::vector<int>>>
        _onDependencyDeinitializationFunctionRefs;
    int _assetsTableRef;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETLOADER___H__
