/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___ASSETMANAGER___H__
#define __OPENSPACE_CORE___ASSETMANAGER___H__

#include <ghoul/lua/luastate.h>
#include <filesystem>
#include <optional>
#include <unordered_map>
#include <list>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Asset;
class ResourceSynchronization;

/**
 * The AssetManager class manages the loading, initialization, and unloading of all assets
 * loaded in OpenSpace. Most actions of this class are operating in a two-step process
 * where first the intent of an action is registered, which is then executed in the next
 * call to the #update function.
 *
 * All assets are loading through the same Lua state.
 */
class AssetManager {
public:
    AssetManager(ghoul::lua::LuaState* state, std::filesystem::path assetRootDirectory);
    ~AssetManager();

    void deinitialize();

    /**
     * Loads the asset at the provided \p path as a new root asset of the AssetManager.
     * If the asset at that path was already loaded, nothing happens.
     *
     * \param path The path from which the Asset is loaded. This path can be either
     *        relative to the base directory (the path starting with . or ..), an absolute
     *        path (that path starting with *:/ or /) or relative to the global asset root
     *        (if the path starts any other way)
     * \pre \p path must not be the empty string
     */
    void add(const std::string& path);

    /**
     * Removes the asset at the provided \p path if it has been loaded by this
     * AssetManager. If the asset at that path was not found, nothing happens.
     *
     * \param path The path from which the Asset is loaded. This path can be either
     *        relative to the base directory (the path starting with . or ..), an absolute
     *        path (that path starting with *:/ or /) or relative to the global asset root
     *        (if the path starts any other way)
     * \pre \p path must not be the empty string
     */
    void remove(const std::string& path);

    /**
     * Reloads the asset at the provided \p path as a new root asset of the AssetManager.
     * If the asset at that path was not previously loaded, acts the same as the add
     * function.
     *
     * \param path The path from which the Asset is loaded. This path can be either
     *        relative to the base directory (the path starting with . or ..), an absolute
     *        path (that path starting with *:/ or /) or relative to the global asset root
     *        (if the path starts any other way)
     * \pre \p path must not be the empty string
     */
    void reload(const std::string& path);

    /**
     * Update function that should be called at least once per frame that will load all
     * queued asset loads and asset removal.
     */
    void update();

    static scripting::LuaLibrary luaLibrary();

    /**
     * Returns all assets that have been loaded by the AssetManager. The order of the
     * assets is undefined.
     *
     * \return A list of all assets that have been previously loaded by the AssetManager
     */
    std::vector<const Asset*> allAssets() const;

    /**
     * Returns all root assets, which are assets that have been loaded directly from the
     * profile or by calling the #add method.
     *
     * \return A list of all root assets
     */
    std::vector<const Asset*> rootAssets() const;

    std::vector<const ResourceSynchronization*> allSynchronizations() const;

    /**
     * Returns whether the provided \p asset has been loaded directly by the user or
     * loaded through a profile file.
     *
     * \param asset The asset that should be tested
     * \return Whether the \p asset has been loaded directly or included in a profile
     */
    bool isRootAsset(const Asset* asset) const;

    /**
     * Loads the provided \p asset as a child of the provided \p parent. Loading an asset
     * means that asset file gets executed and the meta information is extracted from it.
     * The \p parent is the asset file that caused this loading to happen and can be a
     * `nullptr` if the asset is to be loaded as a root asset.
     *
     * \param asset The asset that should be loaded
     * \param parent The parent of the loaded asset file or `nullptr` if the asset is a
     *        root asset
     *
     * \pre \p asset must not be a nullptr
     */
    bool loadAsset(Asset* asset, Asset* parent);

    /**
     * Unload the provided \p asset by removing all information about it from the Lua
     * state and placing the asset onto the deletion queue. Please note that the asset
     * will not actually get destroyed until the next #update call to the AssetManager.
     *
     * \param asset The asset that should get unloaded
     *
     * \pre \p asset must not be a nullptr
     */
    void unloadAsset(Asset* asset);

    /**
     * This function calls the `onInitialize` function that was specified in the file of
     * the provided \p asset.
     *
     * \param asset The asset file whose `onInitialize` function should be called
     */
    void callOnInitialize(Asset* asset) const;

    /**
     * This function calls the `onDeinitialize` function that was specified in the file of
     * the provided \p asset.
     *
     * \param asset The asset file whose `onDeinitialize` function should be called
     */
    void callOnDeinitialize(Asset* asset) const;

private:
    /**
     * Creates and registers all of the callback functions that the asset is expected to
     * call in the file, for example the `localResource`, `require`, etc.
     */
    void setUpAssetLuaTable(Asset* asset);

    /**
     * Returns the loaded Asset by either trying to load the asset at the provided path
     * or returning a previously loaded copy.
     */
    Asset* retrieveAsset(const std::filesystem::path& path,
        const std::filesystem::path& retriever,
        std::optional<bool> explicitEnable = std::nullopt);

    /**
     * Setup the asset table of the provided asset in the shared Lua state.
     */
    void setCurrentAsset(Asset* asset);

    /**
     * Takes the asset path, determines the type of path (relative to base, relative to
     * root or absolute and returns fully formed path.
     */
    std::filesystem::path generateAssetPath(const std::filesystem::path& baseDirectory,
        const std::string& assetPath) const;

    void runRemoveQueue();
    void runAddQueue();

    //
    // Assets
    //

    /// The authoritative list of all assets loaded through the AssetManager
    std::vector<std::unique_ptr<Asset>> _assets;

    /// A list of all root assets that have been loaded directly by the `add` function
    std::vector<Asset*> _rootAssets;

    /// This list contains all of the assets that are queued to be loading in the next
    /// update call
    std::list<std::string> _assetAddQueue;

    /// The list contains all of the assets that should be removed in the next update call
    std::list<std::string> _assetRemoveQueue;

    /// This list contains all assets that need to be initialized in the next update call
    std::vector<Asset*> _toBeInitialized;

    /// This list contains all of the assets that will be deleted in the next update call
    std::vector<std::unique_ptr<Asset>> _toBeDeleted;

    //
    // ResourceSynchronizations
    //

    /// Collection that stores the assets that have requested each ResourceSynchronization
    struct SyncItem {
        std::unique_ptr<ResourceSynchronization> synchronization;
        std::vector<Asset*> assets;
    };
    /// Authoritative list over all ResourceSynchronizations that have been requested by
    /// any asset
    std::unordered_map<std::string, std::unique_ptr<SyncItem>> _synchronizations;

    /// The list of ResourceSynchronizations that were not finished in the last update
    /// call
    std::vector<SyncItem*> _unfinishedSynchronizations;

    //
    // Other values
    //

    /// The location of the asset root directory
    std::filesystem::path _assetRootDirectory;

    /// The Lua state that is used for all asset initialization
    ghoul::lua::LuaState* _luaState = nullptr;

    // References to the onInitialize and the onDeinitialize functions for each Asset
    std::unordered_map<Asset*, std::vector<int>> _onInitializeFunctionRefs;
    std::unordered_map<Asset*, std::vector<int>> _onDeinitializeFunctionRefs;

    int _assetsTableRef = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
