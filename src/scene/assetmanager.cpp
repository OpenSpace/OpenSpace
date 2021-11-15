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

#include <openspace/documentation/documentation.h>
#include <openspace/scene/asset.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include "assetmanager_lua.inl"

namespace {
    constexpr const char* _loggerCat = "AssetLoader";

    constexpr const char* AssetGlobalVariableName = "asset";

    constexpr const char* ExportsTableName = "_exports";
    constexpr const char* AssetTableName = "_asset";

    enum class PathType {
        RelativeToAsset,
        RelativeToAssetRoot,
        Absolute,
        Tokenized
    };

    PathType classifyPath(const std::string& path) {
        if (path.size() > 2 && path[0] == '.' && path[1] == '/') {
            return PathType::RelativeToAsset;
        }
        if (path.size() > 3 && path[0] == '.' && path[1] == '.' && path[2] == '/') {
            return PathType::RelativeToAsset;
        }
        if (path.size() > 3 && path[1] == ':' && (path[2] == '\\' || path[2] == '/')) {
            return PathType::Absolute;
        }
        if (path.size() > 3 && path[0] == '$' && path[1] == '{') {
            return PathType::Tokenized;
        }
        if (path.size() > 1 && (path[0] == '\\' || path[0] == '/')) {
            return PathType::Absolute;
        }
        return PathType::RelativeToAssetRoot;
    }

    struct [[codegen::Dictionary(AssetMeta)]] Parameters {
        std::optional<std::string> name;
        std::optional<std::string> version;
        std::optional<std::string> description;
        std::optional<std::string> author;
        std::optional<std::string> url [[codegen::key("URL")]];
        std::optional<std::string> license;
        std::optional<std::vector<std::string>> identifiers;
    };
#include "assetmanager_codegen.cpp"
} // namespace

namespace openspace {

AssetManager::AssetManager(ghoul::lua::LuaState* state,
                           std::filesystem::path assetRootDirectory)
    : _assetRootDirectory(std::move(assetRootDirectory))
    , _luaState(state)
{
    ghoul_precondition(state, "Lua state must not be nullptr");

    // Create _assets table
    lua_newtable(*_luaState);
    _assetsTableRef = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
}

AssetManager::~AssetManager() {
    _assets.clear();
    luaL_unref(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
}

void AssetManager::deinitialize() {
    ZoneScoped

    for (Asset* asset : _rootAssets) {
        asset->deinitializeIfUnwanted();
        asset->unload();
    }
    _toBeDeleted.clear();
}

void AssetManager::update() {
    ZoneScoped

    // Delete all the assets that have been marked for deletion in the previous frame
    _toBeDeleted.clear();

    // Initialize all assets that have been loaded but not yet initialized
    for (auto it = _toBeInitialized.begin(); it != _toBeInitialized.end(); ++it) {
        ZoneScopedN("Initializing queued assets")
        Asset* a = *it;
        if (a->isSynchronized() && !a->isInitialized()) {
            a->initialize();

            // We are only doing one asset per frame to keep the loading screen working a
            // bit smoother, so we remove the current one and then break out of the loop
            _toBeInitialized.erase(it);
            break;
        }
    }

    // Add all assets that have been queued for loading since the last `update` call
    for (const std::string& asset : _assetAddQueue) {
        ZoneScopedN("Adding queued assets")

        std::filesystem::path path = generateAssetPath(_assetRootDirectory, asset);
        Asset* a = retrieveAsset(path);

        const auto it = std::find(_rootAssets.begin(), _rootAssets.end(), a);
        if (it != _rootAssets.end()) {
            // Do nothing if the request already exists
            continue;
        }

        _rootAssets.push_back(a);
        if (!a->isLoaded()) {
            bool success = a->load(nullptr);
            if (!success) {
                continue;
            }
        }

        if (!a->isSynchronized()) {
            bool success = a->startSynchronizations();
            if (!success) {
                continue;
            }
        }

        _toBeInitialized.push_back(a);
        global::profile->addAsset(asset);
    }
    _assetAddQueue.clear();

    // Remove assets
    for (const std::string& asset : _assetRemoveQueue) {
        ZoneScopedN("Removing queued assets")
        std::filesystem::path path = generateAssetPath(_assetRootDirectory, asset);

        const auto it = std::find_if(
            _assets.begin(),
            _assets.end(),
            [&path](const std::unique_ptr<Asset>& asset) { return asset->path() == path; }
        );
        if (it == _assets.end()) {
            LWARNING(fmt::format("Tried to remove unknown asset {}. Skipping", asset));
            continue;
        }

        Asset* a = it->get();
        auto jt = std::find(_rootAssets.begin(), _rootAssets.end(), a);
        if (jt == _rootAssets.end()) {
            // Tried to remove a non-root asset
            continue;
        }

        _rootAssets.erase(jt);
        a->deinitializeIfUnwanted();
        if (!a->hasLoadedParent()) {
            a->unload();
        }
        global::profile->removeAsset(asset);
    }
    _assetRemoveQueue.clear();

    // Change state based on synchronizations
    _synchronizationWatcher.notify();
}

void AssetManager::add(const std::string& path) {
    ghoul_precondition(!path.empty(), "Path must not be empty");
    // First check if the path is already in the remove queue. If so, remove it from there
    const auto it = _assetRemoveQueue.find(path);
    if (it != _assetRemoveQueue.end()) {
        _assetRemoveQueue.erase(it);
    }

    _assetAddQueue.insert(path);
}

void AssetManager::remove(const std::string& path) {
    ghoul_precondition(!path.empty(), "Path must not be empty");
    
    // First check if the path is already in the add queue. If so, remove it from there
    const auto it = _assetAddQueue.find(path);
    if (it != _assetAddQueue.end()) {
        _assetAddQueue.erase(it);
    }

    _assetRemoveQueue.insert(path);
}

std::vector<const Asset*> AssetManager::allAssets() const {
    std::vector<const Asset*> res;
    res.reserve(_assets.size());
    for (const std::unique_ptr<Asset>& asset : _assets) {
        res.push_back(asset.get());
    }
    return res;
}

bool AssetManager::loadAsset(Asset* asset, Asset* parent) {
    ghoul_precondition(asset, "Asset must not be nullptr");

    const int top = lua_gettop(*_luaState);

    setCurrentAsset(asset);
    defer {
        lua_settop(*_luaState, top);
        setCurrentAsset(parent);
    };

    if (!std::filesystem::is_regular_file(asset->path())) {
        LERROR(fmt::format(
            "Could not load asset {}: File does not exist", asset->path())
        );
        return false;
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->path());
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format("Could not load asset {}: {}", asset->path(), e.message));
        return false;
    }

    // Extract meta information from the asset file if it was provided
    lua_getglobal(*_luaState, AssetGlobalVariableName);
    ghoul_assert(lua_istable(*_luaState, -1), "Expected 'asset' table");
    lua_getfield(*_luaState, -1, "meta");
    ghoul::Dictionary metaDict = ghoul::lua::luaDictionaryFromState(*_luaState);
    if (!metaDict.isEmpty()) {
        Parameters p = codegen::bake<Parameters>(metaDict);

        Asset::MetaInformation meta;
        meta.name = p.name.value_or("");
        meta.version = p.version.value_or("");
        meta.description = p.description.value_or("");
        meta.author = p.author.value_or("");
        meta.url = p.url.value_or("");
        meta.license = p.license.value_or("");
        meta.identifiers = p.identifiers.value_or(std::vector<std::string>());
        asset->setMetaInformation(std::move(meta));
    }

    return true;
}

void AssetManager::unloadAsset(Asset* asset) {
    ghoul_precondition(asset, "Asset must not be nullptr");

    for (int ref : _onInitializeFunctionRefs[asset]) {
       luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onInitializeFunctionRefs[asset].clear();

    for (int ref : _onDeinitializeFunctionRefs[asset]) {
        luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onDeinitializeFunctionRefs[asset].clear();

    asset->clearSynchronizations();

    //
    // Tear down asset lua table
    const int top = lua_gettop(*_luaState);
    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    ghoul::lua::push(*_luaState, ghoul::lua::nil_t());

    // Clear entry from global asset table (pushed to the Lua stack earlier)
    std::string path = asset->path().string();
    lua_setfield(*_luaState, globalTableIndex, path.c_str());
    lua_settop(*_luaState, top);

    
    const auto it = std::find_if(
        _assets.begin(),
        _assets.end(),
        [&asset](const std::unique_ptr<Asset>& a) { return a.get() == asset; }
    );
    if (it != _assets.end()) {
        // Instead of deleting the asset directly, we are moving it into a to-delete queue
        // as this unloadAsset function is called from the asset that we are manipulating
        // right now. And deleting the asset while we are in a function of the same asset
        // might be painful
        _toBeDeleted.push_back(std::move(*it));
        _assets.erase(it);
    }
}

void AssetManager::setUpAssetLuaTable(Asset* asset) {
    // Set up Lua table:
    // AssetInfo
    // |- Exports (table<name, exported data>)
    // |- Asset
    // |  |- localResource
    // |  |- syncedResource
    // |  |- require
    // |  |- exists
    // |  |- export
    // |  |- onInitialize
    // |  |- onDeinitialize
    // |  |- directory
    // |- Dependants (table<dependant, Dependency dep>)
    //
    // where Dependency is a table:
    // Dependency
    // |- onInitialize
    // |- onDeinitialize

    const int top = lua_gettop(*_luaState);

    // Push the global table of AssetInfos
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    // Create a AssetInfo table for the current asset
    lua_newtable(*_luaState);
    const int assetInfoTableIndex = lua_gettop(*_luaState);

    // Register empty Exports table for the current asset
    // (string => exported object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetInfoTableIndex, ExportsTableName);

    // Create Asset table
    // (string => Lua functions)
    lua_newtable(*_luaState);
    const int assetTableIndex = lua_gettop(*_luaState);

    // Register local resource function
    // string localResource(string path)
    ghoul::lua::push(*_luaState, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
            
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::localResourceLua");

            std::string name = ghoul::lua::value<std::string>(L);
            ghoul::lua::push(L, (asset->path().parent_path() / name).string());
            return 1;
        },
        1
    );
    lua_setfield(*_luaState, assetTableIndex, "localResource");

    // Register synced resource function
    // string syncedResource(table)
    ghoul::lua::push(*_luaState, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::syncedResourceLua");

            ghoul::Dictionary d = ghoul::lua::luaDictionaryFromState(L);
            lua_pop(L, 1);

            std::unique_ptr<ResourceSynchronization> sync =
                ResourceSynchronization::createFromDictionary(d);

            const std::string absolutePath = sync->directory();
            asset->addSynchronization(std::move(sync));

            ghoul::lua::push(L, absolutePath);
            return 1;
        },
        1
    );
    lua_setfield(*_luaState, assetTableIndex, "syncedResource");

    // Register require function
    // Asset require(string path)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* asset = ghoul::lua::userData<Asset>(L, 2);

            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::require");
            std::string assetName = ghoul::lua::value<std::string>(L);

            std::filesystem::path path = manager->generateAssetPath(
                asset->path().parent_path(),
                assetName
            );
            Asset* dependency = manager->retrieveAsset(path);
            asset->require(dependency);

            if (!dependency) {
                return ghoul::lua::luaError(
                    L,
                    fmt::format("Asset '{}' not found", assetName)
                );
            }

            // Get the exports table
            lua_rawgeti(L, LUA_REGISTRYINDEX, manager->_assetsTableRef);
            std::string p = dependency->path().string();
            lua_getfield(L, -1, p.c_str());
            lua_getfield(L, -1, ExportsTableName);
            return 1;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "require");

    // Register exists function
    // bool exists(string path)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* asset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::exists");
            const std::string name = ghoul::lua::value<std::string>(L);

            std::filesystem::path path = manager->generateAssetPath(
                asset->path().parent_path(),
                name
            );

            ghoul::lua::push(L, std::filesystem::is_regular_file(path));
            return 1;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "exists");

    // Register export-dependency function
    // export(string key, any value)
    ghoul::lua::push(*_luaState, asset, this);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::exportAsset");

            const std::string exportName = ghoul::lua::value<std::string>(
                L,
                1,
                ghoul::lua::PopValue::No
            );

            lua_rawgeti(L, LUA_REGISTRYINDEX, manager->_assetsTableRef);
            std::string path = asset->path().string();
            lua_getfield(L, -1, path.c_str());
            lua_getfield(L, -1, ExportsTableName);
            const int exportsTableIndex = lua_gettop(L);

            // push the second argument
            lua_pushvalue(L, 2);
            lua_setfield(L, exportsTableIndex, exportName.c_str());

            lua_settop(L, 0);
            return 0;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "export");

    // Register onInitialize function
    // Adds a Lua function to be called upon asset initialization
    // void onInitialize(function<void()> initializationFunction)
    ghoul::lua::push(*_luaState, asset, this);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
            
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::onInitialize");

            const int referenceIndex = luaL_ref(L, LUA_REGISTRYINDEX);
            manager->_onInitializeFunctionRefs[asset].push_back(referenceIndex);
            
            lua_settop(L, 0);
            return 0;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "onInitialize");

    // Register onDeinitialize function
    // Adds a Lua function to be called upon asset deinitialization
    // void onDeinitialize(function<void()> deinitializationFunction)
    ghoul::lua::push(*_luaState, asset, this);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::onDeinitialize");

            const int referenceIndex = luaL_ref(L, LUA_REGISTRYINDEX);
            manager->_onDeinitializeFunctionRefs[asset].push_back(referenceIndex);

            lua_settop(L, 0);
            return 0;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "onDeinitialize");

    // Register directory constant
    // string directory
    ghoul::lua::push(*_luaState, asset->path().parent_path());
    lua_setfield(*_luaState, assetTableIndex, "directory");

    // Register filePath constant
    // string filePath
    ghoul::lua::push(*_luaState, asset->path());
    lua_setfield(*_luaState, assetTableIndex, "filePath");

    // Attach Asset table to AssetInfo table
    lua_setfield(*_luaState, assetInfoTableIndex, AssetTableName);

    // Extend global asset info table (pushed to the Lua stack earlier)
    // with this AssetInfo table
    std::string path = asset->path().string();
    lua_setfield(*_luaState, globalTableIndex, path.c_str());
    lua_settop(*_luaState, top);
}

Asset* AssetManager::retrieveAsset(const std::filesystem::path& path) {
    // Check if asset is already loaded
    const auto it = std::find_if(
        _assets.begin(),
        _assets.end(),
        [&path](const std::unique_ptr<Asset>& asset) { return asset->path() == path; }
    );
    if (it != _assets.end()) {
        return it->get();
    }

    std::unique_ptr<Asset> asset = std::make_unique<Asset>(
        *this,
        _synchronizationWatcher,
        path
    );
    Asset* res = asset.get();

    _assets.push_back(std::move(asset));
    setUpAssetLuaTable(res);
    return res;
}

void AssetManager::callOnInitialize(Asset* asset) const {
    ZoneScoped
    ghoul_precondition(asset, "Asset must not be nullptr");

    auto it = _onInitializeFunctionRefs.find(asset);
    if (it == _onInitializeFunctionRefs.end()) {
        return;
    }

    for (int init : it->second) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(fmt::format(
                "When initializing {}: {}",
                asset->path(),
                ghoul::lua::value<std::string>(*_luaState, -1)
            ));
        }
        // Clean up the stack, in case the pcall left anything there
        lua_settop(*_luaState, 0);
    }
}

void AssetManager::callOnDeinitialize(Asset* asset) const {
    ZoneScoped
    ghoul_precondition(asset, "Asset must not be nullptr");

    auto it = _onDeinitializeFunctionRefs.find(asset);
    if (it == _onDeinitializeFunctionRefs.end()) {
        return;
    }

    for (int deinit : it->second) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, deinit);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(fmt::format(
                "When deinitializing {}: {}",
                asset->path(),
                ghoul::lua::value<std::string>(*_luaState, -1)
            ));
        }
        // Clean up stack, in case the pcall left anything there
        lua_settop(*_luaState, 0);
    }
}

void AssetManager::setCurrentAsset(Asset* asset) {
    const int top = lua_gettop(*_luaState);

    if (asset == nullptr) {
        ghoul::lua::push(*_luaState, ghoul::lua::nil_t());
        lua_setglobal(*_luaState, AssetGlobalVariableName);
        lua_settop(*_luaState, top);
    }
    else {
        // Set `asset` lua global to point to the current asset table
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
        std::string path = asset->path().string();
        lua_getfield(*_luaState, -1, path.c_str());
        lua_getfield(*_luaState, -1, AssetTableName);
        lua_setglobal(*_luaState, AssetGlobalVariableName);
        lua_settop(*_luaState, top);
    }
}

std::filesystem::path AssetManager::generateAssetPath(
                                               const std::filesystem::path& baseDirectory,
                                                       const std::string& assetPath) const
{
    // Support paths that are
    // 1) Relative to baseDirectory (./* or ../*)
    // 3) Absolute paths (*:/* or /*)
    // 2) Relative to the global asset root (*)

    PathType pathType = classifyPath(assetPath);
    std::string prefix;
    if (pathType == PathType::RelativeToAsset) {
        prefix = baseDirectory.string() + '/';
    }
    else if (pathType == PathType::RelativeToAssetRoot) {
        prefix = _assetRootDirectory.string() + '/';
    }

    // Construct the full path including the .asset extension
    std::string fullAssetPath =
        (pathType == PathType::Tokenized) ?
        absPath(assetPath).string() :
        prefix + assetPath;
    
    const bool hasAssetExt = std::filesystem::path(assetPath).extension() == ".asset";
    if (!hasAssetExt) {
        fullAssetPath += ".asset";
    }

    // We don't check whether the file exists here as the error will be more
    // comprehensively logged by Lua either way
    return absPath(fullAssetPath);
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
