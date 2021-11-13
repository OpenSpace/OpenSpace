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
#include <openspace/engine/globals.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/profile.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>

#include "assetmanager_lua.inl"

namespace {
    constexpr const char* _loggerCat = "AssetLoader";

    constexpr const char* AssetGlobalVariableName = "asset";

    constexpr const char* ExportsTableName = "_exports";
    constexpr const char* AssetTableName = "_asset";

    constexpr const char* AssetFileSuffix = "asset";
    constexpr const char* SceneFileSuffix = "scene";

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

    std::filesystem::path generateAssetPath(const std::filesystem::path& baseDirectory,
                                            const std::string& rootDirectory,
                                            const std::string& assetPath)
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
            prefix = rootDirectory + '/';
        }

        // Construct the full path including the .asset extension
        std::string assetSuffix = std::string(".") + AssetFileSuffix;
        const bool hasAssetSuffix =
            (assetPath.size() > assetSuffix.size()) &&
            (assetPath.substr(assetPath.size() - assetSuffix.size()) == assetSuffix);
        std::string fullAssetPath =
            (pathType == PathType::Tokenized) ?
            absPath(assetPath).string() :
            prefix + assetPath;
        if (!hasAssetSuffix) {
            fullAssetPath += assetSuffix;
        }

        // We don't check whether the file exists here as the error will be more
        // comprehensively logged by Lua either way
        return absPath(fullAssetPath);
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

AssetManager::AssetManager(ghoul::lua::LuaState* state, std::string assetRootDirectory)
    : _rootAsset(std::make_shared<Asset>(this, &_synchronizationWatcher))
    , _assetRootDirectory(std::move(assetRootDirectory))
    , _luaState(state)
{
    setCurrentAsset(_rootAsset.get());

    // Create _assets table
    lua_newtable(*_luaState);
    _assetsTableRef = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
}

AssetManager::~AssetManager() {
    _trackedAssets.clear();
    _currentAsset = nullptr;
    _rootAsset = nullptr;
    luaL_unref(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
}

void AssetManager::initialize() {
    ZoneScoped

    _rootAsset->initialize();
}

void AssetManager::deinitialize() {
    ZoneScoped

    _rootAsset->deinitialize();
    _rootAsset->unload();
}

void AssetManager::update() {
    ZoneScoped

    // Add assets
    for (const std::string& asset : _assetAddQueue) {
        ZoneScopedN("(add) Pending State Change")
        setCurrentAsset(_rootAsset.get());
        Asset* a = retrieveAsset(asset);
        _currentAsset->request(a);
        //_currentAsset->require(a);
        global::profile->addAsset(asset);
    }
    _assetAddQueue.clear();

    // Remove assets
    for (const std::string& asset : _assetRemoveQueue) {
        ZoneScopedN("(remove) Pending State change")
        std::filesystem::path directory = currentDirectory();
        std::filesystem::path path = generateAssetPath(
            directory,
            _assetRootDirectory,
            asset
        );

        const auto it = _trackedAssets.find(path.string());
        if (it != _trackedAssets.end()) {
            setCurrentAsset(_rootAsset.get());
            _currentAsset->unrequest(it->second.get());
            global::profile->removeAsset(asset);
        }
    }
    _assetRemoveQueue.clear();

    // Change state based on synchronizations
    _synchronizationWatcher.notify();
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
    for (const Asset* a : _rootAsset->requestedAssets()) {
        _assetRemoveQueue.insert(a->assetFilePath().string());
    }
}

std::vector<const Asset*> AssetManager::allAssets() const {
    std::vector<const Asset*> res;
    res.push_back(_rootAsset.get());
    for (const std::pair<const std::string, std::unique_ptr<Asset>>& p : _trackedAssets) {
        res.push_back(p.second.get());
    }
    return res;
}

bool AssetManager::loadAsset(Asset* asset) {
    const int top = lua_gettop(*_luaState);
    Asset* parentAsset = _currentAsset;

    setCurrentAsset(asset);
    defer {
        setCurrentAsset(parentAsset);
    };

    if (!std::filesystem::is_regular_file(asset->assetFilePath())) {
        LERROR(fmt::format(
            "Could not load asset {}: File does not exist", asset->assetFilePath())
        );
        lua_settop(*_luaState, top);
        return false;
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->assetFilePath());
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Could not load asset {}: {}", asset->assetFilePath(), e.message)
        );
        lua_settop(*_luaState, top);
        return false;
    }

    // Extract meta information from the asset file if it was provided
    // 1. Load the asset table
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

    lua_settop(*_luaState, top);
    return true;
}

void AssetManager::unloadAsset(Asset* asset) {
    for (int ref : _onInitializeFunctionRefs[asset]) {
       luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onInitializeFunctionRefs[asset].clear();

    for (int ref : _onDeinitializeFunctionRefs[asset]) {
        luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onDeinitializeFunctionRefs[asset].clear();

    asset->clearSynchronizations();
    tearDownAssetLuaTable(asset);
    const auto it = _trackedAssets.find(asset->id());
    if (it != _trackedAssets.end()) {
        _trackedAssets.erase(it);
    }
}

void AssetManager::setUpAssetLuaTable(Asset* asset) {
    // Set up lua table:
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

    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    // Create a AssetInfo table for the current asset.
    lua_newtable(*_luaState);
    const int assetInfoTableIndex = lua_gettop(*_luaState);

    // Register empty Exports table for the current asset.
    // (string => exported object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetInfoTableIndex, ExportsTableName);

    // Create Asset table
    // (string => lua functions)
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
            ghoul::lua::push(L, fmt::format("{}/{}", asset->assetDirectory(), name));
            return 1;
        },
        1
    );
    lua_setfield(*_luaState, assetTableIndex, "localResource");

    // Register synced resource function
    // string syncedResource(string path)
    ghoul::lua::push(*_luaState, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            Asset* asset = ghoul::lua::userData<Asset>(L, 1);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::syncedResourceLua");

            ghoul::Dictionary d;
            ghoul::lua::luaDictionaryFromState(L, d);
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

            Asset* dependency = manager->retrieveAsset(assetName);
            manager->_currentAsset->require(dependency);

            if (!dependency) {
                return ghoul::lua::luaError(
                    L,
                    fmt::format("Asset '{}' not found", assetName)
                );
            }

            // Get the exports table
            lua_rawgeti(L, LUA_REGISTRYINDEX, manager->_assetsTableRef);
            lua_getfield(L, -1, dependency->id().c_str());
            lua_getfield(L, -1, ExportsTableName);
            return 1;
        },
        1
    );
    lua_setfield(*_luaState, assetTableIndex, "require");

    // Register exists function
    // bool exists(string path)
    ghoul::lua::push(*_luaState, this);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped
                
            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::exists");
            const std::string name = ghoul::lua::value<std::string>(L);

            const std::filesystem::path directory = manager->currentDirectory();
            std::filesystem::path path = generateAssetPath(
                directory,
                manager->_assetRootDirectory,
                name
            );

            ghoul::lua::push(L, std::filesystem::is_regular_file(path));
            return 1;
        },
        1
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
            lua_getfield(L, -1, asset->id().c_str());
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
    ghoul::lua::push(*_luaState, asset->assetDirectory());
    lua_setfield(*_luaState, assetTableIndex, "directory");

    // Register filePath constant
    // string filePath
    ghoul::lua::push(*_luaState, asset->assetFilePath());
    lua_setfield(*_luaState, assetTableIndex, "filePath");

    // Attach Asset table to AssetInfo table
    lua_setfield(*_luaState, assetInfoTableIndex, AssetTableName);

    // Extend global asset info table (pushed to the lua stack earlier)
    // with this AssetInfo table
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
    lua_settop(*_luaState, top);
}

void AssetManager::tearDownAssetLuaTable(Asset* asset) {
    const int top = lua_gettop(*_luaState);
    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    ghoul::lua::push(*_luaState, ghoul::lua::nil_t());

    // Clear entry from global asset table (pushed to the lua stack earlier)
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
    lua_settop(*_luaState, top);
}

Asset* AssetManager::retrieveAsset(const std::string& name) {
    std::filesystem::path directory = currentDirectory();
    std::filesystem::path path = generateAssetPath(directory, _assetRootDirectory, name);

    // Check if asset is already loaded.
    const auto it = _trackedAssets.find(path.string());
    if (it != _trackedAssets.end()) {
        return it->second.get();
    }

    std::unique_ptr<Asset> asset = std::make_unique<Asset>(
        this,
        &_synchronizationWatcher,
        path.string()
    );
    Asset* res = asset.get();

    _trackedAssets.emplace(asset->id(), std::move(asset));
    setUpAssetLuaTable(res);
    return res;
}

std::filesystem::path AssetManager::currentDirectory() const {
    if (_currentAsset->hasAssetFile()) {
        return _currentAsset->assetDirectory();
    }
    else {
        return _assetRootDirectory;
    }
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
                asset->assetFilePath(),
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            ));
        }
        // Clean up lua stack, in case the pcall left anything there.
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
                asset->assetFilePath(),
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            ));
        }
        // Clean up lua stack, in case the pcall left anything there.
        lua_settop(*_luaState, 0);
    }
}

void AssetManager::setCurrentAsset(Asset* asset) {
    ZoneScoped
    ghoul_precondition(asset, "Asset must not be nullptr");

    const int top = lua_gettop(*_luaState);

    _currentAsset = asset;
    // Set `asset` lua global to point to the current asset table

    if (asset == _rootAsset.get()) {
        ghoul::lua::push(*_luaState, ghoul::lua::nil_t());
        lua_setglobal(*_luaState, AssetGlobalVariableName);
        lua_settop(*_luaState, top);
        return;
    }

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, AssetTableName);
    lua_setglobal(*_luaState, AssetGlobalVariableName);

    lua_settop(*_luaState, top);
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
