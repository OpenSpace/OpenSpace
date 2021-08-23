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

#include <openspace/scene/assetloader.h>

#include <openspace/scene/assetlistener.h>
#include <openspace/util/resourcesynchronization.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/profiling.h>

#include "assetloader_lua.inl"

namespace {
    constexpr const char* AssetGlobalVariableName = "asset";

    constexpr const char* RequireFunctionName = "require";
    constexpr const char* ExistsFunctionName = "exists";
    constexpr const char* ExportFunctionName = "export";

    constexpr const char* SyncedResourceFunctionName = "syncedResource";
    constexpr const char* LocalResourceFunctionName = "localResource";

    constexpr const char* OnInitializeFunctionName = "onInitialize";
    constexpr const char* OnDeinitializeFunctionName = "onDeinitialize";

    constexpr const char* DirectoryConstantName = "directory";
    constexpr const char* FilePathConstantName = "filePath";

    constexpr const char* MetaInformationKey = "meta";
    constexpr const char* MetaInformationName = "Name";
    constexpr const char* MetaInformationVersion = "Version";
    constexpr const char* MetaInformationDescription = "Description";
    constexpr const char* MetaInformationAuthor = "Author";
    constexpr const char* MetaInformationURL = "URL";
    constexpr const char* MetaInformationLicense = "License";
    constexpr const char* MetaInformationIdentifiers = "Identifiers";

    constexpr const char* ExportsTableName = "_exports";
    constexpr const char* AssetTableName = "_asset";
    constexpr const char* DependantsTableName = "_dependants";

    constexpr const char* _loggerCat = "AssetLoader";

    constexpr const char* AssetFileSuffix = "asset";
    constexpr const char* SceneFileSuffix = "scene";

    enum class PathType {
        RelativeToAsset = 0,
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
} // namespace

namespace openspace {

AssetLoader::AssetLoader(ghoul::lua::LuaState* luaState,
                         SynchronizationWatcher* syncWatcher,
                         std::string assetRootDirectory)
    : _rootAsset(std::make_shared<Asset>(this, syncWatcher))
    , _synchronizationWatcher(syncWatcher)
    , _assetRootDirectory(std::move(assetRootDirectory))
    , _luaState(luaState)
{
    setCurrentAsset(_rootAsset.get());

    // Create _assets table
    lua_newtable(*_luaState);
    _assetsTableRef = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
}

AssetLoader::~AssetLoader() {
    _currentAsset = nullptr;
    _rootAsset = nullptr;
    luaL_unref(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
}

void AssetLoader::trackAsset(std::shared_ptr<Asset> asset) {
    _trackedAssets.emplace(asset->id(), asset);
    setUpAssetLuaTable(asset.get());
}

void AssetLoader::untrackAsset(Asset* asset) {
    tearDownAssetLuaTable(asset);
    const auto it = _trackedAssets.find(asset->id());
    if (it != _trackedAssets.end()) {
        _trackedAssets.erase(it);
    }
}

void AssetLoader::setUpAssetLuaTable(Asset* asset) {
    // Set up lua table:
    // AssetInfo
    // |- Exports (table<name, exported data>)
    // |- Asset
    // |  |- localResource
    // |  |- syncedResource
    // |  |- require
    // |  |- request
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
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::localResource, 1);
    lua_setfield(*_luaState, assetTableIndex, LocalResourceFunctionName);

    // Register synced resource function
    // string syncedResource(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::syncedResource, 1);
    lua_setfield(*_luaState, assetTableIndex, SyncedResourceFunctionName);

    // Register require function
    // Asset, Dependency require(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::require, 1);
    lua_setfield(*_luaState, assetTableIndex, RequireFunctionName);

    // Register exists function
    // bool exists(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::exists, 1);
    lua_setfield(*_luaState, assetTableIndex, ExistsFunctionName);

    // Register export-dependency function
    // export(string key, any value)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::exportAsset, 1);
    lua_setfield(*_luaState, assetTableIndex, ExportFunctionName);

    // Register onInitialize function
    // void onInitialize(function<void()> initializationFunction)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::onInitialize, 1);
    lua_setfield(*_luaState, assetTableIndex, OnInitializeFunctionName);

    // Register onDeinitialize function
    // void onDeinitialize(function<void()> deinitializationFunction)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::onDeinitialize, 1);
    lua_setfield(*_luaState, assetTableIndex, OnDeinitializeFunctionName);

    // Register directory constant
    // string directory
    lua_pushstring(*_luaState, asset->assetDirectory().c_str());
    lua_setfield(*_luaState, assetTableIndex, DirectoryConstantName);

    // Register filePath constant
    // string filePath
    lua_pushstring(*_luaState, asset->assetFilePath().c_str());
    lua_setfield(*_luaState, assetTableIndex, FilePathConstantName);

    // Attach Asset table to AssetInfo table
    lua_setfield(*_luaState, assetInfoTableIndex, AssetTableName);

    // Register empty dependant table on AssetInfo table.
    // (importer => dependant object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetInfoTableIndex, DependantsTableName);

    // Extend global asset info table (pushed to the lua stack earlier)
    // with this AssetInfo table
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
    lua_settop(*_luaState, top);
}

void AssetLoader::tearDownAssetLuaTable(Asset* asset) {
    const int top = lua_gettop(*_luaState);
    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    lua_pushnil(*_luaState);

    // Clear entry from global asset table (pushed to the lua stack earlier)
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
    lua_settop(*_luaState, top);
}

bool AssetLoader::loadAsset(Asset* asset) {
    const int top = lua_gettop(*_luaState);
    Asset* parentAsset = _currentAsset;

    setCurrentAsset(asset);
    defer {
        setCurrentAsset(parentAsset);
    };

    if (!std::filesystem::is_regular_file(asset->assetFilePath())) {
        LERROR(fmt::format(
            "Could not load asset '{}': File does not exist", asset->assetFilePath())
        );
        lua_settop(*_luaState, top);
        return false;
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->assetFilePath());
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Could not load asset '{}': {}", asset->assetFilePath(), e.message)
        );
        lua_settop(*_luaState, top);
        return false;
    }

    // Extract meta information from the asset file if it was provided
    // 1. Load the asset table
    lua_getglobal(*_luaState, AssetGlobalVariableName);
    ghoul_assert(lua_istable(*_luaState, -1), "Expected 'asset' table");
    lua_getfield(*_luaState, -1, MetaInformationKey);
    if (!lua_isnil(*_luaState, -1)) {
        // The 'meta' object exist;  quick sanity check that it is a table
        if (!lua_istable(*_luaState, -1)) {
            LWARNING(fmt::format(
                "When loading asset '{}', encountered a '{}' entry that was not a table",
                asset->assetFilePath(), MetaInformationKey
            ));
        }
        else {
            // The 'meta' object exists and it is a table
            ghoul::Dictionary metaDict;
            ghoul::lua::luaDictionaryFromState(*_luaState, metaDict);

            Asset::MetaInformation meta;
            if (metaDict.hasValue<std::string>(MetaInformationName)) {
                meta.name = metaDict.value<std::string>(MetaInformationName);

            }
            if (metaDict.hasValue<std::string>(MetaInformationVersion)) {
                meta.version = metaDict.value<std::string>(MetaInformationVersion);

            }
            if (metaDict.hasValue<std::string>(MetaInformationDescription)) {
                meta.description =
                    metaDict.value<std::string>(MetaInformationDescription);

            }
            if (metaDict.hasValue<std::string>(MetaInformationAuthor)) {
                meta.author = metaDict.value<std::string>(MetaInformationAuthor);

            }
            if (metaDict.hasValue<std::string>(MetaInformationURL)) {
                meta.url = metaDict.value<std::string>(MetaInformationURL);
            }
            if (metaDict.hasValue<std::string>(MetaInformationLicense)) {
                meta.license = metaDict.value<std::string>(MetaInformationLicense);
            }
            if (metaDict.hasValue<ghoul::Dictionary>(MetaInformationIdentifiers)) {
                ghoul::Dictionary iddict =
                    metaDict.value<ghoul::Dictionary>(MetaInformationIdentifiers);
                for (size_t i = 1; i <= iddict.size(); ++i) {
                    std::string key = std::to_string(i);
                    std::string identifier = iddict.value<std::string>(key);
                    meta.identifiers.push_back(identifier);
                }
            }
            asset->setMetaInformation(std::move(meta));
        }
    }

    lua_settop(*_luaState, top);
    return true;
}

void AssetLoader::unloadAsset(Asset* asset) {
    for (int ref : _onInitializationFunctionRefs[asset]) {
       luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onInitializationFunctionRefs[asset].clear();

    for (int ref : _onDeinitializationFunctionRefs[asset]) {
        luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onDeinitializationFunctionRefs[asset].clear();

    for (std::pair<Asset*, std::vector<int>> it :
         _onDependencyInitializationFunctionRefs[asset])
    {
        for (int ref : it.second) {
            luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
        }
    }
    _onDependencyInitializationFunctionRefs.erase(asset);

    for (std::pair<Asset*, std::vector<int>> it :
         _onDependencyDeinitializationFunctionRefs[asset])
    {
        for (int ref : it.second) {
            luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
        }
    }
    _onDependencyDeinitializationFunctionRefs.erase(asset);

    asset->clearSynchronizations();
    untrackAsset(asset);
}

std::string AssetLoader::generateAssetPath(const std::string& baseDirectory,
                                           const std::string& assetPath) const
{
    // Support paths that are
    // 1) Relative to baseDirectory (./* or ../*)
    // 3) Absolute paths (*:/* or /*)
    // 2) Relative to the global asset root (*)

    PathType pathType = classifyPath(assetPath);
    std::string prefix;
    if (pathType == PathType::RelativeToAsset) {
        prefix = baseDirectory + '/';
    }
    else if (pathType == PathType::RelativeToAssetRoot) {
        prefix = _assetRootDirectory + '/';
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
    bool fullAssetPathExists = std::filesystem::is_regular_file(absPath(fullAssetPath));

    // Construct the full path including the .scene extension
    const std::string sceneSuffix = std::string(".") + SceneFileSuffix;
    const bool hasSceneSuffix =
        (assetPath.size() > sceneSuffix.size()) &&
        (assetPath.substr(assetPath.size() - sceneSuffix.size()) == sceneSuffix);
    const std::string fullScenePath =
        hasSceneSuffix ?
        prefix + assetPath :
        prefix + assetPath + sceneSuffix;
    const bool fullScenePathExists =
        std::filesystem::is_regular_file(absPath(fullScenePath));

    if (fullAssetPathExists && fullScenePathExists) {
        LWARNING(fmt::format(
            "'{}' and '{}' file found with non-specific request '{}'. Loading '{}'. "
            "Explicitly add extension to suppress this warning.",
            fullAssetPath, fullScenePath, prefix + assetPath, fullAssetPath
        ));

        return absPath(fullAssetPath).string();
    }

    if (fullScenePathExists) {
        return absPath(fullScenePath).string();
    }

    // We don't check whether the file exists here as the error will be more
    // comprehensively logged by Lua either way
    return absPath(fullAssetPath).string();
}

std::shared_ptr<Asset> AssetLoader::getAsset(const std::string& name) {
    std::filesystem::path directory = currentDirectory();
    const std::string path = generateAssetPath(directory.string(), name);

    // Check if asset is already loaded.
    const auto it = _trackedAssets.find(path);

    if (it != _trackedAssets.end()) {
        if (std::shared_ptr<Asset> a = it->second.lock(); a != nullptr) {
            return a;
        }
    }

    std::shared_ptr<Asset> asset = std::make_shared<Asset>(
        this,
        _synchronizationWatcher,
        path
    );

    trackAsset(asset);
    return asset;
}

int AssetLoader::onInitializeLua(Asset* asset) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::onInitialize");

    const int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onInitializationFunctionRefs[asset].push_back(referenceIndex);

    lua_settop(*_luaState, 0);
    return 0;
}

int AssetLoader::onDeinitializeLua(Asset* asset) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::onDeinitialize");

    const int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDeinitializationFunctionRefs[asset].push_back(referenceIndex);

    lua_settop(*_luaState, 0);
    return 0;
}

int AssetLoader::onInitializeDependencyLua(Asset* dependant, Asset* dependency) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::onInitializeDependency");

    const int refIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDependencyInitializationFunctionRefs[dependant][dependency].push_back(refIndex);

    lua_settop(*_luaState, 0);
    return 0;
}

int AssetLoader::onDeinitializeDependencyLua(Asset* dependant, Asset* dependency) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::onDeinitializeDependency");

    const int refIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDependencyDeinitializationFunctionRefs[dependant][dependency].push_back(refIndex);

    lua_settop(*_luaState, 0);
    return 0;
}

void AssetLoader::unrequest(const std::string& identifier) {
    std::shared_ptr<Asset> asset = has(identifier);
    Asset* parent = _currentAsset;
    parent->unrequest(asset.get());
    assetUnrequested(parent, asset);
}

std::filesystem::path AssetLoader::currentDirectory() const {
    if (_currentAsset->hasAssetFile()) {
        return _currentAsset->assetDirectory();
    }
    else {
        return _assetRootDirectory;
    }
}

std::shared_ptr<Asset> AssetLoader::add(const std::string& identifier) {
    ZoneScoped

    setCurrentAsset(_rootAsset.get());
    std::shared_ptr<Asset> asset = getAsset(identifier);
    Asset* parent = _currentAsset;
    parent->request(asset);
    assetRequested(parent, asset);
    return asset;
}

void AssetLoader::remove(const std::string& identifier) {
    ZoneScoped

    setCurrentAsset(_rootAsset.get());
    unrequest(identifier);
}

std::shared_ptr<Asset> AssetLoader::has(const std::string& identifier) const {
    std::filesystem::path directory = currentDirectory();
    std::string path = generateAssetPath(directory.string(), identifier);

    const auto it = _trackedAssets.find(path);
    if (it == _trackedAssets.end()) {
        return nullptr;
    }
    return it->second.lock();
}

const Asset& AssetLoader::rootAsset() const {
    return *_rootAsset;
}

Asset& AssetLoader::rootAsset() {
    return *_rootAsset;
}

const std::string& AssetLoader::assetRootDirectory() const {
    return _assetRootDirectory;
}

void AssetLoader::callOnInitialize(Asset* asset) {
    ZoneScoped

    for (int init : _onInitializationFunctionRefs[asset]) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When initializing " + asset->assetFilePath() + ": " +
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            );
        }
        // Clean up lua stack, in case the pcall left anything there.
        lua_settop(*_luaState, 0);
    }
}

void AssetLoader::callOnDeinitialize(Asset* asset) {
    ZoneScoped

    const std::vector<int>& funs = _onDeinitializationFunctionRefs[asset];
    for (auto it = funs.rbegin(); it != funs.rend(); it++) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, *it);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When deinitializing " + asset->assetFilePath() + ": " +
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            );
        }
        // Clean up lua stack, in case the pcall left anything there.
        lua_settop(*_luaState, 0);
    }
}

void AssetLoader::callOnDependencyInitialize(Asset* asset, Asset* dependant) {
    ZoneScoped

    for (int init : _onDependencyInitializationFunctionRefs[dependant][asset]) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When initializing dependency " + dependant->assetFilePath() + " -> " +
                asset->assetFilePath() + ": " +
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            );
        }
        // Clean up lua stack, in case the pcall left anything there.
        lua_settop(*_luaState, 0);
    }
    // Potential Todo:
    // Call dependency->onInitialize with the asset table
    // exported by the child asset as argument
}

void AssetLoader::callOnDependencyDeinitialize(Asset* asset, Asset* dependant) {
    ZoneScoped

    const std::vector<int>& funs =
        _onDependencyDeinitializationFunctionRefs[dependant][asset];

    for (auto it = funs.rbegin(); it != funs.rend(); it++) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, *it);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When deinitializing dependency " + dependant->assetFilePath() + " -> " +
                asset->assetFilePath() + ": " +
                ghoul::lua::value<std::string>(*_luaState, -1, ghoul::lua::PopValue::Yes)
            );
        }
        // Clean up lua stack, in case the pcall left anything there.
        lua_settop(*_luaState, 0);
    }
}

int AssetLoader::localResourceLua(Asset* asset) {
    ZoneScoped

    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::localResourceLua");

    std::string name = ghoul::lua::value<std::string>(
        *_luaState,
        1,
        ghoul::lua::PopValue::Yes
    );

    const std::string resolvedName = fmt::format("{}/{}", asset->assetDirectory(), name);

    lua_pushstring(*_luaState, resolvedName.c_str());

    ghoul_assert(lua_gettop(*_luaState) == 1, "Incorrect number of items left on stack");
    return 1;
}

int AssetLoader::syncedResourceLua(Asset* asset) {
    ZoneScoped

    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::syncedResourceLua");

    ghoul::Dictionary d;
    ghoul::lua::luaDictionaryFromState(*_luaState, d);

    std::unique_ptr<ResourceSynchronization> sync =
        ResourceSynchronization::createFromDictionary(d);

    const std::string absolutePath = sync->directory();

    asset->addSynchronization(std::move(sync));

    lua_settop(*_luaState, 0);
    lua_pushstring(*_luaState, absolutePath.c_str());

    ghoul_assert(lua_gettop(*_luaState) == 1, "Incorrect number of items left on stack");
    return 1;
}

void AssetLoader::setCurrentAsset(Asset* asset) {
    ZoneScoped

    const int top = lua_gettop(*_luaState);

    _currentAsset = asset;
    // Set `asset` lua global to point to the current asset table

    if (asset == _rootAsset.get()) {
        lua_pushnil(*_luaState);
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

int AssetLoader::requireLua(Asset* dependant) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::require");

    std::string assetName = luaL_checkstring(*_luaState, 1);
    lua_settop(*_luaState, 0);

    std::shared_ptr<Asset> dependency = getAsset(assetName);
    _currentAsset->require(dependency);

    if (!dependency) {
        return ghoul::lua::luaError(
            *_luaState,
            fmt::format("Asset '{}' not found", assetName)
        );
    }

    addLuaDependencyTable(dependant, dependency.get());

    // Get the exports table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependency->id().c_str());
    lua_getfield(*_luaState, -1, ExportsTableName);
    const int exportsTableIndex = lua_gettop(*_luaState);

    // Get the dependency table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependency->id().c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    lua_getfield(*_luaState, -1, dependant->id().c_str());
    const int dependencyTableIndex = lua_gettop(*_luaState);

    lua_pushvalue(*_luaState, exportsTableIndex);
    lua_pushvalue(*_luaState, dependencyTableIndex);

    lua_replace(*_luaState, 2);
    lua_replace(*_luaState, 1);
    lua_settop(*_luaState, 2);

    ghoul_assert(lua_gettop(*_luaState) == 2, "Incorrect number of items left on stack");
    return 2;
}

int AssetLoader::existsLua(Asset*) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 1, "lua::exists");

    const std::string assetName = luaL_checkstring(*_luaState, 1);

    const std::filesystem::path directory = currentDirectory();
    const std::string path = generateAssetPath(directory.string(), assetName);

    lua_settop(*_luaState, 0);
    lua_pushboolean(*_luaState, std::filesystem::is_regular_file(path));
    ghoul_assert(lua_gettop(*_luaState) == 1, "Incorrect number of items left on stack");
    return 1;
}

int AssetLoader::exportAssetLua(Asset* asset) {
    ghoul::lua::checkArgumentsAndThrow(*_luaState, 2, "lua::exportAsset");

    const std::string exportName = luaL_checkstring(*_luaState, 1);

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, ExportsTableName);
    const int exportsTableIndex = lua_gettop(*_luaState);

    // push the second argument
    lua_pushvalue(*_luaState, 2);
    lua_setfield(*_luaState, exportsTableIndex, exportName.c_str());

    lua_settop(*_luaState, 0);
    ghoul_assert(lua_gettop(*_luaState) == 0, "Incorrect number of items left on stack");
    return 0;
}

void AssetLoader::addLuaDependencyTable(Asset* dependant, Asset* dependency) {
    const int top = lua_gettop(*_luaState);

    const std::string dependantId = dependant->id();
    const std::string dependencyId = dependency->id();

    // Extract the imported asset's dependants table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependencyId.c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    const int dependantsTableIndex = lua_gettop(*_luaState);

    // Set up Dependency object
    lua_newtable(*_luaState);
    const int currentDependantTableIndex = lua_gettop(*_luaState);

    // Register onDependencyInitialize function
    // void onInitialize(function<void()> initializationFunction)
    lua_pushlightuserdata(*_luaState, dependant);
    lua_pushlightuserdata(*_luaState, dependency);
    lua_pushcclosure(*_luaState, &assetloader::onInitializeDependency, 2);
    lua_setfield(*_luaState, currentDependantTableIndex, OnInitializeFunctionName);

    // Register onDependencyDeinitialize function
    // void onDeinitialize(function<void()> deinitializationFunction)
    lua_pushlightuserdata(*_luaState, dependant);
    lua_pushlightuserdata(*_luaState, dependency);
    lua_pushcclosure(*_luaState, &assetloader::onDeinitializeDependency, 2);
    lua_setfield(*_luaState, currentDependantTableIndex, OnDeinitializeFunctionName);

    // Duplicate the table reference on the stack, so it remains after assignment.
    lua_pushvalue(*_luaState, -1);

    // Register the dependant table on the imported asset's dependants table.
    lua_setfield(*_luaState, dependantsTableIndex, dependantId.c_str());

    lua_settop(*_luaState, top);
}

void AssetLoader::addAssetListener(AssetListener* listener) {
    const auto it = std::find(_assetListeners.cbegin(), _assetListeners.cend(), listener);

    if (it == _assetListeners.cend()) {
        _assetListeners.push_back(listener);
    }
}

void AssetLoader::removeAssetListener(AssetListener* listener) {
    _assetListeners.erase(std::remove(
        _assetListeners.begin(),
        _assetListeners.end(),
        listener
    ));
}

void AssetLoader::assetStateChanged(Asset* asset, Asset::State state) {
    for (AssetListener* listener : _assetListeners) {
        listener->assetStateChanged(asset, state);
    }
}

void AssetLoader::assetRequested(Asset* parent, std::shared_ptr<Asset> child) {
    for (AssetListener* listener : _assetListeners) {
        listener->assetRequested(parent, child);
    }
}

void AssetLoader::assetUnrequested(Asset* parent, std::shared_ptr<Asset> child) {
    for (AssetListener* listener : _assetListeners) {
        listener->assetUnrequested(parent, child);
    }
}

} // namespace openspace
