/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/scene/asset.h>
#include <openspace/scripting/script_helper.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/filesystem/filesystem.h>

#include <fmt/format.h>

#include "assetloader_lua.inl"

namespace {
    constexpr const char* AssetGlobalVariableName = "asset";

    constexpr const char* RequireFunctionName = "require";
    constexpr const char* RequestFunctionName = "request";
    constexpr const char* ExportFunctionName = "export";

    constexpr const char* SyncedResourceFunctionName = "syncedResource";
    constexpr const char* LocalResourceFunctionName = "localResource";

    constexpr const char* OnInitializeFunctionName = "onInitialize";
    constexpr const char* OnDeinitializeFunctionName = "onDeinitialize";

    constexpr const char* DirectoryConstantName = "directory";

    constexpr const char* ExportsTableName = "_exports";
    constexpr const char* AssetTableName = "_asset";
    constexpr const char* DependantsTableName = "_dependants";

    constexpr const char* _loggerCat = "AssetLoader";

    constexpr const char* AssetFileSuffix = "asset";
    constexpr const char* SceneFileSuffix = "scene";

    enum class PathType : int {
        RelativeToAsset = 0,
        RelativeToAssetRoot,
        Absolute
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
        if (path.size() > 1 && (path[0] == '\\' || path[0] == '/')) {
            return PathType::Absolute;
        }
        return PathType::RelativeToAssetRoot;
    };
}

namespace openspace {

AssetLoader::AssetLoader(
    ghoul::lua::LuaState& luaState,
    SynchronizationWatcher* syncWatcher,
    std::string assetRootDirectory
)
    : _rootAsset(std::make_shared<Asset>(this, syncWatcher))
    , _synchronizationWatcher(syncWatcher)
    , _assetRootDirectory(assetRootDirectory)
    , _luaState(&luaState)
{
    setCurrentAsset(_rootAsset);

    // Create _assets table.
    lua_newtable(*_luaState);
    _assetsTableRef = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
}

AssetLoader::~AssetLoader() {
    _currentAsset = nullptr;
    _rootAsset = nullptr;
}

void AssetLoader::trackAsset(std::shared_ptr<Asset> asset) {
    _trackedAssets.emplace(asset->id(), asset);
    setUpAssetLuaTable(asset.get());
}

void AssetLoader::untrackAsset(Asset* asset) {
    tearDownAssetLuaTable(asset);
    auto it = _trackedAssets.find(asset->id());
    if (it != _trackedAssets.end()) {
        _trackedAssets.erase(it);
    }
}

void AssetLoader::setUpAssetLuaTable(Asset* asset) {
    /*
    Set up lua table:
    AssetInfo
    |- Exports (table<name, exported data>)
    |- Asset
    |  |- localResource
    |  |- syncedResource
    |  |- require
    |  |- request
    |  |- export
    |  |- onInitialize
    |  |- onDeinitialize
    |  |- directory
    |- Dependants (table<dependant, Dependency dep>)

    where Dependency is a table:
    Dependency
    |- onInitialize
    |- onDeinitialize
    */

    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    int globalTableIndex = lua_gettop(*_luaState);

    // Create a AssetInfo table for the current asset.
    lua_newtable(*_luaState);
    int assetInfoTableIndex = lua_gettop(*_luaState);

    // Register empty Exports table for the current asset.
    // (string => exported object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetInfoTableIndex, ExportsTableName);

    // Create Asset table
    // (string => lua functions)
    lua_newtable(*_luaState);
    int assetTableIndex = lua_gettop(*_luaState);

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

    // Register request function
    // Dependency request(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::request, 1);
    lua_setfield(*_luaState, assetTableIndex, RequestFunctionName);

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

    // Attach Asset table to AssetInfo table
    lua_setfield(*_luaState, assetInfoTableIndex, AssetTableName);

    // Register empty dependant table on AssetInfo table.
    // (importer => dependant object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetInfoTableIndex, DependantsTableName);

    // Extend global asset info table (pushed to the lua stack earlier)
    // with this AssetInfo table
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
}

void AssetLoader::tearDownAssetLuaTable(Asset* asset) {
    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    int globalTableIndex = lua_gettop(*_luaState);

    lua_pushnil(*_luaState);

    // Clear entry from global asset table (pushed to the lua stack earlier)
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());
}

bool AssetLoader::loadAsset(std::shared_ptr<Asset> asset) {
    std::shared_ptr<Asset> parentAsset = _currentAsset;

    setCurrentAsset(asset);
    ghoul::OnScopeExit e([this, parentAsset] {
        setCurrentAsset(parentAsset);
    });
    
    if (!FileSys.fileExists(asset->assetFilePath())) {
        LERROR("Could not load asset '" << asset->assetFilePath() <<
               "': File does not exist.");
        return false;
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->assetFilePath());
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR("Could not load asset '" << asset->assetFilePath() << "': " << e.message);
        return false;
    }

    return true;
}

void AssetLoader::unloadAsset(std::shared_ptr<Asset> asset) {
    for (int ref : _onInitializationFunctionRefs[asset.get()]) {
       luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onInitializationFunctionRefs.clear();

    for (int ref : _onDeinitializationFunctionRefs[asset.get()]) {
        luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onDeinitializationFunctionRefs.clear();

    for (const auto& it : _onDependencyInitializationFunctionRefs[asset.get()]) {
        for (int ref : it.second) {
            luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
        }
    }
    _onDependencyInitializationFunctionRefs[asset.get()].clear();

    for (const auto& it : _onDependencyDeinitializationFunctionRefs[asset.get()]) {
        for (int ref : it.second) {
            luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
        }
    }
    _onDependencyDeinitializationFunctionRefs[asset.get()].clear();
}

std::string AssetLoader::generateAssetPath(const std::string& baseDirectory,
                                           const std::string& assetPath) const
{
    // Support paths that are
    // 1) Relative to baseDirectory (./* or ../*)
    // 3) Absolute paths (*:/* or /*)
    // 2) Relative to the global asset root (*)

    PathType pathType = classifyPath(assetPath);
    std::string prefix = "";
    if (pathType == PathType::RelativeToAsset) {
        prefix = baseDirectory + ghoul::filesystem::FileSystem::PathSeparator;
    } else if (pathType == PathType::RelativeToAssetRoot) {
        prefix = _assetRootDirectory + ghoul::filesystem::FileSystem::PathSeparator;
    }

    // Construct the full path including the .asset extension 
    std::string assetSuffix = std::string(".") + AssetFileSuffix;
    bool hasAssetSuffix =
        (assetPath.size() > assetSuffix.size()) &&
        (assetPath.substr(assetPath.size() - assetSuffix.size()) == assetSuffix);
    std::string fullAssetPath =
        hasAssetSuffix ?
        prefix + assetPath :
        prefix + assetPath + assetSuffix;
    bool fullAssetPathExists = FileSys.fileExists(FileSys.absPath(fullAssetPath));

    // Construct the full path including the .scene extension 
    std::string sceneSuffix = std::string(".") + SceneFileSuffix;
    bool hasSceneSuffix =
        (assetPath.size() > sceneSuffix.size()) &&
        (assetPath.substr(assetPath.size() - sceneSuffix.size()) == sceneSuffix);
    std::string fullScenePath = 
        hasSceneSuffix ?
        prefix + assetPath :
        prefix + assetPath + sceneSuffix;
    bool fullScenePathExists = FileSys.fileExists(FileSys.absPath(fullScenePath));

    if (fullAssetPathExists && fullScenePathExists) {
        LWARNING(
            fmt::format(
                "'{}' and '{}' file found with non-specific request '{}'. Loading '{}'. "
                "Explicitly add extension to suppress this warning.",
                fullAssetPath,
                fullScenePath,
                prefix + assetPath,
                fullAssetPath
            )
        );

        return ghoul::filesystem::File(FileSys.absPath(fullAssetPath));
    }

    if (fullScenePathExists) {
        return ghoul::filesystem::File(FileSys.absPath(fullScenePath));
    }

    // We don't check whether the file exists here as the error will be more
    // comprehensively logged by Lua either way
    return ghoul::filesystem::File(FileSys.absPath(fullAssetPath));
}

std::shared_ptr<Asset> AssetLoader::getAsset(std::string name) {
    ghoul::filesystem::Directory directory = currentDirectory();
    std::string path = generateAssetPath(directory, name);

    // Check if asset is already loaded.
    const auto it = _trackedAssets.find(path);

    if (it != _trackedAssets.end()) {
        std::shared_ptr<Asset> a = it->second.lock();
        if (a != nullptr) {
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
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("onInitialize", *_luaState, 1, nArguments);
    int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onInitializationFunctionRefs[asset].push_back(referenceIndex);
    return 0;
}

int AssetLoader::onDeinitializeLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("onDeinitialize", *_luaState, 1, nArguments);
    int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDeinitializationFunctionRefs[asset].push_back(referenceIndex);
    return 0;
}

int AssetLoader::onInitializeDependencyLua(Asset* dependant, Asset* dependency) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("onInitialize", *_luaState, 1, nArguments);
    int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDependencyInitializationFunctionRefs[dependant][dependency]
        .push_back(referenceIndex);

    return 0;
}

int AssetLoader::onDeinitializeDependencyLua(Asset* dependant, Asset* dependency) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("onDeinitialize", *_luaState, 1, nArguments);
    int referenceIndex = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
    _onDependencyDeinitializationFunctionRefs[dependant][dependency]
        .push_back(referenceIndex);

    return 0;
}

std::shared_ptr<Asset> AssetLoader::require(const std::string& name) {
    std::shared_ptr<Asset> asset = getAsset(name);
    std::shared_ptr<Asset> dependant = _currentAsset;
    dependant->require(asset);
    return asset;
}

std::shared_ptr<Asset> AssetLoader::request(const std::string& name) {
    std::shared_ptr<Asset> asset = getAsset(name);
    std::shared_ptr<Asset> parent = _currentAsset;
    parent->request(asset);
    assetRequested(parent, asset);
    return asset;
}

void AssetLoader::unrequest(const std::string& name) {
    std::shared_ptr<Asset> asset = has(name);
    std::shared_ptr<Asset> parent = _currentAsset;
    parent->unrequest(asset);
    assetUnrequested(parent, asset);
}

ghoul::filesystem::Directory AssetLoader::currentDirectory() const {
    if (_currentAsset->hasAssetFile()) {
        return _currentAsset->assetDirectory();
    } else {
        return _assetRootDirectory;
    }
}

std::shared_ptr<Asset> AssetLoader::add(const std::string& identifier) {
    setCurrentAsset(_rootAsset);
    return request(identifier);
}


void AssetLoader::remove(const std::string& identifier) {
    setCurrentAsset(_rootAsset);
    unrequest(identifier);
}

std::shared_ptr<Asset> AssetLoader::has(const std::string& name) const {
    ghoul::filesystem::Directory directory = currentDirectory();
    std::string path = generateAssetPath(directory, name);

    const auto it = _trackedAssets.find(path);
    if (it == _trackedAssets.end()) {
        return nullptr;
    }
    return it->second.lock();
}

ghoul::lua::LuaState* AssetLoader::luaState() {
    return _luaState;
}

std::shared_ptr<Asset> AssetLoader::rootAsset() const {
    return _rootAsset;
}

const std::string& AssetLoader::assetRootDirectory() const {
    return _assetRootDirectory;
}

void AssetLoader::callOnInitialize(Asset* asset) {
    for (int init : _onInitializationFunctionRefs[asset]) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When initializing " + asset->assetFilePath() + ": " +
                luaL_checkstring(*_luaState, -1)
            );
        }
    }
}

void AssetLoader::callOnDeinitialize(Asset * asset) {
    std::vector<int>& funs = _onDeinitializationFunctionRefs[asset];
    for (auto it = funs.rbegin(); it != funs.rend(); it++) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, *it);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When deinitializing " + asset->assetFilePath() + ": " +
                luaL_checkstring(*_luaState, -1)
            );
        }
    }
}

void AssetLoader::callOnDependencyInitialize(Asset* asset, Asset* dependant) {
    for (int init : _onDependencyInitializationFunctionRefs[dependant][asset]) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When initializing dependency " + dependant->assetFilePath() + " -> " + 
                asset->assetFilePath() + ": " + luaL_checkstring(*_luaState, -1)
            );
        }
    }
    // Potential Todo:
    // Call dependency->onInitialize with the asset table
    // exported by the child asset as argument
}

void AssetLoader::callOnDependencyDeinitialize(Asset* asset, Asset* dependant) {
    std::vector<int>& funs = _onDependencyDeinitializationFunctionRefs[dependant][asset];
    for (auto it = funs.rbegin(); it != funs.rend(); it++) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, *it);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(
                "When deinitializing dependency " + dependant->assetFilePath() + " -> " +
                asset->assetFilePath() + ": " + luaL_checkstring(*_luaState, -1)
            );
        }
    }
}

int AssetLoader::localResourceLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("localResource", *_luaState, 1, nArguments);

    std::string resourceName = luaL_checkstring(*_luaState, -1);
    std::string resolved = asset->resolveLocalResource(resourceName);

    lua_pushstring(*_luaState, resolved.c_str());
    return 1;
}

int AssetLoader::syncedResourceLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("syncedResource", *_luaState, 1, nArguments);

    ghoul::Dictionary d;
    ghoul::lua::luaDictionaryFromState(*_luaState, d);

    std::shared_ptr<ResourceSynchronization> sync =
        ResourceSynchronization::createFromDictionary(d);

    std::string absolutePath = sync->directory();

    asset->addSynchronization(sync);

    lua_pushstring(*_luaState, absolutePath.c_str());
    return 1;
}

void AssetLoader::setCurrentAsset(std::shared_ptr<Asset> asset) {
    _currentAsset = asset;
    // Set `asset` lua global to point to the current asset table

    if (asset == _rootAsset) {
        lua_pushnil(*_luaState);
        lua_setglobal(*_luaState, AssetGlobalVariableName);
        return;
    }

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, AssetTableName);
    lua_setglobal(*_luaState, AssetGlobalVariableName);
}

int AssetLoader::requireLua(Asset* dependant) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("require", *_luaState, 1, nArguments);

    std::string assetName = luaL_checkstring(*_luaState, 1);

    std::shared_ptr<Asset> dependency = require(assetName);

    if (!dependency) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }

    addLuaDependencyTable(dependant, dependency.get());

    // Get the exports table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependency->id().c_str());
    lua_getfield(*_luaState, -1, ExportsTableName);
    int exportsTableIndex = lua_gettop(*_luaState);

    // Get the dependency table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependency->id().c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    lua_getfield(*_luaState, -1, dependant->id().c_str());
    int dependencyTableIndex = lua_gettop(*_luaState);

    lua_pushvalue(*_luaState, exportsTableIndex);
    lua_pushvalue(*_luaState, dependencyTableIndex);
    return 2;
}

int AssetLoader::requestLua(Asset* parent) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("request", *_luaState, 1, nArguments);
    
    std::string assetName = luaL_checkstring(*_luaState, 1);
    
    std::shared_ptr<Asset> child = request(assetName);

    addLuaDependencyTable(parent, child.get());
        
    // Get the dependency table
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, child->id().c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    lua_getfield(*_luaState, -1, parent->id().c_str());
    int dependencyTableIndex = lua_gettop(*_luaState);

    lua_pushvalue(*_luaState, dependencyTableIndex);
    return 1;
}

int AssetLoader::exportAssetLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("exportAsset", *_luaState, 2, nArguments);

    std::string exportName = luaL_checkstring(*_luaState, 1);

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, ExportsTableName);
    int exportsTableIndex = lua_gettop(*_luaState);

    // push the second argument
    lua_pushvalue(*_luaState, 2);
    lua_setfield(*_luaState, exportsTableIndex, exportName.c_str());
    return 0;
}

void AssetLoader::addLuaDependencyTable(Asset* dependant, Asset* dependency) {
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
}

void AssetLoader::addAssetListener(AssetListener* listener) {
    const auto it = std::find(
        _assetListeners.begin(),
        _assetListeners.end(),
        listener
    );

    if (it == _assetListeners.end()) {
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

void AssetLoader::assetStateChanged(std::shared_ptr<Asset> asset, Asset::State state) {
    for (auto& listener : _assetListeners) {
        listener->assetStateChanged(asset, state);
    }
}

void AssetLoader::assetRequested(std::shared_ptr<Asset> parent,
                                 std::shared_ptr<Asset> child)
{
    for (auto& listener : _assetListeners) {
        listener->assetRequested(parent, child);
    }
}

void AssetLoader::assetUnrequested(std::shared_ptr<Asset> parent,
                                   std::shared_ptr<Asset> child)
{
    for (auto& listener : _assetListeners) {
        listener->assetUnrequested(parent, child);
    }
}

}
