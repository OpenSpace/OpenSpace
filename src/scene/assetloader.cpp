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

#include <openspace/scripting/script_helper.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/filesystem/filesystem.h>

#include "assetloader_lua.inl"

namespace {
    const char* AssetFileSuffix = "asset";
    const char* AssetGlobalVariableName = "asset";
    const char* DataFileSuffix = "data";

    const char* SyncTableName = "sync";
    const char* ImportFunctionName = "import";
    const char* ImportToggleFunctionName = "importToggle";
    const char* SyncedResourceFunctionName = "syncedResource";
    const char* LocalResourceFunctionName = "localResource";
    const char* OnInitializeFunctionName = "onInitialize";
    const char* OnDeinitializeFunctionName = "onDeinitialize";

    const char* AssetsTableName = "_assets";
    const char* DependantsTableName = "_dependants";

    const char* _loggerCat = "AssetLoader";

    bool isRelative(std::string path) {
        if (path.size() > 2) {
            if (path[0] == '.' && path[1] == '/') return true;
        }
        if (path.size() > 3) {
            if (path[0] == '.' && path[1] == '.' && path[2] == '/') return true;
        }
        return false;
    };
}

namespace openspace {

namespace assetloader {

int importAsset(lua_State* state) {
    AssetLoader *assetLoader = (AssetLoader*)lua_touserdata(state, lua_upvalueindex(1));

    int nArguments = lua_gettop(state);
    if (nArguments != 1) {
        return luaL_error(state, "Expected 1 argument, got %i", nArguments);
    }

    std::string assetName = luaL_checkstring(state, 1);

    try {
        return assetLoader->importAssetLua(assetName, false, false);
    } catch (const ghoul::RuntimeError& e) {
        return luaL_error(state, "Failed to import asset '%s'. %s: %s", assetName.c_str(), e.message, e.component);
    }
}

int importAssetToggle(lua_State* state) {
    AssetLoader *assetLoader = (AssetLoader*)lua_touserdata(state, lua_upvalueindex(1));

    int nArguments = lua_gettop(state);
    if (nArguments != 2) {
        return luaL_error(state, "Expected 2 arguments, got %i", nArguments);
    }
        
    bool toggleEnabled = lua_toboolean(state, 2);

    std::string assetName = luaL_checkstring(state, 1);
    try {
        return assetLoader->importAssetLua(assetName, true, toggleEnabled);
    } catch (const ghoul::RuntimeError& e) {
        return luaL_error(state, "Failed to import asset '%s'. %s: %s", assetName.c_str(), e.message, e.component);
    }
}

int resolveLocalResource(lua_State* state) {
    AssetLoader::Asset* asset = (AssetLoader::Asset*)lua_touserdata(state, lua_upvalueindex(1));
    return asset->resolveLocalResourceLua();
}

int resolveSyncedResource(lua_State* state) {
    AssetLoader::Asset* asset = (AssetLoader::Asset*)lua_touserdata(state, lua_upvalueindex(1));
    return asset->resolveSyncedResourceLua();
}

int noOperation(lua_State* state) {
    return 0;
}

} // namespace assetloader

AssetLoader::AssetLoader(ghoul::lua::LuaState* luaState, std::string assetRoot, std::string syncRoot)
    : _luaState(luaState)
    , _rootAsset(std::make_unique<Asset>(this, std::move(assetRoot)))
    , _syncRoot(std::move(syncRoot))
{
    pushAsset(_rootAsset.get());

    // Create _assets table.
    lua_newtable(*_luaState);
    lua_setglobal(*_luaState, AssetsTableName);
}

AssetLoader::Asset* AssetLoader::importAsset(
    const std::string& name,
    bool togglableInitializationRequirement,
    bool toggleOn)
{
    ghoul::filesystem::Directory directory = currentDirectory();
    std::unique_ptr<Asset> newAsset = std::make_unique<Asset>(this, directory, name);
    const std::string id = newAsset->id();

    Asset* asset = newAsset.get();

    // Check if asset is already loaded.
    const auto it = _importedAssets.find(id);
    const bool loaded = it != _importedAssets.end();
    if (loaded) {
        asset = it->second.get();
    }

    InitializationRequirement reqInit = InitializationRequirement::Yes;
    if (togglableInitializationRequirement && !toggleOn) {
        reqInit = InitializationRequirement::No;
    }

    Asset* dependant = _assetStack.back();
    dependant->addDependency(asset, togglableInitializationRequirement, reqInit);

    if (loaded) {
        return asset;
    }

    const std::string path = asset->assetFilePath();
    if (!FileSys.fileExists(path)) {
        throw ghoul::FileNotFoundError(path);
    }

    // Actually loading asset.
    pushAsset(asset);
    ghoul::OnScopeExit e([this]() {
        popAsset();
    });

    try {
        ghoul::lua::runScriptFile(*_luaState, path);
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(e.message);
        return nullptr;
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(e.message);
        return nullptr;
    }

    LDEBUG("Imported asset " << asset->id());
    
    _importedAssets.emplace(id, std::move(newAsset));

    return _importedAssets[id].get();
}

ghoul::filesystem::Directory AssetLoader::currentDirectory() {
    return _assetStack.back()->assetDirectory();
}

void AssetLoader::loadAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only load an asset from the root asset");
    try {
        importAsset(identifier);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Error loading asset '" << identifier << "': " << e.message);
    }
}


void AssetLoader::unloadAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only unload an asset from the root asset");

    ghoul::filesystem::Directory directory = currentDirectory();
    Asset tempAsset(this, directory, identifier);
    const std::string id = tempAsset.id();

    _rootAsset->removeDependency(id);

}


ghoul::lua::LuaState* AssetLoader::luaState() {
    return _luaState;
}

AssetLoader::Asset* AssetLoader::rootAsset() {
    return _rootAsset.get();
}

const std::string& AssetLoader::syncRoot() {
    return _syncRoot;
}

void AssetLoader::pushAsset(Asset* asset) {
    _assetStack.push_back(asset);

    if (!asset->hasLuaTable()) {
        return;
    }
    
    // Push the global asset table to the lua stack.
    lua_getglobal(*_luaState, AssetsTableName);
    int globalTableIndex = lua_gettop(*_luaState);

    // Create table for the current asset.
    lua_newtable(*_luaState);
    int assetTableIndex = lua_gettop(*_luaState);

    // Register local resource function
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::resolveLocalResource, 1);
    lua_setfield(*_luaState, assetTableIndex, LocalResourceFunctionName);

    // Register synced resource function
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::resolveSyncedResource, 1);
    lua_setfield(*_luaState, assetTableIndex, SyncedResourceFunctionName);

    // Register import function
    lua_pushlightuserdata(*_luaState, asset->loader());
    lua_pushcclosure(*_luaState, &assetloader::importAsset, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportFunctionName);

    // Register importToggle function
    lua_pushlightuserdata(*_luaState, asset->loader());
    lua_pushcclosure(*_luaState, &assetloader::importAssetToggle, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportToggleFunctionName);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnInitializeFunctionName);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnDeinitializeFunctionName);

    // Register empty data table on imported asset
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetTableIndex, DataTableName);

    // Register empty dependant table on imported asset.
    // (importer => dependant object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetTableIndex, DependantsTableName);

    // Extend global asset table (pushed to the lua stack earlier) with this asset 
    lua_setfield(*_luaState, globalTableIndex, asset->id().c_str());

    // Update lua globals
    updateLuaGlobals();
}

void AssetLoader::popAsset() {
    _assetStack.pop_back();
    updateLuaGlobals();
}

void AssetLoader::updateLuaGlobals() {
    Asset* asset = _assetStack.back();
    // Set `asset` lua global to point to the current asset table
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_setglobal(*_luaState, AssetGlobalVariableName);
}

int AssetLoader::importAssetLua(std::string assetName, bool togglableInitializationRequirement, bool toggleEnabled) {
    Asset* importer = _assetStack.back();

    Asset* importedAsset = importAsset(assetName, togglableInitializationRequirement, toggleEnabled);
    if (!importedAsset) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }
    const std::string importerId = importer->id();
    const std::string importedAssetId = importedAsset->id();

    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, importedAssetId.c_str());
    const int importedAssetIndex = lua_gettop(*_luaState);

    // Extract the imported asset's dependants table
    lua_getfield(*_luaState, -1, DependantsTableName);
    const int dependantsTableIndex = lua_gettop(*_luaState);

    // Set up Dependency object
    lua_newtable(*_luaState);
    const int currentDependantTableIndex = lua_gettop(*_luaState);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, currentDependantTableIndex, OnInitializeFunctionName);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, currentDependantTableIndex, OnDeinitializeFunctionName);

    // duplicate the table reference on the stack, so it remains after assignment.
    lua_pushvalue(*_luaState, -1);

    // Register the dependant table on the imported asset's dependants table.
    lua_setfield(*_luaState, dependantsTableIndex, importerId.c_str());

    lua_pushvalue(*_luaState, importedAssetIndex);
    lua_pushvalue(*_luaState, currentDependantTableIndex);
    return 2;
}


scripting::LuaLibrary AssetLoader::luaLibrary() {
    return {
        "",
        {
            {
                "loadAsset",
                &luascriptfunctions::loadAsset,
                {this},
                "string",
                ""
            },
            {
                "unloadAsset",
                &luascriptfunctions::unloadAsset,
                {this},
                "string",
                ""
            },
        }
    };
}



// Asset methods.

std::string AssetLoader::Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = assetDirectory();
    return currentAssetDirectory + ghoul::filesystem::FileSystem::PathSeparator + resourceName;
}

std::string AssetLoader::Asset::syncDirectory() {
    std::string currentAssetDirectory = assetDirectory();
    std::string rootAssetDirectory = loader()->rootAsset()->assetDirectory();
    std::string relativePath = FileSys.relativePath(currentAssetDirectory, rootAssetDirectory);

    return loader()->syncRoot() +
        ghoul::filesystem::FileSystem::PathSeparator +
        relativePath +
        ghoul::filesystem::FileSystem::PathSeparator +
        assetName();
}

bool AssetLoader::Asset::isInitialized() {
    return _initialized;
}

bool AssetLoader::Asset::hasLuaTable() {
    return _hasLuaTable;
}

void AssetLoader::Asset::initialize() {
    LDEBUG("Initializing asset " << id());
    if (_initialized) {
        return;
    }

    // Initialize dependencies
    for (auto& dependency : _dependencies) {
        dependency.first->initialize();
    }

    _initialized = true;

    // Call onInitialize
    if (_hasLuaTable) {
        ghoul::lua::LuaState* state = loader()->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, OnInitializeFunctionName);
        const int status = lua_pcall(*state, 0, 0, 0);
        if (status != LUA_OK) {
            throw ghoul::lua::LuaExecutionException(lua_tostring(*state, -1));
        }
    }

    // Notify dependencies
    for (auto& dependency : _dependencies) {
        dependency.first->dependantDidInitialize(this);
    }
}

void AssetLoader::Asset::deinitialize() {
    if (!_initialized) {
        return;
    }

    // Notify dependencies
    for (auto& dependency : _dependencies) {
        dependency.first->dependantWillDeinitialize(this);
    }

    // Call onDeinitialize
    if (_hasLuaTable) {
        ghoul::lua::LuaState* state = loader()->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, OnDeinitializeFunctionName);
        const int status = lua_pcall(*state, 0, 0, 0);
        if (status != LUA_OK) {
            throw ghoul::lua::LuaExecutionException(lua_tostring(*state, -1));
        }
    }

    _initialized = false;

    // Make sure no dependencies are left dangling
    for (auto& dependency : _dependencies) {
        if (!dependency.first->hasInitializedDependants(InitializationRequirement::Yes)) {
            dependency.first->deinitialize();
        }
    }
}

std::string AssetLoader::Asset::resolveSyncedResource(std::string resourceName) {
    return syncDirectory() +
        ghoul::filesystem::FileSystem::PathSeparator + 
        resourceName;
}

int AssetLoader::Asset::resolveLocalResourceLua() {
    ghoul::lua::LuaState* state = loader()->luaState();
    int nArguments = lua_gettop(*state);
    if (nArguments != 1) {
        return luaL_error(*state, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string resourceName = luaL_checkstring(*state, -1);
    std::string resolved = resolveLocalResource(resourceName);

    lua_pushstring(*state, resolved.c_str());
    return 1;
}

int AssetLoader::Asset::resolveSyncedResourceLua() {
    ghoul::lua::LuaState* state = loader()->luaState();
    int nArguments = lua_gettop(*state);
    if (nArguments != 1) {
        return luaL_error(*state, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string resourceName = luaL_checkstring(*state, -1);
    std::string resolved = resolveSyncedResource(resourceName);

    lua_pushstring(*state, resolved.c_str());
    return 1;
}

AssetLoader::Asset::Asset(AssetLoader* loader, ghoul::filesystem::Directory directory)
    : PropertyOwner("RootAsset")
    , _assetDirectory(directory)
    , _loader(loader)
    , _initialized(false)
    , _hasLuaTable(false)
{}

AssetLoader::Asset::Asset(AssetLoader* loader, ghoul::filesystem::Directory baseDirectory, std::string assetPath)
    : PropertyOwner(assetPath)
    , _initialized(false)
    , _hasLuaTable(true)
    , _loader(loader)
{
    if (isRelative(assetPath)) {
        ghoul::filesystem::File assetFile =
            static_cast<std::string>(baseDirectory) + 
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        _assetDirectory = assetFile.directoryName();
        _assetName = assetFile.baseName();
    } else {
        std::string assetRoot = ghoul::filesystem::Directory(loader->rootAsset()->assetDirectory());
        ghoul::filesystem::File assetFile =
            assetRoot +
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        _assetDirectory = assetFile.directoryName();
        _assetName = assetFile.baseName();
    }
}

std::string AssetLoader::Asset::assetFilePath() {
    ghoul::filesystem::File dir(_assetDirectory);
    return _assetDirectory + ghoul::filesystem::FileSystem::PathSeparator + _assetName + "." + AssetFileSuffix;
}

std::string AssetLoader::Asset::assetName() {
    return _assetName;
}

std::string AssetLoader::Asset::assetDirectory() {
    return _assetDirectory;
}

std::string AssetLoader::Asset::id() {
    return assetFilePath();
}

AssetLoader* AssetLoader::Asset::loader() {
    return _loader;
}

ghoul::Dictionary AssetLoader::Asset::syncDictionary() {
    ghoul::lua::LuaState* state = loader()->luaState();

    lua_getglobal(*state, AssetsTableName);
    lua_getfield(*state, -1, id().c_str());
    lua_getfield(*state, -1, SyncTableName);

    ghoul::Dictionary dictionary;
    ghoul::lua::luaDictionaryFromState(*state, dictionary);

    return dictionary;
}

bool AssetLoader::Asset::hasDependency(Asset* asset, InitializationRequirement initReq) {
    auto it = std::find_if(_dependencies.begin(), _dependencies.end(), [asset](const Dependency& d) {
        return d.first == asset;
    });
    if (it == _dependencies.end()) {
        return false;
    }
    return initReq ? (it->second == true) : true;
}

void AssetLoader::Asset::addDependency(Asset* dependency, bool togglableInitReq, InitializationRequirement initReq) {
    // Do nothing if the dependency already exists.
    auto it = std::find_if(_dependencies.begin(), _dependencies.end(), [dependency](const Dependency& d) {
        return d.first == dependency;
    });
    if (it != _dependencies.end()) {
        return;
    }

    if (_initialized && initReq) {
        dependency->initialize();
    }
    _dependencies.push_back(std::make_pair(dependency, initReq));
    dependency->_dependants.push_back(this);

    if (togglableInitReq) {
        std::unique_ptr<DependencyToggle> dt = std::make_unique<DependencyToggle>(dependency, this, initReq);
        addPropertySubOwner(dt.get());
        _dependencyToggles.push_back(std::move(dt));
    } else {
        addPropertySubOwner(dependency);
    }
}

void AssetLoader::Asset::setInitializationRequirement(Asset* dependency, InitializationRequirement initReq) {
    auto it = std::find_if(_dependencies.begin(), _dependencies.end(), [dependency](const Dependency& d) {
        return d.first == dependency;
    });
    if (it == _dependencies.end()) {
        LERROR("The dependency does not exist");
    }
    it->second = initReq;

    if (_initialized && initReq) {
        dependency->initialize();
        dependency->dependantDidInitialize(this);
    }
    if (!initReq && !dependency->hasInitializedDependants(InitializationRequirement::Yes)) {
        dependency->dependantWillDeinitialize(this);
        dependency->deinitialize();
    }
}

void AssetLoader::Asset::removeDependency(Asset * dependency) {
    _dependencies.erase(
        std::remove_if(_dependencies.begin(), _dependencies.end(), [dependency](const Dependency& d) {
            return d.first == dependency;
        }),
        _dependencies.end()
    );
    std::vector<Asset*>& dependants = dependency->_dependants;
    dependants.erase(
        std::remove(dependants.begin(), dependants.end(), this),
        dependants.end()
    );

    if (!dependency->hasInitializedDependants(InitializationRequirement::No)) {
        dependency->deinitialize();
    }
}

void AssetLoader::Asset::removeDependency(const std::string& assetId) {
    auto dep = std::find_if(_dependencies.begin(), _dependencies.end(), [&assetId](const Dependency& d) {
        return d.first->id() == assetId;
    });
    if (dep != _dependencies.end()) {
        removeDependency(dep->first);
    } else {
        LERROR("No such dependency '" << assetId << "'");
    }
}

void AssetLoader::Asset::dependantDidInitialize(Asset* dependant) {
    if (dependant->_hasLuaTable) {
        ghoul::lua::LuaState* state = _loader->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, DependantsTableName);
        lua_getfield(*state, -1, dependant->id().c_str());
        lua_getfield(*state, -1, OnInitializeFunctionName);
        const int status = lua_pcall(*state, 0, 0, 0);
        if (status != LUA_OK) {
            throw ghoul::lua::LuaLoadingException(lua_tostring(*state, -1));
        }
    }
}

void AssetLoader::Asset::dependantWillDeinitialize(Asset* dependant) {
    if (dependant->_hasLuaTable) {
        ghoul::lua::LuaState* state = _loader->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, DependantsTableName);
        lua_getfield(*state, -1, dependant->id().c_str());
        lua_getfield(*state, -1, OnDeinitializeFunctionName);
        const int status = lua_pcall(*state, 0, 0, 0);
        if (status != LUA_OK) {
            throw ghoul::lua::LuaLoadingException(lua_tostring(*state, -1));
        }
    }
}

bool AssetLoader::Asset::hasDependant(InitializationRequirement initReq) {
    bool foundDep = false;
    for (auto& dependant : _dependants) {
        if (dependant->hasDependency(this, initReq)) {
            foundDep = true;
        }
    }
    return foundDep;
}

bool AssetLoader::Asset::hasInitializedDependants(InitializationRequirement initReq) {
    bool foundInitializedDep = false;
    for (auto& dependant : _dependants) {
        if (dependant->isInitialized() && dependant->hasDependency(this, initReq)) {
            foundInitializedDep = true;
        }
    }
    return foundInitializedDep;
}

// Dependency toggle
AssetLoader::DependencyToggle::DependencyToggle(Asset* dependency, Asset* dependant, bool enabled)
    : PropertyOwner(dependency->name())
    , _enabled("enabled", "Enabled", enabled)
    , _dependency(dependency)
    , _dependant(dependant)
{
    addProperty(_enabled);
    addPropertySubOwner(_dependency);
    _enabled.onChange([this]() {
        _dependant->setInitializationRequirement(
            _dependency,
            _enabled ? InitializationRequirement::Yes : InitializationRequirement::No
        );
    });
}

}
