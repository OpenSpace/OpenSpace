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

    const char* ImportFunctionName = "import";
    const char* SyncedResourceFunctionName = "syncedResource";
    const char* LocalResourceFunctionName = "localResource";
    const char* OnInitializeFunctionName = "onInitialize";
    const char* OnDeinitializeFunctionName = "onDeinitialize";

    const char* AssetsTableName = "_assets";

    const char* _loggerCat = "AssetLoader";
}

namespace openspace {

namespace assetloader {

int importAsset(lua_State* state) {
    AssetLoader *assetLoader = (AssetLoader*)lua_touserdata(state, lua_upvalueindex(1));
    return assetLoader->importAssetLua();
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

AssetLoader::Asset* AssetLoader::importAsset(const std::string& name) {
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

    // Create dependency link
    Asset* dependant = _assetStack.back();
    dependant->addDependency(asset);

    if (loaded) {
        return asset;
    }

    // Actually loading asset.
    pushAsset(asset);
    ghoul::OnScopeExit e([this]() {
        popAsset();
    });

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->assetFilePath());
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(e.message << ": " << e.component);
    }
    
    _importedAssets.emplace(id, std::move(newAsset));
    return _importedAssets[id].get();
}

ghoul::filesystem::Directory AssetLoader::currentDirectory() {
    return _assetStack.back()->assetDirectory();
}

void AssetLoader::loadAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only load an asset from the root asset");
    importAsset(identifier);
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

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnInitializeFunctionName);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnDeinitializeFunctionName);

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

int AssetLoader::importAssetLua() {
    int nArguments = lua_gettop(*_luaState);
    if (nArguments != 1)
        return luaL_error(*_luaState, "Expected %i arguments, got %i", 1, nArguments);

    std::string assetName = luaL_checkstring(*_luaState, -1);

    Asset* asset = importAsset(assetName);
    if (!asset) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }
    const std::string assetId = asset->id();
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, assetId.c_str());
    return 1;
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
        relativePath;
}

bool AssetLoader::Asset::isInitialized() {
    return _initialized;
}

bool AssetLoader::Asset::hasLuaTable() {
    return _hasLuaTable;
}

void AssetLoader::Asset::initialize() {
    if (_initialized) {
        return;
    }

    // Initialize dependencies
    for (auto& dep : _dependencies) {
        dep->initialize();
    }

    // Call onInitialize
    if (_hasLuaTable) {
        ghoul::lua::LuaState* state = loader()->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, OnInitializeFunctionName);
        lua_call(*state, 0, 0);
    }
    _initialized = true;
}

void AssetLoader::Asset::deinitialize() {
    if (!_initialized) {
        return;
    }

    // Call onDeinitialize
    _initialized = false;
    if (_hasLuaTable) {
        ghoul::lua::LuaState* state = loader()->luaState();
        lua_getglobal(*state, AssetsTableName);
        lua_getfield(*state, -1, id().c_str());
        lua_getfield(*state, -1, OnDeinitializeFunctionName);
        lua_call(*state, 0, 0);
    }

    // Also deinitialize any dangling dependencies
    for (auto& dep : _dependencies) {
        if (!dep->hasInitializedDependants()) {
            dep->deinitialize();
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
    if (nArguments != 1)
        return luaL_error(*state, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(*state, -1);
    if (type != LUA_TSTRING)
        return luaL_error(*state, "Expected string, got %i", type);

    std::string resourceName = luaL_checkstring(*state, -1);
    std::string resolved = resolveLocalResource(resourceName);

    lua_pushstring(*state, resolved.c_str());
    return 1;
}

int AssetLoader::Asset::resolveSyncedResourceLua() {
    ghoul::lua::LuaState* state = loader()->luaState();
    int nArguments = lua_gettop(*state);
    if (nArguments != 1)
        return luaL_error(*state, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(*state, -1);
    if (type != LUA_TSTRING)
        return luaL_error(*state, "Expected string, got %i", type);

    std::string resourceName = luaL_checkstring(*state, -1);
    std::string resolved = resolveSyncedResource(resourceName);

    lua_pushstring(*state, resolved.c_str());
    return 1;
}

AssetLoader::Asset::Asset(AssetLoader* loader, std::string directory)
    : _assetDirectory(directory)
    , _loader(loader)
    , _initialized(false)
    , _hasLuaTable(false)
{}

AssetLoader::Asset::Asset(AssetLoader* loader, std::string directory, std::string name)
    : _initialized(false)
    , _hasLuaTable(true)
{
    std::string base = ghoul::filesystem::Directory(loader->rootAsset()->assetDirectory());
    std::string path = base;

    auto isRelative = [](std::string path) {
        if (path.size() > 2) {
            if (path[0] == '.' && path[1] == '/') return true;
        }
        if (path.size() > 3) {
            if (path[0] == '.' && path[1] == '.' && path[2] == '/') return true;
        }
        return false;
    };

    if (isRelative(name)) {
        _assetDirectory = absPath(directory) + ghoul::filesystem::FileSystem::PathSeparator + name;
    } else {
        _assetDirectory = absPath(base) + ghoul::filesystem::FileSystem::PathSeparator + name;
    }

    _loader = loader;
}

std::string AssetLoader::Asset::assetFilePath() {
    ghoul::filesystem::File dir(_assetDirectory);
    const std::string baseName = dir.baseName();
    return _assetDirectory + ghoul::filesystem::FileSystem::PathSeparator + baseName + "." + AssetFileSuffix;
}

std::string AssetLoader::Asset::dataFilePath() {
    ghoul::filesystem::File dir(_assetDirectory);
    const std::string baseName = dir.baseName();
    return _assetDirectory + ghoul::filesystem::FileSystem::PathSeparator + baseName + "." + DataFileSuffix;
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

ghoul::Dictionary AssetLoader::Asset::dataDictionary() {
    ghoul::lua::LuaState state;
    ghoul::Dictionary dictionary;
    ghoul::lua::loadDictionaryFromFile(dataFilePath(), dictionary, state);
    return dictionary;
}

void AssetLoader::Asset::addDependency(Asset* asset) {
    // Do nothing if the dependency already exists.
    if (std::find(_dependencies.begin(), _dependencies.end(), asset) != _dependencies.end()) {
        return;
    }
    if (_initialized) {
        asset->initialize();
    }
    _dependencies.push_back(asset);
    asset->_dependants.push_back(this);
}

void AssetLoader::Asset::removeDependency(Asset * asset) {
    _dependencies.erase(
        std::remove(_dependencies.begin(), _dependencies.end(), asset),
        _dependencies.end()
    );
    std::vector<Asset*>& dependants = asset->_dependants;
    dependants.erase(
        std::remove(dependants.begin(), dependants.end(), this),
        dependants.end()
    );
    if (!asset->hasInitializedDependants()) {
        asset->deinitialize();
    }
}

void AssetLoader::Asset::removeDependency(const std::string& assetId) {
    auto dep = std::find_if(_dependencies.begin(), _dependencies.end(), [&assetId](Asset* asset) {
        return asset->id() == assetId;
    });
    if (dep != _dependencies.end()) {
        removeDependency(*dep);
    } else {
        LERROR("No such dependency '" << assetId << "'");
    }
}


bool AssetLoader::Asset::hasInitializedDependants() {
    bool foundInitialized = false;
    for (auto& dependant : _dependants) {
        if (dependant->isInitialized()) {
            foundInitialized = true;
        }
    }
    return foundInitialized;
}

}
