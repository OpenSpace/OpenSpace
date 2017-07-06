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
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/onscopeexit.h>

#include <ghoul/filesystem/filesystem.h>

namespace {
    const char* AssetFileSuffix = "asset";
    const char* ImportFunctionName = "import";
    const char* ExportFunctionName = "export";
    const char* SyncedResourceFunctionName = "syncedResource";
    const char* LocalResourceFunctionName = "localResource";
    const char* _loggerCat = "AssetLoader";
    const char* AssetsTableName = "_assets";
    const char* KeySceneGraphNodes = "SceneGraphNodes";
}

namespace openspace {

namespace assetloader {
int importAsset(lua_State* state) {
    AssetLoader *assetLoader = (AssetLoader*)lua_touserdata(state, lua_upvalueindex(1));
    return assetLoader->importAssetLua();
}

int exportAsset(lua_State* state) {
    AssetLoader::Asset* asset = (AssetLoader::Asset*)lua_touserdata(state, lua_upvalueindex(1));
    return asset->exportAssetLua();
}

int resolveLocalResource(lua_State* state) {
    AssetLoader::Asset* asset = (AssetLoader::Asset*)lua_touserdata(state, lua_upvalueindex(1));
    return asset->resolveLocalResourceLua();
}

int resolveSyncedResource(lua_State* state) {
    AssetLoader::Asset* asset = (AssetLoader::Asset*)lua_touserdata(state, lua_upvalueindex(1));
    return asset->resolveSyncedResourceLua();
}

}

AssetLoader::AssetLoader(std::string assetRoot, std::string syncRoot)
   : _rootAsset(std::make_unique<Asset>(this, std::move(assetRoot)))
   , _syncRoot(std::move(syncRoot))
{
    _assetStack.push_back(_rootAsset.get());

    // Create _assets table.
    lua_newtable(_luaState);
    lua_setglobal(_luaState, AssetsTableName);
        
    // Register import function
    lua_pushlightuserdata(_luaState, this);
    lua_pushcclosure(_luaState, &assetloader::importAsset, 1);
    lua_setglobal(_luaState, ImportFunctionName);
}

AssetLoader::Asset* AssetLoader::loadAsset(const std::string& name) {
    ghoul::filesystem::Directory directory = currentDirectory();
    std::unique_ptr<Asset> asset = std::make_unique<Asset>(this, directory, name);
    const std::string id = asset->id();

    // Check if asset is already loaded.
    if (_loadedAssets.find(id) != _loadedAssets.end()) {
        return _loadedAssets[id].get();
    }

    // Create dependency link
    Asset* dependant = _assetStack.back();
    dependant->addDependency(asset.get());
    asset->addDependant(dependant);

    // Actually loading asset.
    pushAsset(asset.get());
    ghoul::OnScopeExit e([this]() {
        popAsset();
    });

    try {
        ghoul::lua::runScriptFile(_luaState, asset->assetFilepath());
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(e.message << ": " << e.component);
    }
    
    _loadedAssets.emplace(id, std::move(asset));
    return _loadedAssets[id].get();
}

ghoul::filesystem::Directory AssetLoader::currentDirectory() {
    return _assetStack.back()->directory();
}

ghoul::lua::LuaState& AssetLoader::luaState() {
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
    updateGlobalLuaFunctions();

}

void AssetLoader::popAsset() {
    _assetStack.pop_back();
    updateGlobalLuaFunctions();
}

void AssetLoader::updateGlobalLuaFunctions() {
    Asset* asset = _assetStack.size() > 0 ? _assetStack.back() : nullptr;

    // Register resolve functions
    lua_pushlightuserdata(_luaState, asset);
    lua_pushcclosure(_luaState, &assetloader::resolveLocalResource, 1);
    lua_setglobal(_luaState, LocalResourceFunctionName);

    lua_pushlightuserdata(_luaState, asset);
    lua_pushcclosure(_luaState, &assetloader::resolveSyncedResource, 1);
    lua_setglobal(_luaState, SyncedResourceFunctionName);

    // Register export function
    lua_pushlightuserdata(_luaState, asset);
    lua_pushcclosure(_luaState, &assetloader::exportAsset, 1);
    lua_setglobal(_luaState, ExportFunctionName);
}

int openspace::AssetLoader::importAssetLua() {
    std::string assetName = luaL_checkstring(_luaState, -1);

    Asset* asset = loadAsset(assetName);
    if (!asset) {
        return luaL_error(_luaState, "Asset '%s' not found", assetName.c_str());
    }
    const std::string assetId = asset->id();
    lua_getglobal(_luaState, AssetsTableName);
    lua_getfield(_luaState, -1, assetId.c_str());
    return 1;
}

int AssetLoader::Asset::exportAssetLua() {
    lua_State* state = _loader->luaState();

    int nArguments = lua_gettop(state);
    if (nArguments != 1)
        return luaL_error(state, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(state, -1);
    if (type != LUA_TTABLE)
        return luaL_error(state, "Expected table, got %i", type);

    // Extend this asset with resource methods
    lua_getglobal(state, SyncedResourceFunctionName);
    lua_setfield(state, -2, SyncedResourceFunctionName);

    lua_getglobal(state, LocalResourceFunctionName);
    lua_setfield(state, -2, LocalResourceFunctionName);

    // Extend global asset table with this asset
    lua_getglobal(state, AssetsTableName);
    lua_insert(state, -2);
    lua_setfield(state, -2, id().c_str());
    return 0;
}


// Asset methods.

std::string AssetLoader::Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = directory();
    return currentAssetDirectory + ghoul::filesystem::FileSystem::PathSeparator + resourceName;
}

std::string AssetLoader::Asset::resolveSyncedResource(std::string resourceName) {
    std::string currentAssetDirectory = directory();
    std::string rootAssetDirectory = loader()->rootAsset()->directory();
    std::string relativePath = FileSys.relativePath(currentAssetDirectory, rootAssetDirectory);

    return loader()->syncRoot() +
        ghoul::filesystem::FileSystem::PathSeparator +
        relativePath +
        ghoul::filesystem::FileSystem::PathSeparator + 
        resourceName;
}

int AssetLoader::Asset::resolveLocalResourceLua() {
    ghoul::lua::LuaState& state = loader()->luaState();
    int nArguments = lua_gettop(state);
    if (nArguments != 1)
        return luaL_error(state, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(state, -1);
    if (type != LUA_TSTRING)
        return luaL_error(state, "Expected string, got %i", type);

    std::string resourceName = luaL_checkstring(state, -1);
    std::string resolved = resolveLocalResource(resourceName);

    lua_pushstring(state, resolved.c_str());
    return 1;
}

int AssetLoader::Asset::resolveSyncedResourceLua() {
    ghoul::lua::LuaState& state = loader()->luaState();
    int nArguments = lua_gettop(state);
    if (nArguments != 1)
        return luaL_error(state, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(state, -1);
    if (type != LUA_TSTRING)
        return luaL_error(state, "Expected string, got %i", type);

    std::string resourceName = luaL_checkstring(state, -1);
    std::string resolved = resolveSyncedResource(resourceName);

    lua_pushstring(state, resolved.c_str());
    return 1;
}

AssetLoader::Asset::Asset(AssetLoader* loader, std::string directory)
    : _directory(directory)
    , _loader(loader)
{}

AssetLoader::Asset::Asset(AssetLoader* loader, std::string directory, std::string name) {
    std::string base = ghoul::filesystem::Directory(loader->rootAsset()->directory());
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
        _directory = absPath(directory) + ghoul::filesystem::FileSystem::PathSeparator + name;
    } else {
        _directory = absPath(base) + ghoul::filesystem::FileSystem::PathSeparator + name;
    }

    _loader = loader;
}

std::string AssetLoader::Asset::assetFilepath() {
    ghoul::filesystem::File dir(_directory);
    const std::string baseName = dir.baseName();
    return _directory + ghoul::filesystem::FileSystem::PathSeparator + baseName + "." + AssetFileSuffix;
}

std::string AssetLoader::Asset::directory() {
    return _directory;
}

std::string AssetLoader::Asset::id() {
    return assetFilepath();
}

ghoul::Dictionary AssetLoader::Asset::dictionary() {
    AssetLoader* assetLoader = loader();
    ghoul::lua::LuaState& state = assetLoader->luaState();
    ghoul::Dictionary dictionary;
    lua_getglobal(state, AssetsTableName);
    lua_getfield(state, -1, id().c_str());


    if (lua_type(state, -1) != LUA_TTABLE) {
        // The asset did not export anything.
        // Return an empty dictionary.
        return dictionary;
    }

    lua_getfield(state, -1, KeySceneGraphNodes);

    if (lua_type(state, -1) != LUA_TTABLE) {        
        // The asset did not export any SceneGraphNodes.
        // Return an empty dictionary.
        return dictionary;
    }

    ghoul::lua::luaDictionaryFromState(state, dictionary);
    return dictionary;
}

AssetLoader* AssetLoader::Asset::loader() {
    return _loader;
}

void AssetLoader::Asset::addDependency(Asset* asset) {
    _dependencies.push_back(asset);
}

void AssetLoader::Asset::addDependant(Asset* asset) {
    _dependants.push_back(asset);
}

}
