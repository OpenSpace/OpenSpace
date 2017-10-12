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

#include "assetloader_lua.inl"

namespace {
    const char* AssetGlobalVariableName = "asset";

    const char* ImportRequiredDependencyFunctionName = "import";
    const char* ImportOptionalDependencyFunctionName = "importOptional";
    const char* ExportFunctionName = "export";

    const char* SyncedResourceFunctionName = "syncedResource";
    const char* LocalResourceFunctionName = "localResource";
    const char* AddSynchronizationFunctionName = "addSynchronization";

    const char* OnInitializeFunctionName = "onInitialize";
    const char* OnDeinitializeFunctionName = "onDeinitialize";

    const char* ExportsTableName = "_exports";
    const char* AssetTableName = "_asset";
    const char* DependantsTableName = "_dependants";

    const char* _loggerCat = "AssetLoader";

    const char* AssetFileSuffix = "asset";

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

AssetLoader::AssetLoader(
    ghoul::lua::LuaState& luaState,
    ResourceSynchronizer& resourceSynchronizer,
    std::string assetRootDirectory,
    std::string syncRootDirectory
)
    : _luaState(&luaState)
    , _rootAsset(std::make_unique<Asset>(this))
    , _assetRootDirectory(assetRootDirectory)
    , _syncRootDirectory(std::move(syncRootDirectory))
    , _resourceSynchronizer(&resourceSynchronizer)
{
    pushAsset(_rootAsset.get());

    // Create _assets table.
    lua_newtable(*_luaState);
    _assetsTableRef = luaL_ref(*_luaState, LUA_REGISTRYINDEX);
}

AssetLoader::~AssetLoader() {
}

Asset* AssetLoader::loadAsset(std::string path) {
    std::unique_ptr<Asset> asset = std::make_unique<Asset>(this, path);

    Asset* rawAsset = asset.get();

    pushAsset(rawAsset);
    ghoul::OnScopeExit e([this]() {
        popAsset();
    });

    if (!FileSys.fileExists(path)) {
        throw ghoul::FileNotFoundError(path);
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, path);
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR("Lua exception" << e.what());
    }
    _importedAssets.emplace(asset->id(), std::move(asset));

    return rawAsset;
}

std::string AssetLoader::generateAssetPath(const std::string& baseDirectory,
                                           const std::string& assetPath) const
{
    ghoul::filesystem::Directory directory = isRelative(assetPath) ?
        baseDirectory :
        _assetRootDirectory;
   
    return directory.path() +
        ghoul::filesystem::FileSystem::PathSeparator +
        assetPath +
        "." +
        AssetFileSuffix;
}

Asset* AssetLoader::getAsset(std::string name) {
    ghoul::filesystem::Directory directory = currentDirectory();
    std::string path = generateAssetPath(directory, name);

    // Check if asset is already loaded.
    const auto it = _importedAssets.find(path);

    return it == _importedAssets.end() ?
        loadAsset(path) :
        it->second.get();
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

Asset* AssetLoader::importRequiredDependency(const std::string& name) {
    Asset* asset = getAsset(name);
    if (!asset) {
        // TODO: Throw
        return nullptr;
    }
    Asset* dependant = _assetStack.back();
    dependant->addRequiredDependency(asset);
    return asset;
}

Asset* AssetLoader::importOptionalDependency(const std::string& name, bool enabled) {
    Asset* asset = getAsset(name);
    if (!asset) {
        // TODO: Throw
        return nullptr;
    }
    Asset* owner = _assetStack.back();
    owner->addOptionalDependency(asset, enabled);
    return asset;
}

ghoul::filesystem::Directory AssetLoader::currentDirectory() {
    if (_assetStack.back()->hasAssetFile()) {
        return _assetStack.back()->assetDirectory();
    } else {
        return _assetRootDirectory;
    }
}

void AssetLoader::synchronizeEnabledAssets() {
    _rootAsset->synchronizeEnabledRecursive();
}

void AssetLoader::loadSingleAsset(const std::string& identifier) {
    Asset* imported = importOptionalDependency(identifier, true);
    std::vector<Asset*> optionals = _rootAsset->optionalAssets();

    // Remove all other optionals
    for (auto& optional : optionals) {
        if (optional != imported) {
            _rootAsset->removeOptionalDependency(optional);
        }
    }
}

void AssetLoader::update() {
    // 1. synchronize assets 
}

void AssetLoader::importAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only import an asset from the root asset");
    try {
        importOptionalDependency(identifier);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Error loading asset '" << identifier << "': " << e.message);
    }
}


void AssetLoader::unimportAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only unimport an asset from the root asset");

    ghoul::filesystem::Directory directory = currentDirectory();
    //_rootAsset->removeOptional(id);
}


ghoul::lua::LuaState* AssetLoader::luaState() {
    return _luaState;
}

Asset* AssetLoader::rootAsset() const {
    return _rootAsset.get();
}

const std::string& AssetLoader::syncRootDirectory() {
    return _syncRootDirectory;
}

void AssetLoader::callOnInitialize(Asset* asset) {
    for (int init : _onInitializationFunctionRefs[asset]) {
        lua_pcall(*_luaState, init, 0, 0);
    }
}

void AssetLoader::callOnDeinitialize(Asset * asset) {
    std::vector<int>& funs = _onDeinitializationFunctionRefs[asset];
    for (auto it = funs.rbegin(); it != funs.rend(); it++) {
        lua_pcall(*_luaState, *it, 0, 0);
    }
}

/*
void AssetLoader::callOnSynchronize(Asset* asset) {
    if (asset == _rootAsset.get()) {
        return;
    }
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, OnSynchronizeFunctionName);

    // onSynchronize(function<void(bool)> onFinish)

    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::onFinishSynchronization, 1);
    
    const int status = lua_pcall(*_luaState, 1, 0, 0);
    if (status != LUA_OK) {
        throw ghoul::lua::LuaExecutionException(lua_tostring(*_luaState, -1));
    }
}*/

/*
void AssetLoader::callOnInitialize(Asset * asset) {
    if (asset == _rootAsset.get()) {
        return;
    }
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, OnInitializeFunctionName);
    const int status = lua_pcall(*_luaState, 0, 0, 0);
    if (status != LUA_OK) {
        throw ghoul::lua::LuaExecutionException(lua_tostring(*_luaState, -1));
    }
}

void AssetLoader::callOnDeinitialize(Asset* asset) {
    if (asset == _rootAsset.get()) {
        return;
    }
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, OnDeinitializeFunctionName);
    const int status = lua_pcall(*_luaState, 0, 0, 0);
    if (status != LUA_OK) {
        throw ghoul::lua::LuaExecutionException(lua_tostring(*_luaState, -1));
    }
}*/

/*
void AssetLoader::callOnDependantInitialize(Asset* asset, Asset* dependant) {
    if (asset == _rootAsset.get()) {
        return;
    }
    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    lua_getfield(*_luaState, -1, dependant->id().c_str());
    lua_getfield(*_luaState, -1, OnInitializeFunctionName);
    const int status = lua_pcall(*_luaState, 0, 0, 0);
    if (status != LUA_OK) {
        throw ghoul::lua::LuaLoadingException(lua_tostring(*_luaState, -1));
    }
}

void AssetLoader::callOnDependantDeinitialize(Asset* asset, Asset* dependant) {
    if (asset == _rootAsset.get()) {
        return;
    }

    lua_getglobal(*_luaState, AssetsTableName);
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, DependantsTableName);
    lua_getfield(*_luaState, -1, dependant->id().c_str());
    lua_getfield(*_luaState, -1, OnDeinitializeFunctionName);
    const int status = lua_pcall(*_luaState, 0, 0, 0);
    if (status != LUA_OK) {
        throw ghoul::lua::LuaLoadingException(lua_tostring(*_luaState, -1));
    }
}

void AssetLoader::synchronizeResource(const ghoul::Dictionary & d, std::function<void(bool)> onFinish) {
    LINFO("DUMMY SYNC RESOURCE CALL");
    onFinish(true);
}
*/

int AssetLoader::resolveLocalResourceLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    if (nArguments != 1) {
        return luaL_error(*_luaState, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string resourceName = luaL_checkstring(*_luaState, -1);
    std::string resolved = asset->resolveLocalResource(resourceName);

    lua_pushstring(*_luaState, resolved.c_str());
    return 1;
}

int AssetLoader::resolveSyncedResourceLua(Asset* asset) {
    int nArguments = lua_gettop(*_luaState);
    if (nArguments != 1) {
        return luaL_error(*_luaState, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string resourceName = luaL_checkstring(*_luaState, -1);
    std::string resolved = asset->resolveSyncedResource(resourceName);

    lua_pushstring(*_luaState, resolved.c_str());
    return 1;
}

int AssetLoader::onFinishSynchronizationLua(Asset* asset) {
    LINFO("Finished syncing resource!" << asset->id());
    return 0;
}

void AssetLoader::pushAsset(Asset* asset) {
    _assetStack.push_back(asset);

    if (asset == _rootAsset.get()) {
        return;
    }
    
    // Push the global assets table to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    int globalTableIndex = lua_gettop(*_luaState);

    /*
    Set up lua table:
       AssetMeta
       |- Exports (table<name, exported data>)
       |- Asset
       |  |- localResource
       |  |- syncedResource
       |  |- import
       |  |- importOptional
       |  |- export
       |  |- onInitialize
       |  |- onDeinitialize
       |  |- addSynchronization
       |- Dependants (table<dependant, Dependency dep>)
     
     where Dependency is a table:
       Dependency
       |- localResource
       |- syncedResource
       |- onInitialize
       |- onDeinitialize
    */ 

    // Create meta table for the current asset.
    lua_newtable(*_luaState);
    int assetMetaTableIndex = lua_gettop(*_luaState);
    
    // Register empty exports table on current asset.
    // (string => exported object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetMetaTableIndex, ExportsTableName);

    // Create asset table
    lua_newtable(*_luaState);  
    int assetTableIndex = lua_gettop(*_luaState);

    // Register local resource function
    // string localResource(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::resolveLocalResource, 1);
    lua_setfield(*_luaState, assetTableIndex, LocalResourceFunctionName);

    // Register synced resource function
    // string syncedResource(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::resolveSyncedResource, 1);
    lua_setfield(*_luaState, assetTableIndex, SyncedResourceFunctionName);

    // Register import-dependency function
    // Asset, Dependency import(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::importRequiredDependency, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportRequiredDependencyFunctionName);

    // Register import-optional function
    // Asset, Dependency importOptional(string path)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::importOptionalDependency, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportOptionalDependencyFunctionName);

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
    // void onInitialize(function<void()> deinitializationFunction)
    lua_pushlightuserdata(*_luaState, asset);
    lua_pushcclosure(*_luaState, &assetloader::onDeinitialize, 1);
    lua_setfield(*_luaState, assetTableIndex, OnDeinitializeFunctionName);

    // Attach asset table to asset meta table
    lua_setfield(*_luaState, assetMetaTableIndex, AssetTableName);

    // Register empty dependant table on asset metatable.
    // (importer => dependant object)
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetMetaTableIndex, DependantsTableName);

    // Extend global asset table (pushed to the lua stack earlier) with this asset meta table 
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

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    if (asset == _rootAsset.get()) {
        lua_pushnil(*_luaState);
        lua_setglobal(*_luaState, AssetGlobalVariableName);
        return;
    }
    lua_getfield(*_luaState, -1, asset->id().c_str());
    lua_getfield(*_luaState, -1, AssetTableName);
    lua_setglobal(*_luaState, AssetGlobalVariableName);
}

int AssetLoader::importRequiredDependencyLua(Asset* dependant) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("import", *_luaState, 1, nArguments);

    std::string assetName = luaL_checkstring(*_luaState, 1);

    Asset* dependency = importRequiredDependency(assetName);

    if (!dependency) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }

    addLuaDependencyTable(dependant, dependency);

    int dependencyTableRef = 0; // TODO: get the ref to the dependency
    int dependencyExportsRef = 0; // TODO: get the ref to the exports

    //lua_pushvalue(*_luaState, dependencyExportsRef);
    //lua_pushvalue(*_luaState, dependencyTableRef);
    return 0;
    /*try {
    
    }
    catch (const ghoul::RuntimeError& e) {
        return luaL_error(
            state,
            "Failed to import asset '%s'. %s: %s",
            assetName.c_str(),
            e.message.c_str(),
            e.component.c_str()
        );
    }*/

}

int AssetLoader::importOptionalDependencyLua(Asset* dependant) {
    int nArguments = lua_gettop(*_luaState);
    SCRIPT_CHECK_ARGUMENTS("importOptional", *_luaState, 2, nArguments);

    std::string assetName = luaL_checkstring(*_luaState, 1);
    bool enabled = lua_toboolean(*_luaState, 2);

    Asset* dependency = importOptionalDependency(assetName, enabled);

    if (!dependency) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }

    addLuaDependencyTable(dependant, dependency);
    int dependencyTableRef = 0; // TODO: get the ref to the dependency
    int dependencyExportsRef = 0; // TODO: get the ref to the exports

    //lua_pushvalue(*_luaState, dependencyExportsRef);
    //lua_pushvalue(*_luaState, dependencyTableRef);
    return 0;
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
}

void AssetLoader::addLuaDependencyTable(const Asset* dependant, const Asset* dependency) {
    const std::string dependantId = dependant->id();
    const std::string dependencyId = dependency->id();

    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    lua_getfield(*_luaState, -1, dependencyId.c_str());
    const int dependencyIndex = lua_gettop(*_luaState);

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
    lua_setfield(*_luaState, dependantsTableIndex, dependantId.c_str());
}


scripting::LuaLibrary AssetLoader::luaLibrary() {
    return {
        "",
        {
            {
                "importAsset",
                &luascriptfunctions::importAsset,
                {this},
                "string",
                ""
            },
            {
                "unimportAsset",
                &luascriptfunctions::unimportAsset,
                {this},
                "string",
                ""
            }
        }
    };
}

}
