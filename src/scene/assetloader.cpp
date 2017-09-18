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

    const char* SyncTableName = "sync";
    const char* ImportFunctionName = "import";
    const char* ImportOptionalFunctionName = "importOptional";

    const char* SyncedResourceFunctionName = "syncedResource";
    const char* LocalResourceFunctionName = "localResource";

    const char* OnSynchronizeFunctionName = "onSynchronize";

    const char* OnInitializeFunctionName = "onInitialize";
    const char* OnDeinitializeFunctionName = "onDeinitialize";

    const char* AssetsTableName = "_assets";
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

namespace assetloader {

int importDependency(lua_State* state) {
    AssetLoader *assetLoader =
        reinterpret_cast<AssetLoader*>(lua_touserdata(state, lua_upvalueindex(1)));

    int nArguments = lua_gettop(state);
    if (nArguments != 1) {
        return luaL_error(state, "Expected 1 argument, got %i", nArguments);
    }

    std::string assetName = luaL_checkstring(state, 1);

    try {
        return assetLoader->importDependencyLua(assetName);
    } catch (const ghoul::RuntimeError& e) {
        return luaL_error(
            state,
            "Failed to import asset '%s'. %s: %s",
            assetName.c_str(),
            e.message.c_str(),
            e.component.c_str()
        );
    }
}

int importOptional(lua_State* state) {
    AssetLoader *assetLoader =
        reinterpret_cast<AssetLoader*>(lua_touserdata(state, lua_upvalueindex(1)));

    int nArguments = lua_gettop(state);
    if (nArguments != 2) {
        return luaL_error(state, "Expected 2 arguments, got %i", nArguments);
    }
    
    std::string assetName = luaL_checkstring(state, 1);
    bool enabled = lua_toboolean(state, 2);
    
    try {
        return assetLoader->importOptionalLua(assetName, enabled);
    } catch (const ghoul::RuntimeError& e) {
        return luaL_error(
            state,
            "Failed to import optional asset '%s'. %s: %s",
            assetName.c_str(),
            e.message.c_str(),
            e.component.c_str());
    }
}

int resolveLocalResource(lua_State* state) {
    Asset* asset =
        reinterpret_cast<Asset*>(lua_touserdata(state, lua_upvalueindex(1)));
    return asset->loader()->resolveLocalResourceLua(asset);
}

int resolveSyncedResource(lua_State* state) {
    Asset* asset =
        reinterpret_cast<Asset*>(lua_touserdata(state, lua_upvalueindex(1)));
    return asset->loader()->resolveSyncedResourceLua(asset);
}

int onFinishSynchronization(lua_State* state) {
    Asset* asset =
        reinterpret_cast<Asset*>(lua_touserdata(state, lua_upvalueindex(1)));
    return asset->loader()->onFinishSynchronizationLua(asset);
}

int noOperation(lua_State* state) {
    return 0;
}

int noSynchronization(lua_State* state) {

    int nArguments = lua_gettop(state);
    // Todo: call onFinish.
    return 0;
}

} // namespace assetloader

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
    lua_setglobal(*_luaState, AssetsTableName);
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
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(e.message);
        return nullptr;
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(e.message);
        return nullptr;
    }

    _importedAssets.emplace(asset->id(), std::move(asset));

    return rawAsset;
}

std::string AssetLoader::generateAssetPath(const std::string& baseDirectory, const std::string& assetPath) const {
    if (isRelative(assetPath)) {
        ghoul::filesystem::File assetFile =
            static_cast<std::string>(baseDirectory) +
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        return assetFile.path();
    }
    else {
        std::string assetRoot = ghoul::filesystem::Directory(_assetRootDirectory);
        ghoul::filesystem::File assetFile =
            assetRoot +
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        return assetFile.path();
    }
}


Asset* AssetLoader::getAsset(std::string name) {
    ghoul::filesystem::Directory directory = currentDirectory();
    std::string path = generateAssetPath(directory, name);

    // Check if asset is already loaded.
    const auto it = _importedAssets.find(path);
    const bool loaded = it != _importedAssets.end();

    return loaded ? it->second.get() : loadAsset(path);
}

Asset* AssetLoader::importDependency(const std::string& name) {
    Asset* asset = getAsset(name);
    if (!asset) {
        // TODO: Throw
        return nullptr;
    }
    Asset* dependant = _assetStack.back();
    dependant->addDependency(asset);
    return asset;
}

Asset* AssetLoader::importOptional(const std::string& name, bool enabled) {
    Asset* asset = getAsset(name);
    if (!asset) {
        // TODO: Throw
        return nullptr;
    }
    Asset* owner = _assetStack.back();
    owner->addOptional(asset, enabled);
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
    Asset* imported = importOptional(identifier, true);
    std::vector<Asset*> optionals = _rootAsset->optionalAssets();

    // Remove all other optionals
    for (auto& optional : optionals) {
        if (optional != imported) {
            _rootAsset->removeOptional(optional);
        }
    }
}

void AssetLoader::importAsset(const std::string & identifier) {
    ghoul_assert(_assetStack.size() == 1, "Can only import an asset from the root asset");
    try {
        importOptional(identifier);
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
}

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
}

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
    onFinish(true);
}


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

    // Register import-dependency function
    lua_pushlightuserdata(*_luaState, this);
    lua_pushcclosure(*_luaState, &assetloader::importDependency, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportFunctionName);

    // Register import-optional function
    lua_pushlightuserdata(*_luaState, this);
    lua_pushcclosure(*_luaState, &assetloader::importOptional, 1);
    lua_setfield(*_luaState, assetTableIndex, ImportOptionalFunctionName);

    // Register default onSynchronize function
    lua_pushlightuserdata(*_luaState, this);
    lua_pushcclosure(*_luaState, &assetloader::noSynchronization, 1);
    lua_setfield(*_luaState, assetTableIndex, OnSynchronizeFunctionName);

    // Register default onInitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnInitializeFunctionName);

    // Register default onDeinitialize function
    lua_pushcfunction(*_luaState, &assetloader::noOperation);
    lua_setfield(*_luaState, assetTableIndex, OnDeinitializeFunctionName);

    // Register empty sync table on imported asset
    lua_newtable(*_luaState);
    lua_setfield(*_luaState, assetTableIndex, SyncTableName);

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

int AssetLoader::importDependencyLua(std::string assetName) {
    Asset* importer = _assetStack.back();

    Asset* importedAsset = importDependency(assetName);
    if (!importedAsset) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }
    return createLuaTableEntries(importer, importedAsset);
}

int AssetLoader::importOptionalLua(std::string assetName, bool enabled) {
    Asset* importer = _assetStack.back();

    Asset* importedAsset = importOptional(assetName, enabled);
    if (!importedAsset) {
        return luaL_error(*_luaState, "Asset '%s' not found", assetName.c_str());
    }
    return createLuaTableEntries(importer, importedAsset);
}



int AssetLoader::createLuaTableEntries(const Asset* importer, const Asset* importedAsset) {
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
            },
            {
                "synchronizeResource",
                &luascriptfunctions::synchronizeResource,
                { this },
                "table, function",
                ""
            },
        }
    };
}

}
