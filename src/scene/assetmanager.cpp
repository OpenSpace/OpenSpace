/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/asset.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>

#include "assetmanager_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "AssetManager";

    constexpr const char* AssetGlobalVariableName = "asset";

    constexpr const char* ExportsTableName = "_exports";
    constexpr const char* AssetTableName = "_asset";

    enum class PathType {
        RelativeToAsset, ///< Specified as a path relative to the requiring asset
        RelativeToAssetRoot, ///< Specified as a path relative to the root folder
        Absolute,  ///< Specified as an absolute path
        Tokenized ///< Specified as a path that starts with a token
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
        if (FileSys.containsToken(path)) {
            return PathType::Tokenized;
        }
        return PathType::RelativeToAssetRoot;
    }

    struct [[codegen::Dictionary(AssetMeta)]] Parameters {
        // The user-facing name of the asset. It should describe to the user what they can
        // expect when loading the asset into a profile
        std::optional<std::string> name;

        // A version number for this specific asset. It is recommended to use SemVer for
        // the versioning. The versioning used here does not have to correspond to any
        // versioning information provided by OpenSpace
        std::optional<std::string> version;

        // A user-facing description of the asset explaining what the contents are, where
        // the data has been acquired from and what a user can do or see with this asset.
        // This might also provide additional URLs for a user to read more details about
        // the content of this asset
        std::optional<std::string> description;

        // The name of the author for this asset file
        std::optional<std::string> author;

        // A reprentative URL for this asset as chosen by the asset author. This might be
        // a URL to the research group that provided the data, the personal URL of the
        // author, or a webpage for the group that is responsible for this asset
        std::optional<std::string> url [[codegen::key("URL")]];

        // The license information under which this asset is released. For suggestions on
        // potential licenses, see https://opensource.org/licenses but we suggest the MIT
        // or BSD 2-Clause or BSD 3-Clause license.
        std::optional<std::string> license;

        // A list of all identifiers that are exposed by this asset. This list is needed
        // to populate the descriptions in the main user interface
        std::optional<std::vector<std::string>> identifiers [[codegen::identifier()]];
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
    ZoneScoped;

    // In general, the potential dependencies in the root assets are ordered, which is
    // index 0 might be the parent of 1, but not vice versa. So it is safer to do the
    // order deinitialization in reverse
    while (!_rootAssets.empty()) {
        Asset* asset = _rootAssets.back();
        if (!asset->hasInitializedParent()) {
            asset->deinitialize();
            asset->unload();
        }
        _rootAssets.pop_back();
    }
    _toBeDeleted.clear();
}

void AssetManager::update() {
    ZoneScoped;

    // Delete all the assets that have been marked for deletion in the previous frame
    {
        ZoneScopedN("Deleting assets");

        _toBeDeleted.clear();
    }

    // Initialize all assets that have been loaded and synchronized but that not yet
    // initialized
    for (auto it = _toBeInitialized.cbegin(); it != _toBeInitialized.cend(); it++) {
        ZoneScopedN("Initializing queued assets");
        Asset* a = *it;

        if (a->isInitialized() || !a->isSynchronized()) {
            // nothing to do here
            continue;
        }

        a->initialize();

        // We are only doing one asset per frame to keep the loading screen working a bit
        // smoother, so we remove the current one and then break out of the loop
        _toBeInitialized.erase(it);

        // OBS: This can't be replaced with a (get the first one and then bail) as the
        // first asset in the list might wait for its child later in the list to finished.
        // So if we check the first one over and over again, we'll be in an infinite loop
        break;
    }

    // Add all assets that have been queued for loading since the last `update` call
    for (const std::string& asset : _assetAddQueue) {
        ZoneScopedN("Adding queued assets");

        const std::filesystem::path path = generateAssetPath(_assetRootDirectory, asset);
        Asset* a = nullptr;
        try {
            a = retrieveAsset(path, "");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
            continue;
        }

        const auto it = std::find(_rootAssets.cbegin(), _rootAssets.cend(), a);
        if (it != _rootAssets.cend()) {
            // Do nothing if the asset has already been requested on a root level. Even if
            // another asset has already required this asset, calling `load`,
            // `startSynchronization`, etc on it are still fine since all of those
            // functions bail out early if they already have been called before
            continue;
        }

        a->load(nullptr);
        if (a->isFailed()) {
            // The loading might fail because of any number of reasons, most likely of
            // them some Lua syntax error
            continue;
        }
        _rootAssets.push_back(a);
        a->startSynchronizations();

        _toBeInitialized.push_back(a);
        global::profile->addAsset(asset);
    }
    _assetAddQueue.clear();

    // Remove assets
    for (const std::string& asset : _assetRemoveQueue) {
        ZoneScopedN("Removing queued assets");
        std::filesystem::path path = generateAssetPath(_assetRootDirectory, asset);

        const auto it = std::find_if(
            _assets.cbegin(),
            _assets.cend(),
            [&path](const std::unique_ptr<Asset>& a) { return a->path() == path; }
        );
        if (it == _assets.cend()) {
            LWARNING(std::format("Tried to remove unknown asset '{}'. Skipping", asset));
            continue;
        }

        Asset* a = it->get();
        auto jt = std::find(_rootAssets.cbegin(), _rootAssets.cend(), a);
        if (jt == _rootAssets.cend()) {
            // Trying to remove an asset from the middle of the tree might have some
            // unexpected behavior since if we were to remove an asset with children, we
            // would have to unload those children as well. Also, we don't know if that
            // Asset's parents are actually requiring the asset we are about to remove so
            // we might break things horribly for people.
            // We should figure out a way to fix this without reintroducing the
            // require/request confusion, but until then, we'll prohibit removing non-root
            // assets

            LWARNING("Tried to remove an asset that was not on the root level. Skipping");
            continue;
        }

        _rootAssets.erase(jt);
        // Even though we are removing a root asset, we might not be the only person that
        // is interested in the asset, so we can only deinitialize it if we were, in fact,
        // the only person, meaning that the asset never had any parents
        if (!a->hasInitializedParent()) {
            a->deinitialize();
        }
        if (!a->hasLoadedParent()) {
            a->unload();
        }
        global::profile->removeAsset(asset);
    }
    _assetRemoveQueue.clear();


    // Change state based on synchronizations. If any of the unfinished synchronizations
    // has finished since the last call of this function, we should notify the assets and
    // remove the synchronization from the list of unfinished ones so that we don't need
    // to check as much next time around
    for (auto it = _unfinishedSynchronizations.begin();
         it != _unfinishedSynchronizations.end();)
    {
        SyncItem* si = *it;
        if (si->synchronization->isResolved()) {
            for (Asset* a : si->assets) {
                a->setSynchronizationStateResolved();
            }
            it = _unfinishedSynchronizations.erase(it);
        }
        else if (si->synchronization->isRejected()) {
            LERROR(std::format(
                "Failed to synchronize resource '{}'", si->synchronization->name()
            ));
            for (Asset* a : si->assets) {
                a->setSynchronizationStateRejected();
            }
            it = _unfinishedSynchronizations.erase(it);
        }
        else {
            it++;
        }
    }
}

void AssetManager::add(const std::string& path) {
    ghoul_precondition(!path.empty(), "Path must not be empty");
    // First check if the path is already in the remove queue. If so, remove it from there
    _assetRemoveQueue.remove(path);
    _assetAddQueue.push_back(path);
}

void AssetManager::remove(const std::string& path) {
    ghoul_precondition(!path.empty(), "Path must not be empty");
    // First check if the path is already in the add queue. If so, remove it from there
    _assetAddQueue.remove(path);
    _assetRemoveQueue.push_back(path);
}

std::vector<const Asset*> AssetManager::allAssets() const {
    std::vector<const Asset*> res;
    res.reserve(_assets.size());
    for (const std::unique_ptr<Asset>& asset : _assets) {
        res.push_back(asset.get());
    }
    return res;
}

std::vector<const Asset*> AssetManager::rootAssets() const {
    std::vector<const Asset*> res;
    res.reserve(_rootAssets.size());
    for (Asset* asset : _rootAssets) {
        res.push_back(asset);
    }
    return res;
}

std::vector<const ResourceSynchronization*> AssetManager::allSynchronizations() const {
    std::vector<const ResourceSynchronization*> res;
    res.reserve(_synchronizations.size());
    using K = std::string;
    using V = std::unique_ptr<SyncItem>;
    for (const std::pair<const K, V>& p : _synchronizations) {
        res.push_back(p.second->synchronization.get());
    }
    return res;
}

bool AssetManager::isRootAsset(const Asset* asset) const {
    auto it = std::find(_rootAssets.begin(), _rootAssets.end(), asset);
    return it != _rootAssets.end();
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
        LERROR(std::format(
            "Could not load asset '{}': File does not exist", asset->path())
        );
        return false;
    }

    try {
        ghoul::lua::runScriptFile(*_luaState, asset->path());
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(std::format("Could not load asset '{}': {}", asset->path(), e.message));
        return false;
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        return false;
    }

    // Extract meta information from the asset file if it was provided
    lua_getglobal(*_luaState, AssetGlobalVariableName);
    ghoul_assert(lua_istable(*_luaState, -1), "Expected 'asset' table");
    lua_getfield(*_luaState, -1, "meta");
    const ghoul::Dictionary metaDict = ghoul::lua::luaDictionaryFromState(*_luaState);
    if (!metaDict.isEmpty()) {
        const Parameters p = codegen::bake<Parameters>(metaDict);

        Asset::MetaInformation meta;
        meta.name = p.name.value_or(meta.name);
        meta.version = p.version.value_or(meta.version);
        meta.description = p.description.value_or(meta.description);
        meta.author = p.author.value_or(meta.author);
        meta.url = p.url.value_or(meta.url);
        meta.license = p.license.value_or(meta.license);
        meta.identifiers = p.identifiers.value_or(meta.identifiers);

        // We need to do this as the asset might have 'export'ed identifiers before
        // defining the meta table.  Therefore the meta information already contains some
        // identifiers that we don't want to throw away
        if (asset->metaInformation().has_value() &&
            !asset->metaInformation()->identifiers.empty())
        {
            std::vector<std::string> ids = asset->metaInformation()->identifiers;
            meta.identifiers.insert(meta.identifiers.end(), ids.begin(), ids.end());
        }
        asset->setMetaInformation(std::move(meta));
    }

    return true;
}

void AssetManager::unloadAsset(Asset* asset) {
    ghoul_precondition(asset, "Asset must not be nullptr");

    for (const int ref : _onInitializeFunctionRefs[asset]) {
       luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onInitializeFunctionRefs[asset].clear();

    for (const int ref : _onDeinitializeFunctionRefs[asset]) {
        luaL_unref(*_luaState, LUA_REGISTRYINDEX, ref);
    }
    _onDeinitializeFunctionRefs[asset].clear();

    //
    // Tear down asset lua table
    const int top = lua_gettop(*_luaState);
    // Push the global table of AssetInfos to the lua stack.
    lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, _assetsTableRef);
    const int globalTableIndex = lua_gettop(*_luaState);

    ghoul::lua::push(*_luaState, ghoul::lua::nil_t());

    // Clear entry from global asset table (pushed to the Lua stack earlier)
    const std::string path = asset->path().string();
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
    // |  |- resource
    // |  |- require
    // |  |- exists
    // |  |- export
    // |  |- onInitialize
    // |  |- onDeinitialize
    // |  |- directory
    // |  |- filePath
    // |  |- enabled
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
    // string resource(string path or table)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::resource");

            if (ghoul::lua::hasValue<ghoul::Dictionary>(L)) {
                const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
                std::unique_ptr<ResourceSynchronization> s =
                    ResourceSynchronization::createFromDictionary(d);

                const std::string uid =
                    d.value<std::string>("Type") + "/" + s->generateUid();
                SyncItem* syncItem = nullptr;
                auto it = manager->_synchronizations.find(uid);
                if (it == manager->_synchronizations.end()) {
                    auto si = std::make_unique<SyncItem>();
                    si->synchronization = std::move(s);
                    si->assets.push_back(thisAsset);
                    syncItem = si.get();
                    manager->_synchronizations[uid] = std::move(si);
                }
                else {
                    syncItem = it->second.get();
                    syncItem->assets.push_back(thisAsset);
                }

                if (!syncItem->synchronization->isResolved()) {
                    manager->_unfinishedSynchronizations.push_back(syncItem);
                }

                thisAsset->addSynchronization(syncItem->synchronization.get());
                std::filesystem::path path = syncItem->synchronization->directory();
                path += std::filesystem::path::preferred_separator;
                ghoul::lua::push(L, path);
            }
            else if (ghoul::lua::hasValue<std::optional<std::string>>(L)) {
                auto [name] = ghoul::lua::values<std::optional<std::string>>(L);
                const std::filesystem::path path =
                    name.has_value() ?
                    thisAsset->path().parent_path() / *name :
                    thisAsset->path().parent_path();
                ghoul::lua::push(L, path);
            }
            else {
                ghoul::lua::luaError(L, "Invalid parameter");
            }

            return 1;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "resource");

    // @DEPRECATED(abock) This should be removed after 0.20.0
    ghoul::lua::push(*_luaState, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            LWARNING(
                "'asset.localResource' has been deprecrated and should be replaced with "
                "'asset.resource' instead. No change to the parameters are needed"
            );

            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 1);
            ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::resource");

            auto [name] = ghoul::lua::values<std::optional<std::string>>(L);
            const std::filesystem::path path =
                name.has_value() ?
                thisAsset->path().parent_path() / *name :
                thisAsset->path().parent_path();
            ghoul::lua::push(L, path);

            return 1;
        },
        1
    );
    lua_setfield(*_luaState, assetTableIndex, "localResource");

    // @DEPRECATED(abock) This should be removed after 0.20.0
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            LWARNING(
                "'asset.syncedResource' has been deprecrated and should be replaced with "
                "'asset.resource' instead. No change to the parameters are needed"
            );

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::resource");

            const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
            std::unique_ptr<ResourceSynchronization> s =
                ResourceSynchronization::createFromDictionary(d);

            const std::string uid = d.value<std::string>("Type") + "/" + s->generateUid();
            SyncItem* syncItem = nullptr;
            auto it = manager->_synchronizations.find(uid);
            if (it == manager->_synchronizations.end()) {
                auto si = std::make_unique<SyncItem>();
                si->synchronization = std::move(s);
                si->assets.push_back(thisAsset);
                syncItem = si.get();
                manager->_synchronizations[uid] = std::move(si);
            }
            else {
                syncItem = it->second.get();
                syncItem->assets.push_back(thisAsset);
            }

            if (!syncItem->synchronization->isResolved()) {
                manager->_unfinishedSynchronizations.push_back(syncItem);
            }

            thisAsset->addSynchronization(syncItem->synchronization.get());
            std::filesystem::path path = syncItem->synchronization->directory();
            path += std::filesystem::path::preferred_separator;
            ghoul::lua::push(L, path);

            return 1;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "syncedResource");


    // Register require function
    // Asset require(string path, bool? explicitEnable = true)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* parent = ghoul::lua::userData<Asset>(L, 2);

            ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::require");
            auto [assetName, explicitEnable] =
                ghoul::lua::values<std::string, std::optional<bool>>(L);

            const std::filesystem::path path = manager->generateAssetPath(
                parent->path().parent_path(),
                assetName
            );
            Asset* dependency = manager->retrieveAsset(
                path,
                parent->path(),
                explicitEnable
            );
            if (!dependency) {
                return ghoul::lua::luaError(
                    L,
                    std::format("Asset '{}' not found", assetName)
                );
            }
            // this = parent ;  child = dependency
            if (parent->isLoaded()) {
                return ghoul::lua::luaError(
                    L,
                    "Cannot require child asset when already loaded"
                );

            }

            if (dependency->isFailed()) {
                return 0;
            }

            dependency->load(parent);
            if (dependency->isLoaded()) {
                if (parent->isSynchronized()) {
                    dependency->startSynchronizations();
                }
                parent->require(dependency);
            }

            // Get the exports table
            lua_rawgeti(L, LUA_REGISTRYINDEX, manager->_assetsTableRef);
            const std::string p = dependency->path().string();
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
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::exists");
            const std::string name = ghoul::lua::value<std::string>(L);

            const std::filesystem::path path = manager->generateAssetPath(
                thisAsset->path().parent_path(),
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
    // or export(table value) with table.Identifier being defined and a string
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            const int n = ghoul::lua::checkArgumentsAndThrow(
                L,
                { 1 , 2 },
                "lua::exportAsset"
            );
            std::string exportName;
            std::string identifier;
            int targetLocation = 0;
            if (n == 1) {
                const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(
                    L,
                    1,
                    ghoul::lua::PopValue::No
                );
                if (!d.hasValue<std::string>("Identifier")) {
                    return ghoul::lua::luaError(
                        L,
                        "Table being exported does not have an Identifier necessary to "
                        "generate the export key automatically"
                    );
                }
                identifier = d.value<std::string>("Identifier");
                exportName = identifier;
                targetLocation = 1;
            }
            else if (n == 2) {
                exportName = ghoul::lua::value<std::string>(
                    L,
                    1,
                    ghoul::lua::PopValue::No
                );
                targetLocation = 2;

                if (lua_type(L, targetLocation) == LUA_TTABLE) {
                    // The second argument might be anything and we only try to extract
                    // the identifier if it actually is a table *and* if that table
                    // contains the 'Identifier' key

                    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(
                        L,
                        2,
                        ghoul::lua::PopValue::No
                    );

                    if (d.hasValue<std::string>("Identifier")) {
                        identifier = d.value<std::string>("Identifier");
                    }
                }
            }
            else {
                throw ghoul::MissingCaseException();
            }


            lua_rawgeti(L, LUA_REGISTRYINDEX, manager->_assetsTableRef);
            const std::string path = thisAsset->path().string();
            lua_getfield(L, -1, path.c_str());
            lua_getfield(L, -1, ExportsTableName);
            const int exportsTableIndex = lua_gettop(L);

            // push the second argument
            lua_pushvalue(L, targetLocation);
            lua_setfield(L, exportsTableIndex, exportName.c_str());

            // Register registerIdentifierWithMeta function to add meta at runtime
            if (!identifier.empty()) {
                thisAsset->addIdentifier(identifier);
            }

            lua_settop(L, 0);
            return 0;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "export");

    // Register onInitialize function to be called upon asset initialization
    // void onInitialize(function<void()> initializationFunction)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::onInitialize");

            const int referenceIndex = luaL_ref(L, LUA_REGISTRYINDEX);
            manager->_onInitializeFunctionRefs[thisAsset].push_back(referenceIndex);

            lua_settop(L, 0);
            return 0;
        },
        2
    );
    lua_setfield(*_luaState, assetTableIndex, "onInitialize");

    // Register onDeinitialize function to be called upon asset deinitialization
    // void onDeinitialize(function<void()> deinitializationFunction)
    ghoul::lua::push(*_luaState, this, asset);
    lua_pushcclosure(
        *_luaState,
        [](lua_State* L) {
            ZoneScoped;

            AssetManager* manager = ghoul::lua::userData<AssetManager>(L, 1);
            Asset* thisAsset = ghoul::lua::userData<Asset>(L, 2);
            ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::onDeinitialize");

            const int referenceIndex = luaL_ref(L, LUA_REGISTRYINDEX);
            manager->_onDeinitializeFunctionRefs[thisAsset].push_back(referenceIndex);

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

    // Register enabled state
    // bool enabled
    ghoul::lua::push(*_luaState, asset->explicitEnabled().value_or(true));
    lua_setfield(*_luaState, assetTableIndex, "enabled");

    // Attach Asset table to AssetInfo table
    lua_setfield(*_luaState, assetInfoTableIndex, AssetTableName);

    // Extend global asset info table (pushed to the Lua stack earlier)
    // with this AssetInfo table
    const std::string path = asset->path().string();
    lua_setfield(*_luaState, globalTableIndex, path.c_str());
    lua_settop(*_luaState, top);
}

Asset* AssetManager::retrieveAsset(const std::filesystem::path& path,
                                   const std::filesystem::path& retriever,
                                   std::optional<bool> explicitEnable)
{
    // Check if asset is already loaded
    const auto it = std::find_if(
        _assets.cbegin(),
        _assets.cend(),
        [&path](const std::unique_ptr<Asset>& asset) { return asset->path() == path; }
    );
    if (it != _assets.end()) {
        Asset* a = it->get();
        // We should warn if an asset is requested twice with different enable settings or
        // else the resulting status will depend on the order of asset loading.
        if (a->explicitEnabled() != explicitEnable) {
            if (a->firstParent()) {
                // The first request came from another asset, so we can mention it in the
                // error message
                LWARNING(std::format(
                    "Loading asset {0} from {1} with enable state {3} different from "
                    "initial loading from {2} with state {4}. Only {4} will have an "
                    "effect",
                    path, retriever, a->firstParent()->path(),
                    explicitEnable.value_or(true), a->explicitEnabled().value_or(true)
                ));
            }
            else {
                // This can only happen if the asset was loaded from the profile directly,
                // in which case we don't have to warn the user since it won't depend on
                // the load order as it is always guaranteed that the profile assets are
                // loaded first
                ghoul_assert(
                    std::find(
                        _rootAssets.begin(),
                        _rootAssets.end(),
                        a
                    ) != _rootAssets.end(),
                    "Asset not loaded from profile"
                );
            }
        }
        return it->get();
    }

    if (!std::filesystem::is_regular_file(path)) {
        throw ghoul::RuntimeError(std::format(
            "Could not find asset file '{}' requested by '{}'",
            path, retriever
        ));
    }
    auto asset = std::make_unique<Asset>(*this, path, explicitEnable);
    Asset* res = asset.get();
    setUpAssetLuaTable(res);
    _assets.push_back(std::move(asset));
    return res;
}

void AssetManager::callOnInitialize(Asset* asset) const {
    ZoneScoped;
    ghoul_precondition(asset, "Asset must not be nullptr");

    auto it = _onInitializeFunctionRefs.find(asset);
    if (it == _onInitializeFunctionRefs.end()) {
        return;
    }

    for (const int init : it->second) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, init);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "When initializing '{}': {}",
                asset->path(),
                ghoul::lua::value<std::string>(*_luaState, -1)
            ));
        }
        // Clean up the stack, in case the pcall left anything there
        lua_settop(*_luaState, 0);
    }
}

void AssetManager::callOnDeinitialize(Asset* asset) const {
    ZoneScoped;
    ghoul_precondition(asset, "Asset must not be nullptr");

    auto it = _onDeinitializeFunctionRefs.find(asset);
    if (it == _onDeinitializeFunctionRefs.end()) {
        return;
    }

    for (const int deinit : it->second) {
        lua_rawgeti(*_luaState, LUA_REGISTRYINDEX, deinit);
        if (lua_pcall(*_luaState, 0, 0, 0) != LUA_OK) {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "When deinitializing '{}': {}",
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
        const std::string path = asset->path().string();
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

    const PathType pathType = classifyPath(assetPath);
    std::string prefix;
    if (pathType == PathType::RelativeToAsset) {
        prefix = baseDirectory.string() + '/';
    }
    else if (pathType == PathType::RelativeToAssetRoot) {
        prefix = _assetRootDirectory.string() + '/';
    }
    // We treat the Absolute and the Tokenized paths the same here since they will
    // behave the same when passed into the `absPath` function

    // Construct the full path including the .asset extension
    std::string fullAssetPath = prefix + assetPath;
    if (std::filesystem::path(assetPath).extension() != ".asset") {
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
            codegen::lua::Add,
            codegen::lua::Remove,
            codegen::lua::RemoveAll,
            codegen::lua::IsLoaded,
            codegen::lua::AllAssets,
            codegen::lua::RootAssets
        }
    };
}

} // namespace openspace
