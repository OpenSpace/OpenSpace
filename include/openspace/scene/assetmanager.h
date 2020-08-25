/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___ASSETMANAGER___H__
#define __OPENSPACE_CORE___ASSETMANAGER___H__

#include <openspace/scene/assetlistener.h>

#include <openspace/scene/assetloader.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <memory>
#include <unordered_map>
#include <vector>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Asset;
class AssetLoader;
class SynchronizationWatcher;

/**
 * Interface for managing assets.
 * The asset manager interface is only concerned with "top level" assets, and not their
 * dependencies. However, an asset is not considered synchronized before all its deps are
 * synchronized. Also, setting a target state of an asset to Unloaded will only unload an
 * asset from the system if it is not a dependency of a loaded asset.
 */
class AssetManager : AssetListener {
public:
    AssetManager(ghoul::lua::LuaState* state, std::string assetRootDirectory);

    virtual ~AssetManager() = default;

    void initialize();
    void deinitialize();
    void add(const std::string& path);
    void remove(const std::string& path);
    void removeAll();
    const Asset& rootAsset() const;
    Asset& rootAsset();

    void assetStateChanged(Asset* asset, Asset::State state) override;
    void assetRequested(Asset* parent, std::shared_ptr<Asset> child) override;
    void assetUnrequested(Asset* parent, std::shared_ptr<Asset> child) override;

    bool update();
    scripting::LuaLibrary luaLibrary();

private:
    std::unordered_map<std::string, bool> _pendingStateChangeCommands;
    std::mutex _pendingInitializationsMutex;
    std::vector<std::shared_ptr<Asset>> _pendingInitializations;

    SynchronizationWatcher _synchronizationWatcher;
    AssetLoader _assetLoader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
