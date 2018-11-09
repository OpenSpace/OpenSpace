/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <memory>
#include <vector>
#include <unordered_map>

namespace openspace {

namespace scripting { struct LuaLibrary; }

class Asset;
class AssetLoader;
class SynchronizationWatcher;

/**
 * Interface for managing assets.
 * The asset manager interface is only concerned with "top level" assets,
 * i.e. assets that are loaded using setTargetAssetState, and not their dependencies.
 * However, an asset is not considered synchronized before all its deps are
 * synchronized.
 * Also, setting a target state of an asset to Unloaded will only unload an asset
 * from the system if it is not a dependency of a loaded asset.
 */

class AssetManager : AssetListener {
public:
    AssetManager(std::unique_ptr<AssetLoader> loader,
        std::unique_ptr<SynchronizationWatcher> syncWatcher);

    virtual ~AssetManager() = default;

    void initialize();
    void deinitialize();
    void add(const std::string& path);
    void remove(const std::string& path);
    void removeAll();
    std::shared_ptr<Asset> rootAsset();

    void assetStateChanged(std::shared_ptr<Asset> asset, Asset::State state) override;
    void assetRequested(std::shared_ptr<Asset> parent,
        std::shared_ptr<Asset> child) override;
    void assetUnrequested(std::shared_ptr<Asset> parent,
        std::shared_ptr<Asset> child) override;

    bool update();
    scripting::LuaLibrary luaLibrary();

private:
    std::unordered_map<std::string, bool> _pendingStateChangeCommands;
    std::mutex _pendingInitializationsMutex;
    std::vector<std::shared_ptr<Asset>> _pendingInitializations;

    std::unique_ptr<SynchronizationWatcher> _synchronizationWatcher;
    std::unique_ptr<AssetLoader> _assetLoader;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSETMANAGER___H__
