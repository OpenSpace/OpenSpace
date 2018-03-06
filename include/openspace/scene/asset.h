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

#ifndef __OPENSPACE_CORE___ASSET___H__
#define __OPENSPACE_CORE___ASSET___H__

#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/synchronizationwatcher.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>

#include <iterator>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace openspace {

class AssetLoader;

class Asset : public std::enable_shared_from_this<Asset> {
public:
    enum class State : unsigned int {
        Unloaded,
        LoadingFailed,
        Loaded,
        Synchronizing,
        SyncResolved,
        SyncRejected,
        Initialized,
        InitializationFailed
    };

    using StateChangeCallback = std::function<void(State)>;
    using CallbackHandle = size_t;

    /**
     * Root asset constructor
     */
    Asset(AssetLoader* loader, SynchronizationWatcher* watcher);

    /**
    * Regular asset constructor
    */
    Asset(AssetLoader* loader,
        SynchronizationWatcher* watcher,
        ghoul::filesystem::File assetPath);

    ~Asset();

    std::string id() const;
    std::string assetFilePath() const;
    bool hasAssetFile() const;
    std::string assetDirectory() const;
    std::string assetName() const;
    AssetLoader* loader() const;
    State state() const;

    void addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization);
    void clearSynchronizations();
    std::vector<std::shared_ptr<ResourceSynchronization>> ownSynchronizations() const;

    void syncStateChanged(ResourceSynchronization::State s);

    /**
     * Load this asset and return true if successful,
     * i.e. if this and all required assets loaded without errors.
     */
    bool load();
    bool hasLoadedParent();
    bool isLoaded() const;
    void unload();
    void unloadIfUnwanted();

    /**
     * Start synchronizations of this asset and return true if all
     * its own synchronizations and required assets' synchronizations could start.
     */
    bool startSynchronizations();
    bool hasSyncingOrResolvedParent() const;
    bool isSynchronized() const;
    bool isSyncingOrResolved() const;
    bool cancelAllSynchronizations();
    bool cancelUnwantedSynchronizations();
    bool restartAllSynchronizations();
    float requestedSynchronizationProgress();
    float requiredSynchronizationProgress();

    /**
     * Initialize this asset and return true if successful,
     * i.e. if this and all required assets initialized without errors.
     */
    bool initialize();
    bool hasInitializedParent() const;
    bool isInitialized() const;
    void deinitialize();
    void deinitializeIfUnwanted();

    // Dependency graph
    bool requires(const Asset* asset) const;
    void require(std::shared_ptr<Asset> asset);
    void unrequire(std::shared_ptr<Asset> asset);

    bool requests(const Asset* child) const;
    void request(std::shared_ptr<Asset> child);
    void unrequest(std::shared_ptr<Asset> child);

    std::vector<std::shared_ptr<Asset>> requestedAssets() const;
    std::vector<std::shared_ptr<Asset>> requestingAssets() const;
    std::vector<std::shared_ptr<Asset>> requiredAssets() const;
    std::vector<std::shared_ptr<Asset>> requiringAssets() const;

    std::vector<std::shared_ptr<Asset>> requiredSubTreeAssets();
    std::vector<std::shared_ptr<Asset>> subTreeAssets();
    std::vector<std::shared_ptr<Asset>> childAssets() const;
    std::vector<std::shared_ptr<Asset>> parentAssets() const;

    bool isRequired() const;
    bool isRequested() const;
    bool shouldBeInitialized() const;

    std::string resolveLocalResource(std::string resourceName);

private:
    void setState(State state);

    void requiredAssetChangedState(std::shared_ptr<Asset> a, Asset::State state);
    void requestedAssetChangedState(std::shared_ptr<Asset> a, Asset::State state);

    bool isSyncResolveReady();

    std::atomic<State> _state;
    AssetLoader* _loader;
    SynchronizationWatcher* _synchronizationWatcher;

    std::vector<std::shared_ptr<ResourceSynchronization>> _synchronizations;

    bool _hasAssetPath;
    // The name of the asset
    std::string _assetName;

    // Absolute path to asset file
    std::string _assetPath;

    // Required assets
    std::vector<std::shared_ptr<Asset>> _requiredAssets;

    // Assets that refers to this asset as a required asset
    std::vector<std::weak_ptr<Asset>> _requiringAssets;

    // Requested assets
    std::vector<std::shared_ptr<Asset>> _requestedAssets;

    // Assets that refers to this asset as a requested asset
    std::vector<std::weak_ptr<Asset>> _requestingAssets;

    // Synchronization watches
    std::vector<SynchronizationWatcher::WatchHandle> _syncWatches;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET___H__
