/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/util/synchronizationwatcher.h>
#include <filesystem>
#include <optional>

namespace openspace {

class AssetManager;

class Asset {
public:
    struct MetaInformation {
        std::string name;
        std::string version;
        std::string description;
        std::string author;
        std::string url;
        std::string license;
        std::vector<std::string> identifiers;
    };

    Asset(AssetManager& manager, SynchronizationWatcher& watcher,
        std::filesystem::path assetPath);

    std::filesystem::path path() const;

    void addSynchronization(std::unique_ptr<ResourceSynchronization> synchronization);
    void clearSynchronizations();
    std::vector<ResourceSynchronization*> synchronizations() const;

    /**
     * Load this asset and return true if successful,
     * i.e. if this and all required assets loaded without errors.
     */
    bool load(Asset* parent);
    bool hasLoadedParent();
    bool isLoaded() const;
    void unload();

    /**
     * Start synchronizations of this asset and return true if all
     * its own synchronizations and required assets' synchronizations could start.
     */
    bool startSynchronizations();

    bool isSynchronized() const;

    /**
     * Initialize this asset and return true if successful,
     * i.e. if this and all required assets initialized without errors.
     */
    void initialize();
    bool isInitialized() const;
    void deinitialize();
    void deinitializeIfUnwanted();

    // Dependency graph
    void require(Asset* child);
    void unrequire(Asset* child);

    void setMetaInformation(MetaInformation metaInformation);
    std::optional<MetaInformation> metaInformation() const;

private:
    enum class State {
        Unloaded,
        LoadingFailed,
        Loaded,
        Synchronizing,
        SyncResolved,
        SyncRejected,
        Initialized,
        InitializationFailed
    };

    void setState(State state);

    bool isSyncingOrResolved() const;
    bool isSyncResolveReady() const;
    bool hasInitializedParent() const;

    std::atomic<State> _state = State::Unloaded;
    AssetManager& _manager;
    SynchronizationWatcher& _synchronizationWatcher;

    std::vector<std::shared_ptr<ResourceSynchronization>> _synchronizations;

    // Absolute path to asset file
    std::filesystem::path _assetPath;

    std::optional<MetaInformation> _metaInformation;

    // Required assets
    std::vector<Asset*> _requiredAssets;

    // Assets that refers to this asset as a required asset
    std::vector<Asset*> _requiringAssets;

    // Synchronization watches
    std::vector<SynchronizationWatcher::WatchHandle> _syncWatches;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET___H__
