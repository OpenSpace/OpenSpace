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

#include <openspace/scene/asset.h>

#include <openspace/scene/assetmanager.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <filesystem>
#include <unordered_set>

namespace openspace {

namespace {
    constexpr const char* _loggerCat = "Asset";
} // namespace

Asset::Asset(AssetManager& manager, SynchronizationWatcher& watcher,
             std::filesystem::path assetPath)
    : _manager(manager)
    , _synchronizationWatcher(watcher)
    , _assetPath(std::move(assetPath))
{}

void Asset::setMetaInformation(MetaInformation metaInformation) {
    _metaInformation = std::move(metaInformation);
}

std::optional<Asset::MetaInformation> Asset::metaInformation() const {
    return _metaInformation;
}

void Asset::setState(Asset::State state) {
    ZoneScoped

    if (_state == state) {
        return;
    }
    
    _state = state;

    for (Asset* requiringAsset : _requiringAssets) {
        if (// Prohibit state change to SyncResolved if additional requirements may still
            // be added
            !requiringAsset->isLoaded() ||
            // Do not do anything if this asset was already initialized. This may happen
            // if there are multiple requirement paths from this asset to the same child,
            // which causes this method to be called more than once
            requiringAsset->isInitialized() ||
            // Do not do anything if the asset failed to initialize
            requiringAsset->_state == State::InitializationFailed)
        {
            continue;
        }

        if (state == State::SyncResolved) {
            if (requiringAsset->isSyncResolveReady()) {
                requiringAsset->setState(State::SyncResolved);
            }
        }
        else if (state == State::SyncRejected) {
            requiringAsset->setState(State::SyncRejected);
        }
    }

    if (hasInitializedParent()) {
        if (state == State::Loaded && _state == State::Loaded) {
            startSynchronizations();
        }
        if (state == State::SyncResolved && _state == State::SyncResolved) {
            initialize();
        }
    }
}

void Asset::addSynchronization(std::unique_ptr<ResourceSynchronization> synchronization) {
    std::shared_ptr<ResourceSynchronization> sync = std::move(synchronization);

    _synchronizations.push_back(sync);

    // Set up callback for synchronization state change
    // The synchronization watcher ensures that callbacks are invoked in the main thread

    SynchronizationWatcher::WatchHandle watch =
        _synchronizationWatcher.watchSynchronization(
            sync,
            [this, sync](ResourceSynchronization::State state) {
                ZoneScoped

                if (state == ResourceSynchronization::State::Resolved) {
                    if (!isSynchronized() && isSyncResolveReady()) {
                        setState(State::SyncResolved);
                    }
                }
                else if (state == ResourceSynchronization::State::Rejected) {
                    LERROR(fmt::format(
                        "Failed to synchronize resource '{}' in asset {}",
                        sync->name(), _assetPath
                    ));

                    setState(State::SyncRejected);
                }
            }
        );
    _syncWatches.push_back(watch);
}

void Asset::clearSynchronizations() {
    for (const SynchronizationWatcher::WatchHandle& h : _syncWatches) {
        _synchronizationWatcher.unwatchSynchronization(h);
    }
    _syncWatches.clear();
}

bool Asset::isSyncResolveReady() const {
    const auto unsynchronizedAsset = std::find_if(
        _requiredAssets.cbegin(),
        _requiredAssets.cend(),
        [](Asset* a) { return !a->isSynchronized(); }
    );

    if (unsynchronizedAsset != _requiredAssets.cend()) {
        // Not considered resolved if there is one or more unresolved children
        return false;
    }

    const auto unresolvedOwnSynchronization = std::find_if(
        _synchronizations.cbegin(),
        _synchronizations.cend(),
        [](const std::shared_ptr<ResourceSynchronization>& s) { return !s->isResolved(); }
    );

    // To be considered resolved, all own synchronizations need to be resolved
    return unresolvedOwnSynchronization == _synchronizations.cend();
}

std::vector<ResourceSynchronization*> Asset::synchronizations() const {
    std::vector<ResourceSynchronization*> res;
    res.reserve(_synchronizations.size());
    for (const std::shared_ptr<ResourceSynchronization>& sync : _synchronizations) {
        res.push_back(sync.get());
    }
    return res;
}

bool Asset::isLoaded() const {
    return _state != State::Unloaded && _state != State::LoadingFailed;
}

bool Asset::isSynchronized() const {
    return _state == State::SyncResolved || _state == State::Initialized ||
           _state == State::InitializationFailed;
}

bool Asset::isSyncingOrResolved() const {
    return _state == State::Synchronizing || _state == State::SyncResolved ||
           _state == State::Initialized || _state == State::InitializationFailed;
}

bool Asset::hasLoadedParent() {
    return std::any_of(
        _requiringAssets.begin(),
        _requiringAssets.end(),
        std::mem_fn(&Asset::isLoaded)
    );
}

bool Asset::hasInitializedParent() const {
    return std::any_of(
        _requiringAssets.begin(),
        _requiringAssets.end(),
        std::mem_fn(&Asset::isInitialized)
    );
}

bool Asset::isInitialized() const {
    return _state == State::Initialized;
}

bool Asset::startSynchronizations() {
    if (!isLoaded()) {
        LWARNING(fmt::format(
            "Cannot start synchronizations of unloaded asset {}", _assetPath
        ));
        return false;
    }

    // Do not attempt to resync if this is already done
    if (isSyncingOrResolved()) {
        return _state != State::SyncResolved;
    }

    setState(State::Synchronizing);

    bool childFailed = false;

    // Start synchronization of all children first
    for (Asset* child : _requiredAssets) {
        if (!child->startSynchronizations()) {
            childFailed = true;
        }
    }

    // Now synchronize its own synchronizations
    for (const std::shared_ptr<ResourceSynchronization>& s : _synchronizations) {
        if (!s->isResolved()) {
            s->start();
        }
    }
    // If all syncs are resolved (or no syncs exist), mark as resolved
    if (!isInitialized() && isSyncResolveReady()) {
        setState(State::SyncResolved);
    }
    return !childFailed;
}

bool Asset::load(Asset* parent) {
    if (isLoaded()) {
        return true;
    }

    const bool loaded = _manager.loadAsset(this, parent);
    setState(loaded ? State::Loaded : State::LoadingFailed);
    return loaded;
}

void Asset::unload() {
    if (!isLoaded()) {
        return;
    }

    setState(State::Unloaded);
    _manager.unloadAsset(this);

    // This while loop looks a bit weird, but it is this way because the unrequire
    // function removes the asset passed into it from the requiredAssets list, so we are
    // guaranteed that the list is shrinking with each loop iteration
    while (!_requiredAssets.empty()) {
        Asset* child = _requiredAssets.front();
        unrequire(child);
    }
}

void Asset::initialize() {
    ZoneScoped

    if (isInitialized()) {
        return;
    }
    if (!isSynchronized()) {
        LERROR(fmt::format("Cannot initialize unsynchronized asset {}", _assetPath));
        return;
    }
    LDEBUG(fmt::format("Initializing asset {}", _assetPath));

    // 1. Initialize requirements
    for (Asset* child : _requiredAssets) {
        child->initialize();
    }

    // 2. Call lua onInitialize
    try {
        _manager.callOnInitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format("Failed to initialize asset {}", path()));
        LERROR(fmt::format("{}: {}", e.component, e.message));
        // TODO: rollback;
        setState(State::InitializationFailed);
        return;
    }

    // 3. Update state
    setState(State::Initialized);
}

void Asset::deinitializeIfUnwanted() {
    if (!hasInitializedParent()) {
        deinitialize();
    }
}

void Asset::deinitialize() {
    if (!isInitialized()) {
        return;
    }
    LDEBUG(fmt::format("Deinitializing asset {}", _assetPath));

    // Perform inverse actions as in initialize, in reverse order (4 - 1)

    // 3. Update state
    setState(Asset::State::SyncResolved);

    // 2. Call lua onInitialize
    try {
        _manager.callOnDeinitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Failed to deinitialize asset {}; {}: {}", _assetPath, e.component, e.message
        ));
        return;
    }

    // 1. Deinitialize unwanted requirements
    for (Asset* dependency : _requiredAssets) {
        dependency->deinitializeIfUnwanted();
    }
}

std::filesystem::path Asset::path() const {
    return _assetPath;
}

void Asset::require(Asset* child) {
    if (_state != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot require child asset when already loaded");
    }

    const auto it = std::find(_requiredAssets.cbegin(), _requiredAssets.cend(), child);
    if (it != _requiredAssets.cend()) {
        // Do nothing if the requirement already exists
        return;
    }

    _requiredAssets.push_back(child);
    child->_requiringAssets.push_back(this);

    if (!child->isLoaded()) {
        child->load(this);
    }
    if (!child->isLoaded()) {
        unrequire(child);
    }

    if (isSynchronized() && child->isLoaded() && !child->isSynchronized()) {
        child->startSynchronizations();
    }

    if (isInitialized()) {
        if (child->isSynchronized() && !child->isInitialized()) {
            child->initialize();
        }
        if (!child->isInitialized()) {
            unrequire(child);
        }
    }
}

void Asset::unrequire(Asset* child) {
    ghoul_assert(
        _state == Asset::State::Unloaded,
        "Cannot unrequire child asset in a loaded state"
    );
    auto childIt = std::find(_requiredAssets.begin(), _requiredAssets.end(), child);
    ghoul_assert(
        childIt != _requiredAssets.end(),
        "Requested node must exist in the parent"
    );

    _requiredAssets.erase(childIt);



    auto parentIt = std::find(
        child->_requiringAssets.cbegin(),
        child->_requiringAssets.cend(),
        this
    );
    ghoul_assert(
        parentIt != child->_requiringAssets.cend(),
        "Parent asset was not correctly registered"
    );

    child->_requiringAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    if (!child->hasLoadedParent()) {
        child->unload();
    }
}

} // namespace openspace
