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

Asset::Asset(AssetManager* loader, SynchronizationWatcher* watcher)
    : _state(State::SyncResolved)
    , _loader(loader)
    , _synchronizationWatcher(watcher)
    , _hasAssetPath(false)
    , _assetName("Root Asset")
{}

Asset::Asset(AssetManager* loader, SynchronizationWatcher* watcher, std::string assetPath)
    : _state(State::Unloaded)
    , _loader(loader)
    , _synchronizationWatcher(watcher)
    , _hasAssetPath(true)
    , _assetPath(std::move(assetPath))
{}

void Asset::setMetaInformation(MetaInformation metaInformation) {
    _metaInformation = std::move(metaInformation);
}

std::optional<Asset::MetaInformation> Asset::metaInformation() const {
    return _metaInformation;
}

Asset::State Asset::state() const {
    return _state;
}

void Asset::setState(Asset::State state) {
    ZoneScoped

    if (_state == state) {
        return;
    }
    _state = state;

    for (Asset* requiringAsset : _requiringAssets) {
        requiringAsset->requiredAssetChangedState(state);
    }

    for (Asset* requestingAsset : _requestingAssets) {
        requestingAsset->requestedAssetChangedState(this, state);
    }
}

void Asset::requiredAssetChangedState(Asset::State childState) {
    if (!isLoaded() || isInitialized() || _state == State::InitializationFailed) {
        // 1. Prohibit state change to SyncResolved if additional requirements may still
        //    be added
        // 2. Do not do anything if this asset was already initialized. This may happen if
        //    there are multiple requirement paths from this asset to the same child,
        //    which causes this method to be called more than once
        // 3. Do not do anything if the asset failed to initialize
        return;
    }

    if (childState == State::SyncResolved) {
        if (isSyncResolveReady()) {
            setState(State::SyncResolved);
        }
    }
    else if (childState == State::SyncRejected) {
        setState(State::SyncRejected);
    }
}

void Asset::requestedAssetChangedState(Asset* child, Asset::State childState) {
    if (child->hasInitializedParent()) {
        if (childState == State::Loaded && child->state() == State::Loaded) {
            child->startSynchronizations();
        }
        if (childState == State::SyncResolved && child->state() == State::SyncResolved) {
            child->initialize();
        }
    }
}

void Asset::addSynchronization(std::unique_ptr<ResourceSynchronization> synchronization) {
    std::shared_ptr<ResourceSynchronization> sync = std::move(synchronization);

    _synchronizations.push_back(sync);

    // Set up callback for synchronization state change
    // The synchronization watcher ensures that callbacks are invoked in the main thread

    SynchronizationWatcher::WatchHandle watch =
        _synchronizationWatcher->watchSynchronization(
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
                        "Failed to synchronize resource '{}' in asset '{}'",
                        sync->name(), id()
                    ));

                    setState(State::SyncRejected);
                }
            }
        );
    _syncWatches.push_back(watch);
}

void Asset::clearSynchronizations() {
    for (const SynchronizationWatcher::WatchHandle& h : _syncWatches) {
        _synchronizationWatcher->unwatchSynchronization(h);
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

std::vector<ResourceSynchronization*> Asset::ownSynchronizations() const {
    std::vector<ResourceSynchronization*> res;
    res.reserve(_synchronizations.size());
    std::transform(
        _synchronizations.begin(), _synchronizations.end(),
        std::back_inserter(res),
        std::mem_fn(&std::shared_ptr<ResourceSynchronization>::get)
    );

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
    for (auto it = _requiringAssets.begin(); it != _requiringAssets.end(); it++) {
        Asset* parent = *it;
        if (!parent) {
            it = _requiringAssets.erase(it);
            continue;
        }
        if (parent->isLoaded()) {
            return true;
        }
    }
    for (auto it = _requestingAssets.begin(); it != _requestingAssets.end(); it++) {
        Asset* parent = *it;
        if (!parent) {
            it = _requestingAssets.erase(it);
            continue;
        }
        if (parent->isLoaded() || parent->hasLoadedParent()) {
            return true;
        }
    }

    return false;
}

bool Asset::hasSyncingOrResolvedParent() const {
    for (Asset* p : _requiringAssets) {
        if (!p) {
            continue;
        }
        if (p->isSyncingOrResolved()) {
            return true;
        }
    }
    for (Asset* p : _requestingAssets) {
        if (!p) {
            continue;
        }
        if (p->isSyncingOrResolved() || p->hasSyncingOrResolvedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::hasInitializedParent() const {
    for (Asset* p : _requiringAssets) {
        if (!p) {
            continue;
        }
        if (p->isInitialized()) {
            return true;
        }
    }
    for (Asset* p : _requestingAssets) {
        if (!p) {
            continue;
        }
        if (p->isInitialized() || p->hasInitializedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::isInitialized() const {
    return _state == State::Initialized;
}

bool Asset::startSynchronizations() {
    if (!isLoaded()) {
        LWARNING(fmt::format("Cannot start synchronizations of unloaded asset {}", id()));
        return false;
    }
    for (Asset* child : requestedAssets()) {
        child->startSynchronizations();
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

bool Asset::cancelAllSynchronizations() {
    bool cancelledAnySync = std::any_of(
        _requiredAssets.cbegin(),
        _requiredAssets.cend(),
        std::mem_fn(&Asset::cancelAllSynchronizations)
    );
    cancelledAnySync |= std::any_of(
        _requestedAssets.cbegin(),
        _requestedAssets.cend(),
        std::mem_fn(&Asset::cancelAllSynchronizations)
    );

    for (const std::shared_ptr<ResourceSynchronization>& s : _synchronizations) {
        if (s->isSyncing()) {
            cancelledAnySync = true;
            s->cancel();
            setState(State::Loaded);
        }
    }
    if (cancelledAnySync) {
        setState(State::Loaded);
    }
    return cancelledAnySync;
}

bool Asset::cancelUnwantedSynchronizations() {
    if (hasSyncingOrResolvedParent()) {
        return false;
    }

    bool cancelledAnySync = std::any_of(
        _requiredAssets.begin(),
        _requiredAssets.end(),
        std::mem_fn(&Asset::cancelUnwantedSynchronizations)
    );
    cancelledAnySync |= std::any_of(
        _requestedAssets.begin(),
        _requestedAssets.end(),
        std::mem_fn(&Asset::cancelUnwantedSynchronizations)
    );

    for (const std::shared_ptr<ResourceSynchronization>& s : _synchronizations) {
        if (s->isSyncing()) {
            cancelledAnySync = true;
            s->cancel();
            setState(State::Loaded);
        }
    }
    if (cancelledAnySync) {
        setState(State::Loaded);
    }
    return cancelledAnySync;
}

bool Asset::load() {
    if (isLoaded()) {
        return true;
    }

    const bool loaded = _loader->loadAsset(this);
    setState(loaded ? State::Loaded : State::LoadingFailed);
    return loaded;
}

void Asset::unload() {
    if (!isLoaded()) {
        return;
    }

    setState(State::Unloaded);
    _loader->unloadAsset(this);

    for (Asset* child : _requiredAssets) {
        unrequire(child);
    }
    for (Asset* child : requestedAssets()) {
        unrequest(child);
    }
}

bool Asset::initialize() {
    ZoneScoped

    if (isInitialized()) {
        return true;
    }
    if (!isSynchronized()) {
        LERROR(fmt::format("Cannot initialize unsynchronized asset {}", id()));
        return false;
    }
    LDEBUG(fmt::format("Initializing asset '{}'", id()));

    // 1. Initialize requirements
    for (Asset* child : _requiredAssets) {
        child->initialize();
    }

    // 2. Initialize requests
    for (Asset* child : _requestedAssets) {
        if (child->isSynchronized()) {
            child->initialize();
        }
    }

    // 3. Call lua onInitialize
    try {
        _loader->callOnInitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format("Failed to initialize asset {}", id()));
        LERROR(fmt::format("{}: {}", e.component, e.message));
        // TODO: rollback;
        setState(State::InitializationFailed);
        return false;
    }

    // 4. Update state
    setState(State::Initialized);
    return true;
}

void Asset::deinitializeIfUnwanted() {
    if (hasInitializedParent()) {
        return;
    }
    deinitialize();
}

void Asset::deinitialize() {
    if (!isInitialized()) {
        return;
    }
    LDEBUG(fmt::format("Deintializing asset '{}'", id()));

    // Perform inverse actions as in initialize, in reverse order (4 - 1)

    // 4. Update state
    setState(Asset::State::SyncResolved);

    // 3. Call lua onInitialize
    try {
        _loader->callOnDeinitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Failed to deinitialize asset {}; {}: {}", id(), e.component, e.message
        ));
        return;
    }

    // 2 and 1. Deinitialize unwanted requirements and requests
    for (Asset* dependency : childAssets()) {
        dependency->deinitializeIfUnwanted();
    }
}

std::string Asset::id() const {
    return _hasAssetPath ? _assetPath : "$root";
}

std::filesystem::path Asset::assetFilePath() const {
    return _assetPath;
}

bool Asset::hasAssetFile() const {
    return _hasAssetPath;
}

std::string Asset::assetDirectory() const {
    return std::filesystem::path(_assetPath).parent_path().string();
}

const std::string& Asset::assetName() const {
    return _assetName;
}

void Asset::require(Asset* child) {
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot require child asset when already loaded");
    }

    const auto it = std::find(_requiredAssets.cbegin(), _requiredAssets.cend(), child);
    if (it != _requiredAssets.cend()) {
        // Do nothing if the requirement already exists.
        return;
    }

    _requiredAssets.push_back(child);
    child->_requiringAssets.push_back(this);

    if (!child->isLoaded()) {
        child->load();
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
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot unrequire child asset is in a loaded state");
    }

    const auto childIt = std::find(_requiredAssets.cbegin(), _requiredAssets.cend(), child);

    if (childIt == _requiredAssets.cend()) {
        // Do nothing if the request node not exist.
        return;
    }

    _requiredAssets.erase(childIt);

    const auto parentIt = std::find(
        child->_requiringAssets.cbegin(),
        child->_requiringAssets.cend(),
        this
    );
    if (parentIt == child->_requiringAssets.cend()) {
        return;
    }

    child->_requiringAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    if (!child->hasLoadedParent()) {
        child->unload();
    }
}

void Asset::request(Asset* child) {
    const auto it = std::find(_requestedAssets.cbegin(), _requestedAssets.cend(), child);
    if (it != _requestedAssets.cend()) {
        // Do nothing if the request already exists
        return;
    }

    _requestedAssets.push_back(child);
    child->_requestingAssets.push_back(this);

    if (!child->isLoaded()) {
        child->load();
    }

    if (isSynchronized() && child->isLoaded() && !child->isSynchronized()) {
        child->startSynchronizations();
    }

    if (isInitialized() && child->isSynchronized() && !child->isInitialized()) {
        child->initialize();
    }
}

void Asset::unrequest(Asset* child) {
    const auto childIt = std::find(
        _requestedAssets.cbegin(),
        _requestedAssets.cend(),
        child
    );
    if (childIt == _requestedAssets.cend()) {
        // Do nothing if the request node not exist
        return;
    }

    _requestedAssets.erase(childIt);

    const auto parentIt = std::find(
        child->_requestingAssets.cbegin(),
        child->_requestingAssets.cend(),
        this
    );
    if (parentIt == child->_requestingAssets.cend()) {
        return;
    }

    child->_requestingAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    if (!child->hasLoadedParent()) {
        child->unload();
    }
}

std::vector<Asset*> Asset::requestedAssets() const {
    std::vector<Asset*> res;
    res.reserve(_requestedAssets.size());
    for (Asset* a : _requestedAssets) {
        res.push_back(a);
    }
    return res;
}

std::vector<Asset*> Asset::childAssets() const {
    std::vector<Asset*> children;
    children.reserve(_requiredAssets.size() + _requestedAssets.size());

    for (Asset* a : _requiredAssets) {
        children.push_back(a);
    }
    for (Asset* a : _requestedAssets) {
        children.push_back(a);
    }
    return children;
}

} // namespace openspace
