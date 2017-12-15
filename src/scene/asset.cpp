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

#include <openspace/scene/asset.h>
#include <openspace/scene/assetloader.h>

#include <ghoul/logging/logmanager.h>

#include <algorithm>
#include <unordered_set>

namespace {
    const char* _loggerCat = "Asset";

    float syncProgress(std::vector<std::shared_ptr<openspace::Asset>> assets) {
        size_t nTotalBytes = 0;
        size_t nSyncedBytes = 0;

        for (const auto& a : assets) {
            const std::vector<std::shared_ptr<openspace::ResourceSynchronization>> syncs =
                a->ownSynchronizations();

            for (const auto& sync : syncs) {
                if (sync->nTotalBytesIsKnown()) {
                    nTotalBytes += sync->nTotalBytes();
                    nSyncedBytes += sync->nSynchronizedBytes();
                } else if (sync->isSyncing()) {
                    // A resource is still synchronizing but its size is unknown.
                    // Impossible to know the global progress.
                    return 0;
                }
            }
        }
        if (nTotalBytes == 0) {
            return 0.f;
        }
        return static_cast<float>(nSyncedBytes) / static_cast<float>(nTotalBytes);
    }
}

namespace openspace {

Asset::Asset(AssetLoader* loader, SynchronizationWatcher* syncWatcher)
    : _state(State::SyncResolved)
    , _loader(loader)
    , _synchronizationWatcher(syncWatcher)
    , _hasAssetPath(false)
    , _assetName("Root Asset")
{}

Asset::Asset(AssetLoader* loader,
    SynchronizationWatcher* syncWatcher,
    ghoul::filesystem::File assetPath
)
    : _state(State::Unloaded)
    , _loader(loader)
    , _synchronizationWatcher(syncWatcher)
    , _hasAssetPath(true)
    , _assetPath(assetPath)
{}

Asset::~Asset() {
    for (const SynchronizationWatcher::WatchHandle& h : _syncWatches) {
        _synchronizationWatcher->unwatchSynchronization(h);
    }
    _loader->untrackAsset(this);
}

std::string Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = assetDirectory();
    return currentAssetDirectory +
        ghoul::filesystem::FileSystem::PathSeparator +
        resourceName;
}

Asset::State Asset::state() const {
    return _state;
}

void Asset::setState(Asset::State state) {
    if (_state == state) {
        return;
    }
    _state = state;

    std::shared_ptr<Asset> thisAsset = shared_from_this();
    _loader->assetStateChanged(thisAsset, state);

    for (auto& requiringAsset : _requiringAssets) {
        if (std::shared_ptr<Asset> a = requiringAsset.lock()) {
            a->requiredAssetChangedState(thisAsset);
        }
    }

    for (auto& requestingAsset : _requestingAssets) {
        if (std::shared_ptr<Asset> a = requestingAsset.lock()) {
            a->requestedAssetChangedState(thisAsset);
        }
    }
}

void Asset::requiredAssetChangedState(std::shared_ptr<Asset> child) {
    ghoul_assert(!isInitialized(),
        "Required asset changing state while parent asset is initialized");

    State childState = child->state();
    if (childState == State::SyncResolved) {
        if (isSyncResolveReady()) {
            setState(State::SyncResolved);
        }
    } else if (childState == State::SyncRejected) {
        setState(State::SyncRejected);
    }
}

void Asset::requestedAssetChangedState(std::shared_ptr<Asset> child) {
    State childState = child->state();
    if (child->hasInitializedParent()) {
        if (childState == State::Loaded) {
            child->startSynchronizations();
        }
        if (childState == State::SyncResolved) {
            child->initialize();
        }
    }
}

void Asset::addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization) {
    _synchronizations.push_back(synchronization);    
   
    // Set up callback for synchronization state change
    // The synchronization watcher will make sure that callbacks are invoked in the main thread.
    SynchronizationWatcher::WatchHandle watch = _synchronizationWatcher->watchSynchronization(
        synchronization,
        [this, synchronization](ResourceSynchronization::State state) {
            syncStateChanged(synchronization, state);
        }
    );
    _syncWatches.push_back(watch);
}
    
void Asset::syncStateChanged(std::shared_ptr<ResourceSynchronization> synchronization,
                             ResourceSynchronization::State state)
{
    ghoul_assert(!isInitialized(),
        "Synchronization changing state while asset is initialized");

    if (state == ResourceSynchronization::State::Resolved) {
        if (!isSynchronized() && isSyncResolveReady()) {
            setState(State::SyncResolved);
        }
    } else if (state == ResourceSynchronization::State::Rejected) {
        setState(State::SyncRejected);
    }
}

bool Asset::isSyncResolveReady() {
    std::vector<std::shared_ptr<Asset>> requiredAssets = this->requiredAssets();

    auto pendingRequiredAsset = std::find_if(
        requiredAssets.begin(),
        requiredAssets.end(),
        [](std::shared_ptr<Asset>& a) {
            return a->state() == Asset::State::Loaded ||
                a->state() == Asset::State::Synchronizing;
        }
    );

    if (pendingRequiredAsset != requiredAssets.end()) {
        // Not considered resolved if all children are not resolved
        return false;
    }

    std::vector<std::shared_ptr<ResourceSynchronization>> syncs =
        this->ownSynchronizations();

    auto unresolvedOwnSynchronization = std::find_if(
        syncs.begin(),
        syncs.end(),
        [](std::shared_ptr<ResourceSynchronization>& s) {
            return !s->isResolved();
        }
    );

    // To be considered resolved, all own synchronizations need to be resolved
    return unresolvedOwnSynchronization == syncs.end();
}

std::vector<std::shared_ptr<ResourceSynchronization>> Asset::ownSynchronizations() const {
    return _synchronizations;
}

std::vector<std::shared_ptr<Asset>> Asset::subTreeAssets() {
    std::unordered_set<std::shared_ptr<Asset>> assets({ shared_from_this() });
    for (auto& c : childAssets()) {
        std::vector<std::shared_ptr<Asset>> subTree = c->subTreeAssets();
        std::copy(subTree.begin(), subTree.end(), std::inserter(assets, assets.end()));
    }
    std::vector<std::shared_ptr<Asset>> assetVector(assets.begin(), assets.end());
    return assetVector;
}

std::vector<std::shared_ptr<Asset>> Asset::requiredSubTreeAssets() {
    std::unordered_set<std::shared_ptr<Asset>> assets({ shared_from_this() });
    for (const auto& dep : _requiredAssets) {
        std::vector<std::shared_ptr<Asset>> subTree = dep->requiredSubTreeAssets();
        std::copy(subTree.begin(), subTree.end(), std::inserter(assets, assets.end()));
    }
    std::vector<std::shared_ptr<Asset>> assetVector(assets.begin(), assets.end());
    return assetVector;
}

bool Asset::isLoaded() const {
    State s = state();
    return s != State::Unloaded &&
        s != State::LoadingFailed;
}

bool Asset::isSynchronized() const {
    State s = state();
    return s == State::SyncResolved ||
           s == State::Initialized ||
           s == State::InitializationFailed;
}

bool Asset::isSyncingOrResolved() const {
    State s = state();
    return s == State::Synchronizing ||
        s == State::SyncResolved ||
        s == State::Initialized ||
        s == State::InitializationFailed;
}

bool Asset::hasLoadedParent() const {
    for (const auto& p : _requiringAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isLoaded()) {
            return true;
        }
    }
    for (const auto& p : _requestingAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isLoaded()) {
            return true;
        }
        if (parent->hasLoadedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::hasSyncingOrResolvedParent() const {
    for (const auto& p : _requiringAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isSyncingOrResolved()) {
            return true;
        }
    }
    for (const auto& p : _requestingAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isSyncingOrResolved()) {
            return true;
        }
        if (parent->hasSyncingOrResolvedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::hasInitializedParent() const {
    for (const auto& p : _requiringAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isInitialized()) {
            return true;
        }
    }
    for (const auto& p : _requestingAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isInitialized()) {
            return true;
        }
        if (parent->hasInitializedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::isInitialized() const {
    State s = state();
    return s == State::Initialized;
}

bool Asset::startSynchronizations() {
    // Do not attempt to resync if this is already initialized
    if (state() == State::Initialized) {
        return false;
    }

    setState(State::Synchronizing);

    bool foundUnresolved = false;

    // Start synchronization of all children first.
    for (auto& child : requiredAssets()) {
        if (child->startSynchronizations()) {
            foundUnresolved = true;
        }
    }
    for (auto& child : requestedAssets()) {
        child->startSynchronizations();
    }

    // Now synchronize its own synchronizations.
    for (const auto& s : ownSynchronizations()) {
        if (!s->isResolved()) {
            foundUnresolved = true;
            s->start();
        }
    }
    // If all syncs are resolved (or no syncs exist), mark as resolved.
    if (!foundUnresolved) {
        setState(State::SyncResolved);
    }
    return foundUnresolved;
}

bool Asset::cancelAllSynchronizations() {
    bool cancelledAnySync = false;
    for (auto& child : childAssets()) {
        bool cancelled = child->cancelAllSynchronizations();
        if (cancelled) {
            cancelledAnySync = true;
        }
    }
    for (const auto& s : ownSynchronizations()) {
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
    bool cancelledAnySync = false;
    for (auto& child : childAssets()) {
        bool cancelled = child->cancelUnwantedSynchronizations();
        if (cancelled) {
            cancelledAnySync = true;
        }
    }
    for (const auto& s : ownSynchronizations()) {
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

bool Asset::restartAllSynchronizations() {
    cancelAllSynchronizations();
    return startSynchronizations();
}

float Asset::requiredSynchronizationProgress() {
    std::vector<std::shared_ptr<Asset>> assets = requiredSubTreeAssets();
    return syncProgress(assets);

}

float Asset::requestedSynchronizationProgress() {
    std::vector<std::shared_ptr<Asset>> assets = subTreeAssets();
    return syncProgress(assets);
}

void Asset::load() {
    LDEBUG("Loading asset " << id());

    if (isLoaded()) {
        return;
    }

    bool loaded = loader()->loadAsset(shared_from_this());  
    setState(loaded ? State::Loaded : State::LoadingFailed);
}

void Asset::unloadIfUnwanted() {
    if (hasLoadedParent()) {
        return;
    }
    unload();
}

void Asset::unload() {
    LDEBUG("Unloading asset " << id());

    if (!isLoaded()) {
        return;
    }

    loader()->unloadAsset(shared_from_this());
    setState(State::Unloaded);

    for (std::shared_ptr<Asset> child : requiredAssets()) {
        unrequire(child);
    }
    for (std::shared_ptr<Asset> child : requestedAssets()) {
        unrequest(child);
    }
}

void Asset::initialize() {
    if (isInitialized()) {
        return;
    }
    if (!isSynchronized()) {
        LERROR("Cannot initialize unsynchronized asset " << id());
        return;
    }
    LDEBUG("Initializing asset " << id());

    // 1. Initialize required children
    for (auto& child : _requiredAssets) {
        child->initialize();
    }

    // 2. Call onInitialize in Lua
    try {
        loader()->callOnInitialize(this);
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR("Failed to initialize asset " << id() << ". " <<
            e.component << ": " << e.message);
        // TODO: rollback;
        return;
    }

    // 3. Update the internal state
    setState(Asset::State::Initialized);

    // 4. Call dependency initialization functions
    // Now that both this and all children are initialized.
    for (auto& child : _requiredAssets) {
        try {
            loader()->callOnDependencyInitialize(child.get(), this);
        } catch (const ghoul::lua::LuaRuntimeException& e) {
            LERROR("Failed to initialize required asset " <<
                child->id() << " of " << id() << ". " <<
                e.component << ": " << e.message);
            // TODO: rollback;
        }
    }

    // 5. Call dependency initialization function of the child and this
    // if the requested child was initialized before this.
    for (auto& child : _requestedAssets) {
        if (child->isInitialized()) {
            try {
                loader()->callOnDependencyInitialize(child.get(), this);
            } catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR("Failed to initialize requested asset " <<
                    child->id() << " of " << id() << ". " <<
                    e.component << ": " << e.message);
                // TODO: rollback;
            }
        }
    }

    // 6. Ask requested children to initialize if they are not already initialized.
    for (auto& child : _requestedAssets) {
        if (child->isSynchronized()) {
            child->initialize();
        }
    }

    // 7. Call dependency initialization function of this and the parent
    // if the requesting parent was initialized before this.
    for (auto& parent : _requestingAssets) {
        std::shared_ptr<Asset> p = parent.lock();
        if (p && p->state() == State::Initialized) {
            try {
                loader()->callOnDependencyInitialize(this, p.get());
            } catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR("Failed to initialize required asset " <<
                    id() << " of " << p->id() << ". " <<
                    e.component << ": " << e.message);
                // TODO: rollback;
            }
        }
    }

}

void Asset::deinitializeIfUnwanted() {
    if (hasInitializedParent()) {
        return;
    }
    deinitialize();
}

void Asset::deinitialize() {
    if (state() != Asset::State::Initialized) {
        return;
    }

    LDEBUG("Denitializing asset " << id());

    // Notify children
    for (auto& dependency : _requiredAssets) {
        try {
            loader()->callOnDependencyDeinitialize(dependency.get(), this);
        }
        catch (const ghoul::lua::LuaRuntimeException& e) {
            LERROR("Failed to deinitialize requested asset " <<
                dependency->id() << " of " << id() << ". " <<
                e.component << ": " << e.message);
            // TODO: rollback?
        }
    }
    for (auto& dependency : _requestedAssets) {
        if (dependency->state() == State::Initialized) {
            try {
                loader()->callOnDependencyDeinitialize(dependency.get(), this);
            }
            catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR("Failed to deinitialize requested asset " <<
                    dependency->id() << " of " << id() << ". " <<
                    e.component << ": " << e.message);
                // TODO: rollback?
            }
        }
    }


    try {
        loader()->callOnDeinitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR("Failed to deinitialize asset " << id() << ". " <<
            e.component << ": " << e.message);
        // TODO: rollback?
        return;
    }
    // Call onDeinitialize in Lua

    setState(Asset::State::Loaded);

    // Make sure no dependencies are left dangling
    for (auto& dependency : childAssets()) {
        dependency->deinitializeIfUnwanted();
    }
}

std::string Asset::id() const {
    return _hasAssetPath ? _assetPath : "$root";
}

std::string Asset::assetFilePath() const {
    return _assetPath;
}

bool Asset::hasAssetFile() const {
    return _hasAssetPath;
}

std::string Asset::assetDirectory() const {
    return ghoul::filesystem::File(_assetPath).directoryName();
}

std::string Asset::assetName() const {
    return _assetName;
}

AssetLoader* Asset::loader() const {
    return _loader;
}

bool Asset::requires(const Asset* asset) const {
    const auto it = std::find_if(
        _requiredAssets.begin(),
        _requiredAssets.end(),
        [asset](std::shared_ptr<Asset> dep) {
            return dep.get() == asset;
        }
    );
    return it != _requiredAssets.end();
}

void Asset::require(std::shared_ptr<Asset> child) {
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot require child asset when already loaded");
    }

    auto it = std::find(_requiredAssets.begin(),
                        _requiredAssets.end(),
                        child);

    if (it != _requiredAssets.end()) {
        // Do nothing if the requirement already exists.
        return;
    }
    _requiredAssets.push_back(child);

    
    child->_requiringAssets.push_back(shared_from_this());

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

void Asset::unrequire(std::shared_ptr<Asset> child) {
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot unrequire child asset is in a loaded state");
    }

    auto childIt = std::find(
        _requiredAssets.begin(),
        _requiredAssets.end(),
        child);

    if (childIt == _requiredAssets.end()) {
        // Do nothing if the request node not exist.
        return;
    }

    _requiredAssets.erase(childIt);

    auto parentIt = std::find_if(
        child->_requiringAssets.begin(),
        child->_requiringAssets.end(),
        [this](std::weak_ptr<Asset> a) {
            return a.lock().get() == this;
        }
    );

    if (parentIt == child->_requiringAssets.end()) {
        return;
    }

    child->_requiringAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    child->unloadIfUnwanted();
}


void Asset::request(std::shared_ptr<Asset> child) {
    auto it = std::find(_requestedAssets.begin(),
        _requestedAssets.end(),
        child);

    if (it != _requestedAssets.end()) {
        // Do nothing if the request already exists.
        return;
    }
    _requestedAssets.push_back(child);
    
    child->_requestingAssets.push_back(shared_from_this());

    if (!child->isLoaded()) {
        child->load();
    }
    if (!child->isLoaded()) {
        unrequest(child);
    }

    if (isSynchronized()) {
        if (child->isLoaded() && !child->isSynchronized()) {
            child->startSynchronizations();
        }
    }

    if (isInitialized()) {
        if (child->isSynchronized() && !child->isInitialized()) {
            child->initialize();
        }
        if (!child->isInitialized()) {
            unrequest(child);
        }
    }
}

void Asset::unrequest(std::shared_ptr<Asset> child) {
    auto childIt = std::find(
        _requestedAssets.begin(),
        _requestedAssets.end(),
        child);

    if (childIt == _requestedAssets.end()) {
        // Do nothing if the request node not exist.
        return;
    }
  
    _requestedAssets.erase(childIt);

    auto parentIt = std::find_if(
        child->_requestingAssets.begin(),
        child->_requestingAssets.end(),
        [this](std::weak_ptr<Asset> a) {
            return a.lock().get() == this;
        }
    );

    if (parentIt == child->_requestingAssets.end()) {
        return;
    }

    child->_requestingAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    child->unloadIfUnwanted();
}

bool Asset::requests(const Asset* asset) const {
    const auto it = std::find_if(
        _requestedAssets.begin(),
        _requestedAssets.end(),
        [asset](std::shared_ptr<Asset> dep) {
           return dep.get() == asset;
        }
    );
    return it != _requiredAssets.end();
}

std::vector<std::shared_ptr<Asset>> Asset::requiredAssets() const {
    return _requiredAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requiringAssets() const {
    std::vector<std::shared_ptr<Asset>> assets;
    assets.reserve(_requiringAssets.size());
    for (auto& a : _requiringAssets) {
        std::shared_ptr<Asset> shared = a.lock();
        if (shared) {
            assets.push_back(shared);
        }
    }
    return assets;
}

std::vector<std::shared_ptr<Asset>> Asset::requestedAssets() const {
    return _requestedAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requestingAssets() const {
    std::vector<std::shared_ptr<Asset>> assets;
    assets.reserve(_requestingAssets.size());
    for (auto& a : _requestingAssets) {
        std::shared_ptr<Asset> shared = a.lock();
        if (shared) {
            assets.push_back(shared);
        }
    }
    return assets;
}

std::vector<std::shared_ptr<Asset>> Asset::childAssets() const {
    std::vector<std::shared_ptr<Asset>> children;
    children.reserve(_requiredAssets.size() + _requestedAssets.size());
    children.insert(children.end(), _requiredAssets.begin(), _requiredAssets.end());
    children.insert(children.end(), _requestedAssets.begin(), _requestedAssets.end());
    return children;
}

std::vector<std::shared_ptr<Asset>> Asset::parentAssets() const {
    std::vector<std::shared_ptr<Asset>> parents;
    std::vector<std::shared_ptr<Asset>> requiring = requiringAssets();
    std::vector<std::shared_ptr<Asset>> requesting = requestingAssets();
    parents.reserve(requiring.size() + requesting.size());
    parents.insert(parents.end(), requiring.begin(), requiring.end());
    parents.insert(parents.end(), requesting.begin(), requesting.end());
    return parents;
}


bool Asset::isRequired() const {
    return !_requiringAssets.empty();
}

bool Asset::isRequested() const {
    return !_requestingAssets.empty();
}

bool Asset::shouldBeInitialized() const {
    std::vector<std::shared_ptr<Asset>> parents = parentAssets();
    const auto initializedAsset = std::find_if(
        parents.begin(),
        parents.end(),
        [](const std::shared_ptr<Asset>& a) {
            return a->state() == State::Initialized;
        }
    );

    return initializedAsset != parents.end();
}

}
