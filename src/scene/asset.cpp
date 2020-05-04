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

#include <openspace/scene/asset.h>

#include <openspace/scene/assetloader.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <algorithm>
#include <unordered_set>

namespace {
    constexpr const char* _loggerCat = "Asset";

    float syncProgress(const std::vector<std::shared_ptr<const openspace::Asset>>& assets)
    {
        size_t nTotalBytes = 0;
        size_t nSyncedBytes = 0;

        for (const std::shared_ptr<const openspace::Asset>& a : assets) {
            const std::vector<std::shared_ptr<openspace::ResourceSynchronization>>& s =
                a->ownSynchronizations();

            for (const std::shared_ptr<openspace::ResourceSynchronization>& sync : s) {
                if (sync->nTotalBytesIsKnown()) {
                    nTotalBytes += sync->nTotalBytes();
                    nSyncedBytes += sync->nSynchronizedBytes();
                }
                else if (sync->isSyncing()) {
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
} // namespace

namespace openspace {

Asset::Asset(AssetLoader* loader, SynchronizationWatcher* watcher)
    : _state(State::SyncResolved)
    , _loader(loader)
    , _synchronizationWatcher(watcher)
    , _hasAssetPath(false)
    , _assetName("Root Asset")
{}

Asset::Asset(AssetLoader* loader, SynchronizationWatcher* watcher, std::string assetPath)
    : _state(State::Unloaded)
    , _loader(loader)
    , _synchronizationWatcher(watcher)
    , _hasAssetPath(true)
    , _assetPath(std::move(assetPath))
{}

std::string Asset::resolveLocalResource(std::string resourceName) const {
    std::string dir = assetDirectory();
    return dir + ghoul::filesystem::FileSystem::PathSeparator + std::move(resourceName);
}

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
    if (_state == state) {
        return;
    }
    for (const std::weak_ptr<Asset>& requiringAsset : _requiringAssets) {
        if (std::shared_ptr<Asset> a = requiringAsset.lock()) {
            ghoul_assert(
                !a->isInitialized(),
                "Required asset changing state while parent asset is initialized"
            );
        }
    }
    _state = state;

    _loader->assetStateChanged(this, state);

    for (const std::weak_ptr<Asset>& requiringAsset : _requiringAssets) {
        if (std::shared_ptr<Asset> a = requiringAsset.lock()) {
            a->requiredAssetChangedState(state);
        }
    }

    for (const std::weak_ptr<Asset>& requestingAsset : _requestingAssets) {
        if (std::shared_ptr<Asset> a = requestingAsset.lock()) {
            a->requestedAssetChangedState(this, state);
        }
    }
}

void Asset::requiredAssetChangedState(Asset::State childState) {
    if (!isLoaded()) {
        // Prohibit state change to SyncResolved if additional requirements
        // may still be added
        return;
    }
    if (isInitialized()) {
        // Do not do anything if this asset was already initialized. This may happen if
        // there are multiple requirement paths from this asset to the same child, which
        // causes this method to be called more than once
        return;
    }
    if (_state == State::InitializationFailed) {
        // Do not do anything if the asset failed to initialize
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

void Asset::addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization) {
    _synchronizations.push_back(synchronization);

    // Set up callback for synchronization state change
    // The synchronization watcher will make sure that callbacks
    // are invoked in the main thread.

    SynchronizationWatcher::WatchHandle watch =
        _synchronizationWatcher->watchSynchronization(
            synchronization,
            [this, synchronization](ResourceSynchronization::State state) {
                syncStateChanged(synchronization.get(), state);
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

void Asset::syncStateChanged(ResourceSynchronization* sync,
                             ResourceSynchronization::State state)
{

    if (state == ResourceSynchronization::State::Resolved) {
        if (!isSynchronized() && isSyncResolveReady()) {
            setState(State::SyncResolved);
        }
    }
    else if (state == ResourceSynchronization::State::Rejected) {
        LERROR(fmt::format(
            "Failed to synchronize resource '{}'' in asset '{}'", sync->name(), id()
        ));

        setState(State::SyncRejected);
    }
}

bool Asset::isSyncResolveReady() {
    std::vector<std::shared_ptr<Asset>> requiredAssets = this->requiredAssets();

    auto unsynchronizedAsset = std::find_if(
        requiredAssets.cbegin(),
        requiredAssets.cend(),
        [](const std::shared_ptr<Asset>& a) { return !a->isSynchronized(); }
    );

    if (unsynchronizedAsset != requiredAssets.cend()) {
        // Not considered resolved if there is one or more unresolved children
        return false;
    }

    const std::vector<std::shared_ptr<ResourceSynchronization>>& syncs =
        ownSynchronizations();

    auto unresolvedOwnSynchronization = std::find_if(
        syncs.cbegin(),
        syncs.cend(),
        [](const std::shared_ptr<ResourceSynchronization>& s) { return !s->isResolved(); }
    );

    // To be considered resolved, all own synchronizations need to be resolved
    return unresolvedOwnSynchronization == syncs.cend();
}

const std::vector<std::shared_ptr<ResourceSynchronization>>&
Asset::ownSynchronizations() const
{
    return _synchronizations;
}

std::vector<std::shared_ptr<const Asset>> Asset::subTreeAssets() const {
    std::unordered_set<std::shared_ptr<const Asset>> assets({ shared_from_this() });
    for (const std::shared_ptr<Asset>& c : childAssets()) {
        if (c.get() == this) {
            throw ghoul::RuntimeError(fmt::format(
                "Detected cycle in asset inclusion for {} at {}", _assetName, _assetPath
            ));
        }

        const std::vector<std::shared_ptr<const Asset>>& subTree = c->subTreeAssets();
        std::copy(subTree.begin(), subTree.end(), std::inserter(assets, assets.end()));
    }
    std::vector<std::shared_ptr<const Asset>> assetVector(assets.begin(), assets.end());
    return assetVector;
}

std::vector<std::shared_ptr<const Asset>> Asset::requiredSubTreeAssets() const {
    std::unordered_set<std::shared_ptr<const Asset>> assets({ shared_from_this() });
    for (const std::shared_ptr<Asset>& dep : _requiredAssets) {
        const std::vector<std::shared_ptr<const Asset>>& subTree =
            dep->requiredSubTreeAssets();

        std::copy(subTree.begin(), subTree.end(), std::inserter(assets, assets.end()));
    }
    std::vector<std::shared_ptr<const Asset>> assetVector(assets.begin(), assets.end());
    return assetVector;
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
    {
        auto it = _requiringAssets.begin();
        while (it != _requiringAssets.end()) {
            std::shared_ptr<Asset> parent = it->lock();
            if (!parent) {
                it = _requiringAssets.erase(it);
                continue;
            }
            if (parent->isLoaded()) {
                return true;
            }
            ++it;
        }
    }
    {
        auto it = _requestingAssets.begin();
        while (it != _requestingAssets.end()) {
            std::shared_ptr<Asset> parent = it->lock();
            if (!parent) {
                it = _requestingAssets.erase(it);
                continue;
            }
            if (parent->isLoaded() || parent->hasLoadedParent()) {
                return true;
            }
            ++it;
        }
    }

    return false;
}

bool Asset::hasSyncingOrResolvedParent() const {
    for (const std::weak_ptr<Asset>& p : _requiringAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isSyncingOrResolved()) {
            return true;
        }
    }
    for (const std::weak_ptr<Asset>& p : _requestingAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isSyncingOrResolved() || parent->hasSyncingOrResolvedParent()) {
            return true;
        }
    }
    return false;
}

bool Asset::hasInitializedParent() const {
    for (const std::weak_ptr<Asset>& p : _requiringAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isInitialized()) {
            return true;
        }
    }
    for (const std::weak_ptr<Asset>& p : _requestingAssets) {
        std::shared_ptr<Asset> parent = p.lock();
        if (!parent) {
            continue;
        }
        if (parent->isInitialized() || parent->hasInitializedParent()) {
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
    for (const std::shared_ptr<Asset>& child : requestedAssets()) {
        child->startSynchronizations();
    }

    // Do not attempt to resync if this is already done
    if (isSyncingOrResolved()) {
        return _state != State::SyncResolved;
    }

    setState(State::Synchronizing);

    bool childFailed = false;

    // Start synchronization of all children first
    for (const std::shared_ptr<Asset>& child : requiredAssets()) {
        if (!child->startSynchronizations()) {
            childFailed = true;
        }
    }

    // Now synchronize its own synchronizations
    for (const std::shared_ptr<ResourceSynchronization>& s : ownSynchronizations()) {
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
    const std::vector<std::shared_ptr<Asset>>& children = childAssets();
    bool cancelledAnySync = std::any_of(
        children.cbegin(),
        children.cend(),
        [](const std::shared_ptr<Asset>& child) {
            return child->cancelAllSynchronizations();
        }
    );

    for (const std::shared_ptr<ResourceSynchronization>& s : ownSynchronizations()) {
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

    const std::vector<std::shared_ptr<Asset>>& children = childAssets();
    bool cancelledAnySync = std::any_of(
        children.begin(),
        children.end(),
        [](const std::shared_ptr<Asset>& child) {
            return child->cancelUnwantedSynchronizations();
        }
    );

    for (const std::shared_ptr<ResourceSynchronization>& s : ownSynchronizations()) {
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

float Asset::requiredSynchronizationProgress() const {
    const std::vector<std::shared_ptr<const Asset>>& assets = requiredSubTreeAssets();
    return syncProgress(assets);
}

float Asset::requestedSynchronizationProgress() {
    const std::vector<std::shared_ptr<const Asset>>& assets = subTreeAssets();
    return syncProgress(assets);
}

bool Asset::load() {
    if (isLoaded()) {
        return true;
    }

    const bool loaded = loader()->loadAsset(this);
    setState(loaded ? State::Loaded : State::LoadingFailed);
    return loaded;
}

void Asset::unload() {
    if (!isLoaded()) {
        return;
    }

    setState(State::Unloaded);
    loader()->unloadAsset(this);

    for (const std::shared_ptr<Asset>& child : requiredAssets()) {
        unrequire(child.get());
    }
    for (const std::shared_ptr<Asset>& child : requestedAssets()) {
        unrequest(child.get());
    }
}

void Asset::unloadIfUnwanted() {
    if (hasLoadedParent()) {
        return;
    }
    unload();
}

bool Asset::initialize() {
    if (isInitialized()) {
        return true;
    }
    if (!isSynchronized()) {
        LERROR(fmt::format("Cannot initialize unsynchronized asset {}", id()));
        return false;
    }
    LDEBUG(fmt::format("Initializing asset {}", id()));

    // 1. Initialize requirements
    for (const std::shared_ptr<Asset>& child : _requiredAssets) {
        child->initialize();
    }

    // 2. Initialize requests
    for (const std::shared_ptr<Asset>& child : _requestedAssets) {
        if (child->isSynchronized()) {
            child->initialize();
        }
    }

    // 3. Call lua onInitialize
    try {
        loader()->callOnInitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Failed to initialize asset {}; {}: {}", id(), e.component, e.message
        ));
        // TODO: rollback;
        setState(State::InitializationFailed);
        return false;
    }

    // 4. Update state
    setState(State::Initialized);

    // 5. Call dependency lua onInitialize of this and requirements
    for (const std::shared_ptr<Asset>& child : _requiredAssets) {
        try {
            loader()->callOnDependencyInitialize(child.get(), this);
        }
        catch (const ghoul::lua::LuaRuntimeException& e) {
            LERROR(fmt::format(
                "Failed to initialize required asset {} of {}; {}: {}",
                child->id(), id(), e.component, e.message
            ));
            // TODO: rollback;
            setState(State::InitializationFailed);
            return false;
        }
    }

    // 6. Call dependency lua onInitialize of this and initialized requests
    for (const std::shared_ptr<Asset>& child : _requestedAssets) {
        if (child->isInitialized()) {
            try {
                loader()->callOnDependencyInitialize(child.get(), this);
            }
            catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR(fmt::format(
                    "Failed to initialize requested asset {} of {}; {}: {}",
                    child->id(), id(), e.component, e.message
                ));
                // TODO: rollback;
            }
        }
    }

    // 7. Call dependency lua onInitialize of initialized requesting assets and this
    for (const std::weak_ptr<Asset>& parent : _requestingAssets) {
        std::shared_ptr<Asset> p = parent.lock();
        if (p && p->isInitialized()) {
            try {
                loader()->callOnDependencyInitialize(this, p.get());
            }
            catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR(fmt::format(
                    "Failed to initialize required asset {} of {}; {}: {}",
                    id(), p->id(), e.component, e.message
                ));
                // TODO: rollback;
            }
        }
    }
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
    LDEBUG(fmt::format("Deintializing asset {}", id()));

    // Perform inverse actions as in initialize, in reverse order (7 - 1)

    // 7. Call dependency lua onDeinitialize of initialized requesting assets and this
    for (const std::weak_ptr<Asset>& parent : _requestingAssets) {
        std::shared_ptr<Asset> p = parent.lock();
        if (p && p->isInitialized()) {
            try {
                loader()->callOnDependencyDeinitialize(this, p.get());
            }
            catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR(fmt::format(
                    "Failed to deinitialize requested asset {} of {}; {}: {}",
                    id(), p->id(), e.component, e.message
                ));
            }
        }
    }

    // 6. Call dependency lua onDeinitialize of this and initialized requests
    for (const std::shared_ptr<Asset>& child : _requestedAssets) {
        if (child->isInitialized()) {
            try {
                loader()->callOnDependencyDeinitialize(child.get(), this);
            }
            catch (const ghoul::lua::LuaRuntimeException& e) {
                LERROR(fmt::format(
                    "Failed to deinitialize requested asset {} of {}; {}: {}",
                    child->id(), id(), e.component, e.message
                ));
            }
        }
    }

    // 5. Call dependency lua onInitialize of this and requirements
    for (const std::shared_ptr<Asset>& child : _requiredAssets) {
        try {
            loader()->callOnDependencyDeinitialize(child.get(), this);
        }
        catch (const ghoul::lua::LuaRuntimeException& e) {
            LERROR(fmt::format(
                "Failed to deinitialize required asset {} of {}; {}: {}",
                child->id(), id(), e.component, e.message
            ));
        }
    }

    // 4. Update state
    setState(Asset::State::SyncResolved);

    // 3. Call lua onInitialize
    try {
        loader()->callOnDeinitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(fmt::format(
            "Failed to deinitialize asset {}; {}: {}", id(), e.component, e.message
        ));
        return;
    }

    // 2 and 1. Deinitialize unwanted requirements and requests
    for (const std::shared_ptr<Asset>& dependency : childAssets()) {
        dependency->deinitializeIfUnwanted();
    }
}

std::string Asset::id() const {
    return _hasAssetPath ? _assetPath : "$root";
}

const std::string& Asset::assetFilePath() const {
    return _assetPath;
}

bool Asset::hasAssetFile() const {
    return _hasAssetPath;
}

std::string Asset::assetDirectory() const {
    return ghoul::filesystem::File(_assetPath).directoryName();
}

const std::string& Asset::assetName() const {
    return _assetName;
}

AssetLoader* Asset::loader() const {
    return _loader;
}

bool Asset::requires(const Asset* asset) const {
    const auto it = std::find_if(
        _requiredAssets.cbegin(),
        _requiredAssets.cend(),
        [asset](const std::shared_ptr<Asset> dep) { return dep.get() == asset; }
    );
    return it != _requiredAssets.cend();
}

void Asset::require(std::shared_ptr<Asset> child) {
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot require child asset when already loaded");
    }

    const auto it = std::find(_requiredAssets.cbegin(), _requiredAssets.cend(), child);
    if (it != _requiredAssets.cend()) {
        // Do nothing if the requirement already exists.
        return;
    }

    _requiredAssets.push_back(child);
    child->_requiringAssets.push_back(shared_from_this());

    if (!child->isLoaded()) {
        child->load();
    }
    if (!child->isLoaded()) {
        unrequire(child.get());
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
            unrequire(child.get());
        }
    }
}

void Asset::unrequire(Asset* child) {
    if (state() != Asset::State::Unloaded) {
        throw ghoul::RuntimeError("Cannot unrequire child asset is in a loaded state");
    }

    const auto childIt = std::find_if(
        _requiredAssets.cbegin(),
        _requiredAssets.cend(),
        [child](const std::shared_ptr<Asset>& asset) { return asset.get() == child; }
    );

    if (childIt == _requiredAssets.cend()) {
        // Do nothing if the request node not exist.
        return;
    }

    _requiredAssets.erase(childIt);

    const auto parentIt = std::find_if(
        child->_requiringAssets.cbegin(),
        child->_requiringAssets.cend(),
        [this](const std::weak_ptr<Asset> a) { return a.lock().get() == this; }
    );
    if (parentIt == child->_requiringAssets.cend()) {
        return;
    }

    child->_requiringAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    child->unloadIfUnwanted();
}

void Asset::request(std::shared_ptr<Asset> child) {
    const auto it = std::find(_requestedAssets.cbegin(), _requestedAssets.cend(), child);
    if (it != _requestedAssets.cend()) {
        // Do nothing if the request already exists.
        return;
    }

    _requestedAssets.push_back(child);
    child->_requestingAssets.push_back(shared_from_this());

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
    const auto childIt = std::find_if(
        _requestedAssets.cbegin(),
        _requestedAssets.cend(),
        [child](const std::shared_ptr<Asset>& asset) { return asset.get() == child; }
    );
    if (childIt == _requestedAssets.cend()) {
        // Do nothing if the request node not exist.
        return;
    }

    _requestedAssets.erase(childIt);

    const auto parentIt = std::find_if(
        child->_requestingAssets.cbegin(),
        child->_requestingAssets.cend(),
        [this](const std::weak_ptr<Asset> a) { return a.lock().get() == this; }
    );
    if (parentIt == child->_requestingAssets.cend()) {
        return;
    }

    child->_requestingAssets.erase(parentIt);

    child->deinitializeIfUnwanted();
    child->cancelUnwantedSynchronizations();
    child->unloadIfUnwanted();
}

bool Asset::requests(Asset* asset) const {
    const auto it = std::find_if(
        _requestedAssets.cbegin(),
        _requestedAssets.cend(),
        [asset](const std::shared_ptr<Asset>& dep) { return dep.get() == asset; }
    );
    return it != _requiredAssets.cend();
}

const std::vector<std::shared_ptr<Asset>>& Asset::requiredAssets() const {
    return _requiredAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requiringAssets() const {
    std::vector<std::shared_ptr<Asset>> assets;
    assets.reserve(_requiringAssets.size());
    for (const std::weak_ptr<Asset>& a : _requiringAssets) {
        if (std::shared_ptr<Asset> shared = a.lock(); shared) {
            assets.push_back(shared);
        }
    }
    return assets;
}

const std::vector<std::shared_ptr<Asset>>& Asset::requestedAssets() const {
    return _requestedAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requestingAssets() const {
    std::vector<std::shared_ptr<Asset>> assets;
    assets.reserve(_requestingAssets.size());
    for (const std::weak_ptr<Asset>& a : _requestingAssets) {
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
        parents.cbegin(),
        parents.cend(),
        [](const std::shared_ptr<Asset>& a) { return a->state() == State::Initialized; }
    );
    return initializedAsset != parents.cend();
}

} // namespace openspace
