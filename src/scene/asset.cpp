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
#include <mutex>

namespace {
    const char* _loggerCat = "Asset";
}

namespace openspace {

Asset::Asset(AssetLoader* loader)
    : _state(State::Unloaded)
    , _loader(loader)
    , _hasAssetPath(false)
    , _assetName("Root Asset")

{}

Asset::Asset(AssetLoader* loader, ghoul::filesystem::File assetPath)
    : _state(State::Unloaded)
    , _loader(loader)
    , _hasAssetPath(true)
    , _assetPath(assetPath)
{}

Asset::~Asset() {
    for (const auto& it : _syncCallbackHandles) {
        it.first->removeStateChangeCallback(it.second);
    }
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

Asset::CallbackHandle Asset::addStateChangeCallback(StateChangeCallback cb) {
    std::lock_guard<std::mutex> guard(_stateChangeCallbackMutex);
    CallbackHandle h = _nextCallbackHandle++;
    _stateChangeCallbacks.emplace(h, cb);
    return h;
}
    
void Asset::removeStateChangeCallback(Asset::CallbackHandle handle) {
    std::lock_guard<std::mutex> guard(_stateChangeCallbackMutex);
    _stateChangeCallbacks.erase(handle);
}

void Asset::setState(Asset::State state) {
    if (_state == state) {
        return;
    }
    _state = state;

    for (auto& requiringAsset : _requiringAssets) {
        if (std::shared_ptr<Asset> a = requiringAsset.lock()) {
            a->requiredAssetChangedState(shared_from_this());
        }
    }
    for (auto& requestingAsset : _requestingAssets) {
        if (std::shared_ptr<Asset> a = requestingAsset.lock()) {
            a->requestedAssetChangedState(shared_from_this());
        }
    }
    std::lock_guard<std::mutex> guard(_stateChangeCallbackMutex);
    for (auto& cb : _stateChangeCallbacks) {
        cb.second(state);
    }
}

void Asset::requiredAssetChangedState(std::shared_ptr<Asset> child) {
    State childState = child->state();

    if (childState == State::SyncResolved) {
        if (isSyncResolveReady()) {
            setState(State::SyncResolved);
        }
    }
    else if (childState == State::SyncRejected) {
        setState(State::SyncRejected);
    }
}

void Asset::requestedAssetChangedState(std::shared_ptr<Asset> child) {
    // TODO: implement this
}

void Asset::addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization) {
    {
        std::lock_guard<std::mutex> guard(_synchronizationsMutex);
        _synchronizations.push_back(synchronization);
    }
    
    // Set up callback for synchronization state change.
    // This will be called from another thread, so we need to mutex protect
    // things that are touched by the callback.
    ResourceSynchronization::CallbackHandle cbh = synchronization->addStateChangeCallback(
        [this, synchronization](ResourceSynchronization::State state) {
            syncStateChanged(synchronization, state);
        }
    );
    _syncCallbackHandles[synchronization.get()] = cbh;
}
    
void Asset::syncStateChanged(std::shared_ptr<ResourceSynchronization> synchronization,
                             ResourceSynchronization::State state)
{
    if (state == ResourceSynchronization::State::Resolved) {
        if (isSyncResolveReady()) {
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
    std::lock_guard<std::mutex> guard(_synchronizationsMutex);
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
    for (auto& dep : _requiredAssets) {
        std::vector<std::shared_ptr<Asset>> subTree = dep->requiredSubTreeAssets();
        std::copy(subTree.begin(), subTree.end(), std::inserter(assets, assets.end()));
    }
    std::vector<std::shared_ptr<Asset>> assetVector(assets.begin(), assets.end());
    return assetVector;
}

bool Asset::isSynchronized() {
    State s = state();
    return s == State::SyncResolved ||
           s == State::Initialized ||
           s == State::InitializationFailed;
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

bool Asset::cancelSynchronizations() {
    bool cancelledAnySync = false;
    for (auto& child : childAssets()) {
        bool cancelled = child->cancelSynchronizations();
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

bool Asset::restartSynchronizations() {
    cancelSynchronizations();
    return startSynchronizations();
}

float Asset::synchronizationProgress() {
    std::vector<std::shared_ptr<Asset>> assets = subTreeAssets();

    size_t nTotalBytes = 0;
    size_t nSyncedBytes = 0;

    for (const auto& a : assets) {
        const std::vector<std::shared_ptr<ResourceSynchronization>> syncs =
            a->ownSynchronizations();

        for (const auto& sync : syncs) {
            if (sync->nTotalBytesIsKnown()) {
                nTotalBytes += sync->nTotalBytes();
                nSyncedBytes += sync->nSynchronizedBytes();
            } else {
                return 0;
            }
        }
    }
    if (nTotalBytes == 0) {
        return 1.f;
    }
    return static_cast<float>(nSyncedBytes) / static_cast<float>(nTotalBytes);
}

bool Asset::isInitReady() const {
    // An asset is ready for initialization if all synchronizations are resolved
    // and all its dependencies are ready for initialization.
    for (const std::shared_ptr<ResourceSynchronization>& sync : ownSynchronizations()) {
        if (!sync->isResolved()) {
            return false;
        }
    }
    for (const auto& dependency : _requiredAssets) {
        if (!dependency->isInitReady()) {
            return false;
        }
    }
    return true;
}

void Asset::initialize() {
    LDEBUG("Initializing asset " << id());
    if (state() == Asset::State::Initialized) {
        return;
    }

    if (!isInitReady()) {
        // TODO: THROW
        return;
    }

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
        if (child->state() == State::Initialized) {
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
    // Initialization may not happen immediately.
    for (auto& child : _requestedAssets) {
        //child->handleRequests();
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

void Asset::deinitialize() {
    if (state() != Asset::State::Initialized) {
        return;
    }

    // Notify children
    for (auto& dependency : _requiredAssets) {
        loader()->callOnDependencyDeinitialize(dependency.get(), this);
    }
    for (auto& dependency : _requestedAssets) {
        if (dependency->state() == State::Initialized) {
            loader()->callOnDependencyDeinitialize(dependency.get(), this);
        }
    }

    // Call onDeinitialize in Lua
    loader()->callOnDeinitialize(this);

    setState(Asset::State::Loaded);

    // Make sure no dependencies are left dangling
    for (auto& dependency : childAssets()) {
        if (!dependency->isRequired() && !dependency->isRequested()) {
            dependency->deinitialize();
        }
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

    child->addRequiringAsset(shared_from_this());
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
    
    child->addRequestingAsset(shared_from_this());
}

void Asset::addRequiringAsset(std::shared_ptr<Asset> parent) {
    _requiringAssets.push_back(parent);
}

void Asset::addRequestingAsset(std::shared_ptr<Asset> parent) {
    _requestingAssets.push_back(parent);

    State parentState = parent->state();
    State childState = state();

    if (parentState == State::Synchronizing ||
        parentState == State::SyncResolved ||
        parentState == State::Initialized)
    {
        if (childState != State::Synchronizing ||
            childState != State::SyncResolved ||
            childState != State::Initialized)
        {
            startSynchronizations();
        }
    }

    if (parentState == State::Initialized && childState == State::SyncResolved) {
        initialize();
    }
}

void Asset::removeRequiringAsset(std::shared_ptr<Asset> parent) {
    auto parentIt = std::find_if(
        _requiringAssets.begin(),
        _requiringAssets.end(),
        [this](std::weak_ptr<Asset> a) {
            return a.lock().get() == this;
        }
    );

    if (parentIt == _requiringAssets.end()) {
        return;
    }

    _requiringAssets.erase(parentIt);
}

void Asset::removeRequestingAsset(std::shared_ptr<Asset> parent) {
    auto parentIt = std::find_if(
        _requestingAssets.begin(),
        _requestingAssets.end(),
        [this](std::weak_ptr<Asset> a) {
            return a.lock().get() == this;
        }
    );

    if (parentIt == _requestingAssets.end()) {
        return;
    }

    _requestingAssets.erase(parentIt);
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
    
    child->removeRequestingAsset(shared_from_this());

    // TODO: update real state!
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

std::vector<std::shared_ptr<Asset>> Asset::requiredAssets() {
    return _requiredAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requiringAssets() {
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

std::vector<std::shared_ptr<Asset>> Asset::requestedAssets() {
    return _requestedAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::requestingAssets() {
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

std::vector<std::shared_ptr<Asset>> Asset::childAssets() {
    std::vector<std::shared_ptr<Asset>> children;
    children.reserve(_requiredAssets.size() + _requestedAssets.size());
    children.insert(children.end(), _requiredAssets.begin(), _requiredAssets.end());
    children.insert(children.end(), _requestedAssets.begin(), _requestedAssets.end());
    return children;
}

bool Asset::isRequired() const {
    return !_requiringAssets.empty();
}

bool Asset::isRequested() const {
    return !_requestingAssets.empty();
}


}
