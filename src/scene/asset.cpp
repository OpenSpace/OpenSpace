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

std::string Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = assetDirectory();
    return currentAssetDirectory +
        ghoul::filesystem::FileSystem::PathSeparator +
        resourceName;
}

void Asset::handleRequests() {
    State currentState = state();

    if (currentState == State::Initialized) {
        // ...
    }
}

void Asset::startSync(ResourceSynchronization& rs) {
    rs.start();
}

void Asset::cancelSync(ResourceSynchronization& rs) {
    rs.cancel();
}

Asset::State Asset::state() const {
    return _state;
}

void Asset::setState(Asset::State state) {
    _state = state;
}

void Asset::addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization) {
    _synchronizations.push_back(synchronization);
}

std::vector<std::shared_ptr<ResourceSynchronization>> Asset::synchronizations()
{
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

bool Asset::startSynchronizations() {
    bool startedAnySync = false;
    for (auto& child : childAssets()) {
        bool started = child->startSynchronizations();
        if (started) {
            startedAnySync = true;
        }
    }
    for (const auto& s : _synchronizations) {
        if (!s->isResolved()) {
            startedAnySync = true;
            startSync(*s);
        }
    }
    if (!startedAnySync) {
        setState(State::SyncResolved);
    }
    return startedAnySync;
}

bool Asset::cancelSynchronizations() {
    bool cancelledAnySync = false;
    for (auto& child : childAssets()) {
        bool cancelled = child->cancelSynchronizations();
        if (cancelled) {
            cancelledAnySync = true;
        }
    }
    for (const auto& s : _synchronizations) {
        if (s->isSyncing()) {
            cancelledAnySync = true;
            cancelSync(*s);
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
            a->synchronizations();

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
    for (const std::shared_ptr<ResourceSynchronization>& sync : _synchronizations) {
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
    loader()->callOnInitialize(this);

    // 3. Update the internal state
    setState(Asset::State::Initialized);

    // 4. Call dependency initialization functions
    // Now that both this and all children are initialized.
    for (auto& child : _requiredAssets) {
        loader()->callOnDependencyInitialize(child.get(), this);
    }

    // 5. Call dependency initialization function of the child and this
    // if the requested child was initialized before this.
    for (auto& child : _requestedAssets) {
        if (child->state() == State::Initialized) {
            loader()->callOnDependencyInitialize(child.get(), this);
        }
    }

    // 6. Ask requested children to initialize if they are not already initialized.
    // Initialization may not happen immediately.
    for (auto& child : _requestedAssets) {
        child->handleRequests();
    }

    // 7. Call dependency initialization function of this and the parent
    // if the requesting parent was initialized before this.
    for (auto& parent : _requestingAssets) {
        std::shared_ptr<Asset> p = parent.lock();
        if (p && p->state() == State::Initialized) {
            loader()->callOnDependencyInitialize(this, p.get());
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
    if (state() == Asset::State::Unloaded) {
        // TODO: Throw: Can only require asset when in unloaded state
        return;
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
    
    child->handleRequests();
}

void Asset::unrequest(std::shared_ptr<Asset> child) {
    auto it = std::find(_requestedAssets.begin(),
        _requestedAssets.end(),
        child);

    if (it != _requestedAssets.end()) {
        // Do nothing if the request already exists.
        return;
    }
    _requestedAssets.push_back(child);
    child->_requestingAssets.push_back(shared_from_this());
    
    child->handleRequests();
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

std::vector<std::shared_ptr<Asset>> Asset::requestedAssets() {
    return _requestedAssets;
}

std::vector<std::shared_ptr<Asset>> Asset::childAssets() {
    std::vector<std::shared_ptr<Asset>> children;
    children.reserve(_requiredAssets.size() + _requestedAssets.size());
    children.insert(_requiredAssets.begin(), _requiredAssets.end(), children.end());
    children.insert(_requestedAssets.begin(), _requestedAssets.end(), children.end());
    return children;
}

bool Asset::isRequired() const {
    return !_requiringAssets.empty();
}

bool Asset::isRequested() const {
    return !_requestingAssets.empty();
}


}
