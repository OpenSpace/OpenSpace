/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/documentation/documentation.h>
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
    constexpr std::string_view _loggerCat = "Asset";
} // namespace

Asset::Asset(AssetManager& manager, std::filesystem::path assetPath,
             std::optional<bool> explicitEnabled)
    : _manager(manager)
    , _assetPath(std::move(assetPath))
    , _explicitEnabled(explicitEnabled)
{
    ghoul_precondition(!_assetPath.empty(), "Asset path must not be empty");
    ghoul_precondition(
        std::filesystem::is_regular_file(_assetPath),
        "Asset path file must exist"
    );
}

std::filesystem::path Asset::path() const {
    return _assetPath;
}

void Asset::setState(State state) {
    ZoneScoped;

    if (_state == state) {
        return;
    }

    _state = state;

    // If we change our state, there might have been a parent of ours that was waiting for
    // us to finish, so we give each asset that required us the chance to update its own
    // state. This might cause a cascade up towards the roo asset in the best/worst case
    for (Asset* parent : _parentAssets) {
        if (// Prohibit state change to SyncResolved if additional requirements may still
            // be added
            !parent->isLoaded() ||
            // Do not do anything if this asset was already initialized. This may happen
            // if there are multiple requirement paths from this asset to the same child,
            // which causes this method to be called more than once
            parent->isInitialized() ||
            // Do not do anything if the parent asset failed to initialize
            parent->_state == State::InitializationFailed)
        {
            continue;
        }

        if (state == State::Synchronized) {
            if (parent->isSyncResolveReady()) {
                parent->setState(State::Synchronized);
            }
        }
        else if (state == State::SyncRejected) {
            parent->setState(State::SyncRejected);
        }
    }
}

void Asset::addSynchronization(ResourceSynchronization* synchronization) {
    ghoul_precondition(synchronization != nullptr, "Synchronization must not be nullptr");
    ghoul_precondition(
        std::find(
            _synchronizations.begin(),
            _synchronizations.end(),
            synchronization
        ) == _synchronizations.end(),
        "Synchronization must not have been added before"
    );
    _synchronizations.push_back(synchronization);
}

void Asset::setSynchronizationStateResolved() {
    ZoneScoped;

    if (!isSynchronized() && isSyncResolveReady()) {
        setState(State::Synchronized);
    }
}

void Asset::setSynchronizationStateRejected() {
    ZoneScoped;

    setState(State::SyncRejected);
}

bool Asset::isSyncResolveReady() const {
    const bool allParentsSynced = std::all_of(
        _requiredAssets.cbegin(),
        _requiredAssets.cend(),
        std::mem_fn(&Asset::isSynchronized)
    );

    const bool allSynced = std::all_of(
        _synchronizations.cbegin(),
        _synchronizations.cend(),
        std::mem_fn(&ResourceSynchronization::isResolved)
    );

    // To be considered resolved, all own synchronizations need to be resolved and all
    // parents have to be synchronized
    return allParentsSynced && allSynced;
}

bool Asset::isLoaded() const {
    return _state != State::Unloaded && _state != State::LoadingFailed;
}

bool Asset::isSynchronized() const {
    return _state == State::Synchronized || _state == State::Initialized ||
           _state == State::InitializationFailed;
}

bool Asset::isSyncingOrResolved() const {
    return _state == State::Synchronizing || _state == State::Synchronized ||
           _state == State::Initialized || _state == State::InitializationFailed;
}

bool Asset::isFailed() const {
    return _state == State::LoadingFailed || _state == State::SyncRejected ||
           _state == State::InitializationFailed;
}

std::optional<bool> Asset::explicitEnabled() const {
    return _explicitEnabled;
}

Asset* Asset::firstParent() const {
    return !_parentAssets.empty() ? _parentAssets[0] : nullptr;
}

bool Asset::hasLoadedParent() {
    return std::any_of(
        _parentAssets.begin(),
        _parentAssets.end(),
        std::mem_fn(&Asset::isLoaded)
    );
}

bool Asset::hasInitializedParent() const {
    return std::any_of(
        _parentAssets.begin(),
        _parentAssets.end(),
        std::mem_fn(&Asset::isInitialized)
    );
}

bool Asset::isInitialized() const {
    return _state == State::Initialized;
}

void Asset::startSynchronizations() {
    ghoul_precondition(isLoaded(), "This Asset must have been Loaded before");

    // Do not attempt to resync if this is already done
    if (isSyncingOrResolved()) {
        return;
    }

    setState(State::Synchronizing);

    // Start synchronization of all children first
    for (Asset* child : _requiredAssets) {
        child->startSynchronizations();
    }

    // Now synchronize its own synchronizations
    for (ResourceSynchronization* s : _synchronizations) {
        if (!s->isResolved()) {
            s->start();
        }
    }
    // If all syncs are resolved (or no syncs exist), mark as resolved. If they are not,
    // this asset will be told by the ResourceSynchronization when it finished instead
    if (!isInitialized() && isSyncResolveReady()) {
        setState(State::Synchronized);
    }
}

void Asset::addIdentifier(std::string identifier) {
    if (!_metaInformation.has_value()) {
        _metaInformation = MetaInformation();
    }

    _metaInformation->identifiers.push_back(std::move(identifier));
}

void Asset::load(Asset* parent) {
    if (!isLoaded()) {
        const bool loaded = _manager.loadAsset(this, parent);
        setState(loaded ? State::Loaded : State::LoadingFailed);
    }
}

void Asset::unload() {
    if (!isLoaded()) {
        return;
    }

    setState(State::Unloaded);
    _manager.unloadAsset(this);

    while (!_requiredAssets.empty()) {
        Asset* child = *_requiredAssets.begin();

        ghoul_assert(
            _state == Asset::State::Unloaded,
            "Cannot unrequire child asset in a loaded state"
        );

        _requiredAssets.erase(_requiredAssets.begin());

        auto parentIt = std::find(
            child->_parentAssets.cbegin(),
            child->_parentAssets.cend(),
            this
        );
        ghoul_assert(
            parentIt != child->_parentAssets.cend(),
            "Parent asset was not correctly registered"
        );

        child->_parentAssets.erase(parentIt);

        // We only want to deinitialize the child if noone is keeping track of it,
        // which is either a still initialized parent or that it is loaded as a root
        if (!child->hasInitializedParent() && !_manager.isRootAsset(child)) {
            child->deinitialize();
        }
        if (!child->hasLoadedParent() && !_manager.isRootAsset(child)) {
            child->unload();
        }
    }
}

void Asset::initialize() {
    ZoneScoped;

    if (isInitialized()) {
        return;
    }
    if (!isSynchronized()) {
        LERROR(std::format("Cannot initialize unsynchronized asset '{}'", _assetPath));
        return;
    }
    LDEBUG(std::format("Initializing asset '{}'", _assetPath));

    // 1. Initialize requirements
    for (Asset* child : _requiredAssets) {
        child->initialize();
    }

    // 2. Call Lua onInitialize
    try {
        _manager.callOnInitialize(this);
    }
    catch (const documentation::SpecificationError& e) {
        LERROR(std::format("Failed to initialize asset '{}'", path()));
        documentation::logError(e);
        setState(State::InitializationFailed);
        return;
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(std::format("Failed to initialize asset '{}'", path()));
        LERROR(std::format("{}: {}", e.component, e.message));
        setState(State::InitializationFailed);
        return;
    }

    // 3. Update state
    setState(State::Initialized);
}

void Asset::deinitialize() {
    if (!isInitialized()) {
        return;
    }
    LDEBUG(std::format("Deinitializing asset '{}'", _assetPath));

    // Perform inverse actions as in initialize, in reverse order (3 - 1)

    // 3. Update state
    setState(Asset::State::Synchronized);

    // 2. Call Lua onInitialize
    try {
        _manager.callOnDeinitialize(this);
    }
    catch (const ghoul::lua::LuaRuntimeException& e) {
        LERROR(std::format("Failed to deinitialize asset '{}'", _assetPath));
        LERROR(std::format("{}: {}", e.component, e.message));
        return;
    }

    // 1. Deinitialize unwanted requirements
    for (Asset* dependency : _requiredAssets) {
        // We only want to deinitialize the dependency if noone is keeping track of it,
        // which is either a still initialized parent or that it is loaded as a root
        if (!dependency->hasInitializedParent() && !_manager.isRootAsset(dependency)) {
            dependency->deinitialize();
        }
    }
}

void Asset::require(Asset* dependency) {
    ghoul_precondition(dependency, "Dependency must not be nullptr");

    auto it = std::find(_requiredAssets.cbegin(), _requiredAssets.cend(), dependency);
    if (it == _requiredAssets.cend()) {
        _requiredAssets.push_back(dependency);
        dependency->_parentAssets.push_back(this);
    }
}

void Asset::setMetaInformation(MetaInformation metaInformation) {
    _metaInformation = std::move(metaInformation);
}

std::optional<Asset::MetaInformation> Asset::metaInformation() const {
    return _metaInformation;
}

} // namespace openspace
