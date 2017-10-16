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
    //: PropertyOwner({ "RootAsset", "Root asset" })
    : _readyState(Asset::ReadyState::Loaded)
    , _loader(loader)
    , _assetName("Root Asset")

{}

Asset::Asset(AssetLoader* loader, ghoul::filesystem::File assetPath)
    //: PropertyOwner({ assetPath, assetPath })
    : _readyState(Asset::ReadyState::Loaded)
    , _loader(loader)
    , _assetPath(assetPath)
{}

std::string Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = assetDirectory();
    return currentAssetDirectory +
        ghoul::filesystem::FileSystem::PathSeparator +
        resourceName;
}

std::string Asset::syncDirectory() const {
    std::string currentAssetDirectory = assetDirectory();
    std::string rootAssetDirectory = loader()->syncRootDirectory();
    std::string relativePath = FileSys.relativePath(currentAssetDirectory,
                                                    rootAssetDirectory);

    return loader()->syncRootDirectory() +
        ghoul::filesystem::FileSystem::PathSeparator +
        relativePath +
        ghoul::filesystem::FileSystem::PathSeparator +
        assetName();
}

Asset::ReadyState Asset::readyState() const {
    return _readyState;
}

void Asset::addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization) {
    _synchronizations.push_back(synchronization);
}

bool Asset::isInitReady() const {
    // An asset is ready for initialization if all synchronizations are resolved
    // and all its required dependencies are ready for initialization.
    for (const std::shared_ptr<ResourceSynchronization>& sync : _synchronizations) {
        if (!sync->isResolved()) {
            return false;
        }
    }
    for (const auto& dependency : _requiredDependencies) {
        if (!dependency->isInitReady()) {
            return false;
        }
    }
    return true;
}

void Asset::initialize() {
    LDEBUG("Initializing asset " << id());
    if (_readyState == Asset::ReadyState::Initialized) {
        return;
    }

    if (!isInitReady()) {
        // TODO: THROW
        return;
    }

    // Initialize dependencies
    for (auto& dependency : _requiredDependencies) {
        dependency->initialize();
    }

    _readyState = Asset::ReadyState::Initialized;

    // Call onInitialize in Lua
    loader()->callOnInitialize(this);

    // Notify dependencies
    for (auto& dependency : _requiredDependencies) {
        dependency->dependantDidInitialize(this);
    }
}

void Asset::deinitialize() {
    if (_readyState != Asset::ReadyState::Initialized) {
        return;
    }

    // Notify dependencies
    for (auto& dependency : _requiredDependencies) {
        dependency->dependantWillDeinitialize(this);
    }

    // Call onDeinitialize in Lua
    loader()->callOnDeinitialize(this);

    _readyState = Asset::ReadyState::Loaded;

    // Make sure no dependencies are left dangling
    for (auto& dependency : _requiredDependencies) {
        if (!dependency->hasInitializedDependants()) {
            dependency->deinitialize();
        }
    }
}

std::string Asset::resolveSyncedResource(std::string resourceName) {
    return syncDirectory() +
        ghoul::filesystem::FileSystem::PathSeparator + 
        resourceName;
}

std::string Asset::id() const {
    return _assetPath.has_value() ? _assetPath.value() : "$root";
}

std::string Asset::assetFilePath() const {
    return _assetPath.value();
}

bool Asset::hasAssetFile() const {
    return _assetPath.has_value();
}

std::string Asset::assetDirectory() const {
    return ghoul::filesystem::File(_assetPath.value()).directoryName();
}

std::string Asset::assetName() const {
    return _assetName;
}



AssetLoader* Asset::loader() const {
    return _loader;
}

bool Asset::hasRequiredDependency(const Asset* asset) const {
    const auto it = std::find(_requiredDependencies.begin(),
                              _requiredDependencies.end(),
                              asset);

    return it != _requiredDependencies.end();
}

void Asset::addRequiredDependency(Asset* dependency) {
    if (_readyState == Asset::ReadyState::Initialized) {
        // TODO: Throw: cannot add dep while asset is initialized.
        return;
    }

    // Do nothing if the dependency already exists.
    auto it = std::find(_requiredDependencies.begin(),
                        _requiredDependencies.end(),
                        dependency);

    if (it != _requiredDependencies.end()) {
        return;
    }

    _requiredDependencies.push_back(dependency);
    dependency->_requiredDependants.push_back(this);

    //addPropertySubOwner(dependency);
}

void Asset::removeRequiredDependency(Asset* dependency) {
    _requiredDependencies.erase(
        std::remove(_requiredDependencies.begin(),
            _requiredDependencies.end(),
            dependency),

        _requiredDependencies.end()
    );
    std::vector<Asset*>& dependants = dependency->_requiredDependants;
    dependants.erase(
        std::remove(dependants.begin(), dependants.end(), this),
        dependants.end()
    );

    if (!dependency->hasInitializedDependants()) {
        dependency->deinitialize();
    }
}

void Asset::removeRequiredDependency(const std::string& assetId) {
    auto dep = std::find_if(
        _requiredDependencies.begin(),
        _requiredDependencies.end(),
        [&assetId](const Asset* d) {
            return d->id() == assetId;
        });

    if (dep != _requiredDependencies.end()) {
       removeRequiredDependency(*dep);
    } else {
        LERROR("No such dependency '" << assetId << "'");
    }
}

void Asset::dependantDidInitialize(Asset* dependant) {
    loader()->callOnDependantInitialize(this, dependant);
}

void Asset::dependantWillDeinitialize(Asset* dependant) {
    loader()->callOnDependantDeinitialize(this, dependant);
}

bool Asset::hasDependants() const {
    for (const auto& dependant : _requiredDependants) {
        if (dependant->hasRequiredDependency(this)) {
            return true;
        }
    }
    for (const auto& dependant : _optionalDependants) {
        if (dependant->hasEnabledOptionalDependency(this)) {
            return true;
        }
    }
    return false;
}

bool Asset::hasInitializedDependants() const {
    for (const auto& dependant : _requiredDependants) {
        if (dependant->readyState() == Asset::ReadyState::Initialized &&
            dependant->hasRequiredDependency(this))
        {
            return true;
        }
    }
    for (const auto& dependant : _optionalDependants) {
        if (dependant->readyState() == Asset::ReadyState::Initialized &&
            dependant->hasEnabledOptionalDependency(this))
        {
            return true;
        }
    }
    return false;
}

std::vector<Asset*> Asset::optionalAssets() const {
    std::vector<Asset*> assets(_optionalDependencies.size());
    std::transform(
        _optionalDependencies.begin(),
        _optionalDependencies.end(),
        assets.begin(),
        [](const Optional& o) {
            return o.first;
        }
    );
    return assets;
}

bool Asset::hasOptionalDependency(const Asset* asset) const {
    auto it = std::find_if(
        _optionalDependencies.begin(),
        _optionalDependencies.end(),
        [&asset](const Optional& o) {
            return o.first == asset;
        }
    );
    return it != _optionalDependencies.end();
}

bool Asset::hasEnabledOptionalDependency(const Asset* asset) const {
        auto it = std::find_if(
            _optionalDependencies.begin(),
            _optionalDependencies.end(),
        [&asset](const Optional& o) {
            return o.first == asset && o.second;
        }
    );
    return it != _optionalDependencies.end();
}

void Asset::setOptionalDependencyEnabled(Asset* asset, bool enabled) {
    auto it = std::find_if(
        _optionalDependencies.begin(),
        _optionalDependencies.end(),
        [&asset](const Optional& o) {
            return o.first == asset;
        }
    );

    if (it != _optionalDependencies.end()) {
        it->second = enabled;
    }
}

void Asset::removeOptionalDependency(Asset* asset) {
    _optionalDependencies.erase(
        std::remove_if(
            _optionalDependencies.begin(),
            _optionalDependencies.end(),
            [&asset](Optional& o) {
                return o.first == asset;
            }
        ),
        _optionalDependencies.end()
    );
    // TODO: Update and validate
}

void Asset::addOptionalDependency(Asset* asset, bool enabled) {
    _optionalDependencies.push_back(std::make_pair(asset, enabled));
    // TODO: Update and validate
}

}
