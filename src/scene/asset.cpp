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

#include <algorithm>

namespace {
    const char* _loggerCat = "Asset";

    bool isRelative(std::string path) {
        if (path.size() > 2) {
            if (path[0] == '.' && path[1] == '/') return true;
        }
        if (path.size() > 3) {
            if (path[0] == '.' && path[1] == '.' && path[2] == '/') return true;
        }
        return false;
    };

    const char* AssetFileSuffix = "asset";
}

namespace openspace {

std::string Asset::resolveLocalResource(std::string resourceName) {
    std::string currentAssetDirectory = assetDirectory();
    return currentAssetDirectory + ghoul::filesystem::FileSystem::PathSeparator + resourceName;
}

std::string Asset::syncDirectory() const {
    std::string currentAssetDirectory = assetDirectory();
    std::string rootAssetDirectory = loader()->rootAsset()->assetDirectory();
    std::string relativePath = FileSys.relativePath(currentAssetDirectory, rootAssetDirectory);

    return loader()->syncRootDirectory() +
        ghoul::filesystem::FileSystem::PathSeparator +
        relativePath +
        ghoul::filesystem::FileSystem::PathSeparator +
        assetName();
}

Asset::ReadyState Asset::readyState() const {
    return _readyState;
}

bool Asset::isInitReady() const {
    // An asset is ready for initialization if it is synchronized
    // and all its dependencies are ready for initialization.
    if (_readyState != Asset::ReadyState::Synchronized) {
        return false;
    }
    for (const auto& dependency : _dependencies) {
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
    }

    // Initialize dependencies
    for (auto& dependency : _dependencies) {
        dependency->initialize();
    }

    _readyState = Asset::ReadyState::Initialized;

    // Call onInitialize in Lua
    loader()->callOnInitialize(this);

    // Notify dependencies
    for (auto& dependency : _dependencies) {
        dependency->dependantDidInitialize(this);
    }
}

void Asset::deinitialize() {
    if (_readyState != Asset::ReadyState::Initialized) {
        return;
    }

    // Notify dependencies
    for (auto& dependency : _dependencies) {
        dependency->dependantWillDeinitialize(this);
    }

    // Call onDeinitialize in Lua
    loader()->callOnDeinitialize(this);

    _readyState = Asset::ReadyState::Synchronized;

    // Make sure no dependencies are left dangling
    for (auto& dependency : _dependencies) {
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

Asset::Asset(AssetLoader* loader, ghoul::filesystem::Directory directory)
    : PropertyOwner({ "RootAsset", "Root asset" })
    , _assetDirectory(directory)
    , _loader(loader)
    , _readyState(Asset::ReadyState::Loaded)
{
    _id = generateAssetId(directory, "");
}

Asset::Asset(AssetLoader* loader, ghoul::filesystem::Directory baseDirectory, std::string assetPath)
    : PropertyOwner({ assetPath, assetPath })
    , _readyState(Asset::ReadyState::Loaded)
    , _loader(loader)
{
    if (isRelative(assetPath)) {
        ghoul::filesystem::File assetFile =
            static_cast<std::string>(baseDirectory) + 
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        _assetDirectory = assetFile.directoryName();
        _assetName = assetFile.baseName();
    } else {
        std::string assetRoot = ghoul::filesystem::Directory(loader->rootAsset()->assetDirectory());
        ghoul::filesystem::File assetFile =
            assetRoot +
            ghoul::filesystem::FileSystem::PathSeparator +
            assetPath +
            "." +
            AssetFileSuffix;

        _assetDirectory = assetFile.directoryName();
        _assetName = assetFile.baseName();
    }
    _id = generateAssetId(_assetDirectory, _assetName);
}

std::string Asset::assetFilePath() const {
    //ghoul::filesystem::File dir(_assetDirectory);
    return _assetDirectory + ghoul::filesystem::FileSystem::PathSeparator + _assetName + "." + AssetFileSuffix;
}

std::string Asset::generateAssetId(std::string directory, std::string name) {
    return directory + ghoul::filesystem::FileSystem::PathSeparator + name;
}

std::string Asset::assetName() const {
    return _assetName;
}

std::string Asset::assetDirectory() const {
    return _assetDirectory;
}

std::string Asset::id() const {
    return _id;
}

AssetLoader* Asset::loader() const {
    return _loader;
}

bool Asset::hasDependency(const Asset* asset) const {
    const auto it = std::find(_dependencies.begin(), _dependencies.end(), asset);
    return it != _dependencies.end();
}

void Asset::addDependency(Asset* dependency) {
    if (_readyState == Asset::ReadyState::Initialized) {
        // TODO: Throw: cannot add dep while asset is initialized.
        return;
    }

    // Do nothing if the dependency already exists.
    auto it = std::find(_dependencies.begin(), _dependencies.end(), dependency);
    if (it != _dependencies.end()) {
        return;
    }

    _dependencies.push_back(dependency);
    dependency->_dependants.push_back(this);

    addPropertySubOwner(dependency);
}

void Asset::removeDependency(Asset* dependency) {
    _dependencies.erase(
        std::remove(_dependencies.begin(), _dependencies.end(), dependency),
        _dependencies.end()
    );
    std::vector<Asset*>& dependants = dependency->_dependants;
    dependants.erase(
        std::remove(dependants.begin(), dependants.end(), this),
        dependants.end()
    );

    if (!dependency->hasInitializedDependants()) {
        dependency->deinitialize();
    }
}

void Asset::removeDependency(const std::string& assetId) {
    auto dep = std::find_if(_dependencies.begin(), _dependencies.end(), [&assetId](const Asset* d) {
        return d->id() == assetId;
    });
    if (dep != _dependencies.end()) {
       removeDependency(*dep);
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
    bool foundDep = false;
    for (const auto& dependant : _dependants) {
        if (dependant->hasDependency(this)) {
            foundDep = true;
        }
    }
    return foundDep;
}

bool Asset::hasInitializedDependants() const {
    bool foundInitializedDep = false;
    for (const auto& dependant : _dependants) {
        if (dependant->readyState() == Asset::ReadyState::Initialized && dependant->hasDependency(this)) {
            foundInitializedDep = true;
        }
    }
    return foundInitializedDep;
}

// Dependency toggle
Asset::Optional::Optional(Asset* asset, Asset* owner, bool enabled)
    : PropertyOwner({ asset->name(), asset->name() })
    , _enabled({ "enabled", "Enabled", "Enable optional" }, enabled)
    , _asset(asset)
    , _owner(owner)
{
    addProperty(_enabled);
    addPropertySubOwner(asset);
    _enabled.onChange([this]() {
        _owner->setOptionalEnabled(
            _asset,
            _enabled
        );
    });
}

}