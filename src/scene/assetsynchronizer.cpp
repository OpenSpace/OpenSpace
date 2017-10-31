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

#include <openspace/scene/assetsynchronizer.h>

#include <algorithm>
#include <memory>

namespace {
    const char* _loggerCat = "AssetSynchronizer";
}

namespace openspace {
AssetSynchronizer::AssetSynchronizer(ResourceSynchronizer* resourceSynchronizer) {
    _resourceSynchronizer = resourceSynchronizer;
}

void AssetSynchronizer::addAsset(std::shared_ptr<Asset> asset) {
    _managedAssets.emplace(asset.get(), 
        AssetSynchronization{ asset, SynchronizationState::Added }
    );

    for (const auto& sync : asset->synchronizations()) {
        _resourceToAssetMap[sync.get()] = asset.get();
    }
}

void AssetSynchronizer::removeAsset(Asset* asset) {
    AssetSynchronization a = _managedAssets[asset];

    std::vector<std::shared_ptr<ResourceSynchronization>> resourceSyncs =
        asset->synchronizations();

    for (const auto& s : resourceSyncs) {
        _resourceToAssetMap.erase(s.get());
        _resourceSynchronizer->cancelSynchronization(s.get(), this);
    }

    _managedAssets.erase(asset);
}

void AssetSynchronizer::syncAsset(Asset* asset) {
    std::vector<std::shared_ptr<ResourceSynchronization>> resourceSyncs =
        asset->synchronizations();

    if (resourceSyncs.empty()) {
        _trivialSynchronizations.push_back(asset);
    }

    _managedAssets[asset].state = SynchronizationState::Synchronizing;

    for (const auto& s : resourceSyncs) {
        if (!s->isResolved()) {
            _resourceSynchronizer->enqueueSynchronization(s, this);
        }
    }
}

void AssetSynchronizer::syncUnsynced() {
    for (auto& it : _managedAssets) {
        if (it.second.state == SynchronizationState::Added) {
            syncAsset(it.first);
        }
    }
}

std::vector<std::shared_ptr<Asset>> AssetSynchronizer::getSynchronizedAssets() {
    std::vector<std::shared_ptr<ResourceSynchronization>> syncs =
        _resourceSynchronizer->finishedSynchronizations(this);

    std::vector<Asset*> affectedAssets;
    for (const auto& sync : syncs) {       
        const auto& it = _resourceToAssetMap.find(sync.get());
        if (it != _resourceToAssetMap.end()) {
            affectedAssets.push_back(it->second);
        }
    }

    std::vector<std::shared_ptr<Asset>> synchronizedAssets;
    for (auto a : affectedAssets) {
        std::vector<std::shared_ptr<ResourceSynchronization>> syncs = a->synchronizations();
        if (assetIsSynchronized(a)) {
            _managedAssets[a].state = SynchronizationState::Synchronized;
        }
        const auto it = _managedAssets.find(a);
        if (it != _managedAssets.end()) {
            synchronizedAssets.push_back(it->second.asset);
        }
    }

    for (auto& finished : _trivialSynchronizations) {
        auto it = _managedAssets.find(finished);
        if (it != _managedAssets.end()) {
            std::shared_ptr<Asset> asset = it->second.asset;
            _managedAssets[asset.get()].state = SynchronizationState::Synchronized;
            synchronizedAssets.push_back(asset);
        }
    }
    _trivialSynchronizations.clear();

    return synchronizedAssets;
}

bool AssetSynchronizer::assetIsSynchronized(Asset * asset) {
    std::vector<std::shared_ptr<ResourceSynchronization>> syncs = asset->synchronizations();
    for (const auto& s : syncs) {
        if (!s->isResolved()) {
            return false;
        }
    }
    return true;
}

}
