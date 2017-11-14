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
#include <numeric>

namespace {
    const char* _loggerCat = "AssetSynchronizer";
}

namespace openspace {
AssetSynchronizer::AssetSynchronizer() {}
/*
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
        s->cancel();
    }

    _managedAssets.erase(asset);
}*/

void AssetSynchronizer::startSync(std::shared_ptr<Asset> asset) {
    std::vector<std::shared_ptr<ResourceSynchronization>> resourceSyncs =
        asset->synchronizations();

    if (resourceSyncs.empty()) {
        _stateChanges.emplace(
            asset.get(), 
            StateChange{ asset, SynchronizationState::Resolved }
        );
    }

    _synchronizingAssets[asset.get()] = asset;

    for (const auto& s : resourceSyncs) {
        if (!s->isResolved()) {
            s->start();
        }
    }
}

void AssetSynchroinier::cancelSync(Asset* asset) {
    // Todo: cancel sync
}
/*
float AssetSynchronizer::assetProgress(Asset* asset) {
    auto it = _managedAssets.find(asset);
    if (it == _managedAssets.end()) {
        return 0.f;
    }
    const std::vector<std::shared_ptr<ResourceSynchronization>> syncs =
        asset->synchronizations();

    size_t nTotalBytes = 0;
    size_t nSyncedBytes = 0;

    for (const auto& sync : syncs) {
        if (sync->nTotalBytesIsKnown()) {
            nTotalBytes += sync->nTotalBytes();
            nSyncedBytes += sync->nSynchronizedBytes();
        } else {
            return 0;
        }
    }

    if (nTotalBytes == 0) {
        return 1.f;
    }

    return static_cast<float>(nSyncedBytes)/static_cast<float>(nTotalBytes);
}
*/

std::vector<AssetSynchronizer::StateChange> AssetSynchronizer::getStateChanges() {
    /*
    std::vector<std::shared_ptr<ResourceSynchronization>> finishedResourceSyncs;
    
    for (auto a : _managedAssets) {
        std::vector<std::shared_ptr<ResourceSynchronization>> syncs = a.first->synchronizations();
        for (auto s : syncs) {
            if (s->isResolved() || s->isRejected()) {
                finishedResourceSyncs.push_back(s);
            }
        }
    }

    std::vector<Asset*> affectedAssets;
    for (const auto& sync : finishedResourceSyncs) {
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
    */
    std::vector<StateChange> stateChangesVector;
    for (auto& s : _stateChanges) {
        stateChangesVector.push_back(std::move(s.second));
    }
    _stateChanges.clear();
    return stateChangesVector;
}

/*
bool AssetSynchronizer::assetIsSynchronized(Asset * asset) {
    std::vector<std::shared_ptr<ResourceSynchronization>> syncs = asset->synchronizations();
    for (const auto& s : syncs) {
        if (!s->isResolved()) {
            return false;
        }
    }
    return true;
}*/

}
