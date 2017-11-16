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

#ifndef __OPENSPACE_CORE___ASSET___H__
#define __OPENSPACE_CORE___ASSET___H__

#include <openspace/util/resourcesynchronization.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <string>
#include <vector>

#include <memory>
#include <unordered_set>
#include <iterator>

namespace openspace {

class AssetLoader;

class Asset : public std::enable_shared_from_this<Asset> {
public:
    using Optional = std::pair<std::shared_ptr<Asset>, bool>;

    enum class State : unsigned int {
        Unloaded,
        LoadingFailed,
        Loaded,
        Synchronizing,
        SyncResolved,
        SyncRejected,
        Initialized,
        InitializationFailed
    };

    /**
     * Root asset constructor
     */
    Asset(AssetLoader* loader);

    /**
    * Dependency or Optional constructor
    */
    Asset(AssetLoader* loader, ghoul::filesystem::File assetPath);

    std::string id() const;
    std::string assetFilePath() const;
    bool hasAssetFile() const;
    std::string assetDirectory() const;
    std::string assetName() const;
    AssetLoader* loader() const;
    State state() const;

    void setState(State state);
    void addSynchronization(std::shared_ptr<ResourceSynchronization> synchronization);
    std::vector<std::shared_ptr<ResourceSynchronization>> synchronizations();


    // Sync
    bool startSynchronizations();
    bool cancelSynchronizations();
    bool restartSynchronizations();
    float synchronizationProgress();

    // Init
    bool isInitReady() const;
    void initialize();
    void deinitialize();

    // Dependency graph
    bool requires(const Asset* asset) const;
    void require(std::shared_ptr<Asset> asset);

    bool requests(const Asset* child) const;
    void request(std::shared_ptr<Asset> child);
    void unrequest(std::shared_ptr<Asset> child);

    std::vector<std::shared_ptr<Asset>> requiredSubTreeAssets();
    std::vector<std::shared_ptr<Asset>> subTreeAssets();
    std::vector<std::shared_ptr<Asset>> requestedAssets();
    std::vector<std::shared_ptr<Asset>> requiredAssets();
    std::vector<std::shared_ptr<Asset>> childAssets();

    bool isRequired() const;
    bool isRequested() const;

    std::string resolveLocalResource(std::string resourceName);
private:

    void handleRequests();
    void startSync(ResourceSynchronization& rs);
    void cancelSync(ResourceSynchronization& rs);

    State _state;
    AssetLoader* _loader;
    std::vector<std::shared_ptr<ResourceSynchronization>> _synchronizations;

    bool _hasAssetPath;
    // The name of the asset
    std::string _assetName;

    // Absolute path to asset file
    std::string _assetPath;

    // Required assets
    std::vector<std::shared_ptr<Asset>> _requiredAssets;

    // Assets that refers to this asset as a required asset
    std::vector<std::weak_ptr<Asset>> _requiringAssets;

    // Requested assets
    std::vector<std::shared_ptr<Asset>> _requestedAssets;

    // Assets that refers to this asset as a requested asset
    std::vector<std::weak_ptr<Asset>> _requestingAssets;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET__H__
