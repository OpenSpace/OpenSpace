/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <filesystem>
#include <optional>

namespace openspace {

class AssetManager;

/**
 * This class represents a successfully loaded Asset. Each Lua asset file results in
 * exactly one instance of this class unless it contains a syntax error or some other
 * error occurred while loading. Each Asset can have 0-* number of resource
 * synchronizations that were requested through the `syncedResource` function in Lua and
 * 0-* number of other Assets that this Asset requires (through the `require` function in
 * Lua. There also is a list of requiring assets that contain all assets which require
 * this asset.
 * An asset goes through three external states. Starting out as unloaded when the instance
 * is newly created but the file has not been processed. In this case the #isLoaded,
 * #isSynchronized and the #isInitialized functions all return \c false. After the asset
 * has been loaded it is in the \c Loaded state (#isLoaded = true,
 * #isSynchronized = false, #isInitialized = false). After all registered synchronizations
 * finish successfully, the Asset transitions into the Synchronized state
 * (#isLoaded = true, #isSynchronized = true, #isInitialized = false) and after the final
 * initialization step, the asset is initialized (#isLoaded = true,
 * #isSynchronized = true and #isInitialized = true)
 */
class Asset {
public:
    /// This struct contains all the meta information about this asset
    struct MetaInformation {
        /// The name of the asset
        std::string name;
        /// The version number of the asset. This is only a string representation and does
        /// not have to follow SemVar (even though it is advised)
        std::string version;
        /// A user-facing description of the asset contents
        std::string description;
        /// The author of the asset
        std::string author;
        /// A URL where a consumer of the asset might find additional information about,
        /// for example, the author or the used dataset(s)
        std::string url;
        /// The license under which this asset has been provided
        std::string license;
        /// A list of all scene graph nodes that have been created in this asset
        std::vector<std::string> identifiers;
    };

    /**
     * Creates a new Asset that is backed by the provided \p assetPath. The \p manager is
     * the AssetManager instance that is responsible for loading this and all the other
     * required Assets, the \p watcher is the instance that will delegate information
     * about successful or unsuccessful synchronizations.
     *
     * \param manager The AssetManager that is responsible for loading this Asset and all
     *        of its dependencies
     * \param assetPath The file path that will be used to load this Asset
     * 
     * \pre The \p assetPath must not be empty and must be an existing file
     */
    Asset(AssetManager& manager, std::filesystem::path assetPath);

    /**
     * Returns the path to the file that was used to initialize this Asset
     * \return The path to the file that was used to initialize this Asset
     */
    std::filesystem::path path() const;

    /**
     * Adds a new dependent ResourceSynchronization object to this Asset. The
     * SynchronizationWatcher passed into this Asset in its constructor will be notified
     * as soon as the synchronization state of the \p synchronization changes
     * 
     * \param synchronization The resource synchronization object that is bound to this
     *        Asset
     * \pre \p synchronization must not be nullptr
     */
    void addSynchronization(ResourceSynchronization* synchronization);
    
    /**
     * Removes all previously registered synchronizations from this Asset, which will both
     * terminate the synchronization and also no longer inform the SynchronizationWatcher
     * about any changes (which includes this termination)
     */
    void clearSynchronizations();
    
    /**
     * Returns a list of all registered ResourceSynchronizations.
     * 
     * \return A list of all registered ResourceSynchronizations
     */
    std::vector<ResourceSynchronization*> synchronizations() const;

    void updateSynchronizationState(ResourceSynchronization::State state);

    /**
     * If the asset has not yet been loaded, this function loads the asset and returns the
     * success state. If the loading succeeded, the Asset transitions into the \c Loaded
     * state. The \p parent that is provided is the Asset that caused this load operation
     * to be triggered and might be \c nullptr if this asset does not have any parents. If
     * this Asset has been previously loaded (even with a different \p parent), this
     * function does nothing.
     * 
     * \param parent The parent asset (or \c nullptr) that triggered this loading
     */
    void load(Asset* parent);

    /**
     * Returns \c true if this Asset has at least one parent that is in the Loaded state.
     * 
     * \return \c true if this Asset has at least one parent that is in the Loaded state
     */
    bool hasLoadedParent();

    /**
     * Returns \c true if this Asset has been successfully #load ed.
     * 
     * /return \c true if this Asset has been successfully loaded
     */
    bool isLoaded() const;

    /**
     * Unloads this Asset and unrequires all of its required assets, potentially causing
     * a cascading effect of further #unload calls if this asset was those required assets
     * only parent. After this call, this Asset will be in the Unloaded state. If this
     * Asset has already been unloaded (or has not yet been loaded), this function does
     * nothing.
     */
    void unload();

    /**
     * Starts the registered synchronizations of this asset and returns \c true if all its
     * synchronizations and required assets' synchronizations could start. When all
     * synchronizations have completed successfully, this Asset transitions into the
     * Synchronized state.
     */
    void startSynchronizations();

    /**
     * Returns \c true if this Asset's synchronizations (if any) have completed
     * successfully.
     * 
     * \return \c true if this Asset is in the Synchronized or Initialized state
     */
    bool isSynchronized() const;

    /**
     * Initializes this asset and returns \ctrue if the initialized succeeded, i.e. if
     * this and all required assets initialized without errors. After this call, if it has
     * been successful, this Asset is in the Initialized state. If the Asset has already
     * been initialized, calling this function does nothing.
     */
    void initialize();

    /**
     * Returns \c true if this Asset has been #initialize d successfully.
     *
     * \return \c true if this Asset has been #initialize d successfully. It returns
     *         \c false both if this initialization failed as well as if thie #initialize
     *         function has not been called on this Asset
     */
    bool isInitialized() const;

    /**
     * Deinitializes this Asset and recursively deinitializes the required assets if this
     * Asset was their ownly initialized parent. If the Asset was already deinitialized,
     * calling this function does nothing.
     */
    void deinitialize();

    void deinitializeIfUnwanted();

    /**
     * Marks the passed \p child as being required by \p this Asset. If the \p child is
     * already required by this asset, this function does nothing.
     * 
     * \param child The asset that is required by this asset
     * \pre \p child must not be nullptr
     */
    void require(Asset* child);

    /**
     * Sets the provided \p metaInformation as the meta information struct for this asset.
     * If previous information existed, it will be silently overwritten.
     *
     * \param metaInformation The meta information about this asset
     */
    void setMetaInformation(MetaInformation metaInformation);

    /**
     * Returns the meta information of this asset back to the caller. If no such
     * information exists, a \c std::nullopt will be returned.
     * 
     * \return The MetaInformation about this asset or \c std::nullopt
     */
    std::optional<MetaInformation> metaInformation() const;

private:
    /// All of the (internal) states that the Asset can move through. The externally
    /// visible states (Loaded, Synchronized, Initialized) are a subset of these states
    enum class State {
        Unloaded,
        LoadingFailed,
        Loaded,
        Synchronizing,
        Synchronized,
        SyncRejected,
        Initialized,
        InitializationFailed
    };

    void setState(State state);

    bool isSyncingOrResolved() const;
    bool isSyncResolveReady() const;
    bool hasInitializedParent() const;

    std::atomic<State> _state = State::Unloaded;
    AssetManager& _manager;

    std::vector<ResourceSynchronization*> _synchronizations;

    // Absolute path to asset file
    std::filesystem::path _assetPath;

    std::optional<MetaInformation> _metaInformation;

    // Required assets
    std::vector<Asset*> _requiredAssets;

    // Assets that refers to this asset as a required asset
    std::vector<Asset*> _requiringAssets;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET___H__
