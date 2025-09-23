/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
 *
 * An asset goes through three external states. Starting out as unloaded when the instance
 * is newly created but the file has not been processed. In this case the #isLoaded,
 * #isSynchronized and the #isInitialized functions all return `false`. After the asset
 * has been loaded it is in the `Loaded` state (#isLoaded = true, #isSynchronized = false,
 * #isInitialized = false). After all registered synchronizations finish successfully, the
 * Asset transitions into the Synchronized state (#isLoaded = true,
 * #isSynchronized = true, #isInitialized = false) and after the final initialization
 * step, the asset is initialized (#isLoaded = true, #isSynchronized = true and
 * #isInitialized = true)
 */
class Asset {
public:
    /// This struct contains all the meta information about this asset
    struct MetaInformation {
        /// The name of the asset
        std::string name;
        /// The version number of the asset. This is only a string representation and does
        /// not have to follow SemVer (even though it is advised)
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
     * required Assets.
     *
     * \param manager The AssetManager that is responsible for loading this Asset and all
     *        of its dependencies
     * \param assetPath The file path that will be used to load this Asset
     * \param explicitEnabled If the contents of this asset should start their life as
     *        being enabled or not
     *
     * \pre The \p assetPath must not be empty and must be an existing file
     */
    Asset(AssetManager& manager, std::filesystem::path assetPath,
        std::optional<bool> explicitEnabled);

    /**
     * Returns the path to the file that was used to initialize this Asset.
     *
     * \return The path to the file that was used to initialize this Asset
     */
    std::filesystem::path path() const;

    /**
     * Adds a new dependent ResourceSynchronization object to this Asset.
     *
     * \param synchronization The resource synchronization object that is bound to this
     *        Asset
     * \pre \p synchronization must not be nullptr
     * \pre \p synchronization must not have been added to this Asset before
     */
    void addSynchronization(ResourceSynchronization* synchronization);

    /**
     * Updates the state of this Asset based on the latest synchronization being
     * successfully resolved. Depending on the sum state of all registered
     * synchronizations this Asset's state changes to successfully Synchronized, or
     * Failed.
     */
    void setSynchronizationStateResolved();

    /**
     * Updates the state of this Asset based on the latest synchronization being rejected.
     * Depending on the sum state of all registered synchronizations this Asset's state
     * changes to successfully Synchronized, or Failed.
     */
    void setSynchronizationStateRejected();


    /**
     * Register a SceneGraphNodeIdentifier with the asset (used to associate Nodes with
     * asset meta).
     */
    void addIdentifier(std::string identifier);

    /**
     * If the asset has not yet been loaded, this function loads the asset and returns the
     * success state. If the loading succeeded, the Asset transitions into the `Loaded`
     * state. The \p parent that is provided is the Asset that caused this load operation
     * to be triggered and might be `nullptr` if this asset does not have any parents. If
     * this Asset has been previously loaded (even with a different \p parent), this
     * function does nothing.
     *
     * \param parent The parent asset (or `nullptr`) that triggered this loading
     */
    void load(Asset* parent);

    /**
     * Returns `true` if this Asset has at least one parent that is in the Loaded state.
     *
     * \return `true` if this Asset has at least one parent that is in the Loaded state
     */
    bool hasLoadedParent();

    /**
     * Returns `true` if this Asset has been successfully #load ed.
     *
     * /return `true` if this Asset has been successfully loaded
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
     * Starts the registered synchronizations of this asset and returns `true` if all its
     * synchronizations and required assets' synchronizations could start. When all
     * synchronizations have completed successfully, this Asset transitions into the
     * Synchronized state.
     *
     * \pre This Asset must have been Loaded before
     */
    void startSynchronizations();

    /**
     * Returns `true` if this Asset's synchronizations (if any) have completed
     * successfully.
     *
     * \return `true` if this Asset is in the Synchronized or Initialized state
     */
    bool isSynchronized() const;

    /**
     * Initializes this asset and returns `true` if the initialized succeeded, i.e. if
     * this and all required assets initialized without errors. After this call, if it has
     * been successful, this Asset is in the Initialized state. If the Asset has already
     * been initialized, calling this function does nothing.
     */
    void initialize();

    /**
     * Returns `true` if this Asset has been #initialize d successfully.
     *
     * \return `true` if this Asset has been #initialize d successfully. It returns
     *         `false` both if this initialization failed as well as if thie #initialize
     *         function has not been called on this Asset
     */
    bool isInitialized() const;

    /**
     * Returns whether any of the parents of this Asset is currently in an initialized
     * state, meaning that any parent is still interested in this Asset at all.
     *
     * \return `true` if there is at least one initialized parent, `false` otherwise
     */
    bool hasInitializedParent() const;

    /**
     * Deinitializes this Asset and recursively deinitializes the required assets if this
     * Asset was their ownly initialized parent. If the Asset was already deinitialized,
     * calling this function does nothing.
     */
    void deinitialize();

    /**
     * Marks the passed \p dependency as being required by this Asset. If the
     * \p dependency is already required by this asset, this function does nothing.
     *
     * \param dependency The asset that is required by this asset
     *
     * \pre \p dependency must not be nullptr
     */
    void require(Asset* dependency);

    /**
     * Returns `true` if the loading of the Asset has failed in any way so that
     * recovering from the error is impossible.
     *
     * \return `true` if the Asset handling failed in any way, `false` otherwise
     */
    bool isFailed() const;

    /**
     * Returns the state of whether this asset was loaded with an explicit enable/disable
     * or not. If it was specified, the optional will have a value and the value is
     * whether the original `require` call asked for a true/false. If no such parameter
     * was used, the optional will not contain a value.
     *
     * \return Whether the asset was loaded with an explicit enable or not
     */
    std::optional<bool> explicitEnabled() const;

    /**
     * Returns the asset that has first loaded this asset or nullptr if that has not been
     * determined yet.
     *
     * \return The asset that has first loaded this asset
     */
    Asset* firstParent() const;

    /**
     * Sets the provided \p metaInformation as the meta information struct for this asset.
     * If previous information existed, it will be silently overwritten.
     *
     * \param metaInformation The meta information about this asset
     */
    void setMetaInformation(MetaInformation metaInformation);

    /**
     * Returns the meta information of this asset back to the caller. If no such
     * information exists, a `std::nullopt` will be returned.
     *
     * \return The MetaInformation about this asset or `std::nullopt`
     */
    std::optional<MetaInformation> metaInformation() const;

private:
    /// All of the (internal) states that the Asset can move through. The externally
    /// visible states (Loaded, Synchronized, Initialized) are a subset of these states
    enum class State {
        /// The asset is created, but the Lua file has not been executed yet
        Unloaded,
        /// The execution of the asset file as Lua failed with some error
        LoadingFailed,
        /// The loading of the asset file succeeded
        Loaded,
        /// The Asset is currently synchronizing its ResourceSynchronizations and waiting
        /// for them to finish
        Synchronizing,
        /// All registered synchronizations have completed successfully
        Synchronized,
        /// At least one of the registered synchronizations failed to synchronize
        SyncRejected,
        /// The onInitialize method (if the asset has one) was executed successfully
        Initialized,
        /// The execution of the onInitialize method (if existing) resulted in a Lua error
        InitializationFailed
    };

    /**
     * Sets the \p state of this Asset to the new state. Depending on the current state of
     * this Asset, if \p state is a state related to the synchronization, it will
     * potentially propagate to this Asset's parents and cause them to be set to be
     * successfully synchronized or faild.
     *
     * \param state The new State that this Asset is set to
     */
    void setState(State state);

    /**
     * Returns whether the Asset is synchronizing or has successfully synchronized.
     */
    bool isSyncingOrResolved() const;

    /**
     * Returns whether the Asset has been successfully synchronized, meaning that both its
     * own resource synchronizations are finished as well as all requiered assets are
     * finished synchronizing.
     */
    bool isSyncResolveReady() const;

    /// The state that this Asset is currently in
    std::atomic<State> _state = State::Unloaded;

    /// Reference to the manager that is responsible for loading and unloading this asset
    AssetManager& _manager;

    /// Absolute path to asset file
    std::filesystem::path _assetPath;

    /// Additional information about this asset, such as its name, author, license, etc
    std::optional<MetaInformation> _metaInformation;

    /// Assets that are required by this asset
    std::vector<Asset*> _requiredAssets;

    /// Assets that refers to this asset as a required asset
    std::vector<Asset*> _parentAssets;

    /// Synchronizations that were requested by this asset
    std::vector<ResourceSynchronization*> _synchronizations;

    /// The parameter that was passed into the original `require` call whether the
    /// contents of this asset should be enabled or disabled
    std::optional<bool> _explicitEnabled = std::nullopt;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET___H__
