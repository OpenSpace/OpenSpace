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

#ifndef __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
#define __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__

#include <atomic>
#include <filesystem>
#include <string>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * The ResourceSynchronization class handles the download of persistent datasets, meaning
 * that the contents of the data is only downloaded once at the beginning of the
 * application. A ResourceSynchronization is created through the #createFromDictionary
 * function, whose dictionary must contain at least a `Type` value that specifies which
 * type of ResourceSynchronization should be create. Any type that is registered to the
 * global factory is a valid type here.
 *
 * A ResourceSynchronization state can be in one of four states that can be queried
 * through the #isSyncing, #isResolved, and #isRejected functions. The available states
 * are:
 *   - `Unsynchronized` (#isSyncing = false, #isResolved = false, #isRejected = false)
 *   - `Syncing` (#isSyncing = true, #isResolved = false, #isRejected = false)
 *   - `Resolved` (#isSyncing = false, #isResolved = true, #isRejected = false)
 *   - `Rejected` (#isSyncing = false, #isResolved = false, #isRejected = true)
 */
class ResourceSynchronization {
public:
    /**
     * Creates a new ResourceSynchronization based on the \p dictionary information that
     * is passed into this function. The dictionary must contain at least a `Type`, an
     * `Identifier`, and a `Name`, with other optional parameters depending on the
     * specific subtype of ResourceSynchronization class is is specified through the
     * provided `Type`.
     *
     * \param dictionary The dictionary containing the parameters with which to choose the
     *        specific type of ResourceSynchronization and all the necessary parameters to
     *        initialize said ResourceSynchronization
     *
     * \throw SpecificationError If the \p dictionary does not contain a `Type`, an
     *        `Identifier`, and a `Name`
     */
    static std::unique_ptr<ResourceSynchronization> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    /**
     * Generates a unique identifying string for ResourceSynchronizaiton.
     */
    virtual std::string generateUid() = 0;

    /**
     * Defaulted virtual constructor.
     */
    virtual ~ResourceSynchronization() = default;

    /**
     * Returns the location to which files downloaded through this ResourceSynchronization
     * are saved.
     *
     * \return The location for files created by this class
     */
    virtual std::filesystem::path directory() const = 0;

    /**
     * Starts the synchronization for this ResourceSynchronization.
     */
    virtual void start() = 0;

    /**
     * Cancels any ongoing synchronization of this ResourceSynchronization.
     */
    virtual void cancel() = 0;

    /**
     * Returns the number of bytes that have already been synchronized or 0 if the
     * synchronization hasn't started yet. This number always will only contain the number
     * of bytes of actual payload data, not any additional data transfers that some
     * subtypes might require.
     *
     * \return The number of synchronized bytes
     */
    size_t nSynchronizedBytes() const;

    /**
     * Returns the number of total bytes that ought to be synchronized for this
     * ResourceSynchronization to be considered complete. If that number is not known
     * (yet), the returned value is 0. This number always will only contain the number of
     * bytes of actual payload data, not any additional data transfers that some subtypes
     * might require.
     *
     * \return The total number of required bytes
     */
    size_t nTotalBytes() const;

    /**
     * Returns `true` if the total number of bytes for this ResourceSynchronization is
     * known. Will return `false` otherwise. This number always will only contain the
     * number of bytes of actual payload data, not any additional data transfers that some
     * subtypes might require.
     *
     * \return The state whether the number of total bytes is known or not
     */
    bool nTotalBytesIsKnown() const;

    /**
     * Returns the unique identifier of this ResourceSynchronization.
     *
     * \return The unique identifier of this ResourceSynchronization
     */
    const std::string& identifier() const;

    /**
     * Returns the name of this ResourceSynchronization.
     *
     * \return The name of this ResourceSynchronization
     */
    const std::string& name() const;

    /**
     * Returns whether this ResourceSynchronization is currently syncing its files and has
     * not finished doing so.
     *
     * \return `true` if this object is currently synchronizing
     */
    bool isSyncing() const;

    /**
     * Returns whether this ResourceSynchronization has successfully finished
     * synchronizing all of its files. Once this has returned `true`, it will stay so
     * until the object is destroyed and it is guaranteed that no more files will be added
     * to the #directory.
     *
     * \return `true` if this object is finished synchronizing
     */
    bool isResolved() const;

    /**
     * Returns whether this ResourceSynchronization has failed to synchronizing all or any
     * of its files. Once this has returned `true`, it will stay so until the object is
     * destroyed. Some subclasses might try to download as many files as possible, but no
     * general guarantee is provided regarding the completeness of the download.
     *
     * \return `true` if this object has failed synchronizing one or more of the required
     *         files
     */
    bool isRejected() const;

    static documentation::Documentation Documentation();

protected:
    /**
     * Empty constructor that just sets the synchronization root.
     */
    ResourceSynchronization(std::filesystem::path synchronizationRoot);

    /**
     * Representation of the state that this object can be in.
     */
    enum class State {
        Unsynced,
        Syncing,
        Resolved,
        Rejected
    };

    /**
     * Creates a file next to the directory that indicates that this.
     * ResourceSynchronization has successfully synchronized its contents.
     */
    virtual void createSyncFile(bool isFullySynchronized = true) const;

    /**
     * Returns whether the synchronization file create in #createSyncFile exists.
     */
    bool hasSyncFile() const;

    /// The internal identifier for this ResourceSynchronization. It is not enforced but
    /// advised that this identifier be different for all instances of the same subtype
    std::string _identifier;

    /// The user-facing name of this ResourceSynchronization
    std::string _name;

    /// The path to the root folder relative to which synchronization files are placed
    const std::filesystem::path _synchronizationRoot;

    /// The current #State of this ResouceSynchronization
    std::atomic<State> _state = State::Unsynced;

    /// Contains the fact whether the total number of payload bytes is known
    std::atomic_bool _nTotalBytesKnown = false;

    /// Contains the total number of payload bytes or 0 if that number is not known
    std::atomic_size_t _nTotalBytes = 0;

    /// Contains the number of already synchronized payload bytes
    std::atomic_size_t _nSynchronizedBytes = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
