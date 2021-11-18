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
 * function, whose dictionary must contain at least a \c Type value that specifies which
 * type of ResourceSynchronization should be create. Any type that is registered to the
 * global factory is a valid type here.
 * A ResourceSynchronization state can be in one of four states that can be queried
 * through the #isSyncing, #isResolved, and #isRejected functions. The available states
 * are:
 * \c Unsynchronized (#isSyncing = false, #isResolved = false, #isRejected = false),
 * \c Syncing (#isSyncing = true, #isResolved = false, #isRejected = false),
 * \c Resolved (#isSyncing = false, #isResolved = true, #isRejected = false),
 * \c Rejected (#isSyncing = false, #isResolved = false, #isRejected = true)
 */
class ResourceSynchronization {
public:
    /**
     * Creates a new ResourceSynchronization based on the \p dictionary information that
     * is passed into this function. The dictionary must contain at least a \c Type, an
     * \c Identifier, and a \c Name, with other optional parameters depending on the
     * specific subtype of ResourceSynchronization class is is specified through the
     * provided \c Type.
     * 
     * \param dictionary The dictionary containing the parameters with which to choose the
     *        specific type of ResourceSynchronization and all the necessary parameters to
     *        initialize said ResourceSynchronization
     * 
     * \throw SpecificationError If the \p dictionary does not contain a \c Type, an
     *        \c Identifier, and a \c Name
     */
    static std::unique_ptr<ResourceSynchronization> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    /**
     * Generates a unique identifying string for the dictionary that is based on the
     * \c Type and the \c Identifier values of the passed \p dictionary. All other
     * parameters are ignored, but as long as the \c Type and/or the \c Identifier values
     * differ, the resulting string will be different.
     * 
     * \param dictionary The dictionary containing the \c Type and the \c Identifier used
     *        to create a unique identifier
     * 
     * \throw SpecificationError If the \p dictionary does not contain a \c Type, an
     *        \c Identifier, and a \c Name
     */
    static std::string generateUid(const ghoul::Dictionary& dictionary);

    /// Defaulted virtual constructor
    virtual ~ResourceSynchronization() = default;

    /**
     * Returns the location to which files downloaded through this ResourceSynchronization
     * are saved.
     * 
     * \return The location for files created by this class
     */
    virtual std::filesystem::path directory() const = 0;

    /// Starts the synchronization for this ResourceSynchronization
    virtual void start() = 0;

    /// Cancels any ongoing synchronization of this ResourceSynchronization
    virtual void cancel() = 0;

    /**
     * Returns the number of bytes that have already been synchronized or 0 if the
     * synchronization hasn't started yet.
     * 
     * \return The number of synchronized bytes
     */
    virtual size_t nSynchronizedBytes() const = 0;

    /**
     * Returns the number of total bytes that ought to be synchronized for this
     * ResourceSynchronization to be considered complete. If that number is not known
     * (yet), the returned value is 0.
     * 
     * \return The total number of required bytes
     */
    virtual size_t nTotalBytes() const = 0;

    /**
     * Returns \c true if the total number of bytes for this ResourceSynchronization is
     * known. Will return \c false otherwise
     * 
     * \return The state whether the number of total bytes is known or not
     */
    virtual bool nTotalBytesIsKnown() const = 0;

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
     * \return \c true if this object is currently synchronizing
     */
    bool isSyncing() const;

    /**
     * Returns whether this ResourceSynchronization has successfully finished
     * synchronizing all of its files. Once this has returned \c true, it will stay so
     * until the object is destroyed and it is guaranteed that no more files will be added
     * to the #directory.
     * 
     * \return \c true if this object is finished synchronizing
     */
    bool isResolved() const;

    /**
     * Returns whether this ResourceSynchronization has failed to synchronizing all or any
     * of its files. Once this has returned \c true, it will stay so until the object is
     * destroyed. Some subclasses might try to download as many files as possible, but no
     * general guarantee is provided regarding the completeness of the download.
     * 
     * \return \c true if this object has failed synchronizing one or more of the required
     *         files
     */
    bool isRejected() const;

    static documentation::Documentation Documentation();

protected:
    /// Representation of the state that this object can be in
    enum class State {
        Unsynced,
        Syncing,
        Resolved,
        Rejected
    };

    /// Creates a file next to the directory that indicates that this
    /// ResourceSynchronization has successfully synchronized its contents
    void createSyncFile() const;

    /// Returns whether the synchronization file create in #createSyncFile exists
    bool hasSyncFile() const;

    /// The internal identifier for this ResourceSynchronization. It is not enforced but
    /// advised that this identifier be different for all instances of the same subtype
    std::string _identifier;

    /// The user-facing name of this ResourceSynchronization
    std::string _name;

    /// The current #State of this ResouceSynchronization
    std::atomic<State> _state = State::Unsynced;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
