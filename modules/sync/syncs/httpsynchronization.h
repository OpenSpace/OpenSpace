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

#ifndef __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__
#define __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__

#include <openspace/util/resourcesynchronization.h>

#include <thread>
#include <optional>
#include <vector>

namespace openspace {

/**
 * A concreate ResourceSynchronization that will request a list of files from a central
 * server (the server list is provided in the constructor) by asking for a specific
 * identifier and a file version and application version addition. The server is expected
 * to return a flat list of files that can be then directly downloaded into the #directory
 * of this synchronization. That list of files can have empty lines and commented out
 * lines (starting with a #) that will be ignored. Every other line is URL that will be
 * downloaded into the #directory.
 *
 * Each requested set of files is identified by a triplet of (identifier, file version,
 * application version). The identifier is denoting the group of files that is requested,
 * the file version is the specific version of this set of files, and the application
 * version is reserved for changes in the data transfer format.
 */
class HttpSynchronization : public ResourceSynchronization {
public:
    /**
     * The constructor for this synchronization object. The \p dict contains information
     * about the `identifier` and the \version (which is the file version), the
     * \p synchronizationRoot is the path to the root folder where the downloaded files
     * will be placed, and the \p synchronizationRepositories is a list of the URLs which
     * will be asked to resolve the (identifier, version) pair. The first URL in the list
     * that can successfully resolve the requested (identifier, version) pair is the one
     * that will be used.
     *
     * \param dict The parameter dictionary (namely the identifier and version)
     * \param synchronizationRoot The path to the root from which the complete #directory
     *        path is constructed
     * \param synchronizationRepositories The list of repositories that will be asked to
     *        resolve the identifier request
     */
    HttpSynchronization(const ghoul::Dictionary& dict,
        std::filesystem::path synchronizationRoot,
        std::vector<std::string> synchronizationRepositories);

    /**
     * Destructor that will close the asynchronous file transfer, if it is still ongoing.
     */
    ~HttpSynchronization() override;

    /**
     * Returns the location to which files downloaded through this ResourceSynchronization
     * are saved.
     *
     * \return The location for files created by this class
     */
    std::filesystem::path directory() const override;

    /**
     * Starts the synchronization for this ResourceSynchronization by first trying to find
     * a synchronization respository that replies to the request, parsing the result and
     * then downloading each of the files that are provided in that result.
     */
    void start() override;

    /**
     * Cancels any ongoing synchronization of this ResourceSynchronization.
     */
    void cancel() override;

    std::string generateUid() override;

    static documentation::Documentation Documentation();

protected:
    /** Creates a file next to the directory that indicates that this
     * ResourceSynchronization has successfully synchronized its contents.
     */
    void createSyncFile(bool isFullySynchronized = true) const override;

    /**
     * Check ossync file and returns true if all files are downloaded or false if
     * partially synched or if there is an ossync file error (rejected).
     */
    bool isEachFileDownloaded();

    /**
     * Representation of 'global' synchronization state that encodes where a fail happen.
     */
    enum class SynchronizationState {
        Success,
        ListDownloadFail,
        FileDownloadFail
    };

private:
    /**
     * Tries to get a reply from the provided URL and returns that success to the caller.
     */
    SynchronizationState trySyncFromUrl(std::string url);

    /// Contains a flag whether the current transfer should be cancelled
    std::atomic_bool _shouldCancel = false;

    /// The file version for the requested files
    int _version = -1;

    bool _unzipFiles = false;
    std::optional<std::string> _unzipFilesDestination = std::nullopt;

    // The list of all repositories that we'll try to sync from
    const std::vector<std::string> _syncRepositories;

    // The thread that will be doing the synchronization
    std::thread _syncThread;

    // The files that have already been synchronized
    std::vector<std::string> _existingSyncedFiles;

    // The files that have been synchronized this time
    std::vector<std::string> _newSyncedFiles;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__
