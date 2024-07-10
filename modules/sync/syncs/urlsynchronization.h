/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_SYNC___URLSYNCHRONIZATION___H__
#define __OPENSPACE_MODULE_SYNC___URLSYNCHRONIZATION___H__

#include <openspace/util/resourcesynchronization.h>

#include <atomic>
#include <filesystem>
#include <string>
#include <thread>
#include <vector>

namespace openspace {

/**
 * The UrlSynchronization will download one or more files by directly being provided with
 * the list of URLs to the files that should be downloaded. The `Override` option in the
 * Dictionary determines what should happen in a file with the same name and the same
 * identifier has been previously downloaded.
 */
class UrlSynchronization : public ResourceSynchronization {
public:
    /**
     * The constructor that takes the parameter \p dictionary and the
     * \p synchronizationRoot to the location that is used as the base to compute the
     * final storage location. The provided list of URLs must not contain any duplicates.
     *
     * \param dictionary The parameter dictionary that contains all information that this
     *        UrlSynchronization needs to download the provided files
     * \param synchronizationRoot The base location based off which the final placement
     *        is calculated
     */
    UrlSynchronization(const ghoul::Dictionary& dictionary,
        std::filesystem::path synchronizationRoot);

    /**
     * Contructor that will terminate the synchronization thread if it is still running.
     */
    ~UrlSynchronization() override;

    /**
     * Returns the location to which files downloaded through this ResourceSynchronization
     * are saved.
     *
     * \return The location for files created by this class
     */
    std::filesystem::path directory() const override;

    /**
     * Starts the synchronization for this ResourceSynchronization.
     */
    void start() override;

    /**
     * Cancels any ongoing synchronization of this ResourceSynchronization.
     */
    void cancel() override;

    std::string generateUid() override;

    static documentation::Documentation Documentation();

protected:
    /**
     * Read the `ossync` file and check if the downloaded files can be used. Returns
     * `true` if they are valid and `false` if we should download them again.
     */
    bool isEachFileValid();

private:
    static constexpr double MaxDateAsJ2000 = 252424036869.18289;

    /**
     * Creates a file next to the directory that indicates that this
     * ResourceSynchronization has successfully synchronized its contents.
     */
    void createSyncFile(bool isFullySynchronized = true) const override;

    /**
     * Tries to get a reply from the asset URLs and returns that success to the caller.
     */
    bool trySyncUrls();

    /// The list of URLs that will be downloaded
    std::vector<std::string> _urls;

    /// Setting whether existing files should be ignored (false) or overwritten (true)
    bool _forceOverride = false;

    /// An optional filename that might overwrite the storage destination. This is only
    /// valid if a single URL is specified
    std::string _filename;

    /// Contains a flag whether the current transfer should be cancelled
    std::atomic_bool _shouldCancel = false;

    // The thread that will be doing the synchronization
    std::thread _syncThread;

    /// Determines how long the file is valid before it should be downloaded again
    double _secondsUntilResync = MaxDateAsJ2000;

    inline static std::mutex _mutex;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___URLSYNCHRONIZATION___H__
