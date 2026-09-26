/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___CACHEMANAGER___H__
#define __GHOUL___CACHEMANAGER___H__

#include <filesystem>
#include <map>
#include <optional>
#include <string_view>

namespace ghoul::filesystem {

/**
 * The CacheManager allows users to request a storage location for a file path to store a
 * cached result. This class only generates and manages the file paths and does not do any
 * caching itself. The use case for this is for expensive operations that has have result,
 * which gets written to a file and the developer wants to retain the results without
 * recomputing it at every application start. Using the same `file` and `information`
 * values, the same path will be retrieved in subsequent application runs. The persistent
 * files are stored in a `cache` file so that they can be retained between application
 * runs. If two CacheManagers are pointing at the same directory, the result is undefined.
 */
class CacheManager {
public:
    /**
     * The constructor will automatically register all persistent cache entries from
     * previous application runs. After the constructor returns, the persistent files are
     * correctly registered and available.
     *
     * \param directory The directory that is used for the CacheManager
     *
     * \throw MalformedCacheException If the cache file could is malformed
     * \throw RuntimeError If the previous cache could not be loaded
     * \pre \p directory must not be empty
     */
    CacheManager(std::filesystem::path directory);

    /**
     * The destructor will save all information in a `cache` file in the cache
     * directory that was passed in the constructor so that they can be retrieved when the
     * application is started up again.
     */
    ~CacheManager();

    /**
     * Returns the path to a storage location for the cached file. If no information is
     * provided, the method will use the date of last modification as a unique identifier
     * for the file. Subsequent calls (in the same run or different) with the same \p file
     * and \p information will consistently produce the same file path. The combination of
     * \p file and \p information is the unique key for the returned cached file.
     *
     * \param file The file name of the file for which the cached entry is to be retrieved
     * \param information Additional information that is used to uniquely identify the
     *        cached file. The combination of the \p file and \p information must uniquely
     *        identify a cached file
     * \param subDirectory If this is a non-empty string, the cached file name will be
     *        located inside the provided subdirectory
     * \return The cached file that can be used by the caller to store the results
     *
     * \throw RuntimeError If there is an illegal character (`/`, `\\`, `?`, `%`, `*`,
     *        `:`, `|`, `"`, `<`, `>`, or `.`) in the \p file
     */
    [[nodiscard]] std::filesystem::path cachedFilename(const std::filesystem::path& file,
        std::optional<std::string_view> information = std::nullopt,
        std::string_view subDirectory = "");

    /**
     * This method checks if a cached \p file has been registered before in this or in a
     * previous application run with the provided \p information. If no information is
     * provided, the method will use the date of last modification as a unique identifier
     * for the file. Note that this only checks if a file has been requested before, not
     * if the cached file has actually been used.
     *
     * \param file The file for which the cached file should be searched
     * \param information The identifying information for the file
     * \return `true` if a cached file was requested before; `false` otherwise
     *
     * \throw IllegalArgumentException If there is an illegal character (`/`, `\`, `?`,
     *        `%`, `*`, `:`, `|`, `"`, `<`, `>`, or `.`) in the \p file
     */
    [[nodiscard]] bool hasCachedFile(const std::filesystem::path& file,
        std::optional<std::string_view> information = std::nullopt) const;

    /**
     * Removes the cached file and deleted the entry from the CacheManager. If the
     * `file` has not previously been used to request a cache entry, no error will be
     * signaled. If no information is provided, the method will use the date of last
     * modification as a unique identifier for the file.
     *
     * \param file The file for which the cache file should be deleted
     * \param information The detailed information for the cached file which should be
     *        deleted
     *
     * \throw IllegalArgumentException If there is an illegal character (`/`, `\`, `?`,
     *        `%`, `*`, `:`, `|`, `"`, `<`, `>`, or `.`) in the \p file
     */
    void removeCacheFile(const std::filesystem::path& file,
        std::optional<std::string_view> information = std::nullopt);

protected:
    /// The cache directory
    std::filesystem::path _directory;

    /// A map containing file hashes and file information
    std::map<unsigned long, std::filesystem::path> _files;
};

} // namespace ghoul::filesystem

#endif // __GHOUL___CACHEMANAGER___H__
