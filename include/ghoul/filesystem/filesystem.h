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

#ifndef __GHOUL___FILESYSTEM___H__
#define __GHOUL___FILESYSTEM___H__

#include <ghoul/filesystem/file.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

#ifndef WIN32
#include <thread>
#endif // WIN32

namespace ghoul::filesystem {

#ifdef WIN32
struct DirectoryHandle;
void readStarter(DirectoryHandle* directoryHandle);
void callbackHandler(DirectoryHandle* directoryHandle, const std::string& filePath);
#endif // WIN32

class CacheManager;

/**
 * The methods are for dealing with path tokens. These are tokens of the form `${...}`
 * which are like variables, pointing to a specific location. These tokens can only be
 * bound once, as some of the tokens might already have been resolved and changing the
 * tokens later might lead to inconsistencies. For the same reason, it is not possible to
 * unregister tokens. Every FileSystem contains one token `${TEMPORARY}` that points to
 * the location of the system's temporary files.
 */
class FileSystem {
public:
    BooleanType(Override);

    static void initialize();
    static void deinitialize();
    static bool isInitialized();
    static FileSystem& ref();

    /**
     * Registers the path token \p token with this FileSystem. Henceforth, every call to,
     * for example, #absPath, the constructors of File, or Directory, will replace the
     * \p token with \p path. The tokens cannot be removed or replaced afterwards, as this
     * might lead to inconsistencies since some files might have replaced the tokens while
     * others have not.
     *
     * \param token The token in the form `${...}`
     * \param path The path the token should point to
     * \param override If `true` an existing token will be silently overriden
     *
     * \pre \p token must not be empty
     * \pre \p token must start with `{` and end with `}`
     * \pre \p token must not have been registered before if \p override is false
     */
    void registerPathToken(std::string token, std::filesystem::path path,
        Override override = Override::No);

    /**
     * Replaces the path tokens present in the \p path if any exist. If all tokens could
     * be replaced, the method returns `true`; if `false` is returned, one or more tokens
     * could not be replaced. In this case, only part of the path is modified.
     *
     * \param path The path whose tokens should be replaced
     * \param ignoredTokens All tokens contained in this list are ignored during the
     *        resolving of path tokens
     * \return The path with the tokens replaced
     */
    [[nodiscard]] std::filesystem::path expandPathTokens(std::string path,
        const std::vector<std::string>& ignoredTokens = std::vector<std::string>()) const;

    /**
     * Returns a vector of all registered path tokens.
     *
     * \return A vector of all registered path tokens
     */
    std::vector<std::string> tokens() const;

    /**
     * Checks whether the \p token has been registered to a path before.
     *
     * \param token The token to be checked
     * \return `true` if the \p token has been registered before, `false` otherwise
     */
    bool hasRegisteredToken(const std::string& token) const;

    /**
     * Returns `true` if the \p path contains any tokens.
     *
     * \param path The path that is checked for tokens
     * \return `true` if the \p path contains any tokens
     *
     * \pre \p path must not be empty
     */
    bool containsToken(const std::string& path) const;

    /**
     * Creates a CacheManager for this FileSystem. If a CacheManager already exists, this
     * method will fail and log an error. The passed \p directory has to be a valid and
     * existing Directory.
     *
     * \param directory The directory in which all cached files will be stored. Has to be
     *        an existing directory with proper read/write access
     *
     * \pre \p directory must point to an existing directory
     * \pre The CacheManager must not have been created before without destroying it
     */
    void createCacheManager(const std::filesystem::path& directory);

    /**
     * Destroys the previously created CacheManager. The destruction of the CacheManager
     * will trigger a cleanup of the cache directory via the CacheManager destructor.
     * After this method returns, a new CacheManager can be reinitialized with a new
     * cache directory.
     *
     * \pre CacheManager must have been created before
     */
    void destroyCacheManager();

    /**
     * Returns the CacheManager associated with this FileSystem.
     *
     * \return The CacheManager or `nullptr` if it has not been initialized
     *
     * \pre CacheManager must have been created before
     */
    CacheManager* cacheManager();

    /**
     * Listen to \p path for changes. When \p path is changed the \p callback will be
     * called.
     *
     * \param path The file object to be tracked
     * \param callback The callback that will be called when the \p file changes
     *
     * \pre \p path must not be a `nullptr`
     * \pre \p path must not have been added before
     */
    int addFileListener(std::filesystem::path path, File::FileChangedCallback callback);

    /**
     * Removes the file object from tracking lists. The file on the filesystem may still
     * be tracked and other File objects may still have callbacks registered.
     *
     * \pre \p file must not be a `nullptr`
     * \pre \p file must have been added before (addFileListener)
     */
    void removeFileListener(int callbackIdentifier);

    /**
     * Triggers callbacks on filesystem. May not be needed depending on the environment.
     */
    void triggerFilesystemEvents();

#ifdef WIN32
    std::filesystem::path resolveShellLink(std::filesystem::path path);
#endif // WIN32

private:
    /**
     * Constructs a FileSystem object.
     *
     * \throw FileSystemException if the temporary folder cannot be found
     */
    FileSystem();

    /**
     * Deinitializes the FileSystem and removes all registered path tokens. As some tokens
     * might already have been resolved in some paths, deleting the tokens might lead to
     * inconsistencies.
     */
    ~FileSystem();

    FileSystem(const FileSystem& rhs) = delete;
    FileSystem(const FileSystem&&) = delete;
    FileSystem& operator=(const FileSystem& rhs) = delete;
    FileSystem& operator=(FileSystem&& rhs) = delete;

    /// This map stores all the tokens that are used in the FileSystem
    std::map<std::string, std::filesystem::path> _tokenMap;

    /// The cache manager object, only allocated if createCacheManager is called
    std::unique_ptr<CacheManager> _cacheManager;

#ifdef WIN32
    /**
     * Windows specific deinitialize function.
     */
    void deinitializeInternalWindows();

    /**
     * Starts watching a directory.
     */
    void beginRead(DirectoryHandle* directoryHandle);

    /**
     * Handles the callback for a directory for the local file path.
     */
    static void callbackHandler(DirectoryHandle* directoryHandle,
        const std::string& filePath);

    /**
     * External function that calls beginRead.
     */
    friend void readStarter(DirectoryHandle* directoryHandle);

    /**
     * External function that calls callbackHandler.
     */
    friend void callbackHandler(DirectoryHandle* directoryHandle,
        const std::string& filePath);

    struct FileChangeInfo {
        static int NextIdentifier;
        int identifier = -1;
        std::filesystem::path path;
        File::FileChangedCallback callback;
    };
    /// The list of all tracked files
    std::vector<FileChangeInfo> _trackedFiles;

    /// The list of tracked directories
    std::map<std::filesystem::path, DirectoryHandle*> _directories;

#else // ^^^^ WIN32 // !WIN32 vvvv
    /**
     * Linux specific initialize function.
     */
    void initializeInternalLinux();

    /**
     * Linux specific deinitialize function.
     */
    void deinitializeInternalLinux();

    /**
     * Function that run by the watcher thread.
     */
    static void inotifyWatcher();

    int _inotifyHandle;
    bool _keepGoing;
    std::thread _t;

    struct FileChangeInfo {
        static int NextIdentifier;
        int identifier;
        int inotifyHandle;
        std::filesystem::path path;
        File::FileChangedCallback callback;
    };
    /// The list of all tracked files
    std::vector<FileChangeInfo> _trackedFiles;
#endif // WIN32

    static FileSystem* _instance;
};

#define FileSys (ghoul::filesystem::FileSystem::ref())

BooleanType(Recursive);
BooleanType(Sorted);

/**
 * Walks the provided directory in \p path and returns a path of all contained files. If
 * \p recursive is `true`, any directory encountered will be recursively walked and if
 * \p sorted is `true`, the returning vector alphabetically sorted. The \p filter
 * determines for each encountered path whether it should be included or not. If no
 * \p filter is provided all provided paths will be accepted.
 *
 * \param path The directory that should be walked
 * \param recursive If this value is set to `true`, then any directory will be recursively
 *        walked and all files will be provided in a single list
 * \param sorted If this value is `true`, the resulting list will be alphabetically
 *        sorted. If it is `false`, the list will be returned in the order as the
 *        operating system determines
 * \param filter This filter function will be executed for each encounted path, both files
 *        and directories (if \p recursive is `true`). If the filter function returns
 *        `false` for a path, it will not be included in the final list, if it was a file,
 *        and its contents will not be considered, if it was a directory
 *
 * \pre \p path must be a valid and existing directory
 */
std::vector<std::filesystem::path> walkDirectory(const std::filesystem::path& path,
    Recursive recursive = Recursive::No, Sorted sorted = Sorted::No,
    std::function<bool(const std::filesystem::path&)> filter =
        [](const std::filesystem::path&) { return true; });

/**
 * Checks whether \p root is a direct parent of \p p.
 *
 * \return `true` if \p root is a direct parent of \p p. `false` otherwise
 */
bool isSubdirectory(std::filesystem::path p, std::filesystem::path root);

} // namespace ghoul::filesystem


/**
 * Returns the absolute path to the passed \p path, resolving any tokens (if present) in
 * the process. The current working directory is used as a base path for this. All tokens
 * contained in the \p ignoredTokens are ignored from the token resolving.
 *
 * \param path The path that should be converted into an absolute path
 * \return The absolute path to the passed \p path
 *
 * \pre \p path must not be empty
 */
std::filesystem::path absPath(std::string path);

/**
 * \overload std::filesystem::path absPath(std::string path)
 */
std::filesystem::path absPath(const std::filesystem::path& path);

/**
 * \overload std::filesystem::path absPath(std::string path)
 */
std::filesystem::path absPath(const char* path);

#endif // __GHOUL___FILESYSTEM___H__
