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

#ifndef __OPENSPACE_CORE___HTTPREQUEST___H__
#define __OPENSPACE_CORE___HTTPREQUEST___H__

#include <ghoul/misc/boolean.h>
#include <atomic>
#include <condition_variable>
#include <filesystem>
#include <fstream>
#include <functional>
#include <optional>
#include <string>
#include <thread>
#include <vector>
#include <chrono>

namespace openspace {

/**
 * This class performs a synchronous HTTP request to the provided URL. Any result that is
 * returned based on this request is returned through three callback functions that can be
 * registered using the #onHeader, #onProgress, and #onData functions. Calling these
 * functions will overwrite any previously registered handler. The ProgressCallback can be
 * used to stop the download if the handler returns `false`.
 *
 * The workflow for this class:
 *   1. Create a new object with the URL that points to the location from which the data
 *      should be loaded
 *   2. Register the callbacks that are needed (at least the #onData callback)
 *   3. Start the download with the #perform function
 */
class HttpRequest {
public:
    /**
     * This callback is called every time there is progress to report for the download. It
     * transmits currently downloaded number of bytes (which can be 0 if the download
     * hasn't started yet) and the total number of bytes if that value is known. The
     * return value determines if the download should continue (if the function returns
     * `true`) or if it should abort (if `false` is returned).
     *
     * \param downloadedBytes The number of bytes that have been downloaded thus far or 0
     *        if the download hasn't started yet
     * \param totalBytes The total number of bytes for the download if it is known, or a
     *        std::nullopt of the total size is unknown
     * \return `true` if the download should continue or `false` if it should be aborted
     */
    using ProgressCallback = std::function<
        bool(size_t downloadedBytes, std::optional<size_t> totalBytes)
    >;

    /**
     * This callback is executed every time a chunk of data arrived for the download. The
     * parameters point to the buffer of this new incoming data and the number of bytes
     * that have arrived. The buffer pointed to will most likely only be valid during the
     * time of the callback and it is the callbacks responsibility to store the contents
     * of the buffer before the callback returns. If the return value is `true`, the
     * download continues, if it is `false`, this signals to the library that an error has
     * occurred from which recovery is not possible.
     *
     * \param buffer The pointer to the beginning of the buffer where the new incoming
     *        data is located. This buffer is only valid during the execution of this
     *        callback and the contents of the buffer should be copied to a different
     *        location
     * \param size The number of bytes that are contained in the \p buffer
     * \return `true` if the download should continue or `false` if it should be aborted
     */
    using DataCallback = std::function<bool(char* buffer, size_t size)>;

    /**
     * This callback is executed when the header of an HTTP request is received
     * successfully. The provided buffer and size contain the contents of the header field
     * for the response. The buffer pointed to will most likely only be valid during the
     * time of the callback and it is the callbacks responsibility to store the contents
     * of the buffer before the callback returns. If the return value is `true`, the
     * download continues, if it is `false`, this signals to the library that an error has
     * occurred from which recovery is not possible. If this function returns `false`, it
     * will cause the main download to not start at all.
     *
     * \param buffer The pointer to the beginning of the buffer where the header
     *        information is located. This buffer is only valid during the execution of
     *        this callback and the contents of the buffer should be copied to a different
     *        location
     * \param size The number of bytes that are contained in the \p buffer
     * \return `true` if the download should continue or `false` if it should be aborted
     */
    using HeaderCallback = std::function<bool(char* buffer, size_t size)>;

    /**
     * Creates a new HttpRequest object that will try to download the contents at the
     * provided \p url.
     *
     * \param url The URL that should be requested by this HttpRequest
     *
     * \pre \p url must not be empty
     */
    explicit HttpRequest(std::string url);

    /**
     * Registers a callback that will be called when the header for the request has been
     * transmitted successfully. The contents of the header will be passed into the
     * callback and the callback returns whether the request should proceed or be aborted.
     *
     * \param cb The callback that should be registered. This will silently replace any
     *        previously registered callback
     */
    void onHeader(HeaderCallback cb);

    /**
     * Registers a callback that will be called whenever there is progress to be reported
     * on the transfer of the request's body. The callback will receive information about
     * the number of bytes that have been downloaded and the total number of bytes that
     * should be downloaded to complete the request. This information might not always be
     * available. The callback's return value determines whether the request should
     * continue or be aborted.
     *
     * \param cb The callback that should be registered. This will silently replace any
     *        previously registered callback
     */
    void onProgress(ProgressCallback cb);

    /**
     * Registers a callback that will be called whenever there is new data that has been
     * received from the request. It is this callback's responsibility to store that data
     * in a place that is persistent across multiple calls to the callback, usually by
     * storing it in an external buffer and appending to it. The callback can return
     * whether the download should proceed (by returning `true`) or be aborted (by
     * returning `false`).
     *
     * \param cb The callback that should be registered. This will silently replace any
     *        previously registered callback
     */
    void onData(DataCallback cb);

    /**
     * Performs the request to the URL provided in the constructor. As this request is
     * handled synchronously, this function will only return once the request has been
     * completed successfully or failed. During this call, the registered callbacks will
     * be called repeatedly until the request finishes. This function returns whether the
     * request was completed successfully or failed.
     *
     * \param timeout The amount of time the request will wait before aborting due to the
     *        server not responding. If this value is 0, there is no timeout on the
     *        request.
     * \return `true` if the request completed successfully, `false` otherwise
     */
    bool perform(std::chrono::milliseconds timeout = std::chrono::milliseconds(0));

    /**
     * Returns the URL that was passed into the constructor of this HttpRequest.
     *
     * \return The URL that was passed into the constructor of this HttpRequest
     */
    const std::string& url() const;

private:
    /// The callback that will be called when the header was received successfully
    HeaderCallback _onHeader;

    /// The callback that will be called when there is progress to be reported
    ProgressCallback _onProgress;

    /// The callback that will be called when there is data to be stored away
    DataCallback _onData;

    /// The URL that this HttpRequest is going to request
    std::string _url;
};

/**
 * This abstract base class uses the HttpRequest class to perform an asynchronous
 * download. Every subclass needs to implement at least the #handleData function that will
 * be called every time a chunk of data has been received from the request. The download
 * is started through the #start function and it is possible to turn this into a
 * synchronous download by executing the #wait function directly afterwards. If a
 * HttpRequest::ProgressCallback has been registered through the #onProgress function,
 * that callback is called every time the download some progress to report.
 */
class HttpDownload {
public:
    /**
     * Creates a new HttpDownload that will start to download the file pointed to by the
     * \p url parameter as soon as the download is #start ed.
     *
     * \param url The URL that should be downloaded by this HttpDownload
     *
     * \pre \p url must not be empty
     */
    explicit HttpDownload(std::string url);

    /**
     * Virtual destructor that will cancel the ongoing download and block until the
     * download is successfully canceled.
     */
    virtual ~HttpDownload();

    /**
     * Registers a callback that will be called whenever there is progress to be reported
     * on the file's download. The callback will receive information about the number of
     * bytes that have been downloaded and the total number of bytes that should be
     * downloaded. This information might not always be available. The callback's return
     * value determines whether the request should continue or be aborted.
     *
     * \param progressCallback The callback that should be registered. This will silently
     *        replace any previously registered callback
     */
    void onProgress(HttpRequest::ProgressCallback progressCallback);

    /**
     * Starts the asynchronous download of the file by starting a new thread that will
     * take care of the download, meaning that this function will return almost
     * instantaneously. If the HttpDownload is already downloading a file this function
     * does nothing.
     *
     * \param timeout The number of milliseconds that the download will be kept alive
     *        while waiting for a reply from the server. If this value is 0, the
     *        connection will never time out
     */
    void start(std::chrono::milliseconds timeout = std::chrono::milliseconds(0));

    /**
     * Cancels the ongoing download. Because of the underlying library that is used, the
     * transfer will only be aborted the next time any piece of data is received or the
     * library reports any progress.
     */
    void cancel();

    /**
     * This function will wait until the download has completed and will return the
     * success of the download back to the caller.
     *
     * \return `true` if the downloaded succeeded or `false` if the download failed
     */
    bool wait();

    /**
     * Returns `true` if the download has completed and it failed, or `false` if either
     * the download is till ongoing or is finished and has succeeded.
     *
     * \return Whether the download has completed and it failed
     */
    bool hasFailed() const;

    /**
     * Returns `true` if the download has completed successfully , or `false` if either
     * the download is till ongoing or is finished and has failed.
     *
     * \return Whether the download has completed successfully
     */
    bool hasSucceeded() const;

    /**
     * Returns the URL that was passed into the constructor of this HttpDownload.
     *
     * \return The URL that was passed into the constructor of this HttpDownload
     */
    const std::string& url() const;

protected:
    /**
     * This abstract function has to be implemented by any concrete subclass to handle an
     * incoming chunk of data from the download. The parameters point to the \p buffer of
     * this new incoming data and the number of bytes that have arrived. The buffer
     * pointed to will most likely only be valid during the time of the callback and it is
     * the callbacks responsibility to store the contents of the buffer before the
     * callback returns. If the return value is `true`, the download continues, if it is
     * `false`, this signals to the library that an error has occurred from which recovery
     * is not possible. This function will be called on a different thread from the one
     * that called the #start method.
     *
     * \param buffer The beginning of the buffer of this chunk of data
     * \param size The number of bytes that the \p buffer contains
     * \return The implementation should return `true` if the downloading should continue
     *         and `false` if the handling of the data caused some error that the
     *         subclass is incapable of recovering from
     */
    virtual bool handleData(char* buffer, size_t size) = 0;

    /**
     * This function is called before the downloading starts and can be used by subclasses
     * to perform one-time setup functions, such as opening a file, reserving a block of
     * storage, etc. This function guaranteed to be only called once per HttpDownload. The
     * return value determines if the setup operation completed successfully or if an
     * error occurred that will cause the download to be terminated. This function will be
     * called on a different thread from the one that called the #start method.
     *
     * \return `true` if the setup completed successfully and `false` if the setup
     *         failed unrecoverably
     */
    virtual bool setup();

    /**
     * This function is called after the downloading has finished and before a potential
     * call to #wait is performed. This function can be used by a subclass to perform
     * one-time operations that are required when the downloading fininshes, such as
     * closing file handles, committing some memory etc. The return value of this function
     * signals whether the teardown completed successfully. This function will be called
     * on a different thread from the one that called the #start method.
     *
     * \return `true` if the teardown completed successfully and `false` if it failed
     */
    virtual bool teardown();

private:
    /// The callback that will be called whenever there is some progress to be reported
    HttpRequest::ProgressCallback _onProgress;

    /// Value indicating whether the HttpDownload is currently downloading a file
    bool _isDownloading = false;

    /// Value indicating whether the download is finished
    bool _isFinished = false;

    /// Value indicated whether the download was successful
    bool _isSuccessful = false;

    /// Marker telling the downloading thread that the download should be cancelled
    bool _shouldCancel = false;

    /// The HttpRequest class that will be used for the download
    HttpRequest _httpRequest;

    /// The thread that contains the HttpRequest to download the file
    std::thread _downloadThread;

    /// This condition variable is used by the #wait function to be able to wait for
    /// completion of the downloading thread
    std::condition_variable _downloadFinishCondition;
};

/**
 * This specific subclass of the HttpDownload downloads the contents of the provided URL
 * into a file on disk. By default, an existing file will not be overwritten and will
 * cause the download to fail. This behavior can be overwritten through a parameter in the
 * constructor of this class.
 */
class HttpFileDownload : public HttpDownload {
public:
    BooleanType(Overwrite);

    /**
     * Constructor that will create a HttpFileDownload which will download the contents of
     * the provided \p url to the \p destinationPath. If the \p destinationPath already
     * contains a file and \p overwrite is Overwrite::No, the download will fail; if it is
     * Overwrite::Yes, the existing content at the \p destinationPath will be overwritten.
     */
    HttpFileDownload(std::string url, std::filesystem::path destinationPath,
        Overwrite overwrite = Overwrite::No);

    /**
     * This destructor will cancel any ongoing download and wait for its completion, so it
     * might not block for a short amount of time.
     */
    virtual ~HttpFileDownload() override = default;

    /**
     * Returns the path where the contents of the URL provided in the constructor will be
     * saved to.
     *
     * \return The path where URL will be downloaded to
     */
    std::filesystem::path destination() const;

private:
    /**
     * Will create all directories that are necessary to reach _destination and then
     * fight with other HttpFileDownloads to get one of a limited number of file handles
     * to open _file.
     */
    bool setup() override;

    /**
     * Closes the _file and returns the handle back to the pool of available handles.
     */
    bool teardown() override;

    /**
     * Stores the chunk of data into the _file handle.
     */
    bool handleData(char* buffer, size_t size) override;

    /// A flag whether this HttpFileDownload got a handle from the limited supply of
    /// handles. This limit is used to prevent all HttpFileDownloads from getting file
    /// handles from the operating system as that resource is limited and downloads would
    /// fail unrecoverably if no handles are available. So we limit the maximum number and
    /// if that number is exceeded, the HttpFileDownload will wait until a handle is
    /// available
    std::atomic_bool _hasHandle = false;

    /// The destination path where the contents of the URL provided in the constructor
    /// will be saved to
    std::filesystem::path _destination;

    /// The file handle to the _destination used to save incoming chunks
    std::ofstream _file;

    /// Mutex that will be prevent multiple HttpFileDownloads to simultaneously try to
    /// create the necessary intermediate directories, which would cause issues
    static std::mutex _directoryCreationMutex;

    /// The maximum number of file handles that all HttpFileDownloads combined can use up
    static constexpr int MaxFileHandles = 32;

    /// Stores the number of currently open file handles across all HttpFileDownloads
    static std::atomic_int nCurrentFileHandles;
};

/**
 * This concerete HttpDownload subclass downloads the contents of the URL passed into the
 * constructor into a buffer of memory that can be retrieve. Please note that that buffer
 * should only be used accessed once the HttpDownload::hasFinished function returns
 * `true`.
 */
class HttpMemoryDownload : public HttpDownload {
public:
    /**
     * Creates an instance of a HttpMemoryDownload that will download the contents of the
     * \p url into memory.
     *
     * \param url The URL whose contents should be downloaded
     */
    explicit HttpMemoryDownload(std::string url);

    /**
     * This destructor will cancel any ongoing download and wait for its completion, so it
     * might not block for a short amount of time.
     */
    virtual ~HttpMemoryDownload() override = default;

    /**
     * Returns a reference to the buffer that is used to store the contents of the URL
     * passed in the constructor. Please observe that while the HttpDownload::hasFinished
     * method returns `false`, this buffer will be changed by a different thread and
     * access is not thread-safe. After that function returns `true`, it is safe to access
     * the buffer.
     *
     * \return A reference to the buffer used to hold the contents of the URL
     */
    const std::vector<char>& downloadedData() const;

private:
    /**
     * Stores each downloaded chunk into the stored buffer.
     */
    bool handleData(char* buffer, size_t size) override;

    /// The buffer where the downloaded chunks are accumulated
    std::vector<char> _buffer;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___HTTPREQUEST___H__
