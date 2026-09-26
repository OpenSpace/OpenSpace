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

#ifndef __GHOUL___BUFFERLOG___H__
#define __GHOUL___BUFFERLOG___H__

#include <ghoul/misc/exception.h>

#include <functional>
#include <string>

namespace ghoul::logging {

/**
 * The BufferLog stores timestamped messages into a provided custom buffer of memory. It
 * automatically reserves a part of the block for use as a header in which the version,
 * possible attributes and the amount of stored data is located. The version is always
 * located in the first byte of the buffer and determines the size and the structure of
 * the rest of the header. Each log entry stores an 8 byte timestamp followed by a `\0`
 * terminated ASCII char array containing the message. Each logging will test if there is
 * enough memory left in the buffer. For memory exhaustion management, a custom callback
 * can be specified (#setCallback) that will have to reset the buffer (#resetBuffer) or a
 * warning will be logged. The buffer can be written to disk (#writeToDisk), or access
 * directly (#buffer). Most of the methods are thread-safe and are marked as such.
 */
class BufferLog {
public:
    /**
     * This exception is thrown if a call to #log exhausted the available memory and there
     * is either no registered MemoryExhaustedCallback or the registered callback failed
     * to free enough memory.
     */
    struct MemoryExhaustionException final : public RuntimeError {
        explicit MemoryExhaustionException(int sizeTotal, int sizeRequested);

        const int totalSize;
        const int requestedSize;
    };

    /**
     * A callback of this time will be called when the logging of a message would exhaust
     * the available memory of the buffer. It is the callbacks responsibility to either
     * reset the buffer (#resetBuffer) or supply a new buffer that will be used instead
     * (#setBuffer). The passed parameters are the BufferLog that is exhausted and the
     * timestamp that will be used in the message after the callback has been resolved.
     */
    using MemoryExhaustedCallback =
        std::function<void(BufferLog&, unsigned long long int&)>;

    /**
     * Constructor that registers a MemoryExhausedCallback that will be used. The
     * constructor will take a small piece of the provided buffer to store a necessary
     * header. The size of the header is version-dependent.
     *
     * \param address The address to the buffer that will be used by this log. The
     *        ownership of the memory is **not** passed to the BufferLog by this
     * \param bufferSize The total size of the buffer. It is the callers responsibility to
     *        assure that the provided buffer is at least as big as \p totalSize
     * \param callback The MemoryExhaustedCallback that will be used when a potential log
     *        entry would exhaust the available memory in the buffer. It is the callback's
     *        responsibility to either clear the buffer (#resetBuffer) or provide a new
     *        buffer that is used instead (#setBuffer)
     *
     * \pre \p address must not be `nullptr`
     * \pre \p bufferSize must be positive
     */
    BufferLog(void* address, size_t bufferSize, MemoryExhaustedCallback callback);

    /**
     * The constructor will take a small piece of the provided buffer to store a necessary
     * header. The size of the header is version-dependent. If no MemoryExhaustedCallback
     * is registered and a subsequent call to #log would exceed the remainder of the
     * buffer, an exception will be thrown.
     *
     * \param address The address to the buffer that will be used by this log. The
     *        ownership of the memory is **not** passed to the BufferLog by this
     * \param bufferSize The total size of the buffer. It is the callers responsibility to
     *        assure that the provided buffer is at least as big as \p bufferSize
     *
     * \pre \p address must not be `nullptr`
     * \pre \p bufferSize must be positive
     */
    BufferLog(void* address, size_t bufferSize);

    /**
     * Sets the callback that will be used to handle out-of-memory situations. The
     * callback will be called when the memory requirement of the next log message would
     * exhaust the available memory in the buffer. It is the callback's responsibility to
     * either clear the used memory (#resetBuffer) or provide a replacement buffer
     * (#setBuffer) that is used instead.
     *
     * \param callback The callback that will be used to handle out-of-memory situations
     */
    void setCallback(MemoryExhaustedCallback callback);

    /**
     * Logs a \p message with a particular \p timestamp. The unit of the timestamp is
     * undefined and depends on the specific use case. The \p timestamp and the \p message
     * will be copied into the buffer. This method will acquire a lock before calling the
     * callback function (if provided). Calling this function from the callback results in
     * undefined behavior. This method is thread-safe.
     *
     * \param timestamp The timestamp of the message
     * \param message The message to store in the buffer
     *
     * \throw MemoryExhaustionException If there was not enough memory left in the buffer
     *        and there either was no MemoryExhaustCallback or the callback failed to
     *        provide new memory
     * \pre \p message must not be empty
     */
    void log(unsigned long long timestamp, const std::string& message);

    /**
     * Returns the buffer that is used by the BufferLog. If this buffer is modified by the
     * caller, especially the Header bytes, the results are undefined.
     *
     * \return The buffer that is used by the BufferLog
     */
    void* buffer();

    /**
     * Returns the total size of the buffer that was specified by the user when the
     * BufferLog was constructed or a new buffer was supplied (#setBuffer).
     *
     * \return The total size of the buffer
     */
    size_t totalSize() const;

    /**
     * Returns the number of bytes that have been used by this BufferLog, including the
     * information for the header fields. This value is guaranteed to be always less than
     * the value returned by #totalSize.
     *
     * \return The number of bytes that have been used by this BufferLog
     */
    size_t usedSize() const;

    /**
     * Provides a new buffer that will be used by this BufferLog. This method can be used
     * by the MemoryExhaustCallback to provide a new buffer while taking take of the old
     * buffer (#buffer) separately. The caller is responsible to ensure that the \p buffer
     * contains at least as many bytes as \p bufferSize. As the BufferLog does not take
     * ownership of the provided buffer, the old buffer might become unavailable after
     * this function call. This has to be taken care of by the caller or a memory leak
     * will occur. This method is thread-safe.
     *
     * \param buffer The buffer that will be used by the BufferLog from now on
     * \param bufferSize The size of the buffer that will be used by the BufferLog to
     *        store messages
     *
     * \pre \p buffer must not be `nullptr`
     * \pre \p bufferSize must be positive
     */
    void setBuffer(void* buffer, size_t bufferSize);

    /**
     * Resets the used buffer so that it can hold as many bytes as it did when the
     * BufferLog was first initialized. This method does not actually overwrite anything
     * in the buffer, but marks the buffer as available again. This method is thread-safe.
     */
    void resetBuffer();

    /**
     * This method writes the contents of the buffer to disk as a binary file. The full
     * buffer, including the header, will be written out. Only the parts of the buffer
     * that have been used will be written to disk, as opposed to the whole buffer. This
     * means that the file may contain less bytes than #totalSize. This method is
     * thread-safe.
     *
     * \param filename The path to the file which will hold the contents of the buffer.
     *        Any existing file at that location will be overwritten in the process
     */
    void writeToDisk(const std::string& filename);

protected:
    /**
     * This method will initialize the individual members of the header fields and make
     * the buffer usable.
     */
    void initializeBuffer();

    /// This block of memory will store all log messages that are added to this BufferLog
    /// it has to be as big as the value provided in `_totalSize`
    void* _buffer;
    /// The total size of the buffer used by this BufferLog
    size_t _totalSize;

    /// This callback will be called when an incoming log message would exhaust the
    /// available memory of the buffer. The callback has to either provide a new buffer
    /// which will be used henceforth (#setBuffer) or mark the current buffer as
    /// reusable (#resetBuffer)
    MemoryExhaustedCallback _callback;

    /// This variable is `true` if this BufferLog has had its callback trigged in the
    /// current callstack. It forces some methods to ignore the `atomic_lock` to ensure
    /// that no deadlock can happen
    bool _inCallbackStack = false;
};

} // namespace ghoul::logging

#endif // __GHOUL___BUFFERLOG___H__
