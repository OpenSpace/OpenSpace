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

#include <ghoul/logging/bufferlog.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <atomic>
#include <fstream>

namespace {
    constexpr uint8_t CurrentVersion = 1;

    struct Header {
        /**
         * The version header contains in increasing unsigned integer, which specifies the
         * general layout of the buffer in this BufferLog. The size of the header and,
         * thus, the offset into the data block may depend on the version.
         */
        uint8_t version;

        /**
         * This atomic is set to `true` if some process is currently writing to the log,
         * otherwise it is `false`. It is not guaranteed that this value is usable when
         * the buffer is written to disk in its entirety.
         */
        std::atomic_flag mutex;

        /**
         * The attributes are used for user-defined behavior. Information that is
         * necessary to interpret the buffer may be put in here.
         */
        uint8_t attributes;

        /**
         * This value provides an offset to find the first byte in the buffer that has not
         * been used already. The values between `_buffer + sizeof(Header)` and
         * `_buffer + sizeof(Header) + firstEmptyByte` are the logs that have been stored
         * before.
         */
        uint32_t firstEmptyByte;
    };

    /**
     * This method returns the beginning part of the buffer that contains the header
     * information.
     */
    Header& header(void* buffer) {
        return *reinterpret_cast<Header*>(buffer);
    }

    /**
     * This method returns the buffer block that begins at the first empty byte. This
     * offset is guaranteed to be in valid memory if the values provided in the
     * constructor or the \see setBuffer function were correct.
    */
    void* firstEmptyMemory(void* buffer) {
        const Header& h = header(buffer);

        return reinterpret_cast<void*>(
            reinterpret_cast<char*>(buffer) + sizeof(Header) + h.firstEmptyByte
        );
    }
} // namespace

namespace ghoul::logging {

BufferLog::MemoryExhaustionException::MemoryExhaustionException(int sizeTotal,
                                                                int sizeRequested)
    : RuntimeError(
        std::format("Exhausted BufferLog ({} of {} byte)", sizeRequested, sizeTotal),
        "BufferLog"
    )
    , totalSize(sizeTotal)
    , requestedSize(sizeRequested)
{}

BufferLog::BufferLog(void* address, size_t bufferSize)
    : _buffer(address)
    , _totalSize(bufferSize)
{
    ghoul_assert(address, "Address must not be nullptr");
    ghoul_assert(bufferSize > 0, "Total size must be positive");

    initializeBuffer();
    Header& h = header(_buffer);
    h.mutex.clear();
}

BufferLog::BufferLog(void* address, size_t bufferSize, MemoryExhaustedCallback callback)
    : _buffer(address)
    , _totalSize(bufferSize)
    , _callback(std::move(callback))
{
    ghoul_assert(address, "Address must not be nullptr");
    ghoul_assert(bufferSize > 0, "Total size must be positive");

    initializeBuffer();
    Header& h = header(_buffer);
    h.mutex.clear();
}

void BufferLog::initializeBuffer() {
    Header& h = header(_buffer);
    h.version = CurrentVersion;
    h.attributes = 0;
    h.firstEmptyByte = 0;
}

void BufferLog::setCallback(MemoryExhaustedCallback callback) {
    _callback = std::move(callback);
}

void BufferLog::resetBuffer() {
    // Resetting the buffer does not overwrite any of the data but resets the
    // `firstEmptyByte` pointer so that following log will overwrite the buffer
    Header& h = header(_buffer);

    // If test_and_set returns 'true', someone else is in the critical section unless we
    // are currently in a callstack containing the callback function. In which case we are
    // already safe
    while (h.mutex.test_and_set() && !_inCallbackStack) {}
    // The moment we return, we own the mutex
    h.firstEmptyByte = 0;
    if (!_inCallbackStack) {
        // Only release the mutex if we are not in the callback stack
        h.mutex.clear();
    }
}

void BufferLog::log(unsigned long long timestamp, const std::string& message) {
    ghoul_assert(!message.empty(), "Message must not be empty");

    Header& h = header(_buffer);
    // This is the full size of the incoming message. +1 for the terminating \0 character
    const size_t fullSize = sizeof(unsigned long long) + message.length() + 1;

    // If test_and_set returns 'true', someone else is in the critical section
    while (h.mutex.test_and_set()) {}
    // The moment we return, we own the mutex

    const size_t requestedSize = h.firstEmptyByte + sizeof(Header) + fullSize;

    // If this message would exceed the available memory...
    if (requestedSize > _totalSize) {
        // ...and we have a valid callback function
        if (_callback) {
            // Delegate the cleaning up to the callback
            _inCallbackStack = true;
            _callback(*this, timestamp);
            _inCallbackStack = false;
            if (h.firstEmptyByte + fullSize > _totalSize) {
                // The callback failed to clear the memory
                h.mutex.clear();
                throw MemoryExhaustionException(
                    static_cast<int>(_totalSize),
                    static_cast<int>(requestedSize)
                );
            }
        }
        else {
            // We have to fail if there is no callback
            h.mutex.clear();
            throw MemoryExhaustionException(
                static_cast<int>(_totalSize),
                static_cast<int>(requestedSize)
            );
        }
    }
    // Copy the values of the timestamp
    std::memcpy(
        firstEmptyMemory(_buffer),
        reinterpret_cast<void*>(&timestamp),
        sizeof(unsigned long long)
    );
    // Advance the empty pointer
    h.firstEmptyByte += sizeof(unsigned long long);

    // Copy the message into the buffer, strcpy will copy the \0 terminator character, too
    char* destination = reinterpret_cast<char*>(firstEmptyMemory(_buffer));
    std::memcpy(destination, message.data(), message.length() + 1);
    // Advance the empty pointer; +1 for the \0 terminator
    h.firstEmptyByte += uint32_t(message.length() + 1);

    h.mutex.clear();
}

void* BufferLog::buffer() {
    return _buffer;
}

size_t BufferLog::totalSize() const {
    return _totalSize;
}

size_t BufferLog::usedSize() const {
    const Header& h = header(_buffer);
    return h.firstEmptyByte + sizeof(Header);
}

void BufferLog::setBuffer(void* buffer, size_t bufferSize) {
    ghoul_assert(buffer, "Buffer must not be nullptr");
    ghoul_assert(bufferSize > 0, "Total size must be positive");

    Header& h = header(_buffer);
    // If test_and_set returns 'true', someone else is in the critical section unless we
    // are currently in a callstack containing the callback function. In which case we are
    // already safe
    while (h.mutex.test_and_set() && !_inCallbackStack) {}
    // The moment we return, we own the mutex

    _buffer = buffer;
    _totalSize = bufferSize;
    initializeBuffer();

    if (!_inCallbackStack) {
        // Only release the mutex if we are not in the callback stack
        h.mutex.clear();
    }
}

void BufferLog::writeToDisk(const std::string& filename) {
    std::ofstream file;
    file.exceptions(std::ofstream::failbit | std::ofstream::badbit);
    file.open(filename);
    Header& h = header(_buffer);
    // If test_and_set returns 'true', someone else is in the critical section unless we
    // are currently in a callstack containing the callback function. In which case we are
    // already safe
    while (h.mutex.test_and_set() && !_inCallbackStack) {}
    // The moment we return, we own the mutex
    file.write(reinterpret_cast<char*>(_buffer), usedSize());
    if (!_inCallbackStack) {
        // Only release the mutex if we are not in the callback stack
        h.mutex.clear();
    }
}

} // namespace ghoul::logging
