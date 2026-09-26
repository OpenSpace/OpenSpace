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

#ifndef __GHOUL___SHAREDMEMORY___H__
#define __GHOUL___SHAREDMEMORY___H__

#include <ghoul/misc/exception.h>
#include <map>
#include <string>

namespace ghoul {

/**
 * This class is a platform-independent implementation of a shared memory architecture. It
 * provides the possibility to create (#create) and remove (#remove) globally visible
 * blocks of memory that can be used across processes on the same machine for use in
 * inter-process communication. The workflow for this is as follows: One process has to
 * #create a SharedMemory block with a specific name and will keep the ownership of that
 * name. It does not have to create a SharedMemory object itself, but it is the creating
 * process' responsibility to #remove the shared memory at the end of its lifetime; then,
 * other processes can use the constructor with the same name to get access to the shared
 * memory. The overloaded operator void* makes the SharedMemory usable just like a `void`
 * pointer in the code. The size of the memory is accessible using the #size method. Due
 * to some necessary header information, the amount of memory that is allocated will be
 * slightly larger than the passed amount. The allocated memory automatically provides
 * storage for a thread-safe locking mechanism. In the current implementation, this is
 * using an atomic bool to provide thread-safety. It is possible for a process to acquire
 * exclusive access to the shared memory by calling #acquireLock and relinquish that
 * access by #releaseLock. Please note that this is not a strong safeguard, as any process
 * not using those two methods to guard their access into the memory will not be stopped
 * from reading or writing into the memory. The #acquireLock method will not return until
 * the lock has been acquired. The #releaseLock will return immediately.
 */
class SharedMemory {
public:
    /**
     * Superclass for all exceptions that are thrown by this class.
     */
    struct SharedMemoryError : public RuntimeError {
        explicit SharedMemoryError(std::string msg);
    };

    /**
     * Exception that is thrown if a specific shared memory did not exist.
     */
    struct SharedMemoryNotFoundError final : public SharedMemoryError {
        explicit SharedMemoryNotFoundError();
    };

    /**
     * Creates a globally visible shared memory block, which is accessible by other
     * processes with the same \p name. A block can only be created once and each attempt
     * to create a block whose name is already used will fail. The amount of memory
     * allocated will be slightly bigger than \p size to accommodate necessary header
     * information. This header is completely transparent and not accessible by the user
     * of the memory block. The process calling this method effectively has ownership of
     * the memory, regardless if it maps it into its own address space or not. That means
     * that the process calling the create method has to be the one that will call #remove
     * at the end of its lifetime (if the ownership is not transferred in the meantime). A
     * failure to call that method will result in a <b>systemwide</b> memory leak. On
     * Windows, the memory mapped file that will be created is only accessible by the same
     * terminal session as the process that has created it.
     *
     * \param name The name of the SharedMemory block. This name must be unique and unused
     *        in the system. A future constructor call using the same \p name will point
     *        to the memory created in this method call
     * \param size The size (in bytes) of the shared memory block that should be created.
     *        The actual size in memory will be slightly larger due to necessary header
     *        information, which are not accessible by the user
     *
     * \throw SharedMemoryError If there was an error creating the SharedMemory block
     */
    static void create(const std::string& name, size_t size);

    /**
     * Removes a previously created shared memory block. The \p name must be a valid name
     * for an accessible shared memory block. The underlying memory will only be freed
     * when no process has an SharedMemory object pointing to that name anymore as long as
     * one process has its memory attached into its own address space, no memory will be
     * freed.
     *
     * \param name The name of the shared memory block that should be marked for removal
     *
     * \throw SharedMemoryNotFoundError If the provided \p name is not a valid
     *        SharedMemory block
     * \throw SharedMemoryError If there was an error accessing an existing SharedMemory
     *        block
     */
    static void remove(const std::string& name);

    /**
     * Tests if a memory block with the given \p name has been created previously or if
     * the name is available.
     *
     * \param name The name of the shared memory block that should be tested
     * \return `true` if a shared memory block exists with the given \p name , `false`
     *         otherwise
     *
     * \throw SharedMemoryError If there was an error retrieving the information about the
     *        SharedMemory block
     */
    static bool exists(const std::string& name);

    /**
     * Creates a SharedMemory object pointing to a previously created shared memory block
     * (#create). If \p name is a valid name for a shared memory block, the constructor
     * will attach the memory into the calling process' address space, making it available
     * through the operator `void*`. If an error occurs either getting a valid handle on
     * the memory mapped file or during mapping the memory into the address space, a
     * SharedMemoryError is thrown. It is possible for the same process to attach the same
     * shared memory block multiple times. It is undefined if two SharedMemory objects
     * with the same name will be attached to the same memory location.
     *
     * \param name The name of the shared memory block to which this process wants to be
     *        attached to
     *
     * \throw SharedMemoryError If there was an error either accessing or mapping the
     *        SharedMemory block
     */
    SharedMemory(std::string name);

    /**
     * The destructor will detach the memory from the calling processes address space and
     * free all previously acquired resources.
     */
    ~SharedMemory();

    /**
     * Returns the pointer to the first usable address of the allocated memory. The
     * transparently handled header is automatically skipped and is invisible to the user.
     *
     * \return A valid pointer into a memory block of the predefined size (#size)
     */
    void* memory() const;

    /**
     * Returns the usable size of the memory block. The value here is the same which was
     * specified in the creation of the shared memory block (#create). The actual size of
     * the memory will be slightly bigger due to header fields.
     *
     * \return The usable size of the memory block
     */
    size_t size() const;

    /**
     * This method acquires a lock for the calling process to provide exclusive access to
     * the shared memory block. While one process owns the lock, any subsequent call to
     * this method will not return until the lock has been released. Please note that
     * this does not guarantee thread-safe execution if it is misused; if one process
     * accesses the memory without surrounding the accesses with #acquireLock,
     * #releaseLock methods, that process is not prevented from changing the memory while
     * a different thread has an exclusive, active lock on that memory. This method will
     * only return as soon as a lock has been successfully acquired.
     */
    void acquireLock();

    /**
     * Releases the acquired exclusive lock on the SharedMemory. As there is only one lock
     * for the SharedMemory and no authorization is used, any process can release the lock
     * of any other process by calling this method.
     */
    void releaseLock();

    /**
     * Returns the name for the SharedMemory object that can be used to remove the data
     * that this object depends on.
     *
     * \return The name for this SharedMemory object
     */
    std::string name() const;

private:
    /// This pointer points to the location in local address space where the shared memory
    /// block has been attached to
    void* _memory = nullptr;

    /// The name for this SharedMemory block
    const std::string _name;

    // On Windows it is not possible to query the size of a memory mapped file, so it has
    // to be stored in the block of memory itself. On POSIX systems, we can store the size
    // in this object itself.

#ifdef WIN32
    /// The handle to the memory mapped file that backs this SharedMemory object. Only a
    /// virtual file is used, so there is no disk IO necessary
    void* _sharedMemoryHandle;

    /// Stores a mapping from name (as specified in the #create method) to the acquired
    /// handle. This is necessary as each new CreateFileMapping call will create new
    /// handle of type `void*`, making it impossible to acquire the handle for a specific
    /// name otherwise
    static std::map<const std::string, void*> _createdSections;
#else // ^^^^ WIN32 // !WIN32 vvvv
    /// The full allocated size of the shared memory block
    size_t _size = 0;

    /// The handle to the virtual file backing this SharedMemory object. Only a virtual
    /// file is used, so there is no disk IO necessary
    int _sharedMemoryHandle;
#endif // WIN32
};

} // namespace ghoul

#endif // __GHOUL___SHAREDMEMORY___H__
