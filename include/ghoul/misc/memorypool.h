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

#ifndef __GHOUL___MEMORYPOOL___H__
#define __GHOUL___MEMORYPOOL___H__

#include <array>
#include <cstddef>
#include <memory>

#if (defined(__linux__) && defined(__clang__))
#include <experimental/memory_resource>
namespace pmr = std::experimental::pmr;
#else // ^^^^ __linux__ && __clang__ // !(__linux__ && __clang__) vvvv
#include <memory_resource>
namespace pmr = std::pmr;
#endif // __linux__ && __clang__

#include <vector>

namespace ghoul {

/**
 * This class represents a MemoryPool with a specific size from which individual memory
 * blocks can be requested. The MemoryPool is organized into multiple separate buckets
 * with a specific size. The number of buckets in the MemoryPool will increase until the
 * MemoryPool is destroyed or the reset method is called.
 *
 * OBS: If the MemoryPool is destroyed, all memory that was returned from the alloc method
 *      is freed, but if the memory was used to create objects, their destructors are not
 *      called
 *
 * \tparam BucketSize The size of each bucket in bytes
 */
template <int BucketSize = 4096, bool InjectDebugMemory = false, bool NoDealloc = false>
class MemoryPool final : public pmr::memory_resource {
public:
    const static int _bucketSize = BucketSize;

    /**
     * Creates the MemoryBool with the specified number of buckets already created.
     *
     * \param nBuckets The number of buckets that should be created at creation time
     */
    explicit MemoryPool(int nBuckets = 1);

    /**
     * Frees the memory that was allocated during the existence of this MemoryPool.
     */
    void reset();

    /**
     * Function that will make sure the list of returned pointers is nice and clean.
     * Should be called regularly (once per frame, for example) to make sure that the list
     * doesn't degenerate.
     */
    void housekeeping();

    /**
     * Returns a pointer to an allocated object of type T. The parameters to this function
     * are passed on to the contructor of T.
     *
     * \tparam T The type of the object that is to be constructed
     * \param args The arguments to the constructor of T
     */
    template <typename T, class... Types>
    T* alloc(Types&&... args);

    void* do_allocate(size_t bytes, size_t alignment) final;
    void do_deallocate(void* p, std::size_t bytes, std::size_t alignment) final;
    bool do_is_equal(const pmr::memory_resource& other) const noexcept final;

    /**
     * Returns the number of buckets that have been allocated.
     */
    int nBuckets() const;

    /**
     * Returns the usages for each of the buckets. The number of values returned is the
     * same as returned by the \see nBuckets function.
     */
    std::vector<int> occupancies() const;

    /**
     * Returns the total occupancy for the whole MemoryPool.
     */
    int totalOccupancy() const;

private:
    struct Bucket {
        /// The number of bytes that have been used in this Bucket
        size_t usage = 0;
        /// The bucket's data storage
        std::array<std::byte, BucketSize> payload;
    };

    struct EmptyPair {
        void* ptr = nullptr;
        size_t size = 0;
    };
    std::vector<EmptyPair> _emptyList;

    /// The number of allocated buckets
    std::vector<std::unique_ptr<Bucket>> _buckets;
    /// The original desired number of buckets
    const int _originalBucketSize;
};

/**
 * This memory pool works similar to the \see TypedMemoryPool execept that instances of
 * the returned pointers can be returned to make them available again for future calls of
 * the allocate method.
 *
 * \tparam T The type for which the MemoryPool should operate
 * \tparam BucketSizeItems The number of Ts that should be stored in a single Bucket
 */
template <typename T, int BucketSizeItems = 128, bool InjectDebugMemory = false>
class ReusableTypedMemoryPool {
public:
    /**
     * Creates the MemoryBool with the specified number of buckets already created.
     *
     * \param nBuckets The number of buckets that should be created at creation time
     */
    explicit ReusableTypedMemoryPool(int nBuckets = 1);

    /**
     * Frees the memory that was allocated during the existence of this MemoryPool or the
     * last call of reset.
     */
    ~ReusableTypedMemoryPool() = default;

    /**
     * Frees the memory that was allocated during the existence of this MemoryPool or the
     * last call of reset and returns the number of buckets to the initial number of
     * buckets as requested in the constructor.
     */
    void reset();

    /**
     * Reserves a memory block that can fit \p n instances of T. Each entry in the
     * returned vector points to the memory location that is big enough to fit a single
     * instance of T.
     *
     * \param n The number of Ts that determine the size of the reserved memory block
     * \return A list of pointers that each are big enough to hold a single T. These are
     *         not guaranteed to be contiguous
     */
    std::vector<void*> allocate(int n);

    /**
     * Returns ownership of the pointer \p ptr back to the ReusableTypedMemoryPool. This
     * pointer will be returned in a future allocate call.
     *
     * \param ptr The pointer that should be returned and marked for reuse. This pointer
     *            must be a pointer that has previously been returned by the allocate
     *            method
     */
    void free(T* ptr);

private:
    struct Bucket {
        /// The data storage of this bucket
        std::array<std::byte, BucketSizeItems * sizeof(T)> payload;
        /// The number of bytes that have been used in this Bucket
        int usage = 0;
    };

    /// The list of pointers that have been returned
    std::vector<T*> _freeList;
    /// The number of allocated buckets
    std::vector<std::unique_ptr<Bucket>> _buckets;
    /// The original desired number of buckets
    int _originalNBuckets;
};

} // namespace ghoul

#include "memorypool.inl"

#endif // __GHOUL___MEMORYPOOL___H__
