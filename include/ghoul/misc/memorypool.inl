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

#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <cstring>
#include <numeric>
#include <stdexcept>

namespace {
    constexpr int DebugByte = 0x0F;
    constexpr int AlignmentByte = 0x1F;
    constexpr int ClearByte = 0xF0;
} // namespace

namespace ghoul {

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::MemoryPool(int nBuckets)
    : _originalBucketSize(nBuckets)
{
    _buckets.reserve(nBuckets);
    for (int i = 0; i < nBuckets; i++) {
        auto b = std::make_unique<Bucket>();
        if (InjectDebugMemory) {
            std::memset(b->payload.data(), DebugByte, BucketSize);
        }
        _buckets.push_back(std::move(b));
    }

    _emptyList.reserve(10);
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
void MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::reset() {
    for (const std::unique_ptr<Bucket>& b : _buckets) {
        b->usage = 0;

        if (InjectDebugMemory) {
            std::memset(b->payload.data(), DebugByte, BucketSize);
        }
    }
    _buckets.resize(_originalBucketSize);

    _emptyList.clear();
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
void MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::housekeeping() {
    ZoneScoped;

    if (_emptyList.empty()) {
        return;
    }

    // Remove all 0-sized empty pairs that have already been reused
    _emptyList.erase(
        std::remove_if(
            _emptyList.begin(),
            _emptyList.end(),
            [](const EmptyPair& ep) { return ep.size == 0; }
        ),
        _emptyList.end()
    );

    std::sort(
        _emptyList.begin(),
        _emptyList.end(),
        [](const EmptyPair& lhs, const EmptyPair& rhs) { return lhs.size < rhs.size; }
    );
    for (size_t i = 0; i < _emptyList.size() - 1; i++) {
        EmptyPair& current = _emptyList[i];
        EmptyPair& next = _emptyList[i + 1];

        if (static_cast<char*>(current.ptr) + current.size == next.ptr) {
            // We found a subsequent pair
            current.size += next.size;
            next.size = 0;
        }
    }
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
void* MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::do_allocate(size_t bytes,
                                                                        size_t alignment)
{
    ZoneScoped;

    ghoul_assert(
        bytes <= BucketSize,
        "Cannot allocate larger memory blocks than available in a bucket"
    );

    for (EmptyPair& ep : _emptyList) {
        if (bytes <= ep.size) {
            // We found an empty pair that works
            ep.size -= bytes;
            void* p = ep.ptr;
            ep.ptr = static_cast<std::byte*>(ep.ptr) + bytes;
            return p;
        }
    }

    // Find the first bucket that has enough space left for the number of items
    auto it = std::find_if(
        _buckets.begin(),
        _buckets.end(),
        [bytes](const std::unique_ptr<Bucket>& i) {
            return i->usage + bytes <= BucketSize;
        }
    );

    // No bucket had enough space, so we have to create a new one
    if (it == _buckets.end()) {
        _buckets.push_back(std::make_unique<Bucket>());
        it = _buckets.end() - 1;
    }


    Bucket* b = it->get();
    std::array<std::byte, BucketSize>& payload = b->payload;
    std::byte* ptr = payload.data() + b->usage;
    b->usage += bytes;

    if (InjectDebugMemory) {
        std::memset(ptr, DebugByte, bytes);
    }
    // Handle unaligned memory by padding to the next alignment boundary
    const size_t align = alignment - (bytes % alignment);
    if (align != alignment) {
        // Mark the extra bytes as "used"
        b->usage += align;
        std::memset(ptr + bytes, AlignmentByte, align);
    }

    return ptr;
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
template <typename T, class... Types>
T* MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::alloc(Types&&... args) {
    void* ptr = do_allocate(sizeof(T), alignof(T));
    T* obj = new (ptr) T(std::forward<Types>(args)...);
    return obj;
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
void MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::do_deallocate(void* ptr,
                                                                         size_t bytes,
                                                                         size_t)
{
    ZoneScoped;

    if (NoDealloc) {
        return;
    }

    for (const std::unique_ptr<Bucket>& b : _buckets) {
        const std::array<std::byte, BucketSize>& bp = b->payload;
        if (ptr >= bp.data() && ptr < (bp.data() + BucketSize)) {
            // We found our bucket to which this pointer belongs
            if (InjectDebugMemory) {
                std::memset(ptr, ClearByte, bytes);
            }
            _emptyList.push_back({ ptr, bytes });
            return;
        }
    }

    throw std::runtime_error("Returned pointer must have been from this MemoryPool");
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
bool MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::do_is_equal(
                                    const pmr::memory_resource& other) const noexcept
{
    return this == &other;
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
int MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::nBuckets() const {
    return static_cast<int>(_buckets.size());
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
std::vector<int> MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::occupancies() const
{
    std::vector<int> res;
    for (const std::unique_ptr<Bucket>& b : _buckets) {
        res.push_back(static_cast<int>(b->usage));
    }
    return res;
}

template <int BucketSize, bool InjectDebugMemory, bool NoDealloc>
int MemoryPool<BucketSize, InjectDebugMemory, NoDealloc>::totalOccupancy() const {
    return std::accumulate(
        _buckets.begin(),
        _buckets.end(),
        0,
        [](int v, const std::unique_ptr<Bucket>& b) {
            return v + static_cast<int>(b->usage);
        }
    );
}

template <typename T, int BucketSizeItems, bool InjectDebugMemory>
ReusableTypedMemoryPool<T, BucketSizeItems, InjectDebugMemory>::ReusableTypedMemoryPool(
                                                                             int nBuckets)
    : _originalNBuckets(nBuckets)
{
    _buckets.reserve(nBuckets);
    for (int i = 0; i < nBuckets; i++) {
        _buckets.push_back(std::make_unique<Bucket>());
    }
}

template <typename T, int BucketSizeItems, bool InjectDebugMemory>
void ReusableTypedMemoryPool<T, BucketSizeItems, InjectDebugMemory>::reset() {
    for (Bucket* b : _buckets) {
        delete b;
    }
    _buckets.resize(_originalNBuckets);
}

template <typename T, int BucketSizeItems, bool InjectDebugMemory>
std::vector<void*>
ReusableTypedMemoryPool<T, BucketSizeItems, InjectDebugMemory>::allocate(int n)
{
    ghoul_assert(n >= 0, "Need to allocate positive size");
    // Hackish implementation to support larger allocations than number of items in a
    // bucket; probably not likely to happen
    if (n > BucketSizeItems) {
        std::vector<void*> res;
        res.reserve(n);
        while (n > BucketSizeItems) {
            std::vector<void*> r = allocate(BucketSizeItems);
            res.insert(res.end(), r.begin(), r.end());
            n -= BucketSizeItems;
        }
        std::vector<void*> r = allocate(n);
        res.insert(res.end(), r.begin(), r.end());
        return res;
    }

    // First check if there are items in the free list, if so, return those
    if (_freeList.size() >= static_cast<size_t>(n)) {
        std::vector<void*> res(n);
        size_t startIndex = _freeList.size() - n;
        for (int i = 0; i < n; i++) {
            res[i] = _freeList[startIndex + i];
        }
        _freeList.erase(_freeList.begin() + startIndex, _freeList.end());
        return res;
    }

    // Find the first bucket that has enough space left for the number of items
    auto it = std::find_if(
        _buckets.begin(),
        _buckets.end(),
        [n](const std::unique_ptr<Bucket>& i) {
            return i->usage + n * sizeof(T) <= BucketSizeItems * sizeof(T);
        }
    );

    // No bucket had enough space, so we have to create a new one
    if (it == _buckets.end()) {
        _buckets.push_back(std::make_unique<Bucket>());
        it = _buckets.end() - 1;
    }


    Bucket* b = it->get();
    void* ptr = reinterpret_cast<std::byte*>(b->payload.data()) + b->usage;
    b->usage += n * sizeof(T);

    std::vector<void*> res(n);
    for (int i = 0; i < n; i++) {
        res[i] = reinterpret_cast<std::byte*>(ptr) + (i * sizeof(T));

        if (InjectDebugMemory) {
            for (int ii = 0; ii < sizeof(T) / sizeof(std::byte); ii++) {
                std::byte* bptr = reinterpret_cast<std::byte*>(res[i]);
                std::memset(bptr + ii, DebugByte, 1);
            }
        }
    }
    return res;
}

template <typename T, int BucketSizeItems, bool InjectDebugMemory>
void ReusableTypedMemoryPool<T, BucketSizeItems, InjectDebugMemory>::free(T* ptr) {
    if (ptr) {
        _freeList.push_back(ptr);
    }
}

} // namespace ghoul
