/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___SYNCDATA___H__
#define __OPENSPACE_CORE___SYNCDATA___H__

#include <openspace/util/syncable.h>

#include <mutex>

namespace openspace {

/**
 * A double buffered implementation of the Syncable interface.
 * Users are encouraged to used this class as a default way to synchronize different
 * C++ data types using the SyncEngine.
 *
 * This class aims to handle the synchronization parts and yet act like a regular
 * instance of T. Implicit casts are supported, however, when accessing member functions
 * or variables, user may have to do explicit casts.
 *
 * ((T&) t).method();
 *
 */
template<class T>
class SyncData : public Syncable {
public:
    SyncData() = default;
    SyncData(const T& val);
    SyncData(const SyncData<T>& o);

    /**
     * Allowing assignment of data as if
     */
    SyncData& operator=(const T& rhs);

    /**
     * Allow implicit cast to referenced T
     */
    operator T&();

    /**
     * Allow implicit cast to const referenced T
     */
    operator const T&() const;

    /**
     * Explicitly access data
     */
    T& data();

    /**
    * Explicitly access const data
    */
    const T& data() const;

protected:
    virtual void encode(SyncBuffer* syncBuffer) override;
    virtual void decode(SyncBuffer* syncBuffer) override;
    virtual void postSync(bool isMaster) override;

    T _data;
    T _doubleBufferedData;
    std::mutex _mutex;
};

} // namespace openspace

#include "syncdata.inl"

#endif // __OPENSPACE_CORE___SYNCDATA___H__
