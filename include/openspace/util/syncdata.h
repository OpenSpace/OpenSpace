/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __SYNC_DATA_H__
#define __SYNC_DATA_H__

#include <memory>
#include <mutex>

#include <ghoul/misc/assert.h>
#include <openspace/util/syncbuffer.h>


namespace openspace {


/**
* Interface for synchronizable data
*
* Used by <code>SyncEngine</code>
*/
class Syncable {
public:
    virtual ~Syncable() {};

protected:
    // Allowing SyncEngine synchronization methods and at the same time hiding them
    // from the used of implementations of the interface
    friend class SyncEngine;
    virtual void presync(bool isMaster) {};
    virtual void encode(SyncBuffer* syncBuffer) = 0;
    virtual void decode(SyncBuffer* syncBuffer) = 0;
    virtual void postsync(bool isMaster) {};
};

/**
* A double buffered implementation of the Syncable interface. 
* Users are encouraged to used this class as a default way to synchronize different 
* C++ data types using the <code>SyncEngine</code>
*
* This class aims to handle the synchronization parts and yet act like a regular  
* instance of T. Implicit casts are supported, however, when accessing member functions or
* or variables, user may have to do explicit casts. 
*
* ((T&) t).method();
*
*/
template<class T>
class SyncData : public Syncable {
public:

    SyncData() {};
    SyncData(const T& val) : data(val) {};
    SyncData(const SyncData<T>& o) : data(o.data) {
        // Should not have to be copied! 
    };

    /**
    * Allowing assignment of data as if
    */
    SyncData& operator=(const T& rhs) {
        data = rhs;
        return *this;
    }

    /**
    * Allow implicit cast to referenced T
    */
    operator T&() {
        return data;
    }

    /**
    * Allow implicit cast to const referenced T
    */
    operator const T&() const {
        return data;
    }

protected:

    virtual void encode(SyncBuffer* syncBuffer) {
        _mutex.lock();
        syncBuffer->encode(data);
        _mutex.unlock();
    }

    virtual void decode(SyncBuffer* syncBuffer) {
        _mutex.lock();
        syncBuffer->decode(doubleBufferedData);
        _mutex.unlock();
    }

    virtual void postsync(bool isMaster) {
        // apply synced update
        if (!isMaster) {
            _mutex.lock();
            data = doubleBufferedData;
            _mutex.unlock();
        }
    };


    T data;
    T doubleBufferedData;
    std::mutex _mutex;

};


} // namespace openspace

#endif //#ifndef __SYNC_DATA_H__
