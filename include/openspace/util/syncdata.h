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
* Interace for synchronizable data
*
* Used by <code>SyncEngine</code> for synchronization
*/
class Syncable {
public:
    virtual ~Syncable() {};

protected:
    friend class SyncEngine;
    virtual void presync(bool isMaster) {};
    virtual void encode(SyncBuffer* syncBuffer) = 0;
    virtual void decode(SyncBuffer* syncBuffer) = 0;
    virtual void postsync(bool isMaster) {};
};


template<class T>
class SyncData : public Syncable {
public:

    SyncData() {};
    SyncData(const T& val) : data(val) {};
    SyncData(const SyncData<T>& o) : data(o.data) {
        // should not be copied!
    };

    SyncData& operator=(const T& rhs) {
        data = rhs;
        return *this;
    }

    operator T&() {
        return data;
    }

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
