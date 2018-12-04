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

#include <openspace/util/syncbuffer.h>

namespace openspace {

template<class T>
SyncData<T>::SyncData(const T& val) : _data(val) {}

template<class T>
SyncData<T>::SyncData(const SyncData<T>& o) : _data(o._data) {}

template<class T>
SyncData<T>& SyncData<T>::operator=(const T& rhs) {
    _data = rhs;
    return *this;
}

template<class T>
SyncData<T>::operator T&() {
    return _data;
}

template<class T>
SyncData<T>::operator const T&() const {
    return _data;
}

template<class T>
T& SyncData<T>::data() {
    return _data;
}

template<class T>
const T& SyncData<T>::data() const {
    return _data;
}

template<class T>
void SyncData<T>::encode(SyncBuffer* syncBuffer) {
    _mutex.lock();
    syncBuffer->encode(_data);
    _mutex.unlock();
}

template<class T>
void SyncData<T>::decode(SyncBuffer* syncBuffer) {
    _mutex.lock();
    syncBuffer->decode(_doubleBufferedData);
    _mutex.unlock();
}

template<class T>
void SyncData<T>::postSync(bool isMaster) {
    // apply synced update
    if (!isMaster) {
        _mutex.lock();
        _data = _doubleBufferedData;
        _mutex.unlock();
    }
}

} // namespace openspace
