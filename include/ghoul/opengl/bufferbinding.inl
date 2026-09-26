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

#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

namespace ghoul::opengl {

template <bufferbinding::Buffer T>
unsigned int BufferBinding<T>::_totalActive = 0;

template <bufferbinding::Buffer T>
unsigned int BufferBinding<T>::_maxBufferBindings = 0;

template <bufferbinding::Buffer T>
std::vector<bool> BufferBinding<T>::_busyBindings = std::vector<bool>();

template <bufferbinding::Buffer T>
bool BufferBinding<T>::_isInitialized = false;

template <bufferbinding::Buffer T>
BufferBinding<T>::BufferBinding() {
    if (!_isInitialized) {
        initialize();
    }
}

template <bufferbinding::Buffer T>
BufferBinding<T>::~BufferBinding() {
    if (assigned()) {
        _busyBindings.at(_number) = false;
        --_totalActive;
    }
}

template <bufferbinding::Buffer T>
GLint BufferBinding<T>::bindingNumber() {
    if (!_assigned) {
        assignBinding();
    }
    return _number;
}

template <bufferbinding::Buffer T>
bool BufferBinding<T>::assigned() {
    return _assigned;
}

template <bufferbinding::Buffer T>
BufferBinding<T>::operator GLint() {
    return bindingNumber();
}

template <bufferbinding::Buffer T>
void BufferBinding<T>::deinitialize() {
    for (size_t i = 0; i < _busyBindings.size(); i++) {
        _busyBindings[i] = false;
    }
    _totalActive = 0;
}

template <bufferbinding::Buffer T>
int BufferBinding<T>::numberActiveBindings() {
    return _totalActive;
}

template <bufferbinding::Buffer T>
unsigned int BufferBinding<T>::maxBufferBindings() {
    if constexpr (T == bufferbinding::Buffer::AtomicCounter) {
        return OpenGLCap.maxAtomicCounterBufferBindings();
    }
    else if constexpr (T == bufferbinding::Buffer::ShaderStorage) {
        return OpenGLCap.maxShaderStorageBufferBindings();
    }
    else {
        return OpenGLCap.maxUniformBufferBindings();
    }
}

template <bufferbinding::Buffer T>
void BufferBinding<T>::assignBinding() {
    if (_totalActive >= _maxBufferBindings) {
        return;
    }

    _assigned = true;

    for (size_t i = 0; i < _maxBufferBindings; i++) {
        if (!_busyBindings[i]) {
            _number = static_cast<GLint>(i);
            _busyBindings[i] = true;
            _totalActive++;
            break;
        }
    }
}

template <bufferbinding::Buffer T>
void BufferBinding<T>::initialize() {
    if (systemcapabilities::SystemCapabilities::isInitialized()) {
        _maxBufferBindings = maxBufferBindings();
    }
    else {
        // Reasonable default setting for OpenGL
        _maxBufferBindings = 8;
    }
    _busyBindings = std::vector<bool>(_maxBufferBindings, false);
    _isInitialized = true;
}

} // namespace ghoul::opengl
