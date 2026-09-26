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

#ifndef __GHOUL___BUFFERBINDING___H__
#define __GHOUL___BUFFERBINDING___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <vector>

namespace ghoul::opengl {

namespace bufferbinding {
    enum class Buffer {
        AtomicCounter,
        ShaderStorage,
        Uniform
    };
} // namespace bufferbinding

/**
 * This class manages buffer bindings for ATOMIC_COUNTER_BUFFER, GL_SHADER_STORAGE_BUFFER,
 * GL_TRANSFORM_FEEDBACK_BUFFER and GL_UNIFORM_BUFFER. It manages which bindings are
 * currently active and which bindings are free to use. To use a BufferBinding, it has to
 * be ProgramObject::activate%d, and then the #bindingNumber can be retrieved so that it
 * can be used in a block binding. A block binding number is assigned as soon as the first
 * call to ProgramObject::activate, ProgramObject::glEnum, or #bindingNumber is made. If
 * there are no free binding numbers left, an `std::runtime_error` will be thrown.
*/
template <bufferbinding::Buffer T>
class BufferBinding {
public:
    /**
     * The constructor will initialize the static variables when the first BufferBinding
     * is created and the non-static variables are initiated.
     */
    BufferBinding();

    /**
     * The destructor will free the used buffer binding and mark it as free again.
     */
    ~BufferBinding();

    /**
     * This method returns the binding number that was assigned to this BufferBinding. If
     * this is the first call to #bindingNumber, a free binding number will be assigned to
     * this object.
     *
     * \return The buffer binding number that was assigned to this BufferBinding
     */
    GLint bindingNumber();

    /**
     * This operator returns the binding number that was assigned to this. If this is the
     * first call to #bindingNumber, a free binding number will be assigned to this
     * object.
     *
     * \return The buffer binding number that was assigned to this BufferBinding
     */
    operator GLint();

    /**
     * Deinitializes all the used BufferBinding%s and marks them as free. The total numbe
     * of used buffer bindings after this call will be `0`.
     */
    static void deinitialize();

    /**
     * This method returns the number of bindings that have been marked as used.
     *
     * \return The number of bindings that have been marked as used
     */
    static int numberActiveBindings();

    /**
    * This method returns the maximum number of buffer bindings that is available.
    *
    * \return The number of buffer bindings in use
    */
    static unsigned int maxBufferBindings();

private:
    /**
     * This method is called the first time #bindingNumber is called. It will assign a new
     * OpenGL buffer binding to this BufferBinding and mark this new binding as used until
     * this BufferBinding is destroyed.
     */
    void assignBinding();

    /**
     * This method returns true if the buffer binding is assigned.
     */
    bool assigned();

    /**
     * Initializes the maximum number of buffer bindings using
     * #ghoul::systemcapabilities::SystemCapabilities and marks all buffer bindings as
     * unused. This method is called the first time a BufferBinding is created.
     */
    static void initialize();

    /// The number in `[0, maxBindings]` referring to this BufferBinding
    GLint _number = 0;

    /// `true` if the list of busy bindings and the maximum number of bindings have been
    /// initialized
    static bool _isInitialized;

    /// Indicates if this buffer has already been assigned
    bool _assigned = false;

    /// The total number of active texture bindings
    static unsigned int _totalActive;

    /// The maximum number of buffer bindings
    static unsigned int _maxBufferBindings;

    /// This vector stores a bool at position `i` if the buffer binding number `i` is
    /// currently in use
    static std::vector<bool> _busyBindings;
};

} // namespace ghoul::opengl

#include "bufferbinding.inl"

#endif // __GHOUL___BUFFERBINDING___H__
