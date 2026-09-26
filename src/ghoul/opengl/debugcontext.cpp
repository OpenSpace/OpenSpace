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

#include <ghoul/opengl/debugcontext.h>

#include <utility>

namespace {
    using namespace ghoul;
    using namespace ghoul::opengl;

    void internalCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei,
                          const GLchar* message, const GLvoid* userParam)
    {
        const CallbackFunction& f = *reinterpret_cast<const CallbackFunction*>(userParam);
        f(Source(source), Type(type), Severity(severity), id, std::string(message));
    }
} // namespace

namespace ghoul::opengl {

void setDebugOutput(DebugOutput debug, SynchronousOutput synchronous) {
    if (debug) {
        glEnable(GL_DEBUG_OUTPUT);
    }
    else {
        glDisable(GL_DEBUG_OUTPUT);
    }

    if (synchronous) {
        glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    }
    else {
        glDisable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    }
}

void setDebugMessageControl(Source source, Type type, Severity severity, Enabled enabled)
{
    glDebugMessageControl(
        static_cast<GLenum>(source),
        static_cast<GLenum>(type),
        static_cast<GLenum>(severity),
        0,
        nullptr,
        enabled ? GL_TRUE : GL_FALSE
    );
}

void setDebugMessageControl(Source source, Type type,
                            const std::vector<unsigned int>& identifiers, Enabled enabled)
{
    ghoul_assert(source != Source::DontCare, "source must not be Source::Dontcare");
    ghoul_assert(type != Type::DontCare, "type must not be Type::Dontcare");

    static_assert(
        std::is_same_v<unsigned int, GLuint>,
        "We exploit that 'int' and 'GLsizei' are the same for glDebugMessageControl"
    );

    glDebugMessageControl(
        static_cast<GLenum>(source),
        static_cast<GLenum>(type),
        GL_DONT_CARE,
        static_cast<GLsizei>(identifiers.size()),
        identifiers.data(),
        enabled ? GL_TRUE : GL_FALSE
    );
}

void setDebugCallback(CallbackFunction callback) {
    // We have to store the function pointer that is passed into this function locally as
    // it might otherwise be destroyed (and still be referenced by `internalCallback`. In
    // a perfect world, we'd want to capture the function pointer, but the OpenGL callback
    // only allows pure C functions
    static CallbackFunction storage;
    storage = std::move(callback);
    glDebugMessageCallback(internalCallback, &storage);
}

} // namespace ghoul::opengl
