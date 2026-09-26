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

#ifndef __GHOUL___FRAMEBUFFEROBJECT___H__
#define __GHOUL___FRAMEBUFFEROBJECT___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <map>
#include <string_view>

namespace ghoul::opengl {

class Texture;

class FramebufferObject {
public:
    static std::string_view errorChecking(GLenum status);

    FramebufferObject();
    ~FramebufferObject();

    void activate() const;
    static void deactivate();

    bool isComplete() const;
    bool isActive() const;

    void attachTexture(Texture* texture, GLenum attachment = GL_COLOR_ATTACHMENT0,
        int mipLevel = 0, int zSlice = 0);

    void detachTexture(GLenum attachment);
    void detachAll();

    Texture* texture(GLenum attachment = GL_COLOR_ATTACHMENT0);
    static GLuint getActiveObject();

private:
    GLuint generateId();

    GLuint _id = 0;
    std::map<GLenum, Texture*> _attachedTextures;
};

} // namespace ghoul::opengl

#endif // __GHOUL___FRAMEBUFFEROBJECT___H__
