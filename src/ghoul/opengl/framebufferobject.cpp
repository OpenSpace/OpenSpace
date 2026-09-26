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

#include <ghoul/opengl/framebufferobject.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr std::string_view _loggerCat = "ghoul.opengl.FramebufferObject";
} // namespace

namespace ghoul::opengl {

std::string_view FramebufferObject::errorChecking(GLenum status) {
    switch (status) {
        case GL_FRAMEBUFFER_COMPLETE:
            return "";
        case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
            return "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT";
        case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
            return "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT";
        case GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
            return "GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT";
        case GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
            return "GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT";
        case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
            return "GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER";
        case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
            return "GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER";
        case GL_FRAMEBUFFER_UNSUPPORTED:
            return "GL_FRAMEBUFFER_UNSUPPORTED";
        default:
            return "Unknown error";
    }
}

FramebufferObject::FramebufferObject() {
    generateId();
}

FramebufferObject::~FramebufferObject() {
    glDeleteFramebuffers(1, &_id);
}

void FramebufferObject::activate() const {
    glBindFramebuffer(GL_FRAMEBUFFER, _id);
}

void FramebufferObject::deactivate() {
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

bool FramebufferObject::isComplete() const {
    const GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    const std::string_view error = errorChecking(status);

    if (!error.empty()) {
        LERROR(error);
    }
    return error.empty();
}

bool FramebufferObject::isActive() const {
    return ((getActiveObject() == _id) && (_id != 0));
}

void FramebufferObject::attachTexture(Texture* texture, GLenum attachment, int mipLevel,
                                      int zSlice)
{
    switch (texture->type()) {
        case GL_TEXTURE_1D:
        case GL_TEXTURE_2D:
        case GL_TEXTURE_RECTANGLE:
            glNamedFramebufferTexture(_id, attachment, *texture, mipLevel);
            break;
        case GL_TEXTURE_3D:
            glNamedFramebufferTextureLayer(_id, attachment, *texture, mipLevel, zSlice);
            break;
        case GL_TEXTURE_2D_ARRAY:
            glNamedFramebufferTextureLayer(_id, attachment, *texture, mipLevel, zSlice);
            break;
        default:
            LERROR("Unknown texture type");
            break;
    }
    _attachedTextures[attachment] = texture;
}

void FramebufferObject::detachTexture(GLenum attachment) {
    auto iterator = _attachedTextures.find(attachment);
    if (iterator != _attachedTextures.end()) {
        _attachedTextures.erase(iterator);
    }
    else {
        LWARNING("Trying to detach unknown texture");
    }

    glNamedFramebufferTexture(_id, attachment, 0, 0);
}

void FramebufferObject::detachAll() {
    while (!_attachedTextures.empty()) {
        detachTexture(_attachedTextures.begin()->first);
    }
}

Texture* FramebufferObject::texture(GLenum attachment) {
    const auto iterator = _attachedTextures.find(attachment);
    return iterator != _attachedTextures.end() ? _attachedTextures[attachment] : nullptr;
}

GLuint FramebufferObject::getActiveObject() {
    GLint fbo = 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &fbo);
    return static_cast<GLuint>(fbo);
}

GLuint FramebufferObject::generateId() {
    glCreateFramebuffers(1, &_id);
    return _id;
}

} // namespace ghoul::opengl
