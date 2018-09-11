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

#ifndef __OPENSPACE_CORE___HELPER___H__
#define __OPENSPACE_CORE___HELPER___H__

#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace::rendering::helper {

enum class Anchor {
    Center,
    NW,
    NE,
    SW,
    SE
};

void initialize();
void deinitialize();

glm::mat4 ortho(const glm::vec2& position, const glm::vec2& size,
    Anchor anchor = Anchor::NW);

void renderBox(ghoul::opengl::ProgramObject& program, GLint orthoLocation,
    GLint colorLocation, const glm::vec2& position, const glm::vec2& size,
    const glm::vec4& color, Anchor anchor = Anchor::NW);

void renderBox(const glm::vec2& position, const glm::vec2& size, const glm::vec4& color,
    Anchor anchor = Anchor::NW);

void renderBox(const glm::vec2& position, const glm::vec2& size, const glm::vec4& color,
    const ghoul::opengl::Texture& texture, Anchor anchor = Anchor::NW);

struct Shaders {
    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        UniformCache(tex, hasTexture, shouldFlipTexture, ortho, color) cache;
    } xyuvrgba;

    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        UniformCache(tex, hasTexture, shouldFlipTexture, ortho, color) cache;
    } screenfilling;
};

struct VertexObjects {
    struct {
        GLuint vao;
        GLuint vbo;
    } square;

    struct {
        GLuint vao;
    } empty;
};

namespace detail {

Shaders& gShadersConstructor();
VertexObjects& gVertexObjectsConstructor();

} // namespace detail

static Shaders& shaders = detail::gShadersConstructor();
static VertexObjects& vertexObjects = detail::gVertexObjectsConstructor();

} // namespace openspace::rendering::helper

#endif // __OPENSPACE_CORE___HELPER___H__
