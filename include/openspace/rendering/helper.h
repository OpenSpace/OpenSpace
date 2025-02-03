/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

namespace openspace {
    class LightSource;
    struct RenderData;
} // namespace openspace

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
        UniformCache(tex, hasTexture, shouldFlipTexture, proj, color) cache;
    } xyuvrgba;

    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        UniformCache(tex, hasTexture, shouldFlipTexture, proj, color) cache;
    } screenfilling;
};

struct VertexObjects {
    struct {
        GLuint vao = 0;
        GLuint vbo = 0;
    } square;

    struct {
        GLuint vao = 0;
        GLuint vbo = 0;
        GLuint ibo = 0;

        int nElements = 64;
    } sphere;

    struct {
        GLuint vao = 0;
        GLuint vbo = 0;
        GLuint ibo = 0;

        int nElements = 64;
    } cylinder;

    struct {
        GLuint vao = 0;
        GLuint vbo = 0;
        GLuint ibo = 0;

        int nElements = 64;
    } cone;

    struct {
        GLuint vao = 0;
    } empty;
};

namespace detail {

Shaders& gShadersConstructor();
VertexObjects& gVertexObjectsConstructor();

} // namespace detail

static Shaders& shaders = detail::gShadersConstructor();
static VertexObjects& vertexObjects = detail::gVertexObjectsConstructor();

struct Vertex {
    GLfloat xyz[3];
    GLfloat uv[2];
    GLfloat rgba[4];
};

struct VertexXYZ {
    GLfloat xyz[3];
};

struct VertexXYZNormal {
    GLfloat xyz[3];
    GLfloat normal[3];
};

template <typename V>
struct VertexIndexListCombo {
    std::vector<V> vertices;
    std::vector<GLushort> indices;
};

VertexXYZ convertToXYZ(const Vertex& v);

std::vector<VertexXYZ> convert(std::vector<Vertex> v);

std::vector<Vertex> createRing(int nSegments, float radius,
    const glm::vec4& colors = glm::vec4(1.f));

std::vector<VertexXYZ> createRingXYZ(int nSegments, float radius);

VertexIndexListCombo<Vertex>
createSphere(int nSegments, glm::vec3 radii, const glm::vec4& colors = glm::vec4(1.f));

VertexIndexListCombo<VertexXYZNormal> createCylinder(unsigned int nSegments,
    float radius, float height);

VertexIndexListCombo<VertexXYZNormal> createCone(unsigned int nSegments, float radius,
    float height);

/**
 * Data structure that can be used for rendering using multiple light directions.
 */
struct LightSourceRenderData {
    unsigned int nLightSources = 0;

    // Buffers for uniform uploading
    std::vector<float> intensitiesBuffer;
    std::vector<glm::vec3> directionsViewSpaceBuffer;

    void updateBasedOnLightSources(const RenderData& renderData,
        const std::vector<std::unique_ptr<LightSource>>& sources);
};

} // namespace openspace::rendering::helper

#endif // __OPENSPACE_CORE___HELPER___H__
