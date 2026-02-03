/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/rendering/helper.h>

#include <openspace/scene/lightsource.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/closest_point.hpp>
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string_view>

namespace {

struct VertexXYUVRGBA {
    std::array<GLfloat, 2> xy;
    std::array<GLfloat, 2> uv;
    std::array<GLfloat, 4> rgba;
};

bool isInitialized = false;

std::filesystem::path xyuvrgbaVertexFile;
std::filesystem::path xyuvrgbaFragmentFile;

std::filesystem::path screenFillingVertexFile;
std::filesystem::path screenFillingFragmentFile;

constexpr std::string_view XyuvrgbaVertexCode = R"(
#version __CONTEXT__

layout(location = 0) in vec2 in_position;
layout(location = 1) in vec2 in_uv;
layout(location = 2) in vec4 in_color;

out float depth;
out vec2 out_position;
out vec2 out_uv;
out vec4 out_color;

uniform mat4 proj;

void main() {
    out_position = in_position;
    out_uv = in_uv;
    out_color = in_color;
    vec4 p = proj * vec4(in_position, 0.0, 1.0);
    gl_Position = p;
    depth = p.w;
}

)";

constexpr std::string_view ScreenFillingQuadVertexCode = R"(
#version __CONTEXT__

vec2 positions[6] = vec2[](
    vec2(-1.0, -1.0), vec2( 1.0, -1.0), vec2( 1.0,  1.0),
    vec2(-1.0, -1.0), vec2( 1.0,  1.0), vec2(-1.0,  1.0)
);

out vec2 out_uv;
out vec4 out_color;

void main() {
    gl_Position = vec4(positions[gl_VertexID], 0.0, 1.0);
    out_uv = (positions[gl_VertexID] + 1.0) / 2.0;
    out_color = vec4(1.0);
}

)";

constexpr std::string_view XyuvrgbaFragmentCode = R"(
#version __CONTEXT__

#include "fragment.glsl"

uniform bool hasTexture = false;
uniform bvec2 shouldFlipTexture = bvec2(false, false);
uniform sampler2D tex;
uniform vec4 color = vec4(1.0, 1.0, 1.0, 1.0);

in float depth;
in vec2 out_uv;
in vec4 out_color;

out vec4 FragColor;

void main() {
    if (hasTexture) {
        vec2 uv = out_uv;
        if (shouldFlipTexture.x) {
            uv.x = 1.0 - uv.x;
        }
        if (shouldFlipTexture.y) {
            uv.y = 1.0 - uv.y;
        }
        FragColor = out_color * color * texture(tex, uv);
    }
    else {
        FragColor = out_color * color;
    }
}
)";

} // namespace

namespace openspace::rendering::helper {

namespace detail {

Shaders& gShadersConstructor() {
    static Shaders g;
    return g;
}

VertexObjects& gVertexObjectsConstructor() {
    static VertexObjects g;
    return g;
}

} // namespace detail

void initialize() {
    ZoneScoped;
    TracyGpuZone("helper::initialize");

    ghoul_assert(!isInitialized, "Rendering Helper initialized twice");

    //
    // XYUVRGBA shader
    //
    xyuvrgbaVertexFile = absPath("${TEMPORARY}/xyuvrgba.vert");
    {
        std::fstream vertexFile;
        vertexFile.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        vertexFile.open(xyuvrgbaVertexFile, std::fstream::out);
        vertexFile << XyuvrgbaVertexCode;
    }

    xyuvrgbaFragmentFile = absPath("${TEMPORARY}/xyuvrgba.frag");
    {
        std::fstream fragmentFile;
        fragmentFile.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        fragmentFile.open(xyuvrgbaFragmentFile, std::fstream::out);
        fragmentFile << XyuvrgbaFragmentCode;
    }
    shaders.xyuvrgba.program = ghoul::opengl::ProgramObject::Build(
        "xyuvrgba",
        xyuvrgbaVertexFile,
        xyuvrgbaFragmentFile
    );
    ghoul::opengl::updateUniformLocations(
        *shaders.xyuvrgba.program,
        shaders.xyuvrgba.cache
    );

    //
    // Screenfilling shader
    //
    screenFillingVertexFile = absPath("${TEMPORARY}/screenfilling.vert");
    {
        std::fstream vertexFile;
        vertexFile.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        vertexFile.open(screenFillingVertexFile, std::fstream::out);
        vertexFile << ScreenFillingQuadVertexCode;
    }

    screenFillingFragmentFile = absPath("${TEMPORARY}/screenfilling.frag");
    {
        std::fstream fragmentFile;
        fragmentFile.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        fragmentFile.open(screenFillingFragmentFile, std::fstream::out);
        fragmentFile << XyuvrgbaFragmentCode;
    }

    shaders.screenfilling.program = ghoul::opengl::ProgramObject::Build(
        "screenfilling",
        xyuvrgbaVertexFile,
        xyuvrgbaFragmentFile
    );
    ghoul::opengl::updateUniformLocations(
        *shaders.screenfilling.program,
        shaders.screenfilling.cache
    );


    //
    // Square vertex objects
    //
    glCreateBuffers(1, &vertexObjects.square.vbo);
    constexpr std::array<VertexXYUVRGBA, 6> Vtx = {
        // X     Y    U    V    R    G    B    A
        VertexXYUVRGBA{ { -1.f, -1.f }, { 0.f, 0.f }, { 1.f, 1.f, 1.f, 1.f } },
        VertexXYUVRGBA{ { -1.f,  1.f }, { 0.f, 1.f }, { 1.f, 1.f, 1.f, 1.f } },
        VertexXYUVRGBA{ {  1.f,  1.f }, { 1.f, 1.f }, { 1.f, 1.f, 1.f, 1.f } },

        VertexXYUVRGBA{ { -1.f, -1.f }, { 0.f, 0.f }, { 1.f, 1.f, 1.f, 1.f } },
        VertexXYUVRGBA{ {  1.f,  1.f }, { 1.f, 1.f }, { 1.f, 1.f, 1.f, 1.f } },
        VertexXYUVRGBA{ {  1.f, -1.f }, { 1.f, 0.f }, { 1.f, 1.f, 1.f, 1.f } }
    };
    glNamedBufferStorage(
        vertexObjects.square.vbo,
        6 * sizeof(VertexXYUVRGBA),
        Vtx.data(),
        GL_NONE_BIT
    );

    glCreateVertexArrays(1, &vertexObjects.square.vao);
    glVertexArrayVertexBuffer(
        vertexObjects.square.vao,
        0,
        vertexObjects.square.vbo,
        0,
        sizeof(VertexXYUVRGBA)
    );

    glEnableVertexArrayAttrib(vertexObjects.square.vao, 0);
    glVertexArrayAttribFormat(vertexObjects.square.vao, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vertexObjects.square.vao, 0, 0);

    glEnableVertexArrayAttrib(vertexObjects.square.vao, 1);
    glVertexArrayAttribFormat(
        vertexObjects.square.vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYUVRGBA, uv)
    );
    glVertexArrayAttribBinding(vertexObjects.square.vao, 1, 0);

    glEnableVertexArrayAttrib(vertexObjects.square.vao, 2);
    glVertexArrayAttribFormat(
        vertexObjects.square.vao,
        2,
        4,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYUVRGBA, rgba)
    );
    glVertexArrayAttribBinding(vertexObjects.square.vao, 2, 0);


    //
    // Sphere vertex array object
    //
    VertexIndexListCombo<Vertex> sphereData = createSphere(
        64, glm::vec3(1.f, 1.f, 1.f), glm::vec4(1.f, 1.f, 1.f, 1.f)
    );
    glCreateBuffers(1, &vertexObjects.sphere.vbo);
    glNamedBufferStorage(
        vertexObjects.sphere.vbo,
        sphereData.vertices.size() * sizeof(Vertex),
        sphereData.vertices.data(),
        GL_NONE_BIT
    );
    glCreateBuffers(1, &vertexObjects.sphere.ibo);
    glNamedBufferStorage(
        vertexObjects.sphere.ibo,
        sphereData.indices.size() * sizeof(GLushort),
        sphereData.indices.data(),
        GL_NONE_BIT
    );

    glCreateVertexArrays(1, &vertexObjects.sphere.vao);
    glVertexArrayVertexBuffer(
        vertexObjects.sphere.vao,
        0,
        vertexObjects.sphere.vbo,
        0,
        sizeof(Vertex)
    );
    glVertexArrayElementBuffer(vertexObjects.sphere.vao, vertexObjects.sphere.ibo);

    glEnableVertexArrayAttrib(vertexObjects.sphere.vao, 0);
    glVertexArrayAttribFormat(vertexObjects.sphere.vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vertexObjects.sphere.vao, 0, 0);

    glEnableVertexArrayAttrib(vertexObjects.sphere.vao, 1);
    glVertexArrayAttribFormat(
        vertexObjects.sphere.vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(Vertex, uv)
    );
    glVertexArrayAttribBinding(vertexObjects.sphere.vao, 1, 0);

    glEnableVertexArrayAttrib(vertexObjects.sphere.vao, 2);
    glVertexArrayAttribFormat(
        vertexObjects.sphere.vao,
        2,
        4,
        GL_FLOAT,
        GL_FALSE,
        offsetof(Vertex, rgba)
    );
    glVertexArrayAttribBinding(vertexObjects.sphere.vao, 2, 0);

    vertexObjects.sphere.nElements = static_cast<int>(sphereData.indices.size());


    //
    // Cylinder vertex array object
    //
    VertexIndexListCombo<VertexXYZNormal> cylinderData = createCylinder(64, 1.f, 1.f);
    glCreateBuffers(1, &vertexObjects.cylinder.vbo);
    glNamedBufferStorage(
        vertexObjects.cylinder.vbo,
        cylinderData.vertices.size() * sizeof(VertexXYZNormal),
        cylinderData.vertices.data(),
        GL_NONE_BIT
    );
    glCreateBuffers(1, &vertexObjects.cylinder.ibo);
    glNamedBufferStorage(
        vertexObjects.cylinder.ibo,
        cylinderData.indices.size() * sizeof(GLushort),
        cylinderData.indices.data(),
        GL_NONE_BIT
    );

    glCreateVertexArrays(1, &vertexObjects.cylinder.vao);
    glVertexArrayVertexBuffer(
        vertexObjects.cylinder.vao,
        0,
        vertexObjects.cylinder.vbo,
        0,
        sizeof(VertexXYZNormal)
    );
    glVertexArrayElementBuffer(vertexObjects.cylinder.vao, vertexObjects.cylinder.ibo);

    glEnableVertexArrayAttrib(vertexObjects.cylinder.vao, 0);
    glVertexArrayAttribFormat(vertexObjects.cylinder.vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vertexObjects.cylinder.vao, 0, 0);

    glEnableVertexArrayAttrib(vertexObjects.cylinder.vao, 1);
    glVertexArrayAttribFormat(
        vertexObjects.cylinder.vao,
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYZNormal, normal)
    );
    glVertexArrayAttribBinding(vertexObjects.cylinder.vao, 1, 0);
    vertexObjects.cylinder.nElements = static_cast<int>(cylinderData.indices.size());


    //
    // Cone vertex array object
    //
    VertexIndexListCombo<VertexXYZNormal> coneData = createCone(64, 1.f, 1.f);
    glCreateBuffers(1, &vertexObjects.cone.vbo);
    glNamedBufferStorage(
        vertexObjects.cone.vbo,
        coneData.vertices.size() * sizeof(VertexXYZNormal),
        coneData.vertices.data(),
        GL_NONE_BIT
    );
    glCreateBuffers(1, &vertexObjects.cone.ibo);
    glNamedBufferStorage(
        vertexObjects.cone.ibo,
        coneData.indices.size() * sizeof(GLushort),
        coneData.indices.data(),
        GL_NONE_BIT
    );

    glCreateVertexArrays(1, &vertexObjects.cone.vao);

    glEnableVertexArrayAttrib(vertexObjects.cone.vao, 0);
    glVertexArrayAttribFormat(vertexObjects.cone.vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vertexObjects.cone.vao, 0, 0);

    glEnableVertexArrayAttrib(vertexObjects.cone.vao, 1);
    glVertexArrayAttribFormat(
        vertexObjects.cone.vao,
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYZNormal, normal)
    );
    glVertexArrayAttribBinding(vertexObjects.cone.vao, 1, 0);
    vertexObjects.cone.nElements = static_cast<int>(coneData.indices.size());


    //
    // Line vertex array object
    //
    glCreateBuffers(1, &vertexObjects.line.vbo);
    glNamedBufferStorage(
        vertexObjects.line.vbo,
        2 * sizeof(VertexXYUVRGBA),
        nullptr,
        GL_DYNAMIC_STORAGE_BIT
    );

    glCreateVertexArrays(1, &vertexObjects.line.vao);
    glVertexArrayVertexBuffer(
        vertexObjects.line.vao,
        0,
        vertexObjects.line.vbo,
        0,
        sizeof(VertexXYUVRGBA)
    );

    glEnableVertexArrayAttrib(vertexObjects.line.vao, 0);
    glVertexArrayAttribFormat(vertexObjects.line.vao, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vertexObjects.line.vao, 0, 0);

    glEnableVertexArrayAttrib(vertexObjects.line.vao, 1);
    glVertexArrayAttribFormat(
        vertexObjects.line.vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYUVRGBA, uv)
    );
    glVertexArrayAttribBinding(vertexObjects.line.vao, 1, 0);

    glEnableVertexArrayAttrib(vertexObjects.line.vao, 2);
    glVertexArrayAttribFormat(
        vertexObjects.line.vao,
        2,
        4,
        GL_FLOAT,
        GL_FALSE,
        offsetof(VertexXYUVRGBA, rgba)
    );
    glVertexArrayAttribBinding(vertexObjects.line.vao, 2, 0);


    //
    // Empty vertex array objects
    //
    glCreateVertexArrays(1, &vertexObjects.empty.vao);

    isInitialized = true;
}

void deinitialize() {
    ghoul_assert(isInitialized, "Rendering Helper not initialized");

    if (!xyuvrgbaVertexFile.empty()) {
        std::filesystem::remove(xyuvrgbaVertexFile);
    }
    if (!xyuvrgbaFragmentFile.empty()) {
        std::filesystem::remove(xyuvrgbaFragmentFile);
    }
    shaders.xyuvrgba.program = nullptr;

    if (!screenFillingVertexFile.empty()) {
        std::filesystem::remove(screenFillingVertexFile);
    }
    if (!screenFillingFragmentFile.empty()) {
        std::filesystem::remove(screenFillingVertexFile);
    }
    shaders.screenfilling.program = nullptr;


    glDeleteVertexArrays(1, &vertexObjects.square.vao);
    glDeleteBuffers(1, &vertexObjects.square.vbo);

    glDeleteVertexArrays(1, &vertexObjects.sphere.vao);
    glDeleteBuffers(1, &vertexObjects.sphere.vbo);
    glDeleteBuffers(1, &vertexObjects.sphere.ibo);

    glDeleteVertexArrays(1, &vertexObjects.cylinder.vao);
    glDeleteBuffers(1, &vertexObjects.cylinder.vbo);
    glDeleteBuffers(1, &vertexObjects.cylinder.ibo);

    glDeleteVertexArrays(1, &vertexObjects.cone.vao);
    glDeleteBuffers(1, &vertexObjects.cone.vbo);
    glDeleteBuffers(1, &vertexObjects.cone.ibo);

    glDeleteVertexArrays(1, &vertexObjects.empty.vao);

    isInitialized = false;
}

glm::mat4 ortho(const glm::vec2& position, const glm::vec2& size, Anchor anchor) {
    const float xSize = size.x;
    const float ySize = size.y;
    float xPos = (position.x - 0.5f) * 2.f;
    float yPos = (1.f - position.y - 0.5f) * 2.f;

    switch (anchor) {
        case Anchor::Center:
            break;
        case Anchor::NW:
            xPos += xSize;
            yPos -= ySize;
            break;
        case Anchor::NE:
            xPos -= xSize;
            yPos -= ySize;
            break;
        case Anchor::SW:
            xPos += xSize;
            yPos += ySize;
            break;
        case Anchor::SE:
            xPos -= xSize;
            yPos += ySize;
            break;
    }

    return glm::mat4(
        xSize, 0.f, 0.f, 0.f,
        0.f, ySize, 0.f, 0.f,
        0.f, 0.f, 1.f, 0.f,
        xPos, yPos, 0.f, 1.f
    );
}


void renderBox(ghoul::opengl::ProgramObject& program, GLint orthoLocation,
               GLint colorLocation, const glm::vec2& position, const glm::vec2& size,
               const glm::vec4& color, Anchor anchor)
{
    program.setUniform(orthoLocation, ortho(position, size, anchor));
    program.setUniform(colorLocation, color);

    glBindVertexArray(vertexObjects.square.vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);
}

void renderBox(const glm::vec2& position, const glm::vec2& size, const glm::vec4& color,
               Anchor anchor)
{
    auto& shdr = shaders.xyuvrgba;
    shdr.program->activate();
    shdr.program->setUniform(shdr.cache.hasTexture, 0);
    renderBox(
        *shdr.program,
        shdr.cache.proj,
        shdr.cache.color,
        position, size,
        color,
        anchor
    );
    shdr.program->deactivate();
}

void renderBox(const glm::vec2& position, const glm::vec2& size, const glm::vec4& color,
               const ghoul::opengl::Texture& texture, Anchor anchor)
{
    auto& shdr = shaders.xyuvrgba;
    shdr.program->activate();
    shdr.program->setUniform(shdr.cache.hasTexture, 1);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    texture.bind();
    shdr.program->setUniform(shdr.cache.tex, unit);
    renderBox(
        *shdr.program,
        shdr.cache.proj,
        shdr.cache.color,
        position,
        size,
        color,
        anchor
    );
    shdr.program->deactivate();
}

void renderLine(const glm::vec2& startPosition, const glm::vec2& endPosition,
                const glm::vec2& size, const glm::vec4& startColor,
                const glm::vec4& endColor)
{
    auto& shdr = shaders.xyuvrgba;
    shdr.program->activate();
    shdr.program->setUniform(shdr.cache.proj, ortho(glm::vec2(0.f), glm::vec2(1.f)));
    shdr.program->setUniform(shdr.cache.hasTexture, 0);


    // Move the start and end position from a [0,res] range to a [-1, 1] range
    const glm::vec2 start = (startPosition / size) * 2.f - glm::vec2(1.f);
    const glm::vec2 end = (endPosition / size) * 2.f - glm::vec2(1.f);
    std::array<VertexXYUVRGBA, 2> vertexData = {
        VertexXYUVRGBA{
            { start.x, -start.y },
            { 0.f, 0.f },
            { startColor.r, startColor.g, startColor.b, startColor.a }
        },
        VertexXYUVRGBA{
            { end.x, -end.y },
            { 0.f, 0.f },
            { endColor.r, endColor.g, endColor.b, endColor.a }
        }
    };
    glNamedBufferSubData(
        vertexObjects.line.vbo,
        0,
        2 * sizeof(VertexXYUVRGBA),
        vertexData.data()
    );

    glBindVertexArray(vertexObjects.line.vao);
    glEnable(GL_LINE_SMOOTH);
    glLineWidth(6.f);
    glDrawArrays(GL_LINES, 0, 2);
    glPointSize(6.f);
    glDrawArrays(GL_POINTS, 0, 2);
    glBindVertexArray(0);
}

VertexXYZ convertToXYZ(const Vertex& v) {
    return VertexXYZ{ v.xyz[0], v.xyz[1], v.xyz[2] };
}

std::vector<VertexXYZ> convert(std::vector<Vertex> v) {
    std::vector<VertexXYZ> result(v.size());
    std::transform(v.begin(), v.end(), result.begin(), convertToXYZ);
    return result;
}

static Vertex computeCircleVertex(int i, int nSegments, float radius,
                           const glm::vec4& color = glm::vec4(1.f))
{
    const float fsegments = static_cast<float>(nSegments);

    const float fi = static_cast<float>(i);

    const float theta = fi * glm::two_pi<float>() / fsegments;  // 0 -> 2*PI

    const float x = radius * std::cos(theta);
    const float y = radius * std::sin(theta);
    const float z = 0.f;

    const float u = std::cos(theta);
    const float v = std::sin(theta);

    return { { x, y, z }, { u, v }, { color.r, color.g, color.b, color.a } };
}

std::vector<Vertex> createRing(int nSegments, float radius, const glm::vec4& colors) {
    const int nVertices = nSegments + 1;
    std::vector<Vertex> vertices(nVertices);

    for (int i = 0; i <= nSegments; i++) {
        vertices[i] = computeCircleVertex(i, nSegments, radius, colors);
    }
    return vertices;
}

std::vector<VertexXYZ> createRingXYZ(int nSegments, float radius) {
    const int nVertices = nSegments + 1;
    std::vector<VertexXYZ> vertices(nVertices);

    for (int i = 0; i <= nSegments; i++) {
        const Vertex fullVertex = computeCircleVertex(i, nSegments, radius);
        vertices[i] = { fullVertex.xyz[0], fullVertex.xyz[1], fullVertex.xyz[2] };
    }
    return vertices;
}

VertexIndexListCombo<Vertex> createSphere(int nSegments, glm::vec3 radii,
                                          const glm::vec4& colors)
{
    std::vector<Vertex> vertices;
    vertices.reserve(nSegments * nSegments);
    for (int i = 0; i <= nSegments; i++) {
        for (int j = 0; j <= nSegments; j++) {
            const float fi = static_cast<float>(i);
            const float fj = static_cast<float>(j);
            // inclination angle (north to south)
            // 0 -> PI
            // azimuth angle (east to west)
            const float theta = fi * glm::pi<float>() / nSegments;

            // 0 -> 2*PI
            const float phi = fj * glm::pi<float>() * 2.f / nSegments;

            const float x = radii[0] * std::sin(theta) * std::cos(phi);
            const float y = radii[1] * std::sin(theta) * std::sin(phi);
            // Z points towards pole (theta = 0)
            const float z = radii[2] * std::cos(theta);

            Vertex v;
            v.xyz[0] = x;
            v.xyz[1] = y;
            v.xyz[2] = z;

            const float t1 = fj / nSegments;
            const float t2 = 1.f - (fi / nSegments);

            v.uv[0] = t1;
            v.uv[1] = t2;

            v.rgba[0] = colors.r;
            v.rgba[1] = colors.g;
            v.rgba[2] = colors.b;
            v.rgba[3] = colors.a;

            vertices.push_back(v);
        }
    }

    std::vector<GLushort> indices;
    indices.reserve(vertices.size() * 3);
    for (int i = 1; i <= nSegments; i++) {
        for (int j = 0; j < nSegments; j++) {
            const int t = nSegments + 1;
            indices.push_back(static_cast<GLushort>(t * (i - 1) + j + 0));
            indices.push_back(static_cast<GLushort>(t * (i + 0) + j + 0));
            indices.push_back(static_cast<GLushort>(t * (i + 0) + j + 1));
            indices.push_back(static_cast<GLushort>(t * (i - 1) + j + 0));
            indices.push_back(static_cast<GLushort>(t * (i + 0) + j + 1));
            indices.push_back(static_cast<GLushort>(t * (i - 1) + j + 1));
        }
    }

    return { vertices, indices };
}

static VertexIndexListCombo<VertexXYZNormal> createConicalCylinder(unsigned int nSegments,
                                                                   float bottomRadius,
                                                                   float topRadius,
                                                                   float height)
{
    // Create a ring for the top and bottom vertices (XY plane)
    std::vector<VertexXYZ> bottomVertices = createRingXYZ(nSegments, bottomRadius);
    std::vector<VertexXYZ> topVertices = createRingXYZ(nSegments, topRadius);

    // Build the 4 rings of vertices (with different normals), that will make up the
    // shape for the cylinder
    std::vector<VertexXYZNormal> vertices;
    vertices.reserve(4 * bottomVertices.size() + 2);

    // Center bottom vertex
    vertices.push_back({
        .xyz = { 0.f, 0.f, 0.f },
        .normal = { 0.f, 0.f, -1.f }
    });

    std::vector<VertexXYZNormal> verts0;
    verts0.reserve(bottomVertices.size());
    std::vector<VertexXYZNormal> verts1;
    verts1.reserve(bottomVertices.size());
    std::vector<VertexXYZNormal> verts2;
    verts2.reserve(bottomVertices.size());
    std::vector<VertexXYZNormal> verts3;
    verts3.reserve(bottomVertices.size());

    for (size_t i = 0; i < bottomVertices.size(); i++) {
        const VertexXYZ& vBot = bottomVertices[i];
        VertexXYZ& vTop = topVertices[i];
        vTop.xyz[2] += height;

        glm::vec3 sideNormal;
        if (std::abs(bottomRadius - topRadius) < std::numeric_limits<float>::epsilon()) {
            sideNormal = glm::normalize(
                glm::vec3(vBot.xyz[0], vBot.xyz[1], vBot.xyz[2])
            );
        }
        else {
            const glm::vec3 p = glm::closestPointOnLine(
                glm::vec3(0.f),
                glm::vec3(vBot.xyz[0], vBot.xyz[1], vBot.xyz[2]),
                glm::vec3(vTop.xyz[0], vTop.xyz[1], vTop.xyz[2])
            );
            sideNormal = glm::normalize(p);
        }

        // Ring 0 - vertices of bottom circle, with normals pointing down
        verts0.push_back({
            .xyz = { vBot.xyz[0], vBot.xyz[1], vBot.xyz[2] },
            .normal = { 0.f, 0.f, -1.f }
        });

        // Ring 1 - bottom vertices of cylider sides with normals pointing outwards
        verts1.push_back({
            .xyz = { vBot.xyz[0], vBot.xyz[1], vBot.xyz[2] },
            .normal = { sideNormal.x, sideNormal.y, sideNormal.z }
        });

        // Ring 2 - top vertices of cylinder side, normals pointing outwards
        // Note that only difference between top and bottom is the height added to Z
        verts2.push_back({
            .xyz = { vTop.xyz[0], vTop.xyz[1], vTop.xyz[2] },
            .normal = { sideNormal.x, sideNormal.y, sideNormal.z }
        });

        // Ring 3 - vertices of top circle, normals pointing up
        verts3.push_back({
            .xyz = { vTop.xyz[0], vTop.xyz[1], vTop.xyz[2] },
            .normal = { 0.f, 0.f, 1.f }
        });
    }

    vertices.insert(vertices.end(), verts0.begin(), verts0.end());
    vertices.insert(vertices.end(), verts1.begin(), verts1.end());
    vertices.insert(vertices.end(), verts2.begin(), verts2.end());
    vertices.insert(vertices.end(), verts3.begin(), verts3.end());

    // Center top vertex
    vertices.push_back({
        .xyz = { 0.f, 0.f, height },
        .normal = { 0.f, 0.f, 1.f }
    });

    // Contruct the index list, based on the above vertex rings
    std::vector<GLushort> indexArray;
    indexArray.reserve(4 * 3 * nSegments);

    auto ringVerticeIndex = [&nSegments](unsigned int ringIndex, unsigned int i) {
        return static_cast<GLushort>(1 + ringIndex * (nSegments + 1) + i);
    };

    const GLushort botCenterIndex = 0;
    const GLushort topCenterIndex = static_cast<GLushort>(vertices.size()) - 1;

    for (unsigned int i = 0; i < nSegments; i++) {
        const bool isLast = (i == nSegments - 1);
        GLushort v0 = 0;
        GLushort v1 = 0;
        GLushort v2 = 0;
        GLushort v3 = 0;

        // Bottom triangle
        v0 = ringVerticeIndex(0, i);
        v1 = ringVerticeIndex(0, isLast ? 0 : i + 1);
        indexArray.push_back(botCenterIndex);
        indexArray.push_back(v1);
        indexArray.push_back(v0);

        // Side of cylinder

        // Bottom ring
        v0 = ringVerticeIndex(1, i);
        v1 = ringVerticeIndex(1, isLast ? 0 : i + 1);
        // Top ring
        v2 = ringVerticeIndex(2, i);
        v3 = ringVerticeIndex(2, isLast ? 0 : i + 1);
        indexArray.push_back(v0);
        indexArray.push_back(v1);
        indexArray.push_back(v2);

        indexArray.push_back(v1);
        indexArray.push_back(v3);
        indexArray.push_back(v2);

        // Top triangle
        v0 = ringVerticeIndex(3, i);
        v1 = ringVerticeIndex(3, isLast ? 0 : i + 1);
        indexArray.push_back(topCenterIndex);
        indexArray.push_back(v0);
        indexArray.push_back(v1);
    }

    return { vertices, indexArray };
}

VertexIndexListCombo<VertexXYZNormal> createCylinder(unsigned int nSegments,
                                                     float radius, float height)
{
    return createConicalCylinder(nSegments, radius, radius, height);
}

VertexIndexListCombo<VertexXYZNormal> createCone(unsigned int nSegments, float radius,
                                                 float height)
{
    return createConicalCylinder(nSegments, radius, 0.f, height);
}

void LightSourceRenderData::updateBasedOnLightSources(const RenderData& renderData,
                                const std::vector<std::unique_ptr<LightSource>>& sources)
{
    unsigned int nEnabledLightSources = 0;
    intensitiesBuffer.resize(sources.size());
    directionsViewSpaceBuffer.resize(sources.size());

    // Get intensities and view space direction for the given light sources,
    // given the provided render data information
    for (const std::unique_ptr<LightSource>& lightSource : sources) {
        if (!lightSource->isEnabled()) {
            continue;
        }
        intensitiesBuffer[nEnabledLightSources] = lightSource->intensity();
        directionsViewSpaceBuffer[nEnabledLightSources] =
            lightSource->directionViewSpace(renderData);

        nEnabledLightSources++;
    }
    nLightSources = nEnabledLightSources;
}

} // namespace openspace::rendering::helper
