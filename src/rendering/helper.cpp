/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

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
    ZoneScoped
    TracyGpuZone("helper::initialize")

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
        shaders.xyuvrgba.cache,
        { "tex", "hasTexture", "shouldFlipTexture", "proj", "color" }
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
        shaders.screenfilling.cache,
        { "tex", "hasTexture", "shouldFlipTexture", "proj", "color" }
    );


    //
    // Square vertex objects
    //
    glGenVertexArrays(1, &vertexObjects.square.vao);
    glGenBuffers(1, &vertexObjects.square.vbo);

    glBindVertexArray(vertexObjects.square.vao);
    glBindBuffer(GL_ARRAY_BUFFER, vertexObjects.square.vbo);

    struct VertexXYUVRGBA {
        GLfloat xy[2];
        GLfloat uv[2];
        GLfloat rgba[4];
    };
    VertexXYUVRGBA data[] = {
        // X     Y    U    V    R    G    B    A
        -1.f, -1.f, 0.f, 0.f, 1.f, 1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f, 1.f, 1.f, 1.f, 1.f,
         1.f,  1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f,

        -1.f, -1.f, 0.f, 0.f, 1.f, 1.f, 1.f, 1.f,
         1.f,  1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f,
         1.f, -1.f, 1.f, 0.f, 1.f, 1.f, 1.f, 1.f
    };
    glBufferData(GL_ARRAY_BUFFER, 6 * sizeof(VertexXYUVRGBA), data, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(VertexXYUVRGBA), nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(VertexXYUVRGBA),
        reinterpret_cast<GLvoid*>(offsetof(VertexXYUVRGBA, uv)));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(VertexXYUVRGBA),
        reinterpret_cast<GLvoid*>(offsetof(VertexXYUVRGBA, rgba)));
    glBindVertexArray(0);

    //
    // Sphere vertex array object
    //
    std::pair<std::vector<Vertex>, std::vector<GLushort>> sphereData = createSphere(
        64, glm::vec3(1.f, 1.f, 1.f), glm::vec4(1.f, 1.f, 1.f, 1.f)
    );

    glGenVertexArrays(1, &vertexObjects.sphere.vao);
    glGenBuffers(1, &vertexObjects.sphere.vbo);
    glGenBuffers(1, &vertexObjects.sphere.ibo);

    glBindVertexArray(vertexObjects.sphere.vao);
    glBindBuffer(GL_ARRAY_BUFFER, vertexObjects.sphere.vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        sphereData.first.size() * sizeof(Vertex),
        sphereData.first.data(),
        GL_STATIC_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vertexObjects.sphere.ibo);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        sphereData.second.size() * sizeof(GLushort),
        sphereData.second.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
        reinterpret_cast<GLvoid*>(offsetof(Vertex, uv)));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
        reinterpret_cast<GLvoid*>(offsetof(Vertex, rgba)));
    glBindVertexArray(0);
    vertexObjects.sphere.nElements = static_cast<int>(sphereData.second.size());


    //
    // Empty vertex array objects
    //
    glGenVertexArrays(1, &vertexObjects.empty.vao);

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

VertexXYZ convertToXYZ(const Vertex& v) {
    return VertexXYZ{ v.xyz[0], v.xyz[1], v.xyz[2] };
}

std::vector<VertexXYZ> convert(std::vector<Vertex> v) {
    std::vector<VertexXYZ> result(v.size());
    std::transform(v.begin(), v.end(), result.begin(), convertToXYZ);
    return result;
}

std::vector<Vertex> createRing(int nSegments, float radius, glm::vec4 colors) {
    const int nVertices = nSegments + 1;
    std::vector<Vertex> vertices(nVertices);

    const float fsegments = static_cast<float>(nSegments);

    for (int i = 0; i <= nSegments; ++i) {
        const float fi = static_cast<float>(i);

        const float theta = fi * glm::pi<float>() * 2.f / fsegments;  // 0 -> 2*PI

        const float x = radius * std::cos(theta);
        const float y = radius * std::sin(theta);
        const float z = 0.f;

        const float u = std::cos(theta);
        const float v = std::sin(theta);

        vertices[i] = { x, y, z, u, v, colors.r, colors.g, colors.b, colors.a };
    }
    return vertices;
}

std::pair<std::vector<Vertex>, std::vector<GLushort>> createSphere(int nSegments,
                                                                   glm::vec3 radii,
                                                                   glm::vec4 colors)
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

            const float x = radii[0] * sin(theta) * cos(phi);
            const float y = radii[1] * sin(theta) * sin(phi);
            const float z = radii[2] * cos(theta); // Z points towards pole (theta = 0)

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

} // namespace openspace::rendering::helper
