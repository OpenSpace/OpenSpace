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

#ifndef __GHOUL___MODELMESH___H__
#define __GHOUL___MODELMESH___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <cstdint>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace ghoul::io {

class ModelMesh {
public:
    enum class TextureType : uint8_t {
        TextureDiffuse = 0,
        TextureNormal,
        TextureSpecular,
        ColorDiffuse,
        ColorSpecular
    };

    struct Vertex {
        GLfloat position[3] = { 0.f, 0.f, 0.f };
        GLfloat tex[2] = { 0.f, 0.f };
        GLfloat normal[3] = { 0.f, 0.f, 0.f };
        GLfloat tangent[3] = { 0.f, 0.f, 0.f };
        GLfloat color[3] = { 0.f, 0.f, 0.f };
    };

    struct Texture {
        opengl::Texture* texture = nullptr;
        TextureType type = TextureType::TextureDiffuse;
        bool hasTexture = false;
        bool useForcedColor = false;
        glm::vec4 color = glm::vec4(0.f, 0.f, 0.f, 1.f);
        bool isTransparent = false;
    };

    static void generateDebugTexture(ModelMesh::Texture& texture);

    ModelMesh(std::vector<Vertex> vertices, std::vector<unsigned int> indices,
        std::vector<Texture> textures, bool isInvisible = false,
        bool hasVertexColors = false);

    ModelMesh(ModelMesh&&) noexcept = default;
    ~ModelMesh() noexcept = default;

    void initialize();
    void deinitialize() const;
    void render(opengl::ProgramObject& program, const glm::mat4& meshTransform,
        bool isFullyTexturedModel = true, bool isProjection = false) const;
    float calculateBoundingRadius(glm::mat4& transform) const;

    void setInvisible(bool isInvisible);
    bool isInvisible() const;
    bool hasVertexColors() const;
    bool isTransparent() const;

    const std::vector<Vertex>& vertices() const;
    const std::vector<unsigned int>& indices() const;
    const std::vector<Texture>& textures() const;

private:
    std::vector<Vertex> _vertices;
    std::vector<unsigned int> _indices;
    std::vector<Texture> _textures;

    bool _isInvisible = false;
    bool _hasVertexColors = false;

    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLuint _ibo = 0;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELMESH___H__
