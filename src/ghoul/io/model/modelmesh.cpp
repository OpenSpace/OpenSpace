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

#include <ghoul/io/model/modelmesh.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <cstddef>
#include <string>
#include <utility>

namespace {
    using namespace ghoul;
    using namespace ghoul::io;

    std::string textureTypeToString(const ModelMesh::TextureType& type) {
        switch (type) {
            case ModelMesh::TextureType::TextureDiffuse:  return "texture_diffuse";
            case ModelMesh::TextureType::TextureNormal:   return "texture_normal";
            case ModelMesh::TextureType::TextureSpecular: return "texture_specular";
            case ModelMesh::TextureType::ColorDiffuse:    return "color_diffuse";
            case ModelMesh::TextureType::ColorSpecular:   return "color_specular";
            default:                                      throw MissingCaseException();
        }
    }
} // namespace

namespace ghoul::io {

ModelMesh::ModelMesh(std::vector<Vertex> vertices, std::vector<unsigned int> indices,
                     std::vector<Texture> textures, bool isInvisible,
                     bool hasVertexColors)
    : _vertices(std::move(vertices))
    , _indices(std::move(indices))
    , _textures(std::move(textures))
    , _isInvisible(isInvisible)
    , _hasVertexColors(hasVertexColors)
{}

void ModelMesh::generateDebugTexture(ModelMesh::Texture& texture) {
    texture.texture = nullptr;
    texture.hasTexture = false;
    texture.useForcedColor = true;
    texture.type = ModelMesh::TextureType::ColorDiffuse;
}

void ModelMesh::render(opengl::ProgramObject& program, const glm::mat4& meshTransform,
                       bool isFullyTexturedModel, bool isProjection) const
{
    // Count how many textures have image textures
    int counter = 0;
    for (const Texture& texture : _textures) {
        if (texture.hasTexture) {
            counter++;
        }
    }

    std::vector<opengl::TextureUnit> textureUnits(counter);
    int textureUnitIndex = 0;

    if (!isProjection) {
        if (isFullyTexturedModel) {
            // Use embeded vertex colors if specified
            program.setUniform("use_vertex_colors", _hasVertexColors);

            // Reset shader
            program.setUniform("has_texture_diffuse", false);
            program.setUniform("has_texture_normal", false);
            program.setUniform("has_texture_specular", false);
            program.setUniform("has_color_specular", false);

            // If mesh is invisible and it has not been forced to render then don't render
            if (_isInvisible && _textures.empty()) {
                return;
            }

            // Bind appropriate textures
            for (const Texture& texture : _textures) {
                // Tell shader wether to render invisible mesh with flashy color or not
                program.setUniform("use_forced_color", texture.useForcedColor);
                if (texture.useForcedColor) {
                    break;
                }

                const std::string name = textureTypeToString(texture.type);
                // Use texture or color
                if (texture.hasTexture) {
                    // Activate proper texture unit before binding
                    textureUnits[textureUnitIndex].bind(*texture.texture);

                    // Specular special case
                    if (texture.type == TextureType::TextureSpecular) {
                        program.setUniform("has_color_specular", false);
                    }

                    // Tell shader to use textures and set texture unit
                    program.setUniform("has_" + name, true);
                    program.setUniform(name, textureUnits[textureUnitIndex]);

                    // Advance the texture unit index
                    textureUnitIndex++;
                }
                else {
                    // Use embedded simple colors instead of textures
                    if (texture.type == TextureType::ColorDiffuse) {
                        program.setUniform("has_texture_diffuse", false);
                    }
                    else if (texture.type == TextureType::ColorSpecular) {
                        program.setUniform("has_texture_specular", false);
                        program.setUniform("has_" + name, true);
                    }

                    // Set the color in shader
                    program.setUniform(name, texture.color);
                }
            }
        }
        else {
            // Reset shader
            program.setUniform("has_texture_diffuse", false);

            // Bind appropriate textures
            for (const Texture& texture : _textures) {
                if (texture.type == TextureType::TextureDiffuse ||
                    texture.type == TextureType::ColorDiffuse)
                {
                    if (texture.hasTexture) {
                        // Use texture or color
                        // Activate proper texture unit before binding
                        textureUnits[textureUnitIndex].bind(*texture.texture);

                        // Tell shader to use textures and set texture unit
                        program.setUniform("has_texture_diffuse", true);
                        program.setUniform("baseTexture", textureUnits[textureUnitIndex]);

                        // Advance the texture unit index
                        textureUnitIndex++;
                        break;
                    }
                    else {
                        // Use embedded simple colors instead of textures
                        program.setUniform("has_texture_diffuse", false);

                        // Set the color in shader
                        program.setUniform("baseColor", texture.color);
                        break;
                    }
                }
            }
        }
    }

    // Transform mesh
    program.setUniform("meshTransform", meshTransform);
    const glm::dmat4 normalTransform = glm::transpose(glm::inverse(meshTransform));
    program.setUniform("meshNormalTransform", glm::mat4(normalTransform));

    // Render the mesh object
    glBindVertexArray(_vao);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_indices.size()),
        GL_UNSIGNED_INT,
        nullptr
    );
    glBindVertexArray(0);
}

float ModelMesh::calculateBoundingRadius(glm::mat4& transform) const {
    // Calculate the bounding sphere of the mesh
    float maximumDistanceSquared = 0.f;
    for (const Vertex& v : _vertices) {
        // Apply the transform to the vertex to get its final position
        glm::vec4 position(v.position[0], v.position[1], v.position[2], 1.f);
        position = transform * position;

        const float d = std::pow(
            position.x, 2.f) +
            std::pow(position.y, 2.f) +
            std::pow(position.z, 2.f
        );

        maximumDistanceSquared = std::max(d, maximumDistanceSquared);
    }
    return maximumDistanceSquared;
}

void ModelMesh::setInvisible(bool isInvisible) {
    _isInvisible = isInvisible;
}

bool ModelMesh::isInvisible() const {
    return _isInvisible;
}

bool ModelMesh::hasVertexColors() const {
    return _hasVertexColors;
}

bool ModelMesh::isTransparent() const {
    for (const Texture& t : _textures) {
        if ((t.type == TextureType::TextureDiffuse ||
             t.type == TextureType::ColorDiffuse) && t.isTransparent)
        {
            return true;
        }
    }
    return false;
}

const std::vector<ModelMesh::Vertex>& ModelMesh::vertices() const {
    return _vertices;
}

const std::vector<unsigned int>& ModelMesh::indices() const {
    return _indices;
}

const std::vector<ModelMesh::Texture>& ModelMesh::textures() const {
    return _textures;
}

void ModelMesh::initialize() {
    ZoneScoped;

    if (_vertices.empty()) {
        LERRORC("ModelMesh", "Cannot initialize empty mesh");
        return;
    }

    glCreateBuffers(1, &_vbo);
    glNamedBufferStorage(
        _vbo,
        _vertices.size() * sizeof(Vertex),
        _vertices.data(),
        GL_NONE_BIT
    );

    glCreateBuffers(1, &_ibo);
    glNamedBufferStorage(
        _ibo,
        _indices.size() * sizeof(unsigned int),
        _indices.data(),
        GL_NONE_BIT
    );

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(Vertex));
    glVertexArrayElementBuffer(_vao, _ibo);

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    glEnableVertexArrayAttrib(_vao, 1);
    glVertexArrayAttribFormat(_vao, 1, 2, GL_FLOAT, GL_FALSE, offsetof(Vertex, tex));
    glVertexArrayAttribBinding(_vao, 1, 0);

    glEnableVertexArrayAttrib(_vao, 2);
    glVertexArrayAttribFormat(_vao, 2, 3, GL_FLOAT, GL_FALSE, offsetof(Vertex, normal));
    glVertexArrayAttribBinding(_vao, 2, 0);

    glEnableVertexArrayAttrib(_vao, 3);
    glVertexArrayAttribFormat(_vao, 3, 3, GL_FLOAT, GL_FALSE, offsetof(Vertex, tangent));
    glVertexArrayAttribBinding(_vao, 3, 0);

    glEnableVertexArrayAttrib(_vao, 4);
    glVertexArrayAttribFormat(_vao, 4, 3, GL_FLOAT, GL_FALSE, offsetof(Vertex, color));
    glVertexArrayAttribBinding(_vao, 4, 0);

    // Initialize textures
    // Also check if there are several textures/colors of the same type for this mesh
    unsigned int nDiffuse = 0;
    unsigned int nSpecular = 0;
    unsigned int nNormal = 0;
    for (const Texture& texture : _textures) {
        switch (texture.type) {
            case TextureType::TextureDiffuse:
            case TextureType::ColorDiffuse:
                nDiffuse++;
                break;
            case TextureType::TextureSpecular:
            case TextureType::ColorSpecular:
                nSpecular++;
                break;
            case TextureType::TextureNormal:
                nNormal++;
                break;
        }
    }

    if (nDiffuse > 1 || nSpecular > 1 || nNormal > 1) {
        LWARNINGC(
            "ModelMesh",
            "More than one texture or color of same type cannot be used for the same "
            "mesh. Only the latest option will be used"
        );
    }
}

void ModelMesh::deinitialize() const {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
    glDeleteBuffers(1, &_ibo);
}

} // namespace ghoul::io
