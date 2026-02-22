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

#include <modules/iswa/rendering/renderabletextureplane.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

Documentation RenderableTexturePlane::Documentation() {
    openspace::Documentation doc = RenderableTextureCygnet::Documentation();
    doc.name = "RenderableTexturePlane";
    doc.id = "iswa_renderable_textureplane";
    return doc;
}

RenderableTexturePlane::RenderableTexturePlane(const ghoul::Dictionary& dictionary)
    : RenderableTextureCygnet(dictionary)
{}

void RenderableTexturePlane::initializeGL() {
    RenderableTextureCygnet::initializeGL();

    if (!_shader) {
        _shader = global::renderEngine->buildRenderProgram(
            "PlaneProgram",
            absPath("${MODULE_ISWA}/shaders/textureplane_vs.glsl"),
            absPath("${MODULE_ISWA}/shaders/textureplane_fs.glsl")
        );
    }
}

void RenderableTexturePlane::deinitializeGL() {
    _shader = nullptr;
    RenderableTextureCygnet::deinitializeGL();
}

void RenderableTexturePlane::setUniforms() {
    ghoul::opengl::TextureUnit unit;
    unit.bind(*_textures[0]);
    _shader->setUniform("texture1", unit);
    _shader->setUniform("transparency", _alpha.value());
}

bool RenderableTexturePlane::createGeometry() {
    struct Vertex {
        glm::vec4 position;
        glm::vec2 texCoords;
    };

    glCreateBuffers(1, &_vbo);
    float s = _data.spatialScale.x;
    const GLfloat x = s * _data.scale.x / 2.f;
    const GLfloat y = s * _data.scale.y / 2.f;
    const GLfloat z = s * _data.scale.z / 2.f;
    const GLfloat w = _data.spatialScale.w;
    const Vertex VertexData[] = {
        { glm::vec4(-x, -y,                  -z,  w), glm::vec2(0.f, 1.f) },
        { glm::vec4( x,  y,                   z,  w), glm::vec2(1.f, 0.f) },
        { glm::vec4(-x, ((x > 0) ? y : -y),   z,  w), glm::vec2(0.f, 0.f) },
        { glm::vec4(-x, -y,                  -z,  w), glm::vec2(0.f, 1.f) },
        { glm::vec4( x,  ((x > 0) ? -y : y), -z,  w), glm::vec2(1.f, 1.f) },
        { glm::vec4( x,  y,                   z,  w), glm::vec2(1.f, 0.f) }
    };
    glNamedBufferStorage(_vbo, sizeof(VertexData), VertexData, GL_NONE_BIT);

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(Vertex));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 4, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    glEnableVertexArrayAttrib(_vao, 1);
    glVertexArrayAttribFormat(
        _vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(Vertex, texCoords)
    );
    glVertexArrayAttribBinding(_vao, 1, 0);

    return true;
}

bool RenderableTexturePlane::destroyGeometry() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);

    return true;
}

void RenderableTexturePlane::renderGeometry() const {
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

} // namespace openspace
