/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#include <modules/iswa/rendering/colorbar.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/textureunit.h>

namespace openspace{
ColorBar::ColorBar()
    :_shader(nullptr)
    ,_texture(nullptr)
    ,_quad(0)
    ,_vertexPositionBuffer(0)
{}

bool ColorBar::initialize(){
    std::cout << "initializeing colorbar" << std::endl;
    if (_shader == nullptr) {
        // DatePlane Program
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("ColorBarProgram",
            "${MODULE_ISWA}/shaders/colorbar_vs.glsl",
            "${MODULE_ISWA}/shaders/colorbar_fs.glsl"
            );
        if (!_shader)
            return false;
    }

    createPlane();

    glm::size3_t dimensions = glm::size3_t(300,100,1);
    ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
    ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;
        
    _texture = 
        std::make_unique<ghoul::opengl::Texture>(dimensions, ghoul::opengl::Texture::Format::Red, GL_RED, GL_FLOAT, filtermode, wrappingmode);
    if(_texture){

        std::cout << "Texture created" << std::endl;
    }

    return true;
}

bool ColorBar::deinitialize(){
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

void ColorBar::render(){

}

void ColorBar::render(ColorBarData& data){
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);

    glm::mat4 transform = glm::mat4(1.0);

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", data.transform);

    _shader->setUniform("top", data.top);
    _shader->setUniform("mid", data.mid);
    _shader->setUniform("bot", data.bot);
    _shader->setUniform("tfValues", data.tfValues);

    setPscUniforms(*_shader.get(), data.camera, data.position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void ColorBar::setPscUniforms(
    ghoul::opengl::ProgramObject& program, 
    const Camera& camera,
    const PowerScaledCoordinate& position) 
{
    program.setUniform("campos", camera.position().vec4());
    program.setUniform("objpos", position.vec4());
    program.setUniform("camrot", camera.viewRotationMatrix());
    program.setUniform("scaling", camera.scaling());
}

void ColorBar::createPlane(){
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
    const GLfloat x = 1;
    const GLfloat y = 3;
    const GLfloat w = 7.5;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //    x      y     z     w     s     t
        -x, -y, 0, w, 0, 1,
         x,  y, 0, w, 1, 0,
        -x,  y, 0, w, 0, 0,
        -x, -y, 0, w, 0, 1,
         x, -y, 0, w, 1, 1,
         x,  y, 0, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

}//namespace openspace