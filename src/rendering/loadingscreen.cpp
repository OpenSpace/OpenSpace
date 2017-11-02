/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/rendering/loadingscreen.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <random>

namespace openspace {

LoadingScreen::LoadingScreen(glm::vec2 windowSize)
    : _windowSize(std::move(windowSize))
{
    _program = ghoul::opengl::ProgramObject::Build(
        "Loading Screen",
        "${SHADERS}/loadingscreen.vert",
        "${SHADERS}/loadingscreen.frag"
    );

    
    _logoTexture = ghoul::io::TextureReader::ref().loadTexture(absPath("${OPENSPACE_DATA}/openspace-logo.png"));
    
    GLfloat data[] = {
        0.f, 0.f,
        1.f, 1.f,
        0.f, 1.f,

        0.f, 0.f,
        1.f, 0.f,
        1.f, 1.f
    };

    glGenVertexArrays(1, &_vao);
    glBindVertexArray(_vao);
    glGenBuffers(1, &_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        2 * sizeof(GLfloat),
        nullptr
    );

    glBindVertexArray(0);

}

LoadingScreen::~LoadingScreen() {
    _logoTexture = nullptr;
}

void LoadingScreen::render() {
    // Clear background
    glClearColor(0.8f, 0.8f, 0.8f, 1.f);
    glClear(ClearBufferMask::GL_COLOR_BUFFER_BIT);

    _program->activate();

    _program->setUniform(
        "ortho",
        glm::ortho(
            0.f, static_cast<float>(_windowSize.x), 0.f, static_cast<float>(_windowSize.y)
        )
    );

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<> dis(0.0, 1.0);

    _program->setUniform(
        "color",
        glm::vec4(dis(gen), dis(gen), dis(gen), 1.f)
    );

    // Draw the background color
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);


    _program->deactivate();


    std::this_thread::sleep_for(std::chrono::milliseconds(16));
    OsEng.windowWrapper().swapBuffer();
}

} // namespace openspace
